#![cfg_attr(feature = "nightly", feature(thread_spawn_hook))]

use std::{
    any::{Any, TypeId},
    cell::{Cell, RefCell},
    collections::{BinaryHeap, LinkedList},
    ffi::OsStr,
    fmt::Display,
    io::{self, Write},
    marker::PhantomData,
    panic::{RefUnwindSafe, UnwindSafe},
    path::{Path, PathBuf},
    sync::{Arc, Mutex, PoisonError},
};

use bstr::ByteSlice;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

use backtrace::Backtrace;

pub use linkme::distributed_slice;

mod serializer;

pub use serializer::to_string;

#[macro_export]
macro_rules! test_corpus {
    ($corpus:expr) => {
        const _: () = {
            #[$crate::distributed_slice($crate::TEST_CORPUSES)]
            static __TEST_CORPUS: &dyn $crate::TestCorpus = &$corpus;
        };
    };
}

#[derive(Clone)]
pub struct Opts {
    pub save_new: bool,
    pub save_changed: bool,
    pub backtrace_style: BacktraceStyle,
    pub filter: Option<String>,
    pub level: Option<tracing::level_filters::LevelFilter>,
}

#[derive(Debug, Default, Clone, Copy)]
pub enum BacktraceStyle {
    Disabled,
    #[default]
    Short,
    Full,
}

impl Opts {
    pub fn from_env() -> Self {
        let bt = std::env::var_os("RUST_BACKTRACE");
        let bt = bt.as_deref();
        let mut args = std::env::args();
        let mut filter = None;

        args.next();

        while let Some(arg) = args.next() {
            if arg == "--color" {
                args.next();
                continue;
            }

            filter = Some(arg);
            break;
        }

        Self {
            filter,
            save_new: std::env::var_os("TEST_SAVE").is_some(),
            save_changed: std::env::var_os("TEST_REGEN").is_some(),
            backtrace_style: if bt == Some(OsStr::new("full")) {
                BacktraceStyle::Full
            } else if bt == Some(OsStr::new("0")) {
                BacktraceStyle::Disabled
            } else if bt.is_some() {
                BacktraceStyle::Short
            } else {
                BacktraceStyle::Disabled
            },
            level: None,
        }
    }

    pub fn with_backtrace_style(self, style: BacktraceStyle) -> Self {
        Self {
            backtrace_style: style,
            ..self
        }
    }
}

#[linkme::distributed_slice]
pub static TEST_CORPUSES: [&dyn TestCorpus] = [..];

pub trait TestCorpus: Any + Sync + RefUnwindSafe {
    fn name(&self) -> &dyn Display;

    fn load_tests(&self, storage: &mut TestStorage);
}

impl dyn TestCorpus {
    pub fn downcast<T: TestCorpus>(&self) -> Option<&T> {
        if self.type_id() == TypeId::of::<T>() {
            Some(unsafe { &*(self as *const Self as *const T) })
        } else {
            None
        }
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum TestResult {
    Pass,
    Regen,
    Create,
    Changed {
        expected: String,
        output: String,
    },
    Fail {
        expected: Option<String>,
        error: String,
    },
    CouldNotRun {
        error: String,
    },
    New {
        output: String,
    },
    Skip,
    Panicked {
        message: String,
        backtrace: Option<Backtrace>,
    },
}

impl TestResult {
    fn kind(&self) -> TestResultKind {
        match self {
            TestResult::Pass => TestResultKind::Pass,
            TestResult::Regen => TestResultKind::Regen,
            TestResult::Create => TestResultKind::Create,
            TestResult::Changed { .. } => TestResultKind::Changed,
            TestResult::Fail { .. } => TestResultKind::Fail,
            TestResult::CouldNotRun { .. } => TestResultKind::CouldNotRun,
            TestResult::New { .. } => TestResultKind::New,
            TestResult::Skip => TestResultKind::Skip,
            TestResult::Panicked { .. } => TestResultKind::Panicked,
        }
    }
}

macro_rules! Counts {
    ($($fields:ident $progress:literal $color:path,)*) => {
        struct Counts {
            total: u64,
            $($fields:u64,)*
        }


        #[allow(non_camel_case_types)]
        mod colors {
            $(pub type $fields = $color;)*
        }

        impl core::ops::AddAssign for Counts {
            fn add_assign(&mut self, other: Self) {
                self.total += other.total;
                $(self.$fields += other.$fields;)*
            }
        }

        impl Counts {
            #[inline]
            pub fn empty() -> Self {
                Self {
                    total: 0,
                    $($fields: 0,)*
                }
            }

            $(
                #[inline]
                pub fn $fields() -> Self {
                    Self {
                        total: 1,
                        $fields: 1,
                        ..Self::empty()
                    }
                }
            )*

            pub fn print(&self) {
                use owo_colors::OwoColorize;
                let mut any_printed = false;
                println!("ran {} tests", self.total);
                $(if self.$fields != 0 {
                    any_printed = true;
                    println!(
                        "tests {}: {}",
                        stringify!($fields).fg::<$color>(),
                        self.$fields.fg::<$color>())
                })*
                if any_printed {
                    println!();
                }
            }

            pub fn print_progress(&self) {
                use owo_colors::OwoColorize;
                $(if self.$fields != 0 {
                    print!("{}", $progress.fg::<$color>())
                })*
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum TestResultKind {
    Pass,
    Skip,
    Regen,
    Create,
    New,
    Changed,
    Fail,
    CouldNotRun,
    Panicked,
}

impl TestResultKind {
    pub fn counts(self) -> Counts {
        match self {
            Self::Pass => Counts::passed(),
            Self::Changed => Counts::changed(),
            Self::Create => Counts::created(),
            Self::Regen => Counts::regened(),
            Self::Fail => Counts::failed(),
            Self::CouldNotRun => Counts::no_run(),
            Self::New => Counts::found(),
            Self::Skip => Counts::skipped(),
            Self::Panicked => Counts::panicked(),
        }
    }
}

Counts! {
    passed '.' owo_colors::colors::Green,
    skipped '-' owo_colors::colors::BrightBlack,
    regened 'R' owo_colors::colors::BrightBlue,
    created '+' owo_colors::colors::Blue,
    found 'n' owo_colors::colors::Cyan,
    changed 'C' owo_colors::colors::BrightRed,
    failed 'F' owo_colors::colors::BrightRed,
    no_run 'x' owo_colors::colors::Red,
    panicked '!' owo_colors::colors::Red,
}

pub trait BoxedTestCase: 'static + Send + Sync + UnwindSafe {
    fn name_boxed(&self) -> String;

    fn run_boxed(self: Box<Self>, corpus: &dyn TestCorpus, opts: &Opts) -> TestResult;
}

impl<T: TestCase> BoxedTestCase for T {
    #[inline]
    fn name_boxed(&self) -> String {
        T::name(self)
    }

    #[inline]
    fn run_boxed(self: Box<Self>, corpus: &dyn TestCorpus, opts: &Opts) -> TestResult {
        (*self).run(corpus, opts)
    }
}

impl<T: ?Sized + BoxedTestCase> TestCase for Box<T> {
    #[inline]
    fn name(&self) -> String {
        T::name_boxed(self)
    }

    #[inline]
    fn run(self, corpus: &dyn TestCorpus, opts: &Opts) -> TestResult {
        self.run_boxed(corpus, opts)
    }
}

pub trait TestCase: BoxedTestCase {
    fn name(&self) -> String;

    fn run(self, corpus: &dyn TestCorpus, opts: &Opts) -> TestResult;
}

pub fn is_hidden_dir(path: &Path) -> bool {
    match path.file_name() {
        #[cfg(unix)]
        Some(x) => {
            use std::os::unix::ffi::OsStrExt;
            return x.as_bytes().starts_with(b".");
        }
        _ => false,
    }
}

pub trait PathTestRunner: 'static + Sync + RefUnwindSafe {
    fn should_skip_directory(&self, path: &Path) -> bool {
        is_hidden_dir(path)
    }

    fn is_test_path(&self, path: &Path) -> bool;

    fn expected_output_path(&self, path: &Path) -> PathBuf;

    fn run_test(&self, path: &Path) -> io::Result<Option<Result<String, String>>>;
}

pub struct TestDirectory<T: PathTestRunner> {
    name: &'static str,
    path: &'static str,
    test_runner: T,
}

pub fn expected_in_directory(path: &Path, directory: &str) -> PathBuf {
    if let Some(parent) = path.parent() {
        let mut new_path = parent.join(directory);
        new_path.push(path.file_name().unwrap_or(OsStr::new("..")));
        new_path
    } else {
        Path::new(directory).join(path)
    }
}

impl<T: PathTestRunner> TestDirectory<T> {
    pub const fn new(name: &'static str, path: &'static str, runner: T) -> Self {
        Self {
            name,
            path,
            test_runner: runner,
        }
    }
}

impl<T: PathTestRunner> TestCorpus for TestDirectory<T> {
    fn name(&self) -> &dyn Display {
        &self.name
    }

    fn load_tests(&self, storage: &mut TestStorage) {
        struct PathTestCase<T> {
            path: PathBuf,
            ty: PhantomData<fn() -> *mut T>,
        }

        impl<T: PathTestRunner> TestCase for PathTestCase<T> {
            fn name(&self) -> String {
                self.path.display().to_string()
            }

            fn run(self, corpus: &dyn TestCorpus, opts: &Opts) -> TestResult {
                let test_corpus = corpus
                    .downcast::<TestDirectory<T>>()
                    .expect("Invalid test corpus for test case");

                if let Some(filter) = opts.filter.as_deref() {
                    if !self
                        .path
                        .as_os_str()
                        .to_str()
                        .expect("All file paths must be utf-8")
                        .contains(filter)
                    {
                        return TestResult::Skip;
                    }
                }

                let expected_path = test_corpus.test_runner.expected_output_path(&self.path);

                let is_skipped = || {
                    let expected_unique_suffix = {
                        let mut expected = expected_path.components();
                        let mut original = self.path.components();

                        loop {
                            let mut new_expected = expected.clone();
                            let mut new_original = original.clone();

                            match (new_expected.next(), new_original.next()) {
                                (Some(e), Some(o)) if e == o => {}
                                _ => break,
                            };

                            expected = new_expected;
                            original = new_original;
                        }

                        expected.as_path()
                    };

                    let expected_ptr = {
                        let mut expected = expected_path.components();
                        expected_unique_suffix.components().for_each(|_| {
                            expected.next_back();
                        });
                        expected.as_path()
                    };

                    expected_path
                        .ancestors()
                        .try_fold((), |(), path| {
                            if core::ptr::eq(path, expected_ptr)
                                || test_corpus.test_runner.should_skip_directory(path)
                            {
                                Err(true)
                            } else {
                                Ok(())
                            }
                        })
                        .err()
                        .unwrap_or(false)
                };

                assert!(
                    !test_corpus.test_runner.is_test_path(&expected_path) || is_skipped(),
                    "expected path may not match a test path: {}",
                    expected_path.display()
                );

                if opts.save_new {
                    if let Some(parent) = expected_path.parent() {
                        if let Err(err) = std::fs::create_dir_all(parent) {
                            return TestResult::CouldNotRun {
                                error: format!("could not run test {} since the expected output file could not be created due to {err}. \
                                                Note the output file is at {}", self.path.display(), expected_path.display()),
                            };
                        }
                    }
                }

                let expected = match std::fs::read_to_string(&expected_path) {
                    Ok(output) => Some(output),
                    Err(err) => {
                        if err.kind() == io::ErrorKind::NotFound {
                            None
                        } else {
                            return TestResult::CouldNotRun {
                                error: format!("could not run test {} since the expected output file could not be opened due to {err}. \
                                                Note the output file is at {}", self.path.display(), expected_path.display()),
                            };
                        }
                    }
                };

                match test_corpus.test_runner.run_test(&self.path) {
                    Ok(Some(Ok(output))) => match expected {
                        Some(expected) => {
                            if expected == output {
                                TestResult::Pass
                            } else if opts.save_changed
                                && std::fs::write(expected_path, &output).is_ok()
                            {
                                TestResult::Regen
                            } else {
                                TestResult::Changed { expected, output }
                            }
                        }
                        None => {
                            if opts.save_new && std::fs::write(expected_path, &output).is_ok() {
                                TestResult::Create
                            } else {
                                TestResult::New { output }
                            }
                        }
                    },
                    Ok(Some(Err(error))) => TestResult::Fail { expected, error },
                    Ok(None) => TestResult::Skip,
                    Err(err) => TestResult::CouldNotRun {
                        error: format!("could not run test {} due to {err}", self.path.display()),
                    },
                }
            }
        }

        if !std::path::Path::new(self.path).exists() {
            panic!("Test path does not exist: {}", self.path)
        }

        let mut walkdir = walkdir::WalkDir::new(self.path)
            .contents_first(false)
            .into_iter();

        while let Some(entry) = walkdir.next() {
            let Ok(entry) = entry else {
                continue;
            };

            if entry.file_type().is_dir() && self.test_runner.should_skip_directory(entry.path()) {
                walkdir.skip_current_dir();
                continue;
            }

            if !self.test_runner.is_test_path(entry.path()) {
                continue;
            }

            storage.add_test(PathTestCase::<T> {
                path: entry.path().to_owned(),
                ty: PhantomData,
            })
        }
    }
}

pub struct TestStorage {
    current_index: usize,
    tests: Vec<(usize, Box<dyn TestCase>)>,
}

impl TestStorage {
    pub fn add_test<T: TestCase>(&mut self, test: T) {
        self.add_boxed_test(Box::new(test))
    }

    pub fn add_boxed_test(&mut self, test: Box<dyn TestCase>) {
        self.tests.push((self.current_index, test))
    }
}

thread_local! {
    static PANIC_INFO: Cell<(String, Option<Backtrace>)> = const { Cell::new((String::new(), None)) };
    static TEST_LOGS: RefCell<Arc<Mutex<Vec<u8>>>> = RefCell::new(Arc::new(Mutex::new(Vec::new())));
    static IS_TEST_THREAD: Cell<bool> = const { Cell::new(false) };
}

struct TestWriter;

impl TestWriter {
    fn take_logs() -> Vec<u8> {
        TEST_LOGS.with_borrow(|x| {
            core::mem::take(&mut *x.lock().unwrap_or_else(std::sync::PoisonError::into_inner))
        })
    }
}

impl std::io::Write for TestWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        TEST_LOGS.with_borrow(|x| {
            x.lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .extend_from_slice(buf)
        });
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

pub fn main() -> ! {
    let has_failures = run_tests(Opts::from_env());
    std::process::exit(has_failures as i32)
}

#[must_use]
pub fn run_tests(opts: Opts) -> bool {
    let fmt = tracing_subscriber::fmt()
        // .with_max_level(opts.subscriber.level)
        .with_level(true)
        .with_ansi(true)
        .with_thread_names(true)
        .with_writer(move || TestWriter);

    if let Some(level) = opts.level {
        fmt.with_max_level(level).init();
    } else {
        fmt.with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .init();
    }

    #[cfg(feature = "nightly")]
    {
        // if we are currently in a test-thread, then all child threads should also log
        // to the same point, so all logs can be shown together
        std::thread::add_spawn_hook(|_thread| {
            let is_test_thread = IS_TEST_THREAD.get();
            let logs = TEST_LOGS.with_borrow(Arc::clone);
            move || {
                if is_test_thread {
                    TEST_LOGS.set(logs.clone());
                }
            }
        });
    }

    let mut storage = TestStorage {
        current_index: 0,
        tests: Vec::new(),
    };

    for (index, corpus) in TEST_CORPUSES.iter().enumerate() {
        storage.current_index = index;
        corpus.load_tests(&mut storage);
    }

    let style = opts.backtrace_style;

    let old_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        PANIC_INFO.with(move |cell| {
            cell.set((
                format!("{panic_info}"),
                match style {
                    BacktraceStyle::Disabled => None,
                    BacktraceStyle::Short | BacktraceStyle::Full => Some(Backtrace::new()),
                },
            ))
        });
    }));

    let mutex = Mutex::new(0);

    print!("\n    ");

    let (all_results, counts) = rayon::ThreadPoolBuilder::new()
        .start_handler(|_| IS_TEST_THREAD.set(true))
        .exit_handler(|_| IS_TEST_THREAD.set(false))
        .thread_name(|id| format!("test-harness-{id}"))
        .build()
        .unwrap()
        .scope(|_| {
            storage
                .tests
                .into_par_iter()
                .map(|(corpus_index, test)| {
                    let name = test.name();

                    let corpus = TEST_CORPUSES[corpus_index];

                    #[inline(never)]
                    fn test_harness_run_test<R>(
                        f: impl FnOnce() -> R + UnwindSafe,
                    ) -> std::thread::Result<R> {
                        std::panic::catch_unwind(f)
                    }

                    let x = match test_harness_run_test(|| test.run(corpus, &opts)) {
                        Ok(err) => err,
                        Err(_) => {
                            let (message, backtrace) =
                                PANIC_INFO.with(|x| x.replace((String::new(), None)));
                            TestResult::Panicked { message, backtrace }
                        }
                    };

                    let logs = TestWriter::take_logs();

                    (corpus_index, name, x, logs)
                })
                .fold(
                    || (Vec::new(), Counts::empty()),
                    |(mut results, mut counts), (corpus_index, name, result, logs)| {
                        let new_counts = result.kind().counts();

                        {
                            let progress =
                                &mut *mutex.lock().unwrap_or_else(PoisonError::into_inner);

                            *progress += 1;

                            new_counts.print_progress();

                            if *progress == 50 {
                                println!("    ");
                            }
                        }

                        counts += new_counts;

                        if !matches!(result.kind(), TestResultKind::Pass | TestResultKind::Skip) {
                            results.push(Item {
                                corpus_index,
                                name,
                                test_result: result,
                                logs,
                            });
                        }

                        (results, counts)
                    },
                )
                .map(|(mut results, counts)| {
                    results.sort();
                    (LinkedList::from_iter([results]), counts)
                })
                .reduce(
                    || (LinkedList::new(), Counts::empty()),
                    |(mut left_results, mut left_counts), (right_results, right_counts)| {
                        left_results.append(&mut { right_results });
                        left_counts += right_counts;
                        (left_results, left_counts)
                    },
                )
        });

    println!();

    std::panic::set_hook(old_hook);

    let mut heap = BinaryHeap::new();

    for mut results in all_results.into_iter() {
        if let Some(item) = results.pop() {
            heap.push(HeapItem { item, results });
        }
    }

    let mut prev_test_kind = None;

    while let Some(HeapItem { item, mut results }) = heap.pop() {
        use owo_colors::OwoColorize;

        if let Some(item) = results.pop() {
            heap.push(HeapItem { item, results })
        }

        let corpus = TEST_CORPUSES[item.corpus_index];

        if prev_test_kind != Some(item.test_result.kind()) {
            prev_test_kind = Some(item.test_result.kind());
            println!();
        }

        match item.test_result {
            TestResult::Pass => {
                println!(
                    "[{}] {} passed",
                    corpus.name().fg::<colors::passed>(),
                    item.name.fg::<colors::passed>()
                );
            }
            TestResult::Regen => {
                println!(
                    "[{}] {} regenerated",
                    corpus.name().fg::<colors::regened>(),
                    item.name.fg::<colors::regened>()
                );
            }
            TestResult::Create => {
                println!(
                    "[{}] {} created",
                    corpus.name().fg::<colors::created>(),
                    item.name.fg::<colors::created>()
                );
            }
            TestResult::Changed {
                mut expected,
                mut output,
            } => {
                println!(
                    "[{}] {} changed",
                    corpus.name().fg::<colors::changed>(),
                    item.name.fg::<colors::changed>()
                );
                println!("{:->120}", "");
                let mut equal_lines = None;
                let mut is_first = true;

                let mut print_equal_lines = |equal_lines: &mut Option<&str>, is_final: bool| {
                    let Some(equal) = equal_lines else {
                        return;
                    };

                    const KEEP_BEFORE: usize = 2;
                    const KEEP_AFTER: usize = 2;

                    let prefix = if is_first {
                        is_first = false;
                        0
                    } else {
                        equal
                            .bytes()
                            .enumerate()
                            .filter(|(_, b)| *b == b'\n')
                            .nth(KEEP_AFTER + 1)
                            .map_or(equal.len(), |x| x.0)
                    };

                    let suffix = if prefix == equal.len() || is_final {
                        equal.len()
                    } else {
                        equal[prefix + 1..]
                            .bytes()
                            .enumerate()
                            .rev()
                            .filter(|(_, b)| *b == b'\n')
                            .nth(KEEP_BEFORE)
                            .map_or(prefix, |x| x.0 + prefix + 1)
                    };

                    print!("{}", (&equal[..prefix]).bright_black());
                    if prefix != suffix {
                        print!(
                            "{}{}",
                            if prefix == 0 { "" } else { "\n" },
                            format_args!(
                                "... skipped {} lines ...",
                                equal[prefix..suffix]
                                    .bytes()
                                    .filter(|b| *b == b'\n')
                                    .count()
                                    // FIXME: these magic numbers need justification!
                                    + 1
                                    - if is_final { 2 } else { 0 }
                            )
                            .bright_black()
                            .bold()
                        );
                    }
                    print!("{}", (&equal[suffix..]).bright_black());
                    if is_final {
                        println!()
                    }

                    // if is_final {
                    //     let last_newline = equal.rfind('\n').unwrap_or(0);
                    //     let (mut equal, _b) = equal.split_at(last_newline);

                    //     let mut n = KEEP_AFTER + 1;
                    //     while let Some((line, e)) = equal.split_once('\n') {
                    //         if n == 0 {
                    //             break;
                    //         }
                    //         n -= 1;
                    //         println!("{}", line.bright_black());
                    //         equal = e;
                    //     }

                    //     let count = equal
                    //         .bytes()
                    //         .filter(|x| *x == b'\n')
                    //         .count()
                    //         .saturating_sub(KEEP_AFTER);

                    //     if count > 0 {
                    //         println!(
                    //             "{}",
                    //             format_args!("... skipped {count} lines ...")
                    //                 .bright_black()
                    //                 .bold()
                    //         );
                    //     }
                    // } else {
                    //     let last_newline = equal.rfind('\n').unwrap_or(0);
                    //     let (equal, b) = equal.split_at(last_newline);

                    //     let kept = if is_first {
                    //         is_first = false;
                    //         // -1
                    //         KEEP_BEFORE - 1
                    //     } else {
                    //         for equal in equal.split("\n").take(KEEP_AFTER + 1) {
                    //             println!("{}", equal.bright_black());
                    //         }

                    //         KEEP_BEFORE + KEEP_AFTER
                    //     };

                    //     let count = equal
                    //         .bytes()
                    //         .filter(|x| *x == b'\n')
                    //         .count()
                    //         .saturating_sub(kept);

                    //     if count > 0 {
                    //         println!(
                    //             "{}",
                    //             format_args!("... skipped {count} lines ...")
                    //                 .bright_black()
                    //                 .bold()
                    //         );
                    //     }

                    //     for equal in Vec::from_iter(equal.rsplit("\n").take(KEEP_BEFORE))
                    //         .into_iter()
                    //         .rev()
                    //     {
                    //         println!("{}", equal.bright_black());
                    //     }

                    //     print!("{}", (&b[1..]).bright_black());
                    // }
                    *equal_lines = None;
                };

                for (tag, s) in
                    similar::utils::diff_chars(similar::Algorithm::Myers, &expected, &output)
                {
                    use owo_colors::OwoColorize;
                    match tag {
                        similar::ChangeTag::Equal => {
                            assert!(equal_lines.is_none());
                            equal_lines = Some(s);
                        }
                        similar::ChangeTag::Delete => {
                            print_equal_lines(&mut equal_lines, false);
                            print!("{}", s.red().strikethrough())
                        }
                        similar::ChangeTag::Insert => {
                            print_equal_lines(&mut equal_lines, false);
                            print!("{}", s.green().underline())
                        }
                    }
                }

                print_equal_lines(&mut equal_lines, true);
                // for (tag, s) in
                //     similar::utils::diff_chars(similar::Algorithm::Myers, &expected, &output)
                // {
                //     use owo_colors::OwoColorize;
                //     match tag {
                //         similar::ChangeTag::Equal => print!("{}", s.bright_black()),
                //         similar::ChangeTag::Delete => print!("{}", s.red().strikethrough()),
                //         similar::ChangeTag::Insert => print!("{}", s.green().underline()),
                //     }
                // }

                if !item.logs.is_empty() {
                    println!("{0:=<57} LOGS {0:=<57}", "");
                    let _ = std::io::stdout().write_all(&item.logs);
                }
                println!("{:=>120}", "");
            }
            TestResult::Fail { expected: _, error } => {
                println!(
                    "[{}] {} failed",
                    corpus.name().fg::<colors::failed>(),
                    item.name.fg::<colors::failed>()
                );
                println!("{error}");

                if !item.logs.is_empty() {
                    println!("{0:=<37} LOGS {0:=<37}", "");
                    let _ = std::io::stdout().write_all(&item.logs);
                    println!("{:=<80}", "");
                }
            }
            TestResult::CouldNotRun { error } => {
                println!(
                    "[{}] {} could not be run",
                    corpus.name().fg::<colors::no_run>(),
                    item.name.fg::<colors::no_run>()
                );
                println!("{error}");

                if !item.logs.is_empty() {
                    println!("{0:=<37} LOGS {0:=<37}", "");
                    let _ = std::io::stdout().write_all(&item.logs);
                    println!("{:=<80}", "");
                }
            }
            TestResult::New { output } => {
                println!(
                    "[{}] {} found",
                    corpus.name().fg::<colors::found>(),
                    item.name.fg::<colors::found>()
                );

                println!("{:->120}", "");
                println!("{output}");

                if !item.logs.is_empty() {
                    println!("{0:=<57} LOGS {0:=<57}", "");
                    let _ = std::io::stdout().write_all(&item.logs);
                }
                println!("{:=<120}", "");
            }
            TestResult::Skip => {
                println!(
                    "[{}] {} skipped",
                    corpus.name().fg::<colors::skipped>(),
                    item.name.fg::<colors::skipped>()
                );
            }
            TestResult::Panicked { message, backtrace } => {
                println!(
                    "[{}] {} panicked",
                    corpus.name().fg::<colors::panicked>(),
                    item.name.fg::<colors::panicked>()
                );

                println!("{}", message);

                if let Some(mut backtrace) = backtrace {
                    if let BacktraceStyle::Short = opts.backtrace_style {
                        let mut found_start = false;
                        let mut frames = Vec::new();
                        for frame in backtrace.frames() {
                            let [sym] = frame.symbols() else { continue };

                            let Some(name) = sym.name() else { continue };

                            let name = format!("{name}");

                            // On all editions, the bulk of the panic formatting machinery is before
                            // the `core::panicking::panic_fmt` or `std::panicking::begin_panic<..>` symbols
                            // On 2015 <= edition < 2021 all of the panic formatting machinery
                            // is before these symbols
                            // on 2021 <= edition, then the panic formatting machinery actually ends with
                            // core::panicking::panic, so we look for this symbol and ignore everything before it
                            if name.as_bytes().contains_str("::panicking::panic")
                                || name.as_bytes().contains_str("::panicking::begin_panic<")
                            {
                                frames.clear();
                                found_start = true;
                                continue;
                            }

                            if found_start {
                                // after we have started the panic, we know that we are not in any user-defined code if we see
                                // test_harness

                                if name.as_bytes().contains_str("test_harness") {
                                    break;
                                }

                                frames.push(frame.clone());
                            }
                        }

                        assert!(
                            !frames.is_empty(),
                            "\
original backtrace
{backtrace:?}"
                        );

                        if found_start {
                            backtrace = Backtrace::from(frames);
                        }
                    }

                    println!("{backtrace:?}");
                } else {
                    println!("Backtrace is disabled, turn on RUST_BACKTRACE=1 to enable backtraces in tests")
                }

                if !item.logs.is_empty() {
                    println!("{0:=<37} LOGS {0:=<37}", "");
                    let _ = std::io::stdout().write_all(&item.logs);
                    println!("{:=<80}", "");
                }
            }
        }
    }

    if prev_test_kind.is_some() {
        println!();
    }

    counts.print();

    if counts.found != 0 {
        println!("set the TEST_SAVE=1 environment variable to save all newly found tests")
    }

    if counts.changed != 0 {
        println!("set the TEST_REGEN=1 environment variable to save all changed tests")
    }

    if counts.found != 0 || counts.changed != 0 {
        println!()
    }

    counts.failed != 0 || counts.no_run != 0 || counts.panicked != 0
}

struct Item {
    corpus_index: usize,
    name: String,
    test_result: TestResult,
    logs: Vec<u8>,
}

struct HeapItem {
    item: Item,
    results: Vec<Item>,
}

impl Eq for Item {}
impl PartialEq for Item {
    fn eq(&self, other: &Self) -> bool {
        self.test_result.kind() == other.test_result.kind() && self.name == other.name
    }
}

impl PartialOrd for Item {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Item {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .test_result
            .kind()
            .cmp(&self.test_result.kind())
            .then_with(|| self.corpus_index.cmp(&other.corpus_index))
            .then_with(|| self.name.cmp(&other.name))
    }
}

impl Eq for HeapItem {}
impl PartialEq for HeapItem {
    fn eq(&self, other: &Self) -> bool {
        self.item == other.item
    }
}

impl PartialOrd for HeapItem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HeapItem {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.item.cmp(&other.item)
    }
}
