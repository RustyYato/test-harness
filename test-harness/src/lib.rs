use std::{
    any::{Any, TypeId},
    cell::Cell,
    collections::{BinaryHeap, LinkedList},
    ffi::OsStr,
    fmt::Display,
    io,
    marker::PhantomData,
    panic::{RefUnwindSafe, UnwindSafe},
    path::{Path, PathBuf},
    sync::{Mutex, PoisonError},
};

use bstr::ByteSlice;
use rayon::{
    iter::plumbing,
    prelude::{IntoParallelIterator, ParallelIterator},
};

use backtrace::Backtrace;

#[derive(Default, Clone, Copy)]
pub struct Opts {
    save_new: bool,
    save_changed: bool,
    backtrace_style: BacktraceStyle,
}

#[derive(Default, Clone, Copy)]
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

        Self {
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
        }
    }
}

#[linkme::distributed_slice]
pub static TEST_CORPUSES: [&dyn TestCorpus] = [..];

pub trait TestCorpus: Any + Send + Sync + RefUnwindSafe {
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

pub trait PathTestRunner: 'static + Send + Sync + RefUnwindSafe {
    fn is_test_path(&self, path: &Path) -> bool;

    fn expected_output_path(&self, path: &Path) -> PathBuf;

    fn run_test(&self, path: &Path) -> io::Result<Option<Result<String, String>>>;
}

pub struct TestDirectory<T: PathTestRunner> {
    name: &'static str,
    path: &'static str,
    test_runner: T,
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

                let expected_path = test_corpus.test_runner.expected_output_path(&self.path);

                assert!(
                    !test_corpus.test_runner.is_test_path(&expected_path),
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

        for entry in walkdir::WalkDir::new(self.path) {
            let Ok(entry) = entry else  {
                continue;
            };

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
    static PANIC_INFO: Cell<(String, Option<Backtrace>)> = Cell::new((String::new(), None));
}

pub fn main() -> ! {
    let has_failures = run_tests(Opts::from_env());
    std::process::exit(has_failures as i32)
}

#[must_use]
pub fn run_tests(opts: Opts) -> bool {
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

    let (all_results, counts) = storage
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

            (corpus_index, name, x)
        })
        .fold(
            || (Vec::new(), Counts::empty()),
            |(mut results, mut counts), (corpus_index, name, result)| {
                let new_counts = result.kind().counts();

                {
                    let progress = &mut *mutex.lock().unwrap_or_else(PoisonError::into_inner);

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
        );

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
            TestResult::Changed { expected, output } => {
                println!(
                    "[{}] {} changed",
                    corpus.name().fg::<colors::changed>(),
                    item.name.fg::<colors::changed>()
                );
            }
            TestResult::Fail { expected: _, error } => {
                println!(
                    "[{}] {} failed",
                    corpus.name().fg::<colors::failed>(),
                    item.name.fg::<colors::failed>()
                );
                println!("{error}");
            }
            TestResult::CouldNotRun { error } => {
                println!(
                    "[{}] {} could not be run",
                    corpus.name().fg::<colors::no_run>(),
                    item.name.fg::<colors::no_run>()
                );
                println!("{error}");
            }
            TestResult::New { output } => {
                println!(
                    "[{}] {} found",
                    corpus.name().fg::<colors::found>(),
                    item.name.fg::<colors::found>()
                );

                println!("{:->120}", "");
                println!("{output}");
                println!("{:=>120}", "");
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
                            if !found_start {
                                let [sym] = frame.symbols() else {
                                    continue
                                };

                                let Some(name) = sym.name() else {
                                    continue
                                };

                                if name.as_bytes() == b"rust_begin_unwind" {
                                    found_start = true;
                                }
                            } else {
                                frames.push(frame.clone());

                                let [sym] = frame.symbols() else {
                                    continue
                                };

                                let Some(name) = sym.name() else {
                                    continue
                                };

                                if name.as_bytes().contains_str("test_harness_run_test") {
                                    break;
                                }
                            }
                        }

                        frames.pop();

                        if found_start {
                            backtrace = Backtrace::from(frames);
                        }
                    }

                    println!("{backtrace:?}");
                } else {
                    println!("Backtrace is disabled, turn on RUST_BACKTRACE=1 to enable backtraces in tests")
                }
            }
        }
    }

    if prev_test_kind.is_some() {
        println!();
    }

    counts.print();

    counts.failed != 0 || counts.no_run != 0 || counts.panicked != 0
}

struct Item {
    corpus_index: usize,
    name: String,
    test_result: TestResult,
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
