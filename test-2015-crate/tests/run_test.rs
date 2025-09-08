test_harness::test_corpus! { Corpus }

struct Corpus;
struct TestPanicNoArgs;
struct TestPanicConstArg;
struct TestPanicDynArg;

impl test_harness::TestCorpus for Corpus {
    fn name(&self) -> &dyn std::fmt::Display {
        &"edition2015"
    }

    fn load_tests(&self, storage: &mut test_harness::TestStorage) {
        storage.add_test(TestPanicNoArgs);
        storage.add_test(TestPanicConstArg);
        storage.add_test(TestPanicDynArg);
    }
}

impl test_harness::TestCase for TestPanicNoArgs {
    fn name(&self) -> String {
        "panic-noarg-test".to_string()
    }

    fn run(self, _corpus: &dyn test_harness::TestCorpus, _opts: &test_harness::Opts) -> test_harness::TestResult {
        panic!();
    }
}

impl test_harness::TestCase for TestPanicConstArg {
    fn name(&self) -> String {
        "panic-const-test".to_string()
    }

    fn run(self, _corpus: &dyn test_harness::TestCorpus, _opts: &test_harness::Opts) -> test_harness::TestResult {
        panic!("hello world");
    }
}

impl test_harness::TestCase for TestPanicDynArg {
    fn name(&self) -> String {
        "panic-dyn-test".to_string()
    }

    fn run(self, _corpus: &dyn test_harness::TestCorpus, _opts: &test_harness::Opts) -> test_harness::TestResult {
        let x = 0;
        panic!("hello world, {}", x);
    }
}

fn main() {
    let _ = test_harness::run_tests(test_harness::Opts::from_env().with_backtrace_style(test_harness::BacktraceStyle::Short));
}