use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

fn main() -> ! {
    test_harness::main()
}

test_harness::test_corpus!(test_harness::TestDirectory::new(
    "my_test",
    "tests/ui/pass/",
    MyRunner
));

#[derive(Clone)]
pub struct MyRunner;

impl test_harness::PathTestRunner for MyRunner {
    fn is_test_path(&self, path: &Path) -> bool {
        path.extension() == Some(OsStr::new("rs"))
    }

    fn expected_output_path(&self, path: &Path) -> PathBuf {
        let mut path = path
            .parent()
            .unwrap()
            .join("expected")
            .join(path.file_name().unwrap());
        path.set_extension("json");
        path
    }

    fn run_test(&self, path: &Path) -> std::io::Result<Option<Result<String, String>>> {
        let input = std::fs::read_to_string(path)?;

        if input == "\n" {
            some_func()
        }

        if input == "124\n" {
            return Ok(Some(Err(input)));
        }

        Ok(Some(Ok(input)))
    }
}

fn some_func() {
    panic!()
}
