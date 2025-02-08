use pretty_assertions::assert_eq;
use std::fs;
use std::path::Path;
use std::process::Command;

/// This function tests the interpreter by running it on a source file and comparing the output
/// to the expected output in the corresponding `.expected` file.
fn test_interpreter(source_file: &Path, expected_output_file: &Path) {
    let expected_output = fs::read_to_string(expected_output_file)
        .expect(&format!("Failed to read expected output file {}", expected_output_file.display()));

    let binary_name = env!("CARGO_PKG_NAME");
    let interpreter_path = format!("target/debug/{binary_name}");

    let output = Command::new(interpreter_path)
        .arg(source_file.to_str().unwrap())
        .arg("new")
        .output()
        .expect("Failed to run interpreter");

    let interpreter_output = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    assert_eq!(
        interpreter_output.trim(),
        expected_output.trim(),
        "Test failed for: {:?}",
        source_file
    );
}

#[test]
fn test_all_todo_files() {
    // Define the directory where `.todo` files are located
    let test_dir = Path::new("tests/todo2");

    // Iterate over all the entries in the `tests/todo` directory
    for entry in fs::read_dir(test_dir).expect("Failed to read test directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        // Check if the file has a `.todo` extension
        if path.extension().and_then(|s| s.to_str()) == Some("todo") {
            // Determine the corresponding `.expected` file
            let expected_output_file = path.with_extension("expected");

            // Run the interpreter test on the `.todo` file and its corresponding `.expected` file
            test_interpreter(&path, &expected_output_file);
        }
    }
}
