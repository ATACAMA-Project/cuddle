extern crate test_generator;

use cuddle::cddl::Cddl;
use std::path::PathBuf;
use test_generator::test_resources;

#[test_resources("tests/fixtures/**/*.cddl")]
fn pass(resource: &str) {
    let p = PathBuf::from(format!("../{}", resource));
    assert!(p.is_file(), "Resource {resource} should be a CDDL _file_.");

    let filename = p.file_name().unwrap().to_string_lossy();
    let dir = p.parent().unwrap();

    let test_name = filename.split(".").next().unwrap();

    let cddl_content = std::fs::read_to_string(&p).unwrap();
    let cddl_root = cuddle::parse_cddl(&cddl_content, &p).expect("Could not parse CDDL");

    for entry in dir.read_dir().unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        if !path.is_file() {
            continue;
        }
        if !path
            .file_name()
            .unwrap()
            .to_string_lossy()
            .starts_with(test_name)
        {
            continue;
        }

        let cddl = Cddl::from_cddl_root(&cddl_root).unwrap();
        let components: Vec<&str> = filename.split(".").collect();

        match components.as_slice() {
            &[_, "fail", "diag"] | &[_, "fail", _, "diag"] => {
                cddl.validate_cbor(&std::fs::read(&path).unwrap())
                    .expect_err("Expected validation error");
            }
            &[_, "pass", "diag"] | &[_, "pass", _, "diag"] => {
                cddl.validate_cbor(&std::fs::read(&path).unwrap())
                    .expect("Expected validation success");
            }
            &[_, "cddl"] => {
                // Ignore, this is the resource file.
            }
            _ => {
                panic!(r#"Found invalid file format: "{filename}""#);
            }
        }
    }
}

#[test_resources("tests/fails/parse/**/*.cddl")]
fn fail_parse(resource: &str) {
    let p = PathBuf::from(format!("../{}", resource));
    assert!(p.is_file(), "Resource {resource} should be a CDDL _file_.");
    let cddl_content = std::fs::read_to_string(&p).unwrap();
    let result = cuddle::parse_cddl(&cddl_content, &p);
    assert!(result.is_err(), "Result was not an error: {result:#?}");
}
