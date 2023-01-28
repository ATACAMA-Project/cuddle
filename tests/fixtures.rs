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

    let _cddl = cuddle::parse_cddl(&std::fs::read_to_string(&p).unwrap(), &p.to_string_lossy())
        .expect("Could not parse CDDL");

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

        let components: Vec<&str> = filename.split(".").collect();

        match components.as_slice() {
            &[_, "fail", "diag"] | &[_, "fail", _, "diag"] => {}
            &[_, "pass", "diag"] | &[_, "pass", _, "diag"] => {}
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
    let filename = p.to_string_lossy();
    let result = cuddle::parse_cddl(&cddl_content, &filename);
    assert!(result.is_err(), "Result was not an error: {result:#?}");
}

#[test_resources("tests/fails/parse/**/*.cddl")]
fn fail_resolve(resource: &str) {
    let p = PathBuf::from(format!("../{}", resource));
    assert!(p.is_file(), "Resource {resource} should be a CDDL _file_.");
    let cddl_content = std::fs::read_to_string(&p).unwrap();
    let filename = p.to_string_lossy();
    let result = cuddle::parse_cddl(&cddl_content, &filename).expect("Should have parsed");

    let resolved = Cddl::from_cddl_root(&result);
    assert!(resolved.is_err(), "Result was not an error: {resolved:#?}")
}
