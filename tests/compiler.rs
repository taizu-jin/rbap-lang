use file_diff::diff;
use std::path::Path;

use rbap_lang::{Compiler, Result};

macro_rules! test_file {
    ($fname:expr) => {
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/resources/", $fname) // assumes Linux ('/')!
    };
}

#[test]
fn compile_source_file() -> Result<()> {
    let source = Path::new(test_file!("compile.rbap"));
    let dest = Path::new(test_file!("artifact_compiled"));
    let compiler = Compiler::compile(source)?;
    compiler.into_file(dest)?;

    assert!(
        diff(test_file!("artifact_compiled"), test_file!("compiled")),
        "compiled artifact does not match test case"
    );

    Ok(())
}

#[test]
fn build_compiler_from_compiled_file() -> Result<()> {
    let source = Path::new(test_file!("compiled"));
    let compiler = Compiler::from_file(source)?;

    let source = Path::new(test_file!("compile.rbap"));
    let compiler_from_source = Compiler::compile(source)?;

    assert_eq!(
        compiler, compiler_from_source,
        "compiler from source and compiler from compiled file do not match"
    );

    Ok(())
}
