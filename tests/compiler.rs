use std::path::Path;

use rbap_lang::{Compiler, Result};

macro_rules! test_file {
    ($fname:expr) => {
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/resources/", $fname) // assumes Linux ('/')!
    };
}

#[test]
fn compile_source_file_and_build_compiler_from_that_compiled_file() -> Result<()> {
    let source = Path::new(test_file!("compile.rbap"));
    let dest = Path::new(test_file!("artifact_compiled"));
    let compiler_source = Compiler::compile(source)?;
    compiler_source.into_file(dest)?;

    let compiler_source = Compiler::compile(source)?;
    let compiler_compiled = Compiler::from_file(dest)?;

    assert_eq!(
        compiler_source, compiler_compiled,
        "compiler from source and compiler from compiled file do not match"
    );

    Ok(())
}
