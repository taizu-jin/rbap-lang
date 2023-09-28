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
