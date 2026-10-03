//! The analysis API never runs a `use`d module (ADR-0065 D4, #11212).
//!
//! A module whose export stash has computed keys used to be run by the
//! parse-time export probe, and a slang-activating module by slang activation.
//! Each fixture here leaves a marker file when its mainline runs.

use super::{check, symbols};
use std::path::PathBuf;

/// A fresh fixture directory holding `module` (named `name`) whose mainline
/// writes `marker`, plus the document that `use`s it.
struct Fixture {
    dir: PathBuf,
    marker: PathBuf,
    document: String,
}

impl Fixture {
    fn new(tag: &str, name: &str, body: &str) -> Self {
        let dir = std::env::temp_dir().join(format!(
            "mutsu-analysis-no-execute-{tag}-{}",
            std::process::id()
        ));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let marker = dir.join("marker");
        let module = format!("'{}'.IO.spurt('ran');\n{body}", marker.display());
        std::fs::write(dir.join(format!("{name}.rakumod")), module).unwrap();
        let document = format!("use lib '{}';\nuse {name};\nsay 1;\n", dir.display());
        Self {
            dir,
            marker,
            document,
        }
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.dir);
    }
}

const DYNAMIC_STASH: &str = "my package EXPORT::DEFAULT {\n    \
     for <a b> -> $c { OUR::{'&postfix:<' ~ $c ~ 'Z>'} := sub ($n) { $n } }\n}\n";

const SLANG: &str = "role SlangNoExecAnalysis { token term-now { nowish } }\n\
     my sub EXPORT {\n    \
     $*LANG.define_slang('MAIN', $*LANG.slang_grammar('MAIN').^mixin(SlangNoExecAnalysis));\n    \
     BEGIN Map.new\n}\n";

#[test]
fn check_does_not_run_a_module_with_computed_exports() {
    let fixture = Fixture::new("check-dyn", "DynNoExecAnalysis", DYNAMIC_STASH);
    check(&fixture.document);
    assert!(!fixture.marker.exists(), "check ran the used module");
}

#[test]
fn symbols_does_not_run_a_module_with_computed_exports() {
    let fixture = Fixture::new("symbols-dyn", "DynNoExecSymbols", DYNAMIC_STASH);
    symbols(&fixture.document);
    assert!(!fixture.marker.exists(), "symbols ran the used module");
}

#[test]
fn check_does_not_activate_a_slang() {
    let fixture = Fixture::new("check-slang", "SlangNoExecAnalysis", SLANG);
    check(&fixture.document);
    assert!(!fixture.marker.exists(), "check ran the slang module");
}
