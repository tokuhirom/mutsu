//! The RakuAST round-trip frontend mode (ADR-10723 Stage 0).
//!
//! Under `MUTSU_RAKUAST`, a compilation unit the interpreter is about to run is
//! converted to its RakuAST tree and lowered back before it is compiled. The
//! result is what the RakuAST frontend will produce once the parser emits
//! RakuAST directly, so the number of test files that still pass in this mode
//! is the migration's metric.
//!
//! - `MUTSU_RAKUAST=1`: the program's own units, the main program and every
//!   `EVAL` string.
//! - `MUTSU_RAKUAST=all`: those, plus every `use`d module and `require`d file,
//!   bundled ones included.
//!
//! The two levels exist because a module is shared by every program that uses
//! it: while `Test.rakumod` itself does not round-trip, `all` fails every test
//! file at its first line, and `1` is the level whose count says something
//! about the files themselves.
//!
//! A construct the converter or the lowerer refuses is a compile error in this
//! mode, never a silent fallback to the parser's own tree: a fallback would
//! count a file as round-tripping when it does not.

use std::sync::OnceLock;

use crate::ast::Stmt;
use crate::value::RuntimeError;

/// Which kind of compilation unit is being parsed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Unit {
    /// The program's main unit.
    Mainline,
    /// An `EVAL` string.
    Eval,
    /// A `use`d module or a `require`d file.
    Module,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Off,
    ProgramUnits,
    All,
}

/// The `MUTSU_RAKUAST` setting. Read once per process.
fn mode() -> Mode {
    static MODE: OnceLock<Mode> = OnceLock::new();
    *MODE.get_or_init(|| match std::env::var("MUTSU_RAKUAST").as_deref() {
        Ok("1") => Mode::ProgramUnits,
        Ok("all") => Mode::All,
        _ => Mode::Off,
    })
}

/// Whether the `MUTSU_RAKUAST` mode runs a unit of this kind through the round
/// trip. Such a unit is parsed with its spellings kept (ADR-12199): the
/// conversion reads them, and the lowering hands the compiler the same tree it
/// would have had without them.
pub(crate) fn covers(unit: Unit) -> bool {
    match mode() {
        Mode::Off => false,
        Mode::ProgramUnits => unit != Unit::Module,
        Mode::All => true,
    }
}

/// Run a parsed compilation unit through the RakuAST round trip when the mode
/// covers its kind; hand it back untouched otherwise.
pub(crate) fn round_trip_if_enabled(
    stmts: Vec<Stmt>,
    unit: Unit,
) -> Result<Vec<Stmt>, RuntimeError> {
    if !covers(unit) {
        return Ok(stmts);
    }
    round_trip(&stmts).map_err(|err| {
        RuntimeError::new(format!(
            "MUTSU_RAKUAST: this compilation unit does not round-trip through RakuAST: {}",
            err.message
        ))
    })
}

/// `parse → RakuAST → lower`, the pipeline the RakuAST frontend replaces the
/// parser's tree with.
pub(crate) fn round_trip(stmts: &[Stmt]) -> Result<Vec<Stmt>, RuntimeError> {
    let node = super::convert::statement_list(stmts)?;
    super::lower(&node)
}
