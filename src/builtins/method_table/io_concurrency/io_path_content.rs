//! `IO::Path`'s content rows (ADR-11276 §9.19): the methods that read the file
//! the path names (`slurp`, `lines`, `words`, `comb`) or open it (`open`). They
//! resolve the path against the cwd and use the interpreter's handle table, so
//! each is an interpreter row.

use super::io_path_ctx::{PathCtx, joined_args};
use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

/// The open adverbs the interpreter's flag parser binds (`:r`, `:bin`, `:enc`,
/// `:nl-in`, ...): what `slurp`, `lines`, `words` and `open` accept.
const OPEN_NAMED: &[&str] = &[
    "r",
    "w",
    "rw",
    "a",
    "append",
    "update",
    "ra",
    "rx",
    "x",
    "exclusive",
    "truncate",
    "bin",
    "chomp",
    "create",
    "nl-in",
    "nl-out",
    "out-buffer",
    "enc",
    "mode",
];

macro_rules! row {
    ($name:literal, $arity:literal, $handler:ident, $flags:expr, $named:expr) => {
        MethodRow {
            owner: "IO::Path",
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: $named,
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("slurp", 0, slurp_row, RowFlags::NONE, OPEN_NAMED),
    row!("lines", 0, lines_row, RowFlags::NONE, OPEN_NAMED),
    row!("lines", 1, lines_row, RowFlags::ANY_ARGS, OPEN_NAMED),
    row!("words", 0, words_row, RowFlags::NONE, OPEN_NAMED),
    row!("words", 1, words_row, RowFlags::ANY_ARGS, OPEN_NAMED),
    row!("open", 0, open_row, RowFlags::NONE, OPEN_NAMED),
    row!("comb", 0, comb_row, RowFlags::NONE, &["match", "close"]),
    row!("comb", 1, comb_row, RowFlags::ANY_ARGS, &["match", "close"]),
    row!("comb", 2, comb_row, RowFlags::ANY_ARGS, &["match", "close"]),
];

/// `IO::Path.slurp`.
// Cost: O(b), b = the file's size in bytes.
fn slurp_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_slurp(&ctx.attributes, &joined_args(args, named.pairs())))
}

/// `IO::Path.lines` and `lines($limit)`.
// Cost: O(1) (an open(2)) without a limit; O(bytes up to the limit-th line)
// with one.
fn lines_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_lines(&ctx.attributes, &joined_args(args, named.pairs())))
}

/// `IO::Path.words` and `words($limit)`.
// Cost: O(1) (an open(2)) without a limit; O(bytes up to the limit-th word)
// with one.
fn words_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_words(&ctx.attributes, &joined_args(args, named.pairs())))
}

/// `IO::Path.open`.
// Cost: O(p) plus one open(2), p = chars of the path.
fn open_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_open(&ctx.attributes, &joined_args(args, named.pairs())))
}

/// `IO::Path.comb`, `comb($matcher)` and `comb($matcher, $limit)`.
// Cost: O(b), b = the file's size in bytes, plus the matcher's own cost.
fn comb_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_comb(&ctx.attributes, &joined_args(args, named.pairs())))
}
