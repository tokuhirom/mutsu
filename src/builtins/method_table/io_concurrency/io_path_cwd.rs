//! `IO::Path`'s cwd rows (ADR-11276 §9.19): the methods that derive a string
//! from the receiver's path and the current working directory, which the
//! interpreter owns (`$*CWD`, the instance's own `CWD`, a chroot). None touches
//! the filesystem.

use super::io_path_ctx::PathCtx;
use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

macro_rules! row {
    ($name:literal, $arity:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: "IO::Path",
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("absolute", 0, absolute_row, RowFlags::NONE),
    row!("absolute", 1, absolute_row, RowFlags::ANY_ARGS),
    row!("relative", 0, relative_row, RowFlags::NONE),
    row!("relative", 1, relative_row, RowFlags::ANY_ARGS),
    row!("CWD", 0, cwd_row, RowFlags::NONE),
    row!("raku", 0, raku_row, RowFlags::NONE),
];

/// `IO::Path.absolute` and `absolute($base)`.
// Cost: O(p), p = chars of the path.
fn absolute_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_absolute(&ctx.attributes, args))
}

/// `IO::Path.relative` and `relative($base)`.
// Cost: O(p + b), p = chars of the path, b = chars of the base.
fn relative_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_relative(&ctx.attributes, args))
}

/// `IO::Path.CWD`: the directory the path is relative to.
// Cost: O(1).
fn cwd_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(Value::str(interp.io_path_cwd_of(&ctx.attributes))))
}

/// `IO::Path.raku`: the `.new` call that rebuilds the path.
// Cost: O(p), p = chars of the path.
fn raku_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(Value::str(
        interp.io_path_raku(ctx.class.as_str(), &ctx.attributes),
    )))
}
