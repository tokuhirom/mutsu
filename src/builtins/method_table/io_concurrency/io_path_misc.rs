//! `IO::Path`'s remaining rows (ADR-11276 §9.19): `Numeric` (the basename as a
//! number), `child` (with `:secure`, which resolves against the filesystem),
//! `resolve`, `dir` and `watch`.

use super::io_path_ctx::{PathCtx, joined_args};
use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

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
    row!("Numeric", 0, numeric_row, RowFlags::NONE, &[]),
    row!("child", 1, child_row, RowFlags::ANY_ARGS, &["secure"]),
    row!("resolve", 0, resolve_row, RowFlags::NONE, &["completely"]),
    row!("dir", 0, dir_row, RowFlags::NONE, &["test"]),
    row!("watch", 0, watch_row, RowFlags::NONE, &[]),
];

/// `IO::Path.Numeric`: the basename as a number, or a `Failure`.
// Cost: O(p), p = chars of the path.
fn numeric_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_numeric(&ctx.attributes))
}

/// `IO::Path.child($name, :secure)`.
// Cost: O(p + n), p = chars of the path, n = chars of the name, plus the
// filesystem walk of `:secure`.
fn child_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_child(
        &ctx.attributes,
        ctx.class,
        &joined_args(args, named.pairs()),
    ))
}

/// `IO::Path.resolve(:completely)`.
// Cost: O(p) plus one filesystem query per path segment, p = path length.
fn resolve_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_resolve(&ctx.attributes, ctx.class, named.pairs()))
}

/// `IO::Path.dir(:test)`.
// Cost: O(n), n = directory entries (plus the `test` smartmatch per entry).
fn dir_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_dir(&ctx.attributes, ctx.class, named.pairs()))
}

/// `IO::Path.watch`: a Supply of the changes under the path.
// Cost: O(1) here; the watcher thread pays O(n) per poll, n = entries of the
// watched directory (1 for a file).
fn watch_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_watch(&ctx.attributes))
}
