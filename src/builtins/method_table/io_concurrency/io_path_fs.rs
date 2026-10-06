//! `IO::Path`'s filesystem-mutation rows (ADR-11276 §9.19): the methods that
//! change the filesystem with one syscall (`spurt`, `mkdir`, `rmdir`, `unlink`,
//! `chmod`, `chown`) and the two-path operations (`copy`, `rename`, `move`,
//! `symlink`, `link`). They resolve their paths against the cwd and allocate no
//! `io_handles` entry.

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
    row!(
        "spurt",
        1,
        spurt_row,
        RowFlags::ANY_ARGS,
        &["append", "createonly", "enc"]
    ),
    row!("mkdir", 0, mkdir_row, RowFlags::NONE, &[]),
    row!("mkdir", 1, mkdir_row, RowFlags::ANY_ARGS, &[]),
    row!("rmdir", 0, rmdir_row, RowFlags::NONE, &[]),
    row!("unlink", 0, unlink_row, RowFlags::NONE, &[]),
    row!("chmod", 1, chmod_row, RowFlags::ANY_ARGS, &[]),
    row!("chown", 0, chown_row, RowFlags::NONE, &["uid", "gid"]),
    row!("copy", 0, copy_row, RowFlags::NONE, &["createonly"]),
    row!("copy", 1, copy_row, RowFlags::ANY_ARGS, &["createonly"]),
    row!("rename", 0, rename_row, RowFlags::NONE, &["createonly"]),
    row!("rename", 1, rename_row, RowFlags::ANY_ARGS, &["createonly"]),
    row!("move", 0, move_row, RowFlags::NONE, &["createonly"]),
    row!("move", 1, move_row, RowFlags::ANY_ARGS, &["createonly"]),
    row!("symlink", 0, symlink_row, RowFlags::NONE, &["absolute"]),
    row!("symlink", 1, symlink_row, RowFlags::ANY_ARGS, &["absolute"]),
    row!("link", 0, link_row, RowFlags::NONE, &[]),
    row!("link", 1, link_row, RowFlags::ANY_ARGS, &[]),
];

/// `IO::Path.spurt($content, :append, :createonly, :enc)`.
// Cost: O(p + c), p = path length, c = content bytes written.
fn spurt_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(interp.io_path_spurt(
        &ctx.attributes,
        &joined_args(args, named.pairs()),
    )))
}

/// `IO::Path.mkdir` and `mkdir($mode)`: the path itself, or a `Failure`.
// Cost: O(p) plus one mkdir(2) per missing parent, p = path length.
fn mkdir_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(interp.io_path_mkdir(&ctx.attributes, args, ctx.same())))
}

/// `IO::Path.rmdir`.
// Cost: O(p) plus one rmdir(2), p = path length.
fn rmdir_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(interp.io_path_rmdir(&ctx.attributes)))
}

/// `IO::Path.unlink`.
// Cost: O(p) plus one unlink(2), p = path length.
fn unlink_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(Ok(interp.io_path_unlink(&ctx.attributes)))
}

/// `IO::Path.chmod($mode)`.
// Cost: O(p) plus one chmod(2), p = path length.
fn chmod_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_chmod(&ctx.attributes, args))
}

/// `IO::Path.chown(:uid, :gid)`.
// Cost: O(p) plus one chown(2), p = path length.
fn chown_row(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_chown(&ctx.attributes, named.pairs()))
}

/// `IO::Path.copy($dest, :createonly)`.
// Cost: O(p + d + b), p, d = path lengths, b = the file's size in bytes.
fn copy_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_copy_or_move(&ctx.attributes, "copy", &joined_args(args, named.pairs())))
}

/// `IO::Path.rename($dest, :createonly)`.
// Cost: O(p + d) plus one rename(2), p, d = path lengths.
fn rename_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_copy_or_move(&ctx.attributes, "rename", &joined_args(args, named.pairs())))
}

/// `IO::Path.move($dest, :createonly)`.
// Cost: O(p + d) plus one rename(2), or a copy across devices, p, d = path
// lengths.
fn move_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_copy_or_move(&ctx.attributes, "move", &joined_args(args, named.pairs())))
}

/// `IO::Path.symlink($name, :absolute)`.
// Cost: O(p + n) plus one symlink(2), p, n = path lengths.
fn symlink_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_symlink(&ctx.attributes, &joined_args(args, named.pairs())))
}

/// `IO::Path.link($name)`.
// Cost: O(p + n) plus one link(2), p, n = path lengths.
fn link_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let ctx = PathCtx::of(target)?;
    Some(interp.io_path_link(&ctx.attributes, args))
}
