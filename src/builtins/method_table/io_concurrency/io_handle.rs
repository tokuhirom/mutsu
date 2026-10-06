//! `IO::Handle`'s rows (ADR-11276 §9.20): the methods of an open file, a
//! standard stream or a pipe end. A handle's state is in the interpreter's
//! handle table, so each row is an interpreter row; its body is one
//! `Interpreter::io_handle_*` method, which the rows share with every layer
//! that used to carry a copy (the VM's per-method fast paths are gone).

use super::io_path_ctx::joined_args;
use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

/// The open adverbs `IO::Handle.open` binds.
const OPEN_NAMED: &[&str] = &[
    "r",
    "w",
    "x",
    "a",
    "update",
    "rw",
    "rx",
    "ra",
    "mode",
    "create",
    "append",
    "truncate",
    "exclusive",
    "bin",
    "enc",
    "chomp",
    "nl-in",
    "nl-out",
    "out-buffer",
];

macro_rules! row {
    ($name:literal, $arity:literal, $handler:ident, $flags:expr, $named:expr) => {
        MethodRow {
            owner: "IO::Handle",
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: $named,
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("open", 0, open_row, RowFlags::OWNER_ONLY, OPEN_NAMED),
    row!("DESTROY", 0, destroy_row, RowFlags::NONE, &[]),
    row!("path", 0, path_row, RowFlags::NONE, &[]),
    row!("IO", 0, path_row, RowFlags::NONE, &[]),
    row!("Str", 0, str_row, RowFlags::NONE, &[]),
    row!("gist", 0, gist_row, RowFlags::NONE, &[]),
    row!("raku", 0, raku_row, RowFlags::NONE, &[]),
    row!("nl-out", 0, nl_out_row, RowFlags::NONE, &[]),
    row!("nl-out", 1, nl_out_row, RowFlags::ANY_ARGS, &[]),
    row!("nl-in", 0, nl_in_row, RowFlags::NONE, &[]),
    row!("nl-in", 1, nl_in_row, RowFlags::ANY_ARGS, &[]),
    row!("chomp", 0, chomp_row, RowFlags::NONE, &[]),
    row!("chomp", 1, chomp_row, RowFlags::ANY_ARGS, &[]),
    row!("out-buffer", 0, out_buffer_row, RowFlags::NONE, &[]),
    row!("out-buffer", 1, out_buffer_row, RowFlags::ANY_ARGS, &[]),
    row!("encoding", 0, encoding_row, RowFlags::NONE, &[]),
    row!("encoding", 1, encoding_row, RowFlags::ANY_ARGS, &[]),
    row!("close", 0, close_row, RowFlags::NONE, &[]),
    row!("flush", 0, flush_row, RowFlags::NONE, &[]),
    row!("tell", 0, tell_row, RowFlags::NONE, &[]),
    row!("eof", 0, eof_row, RowFlags::NONE, &[]),
    row!("t", 0, t_row, RowFlags::NONE, &[]),
    row!("opened", 0, opened_row, RowFlags::NONE, &[]),
    row!(
        "native-descriptor",
        0,
        native_descriptor_row,
        RowFlags::NONE,
        &[]
    ),
    row!("seek", 1, seek_row, RowFlags::ANY_ARGS, &[]),
    row!("seek", 2, seek_row, RowFlags::ANY_ARGS, &[]),
    row!(
        "lock",
        0,
        lock_row,
        RowFlags::NONE,
        &["shared", "non-blocking"]
    ),
    row!("unlock", 0, unlock_row, RowFlags::NONE, &[]),
    row!("get", 0, get_row, RowFlags::NONE, &[]),
    row!("getc", 0, getc_row, RowFlags::NONE, &[]),
    row!("readchars", 0, readchars_row, RowFlags::NONE, &[]),
    row!("readchars", 1, readchars_row, RowFlags::ANY_ARGS, &[]),
    row!("lines", 0, lines_row, RowFlags::NONE, &["close", "chomp"]),
    row!(
        "lines",
        1,
        lines_row,
        RowFlags::ANY_ARGS,
        &["close", "chomp"]
    ),
    row!("words", 0, words_row, RowFlags::NONE, &["close"]),
    row!("words", 1, words_row, RowFlags::ANY_ARGS, &["close"]),
    row!("read", 0, read_row, RowFlags::NONE, &[]),
    row!("read", 1, read_row, RowFlags::ANY_ARGS, &[]),
    row!(
        "slurp",
        0,
        slurp_row,
        RowFlags::NONE,
        &["close", "bin", "enc"]
    ),
    row!(
        "slurp-rest",
        0,
        slurp_row,
        RowFlags::NONE,
        &["close", "bin", "enc"]
    ),
    row!("Supply", 0, supply_row, RowFlags::NONE, &["size"]),
    row!("print-nl", 0, print_nl_row, RowFlags::NONE, &[]),
    row!("write", 1, write_row, RowFlags::ANY_ARGS, &[]),
    row!("spurt", 1, spurt_row, RowFlags::ANY_ARGS, &["close"]),
    row!(
        "split",
        0,
        split_row,
        RowFlags::ANY_ARGS.or(RowFlags::SLURPY),
        &["close"]
    ),
    row!(
        "comb",
        0,
        comb_row,
        RowFlags::ANY_ARGS.or(RowFlags::SLURPY),
        &["close"]
    ),
    row!(
        "print",
        0,
        print_row,
        RowFlags::ANY_ARGS.or(RowFlags::SLURPY),
        &[]
    ),
    row!(
        "put",
        0,
        put_row,
        RowFlags::ANY_ARGS.or(RowFlags::SLURPY),
        &[]
    ),
    row!(
        "say",
        0,
        say_row,
        RowFlags::ANY_ARGS.or(RowFlags::SLURPY),
        &[]
    ),
    row!(
        "printf",
        0,
        printf_row,
        RowFlags::ANY_ARGS.or(RowFlags::SLURPY),
        &[]
    ),
];

/// `IO::Handle.destroy`.
// Cost: O(b), b = buffered bytes flushed by the close.
fn destroy_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_destroy(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.path`.
// Cost: O(p), p = chars of the path.
fn path_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_path(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.str`.
// Cost: O(p), p = chars of the path.
fn str_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_str(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.gist`.
// Cost: O(p), p = chars of the path.
fn gist_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_gist(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.raku`.
// Cost: O(p + a), p = chars of the path, a = attributes of the handle.
fn raku_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_raku(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.nl-out`.
// Cost: O(1).
fn nl_out_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_nl_out(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.nl-in`.
// Cost: O(s), s = separators.
fn nl_in_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_nl_in(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.chomp`.
// Cost: O(1).
fn chomp_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_chomp(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.out-buffer`.
// Cost: O(1).
fn out_buffer_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_out_buffer(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.encoding`.
// Cost: O(1).
fn encoding_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_encoding(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.close`.
// Cost: O(b), b = buffered bytes flushed.
fn close_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_close(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.flush`.
// Cost: O(b), b = buffered bytes flushed.
fn flush_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_flush(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.tell`.
// Cost: O(1).
fn tell_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_tell(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.eof`.
// Cost: O(1) (one peek of the read buffer).
fn eof_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_eof(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.t`.
// Cost: O(1).
fn t_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_t(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.opened`.
// Cost: O(1).
fn opened_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_opened(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.native-descriptor`.
// Cost: O(1).
fn native_descriptor_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_native_descriptor(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.seek`.
// Cost: O(1) plus the flush of buffered output.
fn seek_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_seek(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.lock`.
// Cost: O(1), one flock(2) (blocking unless :non-blocking).
fn lock_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_lock(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.unlock`.
// Cost: O(1).
fn unlock_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_unlock(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.get`.
// Cost: O(l), l = bytes of the line.
fn get_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_get(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.getc`.
// Cost: O(1), one grapheme.
fn getc_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_getc(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.readchars`.
// Cost: O(b), b = bytes of the rest of the file.
fn readchars_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_readchars(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.lines`.
// Cost: O(1) (a lazy line reader).
fn lines_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_lines(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.words`.
// Cost: O(1) (a lazy word reader).
fn words_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_words(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.read`.
// Cost: O(b), b = bytes of the rest of the file.
fn read_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_read(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.slurp`.
// Cost: O(b), b = bytes of the rest of the file.
fn slurp_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_slurp(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.supply`.
// Cost: O(b), b = bytes of the rest of the file.
fn supply_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_supply(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.print-nl`.
// Cost: O(n), n = chars of the line separator.
fn print_nl_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_print_nl(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.write`.
// Cost: O(b), b = bytes written.
fn write_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_write(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.spurt`.
// Cost: O(b), b = bytes written.
fn spurt_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_spurt(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.split`.
// Cost: O(b + m), b = bytes of the rest of the file, m = the matcher's own cost.
fn split_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_split(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.comb`.
// Cost: O(b + m), b = bytes of the rest of the file, m = the matcher's own cost.
fn comb_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_comb(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.print`.
// Cost: O(r), r = chars rendered from the arguments.
fn print_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_print(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.put`.
// Cost: O(r), r = chars rendered from the arguments.
fn put_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_put(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.say`.
// Cost: O(r), r = chars rendered from the arguments.
fn say_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_say(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.printf`.
// Cost: O(f + r), f = chars of the format, r = chars rendered from the arguments.
fn printf_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_printf(target, &joined_args(args, named.pairs())))
}

/// `IO::Handle.open`: opens the handle named by its `path` attribute (or applies
/// the options in place to a live stream) and answers the handle.
// Cost: O(p) plus one open(2), p = chars of the path.
fn open_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    matches!(target.view(), ValueView::Instance { .. })
        .then(|| interp.io_handle_open(target, &joined_args(args, named.pairs())))
}
