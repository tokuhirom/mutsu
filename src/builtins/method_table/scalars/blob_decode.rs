//! `Blob.decode` and `Buf.decode` (ADR-11276 §8.3).
//!
//! An interpreter row: the encoding registry (`find_encoding`, the user's
//! registered encodings) and the newline mode are the interpreter's. The body
//! is [`Interpreter::decode_buf`], which the cascade's `dispatch_decode` also
//! ends in for a receiver with no shape, so there is one decoder; the pure
//! cascade copies (the 0- and 1-argument arms) and the callers' newline
//! translation of their answer are gone.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

/// `replacement` is the only named argument the row binds; `strict` is
/// accepted and ignored, as the cascade always did.
const NAMED: &[&str] = &["replacement", "strict"];

const fn row(owner: &'static str, arity: u8) -> MethodRow {
    MethodRow {
        owner,
        name: "decode",
        arity,
        handler: Handler::Interp(decode),
        // The encoding name is a plain `Str`; anything else is the cascade's.
        flags: RowFlags::NONE,
        named: NAMED,
    }
}

pub(super) static ROWS: &[MethodRow] =
    &[row("Blob", 0), row("Blob", 1), row("Buf", 0), row("Buf", 1)];

/// `.decode`, `.decode($encoding)` and either with `:replacement`.
// Cost: O(e), e = elements (copied out and decoded).
fn decode(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    interp.decode_buf(target, args.first(), named.get("replacement"))
}
