//! `Blob`'s and `Buf`'s `AT-POS` and `EXISTS-POS` (ADR-11276 §9.107).
//!
//! Rakudo declares both on the `Blob` role for `int`/`Int:D` positions, so
//! every buffer class has them; any other index reaches `Any`'s candidates,
//! which [`RowFlags::INDEX_ARG`] binds (`method_table::index_arg`). A `Buf`
//! is resizable: a position past its end reads `0`, where a `Blob` raises
//! `X::OutOfRange`. A negative position is out of range for both.

use super::{Handler, MethodRow, RowFlags};
use crate::value::value_buf::{buf_len_or_zero, with_buf_elems};
use crate::value::{RuntimeError, Value, ValueView};

const fn row(
    owner: &'static str,
    name: &'static str,
    handler: crate::builtins::method_table::NarrowFn,
) -> MethodRow {
    MethodRow {
        owner,
        name,
        arity: 1,
        handler: Handler::Narrow(handler),
        flags: RowFlags::INDEX_ARG,
        named: &[],
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    row("Blob", "AT-POS", blob_at_pos),
    row("Buf", "AT-POS", buf_at_pos),
    row("Blob", "EXISTS-POS", exists_pos),
    row("Buf", "EXISTS-POS", exists_pos),
];

/// The position an `Int` index stands for; a big integer does not fit the
/// native `int` Rakudo's candidates bind it to.
// Cost: O(1).
fn position(arg: &Value) -> Result<i64, RuntimeError> {
    match arg.view() {
        ValueView::BigInt(n) => Err(RuntimeError::new(format!(
            "Cannot unbox {} bit wide bigint into native integer. Did you mix int and Int or literals?",
            n.bits()
        ))),
        _ => Ok(arg.as_int().unwrap_or(0)),
    }
}

/// The element at `pos`, `past_end` reading what a position after the last
/// one holds (`Some(0)` for a resizable `Buf`, `None` to raise).
// Cost: O(1).
fn at_pos(
    target: &Value,
    args: &[Value],
    past_end: Option<i64>,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let pos = match position(args.first()?) {
        Ok(pos) => pos,
        Err(e) => return Some(Err(e)),
    };
    let elems = buf_len_or_zero(&attributes);
    if pos >= 0
        && let Some(item) =
            with_buf_elems(&attributes, |items| items.get(pos as usize).cloned()).flatten()
    {
        return Some(Ok(item));
    }
    match past_end {
        Some(fill) if pos >= 0 => Some(Ok(Value::int(fill))),
        _ => Some(Err(RuntimeError::out_of_range(
            "Index",
            Value::int(pos),
            &format!("0..{}", elems as i64 - 1),
        ))),
    }
}

/// `Blob.AT-POS`: the element, and `X::OutOfRange` outside the buffer.
// Cost: O(1).
fn blob_at_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    at_pos(target, args, None)
}

/// `Buf.AT-POS`: the element, `0` past the end, and `X::OutOfRange` for a
/// negative position.
// Cost: O(1).
fn buf_at_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    at_pos(target, args, Some(0))
}

/// `EXISTS-POS`: whether the position holds an element.
// Cost: O(1).
fn exists_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let pos = match position(args.first()?) {
        Ok(pos) => pos,
        Err(e) => return Some(Err(e)),
    };
    let elems = buf_len_or_zero(&attributes) as i64;
    Some(Ok(Value::truth((0..elems).contains(&pos))))
}
