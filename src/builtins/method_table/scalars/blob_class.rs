//! `Blob`'s and `Buf`'s `allocate`, `encoding` and `chars` (ADR-11276 §9.163),
//! which `call_method_with_values` answered by name for any buffer class.
//!
//! As in `blob.rs`, each row is registered once for `Blob` and once for `Buf`
//! (Rakudo composes the `Blob` role into both, and into the encoding buffers,
//! which share `Blob`'s shape).

use super::{Handler, MethodRow, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! both {
    ($name:literal, $arity:literal, $flags:expr, $handler:ident) => {
        [
            MethodRow {
                owner: "Blob",
                name: $name,
                arity: $arity,
                handler: Handler::Narrow($handler),
                flags: $flags,
                named: &[],
            },
            MethodRow {
                owner: "Buf",
                name: $name,
                arity: $arity,
                handler: Handler::Narrow($handler),
                flags: $flags,
                named: &[],
            },
        ]
    };
}

/// The fill may be any value, a buffer included (`Buf.allocate(3, $blob)`).
const ALLOCATE: RowFlags = RowFlags::TYPE_OBJECT_OK
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::OBJECT_ARGS);

pub(super) static ROWS: &[MethodRow] = &{
    let [a1, b1] = both!("allocate", 1, ALLOCATE, allocate);
    let [a2, b2] = both!("allocate", 2, ALLOCATE, allocate);
    let [e1, e2] = both!("encoding", 0, RowFlags::TYPE_OBJECT_OK, encoding);
    let [c1, c2] = both!("chars", 0, RowFlags::NONE, chars);
    [a1, b1, a2, b2, e1, e2, c1, c2]
};

/// `Blob.allocate($elems, $fill?)`: a buffer of the type object's class with
/// `$elems` elements, zero or the fill (a value, or a list or buffer cycled
/// through). Every candidate is `Blob:U:`, so an instance matches none.
// Cost: O(n), n = the elements allocated.
fn allocate(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Package(name) = target.view() else {
        return Some(Err(
            crate::runtime::methods_signature_errors::make_multi_no_match_error("allocate"),
        ));
    };
    Some(Interpreter::buf_allocate(name, args))
}

/// `Blob.encoding`: the encoding of an encoding buffer (`utf8` is `utf-8`),
/// `Any` for every other buffer, type object or instance.
// Cost: O(1).
fn encoding(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let class = match target.view() {
        ValueView::Package(name) => name,
        ValueView::Instance { class_name, .. } => class_name,
        _ => return None,
    };
    Some(Ok(match class.as_str() {
        "utf8" => Value::str("utf-8".to_string()),
        "utf16" => Value::str("utf-16".to_string()),
        _ => Value::package(crate::symbol::wk::any()),
    }))
}

/// `Blob.chars`: a buffer is not a string, so it throws `X::Buf::AsStr`.
// Cost: O(1).
fn chars(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Err(Interpreter::buf_as_str_error(target, "chars")))
}
