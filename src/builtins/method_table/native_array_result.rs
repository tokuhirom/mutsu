//! A native array's order-preserving methods answer a native array.
//!
//! `my int @a = 3,1,2,1; @a.sort` is an `array[int]` in Rakudo (`sort`,
//! `squish`, `reverse`, `unique`, `head`, `tail`, `rotate` rebuild the receiver's
//! own type), where the same methods on an `Int @` or an untyped array answer a
//! `Seq`. The rows compute the elements as a `Seq` either way; this retypes that
//! answer to the receiver's native array type, in the one place every row
//! answer passes through ([`super::dispatch::call`]).

use crate::value::{ArrayData, ArrayKind, Value, ValueView};

/// Methods whose answer keeps the receiver's native element type.
const RETYPING: &[&str] = &[
    "sort", "squish", "reverse", "unique", "head", "tail", "rotate",
];

/// Whether `method` is one of the methods [`retype`] may retype.
// Cost: O(1), a scan of seven names.
pub(crate) fn retypes(method: &str) -> bool {
    RETYPING.contains(&method)
}

/// Whether `elem` names a native element type (`int`, `uint8`, `num`, `str`).
fn is_native_elem(elem: &str) -> bool {
    crate::native_types::is_native_int_type(elem)
        || matches!(elem, "num" | "num32" | "num64" | "str" | "uint")
        || crate::value::value_buf::native_elem_type(elem).is_some()
}

/// `answer` of `method` called on `receiver`, as a native array of the
/// receiver's type when `receiver` is one and `answer` is a settled `Seq`.
// Cost: O(1) for a receiver that is not a native array or a method that does
// not retype; O(n) otherwise, n = elements of the answer.
pub(crate) fn retype(receiver: &Value, method: &str, answer: Value) -> Value {
    if !retypes(method) {
        return answer;
    }
    let receiver = receiver.deref_container();
    let ValueView::Array(data, ArrayKind::Array) = receiver.view() else {
        return answer;
    };
    let Some(elem) = data.value_type.as_deref().filter(|e| is_native_elem(e)) else {
        return answer;
    };
    let ValueView::Seq(body) = answer.view() else {
        return answer;
    };
    // Only an eagerly produced answer: a deferred source would need pulling.
    if !body.is_settled_for_rows() {
        return answer;
    }
    let mut out = ArrayData::new(body.live_generation().to_vec());
    out.value_type = data.value_type.clone();
    out.key_type = data.key_type.clone();
    out.declared_type = data.declared_type.clone();
    out.default = data.default.clone();
    if data.native_storage_node().is_some() {
        out.promote_native_storage(elem);
    }
    Value::array_with_kind(crate::gc::Gc::new(out), ArrayKind::Array)
}
