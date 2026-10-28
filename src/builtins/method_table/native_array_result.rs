//! A native array's order-preserving methods answer a native array.
//!
//! `my int @a = 3,1,2,1; @a.sort` is an `array[int]` in Rakudo (`sort`,
//! `squish`, `reverse`, `unique`, `head`, `tail`, `rotate` rebuild the receiver's
//! own type), where the same methods on an `Int @` or an untyped array answer a
//! `Seq`. The rows compute the elements as a `Seq` either way; this retypes that
//! answer to the receiver's native array type. A native array's `values` is a
//! plain `List`. It runs where a row answers ([`super::dispatch::call`], which
//! the method-site lane uses) and at the VM's `CallMethod` sites, since the full
//! path reaches the same elements through producers that never pass a row.

use crate::value::{ArrayData, ArrayKind, Value, ValueView};

/// Methods whose answer keeps the receiver's native element type.
const RETYPING: &[&str] = &[
    "sort", "squish", "reverse", "unique", "head", "tail", "rotate", "values",
];

/// Whether `method` is one of the methods [`retype`] may retype.
// Cost: O(1), a scan of eight names.
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
    // `values` is the elements as a plain `List`, not a native array; they are
    // the receiver's own, whatever (possibly lazy) view the full path answered.
    if method == "values" {
        let items = data.items().iter().map(|v| v.deref_container().clone());
        return Value::array_with_kind(
            crate::gc::Gc::new(ArrayData::new(items.collect())),
            ArrayKind::List,
        );
    }
    // The answer is a `Seq` or, when a native fast path built it, a `List`.
    let items = match answer.view() {
        // Only an eagerly produced answer: a deferred source would need pulling.
        // (An element producer's `Seq` holds the receiver's cells: a native
        // array has no cells to share, so the answer takes their values.)
        ValueView::Seq(body) if body.holds_eager_elements() => body
            .live_generation()
            .iter()
            .map(|item| item.deref_container().clone())
            .collect(),
        ValueView::Array(list, ArrayKind::List) => list.items().to_vec(),
        _ => return answer,
    };
    let mut out = ArrayData::new(items);
    out.value_type = data.value_type.clone();
    out.key_type = data.key_type.clone();
    out.declared_type = data.declared_type.clone();
    out.default = data.default.clone();
    if data.native_storage_node().is_some() {
        out.promote_native_storage(elem);
    }
    Value::array_with_kind(crate::gc::Gc::new(out), ArrayKind::Array)
}
