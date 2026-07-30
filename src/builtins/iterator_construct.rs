//! Pure `.iterator` instance construction for plain (non-`Seq`, non-`Iterator`)
//! receivers (`Range`/`Set`/`Bag`/`Mix`/`List`/`Array`/...). Builds an
//! `Iterator` Instance wrapping the receiver's materialized items plus a zero
//! index (and `is_lazy` / `known_count` flags), carrying no interpreter state
//! (env / registry / type metadata). The single authoritative implementation
//! shared by the bytecode VM's native dispatch and the tree-walking interpreter
//! fallback (1 operation = 1 implementation).
//!
//! `Seq` (consumed-state tracking + `squish` env mutation) and an already-built
//! `Iterator` Instance are handled by the caller, not here.
//!
//! Spec: https://docs.raku.org/routine/iterator

use std::collections::HashMap;

use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// The elements of a Buf/Blob receiver, or `None` when it is not one.
fn blob_elements(target: &Value) -> Option<Vec<Value>> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    if !crate::runtime::utils::is_native_elems_class(&class_name.resolve()) {
        return None;
    }
    Some(crate::value::value_buf::buf_elems_or_empty(&attributes))
}

/// Build the `Iterator` instance for a `.iterator` call on a plain receiver.
/// Mirrors the pure tail of `Interpreter::dispatch_iterator_method`.
pub(crate) fn build_iterator_instance(target: &Value) -> Value {
    let lazy = crate::builtins::methods_0arg::is_value_lazy(target);
    // A lazy list with a known logical element count (`42 xx 10**9`, `42 xx ∞`)
    // carries that count so `.count-only` can report it without materializing —
    // the cached `items` are only a bounded prefix.
    let known_count = match target.view() {
        ValueView::LazyList(ll) => ll.elems_count.clone(),
        _ => None,
    };
    let items = if crate::runtime::utils::is_shaped_array(target) {
        crate::runtime::utils::shaped_array_leaves(target)
    } else if let Some(bytes) = blob_elements(target) {
        // A Buf/Blob is an Instance holding its elements in a `bytes` attribute,
        // so `value_to_list` would see one opaque object. It iterates its elements
        // (`for Buf.new(1,2,3) { }` yields 1, 2, 3), so the iterator does too.
        bytes
    } else {
        crate::runtime::utils::value_to_list(target)
    };
    let mut attrs = HashMap::new();
    attrs.insert("items".to_string(), Value::array(items));
    attrs.insert("index".to_string(), Value::int(0));
    if lazy {
        attrs.insert("is_lazy".to_string(), Value::TRUE);
        // `items` above is only whatever prefix the source has produced so far —
        // for a `gather` that has never been forced, nothing at all. Keep the
        // source so the protocol methods can pull more on demand instead of
        // reporting the sentinel as though it were exhausted (see
        // `Interpreter::iterator_topup_from_lazy_source`).
        if matches!(target.view(), ValueView::LazyList(_)) {
            attrs.insert("lazy_source".to_string(), target.clone());
        }
    }
    if let Some(count) = known_count {
        attrs.insert("known_count".to_string(), count);
    }
    Value::make_instance(Symbol::intern("Iterator"), attrs)
}
