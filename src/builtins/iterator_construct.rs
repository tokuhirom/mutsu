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
use crate::value::Value;

/// Build the `Iterator` instance for a `.iterator` call on a plain receiver.
/// Mirrors the pure tail of `Interpreter::dispatch_iterator_method`.
pub(crate) fn build_iterator_instance(target: &Value) -> Value {
    let lazy = crate::builtins::methods_0arg::is_value_lazy(target);
    // A lazy list with a known logical element count (`42 xx 10**9`, `42 xx ∞`)
    // carries that count so `.count-only` can report it without materializing —
    // the cached `items` are only a bounded prefix.
    let known_count = match target {
        Value::LazyList(ll) => ll.elems_count.clone(),
        _ => None,
    };
    let items = if crate::runtime::utils::is_shaped_array(target) {
        crate::runtime::utils::shaped_array_leaves(target)
    } else {
        crate::runtime::utils::value_to_list(target)
    };
    let mut attrs = HashMap::new();
    attrs.insert("items".to_string(), Value::array(items));
    attrs.insert("index".to_string(), Value::Int(0));
    if lazy {
        attrs.insert("is_lazy".to_string(), Value::Bool(true));
    }
    if let Some(count) = known_count {
        attrs.insert("known_count".to_string(), count);
    }
    Value::make_instance(Symbol::intern("Iterator"), attrs)
}
