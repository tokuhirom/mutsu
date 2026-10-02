//! Pure `.iterator` instance construction for plain (non-`Seq`, non-`Iterator`)
//! receivers (`Range`/`Set`/`Bag`/`Mix`/`List`/`Array`/...). Builds an
//! `Iterator` Instance wrapping the receiver's materialized items plus a zero
//! index (and `is_lazy` / `known_count` flags) — or, for a lazy receiver, an
//! empty prefix plus the `lazy_source` it is pulled from on demand — carrying
//! no interpreter state
//! (env / registry / type metadata). The single authoritative implementation
//! shared by the bytecode VM's native dispatch and the tree-walking interpreter
//! fallback (1 operation = 1 implementation).
//!
//! `Seq` (consumed-state tracking + `squish` env mutation) and an already-built
//! `Iterator` Instance are handled by the caller, not here.
//!
//! Spec: <https://docs.raku.org/routine/iterator>

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
///
/// Cost: O(1) for a lazy receiver (nothing is reified); O(n) otherwise,
/// n = the receiver's elements.
pub(crate) fn build_iterator_instance(target: &Value) -> Value {
    let lazy = crate::builtins::methods_0arg::is_value_lazy(target);
    // A lazy list with a known logical element count (`42 xx 10**9`, `42 xx ∞`)
    // carries that count so `.count-only` can report it without materializing —
    // the cached `items` are only a bounded prefix.
    let known_count = match target.view() {
        ValueView::LazyList(ll) => ll.elems_count.clone(),
        _ => None,
    };
    // A lazy source is pulled on demand rather than materialized here: the
    // instance starts with an empty prefix and keeps the source as
    // `lazy_source`, which the protocol methods top up from as far as each call
    // needs (`Interpreter::iterator_topup_from_lazy_source`). An unbounded Range
    // is pulled through its `.succ`-stepping LazyList, so `(1..*).iterator` and
    // `("a"..*).iterator` are O(1) to build and never hit a reification cap
    // (#10782).
    let pull_source = if lazy {
        match target.view() {
            ValueView::LazyList(_) => Some(target.clone()),
            _ => crate::runtime::unbounded_range::lazy_list(target)
                .map(|ll| Value::lazy_list(crate::gc::Gc::new(ll))),
        }
    } else {
        None
    };
    let items = if pull_source.is_some() {
        Vec::new()
    } else if crate::runtime::utils::is_shaped_array(target) {
        crate::runtime::utils::shaped_array_leaves(target)
    } else if let Some(cells) = crate::runtime::Interpreter::array_element_cells(target) {
        // A real mutable Array's iterator yields its element CONTAINERS, as
        // rakudo's `ReifiedArrayIterator` does: `for` over a class whose
        // `iterator` is `@!x.iterator` aliases `@!x`'s elements, and
        // `my $x := @a.iterator.pull-one; $x = 5` writes `@a[0]` (#10350).
        // The same promotion `@a.values` hands out (ADR-0036 slice 3).
        cells
    } else if let ValueView::Array(arr, kind) = target.view()
        && kind.is_itemized()
    {
        // `.iterator` on an ITEMIZED array (`$[1,2,3].iterator`) still iterates
        // the array's elements — itemization only prevents flattening in list
        // context, which `value_to_list` below models by returning the array as
        // one opaque item (that made `pull-one` yield the whole array once;
        // Text::CSV's `CSV::Diag.iterator` returns `$[...].iterator`).
        arr.iter().cloned().collect()
    } else if let Some(bytes) = blob_elements(target) {
        // A Buf/Blob is an Instance holding its elements in a `bytes` attribute,
        // so `value_to_list` would see one opaque object. It iterates its elements
        // (`for Buf.new(1,2,3) { }` yields 1, 2, 3), so the iterator does too.
        bytes
    } else {
        // `target` is the RECEIVER of `.iterator`, and a method call
        // decontainerizes its invocant, so the receiver's own itemization (a
        // `$`-held / element-stored Hash's flag, a `Scalar` wrapper) must not
        // make it one opaque item: `my $s = {a => 1}; $s.iterator.pull-one` is
        // the Pair `:a(1)`, not the whole Hash. `value_to_list` answers "does
        // this flatten as an ELEMENT of another container" (ADR-0040), which is
        // a different question; the receiver-decomposition twin answers this one.
        crate::runtime::utils::value_to_list_for_receiver(target)
    };
    let mut attrs = HashMap::new();
    attrs.insert("items".to_string(), Value::array(items));
    attrs.insert("index".to_string(), Value::int(0));
    if lazy {
        attrs.insert("is_lazy".to_string(), Value::TRUE);
        if let Some(source) = pull_source {
            attrs.insert("lazy_source".to_string(), source);
        }
    }
    if let Some(count) = known_count {
        attrs.insert("known_count".to_string(), count);
    }
    Value::make_instance(Symbol::intern("Iterator"), attrs)
}
