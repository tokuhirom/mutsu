//! The holder flavour of an `=`-shared element (ADR-0079 slice 3).
//!
//! `@a[0] = %h` / `%x<k> = @row` is compiled as a `:=` bind that installs the
//! source's own `ContainerRef` cell in the element (ADR "slice 2b"), so the
//! element and the source variable alias one container — which is raku's
//! semantics: the element's `Scalar` holds the very `Hash`/`Array` object.
//! But an `Array` or `Hash` element *is* a `Scalar` container in raku
//! (`@a[0].VAR.^name` is `Scalar`), so it itemizes what it holds
//! (`@a[0].raku` is `${:a(1)}`), while the source `%h` itself does not.
//!
//! Itemization is a per-holder property (ADR-0079 §2), so it is recorded on
//! the element's own copy of the `ContainerRef` word, never in the shared
//! cell: [`Value::itemize_shared_element`] retags that one word as
//! `ContainerRefItemized` and leaves the source's word plain.

use super::*;

impl Value {
    /// Retag the element of this `Array`/`Hash` at `key` (a decimal index for
    /// an array, the hash key otherwise) as an itemized holder when it is a
    /// plain `ContainerRef` — the shape an `=`-element share leaves behind.
    /// Anything else (a missing element, a value, an already itemized word)
    /// is left untouched.
    // Cost: O(|key|) — one index parse or one hash probe.
    pub(crate) fn itemize_shared_element(&self, key: &str) {
        let retag = |slot: &mut Value| {
            if let ValueView::ContainerRef(cell) = slot.view()
                && !slot.container_ref_is_itemized()
            {
                *slot = Value::container_ref_itemized((*cell).clone());
            }
        };
        match self.view() {
            ValueView::Array(arc, _) => {
                let Ok(idx) = key.parse::<usize>() else {
                    return;
                };
                // SAFETY: aliased in-place mutation of a shared container; see
                // `gc_contents_mut`. Only one slot's word is replaced, and no
                // borrow into the items is live across it.
                let data = unsafe { crate::value::gc_contents_mut(&arc) };
                if idx < data.len() {
                    retag(&mut data[idx]);
                }
            }
            ValueView::Hash(arc) => {
                // SAFETY: as above; the map's structure is unchanged.
                let data = unsafe { crate::value::gc_contents_mut(&arc) };
                if let Some(slot) = data.map.get_mut(key) {
                    retag(slot);
                }
            }
            _ => {}
        }
    }
}
