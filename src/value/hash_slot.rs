//! Hash element promotion: turning a hash's stored leaf into a first-class
//! `ContainerRef` cell that a binding can alias (Phase 2 Stage 1, ADR-0045).
//!
//! [`Value::hash_slot_ref`] promotes one element by key;
//! [`Value::hash_element_cells`] promotes every element in one pass over the
//! map, for the container-aware `.values`/`.kv`/`.pairs` producers. Both share
//! [`promote_leaf`], so a cell minted either way is indistinguishable.

use super::*;

/// The per-hash facts every promoted cell carries: the value constraint of a
/// typed hash, the name of the container the cell is an element of, and the
/// hash's `is default(...)` (what a `Nil` store through the cell decays to).
struct PromotionTag {
    value_type: Option<String>,
    owner: String,
    default: Option<Value>,
}

impl PromotionTag {
    fn of(data: &HashData) -> Self {
        // See `array_slot_ref` for why the owner name comes from the container
        // descriptor, with the bare sigil as the fallback.
        let owner = data
            .descriptor_name
            .as_deref()
            .filter(|n| n.starts_with('%'))
            .unwrap_or("%")
            .to_string();
        PromotionTag {
            value_type: data.value_type.clone(),
            owner,
            default: data.default.as_deref().cloned(),
        }
    }
}

/// Promote the stored leaf `elem` to a shared cell in place and return the
/// cell, or return the existing cell when it already is one.
// Cost: O(1).
fn promote_leaf(elem: &mut Value, tag: &PromotionTag) -> Value {
    if let ValueView::ContainerRef(cell) = elem.view() {
        return Value::ContainerRef(cell.clone());
    }
    let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(std::mem::replace(
        elem,
        Value::Nil,
    )));
    // A typed hash's value constraint rides on the promoted cell (see
    // `array_slot_ref`), named after the bare sigil until the binding site
    // names the container (`retag_element_owner`).
    if let Some(tc) = tag.value_type.as_deref() {
        crate::value::register_element_constraint(&cell, tc, &tag.owner);
    }
    if let Some(def) = &tag.default {
        cell.set_default(def.clone());
    }
    *elem = Value::ContainerRef(cell.clone());
    Value::ContainerRef(cell)
}

impl Value {
    /// Bind to hash element `key`, promoting it to a first-class container
    /// (Phase 2 Stage 1) — the hash analogue of [`array_slot_ref`](crate::value::Value::array_slot_ref). An existing
    /// *scalar* leaf is replaced in place with a shared `ContainerRef` cell
    /// (reusing one if already present), and that same cell is returned so the
    /// binding aliases the element by **cell identity**, surviving COW clones of
    /// any enclosing container on a later write (the staleness that the old
    /// `HashEntryRef` back-reference suffers for deep `%h<a><b>` paths).
    ///
    /// An existing *container* leaf (Array/Hash) is an intermediate level of a
    /// deeper path (`%h<a><b>`, `%h<a>[1]`); it keeps the old `HashEntryRef` so
    /// the deeper traversal resolves through the shared inner Arc and the
    /// eventual leaf promotion lands in the physical map the entry points to.
    /// A *missing* key stays lazy (no entry created) — promotion is deferred to
    /// the first write (a `HashEntryRef` token carries the path until then).
    ///
    /// Reads decontainerize at the single chokepoint (`resolve_hash_entry`);
    /// writes go through `hash_insert_through` (Stage 0).
    // Cost: O(k) expected, k = key length (one hash probe).
    pub fn hash_slot_ref(&self, key: &str, terminal: bool) -> Option<Value> {
        let ValueView::Hash(arc) = self.view() else {
            return None;
        };
        // SAFETY: aliased in-place mutation of a shared container; see
        // `gc_contents_mut`. No borrow into the map is live across the write.
        let data = unsafe { crate::value::gc_contents_mut(&arc) };
        let tag = PromotionTag::of(data);
        let Some(elem) = data.map.get_mut(key) else {
            return Some(Value::from_repr(ValueRepr::HashEntryRef {
                root: crate::value::EntryRoot::Hash(arc.clone()),
                path: vec![crate::value::EntryStep::Key(key.to_string())],
                eager: false,
            }));
        };
        if !terminal
            && !elem.is_container_ref()
            && matches!(elem.view(), ValueView::Array(..) | ValueView::Hash(..))
        {
            // Intermediate container: return the element value itself — it
            // shares the inner Arc, so the eventual leaf promotion by the next
            // index op lands in the physical map the entry points to (Stage 2:
            // no `HashEntryRef` back-reference needed).
            return Some(elem.clone());
        }
        Some(promote_leaf(elem, &tag))
    }

    /// Every element of this hash promoted to its own cell, in the map's
    /// iteration order (the order `keys()` yields), each passed with its key
    /// to `f` — the bulk form of `hash_slot_ref(key, true)` for every key.
    ///
    /// One pass over the map: no key is copied, re-hashed or re-probed, and
    /// the per-hash constraint and owner name are computed once rather than per
    /// element. `f` must not touch this hash (the map is mutably borrowed while
    /// it runs). `None` on a non-hash.
    // Cost: O(n), n = number of elements (at most one cell allocation each,
    // none for an already-promoted element).
    pub(crate) fn hash_element_cells<T>(
        &self,
        mut f: impl FnMut(&str, Value) -> T,
    ) -> Option<Vec<T>> {
        let ValueView::Hash(arc) = self.view() else {
            return None;
        };
        // SAFETY: aliased in-place mutation of a shared container; see
        // `gc_contents_mut`. Promotion replaces values in place and never
        // changes the map's structure, so the iteration is undisturbed.
        let data = unsafe { crate::value::gc_contents_mut(&arc) };
        let tag = PromotionTag::of(data);
        Some(
            data.map
                .iter_mut()
                .map(|(k, elem)| f(k, promote_leaf(elem, &tag)))
                .collect(),
        )
    }
}
