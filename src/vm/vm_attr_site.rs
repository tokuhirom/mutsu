//! ADR-0121 D3: the per-site inline cache of a `$!x` / `$.x` access in a
//! method body.
//!
//! The general resolution (`with_self_attr` in `vm_var_assign_computed_attr`)
//! finds the running method's owner, decides whether it is a role, picks the
//! storage key among the owner-qualified, bare and sigil-prefixed candidates,
//! and probes the attribute map for each. For an instance laid out by a
//! [`crate::value::ClassLayout`] the outcome is a fixed slot of that layout,
//! so the site remembers `(layout id, slot)` and the next access on an
//! instance of the same layout reads or writes the slot directly.
//!
//! The cache is only ever an answer the general resolution would give:
//!
//! - it is filled only from that resolution, and only when the choice of key
//!   cannot vary for other accesses on the same layout
//!   ([`crate::value::ClassLayout::site_cacheable_slot`]);
//! - a hit requires the same layout, no undeclared attribute in the instance
//!   (one could outrank the slot's key), a present slot, and a non-role owner
//!   (a role method on a mixin resolves against the role's own cell first).
//!
//! Anything else falls back to the general resolution, which refills the
//! cache.

use super::*;

impl Interpreter {
    /// The value of the attribute accessed at local slot `idx` of `code`, read
    /// through the site's cache, or `None` on a miss.
    // Cost: O(m), m = mixin layers of `self_val` (bounded by 8); O(1) for a
    // plain instance: one atomic load and one slot read under the read guard.
    pub(super) fn read_attr_site_cached(
        &self,
        code: &CompiledCode,
        idx: usize,
        self_val: &Value,
    ) -> Option<Value> {
        let (layout_id, slot) = code.attr_sites.cached(code.locals.len(), idx)?;
        if self.method_class_top_is_role() {
            return None;
        }
        let attributes = Self::self_instance_attrs(self_val)?;
        let map = attributes.as_map();
        if map.layout()?.id() != layout_id || map.has_undeclared() {
            return None;
        }
        map.slot(slot).map(Value::deref_container)
    }

    /// Store `val` into the attribute accessed at local slot `idx` of `code`
    /// through the site's cache. Hands `val` back on a miss.
    // Cost: O(m), m = mixin layers of `self_val` (bounded by 8); O(1) for a
    // plain instance: one atomic load and one slot store under the write lock.
    pub(super) fn write_attr_site_cached(
        &self,
        code: &CompiledCode,
        idx: usize,
        self_val: &Value,
        val: Value,
    ) -> Result<(), Value> {
        let Some((layout_id, slot)) = code.attr_sites.cached(code.locals.len(), idx) else {
            return Err(val);
        };
        if self.method_class_top_is_role() {
            return Err(val);
        }
        let Some(attributes) = Self::self_instance_attrs(self_val) else {
            return Err(val);
        };
        let key = attributes.store_slot_through(layout_id, slot, val)?;
        self.record_build_attr_write(&attributes, key);
        Ok(())
    }

    /// Remember, for the access at local slot `idx` of `code`, that the general
    /// resolution picked storage key `key` in `map` -- when `map` is laid out
    /// and that choice is one the cache may replay.
    // Cost: O(1).
    pub(super) fn fill_attr_site(
        code: &CompiledCode,
        idx: usize,
        map: &crate::value::AttrMap,
        key: crate::symbol::Symbol,
        bare: crate::symbol::Symbol,
        is_private: bool,
    ) {
        if map.has_undeclared() {
            return;
        }
        let Some(layout) = map.layout() else {
            return;
        };
        if let Some(slot) = layout.site_cacheable_slot(key, bare, is_private) {
            code.attr_sites
                .fill(code.locals.len(), idx, layout.id(), slot);
        }
    }
}
