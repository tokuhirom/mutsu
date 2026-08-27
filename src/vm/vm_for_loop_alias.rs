//! ADR-0045: a `for` loop parameter binds the element *container*.
//!
//! Raku binds a `for` parameter to the item the iterator yields, and when the
//! source is a real mutable `Array` that item **is** the element's `Scalar`
//! container. The binding is therefore an alias with the lifetime of the
//! binding, not of the loop body: a closure or `start` block that outlives the
//! iteration still writes through, a read through the alias sees a write made
//! to the element by anyone else, and a direct `@a[i] = v` in the body is not
//! reverted afterwards.
//!
//! mutsu used to bind a plain value clone and copy it back once per iteration
//! (`write_back_for_rw_param`), rebuilding the entire backing `ArrayData` to
//! change one element — which is both the cause of five divergence classes
//! (ADR-0045 §1.3) and the reason a mutating `<->` loop was O(n²) (§1.5).
//!
//! This module holds the discriminator for **slice 1**: which `for` loop
//! sources may have their elements promoted to first-class containers via
//! [`Value::array_slot_ref`], the primitive ADR-0036 shipped and that
//! `:=`-bound elements already exercise daily. The bind site itself lives in
//! `vm_for_loop_body.rs`.

use super::*;

impl Interpreter {
    /// Whether a `for` loop's tagged `@`-source is a real, mutable, plain
    /// `Array` whose elements ADR-0045 slice 1 may promote to their own
    /// containers.
    ///
    /// The carve-outs (ADR-0045 §5 Q5) are deliberate and stay until slice 5
    /// decides otherwise:
    ///
    /// * a **shaped** array (`my @a[2;3]`) carries its dimensions in
    ///   `ArrayData::shape` / `ArrayKind::Shaped`, and the writeback path
    ///   deliberately preserves that metadata by cloning the whole `ArrayData`
    ///   (see `vm_loop_writeback.rs`'s "clone the original ArrayData" comment);
    ///   `array_slot_ref` has no such provision.
    /// * a **native-backed** array (`array[int]`, ADR-0015 P3b / ADR-0030)
    ///   keeps its elements in a packed `NativeBacking` payload, which cannot
    ///   hold a `ContainerRef` at all.
    /// * a **lazy** array must not be forced by a promotion.
    ///
    /// `t/cas-shaped-and-for-loop.t` and row 26 of `t/for-loop-element-alias.t`
    /// are the pins for the shaped case.
    pub(super) fn for_source_is_aliasable(&self, source: &str) -> bool {
        self.get_env_with_main_alias(source)
            .is_some_and(|raw| Self::array_is_aliasable(&raw.deref_container(), None))
    }

    /// The element container for index `idx` of a `for` loop's `@`-source
    /// (ADR-0045 slice 1). Promotion is idempotent — `array_slot_ref` returns
    /// an existing cell rather than allocating a second one — so re-looping the
    /// same array costs nothing after the first pass.
    ///
    /// `None` when the source is no longer an aliasable array, or the index is
    /// out of range: a body that shrank the source out from under the loop has
    /// no element left to alias, so the caller keeps the plain value bind (and
    /// its writeback) rather than letting `array_slot_ref` autovivify a fresh
    /// hole past the end.
    ///
    /// The source is resolved fresh on every call, on purpose: a body that
    /// reassigns it wholesale (`@a = 7, 8`) must have the remaining iterations
    /// alias the array it left behind, not the one the loop started with.
    pub(super) fn for_element_alias(&mut self, source: &str, idx: usize) -> Option<Value> {
        let arr = self.get_env_with_main_alias(source)?.deref_container();
        if !Self::array_is_aliasable(&arr, Some(idx)) {
            return None;
        }
        arr.array_slot_ref(idx, true)
    }

    /// Shared shape test for the two entry points above. `idx` additionally
    /// requires that index to exist today.
    fn array_is_aliasable(v: &Value, idx: Option<usize>) -> bool {
        match v.view() {
            ValueView::Array(data, kind) => {
                matches!(
                    kind,
                    crate::value::ArrayKind::Array | crate::value::ArrayKind::List
                ) && data.shape.is_none()
                    && data.native_storage_node().is_none()
                    && idx.is_none_or(|i| i < data.len())
            }
            _ => false,
        }
    }
}
