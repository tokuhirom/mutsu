//! The lexical-store fallbacks of `atomic_scalar_cell`
//! (`builtins_atomic_shared.rs`): an atomic op on a scalar that is no local of
//! the running frame boxes its binding in the package-block lexical store
//! (`package_lexicals`) or in the running routine's compunit-level store
//! (`unit_lexicals`) into a shared cell, so every alias shares one binding.

use super::*;

impl Interpreter {
    /// [`Self::box_package_scope_lexical_cell`] for the running routine's
    /// compunit-level lexical (`unit_lexical_slot`).
    // Cost: O(1) amortized (one unit-lexical store probe and, the first time,
    // one replace).
    pub(super) fn box_unit_scope_lexical_cell(
        &mut self,
        name: &str,
    ) -> Option<crate::gc::Gc<crate::value::ContainerCell>> {
        let cur = self.unit_lexical_slot(name)?.clone();
        if let ValueView::ContainerRef(c) = cur.view() {
            return Some(c.clone());
        }
        if refuses_atomic_cell_shape(&cur) {
            return None;
        }
        let container = cur.into_container_ref();
        if !self.unit_scope_lexical_bind(name, &container) {
            return None;
        }
        match container.view() {
            ValueView::ContainerRef(c) => Some(c.clone()),
            _ => None,
        }
    }

    /// [`Self::atomic_scalar_cell`]'s fallback for a package-scoped `my`
    /// lexical (a class-body "static") that has no frame-local slot in the
    /// currently executing chunk. See the call site for the full rationale.
    pub(super) fn box_package_scope_lexical_cell(
        &mut self,
        bare: &str,
    ) -> Option<crate::gc::Gc<crate::value::ContainerCell>> {
        let pkg = self.current_package();
        if pkg.is_empty() || pkg == "GLOBAL" {
            return None;
        }
        let cur = self.package_lexicals.get(&pkg)?.get(bare)?.clone();
        if let ValueView::ContainerRef(c) = cur.view() {
            return Some(c.clone());
        }
        if refuses_atomic_cell_shape(&cur) {
            return None;
        }
        let container = cur.into_container_ref();
        self.package_lexicals_cow_mut()
            .get_mut(&pkg)?
            .insert(bare.to_string(), container.clone());
        match container.view() {
            ValueView::ContainerRef(c) => Some(c.clone()),
            _ => None,
        }
    }
}

/// Whether an atomic op leaves `cur` unboxed. Only plain scalar contents are
/// boxed; reference types already share, and hiding a type object or Proxy
/// behind a `ContainerRef` trips the paths that do not deref one. `Any` is the
/// uninitialized-scalar seed and is boxed like a value (mirrors
/// `box_captured_lexicals`, including its Seq/HyperSeq/RaceSeq/Slip exclusion
/// -- `news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md`).
// Cost: O(1).
fn refuses_atomic_cell_shape(cur: &Value) -> bool {
    !cur.is_any_type_object()
        && matches!(
            cur.view(),
            ValueView::Package(_)
                | ValueView::Array(..)
                | ValueView::Hash(..)
                | ValueView::Sub(..)
                | ValueView::Instance { .. }
                | ValueView::Proxy { .. }
                | ValueView::Seq(..)
                | ValueView::HyperSeq(..)
                | ValueView::RaceSeq(..)
                | ValueView::Slip(..)
        )
}
