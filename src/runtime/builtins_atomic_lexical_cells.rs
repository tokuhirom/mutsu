//! `atomic_scalar_cell`: the shared cell an atomic op on a scalar variable
//! reads and writes. A local of the running frame is boxed in its slot; a scalar
//! that is no local of the running frame boxes its binding in the package-block
//! lexical store (`package_lexicals`) or in the running routine's compunit-level
//! store (`unit_lexicals`) into a shared cell, so every alias shares one binding.

use super::*;

impl Interpreter {
    /// [`Self::scalar_cell_target`] fallback: promote a plain atomic-scalar binding
    /// to a shared `ContainerRef` cell on first use.
    ///
    /// The legacy lane stores an atomic scalar's value under
    /// `__mutsu_atomic_value::N`, reached through a `__mutsu_atomic_name::<name>`
    /// mapping in a **process-global** store. That mapping is keyed by the bare
    /// variable name, so it has no binding identity: an unrelated `my $i`
    /// declared anywhere else in the program wiped the counter, because every
    /// scalar declaration clears the entry for its own name
    /// (`reset_atomic_var_key_decl`). A cell is per-binding and cannot collide,
    /// and its mutex is a better atomic primitive than the store's write lock
    /// (every alias, including a spawned thread's clone, holds the same cell).
    ///
    /// **Which binding.** `slot` is the frame slot the compiler resolved the call
    /// site to (`Compiler::emit_atomic_target`). When it names a local of the
    /// running frame it is the binding, and nothing keyed by the bare name may
    /// answer instead: in `my atomicint $y; { my atomicint $y; $y⚛++ }` the env
    /// entry and the first slot spelled `y` belong to the *outer* `$y` (#12006).
    /// Without a slot (a captured outer lexical, a helper called by name) the
    /// name decides, as before.
    ///
    /// Only a name the RUNNING frame declares as its own local is boxed: that
    /// frame owns the binding, so its slot and `env` can be updated together —
    /// the same pairing `box_captured_lexicals` performs. A captured outer
    /// lexical reached from a closure frame is left alone unless the closure
    /// machinery already boxed it (in which case the lookup finds it).
    ///
    /// **Seed-and-retire protocol.** A same-name entry in the legacy
    /// `__mutsu_atomic_name::`/`__mutsu_atomic_value::` lane (written by an
    /// earlier `cas`/`atomic-*` call in THIS SAME thread, while this frame's
    /// own declared local held a refused shape) is newer than whatever the
    /// local slot currently holds and must not be shadowed by a fresh cell
    /// seeded from the stale slot: (1) peek at the legacy value with
    /// [`Self::legacy_atomic_value`] *before* deciding whether to box — a
    /// refused shape must leave the lane intact, not discard it; (2) box the
    /// peeked value (not the slot's own) into the new cell; (3) retire the
    /// lane with [`Self::reset_atomic_var_key`] only once boxing is
    /// confirmed, so it is not lost to a refusal.
    ///
    /// This protocol is deliberately NOT shared with `box_captured_lexicals`
    /// (`vm_register_ops.rs`) or `box_decl_local_cell`
    /// (`vm_var_assign_local_get.rs`): those fire at closure-creation/
    /// declaration time, which can race with an ALREADY-RUNNING sibling
    /// thread that is actively using the SAME bare name's legacy-lane
    /// mapping (e.g. `for 1..4 { my $head = ...; await start { loop { cas
    /// $head, ... } } xx 4 }` spawns several racing closures under one bare
    /// name). Seeding from the legacy lane there can promote a closure using
    /// a value a *different* thread produced rather than this frame's own
    /// current value, and retiring the mapping there can rip it out from
    /// under that other thread's in-flight retry loop — this regressed
    /// `roast/S17-lowlevel/cas.t` when tried (see
    /// `news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md`). This
    /// function is safe because it runs synchronously within the SAME thread
    /// whose own atomic op is being performed on its OWN declared local —
    /// there is no cross-thread race to seed from or retire out from under.
    // Cost: O(l) for the by-name slot search, l = the running frame's locals;
    // O(1) with a compiler-resolved slot.
    pub(super) fn atomic_scalar_cell(
        &mut self,
        name: &str,
        slot: Option<u32>,
    ) -> Option<crate::gc::Gc<crate::value::ContainerCell>> {
        if let Some(slot) = slot.and_then(|slot| self.own_local_slot(name, slot)) {
            return self.atomic_local_slot_cell(name, slot, true);
        }
        if let Some(cell) = self.scalar_cell_target(name) {
            return Some(cell);
        }
        if name.starts_with(['@', '%', '&', '!', '.']) {
            return None;
        }
        let bare = name.trim_start_matches('$');
        if self.current_code != 0 {
            // SAFETY: `current_code` is the address of the live bytecode frame's
            // `CompiledCode`, kept alive for the whole frame by `vm_call_*`.
            let code = unsafe { &*(self.current_code as *const crate::opcode::CompiledCode) };
            if let Some(slot) = code.locals.iter().position(|n| n == bare) {
                return self.atomic_local_slot_cell(name, slot, false);
            }
        }
        // A class-body `my` variable (e.g. `my atomicint $current-id`) read
        // via `⚛++`/`⚛--`/`cas` from an attribute default-value expression
        // has no frame-local slot to find above: default-value chunks
        // compile standalone with an empty local-slot table
        // (`Compiler::new_decl_chunk_compiler`), so every free name in them
        // resolves through the environment the declaration registers in —
        // here, the per-package "static" store `package_lexicals`
        // (`package_scope_lexical`/`read_package_scope_var`), not `env`.
        // Box that binding into a shared cell the same way a frame-local
        // binding is boxed above, so every alias (including a later atomic
        // op from a different frame/instance) reads and writes the SAME
        // cell instead of a stale by-value snapshot.
        if let Some(cell) = self.box_package_scope_lexical_cell(bare) {
            return Some(cell);
        }
        // A file-scope `my atomicint` of a loaded module, reached from one of
        // its exported routines, is no local of the running frame and not in
        // `env` either (that holds the importer's scope): it lives in the
        // compunit's unit-lexical store, where a plain read finds it. Box it
        // there so the atomic ops share the binding instead of falling back
        // to the name-keyed lane, which knows nothing about it and answered
        // the `atomicint` type object on the first fetch (#11455).
        self.box_unit_scope_lexical_cell(bare)
    }

    /// `slot` as an index into the running frame's locals, when the compiler's
    /// slot really is the local spelled `name` there. A hint that names another
    /// variable (a frame other than the one that was compiled, an alias the name
    /// was canonicalized through) is dropped, and the lookup falls back to the
    /// name.
    // Cost: O(1).
    fn own_local_slot(&self, name: &str, slot: u32) -> Option<usize> {
        if self.current_code == 0 {
            return None;
        }
        // SAFETY: `current_code` is the address of the live bytecode frame's
        // `CompiledCode`, kept alive for the whole frame by `vm_call_*`.
        let code = unsafe { &*(self.current_code as *const crate::opcode::CompiledCode) };
        let bare = name.trim_start_matches('$');
        code.locals
            .get(slot as usize)
            .is_some_and(|n| n == bare)
            .then_some(slot as usize)
    }

    /// The shared cell of the frame-local scalar in `slot`, boxing the slot's
    /// value into one the first time. `None` when its value is a shape that is
    /// left unboxed (the op then falls back to the name-keyed lane).
    // Cost: O(1) amortized (one slot read and, the first time, one cell
    // allocation and env/shared-store write).
    fn atomic_local_slot_cell(
        &mut self,
        name: &str,
        slot: usize,
        slot_addressed: bool,
    ) -> Option<crate::gc::Gc<crate::value::ContainerCell>> {
        let bare = name.trim_start_matches('$');
        // A slot that already holds a cell IS the binding's shared cell. (The
        // by-name lookup finds it through `env`, which a shadowed binding does
        // not own.)
        if let ValueView::ContainerRef(cell) = self.locals.get(slot)?.view() {
            return Some(cell.clone());
        }
        // Peek at the legacy lane without retiring yet -- see the
        // seed-and-retire protocol in the doc comment of
        // `atomic_scalar_cell`. Retiring happens only after the shape check
        // below confirms this value will actually be boxed, so a refused
        // shape doesn't lose the legacy entry.
        let legacy = self.legacy_atomic_value(name);
        let cur = match &legacy {
            Some(v) => v.clone(),
            None => self.locals.get(slot)?.clone(),
        };
        // Only plain scalar containers are boxed; reference types
        // already share, and hiding a type object / Proxy behind a
        // `ContainerRef` trips the paths that do not deref one. `Any`
        // is the uninitialized-scalar seed and is boxed like a value.
        //
        // This list is NOT a mirror of `box_captured_lexicals`', and has
        // not been one since ADR-0055 slice 1 (2026-08-28) let `Package`,
        // `Array` and `Hash` out of that one: it stayed WIDER on purpose,
        // because a refusal here is cheap (the op falls back to the
        // name-keyed legacy lane) while a refusal there costs the closure
        // its shared cell. The two only ever have to agree on
        // Seq/HyperSeq/RaceSeq/Slip
        // (`news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md`).
        // The lane fork the difference used to cause — a `cas` on an
        // Instance-valued scalar landing on the legacy lane while the
        // capture side held a cell — is fixed on the capture side
        // instead: `cas` counts as a write for the compiler's mutation
        // analysis now, so the two lanes are the same cell before this
        // function is ever consulted (`t/cas-captured-lexical-coherence.t`).
        // A compiler-resolved slot names its binding exactly, while the
        // name-keyed lane cannot tell two bindings spelled alike apart (#12107):
        // an object-valued scalar is boxed there, as `box_captured_lexicals`
        // boxes it for a capture.
        let refused = if slot_addressed {
            refuses_slot_addressed_cell_shape(&cur)
        } else {
            refuses_atomic_cell_shape(&cur)
        };
        if refused {
            return None;
        }
        // Retire the legacy lane now that its value is about to
        // become the cell's initial contents: nothing may read it
        // as authoritative again.
        if legacy.is_some() {
            self.reset_atomic_var_key(name);
        }
        let container = cur.into_container_ref();
        self.locals[slot] = container.clone();
        self.env.insert(bare.to_string(), container.clone());
        // A stale plain snapshot left in the cross-thread store would
        // be written back over the cell at the next sync,
        // disconnecting this binding from every alias — replace it
        // (no-op when the name was never snapshotted).
        // A shadowing `my` owns this fresh cell. Publishing it under
        // the bare name would discard the redeclaration mask and let
        // await reconcile the worker value into an unrelated outer
        // lexical with the same spelling.
        if self.threads.shared_vars_active
            && !self.threads.thread_redeclared_vars.borrow().contains(name)
            && !self.threads.thread_redeclared_vars.borrow().contains(bare)
        {
            self.set_shared_var(bare, container.clone());
        }
        match container.view() {
            ValueView::ContainerRef(c) => Some(c.clone()),
            _ => None,
        }
    }

    /// [`Self::box_package_scope_lexical_cell`] for the running routine's
    /// compunit-level lexical (`unit_lexical_slot`).
    // Cost: O(1) amortized (one unit-lexical store probe and, the first time,
    // one replace).
    pub(super) fn box_unit_scope_lexical_cell(
        &mut self,
        name: &str,
    ) -> Option<crate::gc::Gc<crate::value::ContainerCell>> {
        let cur = self.unit_lexical_slot(name, None)?.clone();
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
        if crate::qualified::is_global_package(self.current_package_sym()) {
            return None;
        }
        let pkg = self.current_package();
        let cur = self.lexicals.package_lexicals.get(&pkg)?.get(bare)?.clone();
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

/// [`refuses_atomic_cell_shape`] for a binding the compiler addressed by slot:
/// only the shapes whose `ContainerRef` wrapper trips paths that do not deref
/// one (a Proxy, the lazy sequence kinds) stay unboxed; objects and aggregates
/// share through the cell like any captured scalar.
// Cost: O(1).
fn refuses_slot_addressed_cell_shape(cur: &Value) -> bool {
    !cur.is_any_type_object()
        && matches!(
            cur.view(),
            ValueView::Proxy { .. }
                | ValueView::Seq(..)
                | ValueView::HyperSeq(..)
                | ValueView::RaceSeq(..)
                | ValueView::Slip(..)
        )
}
