//! Slice 2e of `docs/scalar-array-sharing.md`: a `$`-sigil ATTRIBUTE store of
//! a bare `@`/`%` variable (`$obj.w = %src`, #9041) shares the source
//! container by reference, exactly as the local `$n = %src` store (Slice 2a)
//! does. Raku's attribute is a Scalar container, so assigning an Array/Hash
//! into it stores the same object; a later `%src<y> = 2` / `@src.push(..)` is
//! seen through `$obj.w`, and a `$obj.w.push(..)` through the source.
//!
//! The mechanism is Slice 2a's `ContainerRef` cell: the source variable is
//! promoted to a shared cell ([`Interpreter::promote_array_share_source`]) and
//! the attribute slot holds the same cell in its ITEMIZED flavour
//! (`ContainerRefItemized`). That flavour is also the attribute's
//! "value share, not a `:=` bind" marker: a later whole store into the
//! attribute (`$obj.w = 5`, `$!w = 5`) replaces the slot instead of writing
//! through the cell into the source (see the `$` branch of
//! `assign_method_lvalue_with_values`).

use super::*;
use crate::meta_ns::MetaNs;

impl Interpreter {
    /// After `__mutsu_assign_method_lvalue` stored `value` (argument 3 of the
    /// call, compiled from the bare variable `source`) into `target.method`,
    /// turn that store into a reference share when it is one in raku: the
    /// accessor is a `$`-sigil attribute and the slot now holds the very
    /// Array/Hash the source variable holds.
    ///
    /// Deciding AFTER the store is what keeps this exact: every type check,
    /// coercion (`has Array() $.w`), `Nil`-default and itemization the store
    /// applies already ran, and a store that did not keep the source's
    /// backing allocation (a coercion built a new container, a Proxy STORE
    /// kept something else) simply fails the identity test and stays a copy.
    // Cost: O(m + a + f + l), m = MRO length, a = class attributes,
    // f = saved call frames, l = locals of this frame.
    pub(super) fn share_scalar_attr_store_with_source(
        &mut self,
        code: &CompiledCode,
        target: &Value,
        method: &Value,
        method_args: &Value,
        value: &Value,
        source: &str,
    ) {
        if !(source.starts_with('@') || source.starts_with('%')) {
            return;
        }
        let Some(source_ptr) = value.with_deref(Self::aggregate_backing_ptr) else {
            return;
        };
        let target = target.deref_container();
        let ValueView::Instance { attributes, .. } = target.view() else {
            return;
        };
        let Some(method) = method.as_str().map(str::to_string) else {
            return;
        };
        let method_args = match method_args.view() {
            ValueView::Array(items, ..) => items.to_vec(),
            _ => Vec::new(),
        };
        let Some(attr) = self.method_lvalue_scalar_attr_name(&target, &method, &method_args) else {
            return;
        };
        let private_key = format!("{attr}!");
        let key = {
            let map = attributes.as_map();
            let key = if map.contains_key(attr.as_str()) {
                attr
            } else if map.contains_key(private_key.as_str()) {
                private_key
            } else {
                return;
            };
            // A `:=`-bound attribute (plain `ContainerRef`) already received
            // the value THROUGH its binding; leave that binding alone. Only a
            // bare slot that holds the source's own allocation is shared.
            match map.get(key.as_str()) {
                Some(slot)
                    if !slot.is_container_ref()
                        && Self::aggregate_backing_ptr(slot) == Some(source_ptr) => {}
                _ => return,
            }
            key
        };
        let resolved_source = self.resolve_sigilless_alias_source_name(source);
        let cell = self.promote_array_share_source(code, &resolved_source, value);
        attributes.insert(key.as_str(), Value::container_ref_itemized(cell));
    }

    /// Does the `$` scalar `name` (whose current slot is `slot`) hold an
    /// array/hash by `=` VALUE share directly in the slot, so a whole
    /// reassignment REPLACES the slot instead of writing through the shared
    /// cell into the source? Scalar locals with their own holder cell write
    /// into that outer cell; bare share slots and Slice 2e attributes still
    /// need replacement here.
    // Cost: O(1) (one env symbol probe).
    pub(super) fn is_value_share_slot(&self, name: &str, slot: Option<&Value>) -> bool {
        (self.array_share_active
            && self.is_array_share_scalar(name)
            && slot.is_some_and(Value::container_ref_is_itemized))
            || (name.starts_with('!') && slot.is_some_and(Value::container_ref_is_itemized))
    }

    /// The address of an Array/Hash value's backing allocation — the identity
    /// a `$`-itemizing retag keeps (`with_hash_itemized` / `items.clone()`),
    /// so a stored attribute value can be matched against its source.
    fn aggregate_backing_ptr(value: &Value) -> Option<usize> {
        match value.view() {
            ValueView::Array(items, ..) => {
                Some(crate::gc::Gc::as_ptr(&*items) as *const () as usize)
            }
            ValueView::Hash(h) => Some(crate::gc::Gc::as_ptr(&*h) as *const () as usize),
            _ => None,
        }
    }

    /// The source half of an array share (Slice 2a/2c): build (or reuse) the
    /// shared `ContainerRef` cell for `val` and promote the SOURCE container
    /// variable `resolved_source` to it — in this frame's slot, in `env`, and
    /// in every saved call frame that owns the lexical — so the source's own
    /// `.push` / whole-reassign (`@z = (...)`) mutate through and stay visible
    /// to every scalar that holds the returned cell.
    // Cost: O(f + l), f = saved call frames, l = locals of this frame.
    pub(crate) fn promote_array_share_source(
        &mut self,
        code: &CompiledCode,
        resolved_source: &str,
        val: &Value,
    ) -> crate::gc::Gc<crate::value::ContainerCell> {
        // The source variable's binding as this frame sees it, before the
        // promotion below rebinds it: a saved call frame holds the SAME
        // variable only when its entry is this very binding (a closure's view
        // of an enclosing lexical). A caller that merely has an unrelated
        // same-named variable -- a recursive call's own `-> $idx, $item` loop
        // parameter -- must not be overwritten (#11304). A local slot is the
        // authority when the name has one: the env entry may still be the
        // caller's binding inherited by name.
        let prior_binding: Option<Value> =
            match code.locals.iter().rposition(|n| n == resolved_source) {
                Some(idx) => Some(self.locals[idx].clone()),
                None => self.env().get(resolved_source).cloned(),
            };
        // A scalar source already has its own holder cell. A chained share
        // takes the aggregate cell *inside* that holder; promoting the holder
        // itself would make the new scalar follow later assignments to the
        // source scalar rather than mutations of the aggregate.
        if !resolved_source.starts_with('@')
            && !resolved_source.starts_with('%')
            && let Some(source) = self.env().get(resolved_source)
            && let ValueView::ContainerRef(holder) = source.view()
        {
            let held = holder.lock().unwrap().clone();
            if held.container_ref_is_itemized()
                && let ValueView::ContainerRef(shared) = held.view()
            {
                return shared.clone();
            }
        }
        let source_is_scalar = !resolved_source.starts_with('@')
            && !resolved_source.starts_with('%')
            && !resolved_source.starts_with('&')
            && self
                .env()
                .get_sym(MetaNs::BoundDecont.key_for_str(resolved_source))
                .is_none();
        // Build (or reuse) the shared cell: reuse an existing cell carried by the
        // value or already held by the source variable, else wrap the snapshot.
        let cell = if source_is_scalar {
            // A scalar that held an aggregate directly needs its own holder
            // before the aggregate can be shared with another scalar.
            crate::gc::Gc::new(crate::value::ContainerCell::new(val.deref_container()))
        } else {
            match val.view() {
                ValueView::ContainerRef(arc) => arc.clone(),
                // A scalar holding an array share is represented as
                // `Scalar(ContainerRef(cell))` so its `.raku` keeps the `$`
                // marker without changing the source array's own rendering.
                // Chained `$r = $q` must nevertheless reuse that same cell.
                ValueView::Scalar(inner) if inner.is_container_ref() => {
                    if let ValueView::ContainerRef(arc) = inner.view() {
                        arc.clone()
                    } else {
                        unreachable!("ContainerRef tag changed while extracting share cell")
                    }
                }
                _ => match self.env().get(resolved_source).map(Value::view) {
                    Some(ValueView::ContainerRef(arc)) => arc.clone(),
                    _ => crate::gc::Gc::new(crate::value::ContainerCell::new(val.clone())),
                },
            }
        };
        // An aggregate source holds a plain word over this cell. A scalar
        // source holds the itemized word inside its own outer cell. In either
        // case the target takes its own itemized holder over the aggregate.
        let source_idx = code.locals.iter().rposition(|n| n == resolved_source);
        let source_itemized = val.container_ref_is_itemized()
            || self
                .env()
                .get(resolved_source)
                .is_some_and(Value::container_ref_is_itemized);
        let container = if source_is_scalar {
            let itemized = Value::container_ref_itemized(cell.clone());
            let source = source_idx
                .map(|idx| &self.locals[idx])
                .or_else(|| self.env().get(resolved_source));
            if let Some(ValueView::ContainerRef(holder)) = source.map(Value::view) {
                let value_cell = Self::value_cell_of(&holder);
                Value::store_through_cell(&value_cell, &itemized);
                Value::container_ref(holder.clone())
            } else {
                Value::container_ref(crate::gc::Gc::new(crate::value::ContainerCell::new(
                    itemized,
                )))
            }
        } else if source_itemized {
            Value::container_ref_itemized(cell.clone())
        } else {
            Value::container_ref(cell.clone())
        };
        // Promote the SOURCE container variable to the same cell so its own
        // `.push` / whole-reassign (`@z = (...)`) mutate through and stay visible
        // via the scalar.
        if let Some(source_idx) = source_idx {
            self.locals[source_idx] = container.clone();
            self.flush_local_to_env(code, source_idx);
        }
        self.set_env_with_main_alias(resolved_source, container.clone());
        // Propagate the shared cell into saved call frames so the sharing
        // survives method returns (env restore).
        // Slots now live in one shared stack (ADR-0077), so a saved frame is a
        // `[base, end)` region rather than its own vector: walking downwards,
        // the region a frame saved ends where that frame's own base begins, and
        // the topmost one ends at the executing frame's base. Collect the writes
        // during the walk (which borrows `call_frames` mutably for the env
        // inserts) and apply them to the slot stack afterwards.
        let mut end = self.locals.base();
        let mut shared_slot_writes: Vec<usize> = Vec::new();
        for frame in self.call_frames.iter_mut().rev() {
            let Some(base) = frame.saved_locals_base.as_ref().map(|c| c.base()) else {
                continue;
            };
            // `code.locals` is this frame's slot layout, not the parent's; only
            // write a parent frame's slots when that frame owns the source
            // lexical (its saved env holds the name), else the callee slot index
            // clobbers an unrelated same-index local.
            //
            // `__ANON_STATE__` (the anonymous `$`) is the exception: a method
            // frame also keeps a mirror of that key in its env (its implicit
            // `__ANON_STATE__` param), so the ownership test passes for a
            // method caller, and the callee's slot 0 would land on the caller's
            // `self`. An anonymous scalar is never a caller's variable.
            if resolved_source != "__ANON_STATE__"
                && frame.saved_env.contains_key_own_tier(resolved_source)
                && frame.saved_env.get(resolved_source).is_some_and(|saved| {
                    prior_binding
                        .as_ref()
                        .is_some_and(|p| p.same_binding(saved))
                })
            {
                frame
                    .saved_env
                    .insert(resolved_source.to_string(), container.clone());
                for (i, local_name) in code.locals.iter().enumerate() {
                    if local_name == resolved_source && base + i < end {
                        shared_slot_writes.push(base + i);
                    }
                }
            }
            end = base;
        }
        for slot in shared_slot_writes {
            *self.locals.absolute_slot_mut(slot) = container.clone();
        }
        cell
    }
}
