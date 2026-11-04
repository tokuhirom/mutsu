//! The temporary env view `:delete` runs over.
//!
//! The delete handler (`exec_delete_index_named_op_inner`) resolves its
//! container out of `env[var_name]` and writes it back by name. A variable
//! whose container does not live there as a plain Hash/Array (a scalar-held
//! one in a local slot, a compunit's file-scope `@`/`%`, an `our` variable, a
//! `:=`-bound cell, a for-bound Scalar wrapper, a `Mixin`, a lazy array) is
//! therefore exposed in env for the duration of the op ([`DeleteHome::enter`])
//! and handed back to its real home afterwards ([`DeleteHome::exit`]).
//!
//! One type owns that dance so the handler is "enter, delete, exit"; replacing
//! the by-name reads of the inner handler with a resolved container (ADR-14632
//! follow-up, #14701) then only has to change this type.

use super::*;
use crate::value::ContainerCell;

/// What [`DeleteHome::enter`] changed in env, to be undone by [`DeleteHome::exit`].
pub(super) struct DeleteHome {
    lazy_source: Option<crate::gc::Gc<LazyList>>,
    pre_delete_len: Option<usize>,
    unit_cell: Option<crate::gc::Gc<ContainerCell>>,
    saved_env_entry: Option<Option<Value>>,
    our_key: Option<String>,
    saved_our_env_entry: Option<Option<Value>>,
    bound_cell: Option<crate::gc::Gc<ContainerCell>>,
    mixin_overrides: Option<crate::value::MixinOverrides>,
}

impl Interpreter {
    /// Expose the container of `var_name` in env for `:delete`.
    // Cost: O(1) beyond the lazy-array reify of `reify_lazy_array_slot`
    // (bounded by the touched index) and the slot sync.
    pub(super) fn enter_delete_home(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
        var_name: &str,
    ) -> Result<DeleteHome, RuntimeError> {
        let var_name = var_name.to_string();
        // Dual store: a scalar-held container (`my $h = {...}; $h<a>:delete`)
        // keeps its container in the local slot and leaves the env mirror at the
        // `my`-declaration seed (a type object). Every step below resolves the
        // container out of env by name, so without this the op reads `Any`,
        // deletes nothing, and — worse — the closing env-to-slot sync writes that
        // `Any` over the variable, destroying the container. Seed env from the
        // authoritative slot first, as the sibling element-assignment handler
        // does.
        self.seed_env_from_scalar_slot(code, slot, &var_name);
        // A lazy `@`-array reifies a prefix before an element delete
        // (`@a[i]:delete`) — delete needs a materialized backing Array to
        // operate on. Peeking the (not-yet-popped) index bounds the reify to
        // exactly the touched element for the common simple-Int-subscript
        // shape; `restore_lazy_array_slot` at the end of this function
        // rebuilds a still-lazy value around the mutated prefix and the SAME
        // live source afterwards, so the array does not lose its infinite
        // tail. (L2, bounded reify follow-up — see docs/lazy-arrays.md)
        let delete_touched_index = self.stack.last().and_then(|idx| match idx.view() {
            ValueView::Int(n) if n >= 0 => Some(n),
            _ => None,
        });
        let lazy_source =
            self.reify_lazy_array_slot(&var_name, code.const_sym(name_idx), delete_touched_index)?;
        // A lazy array's reified prefix has a live tail beyond it, so
        // deleting its LAST touched slot must NOT shrink the array the way
        // `trim_trailing_array_holes` shrinks a genuinely finite one (raku:
        // `@a[2]:delete` on `1,2,4...Inf` leaves `@a[2]` an `Any` hole,
        // `@a[3]` still `8` -- the array does not lose an element). Record
        // the reified prefix's length now, before `exec_delete_index_named_op_inner`
        // runs and (for a plain Array) may trim a trailing hole off; restored
        // below if trimming shortened it.
        let pre_delete_len = lazy_source.as_ref().and_then(|_| {
            self.env().get(&var_name).and_then(|v| match v.view() {
                ValueView::Array(items, _) => Some(items.len()),
                _ => None,
            })
        });
        // ADR-0039 slice 1: `var_name` may be a compunit's own file-scope
        // `@`/`%` (its authoritative storage is the cell in `unit_lexicals`,
        // not `env[var_name]` — that key can hold a completely unrelated
        // same-named binding, the loading scope's own `my %items`). Unlike
        // the `:=`-bound-cell handling just below (which LEAVES the cell
        // installed in env afterwards, correct for a real bind alias), this
        // must instead temporarily seed env with the cell's inner container,
        // run the op, write the mutated result back through the cell, and
        // then RESTORE env to whatever it held before — leaving the cell in
        // env here would wrongly make this name resolve to the module's
        // container from outside the module too, undoing the isolation this
        // ADR establishes.
        let unit_cell = self.unit_lexical_container_cell_at(&var_name, slot);
        let saved_env_entry = unit_cell.as_ref().map(|cell| {
            let saved = self.env().get(&var_name).cloned();
            let inner = cell.load();
            self.env_mut().insert(var_name.clone(), inner);
            saved
        });
        // The `our`-package twin of the unit-lexical case just above: an
        // `our %h`/`our @a` of the running routine's own package is stored
        // under the package-qualified mirror in `our_vars`, while `env[%h]`
        // holds the loading scope's own same-named binding. Same
        // seed / run / write-back / restore dance, because this op resolves
        // its container out of env by name rather than through
        // `env_root_descended_mut` (which already prefers the mirror).
        // Mutually exclusive with `unit_cell`: a compunit's file-scope `my`
        // wins over a same-named package variable, matching the read-side
        // precedence in `get_env_with_main_alias`.
        let our_key = match unit_cell {
            Some(_) => None,
            None => self.our_package_container_key(&var_name),
        };
        let saved_our_env_entry = our_key.as_ref().map(|key| {
            let saved = self.env().get(&var_name).cloned();
            let inner = self.get_our_var(key).cloned().unwrap_or(Value::NIL);
            self.env_mut().insert(var_name.clone(), inner);
            saved
        });
        let bound_cell = match self.env().get(&var_name).map(Value::view) {
            Some(ValueView::ContainerRef(cell)) => Some(cell.clone()),
            _ => None,
        };
        if let Some(ref cell) = bound_cell {
            let inner = cell.load();
            self.env_mut().insert(var_name.clone(), inner);
        }
        // A for-bound scalar can retain both a shared cell and a Scalar
        // wrapper around the aggregate.  The delete machinery below mutates
        // the concrete Hash/Array representation, so expose that payload in
        // the temporary env mirror just as the ContainerRef branch above
        // does.  The backing aggregate remains shared with the binding.
        if let Some(value) = self.env().get(&var_name).cloned()
            && !matches!(value.view(), ValueView::Hash(..) | ValueView::Array(..))
            && value.with_deref(|inner| {
                matches!(
                    inner.descalarize().view(),
                    ValueView::Hash(..) | ValueView::Array(..)
                )
            })
        {
            let concrete = value.with_deref(|inner| inner.descalarize().clone());
            self.env_mut().insert(var_name.clone(), concrete);
        }
        // A `%h but R` / `@a but R` mixin wraps its container in a `Mixin`, which
        // none of the container arms below recognise: `:delete` silently did
        // nothing and returned Nil. Where a composed role supplies the subscript
        // protocol itself the inner dispatch calls it (see the Mixin arm in
        // `exec_delete_index_named_op_inner`); otherwise the delete belongs to the
        // wrapped container, so unwrap it into env for the duration of the op and
        // re-wrap afterwards. Unwrapping — rather than reimplementing the delete
        // for mixins — keeps every bit of bookkeeping working unchanged, because
        // all of it (`__mutsu_deleted_index::`, trailing-hole trimming, the
        // container-metadata re-tag) is keyed by the *variable name*, which the
        // unwrap does not change.
        let mixin_overrides = match self.env().get(&var_name).cloned() {
            Some(value) => match value.view() {
                ValueView::Mixin(inner, overrides)
                    if !self.mixin_role_has_method(&value, "DELETE-KEY")
                        && !self.mixin_role_has_method(&value, "DELETE-POS") =>
                {
                    let inner = inner.as_ref().clone();
                    let overrides = overrides.as_ref().clone();
                    self.env_mut().insert(var_name.clone(), inner);
                    Some(overrides)
                }
                _ => None,
            },
            None => None,
        };
        Ok(DeleteHome {
            lazy_source,
            pre_delete_len,
            unit_cell,
            saved_env_entry,
            our_key,
            saved_our_env_entry,
            bound_cell,
            mixin_overrides,
        })
    }

    /// Hand the container `:delete` mutated in env back to its real home and
    /// restore what [`Self::enter_delete_home`] displaced.
    // Cost: O(1), plus the pad of a lazy array's trimmed prefix.
    pub(super) fn exit_delete_home(
        &mut self,
        code: &CompiledCode,
        slot: Option<u32>,
        var_name: &str,
        home: DeleteHome,
    ) {
        let var_name = var_name.to_string();
        let DeleteHome {
            lazy_source,
            pre_delete_len,
            unit_cell,
            saved_env_entry,
            our_key,
            saved_our_env_entry,
            bound_cell,
            mixin_overrides,
        } = home;
        if let Some(ll) = lazy_source {
            // Undo a trailing-hole trim `exec_delete_index_named_op_inner`
            // may have applied (see the `pre_delete_len` comment above): pad
            // the array back to its reified length with the same `Any` hole
            // value a delete of a non-trailing slot would have left, so the
            // live tail's positions are not shifted.
            if let Some(min_len) = pre_delete_len
                && let Some(container) = self.env_mut().get_mut(&var_name)
                && matches!(container.view(), ValueView::Array(items, _) if items.len() < min_len)
            {
                // Extend the SAME `ArrayData` node in place (not a fresh
                // `Value::real_array`), so its embedded `default`/type
                // metadata -- already re-tagged by
                // `exec_delete_index_named_op_inner` above -- survives for
                // `restore_lazy_array_slot` to read back.
                container.with_array_mut(|items, _kind| {
                    let arr = crate::value::gc_data_mut(items);
                    while arr.len() < min_len {
                        arr.push_gap(Value::package(crate::symbol::wk::any()));
                    }
                });
                if let Some(padded_val) = self.env().get(&var_name).cloned() {
                    self.locals_set_by_name(code, &var_name, padded_val);
                }
            }
            self.restore_lazy_array_slot(code, &var_name, ll);
        }
        if let Some(overrides) = mixin_overrides
            && let Some(mutated) = self.env().get(&var_name).cloned()
        {
            let rewrapped = Value::mixin_with_state(mutated, overrides);
            self.env_mut().insert(var_name.clone(), rewrapped.clone());
            self.write_local_slot_or_name(code, slot, &var_name, rewrapped);
        }
        if let Some(cell) = bound_cell {
            if let Some(mutated) = self.env().get(&var_name).cloned() {
                cell.store(mutated);
            }
            let cell_val = Value::container_ref(cell);
            self.env_mut().insert(var_name.clone(), cell_val.clone());
            self.write_local_slot_or_name(code, slot, &var_name, cell_val);
        }
        if let Some(key) = our_key {
            if let Some(mutated) = self.env().get(&var_name).cloned() {
                self.our_mirror_store_preserving_identity(&key, &mutated);
            }
            // Restore whatever the bare env key held before the seed — see
            // the `unit_cell` restore below for why leaving the package
            // container installed there would undo the isolation.
            match saved_our_env_entry.flatten() {
                Some(v) => {
                    self.env_mut().insert(var_name.clone(), v);
                }
                None => {
                    self.env_mut().remove(&var_name);
                }
            }
        }
        if let Some(cell) = unit_cell {
            if let Some(mutated) = self.env().get(&var_name).cloned() {
                cell.store(mutated);
            }
            // `saved_env_entry` is `Some(_)` whenever `unit_cell` is (both
            // guarded by the same `if let Some(cell) = ...` above at seed
            // time), so this `flatten()` recovers the ORIGINAL env entry —
            // `Some(Some(v))` (had one) or `Some(None)` (had none) — not a
            // "did we even seed" flag.
            match saved_env_entry.flatten() {
                Some(v) => {
                    self.env_mut().insert(var_name.clone(), v);
                }
                None => {
                    self.env_mut().remove(&var_name);
                }
            }
        }
    }
}
