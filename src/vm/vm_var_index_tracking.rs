//! Deleted-index and initialized-index bookkeeping helpers
//! split from `vm_var_ops` (§7-8 file split).
use super::*;

impl Interpreter {
    pub(super) fn encode_element_index(idx: &Value) -> String {
        match idx.view() {
            ValueView::Array(indices, ..) => indices
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(";"),
            _ => idx.to_string_value(),
        }
    }

    pub(super) fn mark_initialized_index(
        &mut self,
        place: StorePlace,
        var_name: &str,
        encoded: String,
    ) {
        // Record the assigned index in the array's *embedded* `initialized`
        // set so it travels with the value across scopes/closures/method calls
        // (the former env-name-keyed side table was scoped to the assigning
        // frame and lost on scope exit). Only array indices (plain integers)
        // are tracked — hash existence is determined by key presence.
        let Ok(idx) = encoded.parse::<usize>() else {
            return;
        };
        // Mutate the array in place when it is shared, mirroring the
        // assignment path's `use_inplace` choice (vm_var_assign_index_named,
        // container identity §3 — no sigil carve-out). Using `Arc::make_mut`
        // here would clone a shared array and sever the by-ref alias, so a
        // subsequent `.push` on the original would be lost
        // (t/array-push-byref-coherence).
        if let Some(root) = self.place_root_mut(place, var_name) {
            root.with_array_mut(|items, _| {
                let data: &mut crate::value::ArrayData = if crate::gc::Gc::strong_count(items) > 1 {
                    // SAFETY: aliased in-place mutation of a shared array; same
                    // contract as the assignment site's `gc_contents_mut` use.
                    unsafe { crate::value::gc_contents_mut(items) }
                } else {
                    crate::gc::Gc::make_mut(items)
                };
                data.mark_initialized(idx);
            });
        }
    }

    /// Remove deleted indices from the initialized-index tracking set.
    /// This must be called after array element deletion and before
    /// `trim_trailing_array_holes` so that deleted slots are recognized
    /// as holes and can be trimmed.
    pub(super) fn unmark_initialized_indices(&mut self, var_name: &str, idx: &Value) {
        // Collect the integer indices addressed by `idx` (scalar, slice, or
        // range) and drop them from the array's embedded `initialized` set so
        // the deleted slots are recognized as holes and can be trimmed.
        //
        // A zen/whatever slice (`@a[]:delete` / `@a[*]:delete`) addresses
        // EVERY index but arrives here as a single `ValueView::Whatever`
        // value, which `collect_usize_indices` cannot expand without knowing
        // the array's length -- handle it separately below, inside the
        // `with_array_mut` closure where the length is available. (ADR-0049
        // slice 5: this used to be silently compensated for by
        // `trim_trailing_array_holes`'s own now-removed inconsistency --
        // treating `initialized == None` as an empty set rather than "every
        // index present" made every trailing gap-marker element trim
        // regardless of tracking. Folding that function onto the correct,
        // canonical `ArrayData::hole_at` semantics exposed this as the real
        // root cause instead.)
        let is_whatever = matches!(idx.view(), ValueView::Whatever);
        let mut to_remove: Vec<usize> = Vec::new();
        if !is_whatever {
            Self::collect_usize_indices(idx, &mut to_remove);
            if to_remove.is_empty() {
                return;
            }
        }
        if let Some(root) = self.env_root_descended_mut(var_name) {
            root.with_array_mut(|items, _| {
                // Container identity (§3): write through a shared node.
                let data = crate::value::gc_data_mut(items);
                // A freshly-built array tracks "all present" as `initialized = None`.
                // Materialize the set (0..len) before removing the deleted indices so
                // the hole is recorded IN the ArrayData — not only in the name-keyed
                // `__mutsu_deleted_index::` side table, which a `.clone` (bound to a
                // new name) would not inherit. This makes `@b := @a.clone` preserve a
                // deleted slot's `:exists` == False (S02-types/array.t test 108).
                let len = data.len();
                let set = data.initialized.get_or_insert_with(|| (0..len).collect());
                if is_whatever {
                    // Every index was addressed: none remain present.
                    set.clear();
                } else {
                    for i in to_remove {
                        set.remove(&i);
                    }
                }
            });
        }
    }

    /// Flatten a subscript index `Value` (scalar / array slice / range) into the
    /// list of non-negative integer array indices it addresses.
    fn collect_usize_indices(idx: &Value, out: &mut Vec<usize>) {
        match idx.view() {
            ValueView::Array(items, ..) => {
                for item in items.iter() {
                    Self::collect_usize_indices(item, out);
                }
            }
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => {
                for item in &crate::runtime::utils::value_to_list(idx) {
                    Self::collect_usize_indices(item, out);
                }
            }
            // ADR-0049 slice 5: delegate scalar conversion to the same
            // `index_to_usize` every OTHER multi-dim/array index site uses
            // (Int/Num/Rat/FatRat/BigRat, with a finite check) instead of a
            // narrower ad-hoc Int/Num-only match with a string-parse
            // fallback -- the old fallback silently dropped a `Rat` index
            // (`1.5` in raku source is a `Rat`, not a `Num`; its
            // `to_string_value()` is "1.5", which does not parse as a
            // `usize`), so `@a[1.5]:delete` never recorded the deleted slot
            // in `initialized` and a trailing hole was left untrimmed.
            _ => {
                if let Some(i) = Self::index_to_usize(idx) {
                    out.push(i);
                }
            }
        }
    }

    /// Whether element `idx` of `container` has no writable container: it was
    /// `:=`-bound to a bare value (`@a[i] := 42`, `.BIND-POS`/`.BIND-KEY`), which
    /// leaves a read-only cell in the slot (`Value::bound_element`). An
    /// in-place mutator (`++`, `--`) must refuse such an element (#10984).
    // Cost: O(1): one flag load in a program with no such bind; else one
    // element/hash probe.
    pub(super) fn element_has_no_container(container: Option<&Value>, idx: &Value) -> bool {
        if !crate::value::readonly_cells_possible() {
            return false;
        }
        let idx = match idx.view() {
            ValueView::Array(items, _) if items.len() == 1 => items[0].clone(),
            _ => idx.clone(),
        };
        let slot = match container.map(Value::view) {
            Some(ValueView::Array(items, _)) => match idx.view() {
                ValueView::Int(i) => usize::try_from(i).ok().and_then(|i| items.get(i).cloned()),
                _ => None,
            },
            Some(ValueView::Hash(map)) => {
                let key = if map.key_type.is_some() {
                    crate::runtime::utils::value_which_key(&idx)
                } else {
                    idx.to_string_value()
                };
                map.map.get(&key).cloned()
            }
            _ => None,
        };
        matches!(
            slot.as_ref().map(Value::view),
            Some(ValueView::ContainerRef(cell)) if cell.is_readonly()
        )
    }

    /// The value held by the read-only cell at `target[d0;d1;...]`, when that
    /// slot is one (an element bound to a bare value); `None` otherwise,
    /// including for any index that is not a plain in-range `Int`.
    // Cost: O(d), d = dimensions.
    pub(super) fn multidim_readonly_slot(target: &Value, dims: &[Value]) -> Option<Value> {
        let mut cur = target.deref_container();
        for (n, d) in dims.iter().enumerate() {
            let ValueView::Int(i) = d.view() else {
                return None;
            };
            let ValueView::Array(items, _) = cur.view() else {
                return None;
            };
            let slot = items.get(usize::try_from(i).ok()?)?.clone();
            if n + 1 == dims.len() {
                return match slot.view() {
                    ValueView::ContainerRef(cell) if cell.is_readonly() => Some(cell.load()),
                    _ => None,
                };
            }
            cur = slot.deref_container();
        }
        None
    }
}
