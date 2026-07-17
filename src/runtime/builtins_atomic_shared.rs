//! Thread-safe shared-container mutations (`@arr.push`/`@arr[i] = v`/`%h{k} = v`
//! routed through the `__mutsu_atomic_*::` shared store), multi-dimensional and
//! hash-element compare-and-swap, and the instance-attribute cell resolver
//! (`self_attr_cell_target`) shared with `builtins_atomic`/`builtins_atomic_cas`.

use super::*;

impl Interpreter {
    /// Box `v` into a fresh element cell unless it already is one (Track B
    /// slice 1 — ADR-0001 layer 3a's element-cell companion).
    pub(super) fn boxed_elem_cell(v: Value) -> Value {
        if v.is_container_ref() {
            v
        } else {
            Value::container_ref(crate::gc::Gc::new(std::sync::Mutex::new(v)))
        }
    }

    /// Ensure the `__mutsu_atomic_hash::`/`__mutsu_atomic_arr::` store node
    /// for `name` exists with EVERY element value boxed into a `ContainerRef`
    /// cell — the Track B element-cell representation, applied in ONE pass at
    /// the container's first atomic touch.
    ///
    /// Why cells: the store previously kept plain snapshots, so every atomic
    /// element RMW had to clone the whole container to publish one element
    /// (readers on other threads hold the old node without a lock, so
    /// in-place map mutation would be a data race). That made
    /// `cas %h{$_}` × 30k on a 10k-entry hash cost ~300M entry copies
    /// (S17-lowlevel/thread.t test 28: 12.2s GC-off / 21.2s GC-on). With
    /// element cells the map STRUCTURE stays copy-on-write (readers'
    /// snapshots remain immutable), while element VALUES mutate in place
    /// under the cell's own mutex — every holder of any snapshot shares the
    /// cells, so cross-thread reads stay coherent and an element RMW is O(1).
    /// Reader-side deref of `ContainerRef` hash/array values is the
    /// long-standing `%h<k> := $x` binding machinery and is already
    /// universal on the read paths (element read, arithmetic, compare, grep,
    /// sort, stringify — probed before this slice landed).
    pub(super) fn init_celled_atomic_store(&mut self, atomic_key: &str, name: &str) {
        {
            // ADR-0010: atomics are process-wide shared state -> the root lineage.
            let atomic_root = self.shared_vars.root_store();
            let shared = atomic_root.own_map().read().unwrap();
            if shared.contains_key(atomic_key) {
                return;
            }
        }
        let base = self
            .env
            .get(name)
            .cloned()
            .or_else(|| self.get_shared_var(name));
        let celled = match base.as_ref().map(Value::view) {
            Some(ValueView::Hash(h)) => {
                let mut data = h.as_ref().clone();
                for v in data.map.values_mut() {
                    let taken = std::mem::replace(v, Value::NIL);
                    *v = Self::boxed_elem_cell(taken);
                }
                Value::hash_with_data(crate::gc::Gc::new(data))
            }
            Some(ValueView::Array(a, kind)) => {
                let mut data = a.as_ref().clone();
                for v in data.items.iter_mut() {
                    let taken = std::mem::replace(v, Value::NIL);
                    *v = Self::boxed_elem_cell(taken);
                }
                Value::array_with_kind(crate::gc::Gc::new(data), kind)
            }
            _ => {
                if name.starts_with('@') {
                    Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
                        crate::value::ArrayKind::Array,
                    )
                } else {
                    Value::hash_with_data(Value::hash_arc(HashMap::new()))
                }
            }
        };
        // ADR-0010: atomics are process-wide shared state -> the root lineage.
        let atomic_root = self.shared_vars.root_store();
        let mut shared = atomic_root.own_map().write().unwrap();
        if !shared.contains_key(atomic_key) {
            shared.insert(atomic_key.to_string(), celled.clone());
            shared.insert(name.to_string(), celled.clone());
            drop(shared);
            self.env.insert(name.to_string(), celled);
            if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                dirty.insert(atomic_key.to_string());
                dirty.insert(name.to_string());
            }
        }
    }

    /// The element cell for `key` in the celled atomic hash store, creating it
    /// (one COW of the map structure) when the key is missing or was
    /// overwritten with a plain value by a structural assignment. The returned
    /// handle is shared by every snapshot of the container, so mutating
    /// through it is visible everywhere without republishing the node.
    pub(super) fn celled_hash_elem(
        &mut self,
        atomic_key: &str,
        hash_name: &str,
        key: &str,
    ) -> crate::gc::Gc<std::sync::Mutex<Value>> {
        {
            // ADR-0010: atomics are process-wide shared state -> the root lineage.
            let atomic_root = self.shared_vars.root_store();
            let shared = atomic_root.own_map().read().unwrap();
            if let Some(ValueView::Hash(h)) = shared.get(atomic_key).map(Value::view)
                && let Some(ValueView::ContainerRef(c)) = h.get(key).map(Value::view)
            {
                return c.clone();
            }
        }
        // ADR-0010: atomics are process-wide shared state -> the root lineage.
        let atomic_root = self.shared_vars.root_store();
        let mut shared = atomic_root.own_map().write().unwrap();
        // Re-check under the write lock (a racer may have boxed it).
        if let Some(ValueView::Hash(h)) = shared.get(atomic_key).map(Value::view)
            && let Some(ValueView::ContainerRef(c)) = h.get(key).map(Value::view)
        {
            return c.clone();
        }
        let mut data = match shared.get(atomic_key).map(Value::view) {
            Some(ValueView::Hash(h)) => h.as_ref().clone(),
            _ => crate::value::HashData::default(),
        };
        let seed = match data.map.get(key) {
            Some(v) => {
                if let ValueView::ContainerRef(c) = v.view() {
                    return c.clone();
                }
                v.clone()
            }
            None => Value::int(0),
        };
        let cell = crate::gc::Gc::new(std::sync::Mutex::new(seed));
        data.map
            .insert(key.to_string(), Value::container_ref(cell.clone()));
        let updated = Value::hash_with_data(crate::gc::Gc::new(data));
        shared.insert(atomic_key.to_string(), updated.clone());
        shared.insert(hash_name.to_string(), updated.clone());
        drop(shared);
        self.env.insert(hash_name.to_string(), updated);
        cell
    }

    /// The element cell at `idx` in the celled atomic array store, creating it
    /// (one COW, padding missing slots with fresh `0`-cells) when needed.
    pub(super) fn celled_array_elem(
        &mut self,
        atomic_key: &str,
        arr_name: &str,
        index: i64,
    ) -> crate::gc::Gc<std::sync::Mutex<Value>> {
        let resolve =
            |arr: &Value, index: i64| -> (usize, Option<crate::gc::Gc<std::sync::Mutex<Value>>>) {
                if let ValueView::Array(elements, _) = arr.view() {
                    let idx = if index < 0 {
                        (elements.len() as i64 + index).max(0) as usize
                    } else {
                        index as usize
                    };
                    if let Some(ValueView::ContainerRef(c)) = elements.get(idx).map(Value::view) {
                        return (idx, Some(c.clone()));
                    }
                    (idx, None)
                } else {
                    (index.max(0) as usize, None)
                }
            };
        {
            // ADR-0010: atomics are process-wide shared state -> the root lineage.
            let atomic_root = self.shared_vars.root_store();
            let shared = atomic_root.own_map().read().unwrap();
            if let Some(arr) = shared.get(atomic_key) {
                let (_, cell) = resolve(arr, index);
                if let Some(c) = cell {
                    return c;
                }
            }
        }
        // ADR-0010: atomics are process-wide shared state -> the root lineage.
        let atomic_root = self.shared_vars.root_store();
        let mut shared = atomic_root.own_map().write().unwrap();
        let arr = shared
            .get(atomic_key)
            .cloned()
            .unwrap_or(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
                crate::value::ArrayKind::Array,
            ));
        let (idx, cell) = resolve(&arr, index);
        if let Some(c) = cell {
            return c;
        }
        let (mut data, kind) = match arr.view() {
            ValueView::Array(a, kind) => (a.as_ref().clone(), kind),
            _ => (
                crate::value::ArrayData::new(Vec::new()),
                crate::value::ArrayKind::Array,
            ),
        };
        while data.items.len() <= idx {
            data.items.push(Self::boxed_elem_cell(Value::int(0)));
        }
        let seed = {
            let v = &data.items[idx];
            if let ValueView::ContainerRef(c) = v.view() {
                return c.clone();
            }
            v.clone()
        };
        let cell = crate::gc::Gc::new(std::sync::Mutex::new(seed));
        data.items[idx] = Value::container_ref(cell.clone());
        let updated = Value::array_with_kind(crate::gc::Gc::new(data), kind);
        shared.insert(atomic_key.to_string(), updated.clone());
        drop(shared);
        // Arrays deliberately skip the env mirror: GetLocal consults the
        // atomic shared key directly (see builtin_cas_array_elem's note).
        let _ = arr_name;
        cell
    }

    /// Thread-safe `@arr.push(...)` (and `.unshift`) in shared (threaded)
    /// context. (and `.unshift`) in shared (threaded)
    /// context.
    ///
    /// A plain `.push` in a thread reads `@arr` from the thread's *local* env
    /// snapshot, pushes, and only writes back later via `set_shared_var` — so
    /// concurrent threads each start from the same stale snapshot and clobber
    /// each other's pushes (lost update). Route the mutation through the same
    /// `__mutsu_atomic_arr::` shared store the CAS array ops use: a single
    /// lock-protected read-modify-write under the `shared_vars` write lock
    /// serializes all threads, and `set_shared_var` already refuses to
    /// overwrite a key that has an active atomic entry. (Mutating the *base*
    /// key instead is unsound: a parent thread's env write of its stale
    /// snapshot lands via `set_shared_var` on the base key and wipes every
    /// push a worker committed there — the t/lock.t "Lock::Async protects
    /// shared array pushes" lost-update race.) `prepend` inserts the items at
    /// the front (preserving order) for `unshift`. Returns the new array.
    ///
    /// Only plain lexical `@name`s may funnel in here: the store is keyed by
    /// name, so per-instance identities (attribute `@!x`/`@.x`, twigil'd
    /// `@*dyn`) would wrongly accumulate across every object. Callers gate on
    /// `is_plain_lexical_array_name`.
    pub(crate) fn shared_array_extend(
        &mut self,
        arr_name: &str,
        items: Vec<Value>,
        prepend: bool,
    ) -> Value {
        let (_, updated) = self.shared_array_mutate(arr_name, |elements, kind| {
            if prepend {
                for (i, it) in items.into_iter().enumerate() {
                    elements.insert(i, it);
                }
            } else {
                elements.extend(items);
            }
            if *kind == crate::value::ArrayKind::List {
                *kind = crate::value::ArrayKind::Array;
            }
        });
        updated
    }

    /// Whether `@arr` already has an authoritative `__mutsu_atomic_arr::`
    /// entry (created by a prior shared push/extend/CAS). Once it exists,
    /// reads prefer it, so every subsequent mutation must go through
    /// `shared_array_mutate` or it is silently lost.
    pub(crate) fn atomic_array_entry_exists(&self, arr_name: &str) -> bool {
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");
        matches!(
            self.shared_vars
                .root_store()
                .own_map()
                .read()
                .unwrap()
                .get(&atomic_key)
                .map(Value::view),
            Some(ValueView::Array(..))
        )
    }

    /// Generic thread-safe read-modify-write of a plain lexical `@arr` through
    /// the `__mutsu_atomic_arr::` shared store: seeds the atomic entry (same
    /// contract as `shared_array_extend`), applies `f` to the `ArrayData`
    /// under the `shared_vars` write lock, and marks the user-visible name
    /// dirty. Returns `f`'s result (e.g. a popped element) plus the updated
    /// array value. Every mutating array op in shared context must funnel
    /// through here: once the atomic entry exists, reads prefer it, so a
    /// mutation applied anywhere else (a stale base/env copy) is invisible —
    /// the zef `populate-distributions` append-loss bug.
    pub(crate) fn shared_array_mutate<R>(
        &mut self,
        arr_name: &str,
        f: impl FnOnce(&mut crate::value::ArrayData, &mut crate::value::ArrayKind) -> R,
    ) -> (R, Value) {
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");
        let is_thread_clone = self.is_thread_clone();
        if is_thread_clone {
            // Drop this thread's env copy so the atomic entry's Gc stays
            // uniquely referenced and `make_mut` below mutates in place
            // (O(1) amortized) instead of a full-array COW per push.
            self.env.remove(arr_name);
        }
        let (result, updated) = {
            // ADR-0010: atomics are process-wide shared state -> the root lineage.
            let atomic_root = self.shared_vars.root_store();
            let mut shared = atomic_root.own_map().write().unwrap();
            // Seed the atomic entry once from the base key (or this thread's
            // local snapshot), preserving ArrayData metadata (default/
            // initialized/type). Afterwards the atomic entry is authoritative
            // and is mutated in place under the write lock.
            if !matches!(
                shared.get(&atomic_key).map(Value::view),
                Some(ValueView::Array(..))
            ) {
                let seed = match shared
                    .get(arr_name)
                    .or_else(|| self.env.get(arr_name))
                    .map(Value::view)
                {
                    Some(ValueView::Array(elems, _)) => elems.as_ref().clone(),
                    _ => crate::value::ArrayData::default(),
                };
                shared.insert(
                    atomic_key.clone(),
                    Value::array_with_kind(
                        crate::gc::Gc::new(seed),
                        crate::value::ArrayKind::Array,
                    ),
                );
            }
            let Some(slot) = shared.get_mut(&atomic_key) else {
                unreachable!("atomic array entry seeded just above");
            };
            slot.with_array_mut(|arc_items, kind| {
                let elements = crate::gc::Gc::make_mut(arc_items);
                let result = f(elements, kind);
                (
                    result,
                    Value::array_with_kind(crate::gc::Gc::clone(arc_items), *kind),
                )
            })
            .expect("atomic array entry seeded just above")
        };
        // Mark the user-visible name dirty so `sync_shared_vars_to_env`
        // propagates the merged array back to the parent thread.
        if is_thread_clone {
            // Per-key env marker: mark dirty once, and keep env free of a
            // competing Gc handle (reads prefer the atomic entry anyway).
            let dirty_marker = format!("__mutsu_shared_dirty::{arr_name}");
            if !self.env.contains_key(&dirty_marker) {
                self.mark_shared_var_dirty(arr_name);
                self.env.insert(dirty_marker, Value::TRUE);
            }
        } else {
            self.mark_shared_var_dirty(arr_name);
            // Update the local env so this thread observes its own push
            // immediately even on direct env reads.
            self.env.insert(arr_name.to_string(), updated.clone());
        }
        (result, updated)
    }

    /// Thread-safe `@arr[$i] = $v` in shared (threaded) context.
    ///
    /// Mirrors `shared_array_extend`: a single lock-protected read-modify-write
    /// through the `__mutsu_atomic_arr::` shared store, so concurrent
    /// `start { @a[...] = ... }` blocks each writing a different index all land
    /// instead of clobbering a stale snapshot via `set_shared_var`. Grows the
    /// array with `Nil` holes up to `idx`. Returns the assigned element value.
    pub(crate) fn shared_array_elem_set(
        &mut self,
        arr_name: &str,
        idx: usize,
        value: Value,
    ) -> Value {
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");
        // Track B cell fast path: an already-celled slot is assigned through
        // its cell in place — every snapshot holder sees it, no COW, no
        // republish.
        {
            // ADR-0010: atomics are process-wide shared state -> the root lineage.
            let atomic_root = self.shared_vars.root_store();
            let shared = atomic_root.own_map().read().unwrap();
            if let Some(ValueView::Array(elems, _)) = shared.get(&atomic_key).map(Value::view)
                && let Some(ValueView::ContainerRef(c)) = elems.get(idx).map(Value::view)
            {
                let cell = c.clone();
                drop(shared);
                *cell.lock().unwrap_or_else(|e| e.into_inner()) = value.clone();
                if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                    dirty.insert(arr_name.to_string());
                }
                return value;
            }
        }
        let updated = {
            // ADR-0010: atomics are process-wide shared state -> the root lineage.
            let atomic_root = self.shared_vars.root_store();
            let mut shared = atomic_root.own_map().write().unwrap();
            let mut elements: Vec<Value> = match shared.get(&atomic_key).map(Value::view) {
                Some(ValueView::Array(elems, _)) => elems.to_vec(),
                _ => match shared
                    .get(arr_name)
                    .or_else(|| self.env.get(arr_name))
                    .map(Value::view)
                {
                    Some(ValueView::Array(elems, _)) => elems.to_vec(),
                    _ => Vec::new(),
                },
            };
            if idx >= elements.len() {
                elements.resize(idx + 1, Value::NIL);
            }
            elements[idx] = value.clone();
            let new_arr = Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(elements)),
                crate::value::ArrayKind::Array,
            );
            shared.insert(atomic_key, new_arr.clone());
            new_arr
        };
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(arr_name.to_string());
        }
        self.env.insert(arr_name.to_string(), updated);
        value
    }

    /// Thread-safe `%h{$k} = $v` in shared (threaded) context.
    ///
    /// The hash analogue of `shared_array_elem_set`: a single lock-protected
    /// read-modify-write through the `__mutsu_atomic_hash::` shared store, so
    /// concurrent `start { %h{...} = ... }` blocks each writing a different key
    /// all land. Returns the assigned element value.
    pub(crate) fn shared_hash_elem_set(
        &mut self,
        hash_name: &str,
        elem_key: String,
        value: Value,
    ) -> Value {
        let atomic_key = format!("__mutsu_atomic_hash::{hash_name}");
        // Track B cell fast path — see `shared_array_elem_set`.
        {
            // ADR-0010: atomics are process-wide shared state -> the root lineage.
            let atomic_root = self.shared_vars.root_store();
            let shared = atomic_root.own_map().read().unwrap();
            if let Some(ValueView::Hash(h)) = shared.get(&atomic_key).map(Value::view)
                && let Some(ValueView::ContainerRef(c)) = h.get(&elem_key).map(Value::view)
            {
                let cell = c.clone();
                drop(shared);
                *cell.lock().unwrap_or_else(|e| e.into_inner()) = value.clone();
                if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                    dirty.insert(hash_name.to_string());
                }
                return value;
            }
        }
        let updated = {
            // ADR-0010: atomics are process-wide shared state -> the root lineage.
            let atomic_root = self.shared_vars.root_store();
            let mut shared = atomic_root.own_map().write().unwrap();
            let mut map = match shared.get(&atomic_key).map(Value::view) {
                Some(ValueView::Hash(h)) => h.as_ref().clone(),
                _ => match shared
                    .get(hash_name)
                    .or_else(|| self.env.get(hash_name))
                    .map(Value::view)
                {
                    Some(ValueView::Hash(h)) => h.as_ref().clone(),
                    _ => crate::value::HashData::default(),
                },
            };
            Value::hash_insert_through(&mut map.map, elem_key, value.clone());
            let new_hash = Value::hash_with_data(crate::gc::Gc::new(map));
            shared.insert(atomic_key, new_hash.clone());
            new_hash
        };
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(hash_name.to_string());
        }
        self.env.insert(hash_name.to_string(), updated);
        value
    }

    /// CAS on a multi-dimensional array element: cas(@arr[d1;d2;...], $expected, $new)
    /// Args: [array_name_str, dimensions_list, expected, new_val]
    pub(super) fn builtin_cas_array_multidim(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if args.len() != 4 {
            return Err(RuntimeError::new(
                "__mutsu_cas_array_multidim requires 4 arguments",
            ));
        }
        let arr_name = args[0].to_string_value();
        let dims: Vec<i64> = match args[1].view() {
            ValueView::Array(elems, ..) => elems
                .iter()
                .map(|v| match v.view() {
                    ValueView::Int(i) => i,
                    _ => v.to_string_value().parse::<i64>().unwrap_or(0),
                })
                .collect(),
            _ => vec![0],
        };
        let expected = &args[2];
        let new_val = args[3].clone();

        // Typed container: reject a wrong-typed swap value before the compare
        // (raku checks it even when the compare fails — roadmap T5).
        self.check_atomic_elem_type(&arr_name, &new_val)?;

        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");

        // Track B element cells (T4, gc-post-3a-roadmap §2): same template as
        // the 1-dim `builtin_cas_array_elem` — box top-level elements at first
        // atomic touch, then run the whole nested compare+set under the
        // top-level slot's cell lock. The inner structure is COW'd *inside*
        // the cell, so no whole-array republish and no per-op O(container)
        // copy of the top level; concurrent CAS on the same top-level slot
        // serialize on the cell mutex, and every snapshot holder shares the
        // cell. This also makes 1-dim and multidim CAS on the same array
        // coherent (the old republish path read plain elements and returned 0
        // once a 1-dim CAS had celled the store).
        self.init_celled_atomic_store(&atomic_key, &arr_name);
        let cell =
            self.celled_array_elem(&atomic_key, &arr_name, dims.first().copied().unwrap_or(0));
        let inner_dims = if dims.len() > 1 { &dims[1..] } else { &[] };
        let mut did_swap = false;
        let current;
        {
            let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
            if inner_dims.is_empty() {
                current = guard.clone();
                if Self::cas_retry_matches(&current, expected) {
                    *guard = new_val;
                    did_swap = true;
                }
            } else {
                current = Self::multidim_get(&guard, inner_dims);
                if Self::cas_retry_matches(&current, expected) {
                    *guard = Self::multidim_set(&guard, inner_dims, new_val);
                    did_swap = true;
                }
            }
        }

        if did_swap && let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(arr_name.clone());
        }
        Ok(current)
    }

    /// Get an element from a multi-dimensional array by navigating nested
    /// arrays. Reads through `ContainerRef` element cells transparently.
    pub(super) fn multidim_get(arr: &Value, dims: &[i64]) -> Value {
        let mut current = arr.deref_container();
        for &dim in dims {
            if let ValueView::Array(elements, ..) = current.view() {
                let idx = if dim < 0 {
                    (elements.len() as i64 + dim) as usize
                } else {
                    dim as usize
                };
                let next = elements.get(idx).cloned().unwrap_or(Value::int(0));
                current = next.into_deref();
            } else {
                return Value::int(0);
            }
        }
        current
    }

    /// Set an element in a multi-dimensional array by navigating nested arrays.
    /// Returns the updated top-level array. Writes *through* a `ContainerRef`
    /// element cell where one exists (every snapshot holder shares the cell),
    /// COW-rebuilding only the plain nesting levels.
    pub(super) fn multidim_set(arr: &Value, dims: &[i64], value: Value) -> Value {
        if dims.is_empty() {
            return value;
        }
        if let ValueView::ContainerRef(c) = arr.view() {
            let cell = c.clone();
            let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
            let updated = Self::multidim_set(&guard, dims, value);
            *guard = updated;
            drop(guard);
            return arr.clone();
        }
        if let ValueView::Array(elements, kind) = arr.view() {
            let idx = if dims[0] < 0 {
                (elements.len() as i64 + dims[0]) as usize
            } else {
                dims[0] as usize
            };
            let mut new_elements = (**elements).clone();
            while new_elements.len() <= idx {
                new_elements.push(Value::int(0));
            }
            if dims.len() == 1 {
                Value::assign_element_slot(&mut new_elements[idx], value);
            } else {
                new_elements[idx] = Self::multidim_set(&new_elements[idx], &dims[1..], value);
            }
            Value::array_with_kind(crate::gc::Gc::new(new_elements), kind)
        } else {
            arr.clone()
        }
    }

    /// After CAS updates an attribute variable (`!attr_name`), update the
    /// corresponding Instance object in env ("self") and store the updated
    /// Instance in shared_vars so the main thread can pick it up after await.
    /// Phase 3 cell-CAS: resolve an attribute-twigil atomic target (`!x`/`.x`)
    /// to `self`'s shared attribute cell and the map key, preferring the method
    /// owner class's qualified private key (Parent/Child same-named `$!priv`
    /// disambiguation, matching the VM's cell-direct access). Returns `None`
    /// when not in an instance method context, falling back to the shared_vars
    /// atomic machinery for plain variables.
    pub(super) fn self_attr_cell_target(
        &self,
        name: &str,
    ) -> Option<(crate::gc::Gc<crate::value::InstanceAttrs>, String)> {
        let bare = name.strip_prefix('!').or_else(|| name.strip_prefix('.'))?;
        if !bare
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
        {
            return None;
        }
        let Some(ValueView::Instance { attributes, .. }) = self.env.get("self").map(Value::view)
        else {
            return None;
        };
        let attrs = attributes.clone();
        let key = {
            let map = attrs.as_map();
            match self.method_class_stack.last() {
                Some(owner) => {
                    let qualified = format!("{}\0{}", owner, bare);
                    if map.contains_key(&qualified) {
                        qualified
                    } else {
                        bare.to_string()
                    }
                }
                None => bare.to_string(),
            }
        };
        Some((attrs, key))
    }

    /// CAS on a hash element: cas(%hash{key}, &code)
    /// Args: [hash_name_str, key, code]
    /// Uses shared_vars with an atomic key for cross-thread safety.
    pub(super) fn builtin_cas_hash_elem(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if args.len() != 3 {
            return Err(RuntimeError::new(
                "__mutsu_cas_hash_elem requires 3 arguments (hash_name, key, code)",
            ));
        }
        let hash_name = args[0].to_string_value();
        let key = args[1].to_string_value();
        let code = args[2].clone();
        let atomic_key = format!("__mutsu_atomic_hash::{hash_name}");

        // Track B element cells: box every element at the container's first
        // atomic touch, then RMW individual elements in place through their
        // cell — no whole-map COW per op (see `init_celled_atomic_store`).
        self.init_celled_atomic_store(&atomic_key, &hash_name);
        let cell = self.celled_hash_elem(&atomic_key, &hash_name, &key);

        // Check if code is {.succ} or {.pred} for fast path
        if let ValueView::Sub(sub) = code.view() {
            let effective_body: Vec<&Stmt> = sub
                .body
                .iter()
                .filter(|s| !matches!(s, Stmt::SetLine(_)))
                .collect();
            if sub.params.is_empty()
                && effective_body.len() == 1
                && let Stmt::Expr(Expr::MethodCall {
                    target,
                    name: method_name,
                    args: method_args,
                    ..
                }) = effective_body[0]
                && method_args.is_empty()
                && matches!(target.as_ref(), Expr::Var(v) if v == "_" || v == "$_")
            {
                let method_str = method_name.resolve();
                let delta = if method_str == "succ" {
                    Some(1i64)
                } else if method_str == "pred" {
                    Some(-1i64)
                } else {
                    None
                };
                if let Some(d) = delta {
                    // `.succ`/`.pred` never re-enter the VM, so the whole RMW
                    // runs under the element cell's own lock — one locked
                    // add, no retry, no COW, no republish (every snapshot
                    // shares this cell).
                    let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
                    let current = guard.clone();
                    *guard = crate::builtins::arith_add(current, Value::int(d))?;
                    return Ok(Value::NIL);
                }
            }
        }

        // General CAS retry loop over the element cell, with typed-constraint
        // enforcement (shared with the array code form — see
        // `builtins_atomic_cas_code.rs`).
        self.cas_cell_code_loop(&hash_name, &cell, &code)
    }
}
