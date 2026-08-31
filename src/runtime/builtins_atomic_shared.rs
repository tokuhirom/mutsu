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
            Value::container_ref(crate::gc::Gc::new(crate::value::ContainerCell::new(v)))
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
            // The atomic-arr/hash lane resolves at the lineage owning `name`,
            // not unconditionally at root (see `atomic_lane_scope`).
            let atomic_root = self.shared_vars.atomic_lane_scope(name);
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
                data.clear_native_storage();
                for v in data.items_mut().iter_mut() {
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
        let atomic_root = self.shared_vars.atomic_lane_scope(name);
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
    ) -> crate::gc::Gc<crate::value::ContainerCell> {
        {
            let atomic_root = self.shared_vars.atomic_lane_scope(hash_name);
            let shared = atomic_root.own_map().read().unwrap();
            if let Some(ValueView::Hash(h)) = shared.get(atomic_key).map(Value::view)
                && let Some(ValueView::ContainerRef(c)) = h.get(key).map(Value::view)
            {
                return c.clone();
            }
        }
        let atomic_root = self.shared_vars.atomic_lane_scope(hash_name);
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
        let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(seed));
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
    ) -> crate::gc::Gc<crate::value::ContainerCell> {
        let resolve = |arr: &Value,
                       index: i64|
         -> (usize, Option<crate::gc::Gc<crate::value::ContainerCell>>) {
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
            let atomic_root = self.shared_vars.atomic_lane_scope(arr_name);
            let shared = atomic_root.own_map().read().unwrap();
            if let Some(arr) = shared.get(atomic_key) {
                let (_, cell) = resolve(arr, index);
                if let Some(c) = cell {
                    return c;
                }
            }
        }
        let atomic_root = self.shared_vars.atomic_lane_scope(arr_name);
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
            ValueView::Array(a, kind) => {
                let mut data = a.as_ref().clone();
                data.clear_native_storage();
                (data, kind)
            }
            _ => (
                crate::value::ArrayData::new(Vec::new()),
                crate::value::ArrayKind::Array,
            ),
        };
        while data.items().len() <= idx {
            data.items_mut().push(Self::boxed_elem_cell(Value::int(0)));
        }
        let seed = {
            let v = &data.items()[idx];
            if let ValueView::ContainerRef(c) = v.view() {
                return c.clone();
            }
            v.clone()
        };
        let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(seed));
        data.items_mut()[idx] = Value::container_ref(cell.clone());
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
    /// Whether `@arr` is *genuinely shared* across threads: it already has an
    /// authoritative `__mutsu_atomic_arr::` entry, or the base name itself is
    /// bound in the cross-thread store (seeded when a thread was spawned while
    /// this lexical was live). A name that is in neither is frame-local, and
    /// routing its mutations through the name-keyed store would detach it from
    /// every other binding of the same container.
    pub(crate) fn array_name_is_shared(&self, arr_name: &str) -> bool {
        // ...unless this lineage re-declared the name: then the store's entry
        // belongs to the shadowed outer binding, not to this frame's array.
        if self.container_name_is_redeclared(arr_name) {
            return false;
        }
        self.atomic_array_entry_exists(arr_name)
            || self
                .shared_vars
                .get(arr_name)
                .is_some_and(|v| matches!(v.view(), ValueView::Array(..)))
    }

    pub(crate) fn atomic_array_entry_exists(&self, arr_name: &str) -> bool {
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");
        matches!(
            self.shared_vars
                .atomic_lane_scope(arr_name)
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
            let atomic_root = self.shared_vars.atomic_lane_scope(arr_name);
            let mut shared = atomic_root.own_map().write().unwrap();
            // Seed the atomic entry once from the base key (or this thread's
            // local snapshot), preserving ArrayData metadata (default/
            // initialized/type). Afterwards the atomic entry is authoritative
            // and is mutated in place under the write lock.
            if !matches!(
                shared.get(&atomic_key).map(Value::view),
                Some(ValueView::Array(..))
            ) {
                // The env binding may be a `ContainerRef` cell rather than a
                // bare Array — every lexical a closure captures is boxed into
                // one, which is exactly the shape a *module* file-scope `@a`
                // has when its own subs reach it. Reading the cell's view
                // without dereferencing it matched neither arm and seeded the
                // atomic entry EMPTY, silently dropping everything already in
                // the array (`Test.rakumod`'s `@vars` subtest stack lost its
                // outer frame the first time a test file spawned a thread).
                //
                // ADR-0039 slice 1: a compunit's own file-scope `@`/`%` (or a
                // mainline named sub's captured free variable) lives in
                // `unit_lexicals`, NOT `env` — `env[arr_name]` may hold a
                // completely unrelated same-named binding (the loading
                // scope's own `my @items`). Prefer it over the plain `env`
                // fallback for the same reason every other ADR-0039 write
                // chokepoint does.
                let local = shared
                    .get(arr_name)
                    .cloned()
                    .or_else(|| self.unit_lexical_container(arr_name))
                    .or_else(|| self.env.get(arr_name).cloned())
                    .map(|v| v.deref_container());
                let seed = match local.as_ref().map(Value::view) {
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
            // immediately even on direct env reads. A `ContainerRef` binding is
            // written *through*: the cell is the array's identity for every
            // other holder (a closure's captured copy, a `:=` alias), so
            // replacing the binding with a bare Array would detach them all and
            // freeze them at the pre-mutation contents.
            //
            // ADR-0039 slice 1: a unit-lexical container's cell (module
            // file-scope, or a mainline named sub's captured free variable)
            // takes priority over `env` for the identical reason as the seed
            // step above — writing into `env[arr_name]` here would either
            // silently vanish (nothing reads it back) or, worse, pollute the
            // loading scope's own same-named `env` entry.
            if let Some(cell) = self.unit_lexical_container_cell(arr_name) {
                *cell.lock().unwrap_or_else(|e| e.into_inner()) = updated.clone();
            } else if let Some(ValueView::ContainerRef(cell)) =
                self.env.get(arr_name).map(Value::view)
            {
                *cell.lock().unwrap_or_else(|e| e.into_inner()) = updated.clone();
            } else {
                self.env.insert(arr_name.to_string(), updated.clone());
            }
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
            let atomic_root = self.shared_vars.atomic_lane_scope(arr_name);
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
            let atomic_root = self.shared_vars.atomic_lane_scope(arr_name);
            let mut shared = atomic_root.own_map().write().unwrap();
            // ADR-0039 slice 1: fall back to the unit-lexical container
            // (module file-scope / mainline captured free var) before the
            // plain `env` entry, and deref through a `ContainerRef` binding
            // either way — see `shared_array_mutate`'s twin comment.
            let mut elements: Vec<Value> = match shared.get(&atomic_key).map(Value::view) {
                Some(ValueView::Array(elems, _)) => elems.to_vec(),
                _ => match shared
                    .get(arr_name)
                    .cloned()
                    .or_else(|| self.unit_lexical_container(arr_name))
                    .or_else(|| self.env.get(arr_name).cloned())
                    .map(|v| v.deref_container())
                {
                    Some(v) => match v.view() {
                        ValueView::Array(elems, _) => elems.to_vec(),
                        _ => Vec::new(),
                    },
                    None => Vec::new(),
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
        if let Some(cell) = self.unit_lexical_container_cell(arr_name) {
            *cell.lock().unwrap_or_else(|e| e.into_inner()) = updated.clone();
        } else if let Some(ValueView::ContainerRef(cell)) = self.env.get(arr_name).map(Value::view)
        {
            *cell.lock().unwrap_or_else(|e| e.into_inner()) = updated.clone();
        } else {
            self.env.insert(arr_name.to_string(), updated);
        }
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
            let atomic_root = self.shared_vars.atomic_lane_scope(hash_name);
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
            let atomic_root = self.shared_vars.atomic_lane_scope(hash_name);
            let mut shared = atomic_root.own_map().write().unwrap();
            // ADR-0039 slice 1: see `shared_array_elem_set`'s twin comment.
            let mut map = match shared.get(&atomic_key).map(Value::view) {
                Some(ValueView::Hash(h)) => h.as_ref().clone(),
                _ => match shared
                    .get(hash_name)
                    .cloned()
                    .or_else(|| self.unit_lexical_container(hash_name))
                    .or_else(|| self.env.get(hash_name).cloned())
                    .map(|v| v.deref_container())
                {
                    Some(v) => match v.view() {
                        ValueView::Hash(h) => h.as_ref().clone(),
                        _ => crate::value::HashData::default(),
                    },
                    None => crate::value::HashData::default(),
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
        if let Some(cell) = self.unit_lexical_container_cell(hash_name) {
            *cell.lock().unwrap_or_else(|e| e.into_inner()) = updated.clone();
        } else if let Some(ValueView::ContainerRef(cell)) = self.env.get(hash_name).map(Value::view)
        {
            *cell.lock().unwrap_or_else(|e| e.into_inner()) = updated.clone();
        } else {
            self.env.insert(hash_name.to_string(), updated);
        }
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
    /// The shared `ContainerRef` cell backing a plain lexical, if a closure
    /// boxed it (`box_captured_lexicals`). See the call site in `builtin_cas_var`.
    pub(super) fn scalar_cell_target(
        &self,
        name: &str,
    ) -> Option<crate::gc::Gc<crate::value::ContainerCell>> {
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            return None;
        }
        let v = self
            .env
            .get(name)
            .or_else(|| self.env.get(name.trim_start_matches('$')))?;
        match v.view() {
            ValueView::ContainerRef(c) => Some(c.clone()),
            _ => None,
        }
    }

    /// [`scalar_cell_target`], promoting a plain atomic-scalar binding to a
    /// shared `ContainerRef` cell on first use.
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
    /// Only a name the RUNNING frame declares as its own local is boxed: that
    /// frame owns the binding, so its slot and `env` can be updated together —
    /// the same pairing `box_captured_lexicals` performs. A captured outer
    /// lexical reached from a closure frame is left alone unless the closure
    /// machinery already boxed it (in which case the lookup above found it).
    /// True when the name-keyed legacy atomic lane currently owns `bare`'s
    /// value, i.e. an earlier `cas`/`atomic-*` on this name was refused a cell
    /// and parked the authoritative value in `__mutsu_atomic_value::N`.
    ///
    /// While that is the case the lane — not the frame slot — is the source of
    /// truth, so promoting the binding to a `ContainerRef` cell *now* would
    /// seed the cell from a stale slot and fork the binding in two: that is the
    /// mid-sequence promotion hazard
    /// `news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md` documents.
    /// The seed-and-retire protocol that makes promotion safe is deliberately
    /// confined to [`Self::atomic_scalar_cell`] (see its doc comment: it runs
    /// synchronously in the thread that owns the atomic op, so there is no
    /// racing sibling to steal the lane from). Closure-capture and declaration
    /// boxing have no such guarantee, so they must simply DECLINE — a refusal
    /// can only cost an optimisation, never correctness.
    ///
    /// Cheap by construction: the process-global "any atomic ever seen" flag
    /// short-circuits it in programs that use no atomics at all.
    pub(crate) fn legacy_atomic_lane_owns(&self, bare: &str) -> bool {
        if !Self::atomic_var_seen_anywhere() {
            return false;
        }
        // The lane is keyed by the canonical atomic name, which may or may not
        // carry the `$` sigil depending on how the op spelled its argument.
        self.legacy_atomic_value(bare).is_some()
            || self.legacy_atomic_value(&format!("${bare}")).is_some()
    }

    /// The value the legacy name-keyed atomic lane currently holds for `name`,
    /// if it has an entry at all.
    fn legacy_atomic_value(&self, name: &str) -> Option<Value> {
        if !Self::atomic_var_seen_anywhere() {
            return None;
        }
        let name_key = Self::atomic_shared_name_key(name);
        let value_key = self
            .env
            .get(&name_key)
            .cloned()
            .or_else(|| self.shared_vars.get(&name_key))?;
        let value_key = value_key.as_str()?.to_string();
        self.shared_vars.get(&value_key)
    }

    /// [`scalar_cell_target`] fallback: promote a plain atomic-scalar binding
    /// to a shared `ContainerRef` cell on first use.
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
    /// `news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md`). This function
    /// is safe because it runs synchronously within the SAME thread whose own
    /// atomic op is being performed on its OWN declared local ("Only a name
    /// the RUNNING frame declares as its own local is boxed", below) — there
    /// is no cross-thread race to seed from or retire out from under.
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
    pub(super) fn atomic_scalar_cell(
        &mut self,
        name: &str,
    ) -> Option<crate::gc::Gc<crate::value::ContainerCell>> {
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
                // Peek at the legacy lane without retiring yet -- see the
                // seed-and-retire protocol in the doc comment above.
                // Retiring happens only after the shape check below
                // confirms this value will actually be boxed, so a refused
                // shape doesn't lose the legacy entry.
                let legacy = self.legacy_atomic_value(name);
                let cur = match &legacy {
                    Some(v) => v.clone(),
                    None => self.locals.get(slot)?.clone(),
                };
                // Only plain scalar containers are boxed; reference types
                // already share, and hiding a type object / Proxy behind a
                // `ContainerRef` trips the paths that do not deref one. `Any`
                // is the uninitialized-scalar seed and is boxed like a value
                // (mirrors `box_captured_lexicals`, including its
                // Seq/HyperSeq/RaceSeq/Slip exclusion --
                // `news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md`).
                if !cur.is_any_type_object()
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
                {
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
                if self.shared_vars_active
                    && !self.thread_redeclared_vars.borrow().contains(name)
                    && !self.thread_redeclared_vars.borrow().contains(bare)
                {
                    self.set_shared_var(bare, container.clone());
                }
                return match container.view() {
                    ValueView::ContainerRef(c) => Some(c.clone()),
                    _ => None,
                };
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
        self.box_package_scope_lexical_cell(bare)
    }

    /// [`Self::atomic_scalar_cell`]'s fallback for a package-scoped `my`
    /// lexical (a class-body "static") that has no frame-local slot in the
    /// currently executing chunk. See the call site for the full rationale.
    fn box_package_scope_lexical_cell(
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
        // Only plain scalar containers are boxed; reference types already
        // share, and hiding a type object / Proxy behind a `ContainerRef`
        // trips the paths that do not deref one. `Any` is the
        // uninitialized-scalar seed and is boxed like a value (mirrors
        // `box_captured_lexicals`, including its Seq/HyperSeq/RaceSeq/Slip
        // exclusion --
        // `news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md`).
        if !cur.is_any_type_object()
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
        {
            return None;
        }
        let container = cur.into_container_ref();
        self.package_lexicals
            .get_mut(&pkg)?
            .insert(bare.to_string(), container.clone());
        match container.view() {
            ValueView::ContainerRef(c) => Some(c.clone()),
            _ => None,
        }
    }

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
