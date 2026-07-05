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
            Value::ContainerRef(crate::gc::Gc::new(std::sync::Mutex::new(v)))
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
            let shared = self.shared_vars.read().unwrap();
            if shared.contains_key(atomic_key) {
                return;
            }
        }
        let base = self
            .env
            .get(name)
            .cloned()
            .or_else(|| self.get_shared_var(name));
        let celled = match base {
            Some(Value::Hash(h)) => {
                let mut data = h.as_ref().clone();
                for v in data.map.values_mut() {
                    let taken = std::mem::replace(v, Value::Nil);
                    *v = Self::boxed_elem_cell(taken);
                }
                Value::Hash(crate::gc::Gc::new(data))
            }
            Some(Value::Array(a, kind)) => {
                let mut data = a.as_ref().clone();
                for v in data.items.iter_mut() {
                    let taken = std::mem::replace(v, Value::Nil);
                    *v = Self::boxed_elem_cell(taken);
                }
                Value::Array(crate::gc::Gc::new(data), kind)
            }
            _ => {
                if name.starts_with('@') {
                    Value::Array(
                        crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
                        crate::value::ArrayKind::Array,
                    )
                } else {
                    Value::Hash(Value::hash_arc(HashMap::new()))
                }
            }
        };
        let mut shared = self.shared_vars.write().unwrap();
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
            let shared = self.shared_vars.read().unwrap();
            if let Some(Value::Hash(h)) = shared.get(atomic_key)
                && let Some(Value::ContainerRef(c)) = h.get(key)
            {
                return c.clone();
            }
        }
        let mut shared = self.shared_vars.write().unwrap();
        // Re-check under the write lock (a racer may have boxed it).
        if let Some(Value::Hash(h)) = shared.get(atomic_key)
            && let Some(Value::ContainerRef(c)) = h.get(key)
        {
            return c.clone();
        }
        let mut data = match shared.get(atomic_key) {
            Some(Value::Hash(h)) => h.as_ref().clone(),
            _ => crate::value::HashData::default(),
        };
        let seed = match data.map.get(key) {
            Some(Value::ContainerRef(c)) => return c.clone(),
            Some(v) => v.clone(),
            None => Value::Int(0),
        };
        let cell = crate::gc::Gc::new(std::sync::Mutex::new(seed));
        data.map
            .insert(key.to_string(), Value::ContainerRef(cell.clone()));
        let updated = Value::Hash(crate::gc::Gc::new(data));
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
                if let Value::Array(elements, _) = arr {
                    let idx = if index < 0 {
                        (elements.len() as i64 + index).max(0) as usize
                    } else {
                        index as usize
                    };
                    if let Some(Value::ContainerRef(c)) = elements.get(idx) {
                        return (idx, Some(c.clone()));
                    }
                    (idx, None)
                } else {
                    (index.max(0) as usize, None)
                }
            };
        {
            let shared = self.shared_vars.read().unwrap();
            if let Some(arr) = shared.get(atomic_key) {
                let (_, cell) = resolve(arr, index);
                if let Some(c) = cell {
                    return c;
                }
            }
        }
        let mut shared = self.shared_vars.write().unwrap();
        let arr = shared.get(atomic_key).cloned().unwrap_or(Value::Array(
            crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
            crate::value::ArrayKind::Array,
        ));
        let (idx, cell) = resolve(&arr, index);
        if let Some(c) = cell {
            return c;
        }
        let (mut data, kind) = match arr {
            Value::Array(a, kind) => (a.as_ref().clone(), kind),
            _ => (
                crate::value::ArrayData::new(Vec::new()),
                crate::value::ArrayKind::Array,
            ),
        };
        while data.items.len() <= idx {
            data.items.push(Self::boxed_elem_cell(Value::Int(0)));
        }
        let seed = match &data.items[idx] {
            Value::ContainerRef(c) => return c.clone(),
            v => v.clone(),
        };
        let cell = crate::gc::Gc::new(std::sync::Mutex::new(seed));
        data.items[idx] = Value::ContainerRef(cell.clone());
        let updated = Value::Array(crate::gc::Gc::new(data), kind);
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
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");
        let is_thread_clone = self.is_thread_clone();
        if is_thread_clone {
            // Drop this thread's env copy so the atomic entry's Gc stays
            // uniquely referenced and `make_mut` below mutates in place
            // (O(1) amortized) instead of a full-array COW per push.
            self.env.remove(arr_name);
        }
        let updated = {
            let mut shared = self.shared_vars.write().unwrap();
            // Seed the atomic entry once from the base key (or this thread's
            // local snapshot), preserving ArrayData metadata (default/
            // initialized/type). Afterwards the atomic entry is authoritative
            // and is mutated in place under the write lock.
            if !matches!(shared.get(&atomic_key), Some(Value::Array(..))) {
                let seed = match shared.get(arr_name).or_else(|| self.env.get(arr_name)) {
                    Some(Value::Array(elems, _)) => elems.as_ref().clone(),
                    _ => crate::value::ArrayData::default(),
                };
                shared.insert(
                    atomic_key.clone(),
                    Value::Array(crate::gc::Gc::new(seed), crate::value::ArrayKind::Array),
                );
            }
            let Some(Value::Array(arc_items, kind)) = shared.get_mut(&atomic_key) else {
                unreachable!("atomic array entry seeded just above");
            };
            let elements = crate::gc::Gc::make_mut(arc_items);
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
            Value::Array(crate::gc::Gc::clone(arc_items), *kind)
        };
        // Mark the user-visible name dirty so `sync_shared_vars_to_env`
        // propagates the merged array back to the parent thread.
        if is_thread_clone {
            // Per-key env marker: mark dirty once, and keep env free of a
            // competing Gc handle (reads prefer the atomic entry anyway).
            let dirty_marker = format!("__mutsu_shared_dirty::{arr_name}");
            if !self.env.contains_key(&dirty_marker) {
                self.mark_shared_var_dirty(arr_name);
                self.env.insert(dirty_marker, Value::Bool(true));
            }
        } else {
            self.mark_shared_var_dirty(arr_name);
            // Update the local env so this thread observes its own push
            // immediately even on direct env reads.
            self.env.insert(arr_name.to_string(), updated.clone());
        }
        updated
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
            let shared = self.shared_vars.read().unwrap();
            if let Some(Value::Array(elems, _)) = shared.get(&atomic_key)
                && let Some(Value::ContainerRef(c)) = elems.get(idx)
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
            let mut shared = self.shared_vars.write().unwrap();
            let mut elements: Vec<Value> = match shared.get(&atomic_key) {
                Some(Value::Array(elems, _)) => elems.to_vec(),
                _ => match shared.get(arr_name).or_else(|| self.env.get(arr_name)) {
                    Some(Value::Array(elems, _)) => elems.to_vec(),
                    _ => Vec::new(),
                },
            };
            if idx >= elements.len() {
                elements.resize(idx + 1, Value::Nil);
            }
            elements[idx] = value.clone();
            let new_arr = Value::Array(
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
            let shared = self.shared_vars.read().unwrap();
            if let Some(Value::Hash(h)) = shared.get(&atomic_key)
                && let Some(Value::ContainerRef(c)) = h.get(&elem_key)
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
            let mut shared = self.shared_vars.write().unwrap();
            let mut map = match shared.get(&atomic_key) {
                Some(Value::Hash(h)) => h.as_ref().clone(),
                _ => match shared.get(hash_name).or_else(|| self.env.get(hash_name)) {
                    Some(Value::Hash(h)) => h.as_ref().clone(),
                    _ => crate::value::HashData::default(),
                },
            };
            Value::hash_insert_through(&mut map.map, elem_key, value.clone());
            let new_hash = Value::Hash(crate::gc::Gc::new(map));
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
        let dims: Vec<i64> = match &args[1] {
            Value::Array(elems, ..) => elems
                .iter()
                .map(|v| match v {
                    Value::Int(i) => *i,
                    other => other.to_string_value().parse::<i64>().unwrap_or(0),
                })
                .collect(),
            _ => vec![0],
        };
        let expected = &args[2];
        let new_val = args[3].clone();

        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");

        // Initialize shared_vars with the array if not yet set
        {
            let shared = self.shared_vars.read().unwrap();
            if !shared.contains_key(&atomic_key) {
                drop(shared);
                let arr = self.env.get(&arr_name).cloned().unwrap_or(Value::Array(
                    crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
                    crate::value::ArrayKind::Array,
                ));
                let mut shared = self.shared_vars.write().unwrap();
                if !shared.contains_key(&atomic_key) {
                    shared.insert(atomic_key.clone(), arr);
                }
            }
        }

        let mut did_swap = false;
        let current;
        {
            let mut shared = self.shared_vars.write().unwrap();
            let arr = shared.get(&atomic_key).cloned().unwrap_or(Value::Array(
                crate::gc::Gc::new(crate::value::ArrayData::new(Vec::new())),
                crate::value::ArrayKind::Array,
            ));
            // Navigate to the element using the dimension indices
            current = Self::multidim_get(&arr, &dims);
            if Self::cas_retry_matches(&current, expected) {
                let updated = Self::multidim_set(&arr, &dims, new_val);
                shared.insert(atomic_key.clone(), updated);
                did_swap = true;
            }
        }

        if did_swap && let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(arr_name.clone());
        }
        Ok(current)
    }

    /// Get an element from a multi-dimensional array by navigating nested arrays.
    fn multidim_get(arr: &Value, dims: &[i64]) -> Value {
        let mut current = arr.clone();
        for &dim in dims {
            if let Value::Array(ref elements, ..) = current {
                let idx = if dim < 0 {
                    (elements.len() as i64 + dim) as usize
                } else {
                    dim as usize
                };
                current = elements.get(idx).cloned().unwrap_or(Value::Int(0));
            } else {
                return Value::Int(0);
            }
        }
        current
    }

    /// Set an element in a multi-dimensional array by navigating nested arrays.
    /// Returns the updated top-level array.
    fn multidim_set(arr: &Value, dims: &[i64], value: Value) -> Value {
        if dims.is_empty() {
            return value;
        }
        if let Value::Array(elements, kind) = arr {
            let idx = if dims[0] < 0 {
                (elements.len() as i64 + dims[0]) as usize
            } else {
                dims[0] as usize
            };
            let mut new_elements = (**elements).clone();
            while new_elements.len() <= idx {
                new_elements.push(Value::Int(0));
            }
            if dims.len() == 1 {
                new_elements[idx] = value;
            } else {
                new_elements[idx] = Self::multidim_set(&new_elements[idx], &dims[1..], value);
            }
            Value::Array(crate::gc::Gc::new(new_elements), *kind)
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
        let Some(Value::Instance { attributes, .. }) = self.env.get("self") else {
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
        if let Value::Sub(ref sub) = code {
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
                    *guard = crate::builtins::arith_add(current, Value::Int(d))?;
                    return Ok(Value::Nil);
                }
            }
        }

        // General CAS retry loop over the element cell. The user closure runs
        // OUTSIDE the cell lock (it re-enters the VM — the Track B re-entrancy
        // rule shared with GC safepoints, ADR-0001 §3-6): read, compute, then
        // compare-and-store under the lock, retrying on interference.
        loop {
            let current = cell.lock().unwrap_or_else(|e| e.into_inner()).clone();
            let new_val = {
                let call_args = if let Value::Sub(ref sub) = code {
                    if sub.params.is_empty() {
                        self.env.insert("_".to_string(), current.clone());
                        self.env.insert("$_".to_string(), current.clone());
                        Vec::new()
                    } else {
                        vec![current.clone()]
                    }
                } else {
                    vec![current.clone()]
                };
                self.call_sub_value(code.clone(), call_args, true)?
            };
            let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
            if Self::cas_retry_matches(&current, &guard) {
                *guard = new_val;
                return Ok(Value::Nil);
            }
        }
    }
}
