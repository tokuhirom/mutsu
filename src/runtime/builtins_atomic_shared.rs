//! Thread-safe shared-container mutations (`@arr.push`/`@arr[i] = v`/`%h{k} = v`
//! routed through the `__mutsu_atomic_*::` shared store), multi-dimensional and
//! hash-element compare-and-swap, and the instance-attribute cell resolver
//! (`self_attr_cell_target`) shared with `builtins_atomic`/`builtins_atomic_cas`.

use super::*;

impl Interpreter {
    /// Thread-safe `@arr.push(...)` (and `.unshift`) in shared (threaded)
    /// context.
    ///
    /// A plain `.push` in a thread reads `@arr` from the thread's *local* env
    /// snapshot, pushes, and only writes back later via `set_shared_var` — so
    /// concurrent threads each start from the same stale snapshot and clobber
    /// each other's pushes (lost update). Route the mutation through the same
    /// `__mutsu_atomic_arr::` shared store the CAS array ops use: a single
    /// lock-protected read-modify-write under the `shared_vars` write lock
    /// serializes all threads, and `set_shared_var` already refuses to
    /// overwrite a key that has an active atomic entry. `prepend` inserts the
    /// items at the front (preserving order) for `unshift`. Returns the new
    /// array.
    pub(crate) fn shared_array_extend(
        &mut self,
        arr_name: &str,
        items: Vec<Value>,
        prepend: bool,
    ) -> Value {
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");
        let updated = {
            let mut shared = self.shared_vars.write().unwrap();
            // Seed from the authoritative source: the atomic entry if present
            // (another thread already pushed), else the shared base key, else
            // this thread's local snapshot, else an empty array.
            let mut elements: Vec<Value> = match shared.get(&atomic_key) {
                Some(Value::Array(elems, _)) => elems.to_vec(),
                _ => match shared.get(arr_name).or_else(|| self.env.get(arr_name)) {
                    Some(Value::Array(elems, _)) => elems.to_vec(),
                    _ => Vec::new(),
                },
            };
            if prepend {
                for (i, it) in items.into_iter().enumerate() {
                    elements.insert(i, it);
                }
            } else {
                elements.extend(items);
            }
            let new_arr = Value::Array(
                std::sync::Arc::new(crate::value::ArrayData::new(elements)),
                crate::value::ArrayKind::Array,
            );
            shared.insert(atomic_key, new_arr.clone());
            new_arr
        };
        // Mark the user-visible name dirty so `sync_shared_vars_to_env`
        // propagates the merged array back to the parent thread.
        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
            dirty.insert(arr_name.to_string());
        }
        // Update the local env so this thread observes its own push immediately.
        self.env.insert(arr_name.to_string(), updated.clone());
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
                std::sync::Arc::new(crate::value::ArrayData::new(elements)),
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
            let new_hash = Value::Hash(std::sync::Arc::new(map));
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
                    std::sync::Arc::new(crate::value::ArrayData::new(Vec::new())),
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
                std::sync::Arc::new(crate::value::ArrayData::new(Vec::new())),
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
            Value::Array(std::sync::Arc::new(new_elements), *kind)
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
    ) -> Option<(std::sync::Arc<crate::value::InstanceAttrs>, String)> {
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

        // Initialize shared_vars with the hash if not yet set
        {
            let shared = self.shared_vars.read().unwrap();
            if !shared.contains_key(&atomic_key) {
                drop(shared);
                let hash = self
                    .env
                    .get(&hash_name)
                    .cloned()
                    .unwrap_or_else(|| Value::Hash(Value::hash_arc(HashMap::new())));
                let mut shared = self.shared_vars.write().unwrap();
                if !shared.contains_key(&atomic_key) {
                    shared.insert(atomic_key.clone(), hash);
                }
            }
        }

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
                    let mut shared = self.shared_vars.write().unwrap();
                    let hash = shared
                        .get(&atomic_key)
                        .cloned()
                        .unwrap_or_else(|| Value::Hash(Value::hash_arc(HashMap::new())));
                    if let Value::Hash(ref map) = hash {
                        let current = map.get(&key).cloned().unwrap_or(Value::Int(0));
                        let new_val = crate::builtins::arith_add(current, Value::Int(d))?;
                        let mut new_map = (**map).clone();
                        new_map.insert(key, new_val);
                        let updated = Value::Hash(Value::hash_arc(new_map));
                        shared.insert(atomic_key.clone(), updated.clone());
                        shared.insert(hash_name.clone(), updated.clone());
                        drop(shared);
                        self.env.insert(hash_name.clone(), updated);
                        if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                            dirty.insert(atomic_key);
                            dirty.insert(hash_name);
                        }
                        return Ok(Value::Nil);
                    }
                }
            }
        }

        // General CAS loop for hash elements
        loop {
            let current = {
                let shared = self.shared_vars.read().unwrap();
                let hash = shared
                    .get(&atomic_key)
                    .cloned()
                    .unwrap_or_else(|| Value::Hash(Value::hash_arc(HashMap::new())));
                if let Value::Hash(ref map) = hash {
                    map.get(&key).cloned().unwrap_or(Value::Int(0))
                } else {
                    Value::Int(0)
                }
            };
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
            // CAS: check if value is still `current`, if so store `new_val`
            let mut shared = self.shared_vars.write().unwrap();
            let hash = shared
                .get(&atomic_key)
                .cloned()
                .unwrap_or_else(|| Value::Hash(Value::hash_arc(HashMap::new())));
            if let Value::Hash(ref map) = hash {
                let seen = map.get(&key).cloned().unwrap_or(Value::Int(0));
                if Self::cas_retry_matches(&current, &seen) {
                    let mut new_map = (**map).clone();
                    new_map.insert(key, new_val);
                    let updated = Value::Hash(Value::hash_arc(new_map));
                    shared.insert(atomic_key.clone(), updated.clone());
                    shared.insert(hash_name.clone(), updated.clone());
                    drop(shared);
                    self.env.insert(hash_name.clone(), updated);
                    if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                        dirty.insert(atomic_key);
                        dirty.insert(hash_name);
                    }
                    return Ok(Value::Nil);
                }
            }
            drop(shared);
        }
    }
}
