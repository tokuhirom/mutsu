use super::vm_var_assign_ops::BindSourceCell;
use super::*;

impl Interpreter {
    pub(super) fn exec_index_assign_expr_named_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
    ) -> Result<(), RuntimeError> {
        // A lazy `@`-array must reify its prefix before an element assignment
        // (`@a[i] = v`) — the assign machinery needs a materialized backing. (L2)
        self.reify_lazy_array_slot(Self::const_str(code, name_idx))?;
        // Slice 2b: `@aoa[i] = @row` / `%h<k> = @row` was compiled as a `:=` bind
        // (so the bind machinery installs a shared `ContainerRef` cell and
        // promotes the source) plus a `MarkElementShare` flag. Capture which
        // element to mark as a `=` value share — a simple Int/Str subscript — so
        // a later non-share reassignment REPLACES the slot instead of writing
        // through the shared cell. Complex subscripts keep pure `:=` semantics.
        let elem_share_mark: Option<(String, String)> = if self.element_share_pending {
            self.element_share_pending = false;
            let var_name = Self::const_str(code, name_idx).to_string();
            self.stack.last().and_then(|idx| match idx {
                Value::Int(n) if *n >= 0 => Some((var_name, idx.to_string_value())),
                Value::Str(_) => Some((var_name, idx.to_string_value())),
                _ => None,
            })
        } else {
            None
        };
        // --- Track C: shared hash/array element assignment across threads ---
        // `%h{$k} = $v` / `@a[$i] = $v` inside a `start` block must write through
        // the shared cell so concurrent threads all land (snapshot semantics
        // otherwise lose updates).
        if let Some(result) = self.try_shared_hash_element_assign(code, name_idx) {
            return result;
        }
        if let Some(result) = self.try_shared_array_element_assign(code, name_idx) {
            return result;
        }
        // --- Fast path for simple hash element assignment ---
        // Handles the common case: %h{$key} = $val with no type constraints,
        // no binding, no special containers. Skips ~16 HashMap lookups.
        if let Some(result) = self.try_fast_hash_element_assign(code, name_idx, is_positional) {
            return result;
        }
        // Save type metadata and container default by pointer BEFORE the
        // inner op runs. Auto-vivification and Arc::make_mut may
        // reconstruct the array Arc, changing the pointer used as the
        // metadata key. Reapply them on the final container so typed-array
        // hole semantics and `is default(...)` are preserved.
        let save_var_name = Self::const_str(code, name_idx).to_string();
        // Hash type metadata (including the object-hash key constraint) is now
        // embedded in `HashData` and travels with the hash across copy-on-write,
        // so the old name-based reconcile healing is no longer needed.
        let saved_type_meta_outer = self
            .env()
            .get(&save_var_name)
            .cloned()
            .and_then(|v| self.container_type_metadata(&v));
        // Guard against stale pointer-keyed defaults (Arc reuse across
        // allocations): only trust the saved default when a name-based
        // var_default is also registered.
        let saved_default_outer = if self.var_default(&save_var_name).is_some() {
            self.env()
                .get(&save_var_name)
                .and_then(|v| self.container_default(v).cloned())
        } else {
            None
        };
        let result = self.exec_index_assign_expr_named_op_inner(code, name_idx, is_positional);
        // Restore metadata on the post-assignment container when the
        // identity-keyed map lost it OR holds a stale entry. Copy-on-write
        // changes the hash's Arc pointer (the metadata key), and freed pointers
        // get reused by later allocations carrying *different* stale metadata,
        // so a mere `.is_none()` check leaves a reused pointer's wrong entry in
        // place — re-register whenever the current entry differs from the value
        // saved before the assignment. Object-hash element reads (`%h{$int}`)
        // detect their key constraint only through this pointer-keyed metadata
        // (the read op has no variable name to fall back on), so a stale/lost
        // entry silently degrades them to string-keyed lookups returning Nil.
        if let Some(info) = saved_type_meta_outer
            && let Some(container) = self.env().get(&save_var_name).cloned()
            && self.container_type_metadata(&container).as_ref() != Some(&info)
        {
            // Hashes embed metadata in `HashData`, so the re-tagged value must
            // be written back into both env and the fast-path local slot
            // (`tag_container_metadata` returns the same Arc for non-hash
            // containers, whose Arc-pointer side table is updated in place).
            let tagged = self.tag_container_metadata(container, info);
            self.env_mut().insert(save_var_name.clone(), tagged.clone());
            self.locals_set_by_name(code, &save_var_name, tagged);
        }
        if let Some(def) = saved_default_outer
            && let Some(container) = self.env().get(&save_var_name).cloned()
            && self.container_default(&container).is_none()
        {
            let tagged = self.tag_container_default(container, def);
            self.env_mut().insert(save_var_name.clone(), tagged.clone());
            self.locals_set_by_name(code, &save_var_name, tagged);
        }
        // Object-hash original keys are embedded in `HashData` and travel with
        // the hash across copy-on-write, so no pointer migration is needed.
        // Slice 2b: now that the shared cell is installed in the element, record
        // it as a `=` value share so a later non-share reassignment replaces it.
        if result.is_ok()
            && let Some((var_name, encoded)) = elem_share_mark
        {
            self.mark_element_share(&var_name, encoded);
        }
        // Object index-assign (`$obj[i] = v` / `$obj{k} = v` dispatching
        // ASSIGN-POS/ASSIGN-KEY to an Instance or Mixin that does Positional/
        // Associative) writes the mutated object back into `env[var]` but the
        // inner op does not refresh the caller's local slot. The default build's
        // blanket env reconcile carries this; make it a precise slot write-through
        // so the `MUTSU_NO_BLANKET_RECONCILE` single-store path (and the eventual
        // `env_dirty` removal) keeps the slot coherent. Plain Array/Hash element
        // assigns already update the slot via the fast paths and never reach here
        // as an Instance/Mixin, so this only fires for object subscript targets.
        if result.is_ok()
            && matches!(
                self.env().get(&save_var_name),
                Some(Value::Instance { .. }) | Some(Value::Mixin(..))
            )
            && let Some(v) = self.env().get(&save_var_name).cloned()
        {
            self.locals_set_by_name(code, &save_var_name, v);
        }
        result
    }

    fn exec_index_assign_expr_named_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
    ) -> Result<(), RuntimeError> {
        let original_var_name = Self::const_str(code, name_idx).to_string();
        // Resolve sigilless alias: if `h` is a sigilless alias for `%a`,
        // operate on `%a` directly so in-place mutations are visible.
        let sigilless_alias_target = {
            let alias_key = format!("__mutsu_sigilless_alias::{}", original_var_name);
            self.env().get(&alias_key).and_then(|v| {
                if let Value::Str(target) = v {
                    Some(target.to_string())
                } else {
                    None
                }
            })
        };
        let var_name = sigilless_alias_target
            .as_deref()
            .unwrap_or(&original_var_name)
            .to_string();
        // Pre-compute fill value for native typed arrays (e.g. int->0, num->0e0, str->"")
        // Must be done before mutable borrows to avoid borrow conflicts.
        let native_fill = {
            let tc = loan_env!(self, var_type_constraint(&var_name));
            Self::native_fill_for_constraint(tc.as_deref())
        };
        // Capture the old Arc pointer before any mutation so the post-mutation
        // sync code can distinguish stale COW copies from unrelated containers.
        let old_container_arc_ptr: Option<usize> =
            if let Some(container) = self.env().get(&var_name) {
                match container {
                    Value::Array(arc, _) => Some(Arc::as_ptr(arc) as usize),
                    Value::Hash(arc) => Some(Arc::as_ptr(arc) as usize),
                    _ => None,
                }
            } else {
                None
            };
        let declared_type = self
            .env()
            .get(&var_name)
            .cloned()
            .and_then(|v| self.container_type_metadata(&v))
            .and_then(|info| info.declared_type);
        let _target_is_mixhash = declared_type.as_deref().is_some_and(|t| t == "MixHash");
        let _target_is_baghash = declared_type.as_deref().is_some_and(|t| t == "BagHash");
        let _target_is_sethash = declared_type.as_deref().is_some_and(|t| t == "SetHash");
        let declared_shape_key = format!("__mutsu_shaped_array_dims::{var_name}");
        let has_declared_shape = self.env().contains_key(&declared_shape_key);
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        // An *itemized* list/Range subscript (`@a[$(7,8,9)] = …`) is a SINGLE
        // index (its `.Int`, the element count), not a slice — itemization makes
        // it one item. An itemized list reaches here as `ArrayKind::ItemList` (or
        // a `Scalar`-wrapped list/Range). A bare `@a[7,8,9] = …` stays a slice.
        let idx = match &idx {
            Value::Array(items, crate::value::ArrayKind::ItemList) => {
                Value::Int(items.len() as i64)
            }
            Value::Scalar(inner)
                if inner.is_range() || matches!(inner.as_ref(), Value::Array(..)) =>
            {
                Value::Int(crate::runtime::utils::value_to_list(inner).len() as i64)
            }
            _ => idx,
        };
        let index_target = self.env().get(&var_name).cloned();
        let idx = self.resolve_whatever_index_for_target(idx, index_target.as_ref());
        // Normalize Seq/Slip/Range index to Array for uniform handling in assignment.
        // For hash variables, expand Range indices into individual keys so that
        // `%h{^5} = (0 xx 5)` performs a hash slice assignment (5 separate keys)
        // instead of treating the stringified range as a single key.
        // Native typed arrays (e.g. `array[num]`) need numeric Range slice
        // indices expanded to an explicit index list so the slice-assign path
        // distributes each value to a single element (rather than checking the
        // whole RHS list against the scalar element type).
        let vtc_native = loan_env!(self, var_type_constraint(&var_name))
            .as_deref()
            .is_some_and(crate::runtime::native_types::is_native_array_element_type);
        let ct_native = index_target
            .as_ref()
            .cloned()
            .and_then(|v| self.container_type_metadata(&v))
            .is_some_and(|info| {
                crate::runtime::native_types::is_native_array_element_type(&info.value_type)
            });
        let array_var_is_native = !var_name.starts_with('%')
            && matches!(index_target, Some(Value::Array(..)))
            && (vtc_native || ct_native);
        let expand_range = var_name.starts_with('%') || array_var_is_native;
        let idx = match idx {
            Value::Seq(items) => Value::Array(
                crate::value::Value::array_arc(items.to_vec()),
                crate::value::ArrayKind::List,
            ),
            Value::Slip(items) => Value::Array(
                crate::value::Value::array_arc(items.to_vec()),
                crate::value::ArrayKind::List,
            ),
            Value::Range(a, b) if expand_range => {
                let items: Vec<Value> = (a..=b).map(Value::Int).collect();
                Value::Array(
                    Arc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::List,
                )
            }
            Value::RangeExcl(a, b) if expand_range => {
                let items: Vec<Value> = (a..b).map(Value::Int).collect();
                Value::Array(
                    Arc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::List,
                )
            }
            Value::RangeExclStart(a, b) if expand_range => {
                let items: Vec<Value> = ((a + 1)..=b).map(Value::Int).collect();
                Value::Array(
                    Arc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::List,
                )
            }
            Value::RangeExclBoth(a, b) if expand_range => {
                let items: Vec<Value> = ((a + 1)..b).map(Value::Int).collect();
                Value::Array(
                    Arc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::List,
                )
            }
            other => other,
        };
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let (val, bind_mode, bind_sources) = match raw_val {
            Value::Pair(name, payload) if name == "__mutsu_bind_index_value" => match *payload {
                Value::Array(items, ..) if items.len() >= 2 => {
                    let value = items.first().cloned().unwrap_or(Value::Nil);
                    let sources = match items.get(1) {
                        Some(Value::Array(srcs, ..)) => srcs
                            .iter()
                            .map(|src| match src {
                                Value::Str(s) if !s.is_empty() => Some((**s).clone()),
                                _ => None,
                            })
                            .collect(),
                        _ => Vec::new(),
                    };
                    (value, true, sources)
                }
                other => (other, true, Vec::new()),
            },
            other => (other, false, Vec::new()),
        };
        // For typed container elements, explicit `is default(...)` wins over
        // the nominal type-object fallback when Nil is assigned.
        let val = if matches!(val, Value::Nil) {
            if let Some(default) = self.var_default(&var_name) {
                default.clone()
            } else if let Some(constraint) = loan_env!(self, var_type_constraint(&var_name)) {
                let nominal = loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                Value::Package(Symbol::intern(&nominal))
            } else {
                val
            }
        } else {
            val
        };
        // Map containers are immutable - prevent assignment and binding to keys
        if declared_type.as_deref().is_some_and(|t| t == "Map") {
            if bind_mode {
                return Err(RuntimeError::typed(
                    "X::Bind",
                    [("target".to_string(), Value::str(var_name.clone()))]
                        .into_iter()
                        .collect(),
                ));
            } else {
                return Err(RuntimeError::assignment_ro_typename("Map", "Map"));
            }
        }
        // Mix/Set/Bag: prevent auto-vivification of undefined typed variables.
        // When `my Mix $m; $m<key> = val`, the variable is undefined (Nil or
        // type object) but has an immutable type constraint.
        {
            let current_val = self.env().get(&var_name).cloned();
            let is_undefined = current_val.is_none()
                || matches!(&current_val, Some(Value::Nil))
                || matches!(&current_val, Some(Value::Package(_)));
            if is_undefined {
                let constraint_owned = loan_env!(self, var_type_constraint(&var_name));
                let type_name_check = declared_type.as_deref().or(constraint_owned.as_deref());
                if type_name_check.is_some_and(|t| matches!(t, "Mix" | "Set" | "Bag")) {
                    let type_name = type_name_check.unwrap_or("Mix");
                    return Err(RuntimeError::new(format!(
                        "Cannot auto-vivify an immutable {}",
                        type_name
                    )));
                }
            }
        }
        // Immutable List/Range containers - prevent assignment and binding
        if let Some(target_val) = self.env().get(&var_name) {
            let is_immutable = matches!(
                target_val,
                Value::Array(_, crate::value::ArrayKind::List)
                    | Value::Array(_, crate::value::ArrayKind::ItemList)
                    | Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. }
            );
            if is_immutable {
                if bind_mode {
                    return Err(RuntimeError::typed(
                        "X::Bind",
                        [("target".to_string(), Value::str(var_name.clone()))]
                            .into_iter()
                            .collect(),
                    ));
                }
                // For List containers: allow assignment through a Scalar element.
                // `my $l := List.new: 1, 2, my $ = 3` stores a Value::Scalar at
                // position 2; `$l[2] = 42` should update that Scalar in-place.
                let list_scalar_hit = if let Value::Array(items, kind) = target_val
                    && (kind == &crate::value::ArrayKind::List
                        || kind == &crate::value::ArrayKind::ItemList)
                    && let Some(i) = Self::index_to_usize(&idx)
                    && matches!(items.get(i), Some(Value::Scalar(_)))
                {
                    Some((items.clone(), i))
                } else {
                    None
                };
                if let Some((items, i)) = list_scalar_hit {
                    // Update the Scalar element in-place.
                    // SAFETY: aliased in-place mutation of a shared list backing;
                    // see `arc_contents_mut`. No borrow into the items is live
                    // across the write. (The old code cast the `ArrayData` pointer
                    // straight to `*mut Vec<Value>`, assuming `items` sits at
                    // offset 0; this types it properly as `&mut ArrayData`.)
                    let data = unsafe { crate::value::arc_contents_mut(&items) };
                    data.items[i] = Value::Scalar(Box::new(val.clone()));
                    self.stack.push(val);
                    return Ok(());
                }
                let type_name = match target_val {
                    Value::Array(..) => "List",
                    _ => "Range",
                };
                let display = target_val.to_string_value();
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Cannot modify an immutable {} ({})",
                        type_name, display
                    )),
                );
                attrs.insert("value".to_string(), target_val.clone());
                return Err(RuntimeError::typed("X::Assignment::RO", attrs));
            }
        }
        let encoded_idx = Self::encode_bound_index(&idx);
        // Slice 2b: is the existing element a `=` value share (`@aoa[i] = @row`)?
        // A non-bind reassignment to such an element REPLACES the shared cell
        // (raku value semantics) instead of writing through it. Precomputed here
        // because the element-write chokepoints below hold a `&mut` borrow of the
        // container and cannot call `self`; the marker is cleared after the store.
        let elem_is_value_share =
            !bind_mode && self.array_share_active && self.is_element_share(&var_name, &encoded_idx);
        // Native typed arrays store unboxed scalars and cannot bind containers to
        // their elements: `my num @a; @a[0] := $x` is illegal.
        if bind_mode
            && var_name.starts_with('@')
            && let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
            && crate::runtime::native_types::is_native_array_element_type(&constraint)
        {
            return Err(RuntimeError::new(format!(
                "Cannot bind to a native {} array",
                crate::runtime::native_types::native_family_name(&constraint)
            )));
        }
        let is_bound_index = if bind_mode {
            self.is_bound_index(&var_name, &encoded_idx)
        } else {
            false
        };
        if !bind_mode && self.is_bound_index(&var_name, &encoded_idx) {
            // `:=`-bound elements are shared `ContainerRef` cells now (no
            // sentinel back-references), so any remaining bound-index metadata
            // for an existing non-cell element is stale (e.g. the binding was
            // broken by splice or an array reset) — clean it up; the write
            // proceeds either way (cell writes go through the cell arm).
            if let Some(Value::Array(items, ..)) = self.env().get(&var_name)
                && let Some(i) = Self::index_to_usize(&idx)
                && matches!(items.get(i), Some(v) if !v.is_container_ref())
            {
                self.remove_bound_index(&var_name, &encoded_idx);
            }
        }
        if !self.env().contains_key(&var_name)
            && let Some(slot) = self.find_local_slot(code, &var_name)
        {
            self.set_env_with_main_alias(&var_name, self.locals[slot].clone());
        }
        // Junction autothreading: when writing back with a junction index,
        // expand the junction and assign each element separately.
        if let Value::Junction {
            values: junc_keys, ..
        } = &idx
        {
            let junc_vals = if let Value::Junction { values: jv, .. } = &val {
                jv.clone()
            } else {
                // Same value for all junction elements
                Arc::new(vec![val.clone(); junc_keys.len()])
            };
            for (i, key) in junc_keys.iter().enumerate() {
                let v = junc_vals.get(i).cloned().unwrap_or(Value::Nil);
                let k = key.to_string_value();
                if var_name.starts_with('%') {
                    // Descend through any `:=`-bound `ContainerRef` cell so the
                    // junction write reaches the shared container (otherwise it
                    // would detach the bind by overwriting the cell with a fresh
                    // hash — see `my %h := %g; %h{'x'|'y'} = v`).
                    if !matches!(self.env_root_descended_mut(&var_name), Some(Value::Hash(_))) {
                        self.env_mut().insert(
                            var_name.clone(),
                            Value::hash(std::collections::HashMap::new()),
                        );
                    }
                    if let Some(Value::Hash(hash)) = self.env_root_descended_mut(&var_name) {
                        let h = Arc::make_mut(hash);
                        h.insert(k, v);
                    }
                } else if let Some(idx_usize) = Self::index_to_usize(key) {
                    // For array variables with junction index, use numeric indices
                    // (descend through any `:=`-bound cell, same as the hash arm).
                    if let Some(Value::Array(items, ..)) = self.env_root_descended_mut(&var_name) {
                        let arr = Arc::make_mut(items);
                        Self::autoviv_resize(arr, idx_usize + 1, native_fill.clone())?;
                        arr[idx_usize] = v;
                    }
                }
            }
            self.stack.push(val);
            return Ok(());
        }
        match &idx {
            Value::Array(keys, ..) => {
                let mut vals = self.assignment_rhs_values(&val)?;
                // Per-element type check for slice assignment to a typed array,
                // e.g. `my Array @x; @x[0,2] = 2, 3` must reject each Int element.
                if var_name.starts_with('@')
                    && let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
                {
                    for v in &vals {
                        if !matches!(v, Value::Nil) && !self.type_matches_value(&constraint, v) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                &var_name,
                                &constraint,
                                v,
                            ));
                        }
                    }
                }
                // Descend through a whole-container `:=` bound cell (`my @x :=
                // @a`) so an array slice assignment mutates the shared inner
                // Array (every alias observes it) instead of falling through to
                // the hash-slice path below.
                if let Some(container) = self.env_root_descended_mut(&var_name)
                    && matches!(container, Value::Array(..))
                {
                    let is_shaped =
                        has_declared_shape || crate::runtime::utils::is_shaped_array(container);
                    let mut initialized_marks: Vec<String> = Vec::new();
                    if is_shaped {
                        if bind_mode && is_bound_index {
                            return Err(RuntimeError::assignment_ro(None));
                        }
                        let depth = Self::array_depth(container);
                        if depth <= 1 && keys.len() > 1 {
                            // 1D shaped array with multiple indices: slice assignment
                            for (i, key) in keys.iter().enumerate() {
                                let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                                Self::assign_array_multidim(
                                    container,
                                    std::slice::from_ref(key),
                                    v,
                                )?;
                                initialized_marks.push(Self::encode_bound_index(key));
                            }
                        } else {
                            // Multidimensional indexing: @arr[0;0] = 'x'
                            Self::assign_array_multidim(container, keys.as_ref(), val.clone())?;
                            initialized_marks.push(encoded_idx.clone());
                        }
                    } else if keys.is_empty() {
                        // Empty slice assignment (e.g. @n[*] on empty array): no-op
                    } else {
                        // Flat slice assignment: @a[2,3,4,6] = <foo bar foo bar>
                        // Auto-extend the array to accommodate all indices
                        let max_idx = keys
                            .iter()
                            .filter_map(Self::index_to_usize)
                            .max()
                            .unwrap_or(0);
                        if let Value::Array(items, ..) = container {
                            let arr = Arc::make_mut(items);
                            Self::autoviv_resize(arr, max_idx + 1, native_fill.clone())?;
                        }
                        // Assign each value to the corresponding index
                        for (i, key) in keys.iter().enumerate() {
                            let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                            Self::assign_array_multidim(container, std::slice::from_ref(key), v)?;
                            initialized_marks.push(Self::encode_bound_index(key));
                        }
                    }
                    for encoded in initialized_marks {
                        self.mark_initialized_index(&var_name, encoded);
                    }
                    if bind_mode {
                        self.mark_bound_index(&var_name, encoded_idx);
                    }
                    self.stack.push(val);
                    return Ok(());
                }
                if vals.is_empty() {
                    vals.push(Value::Nil);
                }
                // Check value type constraint for hash slice assignment
                if let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
                    && !self.is_container_subclass(&constraint)
                {
                    for v in &vals {
                        if !matches!(v, Value::Nil) && !self.type_matches_value(&constraint, v) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                &var_name,
                                &constraint,
                                v,
                            ));
                        }
                    }
                }
                // Check key type constraint for hash slice assignment
                if let Some(key_constraint) = loan_env!(self, var_hash_key_constraint(&var_name)) {
                    for key in keys.iter() {
                        if !self.type_matches_value(&key_constraint, key) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                &var_name,
                                &key_constraint,
                                key,
                            ));
                        }
                    }
                }
                if !matches!(self.env().get(&var_name), Some(Value::Hash(_))) {
                    self.env_mut().insert(
                        var_name.clone(),
                        Value::hash(std::collections::HashMap::new()),
                    );
                }
                let slice_is_object_hash =
                    loan_env!(self, var_hash_key_constraint(&var_name)).is_some();
                // Phase 2 Stage 2 (hash slice bind): pre-read each bind source
                // before the mutable container borrow, reusing an existing
                // `ContainerRef` cell binding or creating a fresh cell to
                // install back into the source var (the #2914 array pattern).
                let mut slice_bind_cells: Vec<Option<BindSourceCell>> = Vec::new();
                if bind_mode {
                    for (i, _) in keys.iter().enumerate() {
                        let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                        let entry = if let Value::ContainerRef(cell) = &v {
                            // Element source: already promoted to a shared cell.
                            Some((None, cell.clone()))
                        } else if let Some(Some(source_name)) = bind_sources.get(i)
                            && !source_name.contains('\0')
                        {
                            match self.env().get(source_name) {
                                Some(Value::ContainerRef(cell)) => Some((None, cell.clone())),
                                _ => Some((
                                    Some(source_name.clone()),
                                    Arc::new(std::sync::Mutex::new(v)),
                                )),
                            }
                        } else {
                            None
                        };
                        slice_bind_cells.push(entry);
                    }
                }
                let mut pending_source_cells: Vec<(String, Arc<std::sync::Mutex<Value>>)> =
                    Vec::new();
                if let Some(Value::Hash(hash)) = self.env_mut().get_mut(&var_name) {
                    let h = Arc::make_mut(hash);
                    for (i, key) in keys.iter().enumerate() {
                        let k = if slice_is_object_hash {
                            runtime::utils::value_which_key(key)
                        } else {
                            key.to_string_value()
                        };
                        let v = if bind_mode {
                            vals.get(i).cloned().unwrap_or(Value::Nil)
                        } else {
                            vals[i % vals.len()].clone()
                        };
                        if bind_mode
                            && let Some(Some((source_install, cell))) = slice_bind_cells.get(i)
                        {
                            h.insert(k, Value::ContainerRef(cell.clone()));
                            if let Some(source_name) = source_install {
                                pending_source_cells.push((source_name.clone(), cell.clone()));
                            }
                        } else {
                            h.insert(k, v);
                        }
                    }
                    // Store original keys for object hashes (embedded in
                    // HashData, COW-stable) after slice insert.
                    if slice_is_object_hash {
                        let orig = h
                            .original_keys
                            .get_or_insert_with(std::collections::HashMap::new);
                        for key in keys.iter() {
                            let wk = runtime::utils::value_which_key(key);
                            orig.insert(wk, key.clone());
                        }
                    }
                }
                for (source_name, cell) in pending_source_cells {
                    // Bind the source variable to the same cell installed at
                    // the slice entry, so both sides alias one container.
                    let cell_val = Value::ContainerRef(cell);
                    self.set_env_with_main_alias(&source_name, cell_val.clone());
                    self.update_local_if_exists(code, &source_name, &cell_val);
                }
            }
            _ => {
                // For object hashes, use .WHICH as the internal key.
                let key_constraint = if var_name.starts_with('%') {
                    loan_env!(self, var_hash_key_constraint(&var_name))
                } else {
                    None
                };
                let is_object_hash = key_constraint.is_some();
                // A coercion key type (`my %h{Int(Str)}`) coerces the key to the
                // target type before it is stored / `.WHICH`-keyed. A failed
                // coercion (e.g. a non-numeric string into Int) throws, matching
                // raku (`X::Str::Numeric`).
                let idx = if let Some(kc) = &key_constraint
                    && let Some(open) = kc.find('(')
                    && kc.ends_with(')')
                    && open > 0
                    && !self.type_matches_value(&kc[..open], &idx)
                {
                    // A numeric coercion target rejects a non-numeric string the
                    // raku way (`X::Str::Numeric`) rather than silently coercing
                    // it to 0, since the lenient `coerce_value` fast path does not
                    // throw.
                    if matches!(
                        &kc[..open],
                        "Int" | "UInt" | "Num" | "Rat" | "FatRat" | "Real" | "Numeric" | "Complex"
                    ) {
                        runtime::utils::check_str_numeric(&idx)?;
                    }
                    loan_env!(self, try_coerce_value_for_constraint(kc, idx.clone()))?
                } else {
                    idx
                };
                // Determine the keying mode for an object hash. A freshly created
                // or already-`.WHICH`-keyed hash (one carrying `original_keys`) is
                // keyed by `.WHICH`. A *plain-built* object hash — e.g. one bulk
                // assigned from a pair list (`my %h{Mu} = :42a, ...`), whose keys
                // were stringified during hash construction and which carries no
                // `original_keys` — must keep using plain string keys here so that
                // a follow-up element assignment (`%h<c> = ...`) overwrites the
                // existing entry instead of inserting a duplicate under a `.WHICH`
                // key. (Fully unifying object-hash keying across construction,
                // flatten, gist and comparison is a larger change.)
                let use_which = is_object_hash
                    && match &index_target {
                        Some(Value::Hash(h)) => h.original_keys.is_some() || h.map.is_empty(),
                        _ => true,
                    };
                let key = if use_which {
                    runtime::utils::value_which_key(&idx)
                } else {
                    idx.to_string_value()
                };
                let array_elem_constraint = loan_env!(self, var_type_constraint(&var_name));
                // A `%h is BagHash`/`MixHash`/`SetHash` (or `Bag`/`Mix`/`Set`)
                // variable IS that QuantHash: the constraint names the *whole*
                // container, not the element type, and `%h<k> = weight` sets a
                // weight (validated/handled by the QuantHash weight-assign path
                // below), so it must not be element-type-checked against the
                // container type.
                let target_is_quanthash = matches!(
                    &index_target,
                    Some(Value::Mix(..) | Value::Bag(..) | Value::Set(..))
                );
                // A parameterized QuantHash (`BagHash[Int]`, `MixHash[Str]`, ...)
                // constrains its *keys*: `%bh<foo> = 42` on a `BagHash[Int]` must
                // throw because the key "foo" is not an Int. The bracketed key type
                // is carried in the container metadata's `value_type` (e.g.
                // "BagHash[Int]") for Bag/Mix/Set, falling back to `declared_type`.
                if target_is_quanthash
                    && let Some(meta) = index_target
                        .as_ref()
                        .and_then(|c| self.container_type_metadata(c))
                    && let Some(declared) = meta
                        .declared_type
                        .as_deref()
                        .filter(|d| d.contains('['))
                        .or(Some(meta.value_type.as_str()))
                    && let Some(open) = declared.find('[')
                    && declared.ends_with(']')
                {
                    let key_type = &declared[open + 1..declared.len() - 1];
                    // A numeric-looking string key (`%bh<7>`) is an allomorph
                    // (IntStr in Raku) that satisfies a numeric key type, so only a
                    // string that does NOT coerce to the numeric type (`%bh<foo>`)
                    // is rejected.
                    let allomorph_ok = matches!(&idx, Value::Str(s)
                        if crate::runtime::str_numeric::parse_raku_str_to_numeric(s).is_some())
                        && matches!(
                            key_type,
                            "Int" | "UInt" | "Num" | "Rat" | "Real" | "Numeric" | "Cool"
                        );
                    if !matches!(key_type, "" | "Any" | "Mu" | "Str")
                        && !allomorph_ok
                        && !self.type_matches_value(key_type, &idx)
                    {
                        return Err(runtime::utils::type_check_binding_typed_error(
                            key_type, &idx,
                        ));
                    }
                }
                if let Some(constraint) = array_elem_constraint
                    && !target_is_quanthash
                    && !matches!(val, Value::Nil)
                    && !self.type_matches_value(&constraint, &val)
                    // For `$`-sigil variables holding a container (e.g. a `Hash $h`
                    // parameter), a container type constraint describes the whole
                    // container, not its elements, so element assignment like
                    // `$h<k> = v` must not be checked against it. For `@`/`%`
                    // variables the constraint IS the element/value type and must
                    // be enforced (e.g. `my Array @x; @x[0] = 1` is a type error).
                    && (var_name.starts_with('@')
                        || var_name.starts_with('%')
                        || !(matches!(
                            constraint.as_str(),
                            "Hash" | "Array" | "Map" | "List" | "Bag" | "Set" | "Mix"
                                | "BagHash" | "SetHash" | "MixHash" | "Seq"
                        ) || self.is_container_subclass(&constraint)))
                {
                    return Err(runtime::utils::type_check_element_typed_error(
                        &var_name,
                        &constraint,
                        &val,
                    ));
                }
                // Check key type constraint for single-key hash element assignment
                if var_name.starts_with('%')
                    && let Some(key_constraint) =
                        loan_env!(self, var_hash_key_constraint(&var_name))
                    && !self.type_matches_value(&key_constraint, &idx)
                {
                    return Err(runtime::utils::type_check_element_typed_error(
                        &var_name,
                        &key_constraint,
                        &idx,
                    ));
                }
                // Native integer arrays store the wrapped value (`-1` -> `255` in a
                // uint8 array); the assignment expression still yields the original.
                let native_store_val = self.wrap_native_int_for_var(&var_name, val.clone());
                // Resolve GenericRange with WhateverCode endpoints (e.g. @a[*-4 .. *-1] = ...)
                let resolved_idx;
                let idx_for_slice = if let Value::GenericRange { .. } = &idx {
                    let array_len = if let Some(Value::Array(items, ..)) = self.env().get(&var_name)
                    {
                        items.len()
                    } else {
                        0
                    };
                    resolved_idx = self.resolve_generic_range_for_assign(&idx, array_len);
                    resolved_idx.as_ref().unwrap_or(&idx)
                } else {
                    &idx
                };
                let range_slice =
                    if let Some(indices) = Self::slice_indices_from_index(idx_for_slice) {
                        Some((indices, self.assignment_rhs_values(&val)?))
                    } else {
                        None
                    };
                // Per-element type check for slice assignment to a typed array,
                // e.g. `my Array @x; @x[0,2] = 2, 3` must reject each Int element.
                if let Some((_, ref rhs_values)) = range_slice
                    && (var_name.starts_with('@') || var_name.starts_with('%'))
                    && let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
                {
                    for v in rhs_values {
                        if !matches!(v, Value::Nil) && !self.type_matches_value(&constraint, v) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                &var_name,
                                &constraint,
                                v,
                            ));
                        }
                    }
                }
                if let Some(current) = self.env().get(&var_name).cloned()
                    && let Value::Mixin(inner, mixins) = current
                {
                    let mut updated_mixins = (*mixins).clone();
                    let mut assigned_object_slot = false;
                    let delegated_attr_key = if matches!(&idx, Value::Str(_)) {
                        self.delegated_mixin_attr_key(&updated_mixins, "ASSIGN-KEY")
                    } else {
                        self.delegated_mixin_attr_key(&updated_mixins, "ASSIGN-POS")
                    };
                    if let Some(attr_key) = delegated_attr_key
                        && let Some(attr_value) = updated_mixins.get_mut(&attr_key)
                    {
                        assigned_object_slot = Self::assign_mixin_container_slot(
                            attr_value,
                            &idx,
                            &val,
                            &range_slice,
                        )?;
                    }
                    if !assigned_object_slot {
                        for (key, attr_value) in updated_mixins.iter_mut() {
                            if !key.starts_with("__mutsu_attr__") {
                                continue;
                            }
                            if Self::assign_mixin_container_slot(
                                attr_value,
                                &idx,
                                &val,
                                &range_slice,
                            )? {
                                assigned_object_slot = true;
                                break;
                            }
                        }
                    }
                    if assigned_object_slot {
                        self.env_mut().insert(
                            var_name.clone(),
                            Value::Mixin(inner, Arc::new(updated_mixins)),
                        );
                        self.stack.push(val);
                        return Ok(());
                    }
                }
                let mut range_initialized_marks: Vec<String> = Vec::new();
                let mut pending_varref_update: Option<(String, Option<usize>, Value)> = None;
                // Phase 2 Stage 2: a single-level element bind (`@a[i] := ...`,
                // `%h<k> := ...`) stores a shared `ContainerRef` cell. The cell
                // is written back to the source var after the write completes.
                let mut pending_source_cell: Option<(String, Arc<std::sync::Mutex<Value>>)> = None;
                // Whether the bind stored a shared cell at the element (skips
                // the bound-index side table — the cell IS the alias).
                let mut stored_bind_cell = false;
                // Pre-read the bind source before the mutable container borrow.
                // An element source (`:= @b[j]`) arrives already promoted to a
                // shared cell by IndexAutovivifyLazyTerminal; a source variable
                // that is already cell-bound (e.g. `@arr[0] := $x; %h<k> := $x`)
                // must REUSE its existing cell so all aliases stay shared; a
                // plain scalar source gets a fresh cell installed back into the
                // source var after the write. `None` source-install means the
                // cell is already in place.
                let bind_cell: Option<BindSourceCell> = if bind_mode {
                    if let Value::ContainerRef(cell) = &val {
                        Some((None, cell.clone()))
                    } else if let Some(Some(source_name)) = bind_sources.first()
                        && !source_name.contains('\0')
                    {
                        match self.env().get(source_name) {
                            Some(Value::ContainerRef(cell)) => Some((None, cell.clone())),
                            _ => Some((
                                Some(source_name.clone()),
                                Arc::new(std::sync::Mutex::new(val.clone())),
                            )),
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };
                // Pre-compute whether this %-sigiled variable was bound via `:=`.
                // Bound hash variables are marked readonly, so we use that as a
                // signal to allow in-place mutation (preserving shared identity).
                // A `:=` bind also records a dedicated `__mutsu_bound::%name`
                // marker (see `Stmt::MarkBoundContainer`), which distinguishes a
                // writable bound hash from a `constant %M` — both are readonly,
                // but only the former may be mutated in place.
                let is_readonly_hash_var =
                    var_name.starts_with('%') && self.readonly_vars().contains(&var_name);
                let is_bound_hash_var = is_readonly_hash_var
                    && self
                        .env()
                        .contains_key(&format!("__mutsu_bound::{}", var_name));
                // A readonly `%`-var that was NOT `:=`-bound is a `constant %M`
                // (an immutable Map): element assignment must die with
                // X::Assignment::RO, mirroring raku and mutsu's own `constant @A`
                // behavior. Only fire for plain `Value::Hash`; immutable
                // Set/Bag/Mix containers keep their dedicated RO paths below.
                if is_readonly_hash_var
                    && !is_bound_hash_var
                    && matches!(self.env().get(&var_name), Some(Value::Hash(_)))
                {
                    let elem = match self.env().get(&var_name) {
                        Some(Value::Hash(hd)) => hd.map.get(&key).cloned(),
                        _ => None,
                    };
                    return Err(match elem {
                        Some(v) => {
                            let tn = crate::runtime::utils::value_type_name(&v);
                            RuntimeError::assignment_ro_typename(tn, &v.to_string_value())
                        }
                        None => RuntimeError::assignment_ro(None),
                    });
                }
                // Type check for parameterized SetHash[T]/BagHash[T]/MixHash[T]
                // element binding. Only applies when the declared type is explicitly
                // parameterized (e.g. SetHash[Str]), not when the constraint is just
                // `is SetHash`. The subscript key is the element, so it must satisfy
                // the parameterized element (keyof) type.
                let set_val_clone = self
                    .env()
                    .get(&var_name)
                    .filter(|v| matches!(v, Value::Set(..) | Value::Bag(..) | Value::Mix(..)))
                    .cloned();
                // The subscript key is checked against the element (keyof) type.
                // For Set/Bag that equals `value_type`; for Mix the keyof lives in
                // `key_type` (while `value_type` is the weight type, Real).
                let elem_type = set_val_clone.as_ref().and_then(|sv| {
                    self.container_type_metadata(sv).and_then(|info| {
                        if !info
                            .declared_type
                            .as_deref()
                            .is_some_and(|t| t.contains('['))
                        {
                            return None;
                        }
                        info.key_type
                            .filter(|t| !t.is_empty())
                            .or(Some(info.value_type))
                            .filter(|t| !t.is_empty() && t != "Any" && t != "Mu")
                    })
                });
                if let Some(elem_type) = elem_type
                    && !self.type_matches_value(&elem_type, &idx)
                {
                    let got_type = crate::value::what_type_name(&idx);
                    let got_repr = idx.to_string_value();
                    let msg = format!(
                        "Type check failed in binding; expected {} but got {} (\"{}\")",
                        elem_type, got_type, got_repr,
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    attrs.insert("operation".to_string(), Value::str_from("bind"));
                    attrs.insert("got".to_string(), idx.clone());
                    attrs.insert(
                        "expected".to_string(),
                        Value::Package(crate::symbol::Symbol::intern(&elem_type)),
                    );
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::TypeCheck::Binding"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                // When the container is an Instance of a Hash subclass (e.g.
                // `class MyHash is Hash {}`), convert it to a plain Hash
                // before element assignment so hash operations work correctly.
                // But if it inherits from an immutable type (Set, Bag, Mix),
                // throw X::Assignment::RO instead.
                if let Some(Value::Instance {
                    class_name,
                    attributes,
                    ..
                }) = self.env().get(&var_name)
                    && self.is_container_subclass(&class_name.resolve())
                {
                    let cn = class_name.resolve();
                    if self.class_inherits_from_immutable_setty(&cn) {
                        let display = format!("{}()", cn);
                        return Err(RuntimeError::assignment_ro_typename(&cn, &display));
                    }
                    let hash_map: HashMap<String, Value> = HashMap::clone(&attributes.as_map());
                    let hash_val = Value::hash(hash_map);
                    self.env_mut().insert(var_name.clone(), hash_val);
                }
                // Autovivify Nil-valued typed containers (MixHash, BagHash, SetHash)
                // before the main container dispatch so they are handled correctly.
                {
                    let constraint = loan_env!(self, var_type_constraint(&var_name));
                    let effective_type = declared_type.as_deref().or(constraint.as_deref());
                    if let Some(type_name) = effective_type
                        && matches!(type_name, "MixHash" | "BagHash" | "SetHash")
                        && matches!(
                            self.env().get(&var_name),
                            Some(Value::Nil) | Some(Value::Package(_)) | None
                        )
                    {
                        let new_container = match type_name {
                            "MixHash" => {
                                let mut weights = HashMap::new();
                                let weight = Self::mix_assignment_weight(&val)?;
                                if weight != 0.0 {
                                    weights.insert(key.clone(), weight);
                                }
                                Value::mix_hash(weights)
                            }
                            "BagHash" => {
                                let mut counts = HashMap::new();
                                if let Value::Int(n) = &val
                                    && *n > 0
                                {
                                    counts.insert(key.clone(), *n);
                                }
                                Value::bag_hash(counts)
                            }
                            "SetHash" => {
                                let mut items = std::collections::HashSet::new();
                                if val.truthy() {
                                    items.insert(key.clone());
                                }
                                Value::Set(Arc::new(crate::value::SetData::new(items)), true)
                            }
                            _ => unreachable!(),
                        };
                        self.env_mut().insert(var_name.clone(), new_container);
                        self.stack.push(val);
                        // Update local slot
                        if let Some(new_val) = self.env().get(&var_name).cloned() {
                            self.update_local_if_exists(code, &var_name, &new_val);
                        }
                        return Ok(());
                    }
                }
                if let Some(container) = self.env_root_descended_mut(&var_name) {
                    match *container {
                        Value::Hash(ref mut hash) => {
                            let is_self_hash_ref = matches!(
                                &val,
                                Value::Hash(source_hash) if Arc::ptr_eq(hash, source_hash)
                            );
                            // Use in-place mutation instead of Arc::make_mut when the
                            // hash is shared (strong_count > 1) AND the variable is a
                            // scalar ($) container.  This preserves Raku's object
                            // identity semantics: when a hash is stored in an array
                            // slot via `$arr[i] = $hash`, mutations through the original
                            // variable must be visible through the array.
                            // In-place mutation goes through the audited
                            // `arc_contents_mut` choke point below, guarded by
                            // `strong_count > 1` (see its safety contract).
                            // For %-sigiled hash variables (e.g. `%h is copy`),
                            // normally use COW.  For scalar ($) variables holding
                            // hashes, use in-place mutation to preserve identity.
                            // Exception: when a %-sigiled variable is bound via
                            // `:=` (marked readonly), use in-place mutation so
                            // modifications propagate to the bound source.
                            // %-sigiled vars have names like "%h", scalar vars
                            // have names without a sigil prefix (e.g. "bar").
                            let use_inplace = Arc::strong_count(hash) > 1
                                && (!var_name.starts_with('%') || is_bound_hash_var);
                            // Mutate the whole `HashData` (map + embedded
                            // object-hash original keys) so the original-key map
                            // travels with the hash through copy-on-write.
                            let hd: &mut crate::value::HashData = if use_inplace {
                                // SAFETY: aliased in-place mutation of a shared
                                // hash; see `arc_contents_mut`.
                                unsafe { crate::value::arc_contents_mut(hash) }
                            } else {
                                Arc::make_mut(hash)
                            };
                            if bind_mode && let Some((source_install, cell)) = &bind_cell {
                                // Phase 2 Stage 2: a `:=`-bound entry holds a
                                // shared `ContainerRef` cell (no more
                                // BOUND_HASH_REF_SENTINEL back-references).
                                // Reads decont at `resolve_hash_entry`; writes
                                // go through `hash_insert_through`.
                                hd.map
                                    .insert(key.clone(), Value::ContainerRef(cell.clone()));
                                if let Some(source_name) = source_install {
                                    pending_source_cell = Some((source_name.clone(), cell.clone()));
                                }
                            } else if is_self_hash_ref {
                                hd.map.insert(key.clone(), Self::self_hash_ref_marker());
                            } else if elem_is_value_share {
                                // Slice 2b: replace the `=`-shared cell rather than
                                // write through it, so the source stays unaffected.
                                hd.map.insert(key.clone(), val.clone());
                            } else {
                                Value::hash_insert_through(&mut hd.map, key.clone(), val.clone());
                            }
                            // For object hashes, store the original key object in
                            // the embedded `original_keys` map (COW-stable). Skip
                            // for a plain-keyed object hash (see `use_which`) so it
                            // is not flipped into `.WHICH` keying mid-stream.
                            if use_which {
                                hd.original_keys
                                    .get_or_insert_with(std::collections::HashMap::new)
                                    .insert(key.clone(), idx.clone());
                            }
                        }
                        Value::Array(..) => {
                            if has_declared_shape
                                || crate::runtime::utils::is_shaped_array(container)
                            {
                                if bind_mode && is_bound_index {
                                    return Err(RuntimeError::assignment_ro(None));
                                }
                                Self::assign_array_multidim(
                                    container,
                                    std::slice::from_ref(&idx),
                                    val.clone(),
                                )?;
                            } else if let Some((slice_indices, vals)) = &range_slice {
                                if let Value::Array(items, ..) = container {
                                    let arr = Arc::make_mut(items);
                                    if let Some(max_idx) = slice_indices.last().copied()
                                        && max_idx >= arr.len()
                                    {
                                        Self::autoviv_resize(
                                            arr,
                                            max_idx + 1,
                                            Value::Package(Symbol::intern("Any")),
                                        )?;
                                    }
                                }
                                for (offset, i) in slice_indices.iter().enumerate() {
                                    let key = Value::Int(*i as i64);
                                    let v = vals.get(offset).cloned().unwrap_or(Value::Nil);
                                    Self::assign_array_multidim(
                                        container,
                                        std::slice::from_ref(&key),
                                        v,
                                    )?;
                                    range_initialized_marks.push(Self::encode_bound_index(&key));
                                }
                            } else if let Some(i) = Self::index_to_usize(&idx) {
                                if let Value::Array(items, ..) = container {
                                    let is_self_array_ref = matches!(
                                        &val,
                                        Value::Array(source_items, ..) if Arc::ptr_eq(items, source_items)
                                    );
                                    // Use in-place mutation when the array is shared
                                    // (strong_count > 1) to preserve identity semantics
                                    // and support shared `ContainerRef` cell binding.
                                    let use_inplace =
                                        Arc::strong_count(items) > 1 && !var_name.starts_with('@');
                                    let arr: &mut crate::value::ArrayData = if use_inplace {
                                        // SAFETY: aliased in-place mutation of a
                                        // shared array; see `arc_contents_mut`.
                                        unsafe { crate::value::arc_contents_mut(items) }
                                    } else {
                                        Arc::make_mut(items)
                                    };
                                    Self::autoviv_resize(arr, i + 1, native_fill.clone())?;
                                    if bind_mode && let Some((source_install, cell)) = &bind_cell {
                                        // Phase 2 Stage 2: a `:=`-bound element
                                        // holds a shared `ContainerRef` cell (no
                                        // more BOUND_ARRAY_REF_SENTINEL
                                        // back-references). Reads decont at
                                        // `resolve_array_entry`; writes go
                                        // through the cell arm below.
                                        arr[i] = Value::ContainerRef(cell.clone());
                                        stored_bind_cell = true;
                                        if let Some(source_name) = source_install {
                                            pending_source_cell =
                                                Some((source_name.clone(), cell.clone()));
                                        }
                                    } else if let Some((source_name, source_index)) =
                                        Self::varref_target(&arr[i])
                                    {
                                        pending_varref_update =
                                            Some((source_name.clone(), source_index, val.clone()));
                                        arr[i] = Self::make_varref_value(
                                            source_name,
                                            val.clone(),
                                            source_index,
                                        );
                                    } else if let Value::ContainerRef(cell) = &arr[i] {
                                        if elem_is_value_share {
                                            // Slice 2b: a `=`-shared element
                                            // reassigned with a non-share value
                                            // REPLACES the slot (raku value
                                            // semantics) — drop the shared cell so
                                            // the source stays unaffected.
                                            arr[i] = native_store_val.clone();
                                        } else {
                                            // Phase 2: the element is a `:=`-bound
                                            // shared cell — write through it so the
                                            // alias observes the new value.
                                            *cell.lock().unwrap() = native_store_val.clone();
                                        }
                                    } else {
                                        arr[i] = if is_self_array_ref {
                                            Self::self_array_ref_marker()
                                        } else {
                                            // A native integer array stores the
                                            // wrapped value (e.g. -1 -> 255 in a
                                            // uint8 array), while the assignment
                                            // expression still yields the original.
                                            native_store_val.clone()
                                        };
                                    }
                                }
                            } else if let Value::Int(i) = &idx
                                && *i < 0
                            {
                                // Negative index from WhateverCode resolution
                                // (e.g. @arr[*-1] = 42 on empty array)
                                return Err(Self::make_out_of_range_error(*i));
                            } else {
                                return Err(RuntimeError::new("Index out of bounds"));
                            }
                            self.mark_initialized_index(&var_name, encoded_idx.clone());
                            // A cell-bound element does not need the bound-index
                            // side table — the `ContainerRef` cell is the alias
                            // and write-through happens via the cell.
                            if bind_mode && !stored_bind_cell {
                                self.mark_bound_index(&var_name, encoded_idx.clone());
                            }
                        }
                        Value::Set(ref mut set, is_mutable) => {
                            if !is_mutable {
                                return Err(RuntimeError::assignment_ro(Some("Set")));
                            }
                            let s = Arc::make_mut(set);
                            if val.truthy() {
                                s.insert(key.clone());
                            } else {
                                s.remove(&key);
                            }
                        }
                        Value::Bag(ref mut bag, is_mutable) => {
                            if !is_mutable {
                                return Err(RuntimeError::assignment_ro(Some("Bag")));
                            }
                            let b = Arc::make_mut(bag);
                            let count = Self::bag_assignment_count(&val)?;
                            if count == num_bigint::BigInt::from(0) {
                                b.remove(&key);
                            } else {
                                b.insert(key.clone(), count);
                            }
                        }
                        Value::Mix(ref mut mix, is_mutable) => {
                            if !is_mutable {
                                return Err(RuntimeError::assignment_ro(Some("Mix")));
                            }
                            let m = Arc::make_mut(mix);
                            let weight = Self::mix_assignment_weight(&val)?;
                            if weight == 0.0 {
                                m.remove(&key);
                            } else {
                                m.insert(key.clone(), weight);
                            }
                        }
                        // Autovivify typed containers: MixHash, BagHash, SetHash
                        Value::Package(sym)
                            if matches!(
                                sym.resolve().as_str(),
                                "MixHash" | "BagHash" | "SetHash"
                            ) =>
                        {
                            let type_name = sym.resolve();
                            match type_name.as_str() {
                                "MixHash" => {
                                    let mut weights = HashMap::new();
                                    let weight = Self::mix_assignment_weight(&val)?;
                                    if weight != 0.0 {
                                        weights.insert(key.clone(), weight);
                                    }
                                    *container = Value::mix_hash(weights);
                                }
                                "BagHash" => {
                                    let mut counts = HashMap::new();
                                    let count = Self::bag_assignment_count(&val)?;
                                    if num_traits::Signed::is_positive(&count) {
                                        counts.insert(key.clone(), count);
                                    }
                                    *container = Value::bag_hash_big(counts);
                                }
                                "SetHash" => {
                                    let mut items = std::collections::HashSet::new();
                                    if val.truthy() {
                                        items.insert(key.clone());
                                    }
                                    *container = Value::Set(
                                        Arc::new(crate::value::SetData::new(items)),
                                        true,
                                    );
                                }
                                _ => unreachable!(),
                            }
                        }
                        _ => {
                            // Autovivify Nil/uninitialized container: pick
                            // Array if subscript was positional, else Hash.
                            if (var_name.starts_with('@')
                                || (is_positional && !var_name.starts_with('%')))
                                && let Some(i) = Self::index_to_usize(&idx)
                            {
                                let mut arr = vec![Value::Package(Symbol::intern("Any")); i + 1];
                                arr[i] = val.clone();
                                *container = Value::real_array(arr);
                            } else {
                                let mut hash = std::collections::HashMap::new();
                                hash.insert(key.clone(), val.clone());
                                let mut hash_val = Value::hash(hash);
                                if use_which {
                                    let mut orig = HashMap::new();
                                    orig.insert(key.clone(), idx.clone());
                                    hash_val =
                                        runtime::utils::set_hash_original_keys(hash_val, orig);
                                }
                                *container = hash_val;
                            }
                        }
                    }
                } else {
                    // Autovivify the missing variable
                    if (var_name.starts_with('@') || (is_positional && !var_name.starts_with('%')))
                        && let Some(i) = Self::index_to_usize(&idx)
                    {
                        let mut arr = vec![Value::Package(Symbol::intern("Any")); i + 1];
                        arr[i] = val.clone();
                        self.env_mut()
                            .insert(var_name.clone(), Value::real_array(arr));
                    } else {
                        let mut hash = std::collections::HashMap::new();
                        hash.insert(key.clone(), val.clone());
                        let mut hash_val = Value::hash(hash);
                        if use_which {
                            let mut orig = HashMap::new();
                            orig.insert(key.clone(), idx.clone());
                            hash_val = runtime::utils::set_hash_original_keys(hash_val, orig);
                        }
                        self.env_mut().insert(var_name.clone(), hash_val);
                    }
                }
                if let Some((source_name, source_index, source_value)) = pending_varref_update {
                    self.assign_varref_target(&source_name, source_index, source_value)?;
                }
                if let Some((source_name, cell)) = pending_source_cell {
                    // Bind the source variable to the same cell installed at the
                    // element, so both sides alias one container.
                    let cell_val = Value::ContainerRef(cell);
                    self.set_env_with_main_alias(&source_name, cell_val.clone());
                    self.update_local_if_exists(code, &source_name, &cell_val);
                }
                for encoded in range_initialized_marks {
                    self.mark_initialized_index(&var_name, encoded);
                }
                // Slice 2b: a `=`-shared element just reassigned with a non-share
                // value has been replaced by a plain value — drop the share
                // marker so a later `:=` bind of the same element is not mistaken
                // for a value share.
                if elem_is_value_share {
                    self.clear_element_share(&var_name, &encoded_idx);
                }
                // Sync OS environment when %*ENV is modified
                #[cfg(not(target_family = "wasm"))]
                if var_name == "%*ENV" {
                    // SAFETY: std::env::set_var is unsafe because mutating the
                    // process environment races with any concurrent env access
                    // on another thread. mutsu writes %*ENV from the executing
                    // thread during normal evaluation; a spawned worker that
                    // concurrently reads env would be a latent race (tracked
                    // with the cross-thread container work, see aliased_mut.rs).
                    unsafe {
                        std::env::set_var(&key, val.to_string_value());
                    }
                }
                // Sync $*HOME when %*ENV<HOME> changes
                if var_name == "%*ENV" && key == "HOME" {
                    let home_str = val.to_string_value();
                    let home_val = self.make_io_path_instance(&home_str);
                    self.env_mut()
                        .insert("$*HOME".to_string(), home_val.clone());
                    self.env_mut().insert("*HOME".to_string(), home_val);
                }
            }
        }
        if let Some(updated) = self.get_env_with_main_alias(&var_name) {
            self.update_local_if_exists(code, &var_name, &updated);
            // Write the whole topic back to its source variable only when the
            // source is a scalar (e.g. `for $x { $_[1] = ... }`). For array/hash
            // sources (`for @a { $_[1] = ... }`), the for-loop's own per-element
            // writeback (write_back_for_topic_item) syncs the mutated element
            // back at the correct index; replacing the whole container with the
            // single topic value here would corrupt the source.
            if var_name == "_"
                && let Some(ref source_var) = self.topic_source_var
                && !source_var.starts_with('@')
                && !source_var.starts_with('%')
            {
                // For @/% sources, element writeback to a per-index slot is
                // handled by write_back_for_topic_item at the end of the loop
                // body. Overwriting the whole container with $_ here would
                // clobber the aggregate (e.g. `$_[1] = 9 for @a`).
                let source_name = source_var.clone();
                self.set_env_with_main_alias(&source_name, updated.clone());
                self.update_local_if_exists(code, &source_name, &updated);
            }
            // Re-attach the `is default(...)` element default after mutation
            // when a rebuild dropped it (embedded in ArrayData, so plain
            // Arc::make_mut mutations carry it on their own).
            if let Some(def) = self.var_default(&var_name).cloned()
                && self.container_default(&updated).is_none()
            {
                let tagged = self.tag_container_default(updated.clone(), def);
                self.set_env_with_main_alias(&var_name, tagged.clone());
                self.update_local_if_exists(code, &var_name, &tagged);
            }
        }
        // When operating through a sigilless alias (e.g., `h` → `%a`),
        // sync the modified container back to the alias variable so reads
        // of the sigilless variable see the updated value.
        if let Some(ref alias_target) = sigilless_alias_target
            && let Some(updated_container) = self.env().get(alias_target).cloned()
        {
            self.env_mut()
                .insert(original_var_name.clone(), updated_container.clone());
            self.update_local_if_exists(code, &original_var_name, &updated_container);
        }
        // After element assignment, Arc::make_mut may have created a new Arc
        // (COW). Sync HashEntryRef locals whose root `hash` Arc pointed to the
        // OLD container so they reference the new one. Only update refs that
        // pointed to the same container (identified by old_container_arc_ptr),
        // not refs to unrelated nested containers.
        if let Some(old_ptr) = old_container_arc_ptr {
            let current = self
                .get_env_with_main_alias(&var_name)
                .or_else(|| self.env().get(&original_var_name).cloned());
            if let Some(ref container) = current {
                let new_arc_ptr = match container {
                    Value::Array(arc, _) => Some(Arc::as_ptr(arc) as usize),
                    Value::Hash(arc) => Some(Arc::as_ptr(arc) as usize),
                    _ => None,
                };
                if let Some(new_ptr) = new_arc_ptr {
                    // Only sync if the Arc pointer actually changed (COW happened)
                    if new_ptr != old_ptr {
                        for local in self.locals.iter_mut() {
                            // Only update refs that pointed to the OLD container.
                            if let Value::HashEntryRef { hash, .. } = local
                                && Arc::as_ptr(hash) as usize == old_ptr
                                && let Value::Hash(new_arc) = container
                            {
                                *hash = new_arc.clone();
                            }
                        }
                    }
                }
            }
        }
        self.stack.push(val);
        Ok(())
    }
}
