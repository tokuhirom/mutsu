use super::*;
use crate::symbol::Symbol;
use std::collections::HashMap;
use std::sync::Arc;

type BindSourceCell = (Option<String>, Arc<std::sync::Mutex<Value>>);

impl Interpreter {
    pub(crate) fn exec_index_assign_expr_named_op_inner(
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
        // A subscript that names exactly ONE element assigns to a scalar slot, so
        // the assignment's rvalue is itemized just like a scalar-variable
        // assignment (`@z = (@a[0] = 1, 2)` => `@z.elems == 1`, and likewise for
        // `@a[0,]`, `%h<x>`, `%h{'x'}`). A multi-element slice (a list of indices,
        // or a Range) distributes to several elements and keeps the flat list as
        // its value. Computed from the RAW index before normalization: a range
        // (incl. the string `GenericRange` that a hash subscript enumerates into
        // several keys) is always a slice; a 1-element index list acts as a single
        // element; an itemized subscript (`@a[$(7,8,9)]`) is a single index.
        let idx_is_single_element = match &idx {
            Value::Array(items, kind) => {
                matches!(kind, crate::value::ArrayKind::ItemList) || items.len() == 1
            }
            Value::Seq(items) | Value::Slip(items) => items.len() == 1,
            Value::Scalar(inner) => {
                !inner.is_range() && !matches!(inner.as_ref(), Value::Array(..))
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => false,
            _ => true,
        };
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
            // Non-integer (e.g. string) ranges: `%h{'x'..'z'} = ...` is a hash
            // slice over the enumerated keys `x`, `y`, `z`, not a single
            // stringified `"x y z"` key. Expand via the range's own list.
            gr @ Value::GenericRange { .. } if expand_range => {
                let items = crate::runtime::utils::value_to_list(&gr);
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
                    // A 1-element slice (`@a[0,]`) names a single scalar slot, so
                    // its rvalue itemizes like a single-index assignment.
                    let result = if idx_is_single_element {
                        Self::itemize_value(val)
                    } else {
                        val
                    };
                    self.stack.push(result);
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
                                *container = Value::real_array_initialized_at(arr, i);
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
                            .insert(var_name.clone(), Value::real_array_initialized_at(arr, i));
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
        // Positional array elements only: hash single-key / hash-slice assignment
        // exits through other push sites (not reached here), and this restriction
        // keeps a string-range hash subscript from being mis-itemized.
        let result = if idx_is_single_element && !var_name.starts_with('%') {
            Self::itemize_value(val)
        } else {
            val
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_index_assign_pseudo_stash_named_op(
        &mut self,
        code: &CompiledCode,
        stash_name_idx: u32,
        key_name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let stash_name = Self::const_str(code, stash_name_idx);
        // `PROCESS::<$name> = value` sets the process-level dynamic variable, so
        // a later `$*name` (stored in the env as `*name`) resolves to it. This is
        // how `Rakudo::Internals.REGISTER-DYNAMIC` installs defaults (e.g.
        // DBIish's `$*DBI-DEFS`).
        if stash_name == "PROCESS::" {
            let raw_key = Self::const_str(code, key_name_idx).to_string();
            // Map the sigiled stash key to the env dynamic-var key:
            //   $name → *name, @name → @*name, %name → %*name, name → *name
            let env_key = match raw_key.chars().next() {
                Some('$') => format!("*{}", &raw_key[1..]),
                Some('@') => format!("@*{}", &raw_key[1..]),
                Some('%') => format!("%*{}", &raw_key[1..]),
                _ => format!("*{raw_key}"),
            };
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let val = if env_key.starts_with('@') {
                runtime::coerce_to_array(val)
            } else if env_key.starts_with('%') {
                self.coerce_hash_var_value(&env_key, val)?
            } else {
                Self::normalize_scalar_assignment_value(val)
            };
            self.env_mut().insert(env_key, val.clone());
            // An assignment is an expression: leave the assigned value on the
            // stack so `Rakudo::Internals.REGISTER-DYNAMIC`'s block (whose body is
            // `PROCESS::<$x> = ...`) returns it.
            self.stack.push(val);
            return Ok(());
        }
        if stash_name != "MY::" {
            return Err(RuntimeError::new(format!(
                "Unsupported pseudo-stash assignment target {stash_name}"
            )));
        }

        let raw_key = Self::const_str(code, key_name_idx);
        let resolved_name = if let Some(name) = raw_key.strip_prefix('$') {
            name.to_string()
        } else {
            raw_key.to_string()
        };

        let val = self.stack.pop().unwrap_or(Value::Nil);
        if let Some(slot) = self.find_local_slot(code, &resolved_name) {
            self.stack.push(val);
            self.exec_assign_expr_local_op(code, slot as u32)
        } else {
            let mut val = if resolved_name.starts_with('@') {
                runtime::coerce_to_array(val)
            } else if resolved_name.starts_with('%') {
                self.coerce_hash_var_value(&resolved_name, val)?
            } else {
                Self::normalize_scalar_assignment_value(val)
            };

            self.check_readonly_for_modify(&resolved_name)?;
            if let Some(default) = self.var_default(&resolved_name)
                && matches!(val, Value::Nil)
            {
                val = default.clone();
            }
            if resolved_name.starts_with('@') || resolved_name.starts_with('%') {
                val = self.coerce_typed_container_assignment(&resolved_name, val, false)?;
            }
            if let Some(constraint) = loan_env!(self, var_type_constraint(&resolved_name))
                && !resolved_name.starts_with('@')
                && !resolved_name.starts_with('%')
            {
                if matches!(val, Value::Nil) {
                    if constraint != "Mu" {
                        let nominal =
                            loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                        val = Value::Package(Symbol::intern(&nominal));
                    }
                } else if !self.type_matches_value(&constraint, &val) {
                    return Err(runtime::utils::type_check_assignment_typed_error(
                        &resolved_name,
                        &constraint,
                        &val,
                    ));
                }
                if !matches!(val, Value::Nil | Value::Package(_)) {
                    val = loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?;
                }
            }

            self.set_env_with_main_alias(&resolved_name, val.clone());
            self.stack.push(val);
            Ok(())
        }
    }

    pub(super) fn exec_index_assign_expr_nested_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        outer_positional: bool,
        inner_positional: bool,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let native_fill = {
            let tc = loan_env!(self, var_type_constraint(&var_name));
            Self::native_fill_for_constraint(tc.as_deref())
        };
        let inner_idx = self.stack.pop().unwrap_or(Value::Nil);
        let outer_idx = self.stack.pop().unwrap_or(Value::Nil);
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        // Detect bind marker (__mutsu_bind_index_value) and extract the actual value
        let val = match raw_val {
            Value::Pair(ref name, ref payload) if name == "__mutsu_bind_index_value" => {
                match payload.as_ref() {
                    Value::Array(items, ..) if !items.is_empty() => {
                        items.first().cloned().unwrap_or(Value::Nil)
                    }
                    other => other.clone(),
                }
            }
            other => other,
        };

        // Junction / slice outer subscript (`%h<x>{any('p','q')} = v`,
        // `%h<x>{@k} = (...)`): autothread per outer key. The rest of this op
        // handles a single scalar outer key, so re-dispatch for each expanded
        // key (previously the junction/array was stringified into one garbage
        // entry, so a later read of any real key returned Any).
        if matches!(outer_idx, Value::Junction { .. } | Value::Array(..)) {
            let outer_keys: Vec<Value> = match &outer_idx {
                Value::Junction { values, .. } => values.as_ref().clone(),
                Value::Array(a, ..) => a.items.to_vec(),
                _ => unreachable!(),
            };
            // A scalar RHS goes to every junction key; a slice/junction RHS
            // pairs element-wise.
            let vals: Vec<Value> = match (&outer_idx, &val) {
                (Value::Junction { .. }, Value::Junction { values: jv, .. }) => jv.as_ref().clone(),
                (Value::Junction { .. }, _) => vec![val.clone(); outer_keys.len()],
                _ => self.assignment_rhs_values(&val)?,
            };
            for (i, ok) in outer_keys.into_iter().enumerate() {
                let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                // Re-dispatch with a single scalar outer key. Stack order
                // (bottom→top): value, outer_idx, inner_idx.
                self.stack.push(v);
                self.stack.push(ok);
                self.stack.push(inner_idx.clone());
                self.exec_index_assign_expr_nested_op(
                    code,
                    name_idx,
                    outer_positional,
                    inner_positional,
                )?;
                self.stack.pop();
            }
            self.stack.push(val);
            return Ok(());
        }

        let inner_key = inner_idx.to_string_value();
        let outer_key = outer_idx.to_string_value();

        // If the outer hash has a value type constraint, nested autovivification
        // would need to create a Hash value which won't match the constraint
        // (e.g. `my Int %h; %h<z><t> = 3` should die because %h<z> can't be a Hash).
        if var_name.starts_with('%')
            && let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
        {
            let inner_hash = Value::hash(std::collections::HashMap::new());
            if !loan_env!(self, type_matches_value(&constraint, &inner_hash)) {
                return Err(RuntimeError::new(format!(
                    "Type check failed in assignment to {var_name}; expected {constraint} but got Hash (autovivification)"
                )));
            }
        }

        if !self.env().contains_key(&var_name) {
            // Autovivify the variable as Array if the inner subscript was
            // positional (`[...]`), otherwise as Hash. For sigiled vars
            // (`@x`, `%h`) the sigil already constrains the kind, so this
            // mainly matters for scalar `$x` autoviv.
            let init = if var_name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if var_name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else if inner_positional {
                Value::real_array(Vec::new())
            } else {
                Value::hash(std::collections::HashMap::new())
            };
            self.env_mut().insert(var_name.clone(), init);
        }

        // Handle Array-as-outer-container (e.g. `@array[42][23] = 17`,
        // `@array[42]<key> = 17`). `inner_key` is the first/closer subscript
        // value (here "42"). `outer_key` is the outermost subscript value
        // (here "23" or "key"). We autovivify the missing entry at
        // `inner_i` based on `outer_positional`.
        // Drop the locals copy first so the Arc refcount is 1 (avoids
        // unnecessary cloning in Arc::make_mut).
        if let Some(slot) = self.find_local_slot(code, &var_name)
            && matches!(self.locals[slot], Value::Array(..))
        {
            self.locals[slot] = Value::Nil;
        }
        if let Some(Value::Array(outer_arr, _kind)) = self.env_root_descended_mut(&var_name)
            && let Ok(inner_i) = inner_key.parse::<usize>()
        {
            let arr = Arc::make_mut(outer_arr);
            Self::autoviv_resize(arr, inner_i + 1, native_fill.clone())?;
            // Autovivify the slot if it's not already a container. A
            // `:=`-bound element is a shared `ContainerRef` cell holding a
            // container — descend through it (below) instead of clobbering it.
            let needs_viv = !matches!(
                &arr[inner_i],
                Value::Array(..) | Value::Hash(..) | Value::ContainerRef(_)
            );
            if needs_viv {
                arr[inner_i] = if outer_positional {
                    Value::real_array(Vec::new())
                } else {
                    Value::hash(std::collections::HashMap::new())
                };
            }
            match &mut arr[inner_i] {
                // A bound element holds a shared cell: write through it so the
                // mutation reaches the aliased container (`@h[i] := @inner;
                // @h[i][j] = v` updates `@inner[j]`).
                Value::ContainerRef(_) => {
                    Self::assign_into_nested_container(&mut arr[inner_i], &outer_key, val.clone())?;
                }
                Value::Array(inner_arr, _) => {
                    if let Ok(j) = outer_key.parse::<usize>() {
                        // Use interior mutation when the inner array is shared
                        // (e.g., by a `ContainerRef` cell from := binding).
                        if Arc::strong_count(inner_arr) > 1 {
                            // SAFETY: aliased in-place mutation of a shared array
                            // (strong_count > 1); see `arc_contents_mut`.
                            let v = &mut unsafe { crate::value::arc_contents_mut(inner_arr) }.items;
                            Self::autoviv_resize(v, j + 1, Value::Nil)?;
                            Value::assign_element_slot(&mut v[j], val.clone());
                        } else {
                            let inner = Arc::make_mut(inner_arr);
                            Self::autoviv_resize(inner, j + 1, native_fill.clone())?;
                            Value::assign_element_slot(&mut inner[j], val.clone());
                        }
                    }
                }
                Value::Hash(inner_hash) => {
                    if Arc::strong_count(inner_hash) > 1 {
                        // SAFETY: aliased in-place mutation of a shared hash
                        // (strong_count > 1); see `arc_contents_mut`.
                        let hd = unsafe { crate::value::arc_contents_mut(inner_hash) };
                        Value::hash_insert_through(&mut hd.map, outer_key.clone(), val.clone());
                    } else {
                        let h = Arc::make_mut(inner_hash);
                        Value::hash_insert_through(h, outer_key.clone(), val.clone());
                    }
                }
                _ => {}
            }
            if let Some(updated) = self.get_env_with_main_alias(&var_name) {
                self.update_local_if_exists(code, &var_name, &updated);
            }
            self.stack.push(val);
            return Ok(());
        }

        // Hash-based nested assignment (Hash-in-Hash or Hash-containing-Array)
        // Drop the locals copy first so the Arc refcount is 1.
        // This avoids unnecessary cloning in Arc::make_mut which would
        // change the pointer and break .WHICH identity stability.
        if let Some(slot) = self.find_local_slot(code, &var_name) {
            self.locals[slot] = Value::Nil;
        }
        if let Some(Value::Hash(outer_hash)) = self.env_root_descended_mut(&var_name) {
            // At refcount 1, write in place via the raw pointer (same pattern
            // as the bound-cell Array arm below): `Arc::make_mut` relocates a
            // Weak-guarded (metadata-bearing, e.g. object-hash / `is default`)
            // hash even when it is the only strong holder, which would break
            // `.WHICH` pointer identity. When shared, keep the make_mut COW
            // detach. The cast MUST target `HashData` (not its inner map) —
            // see docs/hashdata-migration-plan.md "Latent UB found & fixed".
            let oh: &mut crate::value::HashData = if Arc::strong_count(outer_hash) == 1 {
                // SAFETY: single strong holder; in-place avoids the make_mut
                // relocation that would break `.WHICH` identity. See
                // `arc_contents_mut`.
                unsafe { crate::value::arc_contents_mut(outer_hash) }
            } else {
                Arc::make_mut(outer_hash)
            };
            // Vivify the missing entry as Array if the OUTER (second) subscript
            // is positional (e.g. `%h<key>[42] = ...`), otherwise as Hash.
            let inner_val = oh.entry(inner_key).or_insert_with(|| {
                if outer_positional {
                    Value::real_array(Vec::new())
                } else {
                    Value::hash(std::collections::HashMap::new())
                }
            });
            Self::assign_into_nested_container(inner_val, &outer_key, val.clone())?;
        }
        if let Some(updated) = self.get_env_with_main_alias(&var_name) {
            self.update_local_if_exists(code, &var_name, &updated);
        }
        self.stack.push(val);
        Ok(())
    }

    /// Assign `val` into `target[outer_key]`, descending through any chain of
    /// `:=`-bound container cells (`ContainerRef`). Used by the 2-level nested
    /// assign so that a write to a container-valued bound element
    /// (`%h<key><inner> = ...` where `%h<key>` is a cell) mutates the shared,
    /// held container in place instead of being silently dropped.
    pub(super) fn assign_into_nested_container(
        target: &mut Value,
        outer_key: &str,
        val: Value,
    ) -> Result<(), RuntimeError> {
        match target {
            Value::ContainerRef(cell) => {
                let mut guard = cell.lock().unwrap();
                Self::assign_into_nested_container(&mut guard, outer_key, val)?;
            }
            Value::Array(arr, _) => {
                if let Ok(i) = outer_key.parse::<usize>() {
                    if Arc::strong_count(arr) > 1 {
                        // SAFETY: aliased in-place mutation of a shared array
                        // (strong_count > 1); see `arc_contents_mut`.
                        let v = &mut unsafe { crate::value::arc_contents_mut(arr) }.items;
                        Self::autoviv_resize(v, i + 1, Value::Nil)?;
                        Value::assign_element_slot(&mut v[i], val);
                    } else {
                        let a = Arc::make_mut(arr);
                        Self::autoviv_resize(a, i + 1, Value::Nil)?;
                        Value::assign_element_slot(&mut a[i], val);
                    }
                }
            }
            Value::Hash(h) => {
                Value::hash_insert_through(&mut Arc::make_mut(h).map, outer_key.to_string(), val);
            }
            _ => {}
        }
        Ok(())
    }

    /// Follow `current` through any chain of `:=`-bound container cells
    /// (`ContainerRef`), returning a raw pointer to the innermost held value.
    ///
    /// SAFETY: the pointer derived from the cell's mutex data stays valid after
    /// the transient guard drops because the held `Value` is owned by the `Arc`,
    /// which the caller keeps alive; no aliasing borrow into that `Value` may be
    /// live while the returned pointer is used. (Cross-thread sharing of the same
    /// cell is the tracked, pre-existing gap documented in `aliased_mut.rs`.)
    ///
    /// Cycle-safety (Phase 4): a self-referential bind can make a cell
    /// transitively hold a `ContainerRef` back to itself. A normal cell holds a
    /// Hash/Array (the loop stops immediately); only a cell-holding-cell chain
    /// can loop. Bound the descent depth and stop on overflow so a cyclic bind
    /// terminates instead of hanging.
    pub(crate) unsafe fn descend_container_ref(mut current: *mut Value) -> *mut Value {
        const MAX_DESCENT: usize = 256;
        for _ in 0..MAX_DESCENT {
            let cell = match unsafe { &mut *current } {
                Value::ContainerRef(cell) => cell.clone(),
                _ => return current,
            };
            let mut guard = cell.lock().unwrap();
            current = &mut *guard as *mut Value;
        }
        current
    }

    /// Read a root variable for index-assignment, descending through any
    /// `:=`-bound container cell so a `ContainerRef` root resolves to its held
    /// Hash/Array. Without this, a write to `$x[i]`/`$x<k>` where `$x` is itself
    /// a bound cell would fall through every handler's `Value::Hash`/`Value::Array`
    /// match and be silently dropped (Phase 3).
    ///
    /// SAFETY: the returned reference points into the cell's mutex data, kept
    /// alive by the `Arc` stored in env, and no other borrow into that data may
    /// be live while it is held (see `descend_container_ref`).
    pub(crate) fn env_root_descended_mut(&mut self, var_name: &str) -> Option<&mut Value> {
        let root = self.env_mut().get_mut(var_name)? as *mut Value;
        let descended = unsafe { Self::descend_container_ref(root) };
        Some(unsafe { &mut *descended })
    }

    /// Deep nested index assignment (3+ levels): @a[i][j][k]... = val
    /// Stack order: [value, idx_outermost, ..., idx_innermost] (innermost on top).
    /// positional_flags_idx is a constant index holding an array of booleans
    /// (innermost to outermost) indicating whether each subscript is positional.
    pub(super) fn exec_index_assign_deep_nested_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        depth: u32,
        positional_flags_idx: u32,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let native_fill = {
            let tc = loan_env!(self, var_type_constraint(&var_name));
            Self::native_fill_for_constraint(tc.as_deref())
        };
        let depth = depth as usize;

        // Pop indices from stack: innermost first (top of stack)
        let mut indices_val: Vec<Value> = Vec::with_capacity(depth);
        for _ in 0..depth {
            indices_val.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        // indices_val[0] = innermost, indices_val[depth-1] = outermost

        let raw_val_for_junction = self.stack.pop().unwrap_or(Value::Nil);

        // Junction / slice OUTERMOST subscript (`%h<a><b>{any('p','q')} = v`):
        // autothread per outer key. The rest of this op handles a single scalar
        // outermost key, so re-dispatch for each expanded key (previously the
        // junction/array was stringified into one garbage entry).
        let outermost = &indices_val[depth - 1];
        if !matches!(&raw_val_for_junction, Value::Pair(n, _) if n == "__mutsu_bind_index_value")
            && matches!(outermost, Value::Junction { .. } | Value::Array(..))
        {
            let outer_keys: Vec<Value> = match outermost {
                Value::Junction { values, .. } => values.as_ref().clone(),
                Value::Array(a, ..) => a.items.to_vec(),
                _ => unreachable!(),
            };
            let vals: Vec<Value> = match (outermost, &raw_val_for_junction) {
                (Value::Junction { .. }, Value::Junction { values: jv, .. }) => jv.as_ref().clone(),
                (Value::Junction { .. }, _) => {
                    vec![raw_val_for_junction.clone(); outer_keys.len()]
                }
                _ => self.assignment_rhs_values(&raw_val_for_junction)?,
            };
            for (i, ok) in outer_keys.into_iter().enumerate() {
                let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                // Re-push for recursion. Stack (bottom→top): value, outermost,
                // ..., innermost. Replace the outermost with the scalar key.
                self.stack.push(v);
                self.stack.push(ok);
                for j in (0..depth - 1).rev() {
                    self.stack.push(indices_val[j].clone());
                }
                self.exec_index_assign_deep_nested_op(
                    code,
                    name_idx,
                    depth as u32,
                    positional_flags_idx,
                )?;
                self.stack.pop();
            }
            self.stack.push(raw_val_for_junction);
            return Ok(());
        }

        let indices: Vec<String> = indices_val.iter().map(|v| v.to_string_value()).collect();
        let val = raw_val_for_junction;

        // LHS binding (`$struct[..]<..>[..] := $scalar`): unwrap the bind
        // payload and promote the deep leaf element to a first-class
        // `ContainerRef` cell shared with the source variable (Phase 2 Stage 2,
        // the symmetric form of RHS element binding). Only a plain scalar source
        // is handled here; an indexed element source (`:= $b[j]`, encoded with
        // `\0idx\0`) is a separate slice. A cell survives COW of any enclosing
        // container, so a later write to either side reaches the other — which
        // the old `BOUND_ARRAY_REF_SENTINEL` by-name back-reference lost at depth.
        let (val, bind_source) = Self::unwrap_bind_index_value(val);
        let bind_source = bind_source.filter(|s| !s.contains("\x00idx\x00"));
        let bind_cell: Option<Arc<std::sync::Mutex<Value>>> = bind_source
            .as_ref()
            .map(|_| Arc::new(std::sync::Mutex::new(val.clone())));

        // Extract positional flags from constant
        let flags_val = code.constants[positional_flags_idx as usize].clone();
        let positional_flags: Vec<bool> = if let Value::Array(arr, _) = &flags_val {
            arr.iter().map(|v| matches!(v, Value::Bool(true))).collect()
        } else {
            vec![true; depth]
        };

        // Invalidate local cache for the variable
        if let Some(slot) = self.find_local_slot(code, &var_name)
            && matches!(self.locals[slot], Value::Array(..) | Value::Hash(..))
        {
            self.locals[slot] = Value::Nil;
        }

        // Ensure the root variable exists
        if !self.env().contains_key(&var_name) {
            let init = if var_name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if var_name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else if positional_flags[0] {
                Value::real_array(Vec::new())
            } else {
                Value::hash(std::collections::HashMap::new())
            };
            self.env_mut().insert(var_name.clone(), init);
        }

        // Walk down the chain of indices, autovivifying containers as needed.
        // We need a mutable reference to the current container at each level.
        // Use raw pointer traversal to avoid borrow checker issues with nested mutation.
        let root: *mut Value = self.env_mut().get_mut(&var_name).unwrap() as *mut Value;

        let mut current: *mut Value = root;

        // Walk through indices[0..depth-1], autovivifying intermediate containers
        for level in 0..depth {
            let key = &indices[level];
            let is_positional = positional_flags[level];

            // Descend through any `:=`-bound container cell at this level so the
            // traversal/assignment reaches the shared, held container.
            current = unsafe { Self::descend_container_ref(current) };

            if level < depth - 1 {
                // Intermediate level: autovivify and descend
                let next_positional = positional_flags[level + 1];
                unsafe {
                    match &mut *current {
                        Value::Array(arr_arc, _) => {
                            if let Ok(i) = key.parse::<usize>() {
                                let arr = Arc::make_mut(arr_arc);
                                Self::autoviv_resize(arr, i + 1, native_fill.clone())?;
                                // Autovivify if needed. A `ContainerRef` is a
                                // `:=`-bound cell that holds (and is descended to)
                                // a container on the next iteration; treating it
                                // as "needs vivify" would clobber the binding.
                                let needs_viv = !matches!(
                                    &arr[i],
                                    Value::Array(..) | Value::Hash(..) | Value::ContainerRef(..)
                                );
                                if needs_viv {
                                    arr[i] = if next_positional {
                                        Value::real_array(Vec::new())
                                    } else {
                                        Value::hash(std::collections::HashMap::new())
                                    };
                                }
                                current = &mut arr[i] as *mut Value;
                            }
                        }
                        Value::Hash(hash_arc) => {
                            let hash = Arc::make_mut(hash_arc);
                            if !hash.contains_key(key.as_str()) {
                                let new_val = if next_positional {
                                    Value::real_array(Vec::new())
                                } else {
                                    Value::hash(std::collections::HashMap::new())
                                };
                                hash.insert(key.clone(), new_val);
                            }
                            current = hash.get_mut(key.as_str()).unwrap() as *mut Value;
                        }
                        _ => {
                            // Autovivify the root itself if needed
                            if is_positional {
                                *current = Value::real_array(Vec::new());
                            } else {
                                *current = Value::hash(std::collections::HashMap::new());
                            }
                            // Retry this level
                            match &mut *current {
                                Value::Array(arr_arc, _) => {
                                    if let Ok(i) = key.parse::<usize>() {
                                        let arr = Arc::make_mut(arr_arc);
                                        Self::autoviv_resize(
                                            arr,
                                            i + 1,
                                            Value::Package(Symbol::intern("Any")),
                                        )?;
                                        arr[i] = if next_positional {
                                            Value::real_array(Vec::new())
                                        } else {
                                            Value::hash(std::collections::HashMap::new())
                                        };
                                        current = &mut arr[i] as *mut Value;
                                    }
                                }
                                Value::Hash(hash_arc) => {
                                    let hash = Arc::make_mut(hash_arc);
                                    let new_val = if next_positional {
                                        Value::real_array(Vec::new())
                                    } else {
                                        Value::hash(std::collections::HashMap::new())
                                    };
                                    hash.insert(key.clone(), new_val);
                                    current = hash.get_mut(key.as_str()).unwrap() as *mut Value;
                                }
                                _ => {}
                            }
                        }
                    }
                }
            } else {
                // Final level: assign the value. When binding, install the
                // shared cell *directly* (replacing any prior leaf), rather than
                // writing through it — a fresh `:=` rebinds the element to the
                // source's cell.
                let leaf_val = bind_cell
                    .as_ref()
                    .map(|c| Value::ContainerRef(c.clone()))
                    .unwrap_or_else(|| val.clone());
                unsafe {
                    match &mut *current {
                        Value::Array(arr_arc, _) => {
                            if let Ok(i) = key.parse::<usize>() {
                                let arr = Arc::make_mut(arr_arc);
                                Self::autoviv_resize(arr, i + 1, native_fill.clone())?;
                                if bind_cell.is_some() {
                                    arr[i] = leaf_val;
                                } else {
                                    Value::assign_element_slot(&mut arr[i], leaf_val);
                                }
                            }
                        }
                        Value::Hash(hash_arc) => {
                            let hash = Arc::make_mut(hash_arc);
                            if bind_cell.is_some() {
                                hash.insert(key.clone(), leaf_val);
                            } else {
                                Value::hash_insert_through(hash, key.clone(), leaf_val);
                            }
                        }
                        _ => {
                            // Autovivify at final level
                            if is_positional {
                                let mut arr = Vec::new();
                                if let Ok(i) = key.parse::<usize>() {
                                    Self::autoviv_resize(&mut arr, i + 1, native_fill.clone())?;
                                    arr[i] = leaf_val;
                                }
                                *current = Value::real_array(arr);
                            } else {
                                let mut h = std::collections::HashMap::new();
                                h.insert(key.clone(), leaf_val);
                                *current = Value::hash(h);
                            }
                        }
                    }
                }
            }
        }

        // Write the shared cell back to the source variable so both sides alias
        // the same container (the symmetric counterpart of the leaf store above).
        if let (Some(src), Some(cell)) = (bind_source, bind_cell) {
            let cell_val = Value::ContainerRef(cell);
            self.set_env_with_main_alias(&src, cell_val.clone());
            self.update_local_if_exists(code, &src, &cell_val);
        }

        // Write the updated root container back into the local slot. The descent
        // above mutated `env`'s root in place (possibly COW-detaching its outer
        // `Arc` via `make_mut`), then the slot was invalidated to `Nil` (above)
        // on the assumption reverse-sync would re-pull it. Without reverse-sync a
        // later read through the locals store would see that stale `Nil` (or an
        // outer `Arc` that no longer reaches the freshly bound cell). Refreshing
        // the local from `env` here makes both stores share the same outer `Arc`
        // spine, so deep `:=` binds and writes stay coherent without a pull.
        if let Some(slot) = self.find_local_slot(code, &var_name)
            && let Some(updated) = self.env().get(&var_name).cloned()
        {
            self.locals[slot] = updated;
        }

        self.stack.push(val);
        Ok(())
    }

    /// Generic index assignment on a stack-computed target.
    /// Stack order: target (bottom), index, value (top).
    /// If the target hash has `__callframe_depth`, routes through set_caller_var.
    pub(super) fn exec_index_assign_generic_op(
        &mut self,
        code: &CompiledCode,
    ) -> Result<(), RuntimeError> {
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let target = self.stack.pop().unwrap_or(Value::Nil);
        let key = idx.to_string_value();
        // A single scalar index names one element, so the assignment's rvalue is
        // itemized (like a scalar-variable / named single-index assignment);
        // `@z = (foo()[$b] = l, l)` => `@z.elems == 1`. A multi-element slice
        // (list of indices / Range) keeps the flat list.
        let idx_is_single_element = match &idx {
            Value::Array(items, kind) => {
                matches!(kind, crate::value::ArrayKind::ItemList) || items.len() == 1
            }
            Value::Seq(items) | Value::Slip(items) => items.len() == 1,
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => false,
            _ => true,
        };

        // Detect bind marker (__mutsu_bind_index_value) and extract the actual value
        let (val, bind_source) = match raw_val {
            Value::Pair(ref name, ref payload) if name == "__mutsu_bind_index_value" => {
                match payload.as_ref() {
                    Value::Array(items, ..) if items.len() >= 2 => {
                        let value = items.first().cloned().unwrap_or(Value::Nil);
                        let source = match items.get(1) {
                            Some(Value::Array(srcs, ..)) => srcs.first().and_then(|s| match s {
                                Value::Str(name) if !name.is_empty() => Some((**name).clone()),
                                _ => None,
                            }),
                            _ => None,
                        };
                        (value, source)
                    }
                    other => (other.clone(), None),
                }
            }
            other => (other, None),
        };

        // Phase 2 Stage 2: a `:=` bind to a stack-computed element target
        // (`f()<k> := $s`, `($ref)[i] := $s`) stores a shared `ContainerRef`
        // cell at the element and writes the same cell back to the source
        // variable, so a later write to either side reaches the other. This is
        // the computed-target analogue of the named-handler cell bind (slice
        // 3/4); the old `HashEntryRef`/array element back-reference by-reference into the env
        // was stale (the alias never propagated). Pre-read before any container
        // borrow. An element source (`:= @b[j]`, encoded with `\0`) is left to a
        // later slice unless it already arrived promoted to a cell.
        let bind_cell: Option<BindSourceCell> = if let Value::ContainerRef(cell) = &val {
            Some((None, cell.clone()))
        } else if let Some(source_name) = &bind_source
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
        };

        // Junction / slice key on a stack-computed target
        // (`%h<x>{any('p','q')} = v`, `%h<x>{@k} = (...)`). The named-handler op
        // autothreads these; the generic op (computed target) previously
        // stringified the multi-key index and wrote a single garbage entry, so a
        // later read of any real key returned Any. Only plain assignment is
        // handled here (a multi-key `:=` bind is a separate, exotic path).
        if bind_cell.is_none() && matches!(idx, Value::Junction { .. } | Value::Array(..)) {
            let resolved = match &target {
                Value::HashEntryRef { .. } => target.hash_entry_read(),
                Value::Scalar(inner) => inner.as_ref().clone(),
                other => other.clone(),
            };
            if matches!(resolved, Value::Hash(_) | Value::Array(..)) {
                let (keys, vals): (Vec<Value>, Vec<Value>) = match &idx {
                    Value::Junction { values, .. } => {
                        // A scalar RHS is assigned to every junction key; a
                        // junction RHS pairs element-wise.
                        let vals = match &val {
                            Value::Junction { values: jv, .. } => jv.as_ref().clone(),
                            _ => vec![val.clone(); values.len()],
                        };
                        (values.as_ref().clone(), vals)
                    }
                    Value::Array(keys, ..) => {
                        (keys.items.to_vec(), self.assignment_rhs_values(&val)?)
                    }
                    _ => unreachable!(),
                };
                for (i, k) in keys.iter().enumerate() {
                    let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                    self.assign_into_computed_target(&resolved, k, v)?;
                }
                self.stack.push(val);
                return Ok(());
            }
        }

        match &target {
            Value::Hash(arc) => {
                // Check for callframe .my hash with depth marker
                if let Some(Value::Int(depth)) = arc.get("__callframe_depth") {
                    let depth = *depth as usize;
                    // Strip sigil from key to get bare variable name
                    let bare_name =
                        if key.starts_with('$') || key.starts_with('@') || key.starts_with('%') {
                            &key[1..]
                        } else {
                            &key
                        };
                    loan_env!(self, set_caller_var(bare_name, depth, val.clone()))?;
                    self.stack.push(val);
                    return Ok(());
                }
                // Interior mutation: write into the hash via raw pointer
                // so the change is visible to all holders of the same Arc.
                // In bind mode, store the shared cell at the key; otherwise the
                // plain value.
                let stored = match &bind_cell {
                    Some((_, cell)) => Value::ContainerRef(cell.clone()),
                    None => val.clone(),
                };
                // SAFETY: aliased in-place mutation of a shared hash so the change
                // is visible to all holders of the same Arc; see `arc_contents_mut`.
                let hd = unsafe { crate::value::arc_contents_mut(arc) };
                Value::hash_insert_through(&mut hd.map, key.clone(), stored);
                // For a fresh-cell bind, write the cell back to the source var
                // so both sides alias the same container.
                if let Some((Some(src), cell)) = &bind_cell {
                    let cell_val = Value::ContainerRef(cell.clone());
                    self.set_env_with_main_alias(src, cell_val.clone());
                    self.update_local_if_exists(code, src, &cell_val);
                }
                self.stack.push(val);
            }
            Value::Array(arc, _kind) => {
                // Interior mutation: write into the array via raw pointer
                // so the change is visible to all holders of the same Arc.
                if let Ok(i) = key.parse::<usize>() {
                    // SAFETY: aliased in-place mutation of a shared array so the
                    // change is visible to all holders of the same Arc; see
                    // `arc_contents_mut`.
                    let v = &mut unsafe { crate::value::arc_contents_mut(arc) }.items;
                    Self::autoviv_resize(v, i + 1, Value::Nil)?;
                    match &bind_cell {
                        // Bind mode installs the shared cell at the element;
                        // the same cell is written back to the source var
                        // below so both sides alias.
                        Some((_, cell)) => v[i] = Value::ContainerRef(cell.clone()),
                        // An element source (`:= @b[j]`) not promoted to a
                        // cell installs the payload directly (rare; left to a
                        // later slice).
                        None if bind_source.is_some() => v[i] = val.clone(),
                        None => {
                            // Write THROUGH an existing `:=`-bound cell so an
                            // assignment reached via a stack target (e.g.
                            // `get()<subkey>[1] = …`) updates the shared cell
                            // instead of clobbering it (nested.t 11-12).
                            Value::assign_element_slot(&mut v[i], val.clone());
                        }
                    }
                    // For a fresh-cell bind, write the cell back to the source var.
                    if let Some((Some(src), cell)) = &bind_cell {
                        let cell_val = Value::ContainerRef(cell.clone());
                        self.set_env_with_main_alias(src, cell_val.clone());
                        self.update_local_if_exists(code, src, &cell_val);
                    }
                }
                let result = if idx_is_single_element {
                    Self::itemize_value(val)
                } else {
                    val
                };
                self.stack.push(result);
            }
            Value::HashEntryRef { .. } => {
                // Resolve the HashEntryRef and assign into the resolved container.
                let resolved = target.hash_entry_read();
                self.stack.push(resolved);
                self.stack.push(idx);
                self.stack.push(val);
                return self.exec_index_assign_generic_op(code);
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Stash" => {
                let package = attributes
                    .as_map()
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                if !package.is_empty() {
                    let key_name = if key.starts_with('$')
                        || key.starts_with('@')
                        || key.starts_with('%')
                        || key.starts_with('&')
                    {
                        key
                    } else {
                        format!("${key}")
                    };
                    let pkg = package.trim_end_matches("::");
                    let fq = if pkg.is_empty() || pkg == "GLOBAL" {
                        key_name
                    } else {
                        format!("{pkg}::{key_name}")
                    };
                    self.env_mut().insert(fq, val.clone());
                }
                self.stack.push(val);
            }
            _ => {
                self.stack.push(val);
            }
        }
        Ok(())
    }
}
