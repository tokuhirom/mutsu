use super::*;
use crate::vm::vm_comparison_ops::expand_range_to_list;

impl Interpreter {
    /// Multi-dimensional indexing: @a[$x;$y;$z]
    /// Stack: [target, dim0, dim1, ..., dimN-1] → [result]
    pub(super) fn exec_multi_dim_index_op(&mut self, ndims: u32) -> Result<(), RuntimeError> {
        let ndims = ndims as usize;
        let mut dims = Vec::with_capacity(ndims);
        for _ in 0..ndims {
            dims.push(self.stack.pop().unwrap_or(Value::NIL));
        }
        dims.reverse();
        let dims = Self::expand_pipe_multidim_dims(dims);
        let target = self.stack.pop().unwrap_or(Value::NIL);

        // For shaped arrays, check bounds before reading
        let is_shaped = crate::runtime::utils::is_shaped_array(&target);
        if is_shaped {
            self.check_shaped_array_bounds(&target, &dims, 0)?;
        }

        let result = self.multi_dim_index_read(&target, &dims)?;
        self.stack.push(result);
        Ok(())
    }

    /// Multi-dimensional index as an lvalue (`:=` bind RHS / raw `\target` /
    /// `is rw` argument). Stack: [target, dim0, ..., dimN-1] → [ref|value].
    /// Produces a shared `ContainerRef` cell for the leaf when every dimension
    /// is a single scalar index; otherwise (slice dimensions) it falls back to
    /// the plain read value, which does not alias.
    pub(super) fn exec_multi_dim_index_bind_ref_op(
        &mut self,
        ndims: u32,
    ) -> Result<(), RuntimeError> {
        let ndims = ndims as usize;
        let mut dims = Vec::with_capacity(ndims);
        for _ in 0..ndims {
            dims.push(self.stack.pop().unwrap_or(Value::NIL));
        }
        dims.reverse();
        let dims = Self::expand_pipe_multidim_dims(dims);
        let target = self.stack.pop().unwrap_or(Value::NIL);

        if let Some(slot) = self.multi_dim_slot_ref(&target, &dims)? {
            self.stack.push(slot);
            return Ok(());
        }
        // A subscript containing a slice dimension (`*` or an index list) over
        // ALREADY-EXISTING leaves selects several leaves that cannot collapse to
        // one cell. Promote each selected leaf to a shared `ContainerRef` cell
        // and hand back a plain list of those cells — the array analogue of the
        // `@slice := @array[1,2]` bound-slice (see `array_slot_ref` /
        // `slice_bind_indices`). A `\raw` / `is rw` parameter bound to this list
        // then distributes a `target = values` assignment element-wise through
        // the cells (see the sigilless bound-slice write-through in the assign
        // ops), while a read decontainerizes each cell transparently. Missing
        // leaves and hash roots fall back to the plain (non-aliasing) read.
        let is_slice = dims.iter().any(|d| {
            matches!(
                Self::normalize_multidim_dim(d).view(),
                ValueView::Whatever | ValueView::Array(..)
            )
        });
        if is_slice {
            let deref_target = match target.view() {
                ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
                ValueView::Scalar(inner) => inner.clone(),
                _ => target.clone(),
            };
            let mut cells = Vec::new();
            if self
                .collect_multi_dim_leaf_cells(&deref_target, &dims, &mut cells)
                .is_some()
            {
                self.stack.push(Value::array(cells));
                return Ok(());
            }
        } else if let Some(cell) = self.multi_dim_scalar_autoviv_cell(&target, &dims) {
            // All-scalar dims over a MISSING leaf: autovivify the path (creating
            // any absent intermediate arrays) and promote the terminal element
            // to a shared `ContainerRef` cell, so a `\raw` / `is rw` bind can
            // write to a not-yet-existent leaf (`@a[0;0;3] = v`). Eager, like the
            // single-index `:=` bind (`my $s := @a[5]`). Restricted to holes by
            // `ensure_array_child` / `array_slot_ref`, so a read-only use over an
            // existing structure is untouched.
            self.stack.push(cell);
            return Ok(());
        }
        let result = self.multi_dim_index_read(&target, &dims)?;
        self.stack.push(result);
        Ok(())
    }

    /// Autovivifying all-scalar-dimension descent for `MultiDimIndexBindRef`.
    /// Walks each scalar index, creating any missing intermediate array level,
    /// and returns the terminal element promoted to a shared `ContainerRef`
    /// cell. Returns `None` if a dimension is non-numeric, or the descent meets a
    /// real (non-hole) scalar / hash where a further array index still needs to
    /// descend — the caller then falls back to a plain (non-aliasing) read.
    fn multi_dim_scalar_autoviv_cell(&mut self, target: &Value, dims: &[Value]) -> Option<Value> {
        if dims.is_empty() {
            return None;
        }
        let mut cur = match target.view() {
            ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
            ValueView::Scalar(inner) => inner.clone(),
            _ => target.clone(),
        };
        if !matches!(cur.view(), ValueView::Array(..)) {
            return None;
        }
        for (i, dim) in dims.iter().enumerate() {
            let terminal = i + 1 == dims.len();
            let resolved = self
                .resolve_whatever_code_index(dim, &cur)
                .unwrap_or_else(|| dim.clone());
            let idx = Self::index_to_usize(&resolved)?;
            if terminal {
                return cur.array_slot_ref(idx, true);
            }
            cur = cur.ensure_array_child(idx)?;
        }
        None
    }

    /// Descend a nested array through the (possibly slice) dimensions, promoting
    /// every selected leaf to a shared `ContainerRef` cell and pushing the cells
    /// into `out` in row-major order. Returns `None` (caller falls back to a
    /// plain read) if any selected path is missing, out of bounds, or reaches a
    /// non-array where a further dimension still needs to descend — autovivifying
    /// a missing slot here would corrupt a read-only use of the same subscript.
    fn collect_multi_dim_leaf_cells(
        &mut self,
        cur: &Value,
        dims: &[Value],
        out: &mut Vec<Value>,
    ) -> Option<()> {
        if dims.is_empty() {
            return None;
        }
        let dim = Self::normalize_multidim_dim(&dims[0]);
        let rest = &dims[1..];
        let terminal = rest.is_empty();

        // Hash level: an explicit key (or key list) selects entries by name.
        // A `*` dimension over a hash falls back to the plain read — hash
        // iteration order is unspecified, so a positional `target = values`
        // distribution over it would be meaningless.
        if matches!(cur.view(), ValueView::Hash(..)) {
            let keys: Vec<String> = match dim.view() {
                ValueView::Whatever => return None,
                ValueView::Array(idxs, ..) => idxs.iter().map(Value::hash_key_encode).collect(),
                _ => vec![Value::hash_key_encode(&dim)],
            };
            for key in keys {
                if terminal {
                    out.push(cur.hash_slot_ref(&key, true)?);
                } else {
                    let child = match cur.hash_slot_ref(&key, false)? {
                        v if matches!(v.view(), ValueView::ContainerRef(_)) => {
                            v.with_deref(|inner| inner.clone())
                        }
                        v => v,
                    };
                    if !matches!(child.view(), ValueView::Array(..) | ValueView::Hash(..)) {
                        return None;
                    }
                    self.collect_multi_dim_leaf_cells(&child, rest, out)?;
                }
            }
            return Some(());
        }

        let items_len = match cur.view() {
            ValueView::Array(items, ..) => items.len(),
            _ => return None,
        };

        // Resolve this dimension into the concrete list of integer indices it
        // selects against the CURRENT container.
        let indices: Vec<usize> = match dim.view() {
            ValueView::Whatever => (0..items_len).collect(),
            ValueView::Array(idxs, ..) => {
                let mut v = Vec::with_capacity(idxs.len());
                for it in idxs.iter() {
                    let resolved = self
                        .resolve_whatever_code_index(it, cur)
                        .unwrap_or_else(|| it.clone());
                    v.push(Self::index_to_usize(&resolved)?);
                }
                v
            }
            _ => {
                let resolved = self
                    .resolve_whatever_code_index(&dim, cur)
                    .unwrap_or_else(|| dim.clone());
                vec![Self::index_to_usize(&resolved)?]
            }
        };

        // A `Whatever` (`*`) dimension only ever yields existing indices, so a
        // bare-slice read over an existing structure adds no elements. A missing
        // index reached through an EXPLICIT index (a list dim or the terminal
        // dim) autovivifies, matching the assignment semantics.
        for i in indices {
            if terminal {
                out.push(cur.array_slot_ref(i, true)?);
            } else {
                let child = cur.ensure_array_child(i)?;
                self.collect_multi_dim_leaf_cells(&child, rest, out)?;
            }
        }
        Some(())
    }

    /// Descend a nested array/hash through all-scalar dimensions, promoting the
    /// leaf to a shared `ContainerRef` cell (autovivifying missing intermediate
    /// levels). Returns `None` (caller falls back to a plain read) when any
    /// dimension is a slice, or when a non-terminal level is a non-descendable
    /// scalar / not-yet-existent hash key.
    fn multi_dim_slot_ref(
        &mut self,
        target: &Value,
        dims: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        if dims.is_empty() {
            return Ok(None);
        }
        // A slice dimension (`*`, a list, or a range) selects multiple leaves and
        // cannot collapse to a single cell.
        for d in dims {
            if matches!(
                Self::normalize_multidim_dim(d).view(),
                ValueView::Whatever | ValueView::Array(..)
            ) {
                return Ok(None);
            }
        }

        // Read through a `ContainerRef` / `Scalar` wrapper while keeping the
        // shared `Arc`, so in-place promotions land in the real container.
        let mut cur = match target.view() {
            ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
            ValueView::Scalar(inner) => inner.clone(),
            _ => target.clone(),
        };

        for (i, dim) in dims.iter().enumerate() {
            let terminal = i + 1 == dims.len();
            let resolved = self
                .resolve_whatever_code_index(dim, &cur)
                .unwrap_or_else(|| dim.clone());
            // Only descend a path that ALREADY exists. Autovivifying a missing
            // slot here is wrong: the same `@a[0;1;2]` expression is compiled as a
            // bind-ref for EVERY argument position, including read-only ones
            // (`is-deeply @a[0;1;2], ...`), so eagerly creating the slot would
            // corrupt a plain read. A missing leaf therefore falls back to a plain
            // read (the assignment to it is not aliased — a limitation pending a
            // deferred array-element ref, the array analogue of `HashEntryRef`).
            let next = match cur.view() {
                ValueView::Array(items, ..) => {
                    let Some(idx) = Self::index_to_usize(&resolved) else {
                        return Ok(None);
                    };
                    if idx >= items.len() {
                        return Ok(None);
                    }
                    match cur.array_slot_ref(idx, terminal) {
                        Some(v) => v,
                        None => return Ok(None),
                    }
                }
                ValueView::Hash(map, ..) => {
                    let key = Value::hash_key_encode(&resolved);
                    if !map.contains_key(&key) {
                        // Missing key: defer vivification with a deep-path
                        // `HashEntryRef` covering this and all remaining keys.
                        // A read resolves it to `Any` (`hash_entry_read`); the
                        // first write walk-creates the intermediate hashes
                        // (`hash_entry_terminal`), so `%h{$a;$b;$c} = v` through
                        // a `\target` / `is rw` bind autovivifies like raku.
                        let mut path = vec![key];
                        for d in &dims[i + 1..] {
                            path.push(Value::hash_key_encode(d));
                        }
                        return Ok(Some(Value::hash_entry_ref(map.clone(), path)));
                    }
                    match cur.hash_slot_ref(&key, terminal) {
                        Some(v) => v,
                        None => return Ok(None),
                    }
                }
                _ => return Ok(None),
            };
            if terminal {
                return Ok(Some(next));
            }
            // Descend into the intermediate level (which already exists).
            cur = if let ValueView::ContainerRef(cell) = next.view() {
                let inner = cell.lock().unwrap().clone();
                match inner.view() {
                    ValueView::Array(..) | ValueView::Hash(..) => {}
                    _ => return Ok(None),
                }
                inner
            } else if matches!(next.view(), ValueView::Array(..) | ValueView::Hash(..)) {
                next
            } else {
                return Ok(None);
            };
        }
        Ok(None)
    }

    /// Check that all scalar indices are within bounds for a shaped array.
    /// `dim_offset` tracks the 1-based dimension number for error messages.
    fn check_shaped_array_bounds(
        &self,
        target: &Value,
        dims: &[Value],
        dim_offset: usize,
    ) -> Result<(), RuntimeError> {
        if dims.is_empty() {
            return Ok(());
        }
        let dim = &dims[0];
        let rest = &dims[1..];

        match dim.view() {
            ValueView::Whatever => {
                // * iterates all elements — no bounds check needed at this level,
                // but recurse into each element for remaining dimensions
                if let ValueView::Array(items, ..) = target.view() {
                    for item in items.iter() {
                        self.check_shaped_array_bounds(item, rest, dim_offset + 1)?;
                    }
                }
                Ok(())
            }
            ValueView::Array(indices, ..) => {
                // Multiple indices — check each one
                let items = match target.view() {
                    ValueView::Array(items, ..) => items,
                    _ => return Ok(()),
                };
                for idx in indices.iter() {
                    if let Some(i) = Self::index_to_usize(idx) {
                        if i >= items.len() {
                            return Err(RuntimeError::new(format!(
                                "Index {} for dimension {} out of range (must be 0..{})",
                                i,
                                dim_offset + 1,
                                items.len() - 1
                            )));
                        }
                        self.check_shaped_array_bounds(&items[i], rest, dim_offset + 1)?;
                    }
                }
                Ok(())
            }
            _ => {
                // Scalar index
                let resolved = if let ValueView::Rat(n, d) = dim.view() {
                    Some(Value::int(n / d))
                } else if let ValueView::Num(f) = dim.view() {
                    Some(Value::int(f as i64))
                } else {
                    None
                };
                let idx = resolved.as_ref().unwrap_or(dim);
                if let Some(i) = Self::index_to_usize(idx) {
                    let items = match target.view() {
                        ValueView::Array(items, ..) => items,
                        _ => return Ok(()),
                    };
                    if i >= items.len() {
                        return Err(RuntimeError::new(format!(
                            "Index {} for dimension {} out of range (must be 0..{})",
                            i,
                            dim_offset + 1,
                            items.len() - 1
                        )));
                    }
                    self.check_shaped_array_bounds(&items[i], rest, dim_offset + 1)?;
                }
                Ok(())
            }
        }
    }

    /// Read a value from a nested array using multi-dimensional indices.
    /// Each dimension can be:
    /// - A scalar (Int, Str, Num, Rat, WhateverCode) — index into that level
    /// - Whatever (*) — iterate all elements at that level
    /// - An array/list — iterate specified indices at that level
    fn multi_dim_index_read(
        &mut self,
        target: &Value,
        dims: &[Value],
    ) -> Result<Value, RuntimeError> {
        if dims.is_empty() {
            return Ok(target.clone());
        }
        // A non-positional value behaves as a single-element list when
        // subscripted in a further dimension: in `(10,20,30)[1,2;0]` each
        // selected scalar is indexed by the trailing `0`, and `20[0]` is `20`
        // (raku treats a scalar as a 1-element list under subscript). Without
        // this, deeper dimensions on scalar leaves would all collapse to Nil.
        if !matches!(target.view(), ValueView::Array(..) | ValueView::Hash(..)) {
            let single = Value::array(vec![target.clone()]);
            return self.multi_dim_index_read(&single, dims);
        }
        let dim = Self::normalize_multidim_dim(&dims[0]);
        let dim = &dim;
        let rest = &dims[1..];

        // Hash targets index by key (string), recursing into the nested value
        // for the remaining dimensions: `%h{"a";"b";"c"}`.
        if let ValueView::Hash(map, ..) = target.view() {
            return self.multi_dim_hash_read(map, dim, rest);
        }

        match dim.view() {
            ValueView::Whatever => {
                // Iterate all elements at this level
                let items = match target.view() {
                    ValueView::Array(items, ..) => items,
                    _ => return Ok(Value::NIL),
                };
                let has_more_multi = rest.iter().any(|v| {
                    matches!(
                        Self::normalize_multidim_dim(v).view(),
                        // A block dimension (`{0,1}`) may resolve to a list of
                        // indices, so it too can produce a flattenable slice.
                        crate::value::ValueView::Whatever
                            | crate::value::ValueView::Array(..)
                            | crate::value::ValueView::Sub(..)
                    )
                });
                let mut out = Vec::with_capacity(items.len());
                for item in items.iter() {
                    let result = self.multi_dim_index_read(item, rest)?;
                    if has_more_multi {
                        // Flatten intermediate array results from deeper * or list dims
                        if let ValueView::Array(inner, ..) = result.view() {
                            out.extend(inner.iter().cloned());
                        } else {
                            out.push(result);
                        }
                    } else {
                        out.push(result);
                    }
                }
                Ok(Value::array(out))
            }
            ValueView::Array(indices, ..) => {
                // Multiple indices at this dimension level
                let items = match target.view() {
                    ValueView::Array(items, ..) => items,
                    _ => return Ok(Value::NIL),
                };
                let has_more_multi = rest.iter().any(|v| {
                    matches!(
                        Self::normalize_multidim_dim(v).view(),
                        // A block dimension (`{0,1}`) may resolve to a list of
                        // indices, so it too can produce a flattenable slice.
                        crate::value::ValueView::Whatever
                            | crate::value::ValueView::Array(..)
                            | crate::value::ValueView::Sub(..)
                    )
                });
                let mut out = Vec::with_capacity(indices.len());
                for idx in indices.iter() {
                    let result = if let Some(i) = Self::index_to_usize(idx) {
                        if i < items.len() {
                            self.multi_dim_index_read(&items[i], rest)?
                        } else {
                            self.multi_dim_index_read(&Value::NIL, rest)?
                        }
                    } else {
                        Value::NIL
                    };
                    if has_more_multi {
                        if let ValueView::Array(inner, ..) = result.view() {
                            out.extend(inner.iter().cloned());
                        } else {
                            out.push(result);
                        }
                    } else {
                        out.push(result);
                    }
                }
                Ok(Value::array(out))
            }
            _ => {
                // Scalar index — resolve WhateverCode / block first
                let resolved = self.resolve_whatever_code_index(dim, target);
                // A block subscript may return a Range or a list of indices
                // (e.g. `{0,1}` returns the List `(0,1)`). Re-dispatch such a
                // result as a slice dimension instead of a single scalar index.
                if let Some(r) = &resolved {
                    let norm = Self::normalize_multidim_dim(r);
                    if norm.as_list_items().is_some() {
                        let mut new_dims = Vec::with_capacity(rest.len() + 1);
                        new_dims.push(norm);
                        new_dims.extend_from_slice(rest);
                        return self.multi_dim_index_read(target, &new_dims);
                    }
                }
                let idx = resolved.as_ref().unwrap_or(dim);
                if let Some(i) = Self::index_to_usize(idx) {
                    let items = match target.view() {
                        ValueView::Array(items, ..) => items,
                        _ => return Ok(Value::NIL),
                    };
                    if i < items.len() {
                        self.multi_dim_index_read(&items[i], rest)
                    } else {
                        // Out of bounds — return Nil for scalar index
                        Ok(Value::NIL)
                    }
                } else {
                    // Non-numeric index (e.g., string "0")
                    let i = idx.to_string_value().parse::<usize>().ok();
                    if let Some(i) = i {
                        let items = match target.view() {
                            ValueView::Array(items, ..) => items,
                            _ => return Ok(Value::NIL),
                        };
                        if i < items.len() {
                            self.multi_dim_index_read(&items[i], rest)
                        } else {
                            Ok(Value::NIL)
                        }
                    } else {
                        Ok(Value::NIL)
                    }
                }
            }
        }
    }

    /// Read from a hash using one multi-dim dimension, recursing for the rest.
    /// The dimension may be a single key (scalar), a list of keys (slice), or
    /// `*` (all values). A missing key reads as `Nil`.
    fn multi_dim_hash_read(
        &mut self,
        map: &std::collections::HashMap<String, Value>,
        dim: &Value,
        rest: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Look up one key and recurse into the nested value for `rest`.
        // A missing key reads as the `Any` type object (raku hash semantics),
        // and short-circuits any remaining dimensions.
        let read_key =
            |this: &mut Self, key: &str, rest: &[Value]| -> Result<Value, RuntimeError> {
                match map.get(key) {
                    Some(v) => {
                        // Decontainerize a Scalar-wrapped nested value before recursing.
                        let inner = match v.view() {
                            ValueView::Scalar(b) => b.clone(),
                            _ => v.clone(),
                        };
                        this.multi_dim_index_read(&inner, rest)
                    }
                    None => Ok(Value::package(crate::symbol::Symbol::intern("Any"))),
                }
            };

        match dim.view() {
            // `*` — all values at this level.
            ValueView::Whatever => {
                let has_more_multi = rest.iter().any(|v| {
                    matches!(
                        Self::normalize_multidim_dim(v).view(),
                        // A block dimension (`{0,1}`) may resolve to a list of
                        // indices, so it too can produce a flattenable slice.
                        crate::value::ValueView::Whatever
                            | crate::value::ValueView::Array(..)
                            | crate::value::ValueView::Sub(..)
                    )
                });
                let mut out = Vec::with_capacity(map.len());
                for v in map.values() {
                    let inner = match v.view() {
                        ValueView::Scalar(b) => b.clone(),
                        _ => v.clone(),
                    };
                    let result = self.multi_dim_index_read(&inner, rest)?;
                    if has_more_multi && let ValueView::Array(items, ..) = result.view() {
                        out.extend(items.iter().cloned());
                    } else {
                        out.push(result);
                    }
                }
                Ok(Value::array(out))
            }
            // A list of keys — slice.
            ValueView::Array(keys, ..) => {
                let has_more_multi = rest.iter().any(|v| {
                    matches!(
                        Self::normalize_multidim_dim(v).view(),
                        // A block dimension (`{0,1}`) may resolve to a list of
                        // indices, so it too can produce a flattenable slice.
                        crate::value::ValueView::Whatever
                            | crate::value::ValueView::Array(..)
                            | crate::value::ValueView::Sub(..)
                    )
                });
                let mut out = Vec::with_capacity(keys.len());
                for key in keys.iter() {
                    let result = read_key(self, &key.to_string_value(), rest)?;
                    if has_more_multi && let ValueView::Array(items, ..) = result.view() {
                        out.extend(items.iter().cloned());
                    } else {
                        out.push(result);
                    }
                }
                Ok(Value::array(out))
            }
            // A single key.
            _ => read_key(self, &dim.to_string_value(), rest),
        }
    }

    /// Normalize a multi-dim subscript dimension into the shapes the indexing
    /// logic understands. A `Range`/`Seq` dimension is a multi-index slice, so
    /// it is expanded into an explicit `Array` of indices (matching how a bare
    /// `(0,1,2)` list dimension is already handled). Scalars, `Whatever`,
    /// `WhateverCode` (`Sub`), and `Array` dimensions are returned unchanged.
    /// Expand a `||`-spread subscript's single dimension into the real
    /// dimensions. `@a[|| @list]` parses to a one-dimension `MultiDimIndex`
    /// whose sole dimension is the `||` operand list; each ELEMENT of that list
    /// is a subscript dimension (`@a[|| ((0,1),0)]` ≡ `@a[(0,1);0]`). A single-
    /// dimension `MultiDimIndex` is produced ONLY by `||` (a `;`-list has 2+
    /// dims and a lone index parses to `Index`), so this expansion is
    /// unambiguous. A scalar operand (`@a[|| 5]` ≡ `@a[5]`) stays one dimension.
    pub(super) fn expand_pipe_multidim_dims(dims: Vec<Value>) -> Vec<Value> {
        if dims.len() == 1
            && let Some(items) = dims[0].as_list_items()
        {
            return items.to_vec();
        }
        dims
    }

    pub(super) fn normalize_multidim_dim(dim: &Value) -> Value {
        match dim.view() {
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => Value::array(expand_range_to_list(dim)),
            ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                Value::array(items.as_ref().clone())
            }
            _ => dim.clone(),
        }
    }

    /// Resolve WhateverCode (e.g., *-1) or numeric coercion for a dimension index.
    fn resolve_whatever_code_index(&mut self, dim: &Value, target: &Value) -> Option<Value> {
        if let ValueView::Sub(data) = dim.view() {
            let len = match target.view() {
                ValueView::Array(items, ..) => items.len() as i64,
                _ => 0,
            };
            let mut sub_env = data.env.clone();
            for p in &data.params {
                sub_env.insert(p.to_string(), Value::int(len));
            }
            let saved_env = std::mem::take(self.env_mut());
            *self.env_mut() = sub_env;
            let result = loan_env!(self, eval_block_value(&data.body)).unwrap_or(Value::NIL);
            *self.env_mut() = saved_env;
            return Some(result);
        }
        if let ValueView::Rat(n, d) = dim.view() {
            return Some(Value::int(n / d));
        }
        if let ValueView::Num(f) = dim.view() {
            return Some(Value::int(f as i64));
        }
        if let ValueView::Str(s) = dim.view()
            && let Ok(i) = s.parse::<i64>()
        {
            return Some(Value::int(i));
        }
        None
    }

    /// Multi-dimensional index assignment with named target.
    /// Stack: [value, dim0, dim1, ..., dimN-1]
    pub(super) fn exec_multi_dim_index_assign_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        ndims: u32,
    ) -> Result<(), RuntimeError> {
        let ndims = ndims as usize;
        let mut dims = Vec::with_capacity(ndims);
        for _ in 0..ndims {
            dims.push(self.stack.pop().unwrap_or(Value::NIL));
        }
        dims.reverse();
        let dims = Self::expand_pipe_multidim_dims(dims);
        let value = self.stack.pop().unwrap_or(Value::NIL);

        let var_name = Self::const_str(code, name_idx).to_string();

        // Resolve WhateverCode indices for the bound-index check and the shaped
        // path. The non-shaped assignment uses the RAW `dims` instead, because
        // it resolves each dimension against the actual nested container as it
        // descends (a flat pre-pass mis-resolves a deeper `*`/slice — it would
        // use the outermost length, not the inner container's).
        let target_val = self.env().get(&var_name).cloned().unwrap_or(Value::NIL);
        let resolved_dims = self.resolve_multidim_indices_for_assign(&target_val, &dims)?;

        // Check if the index is bound (read-only)
        let encoded_idx = resolved_dims
            .iter()
            .map(|d| d.to_string_value())
            .collect::<Vec<_>>()
            .join(";");
        if self.is_bound_index(&var_name, &encoded_idx) {
            return Err(RuntimeError::assignment_ro(None));
        }

        // Check if target is a shaped array - use bounds-checked assignment
        let declared_shape_key = format!("__mutsu_shaped_array_dims::{var_name}");
        let has_declared_shape = self.env().contains_key(&declared_shape_key);
        let is_shaped = has_declared_shape
            || self
                .env()
                .get(&var_name)
                .is_some_and(crate::runtime::utils::is_shaped_array);

        // Capture container type metadata before mutation (Arc pointer may change)
        let old_type_info = self
            .env()
            .get(&var_name)
            .cloned()
            .and_then(|v| self.container_type_metadata(&v));

        // If the variable is bound to a shared container cell — e.g. it was
        // assigned by a sub/closure that captured the outer variable, leaving a
        // `ContainerRef` in both env and locals — mutate THROUGH the cell. The
        // cell is shared, so the write is visible everywhere; mutating the env
        // snapshot (the path below) would only touch a copy and silently drop the
        // write. This mirrors the simple-index assignment's ContainerRef handling.
        let container_cell = match self.env().get(&var_name).map(Value::view) {
            Some(ValueView::ContainerRef(cell)) => Some(cell.clone()),
            _ => self
                .locals_get_by_name(code, &var_name)
                .and_then(|v| match v.view() {
                    ValueView::ContainerRef(cell) => Some(cell.clone()),
                    _ => None,
                }),
        };
        if let Some(cell) = container_cell {
            let mut inner = cell.lock().unwrap();
            if is_shaped {
                Self::assign_array_multidim(&mut inner, &resolved_dims, value.clone())?;
                drop(inner);
            } else {
                // Move the contents out of the guard so the assignment can
                // borrow `&mut self` (WhateverCode resolution needs it) without
                // also holding a borrow tied to `self` through the cell.
                let mut contents = std::mem::replace(&mut *inner, Value::NIL);
                drop(inner);
                self.multi_dim_assign(&mut contents, &dims, value.clone())?;
                *cell.lock().unwrap() = contents;
            }
            self.stack.push(value);
            return Ok(());
        }

        // Mutate a clone of the target variable, then store it back. (Taking a
        // `&mut` into env would conflict with the `&mut self` the assignment
        // needs for WhateverCode resolution.)
        if self.env().contains_key(&var_name) {
            let mut container = self.env().get(&var_name).cloned().unwrap_or(Value::NIL);
            if is_shaped {
                // For shaped arrays, use bounds-checked assignment
                Self::assign_array_multidim(&mut container, &resolved_dims, value.clone())?;
            } else {
                self.multi_dim_assign(&mut container, &dims, value.clone())?;
            }
            self.env_mut().insert(var_name.clone(), container);
        }
        // Also sync the updated container into the Interpreter locals slot (if any)
        // so that a later locals write-through does not restore the stale
        // pre-assignment copy from locals into env. Without this, shaped
        // array element writes like `@a[i;j] = v` can be silently lost
        // before a closure captures the env (e.g. `start { ... }`).
        if let Some(updated) = self.env().get(&var_name).cloned() {
            self.update_local_if_exists(code, &var_name, &updated);
        }

        // Re-register container type metadata if Arc pointer changed. Hashes
        // embed metadata in `HashData`, so the re-tagged value must be written
        // back (no-op Arc for array/instance side-table containers).
        if let Some(info) = old_type_info
            && let Some(updated) = self.env().get(&var_name).cloned()
        {
            let tagged = self.tag_container_metadata(updated, info);
            self.env_mut().insert(var_name.clone(), tagged.clone());
            self.update_local_if_exists(code, &var_name, &tagged);
        }

        self.stack.push(value);
        Ok(())
    }

    /// Multi-dimensional index assignment with generic (expression) target.
    /// Stack: [target, dim0, ..., dimN-1, value]
    pub(super) fn exec_multi_dim_index_assign_generic_op(
        &mut self,
        ndims: u32,
    ) -> Result<(), RuntimeError> {
        let ndims = ndims as usize;
        let value = self.stack.pop().unwrap_or(Value::NIL);
        let mut dims = Vec::with_capacity(ndims);
        for _ in 0..ndims {
            dims.push(self.stack.pop().unwrap_or(Value::NIL));
        }
        dims.reverse();
        let dims = Self::expand_pipe_multidim_dims(dims);
        let mut target = self.stack.pop().unwrap_or(Value::NIL);
        let is_shaped = crate::runtime::utils::is_shaped_array(&target);
        if is_shaped {
            let resolved_dims = self.resolve_multidim_indices_for_assign(&target, &dims)?;
            Self::assign_array_multidim(&mut target, &resolved_dims, value.clone())?;
        } else {
            self.multi_dim_assign(&mut target, &dims, value.clone())?;
        }
        self.stack.push(value);
        Ok(())
    }

    /// Whether a multi-dim subscript dimension puts the assignment into slice
    /// (list-distributing) context rather than single-element context. After
    /// normalization, `Whatever`, an explicit index list, and a `WhateverCode`
    /// (`Sub`, e.g. `*-1`) all make a multi-dim subscript a slice: raku assigns
    /// the RHS list element-wise to the selected leaves, so a single leaf takes
    /// only the first RHS element (`@a[0;0;*-1] = (7,8,9)` stores `7`). A plain
    /// scalar index keeps single-element semantics (`@a[0;0;0] = (7,8,9)`
    /// stores the whole list).
    fn dim_is_multi(dim: &Value) -> bool {
        matches!(
            Self::normalize_multidim_dim(dim).view(),
            ValueView::Whatever | ValueView::Array(..) | ValueView::Sub(..)
        )
    }

    /// Resolve one assignment dimension against the current `target` container
    /// into the concrete index/key values it selects. `Whatever` expands to all
    /// existing indices/keys of `target`; an explicit list resolves any
    /// `WhateverCode` elements; a bare `WhateverCode` resolves against the
    /// length of `target`; a scalar passes through unchanged.
    fn resolve_assign_dim(
        &mut self,
        target: &Value,
        dim: &Value,
    ) -> Result<Vec<Value>, RuntimeError> {
        let dim = Self::normalize_multidim_dim(dim);
        let deref = target.with_deref(|v| v.descalarize().clone());
        match dim.view() {
            ValueView::Whatever => match deref.view() {
                ValueView::Array(items, ..) => {
                    Ok((0..items.len() as i64).map(Value::int).collect())
                }
                ValueView::Hash(map, ..) => {
                    Ok(map.keys().map(|k| Value::str(k.to_string())).collect())
                }
                _ => Ok(vec![]),
            },
            ValueView::Array(items, ..) => {
                let len = match deref.view() {
                    ValueView::Array(arr, ..) => Value::int(arr.len() as i64),
                    _ => Value::int(0),
                };
                let mut out = Vec::with_capacity(items.len());
                for it in items.iter() {
                    if let ValueView::Sub(..) = it.view() {
                        out.push(self.call_sub_value(it.clone(), vec![len.clone()], false)?);
                    } else {
                        out.push(it.clone());
                    }
                }
                Ok(out)
            }
            ValueView::Sub(..) => {
                let len = match deref.view() {
                    ValueView::Array(items, ..) => Value::int(items.len() as i64),
                    _ => Value::int(0),
                };
                Ok(vec![self.call_sub_value(dim.clone(), vec![len], false)?])
            }
            _ => Ok(vec![dim.clone()]),
        }
    }

    /// Recursively assign a value into a nested array/hash at the given
    /// dimension subscripts. Each dimension is resolved against the *actual*
    /// nested container as the descent proceeds, so nested `*`/slice/Range/Seq
    /// dimensions select the correct indices at every level (a flat pre-pass
    /// cannot — a deeper `*` needs the inner container's length, not the
    /// outermost). A scalar-only subscript assigns the whole RHS to the single
    /// leaf; a subscript containing any slice dimension flattens the RHS list
    /// and assigns element-wise across the leaf cross-product in row-major
    /// order (extra leaves get `Any`, raku list-assignment semantics).
    fn multi_dim_assign(
        &mut self,
        target: &mut Value,
        dims: &[Value],
        value: Value,
    ) -> Result<(), RuntimeError> {
        if dims.iter().any(Self::dim_is_multi) {
            let values: Vec<Value> = if let ValueView::Array(items, ..) = value.view() {
                items.iter().cloned().collect()
            } else {
                vec![value]
            };
            let mut vi = 0usize;
            self.multi_dim_assign_slice(target, dims, &values, &mut vi)
        } else {
            self.multi_dim_assign_scalar(target, dims, value)
        }
    }

    /// Slice-distribution arm of `multi_dim_assign`: walk the leaf
    /// cross-product in row-major order, pulling the next RHS element for each
    /// leaf.
    fn multi_dim_assign_slice(
        &mut self,
        target: &mut Value,
        dims: &[Value],
        values: &[Value],
        vi: &mut usize,
    ) -> Result<(), RuntimeError> {
        if dims.is_empty() {
            let v = values
                .get(*vi)
                .cloned()
                .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("Any")));
            *vi += 1;
            *target = v;
            return Ok(());
        }
        let keys = self.resolve_assign_dim(target, &dims[0])?;
        let rest = &dims[1..];
        for key in keys {
            if !matches!(target.view(), ValueView::Hash(..))
                && let Some(i) = Self::index_to_usize(&key)
            {
                Self::ensure_array_size(target, i + 1);
                target
                    .with_array_mut(|items, _| {
                        let items = crate::gc::Gc::make_mut(items);
                        self.multi_dim_assign_slice(&mut items[i], rest, values, vi)
                    })
                    .transpose()?;
            } else if let ValueView::Str(s) = key.view() {
                Self::ensure_hash(target);
                target
                    .with_hash_mut(|map| {
                        let map = crate::gc::Gc::make_mut(map);
                        let entry = map.entry(s.as_str().to_string()).or_insert_with(|| {
                            Value::package(crate::symbol::Symbol::intern("Any"))
                        });
                        self.multi_dim_assign_slice(entry, rest, values, vi)
                    })
                    .transpose()?;
            }
        }
        Ok(())
    }

    /// Scalar (single-leaf) arm of `multi_dim_assign`: navigate one index/key
    /// per dimension and assign the whole RHS at the leaf.
    fn multi_dim_assign_scalar(
        &mut self,
        target: &mut Value,
        dims: &[Value],
        value: Value,
    ) -> Result<(), RuntimeError> {
        if dims.is_empty() {
            *target = value;
            return Ok(());
        }
        let key = self
            .resolve_assign_dim(target, &dims[0])?
            .into_iter()
            .next()
            .unwrap_or(Value::NIL);
        let rest = &dims[1..];
        // An array index that arrives as a non-Int scalar (`"0"`, `0e0`, `0/1`)
        // is coerced to its integer when the target is (or autovivifies to) an
        // array; only a genuine hash target keeps the string as a key.
        if !matches!(target.view(), ValueView::Hash(..))
            && let Some(i) = Self::index_to_usize(&key)
        {
            Self::ensure_array_size(target, i + 1);
            target
                .with_array_mut(|items, _| {
                    let items = crate::gc::Gc::make_mut(items);
                    self.multi_dim_assign_scalar(&mut items[i], rest, value)
                })
                .transpose()?;
        } else if let ValueView::Str(s) = key.view() {
            Self::ensure_hash(target);
            target
                .with_hash_mut(|map| {
                    let map = crate::gc::Gc::make_mut(map);
                    let entry = map
                        .entry(s.as_str().to_string())
                        .or_insert_with(|| Value::package(crate::symbol::Symbol::intern("Any")));
                    self.multi_dim_assign_scalar(entry, rest, value)
                })
                .transpose()?;
        } else {
            return Err(RuntimeError::new("Invalid index for multi-dim assignment"));
        }
        Ok(())
    }

    /// Ensure the target is a hash, converting from Nil/Any if necessary.
    fn ensure_hash(target: &mut Value) {
        if matches!(target.view(), ValueView::Nil | ValueView::Package(..)) {
            *target = Value::hash_with_data(Value::hash_arc(std::collections::HashMap::new()));
        }
    }

    /// Resolve WhateverCode indices for multidim assignment.
    fn resolve_multidim_indices_for_assign(
        &mut self,
        target: &Value,
        indices: &[Value],
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut resolved = Vec::with_capacity(indices.len());
        // A file-scoped `@a` shared across frames arrives as a `ContainerRef`
        // cell; read through it so the WhateverCode length probe (and the
        // navigation below) sees the real array.
        let mut current = target.with_deref(|v| v.descalarize().clone());
        for idx in indices {
            if matches!(idx.view(), ValueView::Whatever) {
                // * means "all existing indices" - expand to 0..len
                let len = match current.view() {
                    ValueView::Array(items, ..) => items.len(),
                    ValueView::Hash(..) => {
                        // For hashes, * means all existing keys
                        if let ValueView::Hash(map, ..) = current.view() {
                            let keys: Vec<Value> =
                                map.keys().map(|k| Value::str(k.to_string())).collect();
                            let result = Value::real_array(keys);
                            resolved.push(result);
                            continue;
                        }
                        0
                    }
                    _ => 0,
                };
                let all_indices: Vec<Value> = (0..len as i64).map(Value::int).collect();
                let result = Value::real_array(all_indices);
                // Don't advance current - Whatever applies to all elements
                resolved.push(result);
            } else if let ValueView::Sub(..) = idx.view() {
                let len = match current.view() {
                    ValueView::Array(items, ..) => Value::int(items.len() as i64),
                    _ => Value::int(0),
                };
                let result = self.call_sub_value(idx.clone(), vec![len], false)?;
                current =
                    Self::index_array_multidim(&current, std::slice::from_ref(&result), false)
                        .unwrap_or(Value::NIL);
                resolved.push(result);
            } else if let ValueView::Array(items, ..) = idx.view() {
                // Resolve any Sub/WhateverCode elements within the array
                let len = match current.view() {
                    ValueView::Array(arr, ..) => Value::int(arr.len() as i64),
                    _ => Value::int(0),
                };
                let mut resolved_items = Vec::with_capacity(items.len());
                for item in items.iter() {
                    if let ValueView::Sub(..) = item.view() {
                        let result =
                            self.vm_call_sub_value(item.clone(), vec![len.clone()], false)?;
                        resolved_items.push(result);
                    } else {
                        resolved_items.push(item.clone());
                    }
                }
                let result = Value::real_array(resolved_items);
                resolved.push(result);
            } else {
                current = Self::index_array_multidim(&current, std::slice::from_ref(idx), false)
                    .unwrap_or(Value::NIL);
                resolved.push(idx.clone());
            }
        }
        Ok(resolved)
    }

    /// Ensure the target is an array with at least `min_size` elements.
    fn ensure_array_size(target: &mut Value, min_size: usize) {
        if target
            .with_array_mut(|items, _| {
                if items.len() < min_size {
                    let items = crate::gc::Gc::make_mut(items);
                    items.resize(
                        min_size,
                        Value::package(crate::symbol::Symbol::intern("Any")),
                    );
                }
            })
            .is_none()
            && matches!(target.view(), ValueView::Nil | ValueView::Package(..))
        {
            let mut items = Vec::with_capacity(min_size);
            items.resize(
                min_size,
                Value::package(crate::symbol::Symbol::intern("Any")),
            );
            *target = Value::real_array(items);
        }
    }
}
