use super::*;

impl Interpreter {
    /// `cross(@a, @b, ...)` — Cartesian product of lists.
    /// With `with => &op`, applies the operator to each pair instead of making tuples.
    pub(super) fn builtin_cross(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let mut lists: Vec<Vec<Value>> = Vec::new();
        let mut with_func: Option<Value> = None;

        for arg in &args {
            match arg {
                Value::Pair(k, v) if k.as_str() == "with" => {
                    with_func = Some(v.as_ref().clone());
                }
                _ => {
                    let mut values = super::utils::value_to_list(arg);
                    if values.len() == 1
                        && let Some(single) = values.first()
                    {
                        match single {
                            Value::Array(items, _) => {
                                values = items.as_ref().clone().items;
                            }
                            Value::Seq(items) | Value::Slip(items) => {
                                values = items.as_ref().clone();
                            }
                            _ => {}
                        }
                    }
                    lists.push(values);
                }
            }
        }

        if lists.is_empty() {
            return Ok(Value::Seq(std::sync::Arc::new(vec![])));
        }

        // Compute Cartesian product iteratively
        let mut result: Vec<Vec<Value>> = vec![vec![]];
        for list in &lists {
            let mut new_result = Vec::new();
            for combo in &result {
                for item in list {
                    let mut new_combo = combo.clone();
                    new_combo.push(item.clone());
                    new_result.push(new_combo);
                }
            }
            result = new_result;
        }

        // Apply `with` function or create tuples. `cross` returns a Seq (so
        // `.^name` is Seq, `.raku` shows `.Seq`), matching Rakudo and the `X`
        // metaop.
        if let Some(func) = with_func {
            let mut final_result = Vec::new();
            for combo in result {
                let val = self.call_sub_value(func.clone(), combo, false)?;
                final_result.push(val);
            }
            Ok(Value::Seq(std::sync::Arc::new(final_result)))
        } else {
            // Return as list of lists (tuples)
            let tuples: Vec<Value> = result.into_iter().map(Value::array).collect();
            Ok(Value::Seq(std::sync::Arc::new(tuples)))
        }
    }

    pub(super) fn builtin_roundrobin(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
        }

        // Implement Raku's single-arg rule (+@lol): when called with a single
        // iterable arg, iterate it to get the list of streams.
        let effective_args: Vec<Value> = if args.len() == 1 {
            match &args[0] {
                Value::Array(items, kind) if kind.is_itemized() => args.to_vec(),
                Value::Array(items, _) => items.iter().cloned().collect(),
                Value::Seq(items) | Value::Slip(items) => items.iter().cloned().collect(),
                _ => args.to_vec(),
            }
        } else {
            args.to_vec()
        };

        if effective_args.is_empty() {
            return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
        }

        let streams: Vec<Vec<Value>> = effective_args
            .iter()
            .map(|arg| match arg {
                Value::Capture { positional, named }
                    if named.is_empty() && positional.len() == 1 =>
                {
                    vec![arg.clone()]
                }
                Value::Array(items, kind) if kind.is_itemized() => vec![arg.clone()],
                Value::Array(items, _) => items.iter().cloned().collect(),
                Value::Seq(items) | Value::Slip(items) => items.iter().cloned().collect(),
                Value::Range(a, b) => (*a..=*b).map(Value::Int).collect(),
                Value::RangeExcl(a, b) => (*a..*b).map(Value::Int).collect(),
                v if v.is_range() => crate::runtime::utils::value_to_list(v),
                other => vec![other.clone()],
            })
            .collect();

        let mut indices = vec![0usize; streams.len()];
        let mut rounds = Vec::new();
        loop {
            let mut tuple = Vec::new();
            let mut progressed = false;
            for (i, stream) in streams.iter().enumerate() {
                if indices[i] < stream.len() {
                    tuple.push(stream[indices[i]].clone());
                    indices[i] += 1;
                    progressed = true;
                }
            }
            if !progressed {
                break;
            }
            rounds.push(Value::array(tuple));
        }

        Ok(Value::Seq(std::sync::Arc::new(rounds)))
    }

    /// `duckmap(&block, \obj)` — apply block to each element; on type mismatch
    /// descend recursively into iterables, or return the element unchanged.
    pub(super) fn builtin_duckmap(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("duckmap requires a block and an object"));
        }
        let block = args[0].clone();
        let obj = args[1].clone();
        self.duckmap_iterate(&block, &obj)
    }

    /// `deepmap(&block, \obj)` — apply block to every leaf element, preserving structure.
    pub(super) fn builtin_deepmap(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new("deepmap requires a block and an object"));
        }
        let block = args[0].clone();
        let obj = args[1].clone();
        self.deepmap_iterate(&block, &obj)
    }

    /// Iterate over the elements of a value, applying duckmap to each.
    /// This is the entry point for both the method and function forms.
    pub(crate) fn duckmap_iterate(
        &mut self,
        block: &Value,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        match target {
            Value::Array(items, kind) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.duckmap_element(block, item) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) if e.is_last() => break,
                        Err(e) => return Err(e),
                    }
                }
                if kind.is_real_array() {
                    Ok(Value::real_array(result))
                } else {
                    Ok(Value::array(result))
                }
            }
            Value::Seq(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.duckmap_element(block, item) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) if e.is_last() => break,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::Seq(std::sync::Arc::new(result)))
            }
            Value::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    match self.duckmap_element(block, v) {
                        Ok(mapped) => {
                            result.insert(k.clone(), mapped);
                        }
                        Err(e) if e.is_next() => continue,
                        Err(e) if e.is_last() => break,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::Hash(Value::hash_arc(result)))
            }
            // Single non-iterable value: try the block on it directly
            _ => self.duckmap_element(block, target),
        }
    }

    /// Recursively apply a block to every leaf element, preserving structure.
    pub(crate) fn deepmap_iterate(
        &mut self,
        block: &Value,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        self.deepmap_iterate_inner(block, target, false)
    }

    /// Call the deepmap block on a leaf element through a transient
    /// `ContainerRef` cell, so a mutating callable (`++*`, `*--`, `$_++`)
    /// writes through — Raku's `deepmap` passes each leaf as a *container*
    /// and mutations are visible in the source structure. Returns the
    /// block's (decontainerized) result plus the cell's post-call value for
    /// the caller to write back into the source slot.
    fn deepmap_leaf_call(
        &mut self,
        block: &Value,
        leaf: &Value,
    ) -> Result<(Value, Value), RuntimeError> {
        let cell = std::sync::Arc::new(std::sync::Mutex::new(leaf.clone()));
        let res = self.call_sub_value(
            block.clone(),
            vec![Value::ContainerRef(cell.clone())],
            false,
        )?;
        let new_val = cell.lock().unwrap().clone();
        Ok((res.deref_container(), new_val))
    }

    /// Inner recursive helper. `itemize_result` is true for nested calls
    /// so that sublists get wrapped in Scalar containers.
    fn deepmap_iterate_inner(
        &mut self,
        block: &Value,
        target: &Value,
        itemize_result: bool,
    ) -> Result<Value, RuntimeError> {
        match target {
            // Type objects (e.g. Array, Hash) — return as-is to avoid hanging
            Value::Package(_) => Ok(target.clone()),
            Value::Array(items, kind) => {
                // A sublist is itemized (wrapped in a Scalar container) only when
                // its *parent* is a List, not when the parent is a real Array.
                // Compare Rakudo: `(1,[2,3]).deepmap(*+1)` -> `(2, $[3, 4])` but
                // `[1,[2,3]].deepmap(*+1)` -> `[2, [3, 4]]`.
                let child_itemize = !kind.is_real_array();
                let mut result = Vec::new();
                for (idx, item) in items.iter().enumerate() {
                    let is_leaf = !matches!(
                        item,
                        Value::Package(_) | Value::Array(..) | Value::Seq(_) | Value::Hash(_)
                    );
                    if is_leaf {
                        match self.deepmap_leaf_call(block, item) {
                            Ok((v, new_src)) => {
                                if new_src != *item {
                                    // Write the mutated leaf back into the source
                                    // array in place so all holders of the Arc see
                                    // it (Raku container semantics).
                                    // SAFETY: aliased in-place mutation of a shared
                                    // container; see `arc_contents_mut`.
                                    unsafe {
                                        crate::value::arc_contents_mut(items).items[idx] = new_src;
                                    }
                                }
                                result.push(v);
                            }
                            Err(e) if e.is_next() => continue,
                            Err(e) => return Err(e),
                        }
                        continue;
                    }
                    match self.deepmap_iterate_inner(block, item, child_itemize) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                let mut use_real_array = kind.is_real_array();
                // If the source array has a type constraint (e.g. `my Str @a`)
                // and the mapped values don't conform, downgrade to a List.
                if use_real_array && let Some(info) = self.container_type_metadata(target) {
                    let vt = &info.value_type;
                    if !vt.is_empty() && result.iter().any(|v| !v.isa_check(vt)) {
                        use_real_array = false;
                    }
                }
                let arr_kind = if use_real_array {
                    if itemize_result {
                        crate::value::ArrayKind::ItemArray
                    } else {
                        crate::value::ArrayKind::Array
                    }
                } else if itemize_result {
                    crate::value::ArrayKind::ItemList
                } else {
                    crate::value::ArrayKind::List
                };
                Ok(Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(result)),
                    arr_kind,
                ))
            }
            Value::Seq(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.deepmap_iterate_inner(block, item, true) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                if itemize_result {
                    // Itemize the result as a list
                    Ok(Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(result)),
                        crate::value::ArrayKind::ItemList,
                    ))
                } else {
                    Ok(Value::Seq(std::sync::Arc::new(result)))
                }
            }
            Value::Hash(map) => {
                let mut result = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    let is_leaf = !matches!(
                        v,
                        Value::Package(_) | Value::Array(..) | Value::Seq(_) | Value::Hash(_)
                    );
                    if is_leaf {
                        match self.deepmap_leaf_call(block, v) {
                            Ok((Value::Slip(items), _)) if items.is_empty() => continue,
                            Ok((val, new_src)) => {
                                if new_src != *v {
                                    // Write the mutated leaf back into the source
                                    // hash in place (see the Array arm).
                                    // SAFETY: aliased in-place mutation of a shared
                                    // container; see `arc_contents_mut`.
                                    unsafe {
                                        crate::value::arc_contents_mut(map)
                                            .map
                                            .insert(k.clone(), new_src);
                                    }
                                }
                                result.insert(k.clone(), val);
                            }
                            Err(e) if e.is_next() => continue,
                            Err(e) => return Err(e),
                        }
                        continue;
                    }
                    match self.deepmap_iterate_inner(block, v, true) {
                        Ok(Value::Slip(items)) if items.is_empty() => {
                            // Empty slip means the block returned Empty;
                            // drop the key from the result hash.
                            continue;
                        }
                        Ok(val) => {
                            result.insert(k.clone(), val);
                        }
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::Hash(Value::hash_arc(result)))
            }
            // Leaf value: apply the block (through a transient container so
            // mutating callables write through to a bare top-level leaf too).
            _ => self.deepmap_leaf_call(block, target).map(|(v, _)| v),
        }
    }

    /// `nodemap` — apply a block to each element without descending into sublists.
    pub(crate) fn nodemap_iterate(
        &mut self,
        block: &Value,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        match target {
            // nodemap always returns a List, even from a real Array or a Seq.
            // Compare Rakudo: `[2,3].nodemap(*+1).WHAT` is `List`.
            Value::Array(items, _kind) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.call_sub_value(block.clone(), vec![item.clone()], false) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::array(result))
            }
            Value::Seq(items) => {
                let mut result = Vec::new();
                for item in items.iter() {
                    match self.call_sub_value(block.clone(), vec![item.clone()], false) {
                        Ok(v) => result.push(v),
                        Err(e) if e.is_next() => continue,
                        Err(e) => return Err(e),
                    }
                }
                Ok(Value::array(result))
            }
            // Single value: apply the block directly
            _ => self.call_sub_value(block.clone(), vec![target.clone()], false),
        }
    }

    /// Apply duckmap to a single element: try the block, on failure descend.
    fn duckmap_element(&mut self, block: &Value, value: &Value) -> Result<Value, RuntimeError> {
        // Try to call the block with this value
        match self.call_sub_value(block.clone(), vec![value.clone()], false) {
            Ok(result) => Ok(result),
            Err(e) if e.is_next() || e.is_last() || e.is_redo() => {
                // Propagate loop control signals (next, last, redo)
                Err(e)
            }
            Err(_) => {
                // Block rejected the value (type mismatch, etc.)
                // Try to descend into iterable structures
                match value {
                    Value::Array(items, kind) => {
                        let mut result = Vec::new();
                        for item in items.iter() {
                            result.push(self.duckmap_element(block, item)?);
                        }
                        if kind.is_real_array() {
                            Ok(Value::real_array(result))
                        } else {
                            Ok(Value::array(result))
                        }
                    }
                    Value::Seq(items) => {
                        let mut result = Vec::new();
                        for item in items.iter() {
                            result.push(self.duckmap_element(block, item)?);
                        }
                        Ok(Value::Seq(std::sync::Arc::new(result)))
                    }
                    Value::Hash(map) => {
                        let mut result = std::collections::HashMap::new();
                        for (k, v) in map.iter() {
                            result.insert(k.clone(), self.duckmap_element(block, v)?);
                        }
                        Ok(Value::Hash(Value::hash_arc(result)))
                    }
                    // Not iterable — return unchanged
                    _ => Ok(value.clone()),
                }
            }
        }
    }
}
