use super::*;
use crate::value::RuntimeError;

/// Returns true if the value may have a custom `.gist`/`.Str` method that
/// requires interpreter method dispatch.  For all other (primitive) types
/// we can use the fast `gist_value()` / `to_string_value()` paths directly.
fn needs_method_dispatch(v: &Value) -> bool {
    matches!(
        v,
        Value::Instance { .. }
            | Value::CustomType { .. }
            | Value::CustomTypeInstance { .. }
            | Value::Mixin(..)
            | Value::Proxy { .. }
            | Value::Junction { .. }
    )
}

/// Check if a value is a Rat/FatRat/BigRat with zero denominator and throw
/// X::Numeric::DivideByZero if so (Raku defers the error until the value is used).
fn check_rat_divide_by_zero(v: &Value) -> Result<(), RuntimeError> {
    match v {
        Value::Rat(n, d) if *d == 0 => Err(RuntimeError::numeric_divide_by_zero_with(Some(
            Value::Int(*n),
        ))),
        Value::FatRat(n, d) if *d == 0 => Err(RuntimeError::numeric_divide_by_zero_with(Some(
            Value::Int(*n),
        ))),
        Value::BigRat(n, d) if d.is_zero() => Err(RuntimeError::numeric_divide_by_zero_with(Some(
            Value::from_bigint(n.clone()),
        ))),
        _ => Ok(()),
    }
}

impl VM {
    pub(super) fn exec_make_array_op(&mut self, n: u32, is_real_array: bool) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut elems = Vec::with_capacity(raw.len());
        for val in raw {
            // Force lazy IO lines into eager arrays
            let val = if matches!(&val, Value::LazyIoLines { .. }) {
                match self.force_if_lazy_io_lines(val) {
                    Ok(v) => v,
                    Err(_) => continue,
                }
            } else {
                val
            };
            match val {
                Value::Slip(items) => elems.extend(items.iter().cloned()),
                Value::Array(items, kind) if kind.is_itemized() => {
                    elems.push(Value::Array(items, kind))
                }
                // Scalar-wrapped values (.item / $()) are never flattened.
                Value::Scalar(inner) => elems.push(Value::Scalar(inner)),
                // In bracket-array literals (`[...]`), a single element is in
                // list context and should flatten one level (e.g. `[2..6]`,
                // `[@a]`, `[(1,2,3)]`), while multi-element forms keep each
                // element itemized (e.g. `[(1,2),(3,4)]`).
                other if is_real_array && n == 1 => elems.extend(runtime::value_to_list(&other)),
                other => elems.push(other),
            }
        }
        if is_real_array {
            self.stack.push(Value::real_array(elems));
        } else {
            self.stack.push(Value::array(elems));
        }
    }

    /// Like `exec_make_array_op` with `is_real_array=true` but never flattens
    /// single elements. Used for bracket arrays with trailing comma (`[x,]`)
    /// and for `[$scalar]` / `[$%h]` to prevent hash/array flattening.
    pub(super) fn exec_make_array_no_flatten_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut elems = Vec::with_capacity(raw.len());
        for val in raw {
            match val {
                Value::Slip(items) => elems.extend(items.iter().cloned()),
                // Nil in list context contributes nothing (same as value_to_list).
                Value::Nil => {}
                other => elems.push(other),
            }
        }
        self.stack.push(Value::real_array(elems));
    }

    pub(super) fn exec_make_hash_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n * 2;
        let items: Vec<Value> = self.stack.drain(start..).collect();
        let mut map = HashMap::new();
        for pair in items.chunks(2) {
            let key = Value::hash_key_encode(&pair[0]);
            let val = pair[1].clone();
            map.insert(key, val);
        }
        self.stack.push(Value::hash(map));
    }

    /// Build a Hash from N Pair values on the stack (from `%(k=>v, ...)` syntax).
    pub(super) fn exec_make_hash_from_pairs_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let items: Vec<Value> = self.stack.drain(start..).collect();
        let mut map = HashMap::new();
        for item in items {
            match item {
                Value::Pair(k, v) => {
                    map.insert(k, *v);
                }
                Value::ValuePair(k, v) => {
                    map.insert(k.to_string_value(), *v);
                }
                _ => {
                    // Non-pair values: use stringified value as key mapped to True
                    map.insert(item.to_string_value(), Value::Bool(true));
                }
            }
        }
        self.stack.push(Value::hash(map));
    }

    pub(super) fn exec_make_capture_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut positional = Vec::new();
        let mut named = HashMap::new();
        for val in raw {
            match val {
                Value::Pair(k, v) => {
                    named.insert(k, *v);
                }
                Value::Capture {
                    positional: p,
                    named: n,
                } => {
                    // Flatten inner capture (from |capture slip)
                    positional.extend(p);
                    named.extend(n);
                }
                Value::Slip(items) => {
                    for item in items.iter() {
                        match item {
                            Value::Pair(k, v) => {
                                named.insert(k.clone(), *v.clone());
                            }
                            other => positional.push(other.clone()),
                        }
                    }
                }
                other => positional.push(other),
            }
        }
        self.stack.push(Value::Capture { positional, named });
    }

    pub(super) fn exec_say_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        let mut parts = Vec::new();
        for v in &values {
            let v = self.interpreter.auto_fetch_proxy(v)?;
            check_rat_divide_by_zero(&v)?;
            // Resolve bound-element sentinels inside arrays before gist
            let v = self.resolve_bound_array_elements(v);
            if needs_method_dispatch(&v) {
                parts.push(self.interpreter.render_gist_value(&v));
            } else {
                parts.push(runtime::gist_value(&v));
            }
        }
        let line = parts.join("");
        self.interpreter
            .write_to_named_handle("$*OUT", &line, true)?;
        Ok(())
    }

    pub(super) fn exec_note_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let content = if n == 0 {
            "Noted".to_string()
        } else {
            let start = self.stack.len() - n;
            let values: Vec<Value> = self.stack.drain(start..).collect();
            let mut parts = Vec::new();
            for v in &values {
                if needs_method_dispatch(v) {
                    parts.push(self.interpreter.render_gist_value(v));
                } else {
                    parts.push(runtime::gist_value(v));
                }
            }
            parts.join("")
        };
        self.interpreter
            .write_to_named_handle("$*ERR", &content, true)?;
        Ok(())
    }

    pub(super) fn exec_put_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        // put threads through Junctions: each eigenstate gets put individually
        let mut lines = Vec::new();
        for v in &values {
            let v = self.interpreter.auto_fetch_proxy(v)?;
            check_rat_divide_by_zero(&v)?;
            self.collect_put_lines(&v, &mut lines)?;
        }
        for line in &lines {
            self.interpreter
                .write_to_named_handle("$*OUT", line, true)?;
        }
        Ok(())
    }

    pub(super) fn exec_print_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        let mut content = String::new();
        for v in &values {
            check_rat_divide_by_zero(v)?;
            // For Junctions, thread: call .Str on each element recursively
            self.collect_str_threaded(v, &mut content)?;
        }
        self.interpreter
            .write_to_named_handle("$*OUT", &content, false)?;
        Ok(())
    }

    /// Recursively collect put lines from a value, threading through Junctions.
    fn collect_put_lines(
        &mut self,
        v: &Value,
        lines: &mut Vec<String>,
    ) -> Result<(), RuntimeError> {
        match v {
            Value::Junction { values, .. } => {
                for elem in values.iter() {
                    self.collect_put_lines(elem, lines)?;
                }
            }
            _ if needs_method_dispatch(v) => {
                lines.push(self.interpreter.render_str_value(v));
            }
            _ => {
                lines.push(v.to_str_context());
            }
        }
        Ok(())
    }

    /// Recursively collect .Str output from a value, threading through Junctions.
    fn collect_str_threaded(&mut self, v: &Value, out: &mut String) -> Result<(), RuntimeError> {
        match v {
            Value::Junction { values, .. } => {
                for elem in values.iter() {
                    self.collect_str_threaded(elem, out)?;
                }
            }
            _ if needs_method_dispatch(v) => {
                out.push_str(&self.interpreter.render_str_value(v));
            }
            _ => {
                out.push_str(&v.to_str_context());
            }
        }
        Ok(())
    }
}

impl VM {
    /// Fast path for @arr.push(val) — directly appends to the array Arc.
    pub(super) fn exec_array_push_op(
        &mut self,
        code: &CompiledCode,
        target_name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let target_name = Self::const_str(code, target_name_idx);
        // Fall back to interpreter for shared arrays (threaded context)
        if self.interpreter.shared_vars_active {
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let target = self
                .interpreter
                .env()
                .get(target_name)
                .cloned()
                .unwrap_or(Value::Nil);
            let result = self
                .interpreter
                .call_method_with_values(target, "push", vec![val])?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }
        // Check for shaped arrays — must fall back to interpreter
        // (push is illegal on fixed-dimension arrays)
        if let Some(Value::Array(_, kind)) = self.interpreter.env().get(target_name)
            && *kind == crate::value::ArrayKind::Shaped
        {
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let target = self
                .interpreter
                .env()
                .get(target_name)
                .cloned()
                .unwrap_or(Value::Nil);
            let result = self
                .interpreter
                .call_method_with_values(target, "push", vec![val])?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }
        let val = self.stack.pop().unwrap_or(Value::Nil);

        // Empty (empty Slip) means nothing to push -- return the array as-is.
        if let Value::Slip(ref items) = val
            && items.is_empty()
        {
            let result = self
                .interpreter
                .env()
                .get(target_name)
                .cloned()
                .unwrap_or(Value::Nil);
            self.stack.push(result);
            return Ok(());
        }

        // Check the target exists as a simple Array in env.
        // If not (e.g., captured closure var, or non-Array), fall back to interpreter.
        let is_simple_array = self
            .interpreter
            .env()
            .get(target_name)
            .is_some_and(|v| matches!(v, Value::Array(..)));
        if !is_simple_array {
            let target = self
                .interpreter
                .env()
                .get(target_name)
                .cloned()
                .unwrap_or(Value::Nil);
            let result = self
                .interpreter
                .call_method_with_values(target, "push", vec![val])?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }

        // Check type constraint on the array variable.
        // For Slip values, check each element individually.
        if let Some(type_name) = self
            .interpreter
            .var_type_constraint_fast(target_name)
            .map(|s| s.to_string())
        {
            let items_to_check: Vec<&Value> = match &val {
                Value::Slip(items) => items.iter().collect(),
                other => vec![other],
            };
            for item in items_to_check {
                if !self.interpreter.type_matches_value(&type_name, item) {
                    return Err(RuntimeError::typecheck_assignment(
                        &type_name,
                        crate::runtime::value_type_name(item),
                        Some(target_name),
                    ));
                }
            }
        }

        // Find the local slot and drop it to allow in-place mutation
        let local_slot = self.find_local_slot(code, target_name);
        if let Some(slot) = local_slot {
            self.locals[slot] = Value::Nil;
        }

        let result = if let Some(Value::Array(arr, kind)) =
            self.interpreter.env_mut().get_mut(target_name)
        {
            let items = Arc::make_mut(arr);
            match &val {
                Value::Slip(slip_items) => items.extend(slip_items.iter().cloned()),
                _ => items.push(val),
            }
            Value::Array(arr.clone(), *kind)
        } else {
            // Auto-vivify: create new array
            let arr = match &val {
                Value::Slip(slip_items) => Value::real_array(slip_items.to_vec()),
                _ => Value::real_array(vec![val]),
            };
            self.interpreter
                .env_mut()
                .insert(target_name.to_string(), arr.clone());
            arr
        };

        // Restore local slot
        if let Some(slot) = local_slot {
            self.locals[slot] = result.clone();
        }

        self.stack.push(result);
        // Slice 6.3 step 2: no env_dirty mark. This native push path mutates only
        // `target_name` in env and has already reverse-write-through'd the result
        // into its local slot just above, so the caller's slot is coherent — a
        // pull would be redundant. (The interpreter-fallback push branches above,
        // for shared/shaped/non-simple-array targets, keep their conservative mark.)
        Ok(())
    }
}
