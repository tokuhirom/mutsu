use super::*;

impl VM {
    pub(super) fn exec_get_bare_word_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        let val = if name == "Bool::True" {
            Value::Bool(true)
        } else if name == "Bool::False" {
            Value::Bool(false)
        } else if name == "Order::Less" {
            Value::Enum {
                enum_type: "Order".to_string(),
                key: "Less".to_string(),
                value: -1,
                index: 0,
            }
        } else if name == "Order::Same" {
            Value::Enum {
                enum_type: "Order".to_string(),
                key: "Same".to_string(),
                value: 0,
                index: 1,
            }
        } else if name == "Order::More" {
            Value::Enum {
                enum_type: "Order".to_string(),
                key: "More".to_string(),
                value: 1,
                index: 2,
            }
        } else if let Some(v) = self.interpreter.env().get(name) {
            if matches!(v, Value::Enum { .. } | Value::Nil) {
                v.clone()
            } else if self.interpreter.has_class(name) || Self::is_builtin_type(name) {
                Value::Package(name.to_string())
            } else if !name.starts_with('$') && !name.starts_with('@') && !name.starts_with('%') {
                v.clone()
            } else {
                Value::Str(name.to_string())
            }
        } else if self.interpreter.has_class(name) || Self::is_builtin_type(name) {
            Value::Package(name.to_string())
        } else if self.interpreter.has_function(name) {
            if let Some(cf) = self.find_compiled_function(compiled_fns, name, &[]) {
                let pkg = self.interpreter.current_package().to_string();
                let result =
                    self.call_compiled_function_named(cf, Vec::new(), compiled_fns, &pkg, name)?;
                self.sync_locals_from_env(code);
                result
            } else if let Some(native_result) = Self::try_native_function(name, &[]) {
                native_result?
            } else {
                let result = self.interpreter.call_function(name, Vec::new())?;
                self.sync_locals_from_env(code);
                result
            }
        } else if self.interpreter.has_multi_function(name) {
            if let Some(cf) = self.find_compiled_function(compiled_fns, name, &[]) {
                let pkg = self.interpreter.current_package().to_string();
                let result =
                    self.call_compiled_function_named(cf, Vec::new(), compiled_fns, &pkg, name)?;
                self.sync_locals_from_env(code);
                result
            } else {
                let result = self.interpreter.call_function(name, Vec::new())?;
                self.sync_locals_from_env(code);
                result
            }
        } else if name == "NaN" {
            Value::Num(f64::NAN)
        } else if name == "Inf" {
            Value::Num(f64::INFINITY)
        } else {
            Value::Str(name.to_string())
        };
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_index_op(&mut self) -> Result<(), RuntimeError> {
        let index = self.stack.pop().unwrap();
        let mut target = self.stack.pop().unwrap();
        if let Value::LazyList(ref ll) = target {
            target = Value::Array(self.interpreter.force_lazy_list_bridge(ll)?);
        }
        let result = match (target, index) {
            (Value::Array(items), Value::Int(i)) => {
                if i < 0 {
                    Value::Nil
                } else {
                    items.get(i as usize).cloned().unwrap_or(Value::Nil)
                }
            }
            (Value::Array(items), Value::Range(a, b)) => {
                let start = a.max(0) as usize;
                let end = b.max(-1) as usize;
                let slice = if start >= items.len() {
                    Vec::new()
                } else {
                    let end = end.min(items.len().saturating_sub(1));
                    items[start..=end].to_vec()
                };
                Value::Array(slice)
            }
            (Value::Array(items), Value::RangeExcl(a, b)) => {
                let start = a.max(0) as usize;
                let end_excl = b.max(0) as usize;
                let slice = if start >= items.len() {
                    Vec::new()
                } else {
                    let end_excl = end_excl.min(items.len());
                    if start >= end_excl {
                        Vec::new()
                    } else {
                        items[start..end_excl].to_vec()
                    }
                };
                Value::Array(slice)
            }
            (Value::Hash(items), Value::Num(f)) if f.is_infinite() && f > 0.0 => {
                Value::Array(items.values().cloned().collect())
            }
            (Value::Hash(items), Value::Nil) => Value::Hash(items),
            (Value::Hash(items), Value::Array(keys)) => Value::Array(
                keys.iter()
                    .map(|k| {
                        let key = k.to_string_value();
                        items.get(&key).cloned().unwrap_or(Value::Nil)
                    })
                    .collect(),
            ),
            (Value::Hash(items), Value::Str(key)) => items.get(&key).cloned().unwrap_or(Value::Nil),
            (Value::Hash(items), Value::Int(key)) => {
                items.get(&key.to_string()).cloned().unwrap_or(Value::Nil)
            }
            (Value::Set(s), Value::Str(key)) => Value::Bool(s.contains(&key)),
            (Value::Set(s), idx) => Value::Bool(s.contains(&idx.to_string_value())),
            (Value::Bag(b), Value::Str(key)) => Value::Int(*b.get(&key).unwrap_or(&0)),
            (Value::Bag(b), idx) => Value::Int(*b.get(&idx.to_string_value()).unwrap_or(&0)),
            (Value::Mix(m), Value::Str(key)) => Value::Num(*m.get(&key).unwrap_or(&0.0)),
            (Value::Mix(m), idx) => Value::Num(*m.get(&idx.to_string_value()).unwrap_or(&0.0)),
            // Range indexing (supports infinite ranges)
            (ref range, Value::Int(i)) if range.is_range() => {
                let (start, _end, _excl_start, _excl_end) = range_params(range);
                if i < 0 {
                    Value::Nil
                } else {
                    Value::Int(start + i)
                }
            }
            (ref range, Value::RangeExcl(a, b)) if range.is_range() => {
                let (start, end, _excl_start, excl_end) = range_params(range);
                let actual_end = if excl_end { end - 1 } else { end };
                let mut result = Vec::new();
                for i in a..b {
                    let val = start + i;
                    if val > actual_end {
                        break;
                    }
                    result.push(Value::Int(val));
                }
                Value::Array(result)
            }
            (ref range, Value::Range(a, b)) if range.is_range() => {
                let (start, end, _excl_start, excl_end) = range_params(range);
                let actual_end = if excl_end { end - 1 } else { end };
                let mut result = Vec::new();
                for i in a..=b {
                    let val = start + i;
                    if val > actual_end {
                        break;
                    }
                    result.push(Value::Int(val));
                }
                Value::Array(result)
            }
            (ref range, Value::Array(indices)) if range.is_range() => {
                let (start, _end, _excl_start, _excl_end) = range_params(range);
                let result: Vec<Value> = indices
                    .iter()
                    .map(|idx| match idx {
                        Value::Int(i) => Value::Int(start + i),
                        _ => Value::Nil,
                    })
                    .collect();
                Value::Array(result)
            }
            // Type parameterization: e.g. Buf[uint8] → returns the type unchanged
            (pkg @ Value::Package(_), _) => pkg,
            _ => Value::Nil,
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_string_concat_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        let mut result = String::new();
        for v in values {
            result.push_str(&v.to_string_value());
        }
        self.stack.push(Value::Str(result));
    }

    pub(super) fn exec_post_increment_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = self.get_env_with_main_alias(name).unwrap_or(Value::Int(0));
        let new_val = match &val {
            Value::Int(i) => Value::Int(i + 1),
            Value::Bool(_) => Value::Bool(true),
            Value::Rat(n, d) => make_rat(n + d, *d),
            _ => Value::Int(1),
        };
        self.set_env_with_main_alias(name, new_val.clone());
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(val);
    }

    pub(super) fn exec_post_decrement_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = self.get_env_with_main_alias(name).unwrap_or(Value::Int(0));
        let new_val = match &val {
            Value::Int(i) => Value::Int(i - 1),
            Value::Bool(_) => Value::Bool(false),
            Value::Rat(n, d) => make_rat(n - d, *d),
            _ => Value::Int(-1),
        };
        self.set_env_with_main_alias(name, new_val.clone());
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(val);
    }

    pub(super) fn exec_post_increment_index_op(&mut self, code: &CompiledCode, name_idx: u32) {
        self.exec_post_inc_dec_index_op(code, name_idx, true);
    }

    pub(super) fn exec_post_decrement_index_op(&mut self, code: &CompiledCode, name_idx: u32) {
        self.exec_post_inc_dec_index_op(code, name_idx, false);
    }

    fn exec_post_inc_dec_index_op(&mut self, code: &CompiledCode, name_idx: u32, increment: bool) {
        let name = Self::const_str(code, name_idx).to_string();
        let idx_val = self.stack.pop().unwrap_or(Value::Nil);
        let key = idx_val.to_string_value();
        let current = if let Some(container) = self.interpreter.env().get(&name) {
            match container {
                Value::Hash(h) => h.get(&key).cloned().unwrap_or(Value::Nil),
                Value::Array(arr) => {
                    if let Ok(i) = key.parse::<usize>() {
                        arr.get(i).cloned().unwrap_or(Value::Nil)
                    } else {
                        Value::Nil
                    }
                }
                _ => Value::Nil,
            }
        } else {
            Value::Nil
        };
        let effective = match &current {
            Value::Nil => Value::Int(0),
            other => other.clone(),
        };
        let new_val = match &effective {
            Value::Int(i) => Value::Int(if increment { i + 1 } else { i - 1 }),
            Value::Rat(n, d) => {
                if increment {
                    make_rat(n + d, *d)
                } else {
                    make_rat(n - d, *d)
                }
            }
            _ => Value::Int(if increment { 1 } else { -1 }),
        };
        if let Some(container) = self.interpreter.env_mut().get_mut(&name) {
            match container {
                Value::Hash(h) => {
                    h.insert(key, new_val);
                }
                Value::Array(arr) => {
                    if let Ok(i) = idx_val.to_string_value().parse::<usize>() {
                        while arr.len() <= i {
                            arr.push(Value::Nil);
                        }
                        arr[i] = new_val;
                    }
                }
                _ => {}
            }
        }
        self.stack.push(effective);
    }

    pub(super) fn exec_index_assign_expr_named_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let var_name = Self::const_str(code, name_idx).to_string();
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let val = self.stack.pop().unwrap_or(Value::Nil);
        match &idx {
            Value::Array(keys) => {
                let vals = match &val {
                    Value::Array(v) => v.clone(),
                    _ => vec![val.clone()],
                };
                if let Some(Value::Hash(hash)) = self.interpreter.env_mut().get_mut(&var_name) {
                    for (i, key) in keys.iter().enumerate() {
                        let k = key.to_string_value();
                        let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                        hash.insert(k, v);
                    }
                }
            }
            _ => {
                let key = idx.to_string_value();
                if let Some(Value::Hash(hash)) = self.interpreter.env_mut().get_mut(&var_name) {
                    hash.insert(key, val.clone());
                } else {
                    let mut hash = std::collections::HashMap::new();
                    hash.insert(key, val.clone());
                    self.interpreter
                        .env_mut()
                        .insert(var_name, Value::Hash(hash));
                }
            }
        }
        self.stack.push(val);
    }

    pub(super) fn exec_index_assign_expr_nested_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let var_name = Self::const_str(code, name_idx).to_string();
        let inner_idx = self.stack.pop().unwrap_or(Value::Nil);
        let outer_idx = self.stack.pop().unwrap_or(Value::Nil);
        let val = self.stack.pop().unwrap_or(Value::Nil);
        let inner_key = inner_idx.to_string_value();
        let outer_key = outer_idx.to_string_value();

        if !self.interpreter.env().contains_key(&var_name) {
            self.interpreter.env_mut().insert(
                var_name.clone(),
                Value::Hash(std::collections::HashMap::new()),
            );
        }
        if let Some(Value::Hash(outer_hash)) = self.interpreter.env_mut().get_mut(&var_name) {
            let inner_hash = outer_hash
                .entry(inner_key)
                .or_insert_with(|| Value::Hash(std::collections::HashMap::new()));
            if let Value::Hash(h) = inner_hash {
                h.insert(outer_key, val.clone());
            }
        }
        self.stack.push(val);
    }

    pub(super) fn exec_get_local_op(&mut self, idx: u32) {
        self.stack.push(self.locals[idx as usize].clone());
    }

    pub(super) fn exec_set_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        let idx = idx as usize;
        let name = &code.locals[idx];
        let val = if name.starts_with('@') {
            if let Value::LazyList(ref list) = val {
                Value::Array(self.interpreter.force_lazy_list_bridge(list)?)
            } else {
                val
            }
        } else {
            val
        };
        let val = if name.starts_with('%') {
            runtime::coerce_to_hash(val)
        } else if name.starts_with('@') {
            runtime::coerce_to_array(val)
        } else {
            val
        };
        self.locals[idx] = val.clone();
        self.interpreter.env_mut().insert(name.clone(), val);
        Ok(())
    }

    pub(super) fn exec_assign_expr_local_op(&mut self, code: &CompiledCode, idx: u32) {
        let val = self.stack.last().unwrap().clone();
        let idx = idx as usize;
        let name = &code.locals[idx];
        let val = if name.starts_with('%') {
            runtime::coerce_to_hash(val)
        } else if name.starts_with('@') {
            runtime::coerce_to_array(val)
        } else {
            val
        };
        self.locals[idx] = val.clone();
        self.interpreter.env_mut().insert(name.clone(), val);
    }
}

/// Extract (start, end, excl_start, excl_end) from a Range value.
fn range_params(v: &Value) -> (i64, i64, bool, bool) {
    match v {
        Value::Range(a, b) => (*a, *b, false, false),
        Value::RangeExcl(a, b) => (*a, *b, false, true),
        Value::RangeExclStart(a, b) => (*a + 1, *b, true, false),
        Value::RangeExclBoth(a, b) => (*a + 1, *b, true, true),
        Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let s = start.to_f64() as i64;
            let e = end.to_f64() as i64;
            let s = if *excl_start { s + 1 } else { s };
            (s, e, *excl_start, *excl_end)
        }
        _ => (0, 0, false, false),
    }
}
