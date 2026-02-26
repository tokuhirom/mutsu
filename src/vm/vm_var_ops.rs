use super::*;
use std::sync::Arc;

const SELF_HASH_REF_SENTINEL: &str = "__mutsu_self_hash_ref";

impl VM {
    fn self_hash_ref_marker() -> Value {
        Value::Pair(
            SELF_HASH_REF_SENTINEL.to_string(),
            Box::new(Value::Bool(true)),
        )
    }

    fn resolve_hash_entry(items: &Arc<HashMap<String, Value>>, key: &str) -> Value {
        match items.get(key) {
            Some(Value::Pair(name, _)) if name == SELF_HASH_REF_SENTINEL => {
                Value::Hash(items.clone())
            }
            Some(value) => value.clone(),
            None => Value::Nil,
        }
    }

    pub(super) fn anon_state_key(name: &str) -> Option<String> {
        if name.starts_with("__ANON_STATE_") {
            Some(format!("__anon_state::{name}"))
        } else {
            None
        }
    }

    /// Convert a Failure's exception Value into a RuntimeError.
    fn failure_to_error(exception: &Value) -> RuntimeError {
        let message = if let Value::Instance { attributes, .. } = exception {
            attributes
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| "Died".to_string())
        } else {
            "Died".to_string()
        };
        let mut err = RuntimeError::new(&message);
        err.exception = Some(Box::new(exception.clone()));
        err
    }

    /// If the value is a Failure, throw its contained exception.
    fn throw_if_failure(value: &Value) -> Result<(), RuntimeError> {
        match value {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => {
                if let Some(ex) = attributes.get("exception") {
                    return Err(Self::failure_to_error(ex));
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub(super) fn anon_state_value(&self, name: &str) -> Option<Value> {
        let key = Self::anon_state_key(name)?;
        self.interpreter.get_state_var(&key).cloned()
    }

    pub(super) fn sync_anon_state_value(&mut self, name: &str, value: &Value) {
        if let Some(key) = Self::anon_state_key(name) {
            self.interpreter.set_state_var(key, value.clone());
        }
    }

    fn term_symbol_from_name(name: &str) -> Option<&str> {
        let bytes = name.as_bytes();
        if bytes.is_empty() {
            return None;
        }
        let starts_like_term = if bytes[0] == b't' {
            name.starts_with("term:<")
        } else if bytes[0] == b'&' {
            name.starts_with("&term:<")
        } else {
            false
        };
        if !starts_like_term {
            return None;
        }
        name.strip_prefix("term:<")
            .or_else(|| name.strip_prefix("&term:<"))
            .and_then(|s| s.strip_suffix('>'))
    }

    fn array_depth(value: &Value) -> usize {
        if let Some(shape) = crate::runtime::utils::shaped_array_shape(value)
            && !shape.is_empty()
        {
            return shape.len();
        }
        match value {
            Value::Array(items, ..) => {
                let child = items
                    .first()
                    .map(Self::array_depth)
                    .filter(|d| *d > 0)
                    .unwrap_or(0);
                1 + child
            }
            _ => 0,
        }
    }

    fn index_to_usize(idx: &Value) -> Option<usize> {
        match idx {
            Value::Int(i) if *i >= 0 => Some(*i as usize),
            Value::Num(f) if *f >= 0.0 => Some(*f as usize),
            _ => idx.to_string_value().parse::<usize>().ok(),
        }
    }

    fn not_enough_dimensions_error(operation: &str, got: usize, needed: usize) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("operation".to_string(), Value::Str(operation.to_string()));
        attrs.insert("got-dimensions".to_string(), Value::Int(got as i64));
        attrs.insert("needed-dimensions".to_string(), Value::Int(needed as i64));
        attrs.insert(
            "message".to_string(),
            Value::Str(format!(
                "Not enough dimensions: got {}, needed {}",
                got, needed
            )),
        );
        let mut err = RuntimeError::new("X::NotEnoughDimensions");
        err.exception = Some(Box::new(Value::make_instance(
            "X::NotEnoughDimensions".to_string(),
            attrs,
        )));
        err
    }

    fn index_array_multidim(
        target: &Value,
        indices: &[Value],
        strict_oob: bool,
    ) -> Result<Value, RuntimeError> {
        if indices.is_empty() {
            return Ok(target.clone());
        }
        let head = &indices[0];
        if matches!(head, Value::Whatever)
            || matches!(head, Value::Num(f) if f.is_infinite() && *f > 0.0)
        {
            if indices.len() > 1 && crate::runtime::utils::is_shaped_array(target) {
                return Err(RuntimeError::new("X::NYI"));
            }
            let Value::Array(items, ..) = target else {
                return Ok(Value::Nil);
            };
            let mut out = Vec::with_capacity(items.len());
            for item in items.iter() {
                out.push(Self::index_array_multidim(item, &indices[1..], strict_oob)?);
            }
            return Ok(Value::array(out));
        }
        let Some(i) = Self::index_to_usize(head) else {
            return Ok(Value::Nil);
        };
        let Value::Array(items, ..) = target else {
            return Ok(Value::Nil);
        };
        if i >= items.len() {
            if strict_oob {
                return Err(RuntimeError::new("Index out of bounds"));
            }
            return Ok(Value::Nil);
        }
        Self::index_array_multidim(&items[i], &indices[1..], strict_oob)
    }

    fn assign_array_multidim(
        target: &mut Value,
        indices: &[Value],
        val: Value,
    ) -> Result<(), RuntimeError> {
        let shape = crate::runtime::utils::shaped_array_shape(target);
        let depth = shape
            .as_ref()
            .map_or_else(|| Self::array_depth(target), |s| s.len());
        if indices.len() < depth && depth > 1 {
            return Err(Self::not_enough_dimensions_error(
                "assign to",
                indices.len(),
                depth,
            ));
        }
        if indices.is_empty() {
            return Err(RuntimeError::new("Index out of bounds"));
        }
        let Some(i) = Self::index_to_usize(&indices[0]) else {
            return Err(RuntimeError::new("Index out of bounds"));
        };
        let Value::Array(items, ..) = target else {
            return Err(RuntimeError::new("Index out of bounds"));
        };
        if i >= items.len() {
            return Err(RuntimeError::new("Index out of bounds"));
        }
        let arr = Arc::make_mut(items);
        if indices.len() == 1 {
            arr[i] = val;
        } else {
            Self::assign_array_multidim(&mut arr[i], &indices[1..], val)?;
        }
        if let Some(shape) = shape.as_deref() {
            crate::runtime::utils::mark_shaped_array_items(items, Some(shape));
        }
        Ok(())
    }

    fn delete_array_multidim(target: &mut Value, indices: &[Value]) -> Result<Value, RuntimeError> {
        let shape = crate::runtime::utils::shaped_array_shape(target);
        let depth = shape
            .as_ref()
            .map_or_else(|| Self::array_depth(target), |s| s.len());
        if indices.len() < depth && depth > 1 {
            return Err(Self::not_enough_dimensions_error(
                "delete from",
                indices.len(),
                depth,
            ));
        }
        if indices.is_empty() {
            return Ok(Value::Package("Any".to_string()));
        }
        let Some(i) = Self::index_to_usize(&indices[0]) else {
            return Ok(Value::Package("Any".to_string()));
        };
        let Value::Array(items, ..) = target else {
            return Ok(Value::Package("Any".to_string()));
        };
        if i >= items.len() {
            return Ok(Value::Package("Any".to_string()));
        }
        let arr = Arc::make_mut(items);
        if indices.len() == 1 {
            let prev = arr[i].clone();
            arr[i] = Value::Nil;
            if let Some(shape) = shape.as_deref() {
                crate::runtime::utils::mark_shaped_array_items(items, Some(shape));
            }
            return Ok(prev);
        }
        let deleted = Self::delete_array_multidim(&mut arr[i], &indices[1..])?;
        if let Some(shape) = shape.as_deref() {
            crate::runtime::utils::mark_shaped_array_items(items, Some(shape));
        }
        Ok(deleted)
    }

    fn encode_bound_index(idx: &Value) -> String {
        match idx {
            Value::Array(indices, ..) => indices
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(";"),
            _ => idx.to_string_value(),
        }
    }

    fn is_bound_index(&self, var_name: &str, encoded: &str) -> bool {
        let key = format!("__mutsu_bound_index::{}", var_name);
        if let Some(Value::Hash(map)) = self.interpreter.env().get(&key) {
            map.contains_key(encoded)
        } else {
            false
        }
    }

    fn mark_bound_index(&mut self, var_name: &str, encoded: String) {
        let key = format!("__mutsu_bound_index::{}", var_name);
        if let Some(Value::Hash(map)) = self.interpreter.env_mut().get_mut(&key) {
            Arc::make_mut(map).insert(encoded, Value::Bool(true));
            return;
        }
        let mut map = std::collections::HashMap::new();
        map.insert(encoded, Value::Bool(true));
        self.interpreter.env_mut().insert(key, Value::hash(map));
    }

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
        } else if (name.starts_with("infix:<")
            || name.starts_with("prefix:<")
            || name.starts_with("postfix:<"))
            && name.ends_with('>')
        {
            Value::Routine {
                package: "GLOBAL".to_string(),
                name: name.to_string(),
                is_regex: false,
            }
        } else if self.interpreter.is_name_suppressed(name) {
            return Err(RuntimeError::new(format!(
                "X::Undeclared::Symbols: Undeclared name:\n    {} used at line 1",
                name,
            )));
        } else if let Some((pkg, sym)) = name.rsplit_once("::")
            && let Some(stripped_sym) = sym.strip_prefix('&')
        {
            let qualified_name = format!("{pkg}::{stripped_sym}");
            if self.interpreter.has_function(&qualified_name)
                || self.interpreter.has_multi_function(&qualified_name)
            {
                Value::Routine {
                    package: pkg.to_string(),
                    name: qualified_name,
                    is_regex: false,
                }
            } else {
                Value::Str(name.to_string())
            }
        } else if let Some(v) = self.interpreter.env().get(name) {
            if matches!(v, Value::Enum { .. } | Value::Nil) {
                v.clone()
            } else if self.interpreter.has_class(name)
                || self.interpreter.is_role(name)
                || Self::is_builtin_type(name)
            {
                Value::Package(name.to_string())
            } else if !name.starts_with('$') && !name.starts_with('@') && !name.starts_with('%') {
                v.clone()
            } else {
                Value::Str(name.to_string())
            }
        } else if self.interpreter.has_class(name)
            || self.interpreter.is_role(name)
            || Self::is_builtin_type(name)
            || Self::is_type_with_smiley(name, &self.interpreter)
        {
            Value::Package(name.to_string())
        } else if self.interpreter.has_function(name)
            || Interpreter::is_implicit_zero_arg_builtin(name)
        {
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
        } else if name == "callsame" || name == "nextsame" || name == "nextcallee" {
            let result = self.interpreter.call_function(name, Vec::new())?;
            self.sync_locals_from_env(code);
            result
        } else if name == "NaN" {
            Value::Num(f64::NAN)
        } else if name == "Inf" {
            Value::Num(f64::INFINITY)
        } else if name == "Empty" {
            Value::Slip(std::sync::Arc::new(vec![]))
        } else if name.starts_with("Metamodel::") {
            // Meta-object protocol type objects
            Value::Package(name.to_string())
        } else if name.contains("::") {
            Value::Package(name.to_string())
        } else if name.chars().count() == 1 {
            // Single unicode character — check for vulgar fractions etc.
            let ch = name.chars().next().unwrap();
            if let Some((n, d)) = crate::builtins::unicode::unicode_rat_value(ch) {
                Value::Rat(n, d)
            } else if let Some(val) = crate::builtins::unicode::unicode_numeric_int_value(ch) {
                Value::Int(val)
            } else {
                Value::Str(name.to_string())
            }
        } else {
            Value::Str(name.to_string())
        };
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_index_op(&mut self) -> Result<(), RuntimeError> {
        let index = self.stack.pop().unwrap();
        let mut target = self.stack.pop().unwrap();
        // If target is a Failure, propagate it (// will catch it as undefined)
        if matches!(&target, Value::Instance { class_name, .. } if class_name == "Failure") {
            self.stack.push(target);
            return Ok(());
        }
        if let Value::LazyList(ref ll) = target {
            target = Value::array(self.interpreter.force_lazy_list_bridge(ll)?);
        }
        let result = match (target, index) {
            (Value::Array(items, ..), Value::Int(i)) => {
                if i < 0 {
                    // Return a Failure wrapping X::OutOfRange — `//` treats it as
                    // undefined but any further use (e.g. subscripting) will throw.
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("what".to_string(), Value::Str("Index".to_string()));
                    attrs.insert("got".to_string(), Value::Int(i));
                    attrs.insert("range".to_string(), Value::Str("0..^Inf".to_string()));
                    attrs.insert(
                        "message".to_string(),
                        Value::Str(format!(
                            "Index out of range. Is: {}, should be in 0..^Inf",
                            i
                        )),
                    );
                    let ex = Value::make_instance("X::OutOfRange".to_string(), attrs);
                    let mut failure_attrs = std::collections::HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Value::make_instance("Failure".to_string(), failure_attrs)
                } else {
                    items.get(i as usize).cloned().unwrap_or(Value::Nil)
                }
            }
            (target @ Value::Array(..), Value::Array(indices, ..)) => {
                let depth = Self::array_depth(&target);
                if depth <= 1 && indices.len() > 1 {
                    // Positional slice: @a[0,1,2] returns (@a[0], @a[1], @a[2])
                    let Value::Array(items, ..) = &target else {
                        unreachable!()
                    };
                    let mut out = Vec::with_capacity(indices.len());
                    for idx in indices.iter() {
                        if let Some(i) = Self::index_to_usize(idx) {
                            out.push(items.get(i).cloned().unwrap_or(Value::Nil));
                        } else {
                            out.push(Value::Nil);
                        }
                    }
                    Value::array(out)
                } else {
                    let strict_oob = indices.len() > 1;
                    Self::index_array_multidim(&target, indices.as_ref(), strict_oob)?
                }
            }
            (Value::Array(items, ..), Value::Range(a, b)) => {
                let start = a.max(0) as usize;
                let end = b.max(-1) as usize;
                let slice = if start >= items.len() {
                    Vec::new()
                } else {
                    let end = end.min(items.len().saturating_sub(1));
                    items[start..=end].to_vec()
                };
                Value::array(slice)
            }
            (Value::Array(items, ..), Value::RangeExcl(a, b)) => {
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
                Value::array(slice)
            }
            (Value::Seq(items), Value::Int(i)) => {
                if i < 0 {
                    Value::Nil
                } else {
                    items.get(i as usize).cloned().unwrap_or(Value::Nil)
                }
            }
            (Value::Seq(items), Value::Range(a, b)) => {
                let start = a.max(0) as usize;
                let end = b.max(-1) as usize;
                let slice = if start >= items.len() {
                    Vec::new()
                } else {
                    let end = end.min(items.len().saturating_sub(1));
                    items[start..=end].to_vec()
                };
                Value::array(slice)
            }
            (Value::Seq(items), Value::RangeExcl(a, b)) => {
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
                Value::array(slice)
            }
            (Value::Hash(items), Value::Whatever) => {
                Value::array(items.values().cloned().collect())
            }
            (Value::Hash(items), Value::Num(f)) if f.is_infinite() && f > 0.0 => {
                Value::array(items.values().cloned().collect())
            }
            (Value::Hash(items), Value::Nil) => Value::Hash(items),
            (Value::Hash(items), Value::Array(keys, ..)) => Value::array(
                keys.iter()
                    .map(|k| Self::resolve_hash_entry(&items, &k.to_string_value()))
                    .collect(),
            ),
            (Value::Hash(items), Value::Str(key)) => Self::resolve_hash_entry(&items, &key),
            (Value::Hash(items), Value::Int(key)) => {
                Self::resolve_hash_entry(&items, &key.to_string())
            }
            (
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
                Value::Str(key),
            ) if class_name == "Match" => {
                if let Some(Value::Hash(named)) = attributes.get("named") {
                    named.get(&key).cloned().unwrap_or(Value::Nil)
                } else {
                    Value::Nil
                }
            }
            (
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                },
                Value::Int(i),
            ) if class_name == "Match" => {
                if i < 0 {
                    Value::Nil
                } else if let Some(Value::Array(items, ..)) = attributes.get("list") {
                    items.get(i as usize).cloned().unwrap_or(Value::Nil)
                } else {
                    Value::Nil
                }
            }
            (instance @ Value::Instance { .. }, Value::Str(key)) => self
                .interpreter
                .call_method_with_values(instance, "AT-KEY", vec![Value::Str(key)])
                .unwrap_or(Value::Nil),
            (instance @ Value::Instance { .. }, Value::Int(i)) => {
                let fallback = instance.clone();
                self.interpreter
                    .call_method_with_values(instance, "AT-POS", vec![Value::Int(i)])
                    .or_else(|_| {
                        self.interpreter.call_method_with_values(
                            fallback,
                            "AT-KEY",
                            vec![Value::Int(i)],
                        )
                    })
                    .unwrap_or(Value::Nil)
            }
            (instance @ Value::Instance { .. }, Value::Array(keys, ..)) => Value::array(
                keys.iter()
                    .cloned()
                    .map(|k| {
                        self.interpreter
                            .call_method_with_values(instance.clone(), "AT-KEY", vec![k])
                            .unwrap_or(Value::Nil)
                    })
                    .collect(),
            ),
            (Value::Str(_), Value::Str(key)) => self
                .interpreter
                .env()
                .get(&format!("<{}>", key))
                .cloned()
                .unwrap_or(Value::Nil),
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
                Value::array(result)
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
                Value::array(result)
            }
            (ref range, Value::Array(indices, ..)) if range.is_range() => {
                let (start, _end, _excl_start, _excl_end) = range_params(range);
                let result: Vec<Value> = indices
                    .iter()
                    .map(|idx| match idx {
                        Value::Int(i) => Value::Int(start + i),
                        _ => Value::Nil,
                    })
                    .collect();
                Value::array(result)
            }
            // WhateverCode index: @a[*-1] → evaluate the lambda with array length
            (Value::Array(ref items, ..), Value::Sub(ref data)) => {
                let len = items.len() as i64;
                let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                let mut sub_env = data.env.clone();
                sub_env.insert(param.to_string(), Value::Int(len));
                let saved_env = std::mem::take(self.interpreter.env_mut());
                *self.interpreter.env_mut() = sub_env;
                let idx = self
                    .interpreter
                    .eval_block_value(&data.body)
                    .unwrap_or(Value::Nil);
                *self.interpreter.env_mut() = saved_env;
                match idx {
                    Value::Int(i) => {
                        if i < 0 {
                            Value::Nil
                        } else {
                            items.get(i as usize).cloned().unwrap_or(Value::Nil)
                        }
                    }
                    _ => Value::Nil,
                }
            }
            // GenericRange with WhateverCode endpoint: @a[0..*-2]
            (
                Value::Array(ref items, ..),
                Value::GenericRange {
                    ref start,
                    ref end,
                    excl_start,
                    excl_end,
                },
            ) => {
                let len = items.len() as i64;
                let mut resolve_endpoint = |val: &Value| -> i64 {
                    match val {
                        Value::Int(i) => *i,
                        Value::Sub(data) => {
                            let param = data.params.first().map(|s| s.as_str()).unwrap_or("_");
                            let mut sub_env = data.env.clone();
                            sub_env.insert(param.to_string(), Value::Int(len));
                            let saved_env = std::mem::take(self.interpreter.env_mut());
                            *self.interpreter.env_mut() = sub_env;
                            let result = self
                                .interpreter
                                .eval_block_value(&data.body)
                                .unwrap_or(Value::Nil);
                            *self.interpreter.env_mut() = saved_env;
                            match result {
                                Value::Int(i) => i,
                                _ => 0,
                            }
                        }
                        _ => match val {
                            Value::Num(f) => *f as i64,
                            _ => 0,
                        },
                    }
                };
                let s = resolve_endpoint(start);
                let e = resolve_endpoint(end);
                let actual_start = if excl_start { s + 1 } else { s }.max(0) as usize;
                let actual_end = if excl_end { e } else { e + 1 }.max(0) as usize;
                let actual_end = actual_end.min(items.len());
                let slice = if actual_start >= actual_end {
                    Vec::new()
                } else {
                    items[actual_start..actual_end].to_vec()
                };
                Value::array(slice)
            }
            // Capture indexing: $capture<key> (named) or $capture[idx] (positional)
            (
                Value::Capture {
                    positional: _,
                    named,
                },
                Value::Str(key),
            ) => named.get(&key).cloned().unwrap_or(Value::Nil),
            (Value::Capture { positional, .. }, Value::Int(i)) => {
                if i < 0 {
                    Value::Nil
                } else {
                    positional.get(i as usize).cloned().unwrap_or(Value::Nil)
                }
            }
            // Role parameterization: e.g. R1[C1] → ParametricRole
            (Value::Package(name), idx) if self.interpreter.is_role(&name) => {
                let type_args = match idx {
                    Value::Array(items, ..) => items.as_ref().clone(),
                    other => vec![other],
                };
                Value::ParametricRole {
                    base_name: name,
                    type_args,
                }
            }
            // Type parameterization: e.g. Buf[uint8] → returns the type unchanged
            (pkg @ Value::Package(_), _) => pkg,
            // Pair subscript: $pair<key> returns value if key matches, Nil otherwise
            (Value::Pair(key, value), Value::Str(idx)) => {
                if key == idx {
                    *value
                } else {
                    Value::Nil
                }
            }
            (Value::ValuePair(key, value), Value::Str(idx)) => {
                if key.to_string_value() == idx {
                    *value
                } else {
                    Value::Nil
                }
            }
            _ => Value::Nil,
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_delete_index_named_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        // Check if we're deleting HOME from %*ENV — need to sync $*HOME to Nil
        if var_name == "%*ENV" {
            let deletes_home = match &idx {
                Value::Array(keys, ..) => keys.iter().any(|k| k.to_string_value() == "HOME"),
                _ => idx.to_string_value() == "HOME",
            };
            if deletes_home {
                self.interpreter
                    .env_mut()
                    .insert("$*HOME".to_string(), Value::Nil);
                self.interpreter
                    .env_mut()
                    .insert("*HOME".to_string(), Value::Nil);
            }
        }
        let result = if let Some(container) = self.interpreter.env_mut().get_mut(&var_name) {
            Self::delete_from_container(container, idx)?
        } else {
            Self::delete_from_missing_container(idx)
        };
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_delete_index_expr_op(&mut self) -> Result<(), RuntimeError> {
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let mut target = self.stack.pop().unwrap_or(Value::Nil);
        let result = Self::delete_from_container(&mut target, idx)?;
        self.stack.push(result);
        Ok(())
    }

    /// Rich :exists adverb handler supporting negation, parameterized arg,
    /// zen slice, and secondary adverbs (:kv, :!kv, :p, :!p, :!v).
    pub(super) fn exec_exists_index_adv_op(
        &mut self,
        flags: u32,
    ) -> Result<(), crate::value::RuntimeError> {
        let negated_flag = flags & 1 != 0;
        let has_arg = flags & 2 != 0;
        let is_zen = flags & 4 != 0;
        let adverb_bits = (flags >> 4) & 0xF;

        // Pop arg if present (it's on top of stack)
        let arg_val = if has_arg {
            self.stack.pop().unwrap_or(Value::Nil)
        } else {
            Value::Nil
        };

        // Determine effective negation
        let effective_negated = if has_arg {
            !arg_val.truthy() // falsy arg => negate
        } else {
            negated_flag
        };

        // Check for invalid adverb combos — die at runtime
        match adverb_bits {
            6 | 7 => {
                // InvalidK, InvalidNotK
                return Err(crate::value::RuntimeError::new(
                    "Unsupported combination of :exists and :k adverbs".to_string(),
                ));
            }
            8 => {
                // InvalidV
                return Err(crate::value::RuntimeError::new(
                    "Unsupported combination of :exists and :v adverbs".to_string(),
                ));
            }
            _ => {}
        }

        let (target, indices) = if is_zen {
            let target = self.stack.pop().unwrap_or(Value::Nil);
            Self::throw_if_failure(&target)?;
            let len = match &target {
                Value::Array(items, ..) => items.len(),
                _ => 0,
            };
            let idxs: Vec<i64> = (0..len as i64).collect();
            (target, idxs)
        } else {
            let idx = self.stack.pop().unwrap_or(Value::Nil);
            let target = self.stack.pop().unwrap_or(Value::Nil);
            Self::throw_if_failure(&target)?;
            let idxs = match &idx {
                Value::Int(i) => vec![*i],
                Value::Array(items, ..) if crate::runtime::utils::is_shaped_array(&target) => {
                    // Shaped array: multi-dimensional exists (e.g. @arr[0;0]:exists)
                    let exists = Self::index_array_multidim(&target, items.as_ref(), false)
                        .ok()
                        .is_some_and(|v| !matches!(v, Value::Nil));
                    let result = Value::Bool(exists ^ effective_negated);
                    self.stack.push(result);
                    return Ok(());
                }
                Value::Array(items, ..) => items
                    .iter()
                    .map(|v| match v {
                        Value::Int(i) => *i,
                        _ => {
                            let n = v.to_bigint();
                            n.try_into().unwrap_or(0)
                        }
                    })
                    .collect(),
                // Whatever (*) — treat as zen slice (all elements)
                Value::Whatever => {
                    let len = match &target {
                        Value::Array(items, ..) => items.len(),
                        _ => 0,
                    };
                    (0..len as i64).collect()
                }
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                    let len = match &target {
                        Value::Array(items, ..) => items.len(),
                        _ => 0,
                    };
                    (0..len as i64).collect()
                }
                _ => {
                    // For hash access, delegate to single key exists
                    let exists = match (&target, &idx) {
                        (Value::Hash(map), Value::Str(key)) => map.contains_key(key),
                        (Value::Hash(map), _) => map.contains_key(&idx.to_string_value()),
                        _ => false,
                    };
                    let result = Value::Bool(exists ^ effective_negated);
                    self.stack.push(result);
                    return Ok(());
                }
            };
            (target, idxs)
        };

        let items = match &target {
            Value::Array(items, ..) => items.as_ref(),
            _ => &[] as &[Value],
        };

        let is_multi = indices.len() != 1 || is_zen;

        if !is_multi {
            // Single index
            let i = indices[0];
            let exists = i >= 0
                && items
                    .get(i as usize)
                    .is_some_and(|v| !matches!(v, Value::Nil));
            let result = exists ^ effective_negated;
            self.stack.push(Value::Bool(result));
            return Ok(());
        }

        // Multi-index: compute (index, exists_bool) pairs
        let pairs: Vec<(i64, bool)> = indices
            .iter()
            .map(|&i| {
                let exists = i >= 0
                    && items
                        .get(i as usize)
                        .is_some_and(|v| !matches!(v, Value::Nil));
                (i, exists)
            })
            .collect();

        let result = match adverb_bits {
            0 => {
                // Plain :exists — list of Bools
                let vals: Vec<Value> = pairs
                    .iter()
                    .map(|(_, e)| Value::Bool(*e ^ effective_negated))
                    .collect();
                Value::array(vals)
            }
            1 => {
                // :kv — filter by original exists, interleave (index, bool)
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    if *exists {
                        vals.push(Value::Int(*i));
                        vals.push(Value::Bool(*exists ^ effective_negated));
                    }
                }
                Value::array(vals)
            }
            2 => {
                // :!kv — no filter, interleave all (index, bool)
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    vals.push(Value::Int(*i));
                    vals.push(Value::Bool(*exists ^ effective_negated));
                }
                Value::array(vals)
            }
            3 => {
                // :p — filter by original exists, return Pairs
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    if *exists {
                        vals.push(Value::ValuePair(
                            Box::new(Value::Int(*i)),
                            Box::new(Value::Bool(*exists ^ effective_negated)),
                        ));
                    }
                }
                Value::array(vals)
            }
            4 => {
                // :!p — no filter, return all Pairs
                let mut vals = Vec::new();
                for (i, exists) in &pairs {
                    vals.push(Value::ValuePair(
                        Box::new(Value::Int(*i)),
                        Box::new(Value::Bool(*exists ^ effective_negated)),
                    ));
                }
                Value::array(vals)
            }
            5 => {
                // :!v — no filter, return all Bools
                let vals: Vec<Value> = pairs
                    .iter()
                    .map(|(_, e)| Value::Bool(*e ^ effective_negated))
                    .collect();
                Value::array(vals)
            }
            _ => Value::Nil,
        };

        self.stack.push(result);
        Ok(())
    }

    fn delete_from_missing_container(idx: Value) -> Value {
        match idx {
            Value::Array(keys, ..) => Value::array(vec![Value::Nil; keys.len()]),
            _ => Value::Nil,
        }
    }

    fn delete_from_container(container: &mut Value, idx: Value) -> Result<Value, RuntimeError> {
        Ok(match container {
            Value::Hash(hash) => match idx {
                Value::Array(keys, ..) => {
                    let h = Arc::make_mut(hash);
                    let removed = keys
                        .iter()
                        .map(|key| h.remove(&key.to_string_value()).unwrap_or(Value::Nil))
                        .collect();
                    Value::array(removed)
                }
                _ => Arc::make_mut(hash)
                    .remove(&idx.to_string_value())
                    .unwrap_or(Value::Nil),
            },
            Value::Package(type_name) if type_name == "Hash" || type_name == "Hash:U" => {
                match idx {
                    Value::Array(keys, ..) => Value::array(vec![Value::Nil; keys.len()]),
                    _ => Value::Nil,
                }
            }
            Value::Array(..) => match idx {
                Value::Array(indices, ..) => {
                    Self::delete_array_multidim(container, indices.as_ref())?
                }
                _ => Self::delete_array_multidim(container, std::slice::from_ref(&idx))?,
            },
            _ => match idx {
                Value::Array(keys, ..) => Value::array(vec![Value::Nil; keys.len()]),
                _ => Value::Nil,
            },
        })
    }

    pub(super) fn exec_string_concat_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        let mut result = String::new();
        for v in values {
            result.push_str(&crate::runtime::utils::coerce_to_str(&v));
        }
        self.stack.push(Value::Str(result));
    }

    pub(super) fn exec_post_increment_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        // Handle $CALLER::varname++ — increment through caller scope
        if let Some((bare_name, depth)) = crate::compiler::Compiler::parse_caller_prefix(name) {
            let val = self.interpreter.get_caller_var(&bare_name, depth)?;
            let new_val = match &val {
                Value::Int(i) => Value::Int(i + 1),
                Value::Bool(_) => Value::Bool(true),
                Value::Rat(n, d) => make_rat(n + d, *d),
                _ => Value::Int(1),
            };
            self.interpreter
                .set_caller_var(&bare_name, depth, new_val)?;
            self.stack.push(val);
            return Ok(());
        }
        self.interpreter.check_readonly_for_increment(name)?;
        let val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        let new_val = match &val {
            Value::Int(i) => Value::Int(i + 1),
            Value::Bool(_) => Value::Bool(true),
            Value::Rat(n, d) => make_rat(n + d, *d),
            _ => Value::Int(1),
        };
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        // Write back to source variable when incrementing $_ bound to a container
        if name == "_"
            && let Some(ref source_var) = self.topic_source_var
        {
            let sv = source_var.clone();
            self.set_env_with_main_alias(&sv, new_val.clone());
            self.update_local_if_exists(code, &sv, &new_val);
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_post_decrement_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        self.interpreter.check_readonly_for_increment(name)?;
        let val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        let new_val = match &val {
            Value::Int(i) => Value::Int(i - 1),
            Value::Bool(_) => Value::Bool(false),
            Value::Rat(n, d) => make_rat(n - d, *d),
            _ => Value::Int(-1),
        };
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(val);
        Ok(())
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
                Value::Array(arr, ..) => {
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
            match *container {
                Value::Hash(ref mut h) => {
                    Arc::make_mut(h).insert(key, new_val);
                }
                Value::Array(ref mut arr, ..) => {
                    if let Ok(i) = idx_val.to_string_value().parse::<usize>() {
                        let a = Arc::make_mut(arr);
                        while a.len() <= i {
                            a.push(Value::Nil);
                        }
                        a[i] = new_val;
                    }
                }
                _ => {}
            }
        }
        self.stack.push(effective);
    }

    pub(super) fn exec_index_assign_expr_named_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let (val, bind_mode) = match raw_val {
            Value::Pair(name, value) if name == "__mutsu_bind_index_value" => (*value, true),
            other => (other, false),
        };
        // When assigning Nil to a typed container element, use the type object
        let val = if matches!(val, Value::Nil) {
            if let Some(constraint) = self.interpreter.var_type_constraint(&var_name) {
                Value::Package(constraint)
            } else {
                val
            }
        } else {
            val
        };
        let encoded_idx = Self::encode_bound_index(&idx);
        let is_bound_index = if bind_mode {
            self.is_bound_index(&var_name, &encoded_idx)
        } else {
            false
        };
        if !bind_mode && self.is_bound_index(&var_name, &encoded_idx) {
            return Err(RuntimeError::new("X::Assignment::RO"));
        }
        if !self.interpreter.env().contains_key(&var_name)
            && let Some(slot) = self.find_local_slot(code, &var_name)
        {
            self.set_env_with_main_alias(&var_name, self.locals[slot].clone());
        }
        match &idx {
            Value::Array(keys, ..) => {
                if let Some(container) = self.interpreter.env_mut().get_mut(&var_name)
                    && matches!(container, Value::Array(..))
                {
                    let is_shaped = crate::runtime::utils::is_shaped_array(container);
                    if is_shaped {
                        if bind_mode && is_bound_index {
                            return Err(RuntimeError::new("X::Assignment::RO"));
                        }
                        // Multidimensional indexing: @arr[0;0] = 'x'
                        Self::assign_array_multidim(container, keys.as_ref(), val.clone())?;
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
                            if max_idx >= arr.len() {
                                arr.resize(max_idx + 1, Value::Package("Any".to_string()));
                            }
                        }
                        // Assign each value to the corresponding index
                        let vals = match &val {
                            Value::Array(v, ..) => (**v).clone(),
                            _ => vec![val.clone()],
                        };
                        for (i, key) in keys.iter().enumerate() {
                            let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                            Self::assign_array_multidim(container, std::slice::from_ref(key), v)?;
                        }
                    }
                    if bind_mode {
                        self.mark_bound_index(&var_name, encoded_idx);
                    }
                    self.stack.push(val);
                    return Ok(());
                }
                let mut vals = match &val {
                    Value::Array(v, ..) => (**v).clone(),
                    _ => vec![val.clone()],
                };
                if vals.is_empty() {
                    vals.push(Value::Nil);
                }
                if !matches!(self.interpreter.env().get(&var_name), Some(Value::Hash(_))) {
                    self.interpreter.env_mut().insert(
                        var_name.clone(),
                        Value::hash(std::collections::HashMap::new()),
                    );
                }
                if let Some(Value::Hash(hash)) = self.interpreter.env_mut().get_mut(&var_name) {
                    let h = Arc::make_mut(hash);
                    for (i, key) in keys.iter().enumerate() {
                        let k = key.to_string_value();
                        let v = vals[i % vals.len()].clone();
                        h.insert(k, v);
                    }
                }
            }
            _ => {
                // Check if the target is an array variable — use numeric index assignment
                let key = idx.to_string_value();
                let array_elem_constraint = self.interpreter.var_type_constraint(&var_name);
                if let Some(constraint) = array_elem_constraint
                    && !matches!(val, Value::Nil)
                    && !self.interpreter.type_matches_value(&constraint, &val)
                {
                    return Err(RuntimeError::new(format!(
                        "X::TypeCheck::Assignment: Type check failed in assignment to '{}'; expected {}, got {}",
                        var_name,
                        constraint,
                        runtime::utils::value_type_name(&val)
                    )));
                }
                if let Some(container) = self.interpreter.env_mut().get_mut(&var_name) {
                    match *container {
                        Value::Hash(ref mut hash) => {
                            if let Value::Hash(source_hash) = &val
                                && Arc::ptr_eq(hash, source_hash)
                            {
                                Arc::make_mut(hash)
                                    .insert(key.clone(), Self::self_hash_ref_marker());
                            } else {
                                Arc::make_mut(hash).insert(key.clone(), val.clone());
                            }
                        }
                        Value::Array(..) => {
                            if crate::runtime::utils::is_shaped_array(container) {
                                if bind_mode && is_bound_index {
                                    return Err(RuntimeError::new("X::Assignment::RO"));
                                }
                                Self::assign_array_multidim(
                                    container,
                                    std::slice::from_ref(&idx),
                                    val.clone(),
                                )?;
                            } else if let Some(i) = Self::index_to_usize(&idx) {
                                if let Value::Array(items, ..) = container {
                                    let arr = Arc::make_mut(items);
                                    if i >= arr.len() {
                                        arr.resize(i + 1, Value::Package("Any".to_string()));
                                    }
                                    arr[i] = val.clone();
                                }
                            } else {
                                return Err(RuntimeError::new("Index out of bounds"));
                            }
                            if bind_mode {
                                self.mark_bound_index(&var_name, encoded_idx.clone());
                            }
                        }
                        _ => {
                            let mut hash = std::collections::HashMap::new();
                            hash.insert(key.clone(), val.clone());
                            *container = Value::hash(hash);
                        }
                    }
                } else {
                    let mut hash = std::collections::HashMap::new();
                    hash.insert(key.clone(), val.clone());
                    self.interpreter
                        .env_mut()
                        .insert(var_name.clone(), Value::hash(hash));
                }
                // Sync $*HOME when %*ENV<HOME> changes
                if var_name == "%*ENV" && key == "HOME" {
                    let home_str = val.to_string_value();
                    let home_val = self.interpreter.make_io_path_instance(&home_str);
                    self.interpreter
                        .env_mut()
                        .insert("$*HOME".to_string(), home_val.clone());
                    self.interpreter
                        .env_mut()
                        .insert("*HOME".to_string(), home_val);
                }
            }
        }
        if let Some(updated) = self.get_env_with_main_alias(&var_name) {
            self.update_local_if_exists(code, &var_name, &updated);
        }
        self.stack.push(val);
        Ok(())
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
                Value::hash(std::collections::HashMap::new()),
            );
        }
        if let Some(Value::Hash(outer_hash)) = self.interpreter.env_mut().get_mut(&var_name) {
            let oh = Arc::make_mut(outer_hash);
            let inner_hash = oh
                .entry(inner_key)
                .or_insert_with(|| Value::hash(std::collections::HashMap::new()));
            if let Value::Hash(ref mut h) = *inner_hash {
                Arc::make_mut(h).insert(outer_key, val.clone());
            }
        }
        if let Some(updated) = self.get_env_with_main_alias(&var_name) {
            self.update_local_if_exists(code, &var_name, &updated);
        }
        self.stack.push(val);
    }

    pub(super) fn exec_get_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;
        // Check if this variable has a binding alias (e.g. from $CALLER::foo := $other_var)
        let name = code.locals.get(idx).cloned().unwrap_or_default();
        if let Some(bound_to) = self.interpreter.resolve_binding(&name) {
            let bound_to = bound_to.to_string();
            if let Some(val) = self.interpreter.env().get(&bound_to).cloned() {
                self.stack.push(val);
                return Ok(());
            }
        }
        let val = self.locals[idx].clone();
        // Fast path: non-Nil values are always valid — skip env lookup
        if matches!(val, Value::Nil) {
            let is_internal = name.starts_with("__");
            let is_special = matches!(name.as_str(), "_" | "/" | "!" | "¢");
            if !is_internal && !is_special && !self.interpreter.env().contains_key(&name) {
                return Err(RuntimeError::new(format!(
                    "X::Undeclared::Symbols: Variable '{name}' is not declared"
                )));
            }
        }
        self.stack.push(val);
        Ok(())
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
                Value::array(self.interpreter.force_lazy_list_bridge(list)?)
            } else {
                val
            }
        } else {
            val
        };
        let mut val = if name.starts_with('%') {
            runtime::coerce_to_hash(val)
        } else if name.starts_with('@') {
            runtime::coerce_to_array(val)
        } else {
            val
        };
        if let Some(constraint) = self.interpreter.var_type_constraint(name)
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if matches!(val, Value::Nil) && self.interpreter.is_definite_constraint(&constraint) {
                return Err(RuntimeError::new(
                    "X::Syntax::Variable::MissingInitializer: Definite typed variable requires initializer",
                ));
            }
            if !matches!(val, Value::Nil) && !self.interpreter.type_matches_value(&constraint, &val)
            {
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Assignment: Type check failed in assignment to '{}'; expected {}, got {}",
                    name,
                    constraint,
                    runtime::utils::value_type_name(&val)
                )));
            }
            if !matches!(val, Value::Nil) {
                val = self
                    .interpreter
                    .try_coerce_value_for_constraint(&constraint, val)?;
            }
        }
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(
            self.interpreter.env().get(&readonly_key),
            Some(Value::Bool(true))
        ) && !matches!(self.interpreter.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::new("X::Assignment::RO"));
        }
        self.locals[idx] = val.clone();
        self.set_env_with_main_alias(name, val.clone());
        if let Some(symbol) = Self::term_symbol_from_name(name) {
            self.interpreter
                .env_mut()
                .insert(symbol.to_string(), val.clone());
            let pkg = self.interpreter.current_package().to_string();
            if pkg != "GLOBAL" {
                self.interpreter
                    .env_mut()
                    .insert(format!("{pkg}::term:<{symbol}>"), val.clone());
            }
        }
        if let Some(alias_name) = self.interpreter.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.clone())
            } else {
                None
            }
        }) {
            self.update_local_if_exists(code, &alias_name, &val);
            self.interpreter.env_mut().insert(alias_name, val.clone());
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.interpreter
                .env_mut()
                .insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.interpreter
                .env_mut()
                .insert(format!(".{}", attr), val.clone());
        }
        Ok(())
    }

    pub(super) fn exec_set_var_dynamic_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        dynamic: bool,
    ) {
        let name = Self::const_str(code, name_idx);
        self.interpreter.set_var_dynamic(name, dynamic);
    }

    pub(super) fn exec_assign_expr_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let idx = idx as usize;
        let name = &code.locals[idx];
        self.interpreter.check_readonly_for_modify(name)?;
        let mut val = if name.starts_with('%') {
            runtime::coerce_to_hash(raw_val)
        } else if name.starts_with('@') {
            runtime::coerce_to_array(raw_val)
        } else {
            raw_val
        };
        if let Some(constraint) = self.interpreter.var_type_constraint(name)
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if matches!(val, Value::Nil) {
                // Assigning Nil to a typed variable resets it to the type object
                val = Value::Package(constraint.clone());
            } else if !self.interpreter.type_matches_value(&constraint, &val) {
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Assignment: Type check failed in assignment to '{}'; expected {}, got {}",
                    name,
                    constraint,
                    runtime::utils::value_type_name(&val)
                )));
            }
            if !matches!(val, Value::Nil | Value::Package(_)) {
                val = self
                    .interpreter
                    .try_coerce_value_for_constraint(&constraint, val)?;
            }
        }
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(
            self.interpreter.env().get(&readonly_key),
            Some(Value::Bool(true))
        ) && !matches!(self.interpreter.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::new("X::Assignment::RO"));
        }
        self.locals[idx] = val.clone();
        self.set_env_with_main_alias(name, val.clone());
        if let Some(alias_name) = self.interpreter.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.clone())
            } else {
                None
            }
        }) {
            self.update_local_if_exists(code, &alias_name, &val);
            self.interpreter.env_mut().insert(alias_name, val.clone());
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.interpreter
                .env_mut()
                .insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.interpreter
                .env_mut()
                .insert(format!(".{}", attr), val.clone());
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_get_pseudo_stash_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        if let Some(package) = name.strip_suffix("::")
            && package != "MY"
        {
            self.stack
                .push(self.interpreter.package_stash_value(package));
            return;
        }

        // MY:: pseudo-stash: collect all variable names from current scope.
        let mut entries: HashMap<String, Value> = HashMap::new();
        for (i, var_name) in code.locals.iter().enumerate() {
            let val = self.locals[i].clone();
            let key = Self::add_sigil_prefix(var_name);
            entries.insert(key, val);
        }
        for (key, val) in self.interpreter.env() {
            let display_key = Self::add_sigil_prefix(key);
            entries.entry(display_key).or_insert_with(|| val.clone());
        }
        self.stack.push(Value::Hash(Arc::new(entries)));
    }

    /// Add a sigil prefix to a variable name for display in pseudo-stash.
    /// Names starting with @, %, & already have sigils. Others get $ prefix.
    fn add_sigil_prefix(name: &str) -> String {
        if name.starts_with('$')
            || name.starts_with('@')
            || name.starts_with('%')
            || name.starts_with('&')
        {
            name.to_string()
        } else if name.starts_with('*') || name.starts_with('?') || name.starts_with('!') {
            // Twigil variables like *CWD → $*CWD
            format!("${}", name)
        } else if name.chars().next().is_some_and(|c| c.is_uppercase()) {
            // Type names, package names — no sigil
            name.to_string()
        } else {
            format!("${}", name)
        }
    }

    /// Execute HyperSlice opcode: recursively iterate a hash.
    pub(super) fn exec_hyper_slice_op(&mut self, adverb: u8) -> Result<(), RuntimeError> {
        use crate::ast::HyperSliceAdverb;

        let target = self.stack.pop().unwrap();
        let adverb = match adverb {
            0 => HyperSliceAdverb::Kv,
            1 => HyperSliceAdverb::K,
            2 => HyperSliceAdverb::V,
            3 => HyperSliceAdverb::Tree,
            4 => HyperSliceAdverb::DeepK,
            5 => HyperSliceAdverb::DeepKv,
            _ => HyperSliceAdverb::Kv,
        };

        let hash = match target {
            Value::Hash(h) => h,
            _ => {
                return Err(RuntimeError::new(
                    "Cannot use {**} hyperslice on a non-Hash value".to_string(),
                ));
            }
        };

        let mut result = Vec::new();
        let path: Vec<String> = Vec::new();
        Self::hyperslice_recurse(&hash, &path, adverb, &mut result);
        self.stack.push(Value::array(result));
        Ok(())
    }

    fn hyperslice_recurse(
        hash: &std::collections::HashMap<String, Value>,
        path: &[String],
        adverb: crate::ast::HyperSliceAdverb,
        result: &mut Vec<Value>,
    ) {
        use crate::ast::HyperSliceAdverb;

        for (key, value) in hash.iter() {
            let mut cur_path: Vec<String> = path.to_vec();
            cur_path.push(key.clone());

            match adverb {
                HyperSliceAdverb::Kv => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        result.push(Value::Str(key.clone()));
                        result.push(value.clone());
                    }
                }
                HyperSliceAdverb::K => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        result.push(Value::Str(key.clone()));
                    }
                }
                HyperSliceAdverb::V => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        result.push(value.clone());
                    }
                }
                HyperSliceAdverb::Tree => {
                    // Tree mode: yield key-value pairs for all entries,
                    // including sub-hashes (as their original Value::Hash)
                    result.push(Value::Str(key.clone()));
                    result.push(value.clone());
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    }
                }
                HyperSliceAdverb::DeepK => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        let key_array: Vec<Value> =
                            cur_path.iter().map(|s| Value::Str(s.clone())).collect();
                        result.push(Value::array(key_array));
                    }
                }
                HyperSliceAdverb::DeepKv => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        let key_array: Vec<Value> =
                            cur_path.iter().map(|s| Value::Str(s.clone())).collect();
                        result.push(Value::array(key_array));
                        result.push(value.clone());
                    }
                }
            }
        }
    }

    /// Execute HyperIndex opcode: drill into nested hash by key path.
    pub(super) fn exec_hyper_index_op(&mut self) -> Result<(), RuntimeError> {
        let keys = self.stack.pop().unwrap();
        let target = self.stack.pop().unwrap();

        let key_list = match keys {
            Value::Array(items, ..) => items,
            Value::Seq(items) => Arc::new(items.to_vec()),
            _ => Arc::new(vec![keys]),
        };

        let mut current = target;
        for key in key_list.iter() {
            match current {
                Value::Hash(ref h) => {
                    let k = key.to_string_value();
                    current = h.get(&k).cloned().unwrap_or(Value::Nil);
                }
                _ => {
                    current = Value::Nil;
                    break;
                }
            }
        }

        self.stack.push(current);
        Ok(())
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
