use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn assign_substr_rw(
        &mut self,
        target_var: Option<&str>,
        target: Value,
        method_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let s = target.to_string_value();
        let chars: Vec<char> = s.chars().collect();
        let str_len = chars.len();

        // Resolve start and end using the same logic as dispatch_substr
        let (start, end) = self.resolve_substr_rw_range(&method_args, str_len)?;

        let replacement = value.to_string_value();

        // Build new string: prefix + replacement + suffix
        let prefix: String = chars[..start].iter().collect();
        let suffix: String = chars[end..].iter().collect();
        let new_str = format!("{}{}{}", prefix, replacement, suffix);

        let result = Value::str(new_str);
        if let Some(var) = target_var {
            self.env.insert(var.to_string(), result.clone());
        }
        Ok(result)
    }

    /// Resolve substr-rw start and end positions from arguments.
    /// Reuses the same logic as dispatch_substr for consistency.
    pub(crate) fn resolve_substr_rw_range(
        &mut self,
        method_args: &[Value],
        str_len: usize,
    ) -> Result<(usize, usize), RuntimeError> {
        // Check if first arg is a Range
        if let Some(first_arg) = method_args.first()
            && let Some((range_start, range_end)) = self.substr_extract_range(first_arg, str_len)?
        {
            let rs: usize = range_start.min(str_len);
            let re: usize = range_end.min(str_len);
            return Ok((rs, re));
        }

        // First arg: start position
        let start_raw: i64 = if let Some(pos) = method_args.first() {
            self.substr_resolve_position(pos, str_len)?
        } else {
            0
        };

        let start = if start_raw < 0 {
            (str_len as i64 + start_raw).max(0) as usize
        } else {
            (start_raw as usize).min(str_len)
        };

        // Second arg: length
        let end = if let Some(len_val) = method_args.get(1) {
            match len_val {
                Value::Int(i) => {
                    let len = (*i).max(0) as usize;
                    (start + len).min(str_len)
                }
                Value::Num(f) if f.is_infinite() && *f > 0.0 => str_len,
                Value::Num(f) => {
                    let len = (*f as i64).max(0) as usize;
                    (start + len).min(str_len)
                }
                Value::Rat(n, d) if *d != 0 => {
                    let len = (*n / *d).max(0) as usize;
                    (start + len).min(str_len)
                }
                Value::Whatever => str_len,
                Value::Sub { .. } => {
                    // WhateverCode/Callable: call with remaining length
                    let remaining = if start <= str_len {
                        (str_len - start) as i64
                    } else {
                        0
                    };
                    let result =
                        self.eval_call_on_value(len_val.clone(), vec![Value::Int(remaining)])?;
                    let len = match &result {
                        Value::Int(i) => (*i).max(0) as usize,
                        Value::Num(f) => (*f as i64).max(0) as usize,
                        Value::Rat(n, d) if *d != 0 => (*n / *d).max(0) as usize,
                        _ => 0,
                    };
                    (start + len).min(str_len)
                }
                _ => str_len,
            }
        } else {
            str_len
        };

        Ok((start, end))
    }

    pub(crate) fn assign_subbuf_rw(
        &mut self,
        target_var: Option<&str>,
        target: Value,
        method_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let mut items = if let Value::Instance { attributes, .. } = &target
            && let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes")
        {
            items.to_vec()
        } else {
            Vec::new()
        };

        let from = method_args.first().map(crate::runtime::to_int).unwrap_or(0) as usize;
        let len = method_args.get(1).map(crate::runtime::to_int).unwrap_or(0) as usize;

        let new_bytes = if let Value::Instance { attributes, .. } = &value
            && let Some(Value::Array(new_items, ..)) = attributes.as_map().get("bytes")
        {
            new_items.to_vec()
        } else {
            Vec::new()
        };

        // splice: remove `len` items at `from`, insert `new_bytes`
        let end = (from + len).min(items.len());
        items.splice(from..end, new_bytes);

        let class_name = if let Value::Instance { class_name, .. } = &target {
            class_name.resolve().to_string()
        } else {
            "Buf".to_string()
        };
        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(items));
        let new_buf = Value::make_instance(Symbol::intern(&class_name), attrs);

        if let Some(var) = target_var {
            self.env.insert(var.to_string(), new_buf.clone());
        }
        Ok(new_buf)
    }

    pub(crate) fn buf_reallocate(
        &mut self,
        target_var: &str,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let (class_name_sym, mut bytes, orig_id, attrs_cell) = if let Value::Instance {
            class_name,
            attributes,
            id,
            ..
        } = &target
        {
            // Blob is immutable — cannot reallocate
            let cn = class_name.resolve();
            if cn == "Blob" || cn.starts_with("Blob[") || cn.starts_with("blob") {
                return Err(RuntimeError::new(format!(
                    "Cannot reallocate an immutable {}",
                    cn
                )));
            }
            let items = if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                items.to_vec()
            } else {
                Vec::new()
            };
            (*class_name, items, *id, attributes.clone())
        } else {
            return Err(RuntimeError::new("Not a Buf".to_string()));
        };
        let new_size = match args.first() {
            Some(v) => super::to_int(v) as usize,
            None => 0,
        };
        // Guard the grow path with a fallible reservation so an absurd size
        // (`$b.reallocate(1e15)`) yields a catchable `X::` instead of an
        // uncatchable abort; `truncate` then handles the shrink case.
        Self::autoviv_resize(&mut bytes, new_size, Value::Int(0))?;
        bytes.truncate(new_size);
        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(bytes));
        let updated = Value::write_back_sharing(&attrs_cell, class_name_sym, attrs, orig_id);
        self.env.insert(target_var.to_string(), updated.clone());
        Ok(updated)
    }

    /// Recursively flatten arguments for Buf mutate methods.
    /// Handles nested arrays, Seq, Slip, and Buf/Blob instances.
    pub(in crate::runtime) fn flatten_buf_args(args: Vec<Value>) -> Vec<Value> {
        let mut result = Vec::new();
        for a in args {
            match &a {
                Value::Int(_) => result.push(a),
                Value::Array(items, ..) => {
                    // Recursively flatten
                    result.extend(Self::flatten_buf_args(items.to_vec()));
                }
                Value::Seq(items) | Value::Slip(items) => {
                    // Recursively flatten
                    result.extend(Self::flatten_buf_args(items.to_vec()));
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                        result.extend(items.to_vec());
                    }
                }
                _ => result.push(a),
            }
        }
        result
    }

    pub(crate) fn is_buf_like_value(val: &Value) -> bool {
        if let Value::Instance { class_name, .. } = val {
            let cn = class_name.resolve();
            cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
                || cn.starts_with("buf")
                || cn.starts_with("blob")
        } else {
            false
        }
    }

    pub(crate) fn buf_mutate_method(
        &mut self,
        target_var: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let (class_name_sym, mut bytes, orig_id, attrs_cell) = if let Value::Instance {
            class_name,
            attributes,
            id,
            ..
        } = &target
        {
            let items = if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                items.to_vec()
            } else {
                Vec::new()
            };
            (*class_name, items, *id, attributes.clone())
        } else {
            return Err(RuntimeError::new("Not a Buf".to_string()));
        };

        // Validate and flatten args to int values
        // String args should throw X::TypeCheck
        for a in &args {
            if matches!(a, Value::Str(_)) {
                let msg = "Type check failed in assignment; expected Int but got Str".to_string();
                let mut ex_attrs = HashMap::new();
                ex_attrs.insert("message".to_string(), Value::str(msg.clone()));
                ex_attrs.insert("got".to_string(), a.clone());
                ex_attrs.insert("expected".to_string(), Value::str("Int".to_string()));
                let exception =
                    Value::make_instance(crate::symbol::Symbol::intern("X::TypeCheck"), ex_attrs);
                let mut err = RuntimeError::new(msg);
                err.exception = Some(Box::new(exception));
                return Err(err);
            }
        }
        let new_items: Vec<Value> = Self::flatten_buf_args(args);

        match method {
            "append" | "push" => {
                bytes.extend(new_items);
            }
            "prepend" | "unshift" => {
                let mut combined = new_items;
                combined.extend(bytes);
                bytes = combined;
            }
            _ => unreachable!(),
        }

        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(bytes));
        let updated = Value::write_back_sharing(&attrs_cell, class_name_sym, attrs, orig_id);
        self.env.insert(target_var.to_string(), updated.clone());
        Ok(updated)
    }

    pub(crate) fn buf_pop_shift_splice(
        &mut self,
        target_var: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let (class_name_sym, mut bytes, orig_id, attrs_cell) = if let Value::Instance {
            class_name,
            attributes,
            id,
            ..
        } = &target
        {
            let cn = class_name.resolve();
            // Blob is immutable
            if crate::runtime::utils::is_blob_like_class(&cn) {
                return Err(RuntimeError::new(format!(
                    "Cannot modify immutable {} with {}",
                    cn, method
                )));
            }
            let items = if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                items.to_vec()
            } else {
                Vec::new()
            };
            (*class_name, items, *id, attributes.clone())
        } else {
            return Err(RuntimeError::new("Not a Buf".to_string()));
        };

        match method {
            "pop" => {
                if bytes.is_empty() {
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("action".to_string(), Value::str("pop".to_string()));
                    ex_attrs.insert("what".to_string(), Value::str("Buf".to_string()));
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str("Cannot pop from an empty Buf".to_string()),
                    );
                    let exception = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Cannot::Empty"),
                        ex_attrs,
                    );
                    let mut err = RuntimeError::new("Cannot pop from an empty Buf".to_string());
                    err.exception = Some(Box::new(exception));
                    return Err(err);
                }
                let popped = bytes.pop().unwrap();
                let mut attrs = HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes));
                let updated =
                    Value::write_back_sharing(&attrs_cell, class_name_sym, attrs, orig_id);
                self.env.insert(target_var.to_string(), updated);
                Ok(popped)
            }
            "shift" => {
                if bytes.is_empty() {
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("action".to_string(), Value::str("shift".to_string()));
                    ex_attrs.insert("what".to_string(), Value::str("Buf".to_string()));
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str("Cannot shift from an empty Buf".to_string()),
                    );
                    let exception = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Cannot::Empty"),
                        ex_attrs,
                    );
                    let mut err = RuntimeError::new("Cannot shift from an empty Buf".to_string());
                    err.exception = Some(Box::new(exception));
                    return Err(err);
                }
                let shifted = bytes.remove(0);
                let mut attrs = HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes));
                let updated =
                    Value::write_back_sharing(&attrs_cell, class_name_sym, attrs, orig_id);
                self.env.insert(target_var.to_string(), updated);
                Ok(shifted)
            }
            "splice" => {
                // .splice(offset?, count?, replacement*): remove `count` elements
                // at `offset` (defaults: offset 0, count = rest), insert the
                // replacement bytes, return a Buf of the removed elements. With no
                // args this removes everything (offset 0, count = len). A negative
                // offset/count counts from the end (Raku semantics).
                let len = bytes.len() as i64;
                let resolve = |v: &Value| -> i64 {
                    match v {
                        Value::Int(i) => *i,
                        Value::Whatever => len,
                        Value::Num(n) => *n as i64,
                        Value::Str(s) => s.parse::<i64>().unwrap_or(0),
                        Value::Mixin(inner, _) => match inner.as_ref() {
                            Value::Int(i) => *i,
                            _ => 0,
                        },
                        _ => 0,
                    }
                };
                let offset = match args.first() {
                    Some(v) => {
                        let o = resolve(v);
                        if o < 0 { (len + o).max(0) } else { o.min(len) }
                    }
                    None => 0,
                };
                let count = match args.get(1) {
                    Some(v) => {
                        let c = resolve(v);
                        if c < 0 { (len - offset + c).max(0) } else { c }
                    }
                    None => len - offset,
                };
                let start = offset as usize;
                let end = ((offset + count).min(len).max(offset)) as usize;
                // Replacement bytes: flatten any remaining args (Buf/Blob bytes or
                // integer bytes).
                let mut replacement: Vec<Value> = Vec::new();
                for arg in args.iter().skip(2) {
                    match arg {
                        Value::Instance { attributes, .. }
                            if matches!(
                                attributes.as_map().get("bytes"),
                                Some(Value::Array(..))
                            ) =>
                        {
                            if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes")
                            {
                                replacement.extend(items.iter().cloned());
                            }
                        }
                        Value::Array(items, ..) => replacement.extend(items.iter().cloned()),
                        other => replacement.push(Value::Int(resolve(other) & 0xff)),
                    }
                }
                let removed: Vec<Value> = bytes.splice(start..end, replacement).collect();
                let mut attrs = HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes));
                let updated =
                    Value::write_back_sharing(&attrs_cell, class_name_sym, attrs, orig_id);
                self.env.insert(target_var.to_string(), updated);
                let mut result_attrs = HashMap::new();
                result_attrs.insert("bytes".to_string(), Value::array(removed));
                Ok(Value::make_instance(class_name_sym, result_attrs))
            }
            _ => unreachable!(),
        }
    }
}
