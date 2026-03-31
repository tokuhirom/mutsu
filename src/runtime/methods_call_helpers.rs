use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn promise_combinator_error(combinator: &str) -> RuntimeError {
        let message = format!(
            "Can only use {} to combine defined Promise objects",
            combinator
        );
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(Symbol::intern("X::Promise::Combinator"), attrs);
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(ex));
        err
    }

    pub(super) fn collect_promise_combinator_inputs(
        &self,
        combinator: &str,
        args: &[Value],
    ) -> Result<Vec<SharedPromise>, RuntimeError> {
        let mut promises = Vec::new();
        for arg in args {
            match arg {
                Value::Promise(promise) => promises.push(promise.clone()),
                _ if arg.as_list_items().is_some() => {
                    for item in arg.as_list_items().unwrap().iter() {
                        if let Value::Promise(promise) = item {
                            promises.push(promise.clone());
                        } else {
                            return Err(Self::promise_combinator_error(combinator));
                        }
                    }
                }
                _ => return Err(Self::promise_combinator_error(combinator)),
            }
        }
        Ok(promises)
    }

    pub(super) fn supply_list_values(
        &mut self,
        attributes: &HashMap<String, Value>,
        wait_until_done: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut items = match attributes.get("values") {
            Some(Value::Array(values, ..)) => values.to_vec(),
            _ => Vec::new(),
        };

        if let Some(Value::Int(supplier_id)) = attributes.get("supplier_id")
            && *supplier_id > 0
        {
            let supplier_id = *supplier_id as u64;
            let live = matches!(attributes.get("live"), Some(Value::Bool(true)));
            let deadline = if wait_until_done && live {
                Some(std::time::Instant::now() + std::time::Duration::from_secs(5))
            } else {
                None
            };
            let mut seen_emitted = 0usize;
            loop {
                let (emitted, done, quit_reason) =
                    crate::runtime::native_methods::supplier_snapshot(supplier_id);
                if emitted.len() > seen_emitted {
                    items.extend_from_slice(&emitted[seen_emitted..]);
                    seen_emitted = emitted.len();
                }
                if let Some(reason) = quit_reason {
                    let message = reason.to_string_value();
                    let mut err = RuntimeError::new(message);
                    err.exception = Some(Box::new(reason));
                    return Err(err);
                }
                if done || deadline.is_none() {
                    break;
                }
                if let Some(limit) = deadline
                    && std::time::Instant::now() >= limit
                {
                    break;
                }
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
        }

        Ok(items)
    }

    pub(super) fn should_autothread_method(method: &str) -> bool {
        !matches!(
            method,
            "Bool"
                | "so"
                | "WHAT"
                | "^name"
                | "gist"
                | "Str"
                | "defined"
                | "THREAD"
                | "raku"
                | "perl"
                | "first"
                | "grep"
        )
    }

    pub(super) fn iterator_supports_predictive_methods(
        attributes: &HashMap<String, Value>,
    ) -> bool {
        matches!(attributes.get("items"), Some(Value::Array(..)))
            || matches!(attributes.get("squish_source"), Some(Value::Array(..)))
    }

    pub(super) fn iterator_count_only_from_attrs(
        &mut self,
        attributes: &HashMap<String, Value>,
    ) -> Result<Option<Value>, RuntimeError> {
        if let Some(Value::Array(items, ..)) = attributes.get("items") {
            let index = match attributes.get("index") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => 0,
            };
            return Ok(Some(Value::Int(items.len().saturating_sub(index) as i64)));
        }

        let Some(Value::Array(source, ..)) = attributes.get("squish_source") else {
            return Ok(None);
        };

        let mut scan_index = match attributes.get("squish_scan_index") {
            Some(Value::Int(i)) if *i >= 0 => *i as usize,
            _ => 0,
        };
        let mut prev_key = attributes
            .get("squish_prev_key")
            .cloned()
            .unwrap_or(Value::Nil);
        let initialized = matches!(
            attributes.get("squish_initialized"),
            Some(Value::Bool(true))
        );
        let as_func = attributes
            .get("squish_as")
            .cloned()
            .filter(|v| !matches!(v, Value::Nil));
        let with_func = attributes
            .get("squish_with")
            .cloned()
            .filter(|v| !matches!(v, Value::Nil));

        let mut remaining = 0usize;

        if !initialized {
            let Some(first) = source.first().cloned() else {
                return Ok(Some(Value::Int(0)));
            };
            prev_key = if let Some(func) = as_func.clone() {
                self.call_sub_value(func, vec![first], true)?
            } else {
                first
            };
            scan_index = 1;
            remaining += 1;
        }

        while scan_index < source.len() {
            let item = source[scan_index].clone();
            let key = if let Some(func) = as_func.clone() {
                self.call_sub_value(func, vec![item.clone()], true)?
            } else {
                item
            };
            let duplicate = if let Some(func) = with_func.clone() {
                self.call_sub_value(func, vec![prev_key.clone(), key.clone()], true)?
                    .truthy()
            } else {
                crate::runtime::values_identical(&prev_key, &key)
            };
            prev_key = key;
            scan_index += 1;
            if !duplicate {
                remaining += 1;
            }
        }

        Ok(Some(Value::Int(remaining as i64)))
    }

    pub(super) fn iterator_bool_only_from_attrs(
        &mut self,
        attributes: &HashMap<String, Value>,
    ) -> Result<Option<Value>, RuntimeError> {
        let Some(count) = self.iterator_count_only_from_attrs(attributes)? else {
            return Ok(None);
        };
        Ok(Some(Value::Bool(super::to_int(&count) > 0)))
    }

    /// Produce a modified copy of an Array value for push/pop/shift/unshift/append/prepend.
    /// Used when the target is not a named variable (e.g., a hash or array element).
    pub(super) fn array_mutate_copy(
        &self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mut items = match target {
            Value::Array(ref v, ..) => v.to_vec(),
            _ => Vec::new(),
        };
        let kind = match &target {
            Value::Array(_, k) => *k,
            _ => crate::value::ArrayKind::Array,
        };
        match method {
            "push" => {
                let normalized = Self::normalize_push_args_for_copy(args);
                items.extend(normalized);
                Ok(Value::Array(Arc::new(items), kind))
            }
            "append" => {
                let flat = flatten_append_args(args);
                items.extend(flat);
                Ok(Value::Array(Arc::new(items), kind))
            }
            "pop" => {
                if !args.is_empty() {
                    return Err(RuntimeError::new(format!(
                        "Too many positionals passed; expected 1 argument but got {}",
                        args.len() + 1
                    )));
                }
                if kind.is_lazy() {
                    return Err(RuntimeError::cannot_lazy("pop"));
                }
                if items.is_empty() {
                    Ok(Value::Array(Arc::new(items), kind))
                } else {
                    items.pop();
                    Ok(Value::Array(Arc::new(items), kind))
                }
            }
            "shift" => {
                if !args.is_empty() {
                    return Err(RuntimeError::new(format!(
                        "Too many positionals passed; expected 1 argument but got {}",
                        args.len() + 1
                    )));
                }
                if items.is_empty() {
                    Ok(Value::Array(Arc::new(items), kind))
                } else {
                    items.remove(0);
                    Ok(Value::Array(Arc::new(items), kind))
                }
            }
            "unshift" | "prepend" => {
                let normalized = Self::normalize_push_args_for_copy(args);
                for (i, arg) in normalized.into_iter().enumerate() {
                    items.insert(i, arg);
                }
                Ok(Value::Array(Arc::new(items), kind))
            }
            _ => unreachable!(),
        }
    }

    /// Normalize push/unshift arguments (unwrap Scalar containers, deitemize).
    pub(super) fn normalize_push_args_for_copy(args: Vec<Value>) -> Vec<Value> {
        args.into_iter()
            .map(|arg| match arg {
                Value::Scalar(inner) => *inner,
                Value::Array(items, kind) if kind.is_itemized() => Value::Array(items, kind),
                other => other,
            })
            .collect()
    }

    pub(super) fn buf_allocate(
        &mut self,
        class_name: Symbol,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let size = match args.first() {
            Some(v) => super::to_int(v) as usize,
            None => 0,
        };
        let fill_arg = args.get(1);
        let byte_vals: Vec<Value> = if let Some(fill) = fill_arg {
            match fill {
                Value::Int(n) => vec![Value::Int(*n); size],
                Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                    let pattern: Vec<Value> = items.to_vec();
                    if pattern.is_empty() {
                        vec![Value::Int(0); size]
                    } else {
                        (0..size)
                            .map(|i| pattern[i % pattern.len()].clone())
                            .collect()
                    }
                }
                _ => vec![fill.clone(); size],
            }
        } else {
            vec![Value::Int(0); size]
        };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(byte_vals));
        Ok(Value::make_instance(class_name, attrs))
    }

    /// Check if a builtin type inherits from a given ancestor type.
    /// Covers the standard Raku type hierarchy for builtin types.
    pub(super) fn type_inherits(type_name: &str, ancestor: &str) -> bool {
        // Standard hierarchy chains for builtin types
        let chain: &[&str] = match type_name {
            "Int" => &["Int", "Cool", "Any", "Mu"],
            "Num" => &["Num", "Cool", "Any", "Mu"],
            "Rat" | "FatRat" => &["Rat", "Cool", "Any", "Mu"],
            "Str" => &["Str", "Cool", "Any", "Mu"],
            "Bool" => &["Bool", "Int", "Cool", "Any", "Mu"],
            "Array" => &["Array", "List", "Cool", "Any", "Mu"],
            "List" => &["List", "Cool", "Any", "Mu"],
            "Hash" => &["Hash", "Cool", "Any", "Mu"],
            "Range" => &["Range", "Cool", "Any", "Mu"],
            "Pair" => &["Pair", "Cool", "Any", "Mu"],
            "Set" => &["Set", "Any", "Mu"],
            "Bag" => &["Bag", "Any", "Mu"],
            "Mix" => &["Mix", "Any", "Mu"],
            "Complex" => &["Complex", "Cool", "Any", "Mu"],
            "Regex" => &["Regex", "Method", "Routine", "Block", "Code", "Any", "Mu"],
            "Sub" => &["Sub", "Routine", "Block", "Code", "Any", "Mu"],
            "Junction" => &["Junction", "Mu"],
            _ => &["Any", "Mu"],
        };
        chain.contains(&ancestor)
    }
}
