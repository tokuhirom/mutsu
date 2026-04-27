use super::*;
use crate::symbol::Symbol;

impl VM {
    pub(super) fn append_slip_item(args: &mut Vec<Value>, item: &Value) {
        match item {
            Value::Capture { positional, named } => {
                args.extend(positional.iter().cloned());
                for (k, v) in named.iter() {
                    args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
            }
            // Hash values inside a Slip are kept as single positional args.
            // Top-level `|%hash` flattening is handled by MakeSlip, which converts
            // a bare Hash into pairs before wrapping in a Slip. A Hash that is already
            // inside a Slip (e.g. from a Capture's positional list) should stay as-is.
            Value::Hash(_) => args.push(item.clone()),
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                args.extend(crate::runtime::utils::value_to_list(item));
            }
            other => args.push(other.clone()),
        }
    }

    pub(super) fn append_flattened_call_arg(
        args: &mut Vec<Value>,
        arg: Value,
        preserve_empty_slip: bool,
    ) {
        match arg {
            Value::Slip(items) => {
                if preserve_empty_slip && items.is_empty() {
                    args.push(Value::Slip(items));
                    return;
                }
                for item in items.iter() {
                    Self::append_slip_item(args, item);
                }
            }
            other => args.push(other),
        }
    }

    pub(super) fn preserve_empty_slip_arg(name: &str) -> bool {
        matches!(
            name,
            "infix:<andthen>"
                | "infix:<notandthen>"
                | "andthen"
                | "notandthen"
                | "__mutsu_andthen_finalize"
        )
    }

    pub(super) fn append_slip_value(args: &mut Vec<Value>, slip_val: Value) {
        match slip_val {
            Value::Array(elements, ..)
            | Value::Seq(elements)
            | Value::HyperSeq(elements)
            | Value::RaceSeq(elements) => {
                args.extend(elements.iter().cloned());
            }
            Value::Capture { positional, named } => {
                args.extend(positional);
                for (k, v) in named {
                    args.push(Value::Pair(k, Box::new(v)));
                }
            }
            Value::Slip(items) => {
                for item in items.iter() {
                    Self::append_slip_item(args, item);
                }
            }
            Value::Hash(map) => {
                for (k, v) in map.iter() {
                    args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
            }
            // When a Pair or ValuePair is slipped via |, it becomes a named
            // argument (regular Pair).  ValuePair is the "positional pair"
            // wrapper produced by (:key(val)), but |$pair always flattens it
            // back to a named argument in Raku.
            Value::ValuePair(key, val) => {
                if let Value::Str(name) = key.as_ref() {
                    args.push(Value::Pair(name.to_string(), val));
                } else {
                    args.push(Value::ValuePair(key, val));
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                args.extend(crate::runtime::utils::value_to_list(&slip_val));
            }
            other => {
                args.push(other);
            }
        }
    }

    /// Auto-FETCH any Proxy values in function call arguments.
    pub(super) fn auto_fetch_proxy_args(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut out = Vec::with_capacity(args.len());
        for arg in args {
            out.push(self.interpreter.auto_fetch_proxy(&arg)?);
        }
        Ok(out)
    }

    pub(super) fn decode_arg_sources(
        &self,
        code: &CompiledCode,
        arg_sources_idx: Option<u32>,
    ) -> Option<Vec<Option<String>>> {
        let idx = arg_sources_idx?;
        let Value::Array(items, ..) = &code.constants[idx as usize] else {
            return None;
        };
        Some(
            items
                .iter()
                .map(|item| match item {
                    Value::Str(name) => Some(name.to_string()),
                    _ => None,
                })
                .collect(),
        )
    }

    pub(super) fn unwrap_var_ref_value(value: Value) -> Value {
        if let Value::Capture { positional, named } = &value
            && positional.is_empty()
            && let Some(Value::Str(_)) = named.get("__mutsu_varref_name")
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            return inner.clone();
        }
        value
    }

    pub(super) fn normalize_call_args_for_target(
        &mut self,
        name: &str,
        raw_args: Vec<Value>,
    ) -> Vec<Value> {
        let plain_args: Vec<Value> = raw_args
            .iter()
            .cloned()
            .map(Self::unwrap_var_ref_value)
            .collect();
        if self.interpreter.has_declared_function(name)
            || self.interpreter.has_multi_function(name)
            || self.interpreter.has_proto(name)
        {
            raw_args
        } else {
            plain_args
        }
    }

    pub(super) fn rewrite_method_name(method_raw: &str, modifier: Option<&str>) -> String {
        match modifier {
            Some("^") => format!("^{}", method_raw),
            Some("!") => format!("!{}", method_raw),
            _ => method_raw.to_string(),
        }
    }

    pub(super) fn call_method_all_with_fallback(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
        skip_native: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        if !skip_native
            && let Some(native_result) =
                self.try_native_method(target, Symbol::intern(method), args)
        {
            return Ok(vec![native_result?]);
        }
        self.interpreter
            .call_method_all_with_values(target.clone(), method, args.to_vec())
    }

    pub(super) fn call_method_mut_with_temp_target(
        &mut self,
        item: &Value,
        method: &str,
        args: Vec<Value>,
        slot: usize,
    ) -> Result<(Value, Value), RuntimeError> {
        let temp_name = format!("__mutsu_hyper_target_{slot}");
        self.interpreter
            .env_mut()
            .insert(temp_name.clone(), item.clone());
        let result =
            self.interpreter
                .call_method_mut_with_values(&temp_name, item.clone(), method, args)?;
        let updated = self
            .interpreter
            .env()
            .get(&temp_name)
            .cloned()
            .unwrap_or_else(|| item.clone());
        self.interpreter.env_mut().remove(&temp_name);
        Ok((result, updated))
    }

    pub(super) fn call_method_all_with_temp_target(
        &mut self,
        item: &Value,
        method: &str,
        args: Vec<Value>,
        slot: usize,
    ) -> Result<(Vec<Value>, Value), RuntimeError> {
        let temp_name = format!("__mutsu_hyper_target_{slot}");
        self.interpreter
            .env_mut()
            .insert(temp_name.clone(), item.clone());
        let result = self
            .interpreter
            .call_method_all_with_values(item.clone(), method, args)?;
        let updated = self
            .interpreter
            .env()
            .get(&temp_name)
            .cloned()
            .unwrap_or_else(|| item.clone());
        self.interpreter.env_mut().remove(&temp_name);
        Ok((result, updated))
    }
}
