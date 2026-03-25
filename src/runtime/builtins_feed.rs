use super::*;

impl Interpreter {
    pub(super) fn builtin_feed_whatever(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = args.first().cloned().unwrap_or(Value::Nil);
        let list = crate::runtime::value_to_list(&value);
        let list_value = Value::array(list.clone());
        let mut hash_items = std::collections::HashMap::new();
        for chunk in list.chunks(2) {
            if let [k, v] = chunk {
                hash_items.insert(k.to_string_value(), v.clone());
            }
        }
        self.env.insert("$(*)".to_string(), value.clone());
        self.env.insert("@(*)".to_string(), list_value);
        self.env.insert("%(*)".to_string(), Value::hash(hash_items));
        Ok(value)
    }

    pub(super) fn builtin_feed_append(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "__mutsu_feed_append expects sink and source values",
            ));
        }
        let mut out = crate::runtime::value_to_list(&args[0]);
        out.extend(crate::runtime::value_to_list(&args[1]));
        Ok(Value::array(out))
    }

    pub(super) fn builtin_feed_append_whatever(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let source = args.first().cloned().unwrap_or(Value::Nil);
        let current = self
            .env
            .get("@(*)")
            .cloned()
            .unwrap_or_else(|| Value::array(Vec::new()));
        let appended = self.builtin_feed_append(&[current, source])?;
        let list = crate::runtime::value_to_list(&appended);
        let mut hash_items = std::collections::HashMap::new();
        for chunk in list.chunks(2) {
            if let [k, v] = chunk {
                hash_items.insert(k.to_string_value(), v.clone());
            }
        }
        self.env.insert("$(*)".to_string(), appended.clone());
        self.env
            .insert("@(*)".to_string(), Value::array(list.clone()));
        self.env.insert("%(*)".to_string(), Value::hash(hash_items));
        Ok(appended)
    }

    pub(super) fn builtin_feed_array_assign(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let value = args.first().cloned().unwrap_or(Value::Nil);
        let int_max = i64::MAX;
        let infinite = match &value {
            Value::Range(_, end) | Value::RangeExcl(_, end) => *end == int_max,
            Value::GenericRange { end, .. } => end.to_f64().is_infinite(),
            Value::Int(end) => *end == int_max,
            _ => false,
        };
        if infinite {
            return Err(RuntimeError::new(
                "Cannot eagerly assign an infinite feed source to an array",
            ));
        }
        Ok(crate::runtime::coerce_to_array(value))
    }

    pub(super) fn builtin_reverse_xx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(
                "__mutsu_reverse_xx expects count and thunk",
            ));
        }
        let count = crate::runtime::to_int(&args[0]);
        if count <= 0 {
            return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
        }
        let thunk = args[1].clone();
        let mut values = Vec::with_capacity(count as usize);
        for _ in 0..count {
            values.push(self.eval_call_on_value(thunk.clone(), Vec::new())?);
        }
        Ok(Value::Seq(std::sync::Arc::new(values)))
    }

    pub(super) fn builtin_reverse_andthen(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(
                "__mutsu_reverse_andthen expects condition and thunk",
            ));
        }
        let cond = args[0].clone();
        if !crate::runtime::types::value_is_defined(&cond) {
            return Ok(cond);
        }
        let saved_topic = self.env.get("_").cloned();
        self.env.insert("_".to_string(), cond);
        let result = self.eval_call_on_value(args[1].clone(), Vec::new());
        match saved_topic {
            Some(value) => {
                self.env.insert("_".to_string(), value);
            }
            None => {
                self.env.remove("_");
            }
        }
        result
    }

    pub(super) fn builtin_andthen_finalize(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(
                "__mutsu_andthen_finalize expects lhs and rhs",
            ));
        }
        let lhs = args[0].clone();
        let rhs = args[1].clone();
        if matches!(
            rhs,
            Value::Sub(_)
                | Value::WeakSub(_)
                | Value::Routine { .. }
                | Value::Instance { .. }
                | Value::Mixin(..)
        ) {
            let saved_topic = self.env.get("_").cloned();
            self.env.insert("_".to_string(), lhs.clone());
            let result = self.eval_call_on_value(rhs, vec![lhs]);
            match saved_topic {
                Some(value) => {
                    self.env.insert("_".to_string(), value);
                }
                None => {
                    self.env.remove("_");
                }
            }
            result
        } else {
            Ok(rhs)
        }
    }

    pub(super) fn builtin_cross_shortcircuit(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 3 {
            return Err(RuntimeError::new(
                "__mutsu_cross_shortcircuit expects op, lhs, thunk",
            ));
        }
        let op = args[0].to_string_value();
        let left_values = if matches!(args[1], Value::Nil) {
            vec![Value::Nil]
        } else {
            crate::runtime::value_to_list(&args[1])
        };
        let thunk = args[2].clone();
        let mut out = Vec::new();
        for left in left_values {
            let needs_rhs = match op.as_str() {
                "and" | "&&" => left.truthy(),
                "or" | "||" => !left.truthy(),
                "andthen" => crate::runtime::types::value_is_defined(&left),
                "orelse" => !crate::runtime::types::value_is_defined(&left),
                _ => false,
            };
            if !needs_rhs {
                out.push(left);
                continue;
            }
            let rhs_value = if op == "andthen" || op == "orelse" {
                let saved_topic = self.env.get("_").cloned();
                self.env.insert("_".to_string(), left.clone());
                let result = self.eval_call_on_value(thunk.clone(), Vec::new());
                match saved_topic {
                    Some(value) => {
                        self.env.insert("_".to_string(), value);
                    }
                    None => {
                        self.env.remove("_");
                    }
                }
                result?
            } else {
                self.eval_call_on_value(thunk.clone(), Vec::new())?
            };
            let rhs_values = crate::runtime::value_to_list(&rhs_value);
            for rhs in rhs_values {
                out.push(rhs);
            }
        }
        Ok(Value::array(out))
    }

    /// Zip with short-circuit semantics for and/&&/or/||/andthen/orelse.
    /// Args: [op_name, left_list, right_thunk]
    /// The right thunk is only evaluated per-element when the operator needs it.
    pub(super) fn builtin_zip_shortcircuit(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 3 {
            return Err(RuntimeError::new(
                "__mutsu_zip_shortcircuit expects op, lhs, thunk",
            ));
        }
        let op = args[0].to_string_value();
        // For zip, treat scalars (including Nil) as single-element lists
        let left_values = if matches!(args[1], Value::Nil) {
            vec![Value::Nil]
        } else {
            let list = crate::runtime::value_to_list(&args[1]);
            if list.is_empty() {
                vec![args[1].clone()]
            } else {
                list
            }
        };
        let thunk = args[2].clone();
        let mut out = Vec::new();
        for left in left_values {
            let needs_rhs = match op.as_str() {
                "and" | "&&" => left.truthy(),
                "or" | "||" => !left.truthy(),
                "andthen" => crate::runtime::types::value_is_defined(&left),
                "orelse" => !crate::runtime::types::value_is_defined(&left),
                _ => false,
            };
            if !needs_rhs {
                out.push(left);
                continue;
            }
            let rhs_value = if op == "andthen" || op == "orelse" {
                let saved_topic = self.env.get("_").cloned();
                self.env.insert("_".to_string(), left.clone());
                let result = self.eval_call_on_value(thunk.clone(), Vec::new());
                match saved_topic {
                    Some(value) => {
                        self.env.insert("_".to_string(), value);
                    }
                    None => {
                        self.env.remove("_");
                    }
                }
                result?
            } else {
                self.eval_call_on_value(thunk.clone(), Vec::new())?
            };
            // The thunk returns a list; zip pairs elements 1:1
            let rhs_items = crate::runtime::value_to_list(&rhs_value);
            // Take the first element from the thunked result (zip is 1:1)
            out.push(rhs_items.into_iter().next().unwrap_or(Value::Nil));
        }
        Ok(Value::array(out))
    }

    /// Zip xx with thunked left side: ($thunk,) Zxx (counts,)
    /// Args: [right_list, left_thunk]
    /// The left thunk is evaluated per-element, repeated `count` times.
    pub(super) fn builtin_zip_xx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(
                "__mutsu_zip_xx expects right_list and left_thunk",
            ));
        }
        let right_values = crate::runtime::value_to_list(&args[0]);
        let thunk = args[1].clone();
        let mut out = Vec::new();
        for count_val in right_values {
            let count = crate::runtime::to_int(&count_val);
            for _ in 0..count {
                let val = self.eval_call_on_value(thunk.clone(), Vec::new())?;
                let items = crate::runtime::value_to_list(&val);
                out.push(items.into_iter().next().unwrap_or(Value::Nil));
            }
        }
        Ok(Value::Seq(std::sync::Arc::new(out)))
    }
}
