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
        let lhs = &args[1];
        let thunk = &args[2];

        // Flatten lhs to list
        let lhs_items = crate::runtime::value_to_list(lhs);

        // For each lhs element, check short-circuit condition
        for left in &lhs_items {
            let should_short = match op.as_str() {
                "and" | "&&" => left.truthy(),
                "or" | "||" => !left.truthy(),
                "andthen" => crate::runtime::types::value_is_defined(left),
                "orelse" => !crate::runtime::types::value_is_defined(left),
                _ => true,
            };
            if !should_short {
                // Short circuit: return lhs element(s) without evaluating rhs
                if lhs_items.len() == 1 {
                    return Ok(left.clone());
                }
                return Ok(Value::array(lhs_items));
            }
        }

        // All passed: evaluate thunk (rhs) and compute cross product
        let rhs = self.eval_call_on_value(thunk.clone(), Vec::new())?;
        let rhs_items = crate::runtime::value_to_list(&rhs);

        let mut results = Vec::new();
        for left in &lhs_items {
            for right in &rhs_items {
                let pair_result = match op.as_str() {
                    "and" | "&&" => {
                        if left.truthy() {
                            right.clone()
                        } else {
                            left.clone()
                        }
                    }
                    "or" | "||" => {
                        if left.truthy() {
                            left.clone()
                        } else {
                            right.clone()
                        }
                    }
                    "andthen" => {
                        if crate::runtime::types::value_is_defined(left) {
                            right.clone()
                        } else {
                            left.clone()
                        }
                    }
                    "orelse" => {
                        if crate::runtime::types::value_is_defined(left) {
                            left.clone()
                        } else {
                            right.clone()
                        }
                    }
                    _ => right.clone(),
                };
                results.push(pair_result);
            }
        }
        Ok(Value::array(results))
    }

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
        let lhs = &args[1];
        let thunk = &args[2];

        let lhs_items = crate::runtime::value_to_list(lhs);

        // For each lhs element, check short-circuit condition
        for left in &lhs_items {
            let should_short = match op.as_str() {
                "and" | "&&" => left.truthy(),
                "or" | "||" => !left.truthy(),
                "andthen" => crate::runtime::types::value_is_defined(left),
                "orelse" => !crate::runtime::types::value_is_defined(left),
                _ => true,
            };
            if !should_short {
                if lhs_items.len() == 1 {
                    return Ok(left.clone());
                }
                return Ok(Value::array(lhs_items));
            }
        }

        // All passed: evaluate thunk (rhs) and zip
        let rhs = self.eval_call_on_value(thunk.clone(), Vec::new())?;
        let rhs_items = crate::runtime::value_to_list(&rhs);

        let mut results = Vec::new();
        let len = lhs_items.len().min(rhs_items.len());
        for i in 0..len {
            let left = &lhs_items[i];
            let right = &rhs_items[i];
            let pair_result = match op.as_str() {
                "and" | "&&" => {
                    if left.truthy() {
                        right.clone()
                    } else {
                        left.clone()
                    }
                }
                "or" | "||" => {
                    if left.truthy() {
                        left.clone()
                    } else {
                        right.clone()
                    }
                }
                "andthen" => {
                    if crate::runtime::types::value_is_defined(left) {
                        right.clone()
                    } else {
                        left.clone()
                    }
                }
                "orelse" => {
                    if crate::runtime::types::value_is_defined(left) {
                        left.clone()
                    } else {
                        right.clone()
                    }
                }
                _ => right.clone(),
            };
            results.push(pair_result);
        }
        Ok(Value::array(results))
    }

    pub(super) fn builtin_zip_xx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(
                "__mutsu_zip_xx expects right_list and left_thunk",
            ));
        }
        let right = &args[0];
        let left_thunk = &args[1];

        let right_items = crate::runtime::value_to_list(right);
        let left = self.eval_call_on_value(left_thunk.clone(), Vec::new())?;
        let left_items = crate::runtime::value_to_list(&left);

        // Zip: pair up elements, shorter list determines length
        let len = left_items.len().min(right_items.len());
        let mut results = Vec::with_capacity(len);
        for i in 0..len {
            results.push(Value::array(vec![
                left_items[i].clone(),
                right_items[i].clone(),
            ]));
        }
        Ok(Value::array(results))
    }
}
