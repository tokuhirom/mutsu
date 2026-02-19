use super::*;

impl VM {
    pub(super) fn pop_binary_operands(&mut self) -> (Value, Value) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        (left, right)
    }

    pub(super) fn exec_binary_infallible<F>(&mut self, f: F)
    where
        F: FnOnce(Value, Value) -> Value,
    {
        let (left, right) = self.pop_binary_operands();
        self.stack.push(f(left, right));
    }

    pub(super) fn exec_binary_fallible<F>(&mut self, f: F) -> Result<(), RuntimeError>
    where
        F: FnOnce(Value, Value) -> Result<Value, RuntimeError>,
    {
        let (left, right) = self.pop_binary_operands();
        self.stack.push(f(left, right)?);
        Ok(())
    }

    pub(super) fn compare_order_values(left: &Value, right: &Value) -> std::cmp::Ordering {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::Rat(_, _), _)
            | (_, Value::Rat(_, _))
            | (Value::FatRat(_, _), _)
            | (_, Value::FatRat(_, _)) => {
                if let (Some((an, ad)), Some((bn, bd))) =
                    (runtime::to_rat_parts(left), runtime::to_rat_parts(right))
                {
                    (an.wrapping_mul(bd)).cmp(&(bn.wrapping_mul(ad)))
                } else {
                    left.to_string_value().cmp(&right.to_string_value())
                }
            }
            (Value::Num(a), Value::Num(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
            (Value::Int(a), Value::Num(b)) => (*a as f64)
                .partial_cmp(b)
                .unwrap_or(std::cmp::Ordering::Equal),
            (Value::Num(a), Value::Int(b)) => a
                .partial_cmp(&(*b as f64))
                .unwrap_or(std::cmp::Ordering::Equal),
            (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
                runtime::version_cmp_parts(ap, bp)
            }
            _ => left.to_string_value().cmp(&right.to_string_value()),
        }
    }

    pub(super) fn exec_set_relation_op<F>(&mut self, relation: F)
    where
        F: FnOnce(&std::collections::HashSet<String>, &std::collections::HashSet<String>) -> bool,
    {
        let (left, right) = self.pop_binary_operands();
        let a = runtime::coerce_to_set(&left);
        let b = runtime::coerce_to_set(&right);
        self.stack.push(Value::Bool(relation(&a, &b)));
    }

    pub(super) fn exec_pre_inc_dec_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        increment: bool,
    ) {
        let name = Self::const_str(code, name_idx);
        let val = self
            .interpreter
            .env()
            .get(name)
            .cloned()
            .unwrap_or(Value::Int(0));
        let new_val = match val {
            Value::Int(i) => Value::Int(if increment { i + 1 } else { i - 1 }),
            Value::Bool(_) => Value::Bool(increment),
            Value::Rat(n, d) => {
                if increment {
                    make_rat(n + d, d)
                } else {
                    make_rat(n - d, d)
                }
            }
            _ => Value::Int(if increment { 1 } else { -1 }),
        };
        self.interpreter
            .env_mut()
            .insert(name.to_string(), new_val.clone());
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(new_val);
    }

    pub(super) fn exec_subst_op(
        &mut self,
        code: &CompiledCode,
        pattern_idx: u32,
        replacement_idx: u32,
        destructive: bool,
    ) {
        let pattern = Self::const_str(code, pattern_idx).to_string();
        let replacement = Self::const_str(code, replacement_idx).to_string();
        let target = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        let text = target.to_string_value();

        if let Some((start, end)) = self.interpreter.regex_find_first_bridge(&pattern, &text) {
            let start_b = runtime::char_idx_to_byte(&text, start);
            let end_b = runtime::char_idx_to_byte(&text, end);
            let mut out = String::new();
            out.push_str(&text[..start_b]);
            out.push_str(&replacement);
            out.push_str(&text[end_b..]);
            let out_value = Value::Str(out);
            if destructive {
                self.interpreter
                    .env_mut()
                    .insert("_".to_string(), out_value.clone());
            }
            self.stack.push(out_value);
        } else {
            self.stack.push(Value::Str(text));
        }
    }
}
