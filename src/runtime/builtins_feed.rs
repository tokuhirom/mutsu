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
        // The RHS of `andthen`/`orelse`/`notandthen` is invoked with the topic
        // only when it is actually callable. A plain instance (`$x orelse
        // Foo.new`) is a value, not a block — call it only if it is `Callable`
        // (has a `CALL-ME`), otherwise `Foo.new` would be mis-invoked as `Foo()`.
        let rhs_is_callable = match &rhs {
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } | Value::Mixin(..) => true,
            Value::Instance { class_name, .. } => {
                let cn = class_name.resolve();
                self.class_has_method(&cn, "CALL-ME")
            }
            _ => false,
        };
        if rhs_is_callable {
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
        let mut thunk_ran = false;
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
            thunk_ran = true;
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
        // The thunk (`X` short-circuit RHS, e.g. `Nil Xorelse ($t = $_,)`) is an
        // immediately-invoked closure, so escape analysis never boxes the outer
        // lexicals it captures-and-writes. Record its captured-outer writes so the
        // enclosing `__mutsu_cross_shortcircuit` call site drains them back into
        // the caller's locals (mirrors the lazy-map / gather / subst carriers,
        // slice 1.6) — without this they are lost once the blanket reconcile is
        // removed (docs/captured-outer-cell-sharing.md §7.2).
        if thunk_ran
            && let Value::Sub(ref data) = thunk
            && let Some(code) = data.compiled_code.clone()
        {
            self.record_eager_block_free_var_writeback(&code, &data.params);
        }
        Ok(Value::array(out))
    }

    /// Zip with short-circuit semantics for and/&&/or/||/andthen/orelse.
    /// Args: [op_name, left_list, right_thunks]
    /// `right_thunks` is a list with one entry per right-hand element. Each entry
    /// is either a 0-arg thunk (from a literal list, so side effects only fire
    /// when the operator actually needs the right operand) or a plain value (from
    /// a pre-evaluated right operand). Zip is 1:1, so element `i` of the left list
    /// pairs with thunk `i`.
    pub(super) fn builtin_zip_shortcircuit(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 3 {
            return Err(RuntimeError::new(
                "__mutsu_zip_shortcircuit expects op, lhs, thunks",
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
        let right_thunks = crate::runtime::value_to_list(&args[2]);
        let len = left_values.len().min(right_thunks.len());
        let mut out = Vec::with_capacity(len);
        let mut ran_thunks: Vec<Value> = Vec::new();
        for i in 0..len {
            let left = left_values[i].clone();
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
            let entry = right_thunks[i].clone();
            // A plain value (not a thunk) is used as-is; a thunk is evaluated now.
            let rhs_value = if !matches!(entry, Value::Sub(_)) {
                entry
            } else if op == "andthen" || op == "orelse" {
                ran_thunks.push(entry.clone());
                let saved_topic = self.env.get("_").cloned();
                self.env.insert("_".to_string(), left.clone());
                let result = self.eval_call_on_value(entry, Vec::new());
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
                ran_thunks.push(entry.clone());
                self.eval_call_on_value(entry, Vec::new())?
            };
            out.push(rhs_value);
        }
        // Record each evaluated thunk's captured-outer writes so the call site
        // drains them into the caller's locals (carrier; see the topic variant
        // and slice 1.9 / docs §7.1i).
        for thunk in ran_thunks {
            if let Value::Sub(ref data) = thunk
                && let Some(code) = data.compiled_code.clone()
            {
                self.record_eager_block_free_var_writeback(&code, &data.params);
            }
        }
        Ok(Value::array(out))
    }

    /// Zip with short-circuit semantics for `andthen`/`orelse`, which topicalize
    /// the right operand with the corresponding left value. Args: [op, lhs,
    /// whole_list_thunk]. The right operand is a single thunk over the whole
    /// list so that referencing `$_` inside resolves to the topic we set rather
    /// than an implicit block parameter.
    pub(super) fn builtin_zip_shortcircuit_topic(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != 3 {
            return Err(RuntimeError::new(
                "__mutsu_zip_shortcircuit_topic expects op, lhs, thunk",
            ));
        }
        let op = args[0].to_string_value();
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
        let mut thunk_ran = false;
        for (i, left) in left_values.iter().enumerate() {
            let needs_rhs = match op.as_str() {
                "andthen" => crate::runtime::types::value_is_defined(left),
                "orelse" => !crate::runtime::types::value_is_defined(left),
                _ => false,
            };
            if !needs_rhs {
                out.push(left.clone());
                continue;
            }
            thunk_ran = true;
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
            let rhs_value = result?;
            // The thunk returns the whole right list; pick element i (zip is 1:1).
            let rhs_items = crate::runtime::value_to_list(&rhs_value);
            out.push(rhs_items.into_iter().nth(i).unwrap_or(Value::Nil));
        }
        // Record the topicalizing thunk's captured-outer writes (`$x = $_`) so the
        // call site drains them into the caller's locals — same as the
        // `__mutsu_cross_shortcircuit` carrier (slice 1.9). Without this the write
        // is lost once the blanket reconcile is removed
        // (docs/captured-outer-cell-sharing.md §7.1i).
        if thunk_ran
            && let Value::Sub(ref data) = thunk
            && let Some(code) = data.compiled_code.clone()
        {
            self.record_eager_block_free_var_writeback(&code, &data.params);
        }
        Ok(Value::array(out))
    }

    /// Zip xx with thunked left side: `<a b c> Zxx (0,1,0)`
    /// For each count in the right list, re-evaluates the thunk `count` times,
    /// extracting element at position `i`. Preserves side-effect semantics.
    pub(super) fn builtin_zip_xx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(RuntimeError::new(
                "__mutsu_zip_xx expects right_list and left_thunk",
            ));
        }
        let right_values = crate::runtime::value_to_list(&args[0]);
        let thunk = args[1].clone();
        let mut out = Vec::new();
        for (i, count_val) in right_values.iter().enumerate() {
            let count = crate::runtime::to_int(count_val).max(0) as usize;
            let mut repeated = Vec::with_capacity(count);
            for _ in 0..count {
                let val = self.eval_call_on_value(thunk.clone(), Vec::new())?;
                let items = crate::runtime::value_to_list(&val);
                if i >= items.len() {
                    return Ok(Value::Seq(std::sync::Arc::new(out)));
                }
                repeated.push(items.into_iter().nth(i).unwrap_or(Value::Nil));
            }
            out.push(Value::Seq(std::sync::Arc::new(repeated)));
        }
        Ok(Value::Seq(std::sync::Arc::new(out)))
    }
}
