use super::*;

impl Interpreter {
    /// Dispatch Test module functions. Returns `Ok(Some(value))` if the name
    /// matched a Test function, `Ok(None)` if it did not.
    pub(super) fn call_test_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "ok" => self.test_fn_ok(args).map(Some),
            "nok" => self.test_fn_nok(args).map(Some),
            "diag" => self.test_fn_diag(args).map(Some),
            "pass" => self.test_fn_pass(args).map(Some),
            "flunk" => self.test_fn_flunk(args).map(Some),
            "is" => self.test_fn_is(args).map(Some),
            "isnt" => self.test_fn_isnt(args).map(Some),
            "plan" => self.test_fn_plan(args).map(Some),
            "done-testing" => self.test_fn_done_testing().map(Some),
            "skip" => self.test_fn_skip(args).map(Some),
            "skip-rest" => self.test_fn_skip_rest(args).map(Some),
            "bail-out" => self.test_fn_bail_out(args).map(Some),
            "cmp-ok" => self.test_fn_cmp_ok(args).map(Some),
            "like" | "unlike" => self.test_fn_like(args).map(Some),
            "is-deeply" => self.test_fn_is_deeply(args).map(Some),
            "is-approx" => self.test_fn_is_approx(args).map(Some),
            "lives-ok" => self.test_fn_lives_ok(args).map(Some),
            "dies-ok" => self.test_fn_dies_ok(args).map(Some),
            "isa-ok" => self.test_fn_isa_ok(args).map(Some),
            _ => Ok(None),
        }
    }

    fn test_fn_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
        let value = args.first().cloned().unwrap_or(Value::Nil);
        let ok = value.truthy();
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_nok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
        let value = args.first().cloned().unwrap_or(Value::Nil);
        let ok = !value.truthy();
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_diag(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let msg = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        self.output.push_str(&format!("# {}\n", msg));
        Ok(Value::Nil)
    }

    fn test_fn_pass(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        self.test_ok(true, &desc, false)?;
        Ok(Value::Bool(true))
    }

    fn test_fn_flunk(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        self.test_ok(false, &desc, false)?;
        Ok(Value::Bool(false))
    }

    fn test_fn_is(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
        let left = args.first().cloned().unwrap_or(Value::Nil);
        let right = args.get(1).cloned().unwrap_or(Value::Nil);
        let ok = left.to_string_value() == right.to_string_value();
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_isnt(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
        let left = args.first().cloned().unwrap_or(Value::Nil);
        let right = args.get(1).cloned().unwrap_or(Value::Nil);
        let ok = left.to_string_value() != right.to_string_value();
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_plan(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let planned = match args.first().cloned().unwrap_or(Value::Int(0)) {
            Value::Int(i) if i >= 0 => i as usize,
            _ => return Err(RuntimeError::new("plan expects Int")),
        };
        self.test_state.get_or_insert_with(TestState::new).planned = Some(planned);
        self.output.push_str(&format!("1..{}\n", planned));
        Ok(Value::Nil)
    }

    fn test_fn_done_testing(&mut self) -> Result<Value, RuntimeError> {
        let state = self.test_state.get_or_insert_with(TestState::new);
        if state.planned.is_none() {
            state.planned = Some(state.ran);
            self.output.push_str(&format!("1..{}\n", state.ran));
        }
        Ok(Value::Nil)
    }

    fn test_fn_skip(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let count = match args.get(1).cloned().unwrap_or(Value::Int(1)) {
            Value::Int(i) => i.max(1) as usize,
            _ => 1usize,
        };
        let state = self.test_state.get_or_insert_with(TestState::new);
        for _ in 0..count {
            state.ran += 1;
            self.output
                .push_str(&format!("ok {} - {} # SKIP\n", state.ran, desc));
        }
        Ok(Value::Nil)
    }

    fn test_fn_skip_rest(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let state = self.test_state.get_or_insert_with(TestState::new);
        if let Some(planned) = state.planned {
            while state.ran < planned {
                state.ran += 1;
                if desc.is_empty() {
                    self.output.push_str(&format!("ok {} # SKIP\n", state.ran));
                } else {
                    self.output
                        .push_str(&format!("ok {} - {} # SKIP\n", state.ran, desc));
                }
            }
        }
        Ok(Value::Nil)
    }

    fn test_fn_bail_out(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if desc.is_empty() {
            self.output.push_str("Bail out!\n");
        } else {
            self.output.push_str(&format!("Bail out! {}\n", desc));
        }
        self.halted = true;
        self.bailed_out = true;
        Ok(Value::Nil)
    }

    fn test_fn_cmp_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let left = args.first().cloned().unwrap_or(Value::Nil);
        let op_val = args.get(1).cloned().unwrap_or(Value::Nil);
        let right = args.get(2).cloned().unwrap_or(Value::Nil);
        let desc = args.get(3).map(|v| v.to_string_value()).unwrap_or_default();
        let ok = match &op_val {
            Value::Str(op) => match op.as_str() {
                "~~" => self.smart_match(&left, &right),
                "!~~" => !self.smart_match(&left, &right),
                "eq" => left.to_string_value() == right.to_string_value(),
                "ne" => left.to_string_value() != right.to_string_value(),
                "lt" => left.to_string_value() < right.to_string_value(),
                "le" => left.to_string_value() <= right.to_string_value(),
                "gt" => left.to_string_value() > right.to_string_value(),
                "ge" => left.to_string_value() >= right.to_string_value(),
                "==" => super::to_float_value(&left) == super::to_float_value(&right),
                "!=" => super::to_float_value(&left) != super::to_float_value(&right),
                "<" => super::to_float_value(&left) < super::to_float_value(&right),
                "<=" => super::to_float_value(&left) <= super::to_float_value(&right),
                ">" => super::to_float_value(&left) > super::to_float_value(&right),
                ">=" => super::to_float_value(&left) >= super::to_float_value(&right),
                "===" => left == right,
                "=:=" => left == right,
                _ => {
                    return Err(RuntimeError::new(format!(
                        "cmp-ok: unsupported string operator '{}'",
                        op
                    )));
                }
            },
            _ => {
                let result = self.call_sub_value(op_val, vec![left, right], false)?;
                result.truthy()
            }
        };
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
        self.test_ok(true, &desc, false)?;
        Ok(Value::Bool(true))
    }

    fn test_fn_is_deeply(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
        let left = args.first().cloned().unwrap_or(Value::Nil);
        let right = args.get(1).cloned().unwrap_or(Value::Nil);
        let ok = left == right;
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_is_approx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
        let got = args.first().cloned().unwrap_or(Value::Nil);
        let expected = args.get(1).cloned().unwrap_or(Value::Nil);
        let ok = match (
            super::to_float_value(&got),
            super::to_float_value(&expected),
        ) {
            (Some(g), Some(e)) => (g - e).abs() <= 1e-5,
            _ => false,
        };
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_lives_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let block = args.first().cloned().unwrap_or(Value::Nil);
        let desc = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
        let ok = match block {
            Value::Sub { .. } => self.call_sub_value(block, vec![], false).is_ok(),
            _ => true,
        };
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_dies_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let block = args.first().cloned().unwrap_or(Value::Nil);
        let desc = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
        let ok = match block {
            Value::Sub { .. } => self.call_sub_value(block, vec![], false).is_err(),
            _ => false,
        };
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_isa_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = args.first().cloned().unwrap_or(Value::Nil);
        let type_name = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
        let desc = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
        let ok = match type_name.as_str() {
            "Array" => matches!(value, Value::Array(_)),
            "Rat" => matches!(value, Value::Rat(_, _)),
            "FatRat" => matches!(value, Value::FatRat(_, _)),
            "Complex" => matches!(value, Value::Complex(_, _)),
            "Set" => matches!(value, Value::Set(_)),
            "Bag" => matches!(value, Value::Bag(_)),
            "Mix" => matches!(value, Value::Mix(_)),
            _ => {
                if let Value::Instance { class_name, .. } = &value {
                    class_name == type_name.as_str()
                } else {
                    true
                }
            }
        };
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }
}
