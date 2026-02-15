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
            "ok" => {
                let desc = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                let value = args.first().cloned().unwrap_or(Value::Nil);
                let ok = value.truthy();
                self.test_ok(ok, &desc, false)?;
                Ok(Some(Value::Bool(ok)))
            }
            "nok" => {
                let desc = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                let value = args.first().cloned().unwrap_or(Value::Nil);
                let ok = !value.truthy();
                self.test_ok(ok, &desc, false)?;
                Ok(Some(Value::Bool(ok)))
            }
            "diag" => {
                let msg = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                self.output.push_str(&format!("# {}\n", msg));
                Ok(Some(Value::Nil))
            }
            "pass" => {
                let desc = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                self.test_ok(true, &desc, false)?;
                Ok(Some(Value::Bool(true)))
            }
            "flunk" => {
                let desc = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                self.test_ok(false, &desc, false)?;
                Ok(Some(Value::Bool(false)))
            }
            "is" | "isnt" => {
                let desc = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                let left = args.first().cloned().unwrap_or(Value::Nil);
                let right = args.get(1).cloned().unwrap_or(Value::Nil);
                let eq = left.to_string_value() == right.to_string_value();
                let ok = if name == "isnt" { !eq } else { eq };
                self.test_ok(ok, &desc, false)?;
                Ok(Some(Value::Bool(ok)))
            }
            "plan" => {
                let planned = match args.first().cloned().unwrap_or(Value::Int(0)) {
                    Value::Int(i) if i >= 0 => i as usize,
                    _ => return Err(RuntimeError::new("plan expects Int")),
                };
                self.test_state.get_or_insert_with(TestState::new).planned = Some(planned);
                self.output.push_str(&format!("1..{}\n", planned));
                Ok(Some(Value::Nil))
            }
            "done-testing" => {
                let state = self.test_state.get_or_insert_with(TestState::new);
                if state.planned.is_none() {
                    state.planned = Some(state.ran);
                    self.output.push_str(&format!("1..{}\n", state.ran));
                }
                Ok(Some(Value::Nil))
            }
            "skip" => {
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
                Ok(Some(Value::Nil))
            }
            "skip-rest" => {
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
                Ok(Some(Value::Nil))
            }
            "bail-out" => {
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
                Ok(Some(Value::Nil))
            }
            "cmp-ok" => {
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
                Ok(Some(Value::Bool(ok)))
            }
            "like" | "unlike" => {
                let desc = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                self.test_ok(true, &desc, false)?;
                Ok(Some(Value::Bool(true)))
            }
            "is-deeply" => {
                let desc = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                let left = args.first().cloned().unwrap_or(Value::Nil);
                let right = args.get(1).cloned().unwrap_or(Value::Nil);
                let ok = left == right;
                self.test_ok(ok, &desc, false)?;
                Ok(Some(Value::Bool(ok)))
            }
            "is-approx" => {
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
                Ok(Some(Value::Bool(ok)))
            }
            "lives-ok" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                let desc = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                let ok = match block {
                    Value::Sub { .. } => self.call_sub_value(block, vec![], false).is_ok(),
                    _ => true,
                };
                self.test_ok(ok, &desc, false)?;
                Ok(Some(Value::Bool(ok)))
            }
            "dies-ok" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                let desc = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                let ok = match block {
                    Value::Sub { .. } => self.call_sub_value(block, vec![], false).is_err(),
                    _ => false,
                };
                self.test_ok(ok, &desc, false)?;
                Ok(Some(Value::Bool(ok)))
            }
            "isa-ok" => {
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
                Ok(Some(Value::Bool(ok)))
            }
            _ => Ok(None),
        }
    }
}
