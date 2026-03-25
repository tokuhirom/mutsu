use super::*;

impl Interpreter {
    pub(super) fn builtin_make(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = args.first().cloned().unwrap_or(Value::Nil);
        self.env.insert("made".to_string(), value.clone());
        self.action_made = Some(value.clone());
        Ok(value)
    }

    pub(super) fn builtin_made(&self) -> Result<Value, RuntimeError> {
        Ok(self.env.get("made").cloned().unwrap_or(Value::Nil))
    }

    pub(crate) fn builtin_callframe(
        &self,
        args: &[Value],
        default_depth: usize,
    ) -> Result<Value, RuntimeError> {
        let mut depth = default_depth;
        let mut callsite_line: Option<i64> = None;
        for arg in args {
            match arg {
                Value::Int(i) if *i >= 0 => depth = *i as usize,
                Value::Num(f) if *f >= 0.0 => depth = *f as usize,
                Value::Pair(k, v) if k == "__callframe_line" => {
                    if let Value::Int(line) = v.as_ref() {
                        callsite_line = Some(*line);
                    }
                }
                _ => {}
            }
        }
        if let Some(frame) = self.callframe_value(depth, callsite_line) {
            return Ok(frame);
        }
        Ok(Value::Nil)
    }

    pub(super) fn builtin_evalfile(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .ok_or_else(|| RuntimeError::new("EVALFILE requires a filename"))?;
        let code = fs::read_to_string(&path)
            .map_err(|err| RuntimeError::new(format!("Failed to read {}: {}", path, err)))?;
        let saved_file = self.env.get("?FILE").cloned();
        self.env.insert("?FILE".to_string(), Value::str(path));
        let result = self.eval_eval_string(&code);
        if let Some(prev) = saved_file {
            self.env.insert("?FILE".to_string(), prev);
        } else {
            self.env.remove("?FILE");
        }
        result
    }

    pub(super) fn builtin_eval(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // EVAL only accepts strings (and Buf), not blocks
        if let Some(first_arg) = Self::positional_value(args, 0) {
            match first_arg {
                Value::Sub(_) | Value::Routine { .. } | Value::WeakSub(_) => {
                    return Err(RuntimeError::new(
                        "EVAL() requires a string or Buf argument, not a Block",
                    ));
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_like_class(&class_name.resolve()) => {
                    // Buf argument: decode as UTF-8
                    if let Some(Value::Array(items, _)) = attributes.get("bytes") {
                        let bytes: Vec<u8> = items
                            .iter()
                            .map(|v| match v {
                                Value::Int(n) => *n as u8,
                                _ => v.to_string_value().parse::<u8>().unwrap_or(0),
                            })
                            .collect();
                        let code = String::from_utf8_lossy(&bytes).to_string();
                        return self.eval_eval_string(&code);
                    }
                }
                _ => {}
            }
        }
        let code = Self::positional_string(args, 0);
        if Self::has_invalid_anonymous_rw_trait(&code) {
            return Err(RuntimeError::new(
                "X::Trait::Invalid: trait 'rw' is not valid on anonymous parameter",
            ));
        }
        if let Some(lang) = Self::named_value(args, "lang") {
            let lang = lang.to_string_value();
            if !lang.eq_ignore_ascii_case("raku") && !lang.eq_ignore_ascii_case("perl6") {
                return Err(RuntimeError::new(format!(
                    "EVAL with :lang<{}> is not supported",
                    lang
                )));
            }
        }
        if code.contains("&?ROUTINE") && self.routine_stack.is_empty() {
            return Err(RuntimeError::undeclared_symbols("Undeclared name"));
        }
        self.eval_eval_string(&code)
    }

    pub(super) fn builtin_dd(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned().unwrap_or(Value::Nil);
        self.emit_output(&format!("{:?}\n", val));
        Ok(val)
    }
}
