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
        let content = std::fs::read_to_string(&path)
            .map_err(|e| RuntimeError::new(format!("Failed to read file '{}': {}", path, e)))?;
        self.eval_eval_string(&content)
    }

    pub(super) fn builtin_eval(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code = if let Some(first) = args.first() {
            match first {
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                    return Err(RuntimeError::new(
                        "EVAL() requires a string or Buf argument, not a Block",
                    ));
                }
                _ => first.to_string_value(),
            }
        } else {
            String::new()
        };
        // Check for :lang adverb
        for arg in args.iter().skip(1) {
            if let Value::Pair(k, v) = arg
                && k == "lang"
            {
                let lang = v.to_string_value();
                if lang == "Raku" || lang == "raku" {
                    // Default language, continue
                } else {
                    return Err(RuntimeError::new(format!(
                        "EVAL with :lang<{}> is not supported",
                        lang
                    )));
                }
            }
        }
        // Check for anonymous `$ is rw` trait which Raku rejects at compile time
        if Self::has_invalid_anonymous_rw_trait(&code) {
            return Err(RuntimeError::new(
                "X::Trait::Invalid: trait 'rw' is not valid on anonymous parameter",
            ));
        }
        // Check if EVAL references &?ROUTINE when not inside a routine
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
