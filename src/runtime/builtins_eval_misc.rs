use super::*;

impl Interpreter {
    pub(super) fn builtin_make(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = if args.len() > 1 {
            Value::Slip(std::sync::Arc::new(args.to_vec()))
        } else {
            args.first().cloned().unwrap_or(Value::Nil)
        };
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

    /// Implementation of `caller()` function.
    pub(crate) fn builtin_caller(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut type_filter: Option<String> = None;
        let mut skip: usize = 0;
        let mut callsite_line: Option<i64> = None;
        for arg in args {
            match arg {
                Value::Pair(k, v) if k == "__callframe_line" => {
                    if let Value::Int(line) = v.as_ref() {
                        callsite_line = Some(*line);
                    }
                }
                Value::Pair(k, v) if k == "skip" => {
                    skip = match v.as_ref() {
                        Value::Int(i) => *i as usize,
                        other => other.to_string_value().parse::<usize>().unwrap_or(0),
                    };
                }
                Value::Package(name) => {
                    type_filter = Some(name.resolve());
                }
                Value::Str(s) => {
                    type_filter = Some(s.to_string());
                }
                Value::Mixin(inner, _) => {
                    type_filter = Some(inner.to_string_value());
                }
                _ => {}
            }
        }

        let stack = &self.routine_stack;

        // Step 1: Find the current routine by walking from the top,
        // skipping block frames (is_block == true).
        // Then go one more frame up to find the caller of that routine.
        let mut caller_start_idx: Option<usize> = None;
        let mut found_routine = false;
        for i in (0..stack.len()).rev() {
            let frame = &stack[i];
            if !frame.is_block {
                if found_routine {
                    // This is the caller of the current routine
                    caller_start_idx = Some(i);
                    break;
                }
                // This is the current routine; skip it
                found_routine = true;
            }
        }

        // If no caller found in routine_stack, fall back to callframe
        if caller_start_idx.is_none() && type_filter.is_none() && skip == 0 {
            if let Some(frame) = self.callframe_value(1, callsite_line) {
                return Ok(frame);
            }
            return Ok(Value::Package(Symbol::intern("Mu")));
        }

        let start_idx = match caller_start_idx {
            Some(idx) => idx,
            None => return Ok(Value::Package(Symbol::intern("Mu"))),
        };

        if let Some(ref filter) = type_filter {
            let is_any = filter == "Any" || filter == "Mu";
            let mut found = 0;
            for i in (0..=start_idx).rev() {
                let frame = &stack[i];
                if frame.is_block {
                    continue;
                }
                let matches = if is_any {
                    true
                } else if filter == "Method" {
                    frame.is_method
                } else if filter == "Sub" || filter == "SubRoutine" {
                    !frame.is_method
                } else {
                    false
                };
                if matches {
                    if found == skip {
                        // Get callsite line from the frame above this one
                        let callsite = self.get_callsite_line_for_frame(i);
                        return Ok(self.build_caller_frame(frame, callsite));
                    }
                    found += 1;
                }
            }
            return Ok(Value::Package(Symbol::intern("Mu")));
        }

        // No type filter: skip N non-block frames from the caller position
        let mut skipped = 0;
        for i in (0..=start_idx).rev() {
            let frame = &stack[i];
            if frame.is_block {
                continue;
            }
            if skipped == skip {
                let callsite = self.get_callsite_line_for_frame(i);
                return Ok(self.build_caller_frame(frame, callsite));
            }
            skipped += 1;
        }
        Ok(Value::Package(Symbol::intern("Mu")))
    }

    /// Get the call-site line for a routine frame at the given index.
    /// This is the line in the frame's code where it called the next frame.
    /// We look at the frame one index above (the callee) for this info.
    fn get_callsite_line_for_frame(&self, frame_idx: usize) -> Option<i64> {
        let stack = &self.routine_stack;
        // Walk up from frame_idx+1 to find the next non-block frame
        for f in stack.iter().skip(frame_idx + 1) {
            if !f.is_block {
                return f.line.map(|l| l as i64);
            }
        }
        // If no callee found, try the topmost frame
        if let Some(f) = stack.get(frame_idx + 1) {
            return f.line.map(|l| l as i64);
        }
        None
    }

    fn build_caller_frame(&self, frame: &RoutineFrame, callsite_line: Option<i64>) -> Value {
        let mut attrs = HashMap::new();
        let pkg = if frame.package.is_empty() || frame.package == "GLOBAL" {
            "Main".to_string()
        } else {
            frame.package.clone()
        };
        let subname = format!("&{}::{}", pkg, frame.name);
        attrs.insert("subname".to_string(), Value::str(subname));
        attrs.insert("package".to_string(), Value::str(pkg));

        let file = frame
            .file
            .clone()
            .or_else(|| self.env.get("?FILE").map(|v| v.to_string_value()))
            .unwrap_or_default();
        attrs.insert("file".to_string(), Value::str(file));

        let line = callsite_line
            .or_else(|| frame.line.map(|l| l as i64))
            .unwrap_or(0);
        attrs.insert("line".to_string(), Value::Int(line));

        let subtype = if frame.is_method {
            "Method"
        } else {
            "SubRoutine"
        };
        attrs.insert("subtype".to_string(), Value::str(subtype.to_string()));

        // Search the block_stack for a Sub matching this frame's name
        let sub_val = self
            .block_stack
            .iter()
            .rev()
            .find(|v| {
                if let Value::Sub(sd) = v {
                    sd.name.resolve() == frame.name
                        && (sd.package.resolve() == frame.package
                            || (frame.package == "GLOBAL" && sd.package.resolve().is_empty())
                            || (sd.package.resolve() == "GLOBAL" && frame.package.is_empty()))
                } else {
                    false
                }
            })
            .cloned()
            .or_else(|| self.env.get(&format!("&{}", frame.name)).cloned())
            .or_else(|| self.env.get(&frame.name).cloned())
            .unwrap_or(Value::Nil);
        attrs.insert("sub".to_string(), sub_val);
        attrs.insert("inline".to_string(), Value::Bool(false));
        attrs.insert("annotations".to_string(), self.build_annotations(&attrs));
        // caller() returns Control::Caller type per spec
        Value::make_instance(Symbol::intern("Control::Caller"), attrs)
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
                    if let Some(Value::Array(items, _)) = attributes.as_map().get("bytes") {
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
                let message = format!("No compiler available for language '{}'", lang);
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("lang".to_string(), Value::str(lang));
                attrs.insert("message".to_string(), Value::str(message.clone()));
                let ex = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Eval::NoSuchLang"),
                    attrs,
                );
                let mut err = RuntimeError::new(message);
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
        }
        if code.contains("&?ROUTINE") && self.routine_stack.is_empty() {
            return Err(RuntimeError::undeclared_symbols("Undeclared name"));
        }
        if let Some(check_val) = Self::named_value(args, "check")
            && check_val.truthy()
        {
            return self.eval_eval_string_check_only(&code);
        }
        self.eval_eval_string(&code)
    }

    pub(super) fn builtin_dd(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let arg_sources = self.pending_call_arg_sources.clone().unwrap_or_default();
        for (i, val) in args.iter().enumerate() {
            let source_name = arg_sources.get(i).and_then(|entry| entry.as_deref());
            let repr = Self::dd_format(val, source_name);
            self.emit_stderr(&format!("{}\n", repr));
        }
        Ok(args.first().cloned().unwrap_or(Value::Nil))
    }

    /// Format a value for `dd` output (Raku-style debug representation).
    ///
    /// The value part is the value's `.raku` representation. When the argument
    /// is a plain variable (e.g. `dd %h`), Raku prefixes it with the runtime
    /// type and the variable name: `Hash %h = {:a(1)}`. Literals and complex
    /// expressions render as just the value.
    fn dd_format(val: &Value, source_name: Option<&str>) -> String {
        let value_repr = crate::builtins::methods_0arg::raku_repr::raku_value(val);
        match source_name {
            Some(name) if Self::dd_is_plain_var(name) => {
                let ty = crate::runtime::utils::value_type_name(val);
                format!("{} {} = {}", ty, name, value_repr)
            }
            _ => value_repr,
        }
    }

    /// A `dd` argument source counts as a named variable only when it is a bare
    /// sigil + identifier (`$x`, `@a`, `%h`, `&c`) — not an index/expression.
    fn dd_is_plain_var(name: &str) -> bool {
        let mut chars = name.chars();
        match chars.next() {
            Some('$') | Some('@') | Some('%') | Some('&') => {}
            _ => return false,
        }
        let rest = &name[1..];
        !rest.is_empty()
            && rest
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == ':')
    }
}
