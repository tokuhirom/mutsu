//! Stream I/O builtins: `print`/`prompt`/`get`/`getc`/`lines`/`words`.
use super::*;

impl Interpreter {
    pub(super) fn builtin_print(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // put and print thread through Junctions: each eigenstate is output individually
        if matches!(name, "put" | "print") {
            let has_junctions = args.iter().any(|a| matches!(a, Value::Junction { .. }));
            if has_junctions {
                let mut flat = Vec::new();
                for arg in args {
                    Self::collect_junction_eigenstates(arg, &mut flat);
                }
                let (handle, newline) = if name == "put" {
                    ("$*OUT", true)
                } else {
                    ("$*OUT", false)
                };
                for v in &flat {
                    let content = self.render_str_value(v);
                    self.write_to_named_handle(handle, &content, newline)?;
                }
                return Ok(Value::Bool(true));
            }
            // No junctions: regular put/print behavior
            let mut content = String::new();
            for arg in args {
                content.push_str(&self.render_str_value(arg));
            }
            let (handle, newline) = if name == "put" {
                ("$*OUT", true)
            } else {
                ("$*OUT", false)
            };
            self.write_to_named_handle(handle, &content, newline)?;
            return Ok(Value::Bool(true));
        }
        let mut content = String::new();
        if args.is_empty() && name == "note" {
            content.push_str("Noted");
        } else if name == "note" || name == "say" {
            // say and note use .gist for rendering
            for arg in args {
                content.push_str(&self.render_gist_value(arg));
            }
        } else {
            for arg in args {
                content.push_str(&self.render_str_value(arg));
            }
        }
        let (handle, newline) = match name {
            "print" => ("$*OUT", false),
            "say" | "put" => ("$*OUT", true),
            _ => ("$*ERR", true),
        };
        self.write_to_named_handle(handle, &content, newline)?;
        Ok(Value::Bool(true))
    }

    /// Collect all non-junction eigenstates from a value, flattening junctions recursively.
    fn collect_junction_eigenstates(v: &Value, out: &mut Vec<Value>) {
        if let Value::Junction { values, .. } = v {
            for elem in values.iter() {
                Self::collect_junction_eigenstates(elem, out);
            }
        } else {
            out.push(v.clone());
        }
    }

    pub(super) fn builtin_prompt(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(first) = args.first() {
            let msg = self
                .call_method_with_values(first.clone(), "Str", vec![])
                .map(|v| v.to_string_value())
                .unwrap_or_else(|_| first.to_string_value());
            self.write_to_named_handle("$*OUT", &msg, false)?;
        }
        let handle = self
            .get_dynamic_handle("$*IN")
            .or_else(|| self.default_input_handle());
        if let Some(handle) = handle {
            let line = self
                .read_line_from_handle_value(&handle)?
                .unwrap_or_default();
            return Ok(Value::str(line));
        }
        Ok(Value::str(String::new()))
    }

    pub(super) fn builtin_get(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let handle = args
            .first()
            .cloned()
            .or_else(|| self.default_input_handle());
        if let Some(handle) = handle {
            return Ok(self
                .read_line_from_handle_value(&handle)?
                .map(Value::str)
                .unwrap_or(Value::Nil));
        }
        Ok(Value::Nil)
    }

    pub(super) fn builtin_getc(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let handle = args
            .first()
            .cloned()
            .or_else(|| self.default_input_handle());
        if let Some(handle) = handle {
            let s = self.read_chars_from_handle_value(&handle, Some(1))?;
            if s.is_empty() {
                return Ok(Value::Nil);
            }
            return Ok(Value::str(s));
        }
        Ok(Value::Nil)
    }

    pub(super) fn builtin_lines(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(first) = args.first()
            && Self::handle_id_from_value(first).is_none()
        {
            let text = first.to_string_value();
            let mut limit: Option<usize> = None;
            let mut chomp = true;
            for arg in &args[1..] {
                match arg {
                    Value::Pair(key, value) if key == "chomp" => {
                        chomp = value.truthy();
                    }
                    Value::Int(i) => {
                        limit = Some((*i).max(0) as usize);
                    }
                    Value::BigInt(bi) => {
                        use num_traits::ToPrimitive;
                        limit = Some(bi.to_usize().unwrap_or(usize::MAX));
                    }
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                        limit = None;
                    }
                    Value::Num(f) if *f >= 0.0 => {
                        limit = Some(*f as usize);
                    }
                    Value::Rat(n, d) if *d == 0 && *n > 0 => {
                        limit = None;
                    }
                    _ => {}
                }
            }
            let mut lines = crate::builtins::split_lines_with_chomp(&text, chomp);
            if let Some(n) = limit {
                lines.truncate(n);
            }
            let values: Vec<Value> = lines.into_iter().map(Value::str).collect();
            // `lines` returns a Seq (so `.^name` is Seq), matching Rakudo and
            // the `Str.lines` method form.
            return Ok(Value::Seq(std::sync::Arc::new(values)));
        }

        let handle = args
            .first()
            .cloned()
            .or_else(|| self.default_input_handle());
        if let Some(handle) = handle {
            let mut limit: Option<usize> = None;
            let mut close_after = false;
            let extra_args = if args.len() > 1 { &args[1..] } else { &[] };
            for arg in extra_args {
                match arg {
                    Value::Pair(k, v) if k == "close" => {
                        close_after = v.truthy();
                    }
                    Value::Pair(..) => {}
                    Value::Int(i) => limit = Some((*i).max(0) as usize),
                    Value::BigInt(bi) => {
                        use num_traits::ToPrimitive;
                        limit = Some(bi.to_usize().unwrap_or(usize::MAX));
                    }
                    Value::Whatever => {}
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {}
                    Value::Num(f) if *f >= 0.0 => limit = Some(*f as usize),
                    Value::Rat(n, d) if *d == 0 && *n > 0 => {}
                    _ => {}
                }
            }
            if limit.is_none() {
                // No limit: return a lazy IO lines iterator so that
                // consumers (e.g. for-loop) can read on demand.
                // This allows `last` in `-ne` mode to exit without
                // waiting for stdin EOF.
                return Ok(Value::LazyIoLines {
                    handle: Box::new(handle),
                    kv: false,
                    words: false,
                });
            }
            let mut lines = Vec::new();
            while let Some(line) = self.read_line_from_handle_value(&handle)? {
                lines.push(Value::str(line));
                if let Some(n) = limit
                    && lines.len() >= n
                {
                    break;
                }
            }
            if close_after {
                self.close_handle_value(&handle)?;
            }
            return Ok(Value::Seq(std::sync::Arc::new(lines)));
        }
        Ok(Value::array(Vec::new()))
    }

    pub(super) fn builtin_words(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let handle = if args.is_empty() {
            self.default_input_handle()
        } else if args.first().and_then(Self::handle_id_from_value).is_some() {
            args.first().cloned()
        } else {
            None
        };
        if let Some(handle) = handle {
            let mut limit: Option<usize> = None;
            let mut close_after = false;
            for arg in args.get(1..).unwrap_or(&[]) {
                match arg {
                    Value::Pair(k, v) if k == "close" => {
                        close_after = v.truthy();
                    }
                    Value::Pair(..) => {}
                    Value::Int(i) => limit = Some((*i).max(0) as usize),
                    Value::BigInt(bi) => {
                        use num_traits::ToPrimitive;
                        limit = Some(bi.to_usize().unwrap_or(usize::MAX));
                    }
                    Value::Whatever => {}
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {}
                    Value::Num(f) if *f >= 0.0 => limit = Some(*f as usize),
                    _ => {}
                }
            }
            if limit.is_none() {
                // No limit: return a lazy word iterator so a partial consumer
                // (e.g. `words($fh, :close)[1,2]`) leaves the handle open, while a
                // full consumer triggers close-on-exhaust when `:close` was given.
                if close_after {
                    self.with_handle_mut(&handle, |state| {
                        state.close_on_word_exhaust = true;
                        Ok(())
                    })?;
                }
                return Ok(Value::LazyIoLines {
                    handle: Box::new(handle),
                    kv: false,
                    words: true,
                });
            }
            let mut words = Vec::new();
            'outer: while let Some(word) = self.read_word_from_handle_value(&handle)? {
                words.push(Value::str(word));
                if let Some(n) = limit
                    && words.len() >= n
                {
                    break 'outer;
                }
            }
            if close_after {
                self.close_handle_value(&handle)?;
            }
            return Ok(Value::Seq(std::sync::Arc::new(words)));
        }
        // Non-handle argument: delegate to string-splitting words (native function)
        if !args.is_empty()
            && let Some(result) =
                crate::builtins::native_function(crate::symbol::Symbol::intern("words"), args)
        {
            return result;
        }
        Ok(Value::array(Vec::new()))
    }
}
