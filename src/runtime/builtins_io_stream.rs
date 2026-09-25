//! Stream I/O builtins: `print`/`prompt`/`get`/`getc`/`lines`/`words`.
use super::*;

impl Interpreter {
    /// `lines`/`words` in *sub* form take an `IO()`-coercible positional exactly
    /// like `slurp` does, so an `IO::Path` argument must open and read the file
    /// rather than being stringified into a one-element list
    /// (raku: `lines($path)` is `$path.lines`). Delegating to
    /// `try_io_path_content_read` — the single implementation the `.lines`/`.words`
    /// *method* forms already use — keeps the two spellings in agreement instead
    /// of growing a second read+split path here.
    fn try_io_path_content_sub(
        &mut self,
        args: &[Value],
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        let first = args
            .iter()
            .find(|a| !matches!(a.view(), ValueView::Pair(..)))?;
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = first.view()
        else {
            return None;
        };
        if !Self::is_io_path_lexical_class(&class_name.resolve()) {
            return None;
        }
        let rest: Vec<Value> = args
            .iter()
            .filter(|a| !std::ptr::eq(*a, first))
            .cloned()
            .collect();
        self.try_io_path_content_read(&attributes.to_map(), method, &rest)
    }

    /// The routine form of `say`/`put`/`print`/`note` (`&say(...)`, an alias
    /// `my &s = &say`, ...): the same renderer the opcodes use.
    // Cost: as `render_output`.
    pub(super) fn builtin_print(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let kind = crate::vm::OutputKind::from_name(name)
            .ok_or_else(|| RuntimeError::new(format!("not an output routine: {name}")))?;
        self.render_output(kind, args.to_vec())?;
        Ok(Value::TRUE)
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
            // At end of input `prompt` returns the `Any` type object (like
            // `get`), not an empty string: `prompt(...).defined` is False.
            return Ok(match self.read_line_from_handle_value(&handle)? {
                Some(line) => Value::str(line),
                None => Value::package(crate::symbol::wk::any()),
            });
        }
        Ok(Value::package(crate::symbol::wk::any()))
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
                .unwrap_or(Value::NIL));
        }
        Ok(Value::NIL)
    }

    pub(super) fn builtin_getc(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let handle = args
            .first()
            .cloned()
            .or_else(|| self.default_input_handle());
        if let Some(handle) = handle {
            // Read one grapheme cluster (base + extending codepoints), matching
            // `.chars`/`.comb`; seekable files use the grapheme path, other
            // targets fall back to a single codepoint.
            if let Some(g) = self.read_grapheme_from_handle_value(&handle, 1)? {
                return Ok(if g.is_empty() {
                    Value::NIL
                } else {
                    Value::str(g)
                });
            }
            let s = self.read_chars_from_handle_value(&handle, Some(1))?;
            if s.is_empty() {
                return Ok(Value::NIL);
            }
            return Ok(Value::str(s));
        }
        Ok(Value::NIL)
    }

    pub(super) fn builtin_lines(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(result) = self.try_io_path_content_sub(args, "lines") {
            return result;
        }
        // Named args (`:chomp`, `:count`) may appear before or after the string
        // argument (`lines(:!chomp, "a\nb")`), so partition them out first and
        // treat the first *positional* argument as the string/handle.
        let mut chomp = true;
        let mut count_only = false;
        let mut positional: Vec<&Value> = Vec::new();
        for arg in args {
            match arg.view() {
                ValueView::Pair(key, value) if key == "chomp" => chomp = value.truthy(),
                ValueView::Pair(key, value) if key == "count" => count_only = value.truthy(),
                _ => positional.push(arg),
            }
        }
        if let Some(first) = positional.first()
            && Self::handle_id_from_value(first).is_none()
        {
            let mut limit: Option<usize> = None;
            for arg in &positional[1..] {
                if let Some(parsed) = crate::value::str_iter_limit(arg) {
                    limit = parsed;
                }
            }
            let mode = crate::value::StrIterMode::Lines { chomp };
            // `lines(:count)` returns the number of lines instead of the list.
            if count_only {
                let n = crate::value::str_iter_count(first, mode, limit);
                return Ok(Value::int(n as i64));
            }
            // `lines` returns the same Seq as the `Str.lines` method form (so
            // `.^name` is Seq and `.head(3)` reads only a prefix).
            return Ok(crate::value::str_iter_seq(first, mode, limit));
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
                match arg.view() {
                    ValueView::Pair(k, v) if k == "close" => {
                        close_after = v.truthy();
                    }
                    ValueView::Pair(..) => {}
                    ValueView::Int(i) => limit = Some(i.max(0) as usize),
                    ValueView::BigInt(bi) => {
                        use num_traits::ToPrimitive;
                        limit = Some(bi.to_usize().unwrap_or(usize::MAX));
                    }
                    ValueView::Whatever => {}
                    ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => {}
                    ValueView::Num(f) if f >= 0.0 => limit = Some(f as usize),
                    ValueView::Rat(n, d) if d == 0 && n > 0 => {}
                    _ => {}
                }
            }
            if limit.is_none() {
                // No limit: return a lazy IO lines iterator so that
                // consumers (e.g. for-loop) can read on demand.
                // This allows `last` in `-ne` mode to exit without
                // waiting for stdin EOF.
                return Ok(Value::lazy_io_lines(handle, false, false));
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
            return Ok(Value::seq(lines));
        }
        Ok(Value::array(Vec::new()))
    }

    pub(super) fn builtin_words(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(result) = self.try_io_path_content_sub(args, "words") {
            return result;
        }
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
                match arg.view() {
                    ValueView::Pair(k, v) if k == "close" => {
                        close_after = v.truthy();
                    }
                    ValueView::Pair(..) => {}
                    ValueView::Int(i) => limit = Some(i.max(0) as usize),
                    ValueView::BigInt(bi) => {
                        use num_traits::ToPrimitive;
                        limit = Some(bi.to_usize().unwrap_or(usize::MAX));
                    }
                    ValueView::Whatever => {}
                    ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => {}
                    ValueView::Num(f) if f >= 0.0 => limit = Some(f as usize),
                    _ => {}
                }
            }
            if limit.is_none() {
                // No limit: return a lazy word iterator so a partial consumer
                // (e.g. `words($fh, :close)[1,2]`) leaves the handle open, while a
                // full consumer triggers close-on-exhaust when `:close` was given.
                if close_after {
                    self.with_handle_mut(&handle, |state| {
                        state.close_on_exhaust = true;
                        Ok(())
                    })?;
                }
                return Ok(Value::lazy_io_lines(handle, false, true));
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
            return Ok(Value::seq(words));
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
