use super::*;

impl Interpreter {
    /// Dispatch methods by name - first group (string, IO, coercion, misc).
    /// Returns Some(result) if the method was handled, None to fall through.
    #[allow(clippy::too_many_lines)]
    pub(super) fn dispatch_method_by_name_1(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            "are" => Some(self.dispatch_are(target, &args)),
            "classify" | "categorize" if !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply") =>
            {
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.extend(args.iter().cloned());
                call_args.push(target);
                Some(self.builtin_classify(method, &call_args))
            }
            "classify-list" | "categorize-list" => {
                // Immutable Bag and Mix cannot be classified into
                let type_name = match &target {
                    Value::Bag(_, false) => Some("Bag"),
                    Value::Mix(_, false) => Some("Mix"),
                    _ => None,
                };
                if let Some(tname) = type_name {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("typename".to_string(), Value::str_from(tname));
                    attrs.insert("method".to_string(), Value::str_from(method));
                    let exception = Value::make_instance(Symbol::intern("X::Immutable"), attrs);
                    let mut err = RuntimeError::new(format!(
                        "Cannot call '{}' on an immutable '{}'",
                        method, tname
                    ));
                    err.exception = Some(Box::new(exception));
                    return Some(Err(err));
                }
                let classify_name = if method == "classify-list" {
                    "classify"
                } else {
                    "categorize"
                };
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.extend(args.iter().cloned());
                call_args.push(Value::Pair("into".to_string(), Box::new(target)));
                Some(self.builtin_classify(classify_name, &call_args))
            }
            "from-loop" | "from_loop" if matches!(&target, Value::Package(name) if name == "Seq") => {
                Some(self.dispatch_seq_from_loop(args))
            }
            "say" if args.is_empty() => Some(self.dispatch_say(&target)),
            "print" if args.is_empty() => Some(self.dispatch_print(&target)),
            "put" if args.is_empty() => Some(self.dispatch_put(&target)),
            "printf" if args.is_empty() => Some(self.dispatch_printf(&target)),
            "sprintf" if args.is_empty() => Some(self.dispatch_sprintf(&target)),
            "shape" if args.is_empty() => self.dispatch_shape(&target),
            "default" if args.is_empty() => Self::dispatch_default(&target),
            "note" if args.is_empty() => Some(self.dispatch_note(&target)),
            "return-rw" if args.is_empty() => Some(Ok(target)),
            "encode" if !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply") => {
                Some(self.dispatch_encode(&target, &args))
            }
            "decode" => self.dispatch_decode(&target, &args),
            "subbuf" => self.dispatch_subbuf(&target, &args),
            "polymod" => Some(self.method_polymod(&target, &args)),
            "VAR" if args.is_empty() => {
                // Proxy .VAR returns a decontainerized copy
                if matches!(&target, Value::Proxy { .. }) {
                    return Some(Ok(Value::proxy_var_object(target, String::new())));
                }
                Some(Ok(target))
            }
            "can" if args.len() == 1 => {
                let method_name = args[0].to_string_value();
                let results = self.collect_can_methods(&target, &method_name);
                Some(Ok(Value::array(results)))
            }
            "does" if args.len() == 1 => {
                let type_name = match &args[0] {
                    Value::Package(name) => name.resolve(),
                    Value::Str(name) => name.to_string(),
                    Value::Instance { class_name, .. } => class_name.resolve(),
                    other => other.to_string_value(),
                };
                Some(Ok(Value::Bool(
                    self.type_matches_value(&type_name, &target),
                )))
            }
            "start" => self.dispatch_promise_start(&target, &args),
            "in" => self.dispatch_promise_in(&target, &args),
            "THREAD" => {
                if let Value::Junction { values, .. } = &target {
                    let code = args.first().cloned().unwrap_or(Value::Nil);
                    for value in values.iter() {
                        match self.call_sub_value(code.clone(), vec![value.clone()], false) {
                            Ok(_) => {}
                            Err(e) => return Some(Err(e)),
                        }
                    }
                    return Some(Ok(Value::Nil));
                }
                None
            }
            "at" => self.dispatch_promise_at(&target, &args),
            "kept" => self.dispatch_promise_kept(&target, &args),
            "broken" => self.dispatch_promise_broken(&target, &args),
            "allof" => self.dispatch_promise_allof(&target, &args),
            "anyof" => self.dispatch_promise_anyof(&target, &args),
            "WHAT" if args.is_empty() => Some(self.dispatch_what(&target, args)),
            "HOW" => Some(self.dispatch_how(&target, &args)),
            "WHO" if args.is_empty() => Some(self.dispatch_who(&target)),
            "WHY" if args.is_empty() => Some(self.dispatch_why(&target)),
            "^name" if args.is_empty() => Some(self.dispatch_caret_name(&target)),
            "^enum_value_list" | "enum_value_list" => self.dispatch_enum_value_list(&target),
            "enums" => self.dispatch_enums(&target),
            "invert" => self.dispatch_invert_enum(&target),
            "subparse" | "parse" | "parsefile" => {
                if let Value::Package(ref package_name) = target {
                    Some(self.dispatch_package_parse(&package_name.resolve(), method, &args))
                } else {
                    None
                }
            }
            "match" => Some(self.dispatch_match_method(target, &args)),
            "subst" => Some(self.dispatch_subst(target, &args)),
            "comb" if !args.is_empty() => {
                if matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply") {
                    Some(self.dispatch_supply_transform(target, "comb", &args))
                } else {
                    self.dispatch_comb_with_args(target, &args)
                }
            }
            "IO" if args.is_empty() => {
                let s = target.to_string_value();
                if s.contains('\0') {
                    return Some(Err(RuntimeError::new(
                        "X::IO::Null: Found null byte in pathname",
                    )));
                }
                Some(Ok(self.make_io_path_instance(&s)))
            }
            "contains" => Some(self.dispatch_contains(target, &args)),
            "starts-with" => Some(self.dispatch_starts_with(target, &args)),
            "ends-with" => Some(self.dispatch_ends_with(target, &args)),
            "index" => Some(self.dispatch_index(target, &args)),
            "rindex" => Some(self.dispatch_rindex(target, &args)),
            "substr-eq" => Some(self.dispatch_substr_eq(target, &args)),
            "substr" => Some(self.dispatch_substr(target, &args)),
            "substr-rw" => Some(self.dispatch_substr_rw(target, &args)),
            "trans" => Some(self.dispatch_trans(target, &args)),
            _ => None,
        }
    }

    /// Helper for .comb with arguments.
    /// Handles: .comb($matcher), .comb($matcher, $limit), .comb($matcher, :match)
    fn dispatch_comb_with_args(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        use unicode_segmentation::UnicodeSegmentation;

        let text = target.to_string_value();

        // Separate positional args from named :match pair
        let mut positional: Vec<&Value> = Vec::new();
        let mut return_match = false;
        for arg in args {
            if let Value::Pair(key, val) = arg
                && key == "match"
            {
                return_match = val.truthy();
                continue;
            }
            positional.push(arg);
        }

        // Extract limit from second positional arg (default: unlimited)
        let limit: Option<i64> = if positional.len() >= 2 {
            Some(positional[1].to_f64() as i64)
        } else {
            None
        };

        // If limit is 0 or negative, return empty
        if let Some(lim) = limit
            && lim <= 0
        {
            return Some(Ok(Value::Seq(std::sync::Arc::new(Vec::new()))));
        }

        let matcher = positional.first().copied();

        let make_seq = |items: Vec<Value>| Value::Seq(std::sync::Arc::new(items));

        match matcher {
            Some(Value::Int(n)) => {
                let chunk_size = if *n <= 0 { 1usize } else { *n as usize };
                let graphemes: Vec<&str> = text.graphemes(true).collect();
                let result: Vec<Value> = graphemes
                    .chunks(chunk_size)
                    .map(|chunk| Value::str(chunk.concat()))
                    .collect();
                let result = Self::apply_limit(result, limit);
                Some(Ok(make_seq(result)))
            }
            Some(Value::Str(needle)) => {
                if needle.is_empty() {
                    let chars: Vec<Value> = text
                        .graphemes(true)
                        .map(|g| Value::str(g.to_string()))
                        .collect();
                    let chars = Self::apply_limit(chars, limit);
                    return Some(Ok(make_seq(chars)));
                }
                let mut result = Vec::new();
                let mut offset = 0usize;
                while offset <= text.len() {
                    if let Some(lim) = limit
                        && result.len() >= lim as usize
                    {
                        break;
                    }
                    let Some(pos) = text[offset..].find(needle.as_str()) else {
                        break;
                    };
                    let start = offset + pos;
                    let end = start + needle.len();
                    result.push(Value::str(text[start..end].to_string()));
                    offset = end;
                }
                Some(Ok(make_seq(result)))
            }
            Some(Value::Regex(pat)) => {
                // Use the capturing path only when the regex contains code
                // blocks that need eager execution (e.g. `{ take $/.Str }`).
                // For regular regexes, use the faster non-capturing path.
                let has_code = self.has_code_block_in_prefix(pat);
                if has_code {
                    self.enable_eager_code_blocks();
                    let matches = self.regex_find_all_with_caps(pat, &text);
                    let eager_blocks = self.drain_eager_code_blocks();
                    if !eager_blocks.is_empty() {
                        self.execute_regex_code_blocks(&eager_blocks);
                    } else {
                        for (_, _, caps) in &matches {
                            if !caps.code_blocks.is_empty() {
                                self.execute_regex_code_blocks(&caps.code_blocks);
                            }
                        }
                    }
                    if return_match {
                        let result: Vec<Value> = matches
                            .iter()
                            .map(|(start, end, _)| {
                                self.create_match_object(&text, *start, *end, pat)
                            })
                            .collect();
                        let result = Self::apply_limit(result, limit);
                        Some(Ok(make_seq(result)))
                    } else {
                        let chars: Vec<char> = text.chars().collect();
                        let result: Vec<Value> = matches
                            .iter()
                            .map(|(start, end, _)| {
                                let s: String = chars[*start..*end].iter().collect();
                                Value::str(s)
                            })
                            .collect();
                        let result = Self::apply_limit(result, limit);
                        Some(Ok(make_seq(result)))
                    }
                } else {
                    let matches = self.regex_find_all(pat, &text);
                    if return_match {
                        let result: Vec<Value> = matches
                            .iter()
                            .map(|(start, end)| self.create_match_object(&text, *start, *end, pat))
                            .collect();
                        let result = Self::apply_limit(result, limit);
                        Some(Ok(make_seq(result)))
                    } else {
                        let chars: Vec<char> = text.chars().collect();
                        let result: Vec<Value> = matches
                            .iter()
                            .map(|(start, end)| {
                                let s: String = chars[*start..*end].iter().collect();
                                Value::str(s)
                            })
                            .collect();
                        let result = Self::apply_limit(result, limit);
                        Some(Ok(make_seq(result)))
                    }
                }
            }
            Some(Value::Sub(_)) | Some(Value::WeakSub(_)) => Some(Err(RuntimeError::new(
                "none of these signatures match: comb does not accept a Code argument",
            ))),
            _ => {
                if let Some(m) = matcher {
                    let pattern = m.to_string_value();
                    let matches = self.regex_find_all(&pattern, &text);
                    let chars: Vec<char> = text.chars().collect();
                    let result: Vec<Value> = matches
                        .iter()
                        .map(|(start, end)| {
                            let s: String = chars[*start..*end].iter().collect();
                            Value::str(s)
                        })
                        .collect();
                    let result = Self::apply_limit(result, limit);
                    Some(Ok(make_seq(result)))
                } else {
                    None
                }
            }
        }
    }

    /// Apply a limit to a result vector.
    fn apply_limit(result: Vec<Value>, limit: Option<i64>) -> Vec<Value> {
        if let Some(lim) = limit {
            if lim <= 0 {
                Vec::new()
            } else {
                result.into_iter().take(lim as usize).collect()
            }
        } else {
            result
        }
    }

    /// Create a Match object from regex match positions.
    fn create_match_object(&self, text: &str, start: usize, end: usize, _pat: &str) -> Value {
        let chars: Vec<char> = text.chars().collect();
        let matched: String = chars[start..end].iter().collect();
        Value::make_match_object_full(
            matched,
            start as i64,
            end as i64,
            &[],
            &std::collections::HashMap::new(),
            &std::collections::HashMap::new(),
            &[],
            &[],
            Some(text),
        )
    }

    /// Dispatch trig methods on Instance values via Numeric/Bridge coercion.
    pub(super) fn dispatch_trig_instance_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "sin"
                | "cos"
                | "tan"
                | "asin"
                | "acos"
                | "atan"
                | "atan2"
                | "sec"
                | "cosec"
                | "cotan"
                | "asec"
                | "acosec"
                | "acotan"
                | "sinh"
                | "cosh"
                | "tanh"
                | "sech"
                | "cosech"
                | "cotanh"
                | "asinh"
                | "acosh"
                | "atanh"
                | "asech"
                | "acosech"
                | "acotanh"
        ) {
            return None;
        }

        if matches!(&target, Value::Instance { .. }) {
            let coerced = if let Ok(v) =
                self.call_method_with_values(target.clone(), "Numeric", vec![])
            {
                v
            } else if let Ok(v) = self.call_method_with_values(target.clone(), "Bridge", vec![]) {
                v
            } else {
                return Some(Err(RuntimeError::new(format!(
                    "Cannot coerce to numeric for {}",
                    method
                ))));
            };
            return Some(self.call_method_with_values(coerced, method, args));
        }

        // .atan2(Instance) — coerce Instance arg
        if method == "atan2" && args.len() == 1 && matches!(&args[0], Value::Instance { .. }) {
            let coerced_arg = match self
                .call_method_with_values(args[0].clone(), "Numeric", vec![])
                .or_else(|_| self.call_method_with_values(args[0].clone(), "Bridge", vec![]))
            {
                Ok(v) => v,
                Err(e) => return Some(Err(e)),
            };
            return Some(self.call_method_with_values(target, "atan2", vec![coerced_arg]));
        }

        None
    }
}
