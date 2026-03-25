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
            "comb" if args.len() == 1 => self.dispatch_comb_1arg(target, &args),
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

    /// Helper for .comb with 1 argument.
    fn dispatch_comb_1arg(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let text = target.to_string_value();
        match &args[0] {
            Value::Int(n) if *n > 0 => {
                let chunk_size = *n as usize;
                let chars: Vec<char> = text.chars().collect();
                let result: Vec<Value> = chars
                    .chunks(chunk_size)
                    .map(|chunk| Value::str(chunk.iter().collect()))
                    .collect();
                Some(Ok(Value::array(result)))
            }
            Value::Str(needle) => {
                if needle.is_empty() {
                    let chars = text
                        .chars()
                        .map(|ch| Value::str(ch.to_string()))
                        .collect::<Vec<_>>();
                    return Some(Ok(Value::array(chars)));
                }
                let mut result = Vec::new();
                let mut offset = 0usize;
                while offset <= text.len() {
                    let Some(pos) = text[offset..].find(needle.as_str()) else {
                        break;
                    };
                    let start = offset + pos;
                    let end = start + needle.len();
                    result.push(Value::str(text[start..end].to_string()));
                    offset = end;
                }
                Some(Ok(Value::array(result)))
            }
            Value::Regex(pat) => {
                let matches = self.regex_find_all(pat, &text);
                let chars: Vec<char> = text.chars().collect();
                let result: Vec<Value> = matches
                    .iter()
                    .map(|(start, end)| {
                        let s: String = chars[*start..*end].iter().collect();
                        Value::str(s)
                    })
                    .collect();
                Some(Ok(Value::array(result)))
            }
            _ => {
                let pattern = args[0].to_string_value();
                let matches = self.regex_find_all(&pattern, &text);
                let chars: Vec<char> = text.chars().collect();
                let result: Vec<Value> = matches
                    .iter()
                    .map(|(start, end)| {
                        let s: String = chars[*start..*end].iter().collect();
                        Value::str(s)
                    })
                    .collect();
                Some(Ok(Value::array(result)))
            }
        }
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
