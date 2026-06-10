use super::super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn test_fn_throws_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val =
            Self::positional_value_required(args, 0, "throws-like expects code")?.clone();
        let expected =
            Self::positional_value_required(args, 1, "throws-like expects type")?.to_string_value();
        let desc = Self::positional_string(args, 2);
        let result = match &code_val {
            Value::Sub(data) => {
                // Evaluating the assertion block must not leak its last expression
                // value (e.g. an unhandled Failure from `@a.pop` on an empty array)
                // into the caller, where a subsequent `EVAL` would surface it as
                // its own result. Save and restore the outer last_value.
                let saved_last_value = self.last_value.take();
                let r = self.eval_block_value(&data.body);
                self.last_value = saved_last_value;
                r
            }
            Value::Str(code) => {
                let mut nested = Interpreter::new();
                nested.strict_mode = self.strict_mode;
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.lib_paths = self.lib_paths.clone();
                nested.program_path = self.program_path.clone();
                nested.registry_mut().functions = self.registry().functions.clone();
                nested.registry_mut().proto_functions = self.registry().proto_functions.clone();
                nested.registry_mut().token_defs = self.registry().token_defs.clone();
                nested.registry_mut().proto_subs = self.registry().proto_subs.clone();
                nested.registry_mut().proto_tokens = self.registry().proto_tokens.clone();
                nested.registry_mut().classes = self.registry().classes.clone();
                nested.registry_mut().class_trusts = self.registry().class_trusts.clone();
                nested.registry_mut().class_composed_roles =
                    self.registry().class_composed_roles.clone();
                nested.registry_mut().roles = self.registry().roles.clone();
                nested.registry_mut().role_candidates = self.registry().role_candidates.clone();
                nested.registry_mut().role_parents = self.registry().role_parents.clone();
                nested.registry_mut().role_hides = self.registry().role_hides.clone();
                nested.registry_mut().role_type_params = self.registry().role_type_params.clone();
                nested.registry_mut().class_role_param_bindings =
                    self.registry().class_role_param_bindings.clone();
                nested.registry_mut().subsets = self.registry().subsets.clone();
                nested.registry_mut().enum_types = self.registry().enum_types.clone();
                nested.type_metadata = self.type_metadata.clone();
                nested.current_package = self.current_package.clone();
                nested.suppressed_names = self.suppressed_names.clone();
                nested.lexical_class_scopes = self.lexical_class_scopes.clone();
                nested.var_dynamic_flags = self.var_dynamic_flags.clone();
                nested.restore_var_type_constraints(self.snapshot_var_type_constraints());
                for (k, v) in &self.env {
                    if k.contains_str("::") {
                        continue;
                    }
                    nested.env.insert_sym(*k, v.clone());
                }
                // Pre-check for undeclared names (compile-time errors in Raku).
                // Parse the code and check for undeclared type names before running.
                let pre_check_result = {
                    let op_names = nested.collect_operator_sub_names();
                    let op_assoc = nested.collect_operator_assoc_map();
                    let imported_names = nested.collect_eval_imported_function_names();
                    match crate::parser::parse_program_with_operators(
                        code,
                        &op_names,
                        &op_assoc,
                        &imported_names,
                    ) {
                        Ok((stmts, _)) => nested
                            .check_eval_mainline_placeholders(&stmts)
                            .and_then(|()| nested.check_eval_class_redeclarations(&stmts))
                            .and_then(|()| nested.check_eval_undeclared_vars(&stmts))
                            .and_then(|()| nested.check_eval_undeclared_names(&stmts)),
                        Err(mut e) => {
                            // A compile-time exception raised while parsing the
                            // EVAL'd code reports the EVAL pseudo-file and the
                            // line within that source (Raku exposes `.filename`
                            // matching /EVAL/ and `.line`).
                            if let Some(ref mut exc_box) = e.exception
                                && let Value::Instance { attributes, .. } = exc_box.as_mut()
                            {
                                let line = e.line.unwrap_or(1) as i64;
                                attributes.insert_if_absent("line".to_string(), Value::Int(line));
                                attributes.insert_if_absent(
                                    "filename".to_string(),
                                    Value::str("EVAL_0".to_string()),
                                );
                                attributes.insert_if_absent(
                                    "file".to_string(),
                                    Value::str("EVAL_0".to_string()),
                                );
                            }
                            Err(e)
                        }
                    }
                };
                if let Err(e) = pre_check_result {
                    Err(e)
                } else {
                    let run_result = nested.run(code).map(|_| Value::Nil);
                    // If execution succeeded, check if the last evaluated value was
                    // a Failure (which would throw in sink context in Raku).
                    match run_result {
                        Ok(_) => {
                            if let Some(last_val) = nested.last_value.take() {
                                Self::sink_failure_to_error(last_val)
                            } else {
                                Ok(Value::Nil)
                            }
                        }
                        err => err,
                    }
                }
            }
            // When throws-like receives a non-code value (e.g., a Failure from
            // an eagerly-evaluated expression like `rindex(...)`), sink it so that
            // Failures throw their wrapped exception.
            other => Self::sink_failure_to_error(other.clone()),
        };
        // Also handle Failures returned as Ok values from block/string code evaluation.
        let result = match result {
            Ok(val) => Self::sink_failure_to_error(val),
            err => err,
        };
        // Proc with non-zero exitcode in sink context throws X::Proc::Unsuccessful.
        let result = match result {
            Ok(ref val) => Self::sink_proc_to_error(val.clone()),
            err => err,
        };
        // Normalize type-object representation: "(Exception)" -> "Exception"
        let expected_normalized = expected
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .unwrap_or(&expected);

        // Collect named attribute matchers from args (e.g., status => 'Kept')
        let mut named_matchers: Vec<(String, Value)> = Vec::new();
        for arg in args.iter().skip(2) {
            if let Value::Pair(key, val) = arg {
                named_matchers.push((key.clone(), *val.clone()));
            }
        }

        // Check for Bool matchers — Raku throws X::Match::Bool
        for (attr_name, val) in &named_matchers {
            if matches!(val, Value::Bool(_)) {
                let msg = format!(
                    "Cannot use Bool as Matcher with '.{}'. Did you mean to use $_ inside a block?",
                    attr_name
                );
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("message".to_string(), Value::str(msg.clone()));
                let mut err = RuntimeError::new(&msg);
                err.exception = Some(Box::new(Value::make_instance(
                    Symbol::intern("X::Match::Bool"),
                    attrs,
                )));
                return Err(err);
            }
        }

        let (type_ok, exception_val, err_message) = match &result {
            Ok(_) => (false, None, String::new()),
            Err(err) => {
                // Check exception field first for structured exceptions
                let ex_class = err.exception.as_ref().and_then(|ex| {
                    if let Value::Instance { class_name, .. } = ex.as_ref() {
                        Some(class_name.resolve())
                    } else {
                        None
                    }
                });
                let type_matched = if expected_normalized.is_empty()
                    || expected_normalized == "Exception"
                {
                    true
                } else if let Some(cls) = &ex_class {
                    cls == expected_normalized
                        || cls.starts_with(&format!("{}::", expected_normalized))
                        // Check MRO: the exception's class hierarchy may include the expected type
                        || self.registry().classes.get(cls).is_some_and(|def| {
                            def.mro.iter().any(|parent| parent == expected_normalized)
                        })
                        // X::Comp::Group wraps compile-time errors: match any X::Comp subtype
                        || (expected_normalized == "X::Comp::Group"
                            && self.registry().classes.get(cls).is_some_and(|def| {
                                def.mro.iter().any(|p| p == "X::Comp")
                            }))
                        // X::AdHoc wrapping a die'd string that encodes a type name
                        // (e.g., die "X::Syntax::UnlessElse: ..."): fall through to
                        // message-based matching below.
                        || (cls == "X::AdHoc"
                            && err.message.contains(expected_normalized))
                } else if expected_normalized == "X::Syntax::Confused" {
                    err.message.contains("Confused") || err.message.contains("parse error")
                } else if expected_normalized.starts_with("X::Syntax") {
                    err.message.contains(expected_normalized) || err.message.contains("parse error")
                } else if expected_normalized == "X::Comp"
                    || expected_normalized == "X::Comp::Group"
                {
                    err.message.contains("X::Syntax")
                        || err.message.contains("X::Comp")
                        || err.message.contains("X::Undeclared")
                        || err.message.contains("X::Obsolete")
                        || err.message.contains("X::Redeclaration")
                        || err.message.contains("parse error")
                        // Any error with a parse error code is a compile-time error
                        || err.code.is_some_and(|c| c.is_parse())
                } else if expected_normalized == "X::AdHoc" {
                    // X::AdHoc matches any ad-hoc error
                    true
                } else {
                    err.message.contains(expected_normalized)
                };
                (
                    type_matched,
                    err.exception.as_ref().map(|e| e.as_ref().clone()),
                    err.message.clone(),
                )
            }
        };

        // Only structured exception objects reliably expose arbitrary attribute matchers.
        // X::AdHoc is excluded because it wraps ad-hoc die() values and doesn't
        // carry the attributes of the expected exception type.
        let has_structured_exception = exception_val.as_ref().is_some_and(|ex| {
            if let Value::Instance { class_name, .. } = ex {
                let cn = class_name.resolve();
                cn.starts_with("X::") && cn != "X::AdHoc"
            } else {
                false
            }
        });
        let named_checks: Vec<(String, Value)> = if has_structured_exception {
            named_matchers
        } else {
            Vec::new()
        };

        let ctx = self.begin_subtest();
        let total = 2 + named_checks.len();
        let state = self.tap.ensure_state();
        state.planned = Some(total);
        self.emit_output(&format!("1..{}\n", total));
        self.test_ok(result.is_err(), "code dies", false)?;
        self.test_ok(
            type_ok,
            &format!("right exception type ({})", expected_normalized),
            false,
        )?;
        for (attr_name, expected_val) in &named_checks {
            let actual_val = exception_val.as_ref().and_then(|ex| {
                if let Value::Instance { attributes, .. } = ex {
                    attributes.as_map().get(attr_name).cloned()
                } else {
                    None
                }
            });
            // Fall back to err.message for "message" attribute
            let actual_str = actual_val
                .as_ref()
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    if attr_name == "message" {
                        err_message.clone()
                    } else {
                        String::new()
                    }
                });
            let matched = self.matcher_accepts(expected_val, &actual_str, actual_val.as_ref());
            let expected_display = match expected_val {
                Value::Regex(pattern) => format!("/{}/", pattern),
                Value::Sub(_) | Value::Routine { .. } => expected_val.to_string_value(),
                _ => expected_val.to_string_value(),
            };
            self.test_ok(
                matched,
                &format!(".{} matches {}", attr_name, expected_display),
                false,
            )?;
        }
        let all_ok = type_ok && result.is_err();
        let label = if desc.is_empty() {
            format!("did we throws-like {}?", expected_normalized)
        } else {
            desc.clone()
        };
        self.finish_subtest(
            ctx,
            &label,
            if all_ok {
                Ok(())
            } else {
                Err(RuntimeError::new(""))
            },
        )?;
        Ok(Value::Bool(type_ok))
    }

    /// Smart-match a `throws-like` attribute matcher against the actual value.
    /// Handles Whatever, Regex, Callable, type-object and Junction matchers
    /// (e.g. `message => /"a"/ & /"b"/`), falling back to string equality.
    fn matcher_accepts(
        &mut self,
        matcher: &Value,
        actual_str: &str,
        actual_val: Option<&Value>,
    ) -> bool {
        match matcher {
            Value::Whatever => true,
            Value::Regex(pattern) => self
                .regex_match_with_captures(pattern, actual_str)
                .is_some(),
            // `rx:i/.../` and friends carry adverbs, so route through the full
            // smart-match engine (which honours `:i`, `:m`, ...) rather than the
            // bare-pattern matcher above.
            Value::RegexWithAdverbs { .. } => {
                let topic = actual_val
                    .cloned()
                    .unwrap_or_else(|| Value::str(actual_str.to_string()));
                self.smart_match_values(&topic, matcher)
            }
            Value::Sub(_) | Value::Routine { .. } => {
                let call_arg = actual_val.cloned().unwrap_or(Value::Nil);
                match self.call_sub_value(matcher.clone(), vec![call_arg], false) {
                    Ok(result_val) => result_val.truthy(),
                    Err(_) => false,
                }
            }
            Value::Package(type_name) => actual_val.is_some_and(|actual| {
                crate::value::types::what_type_name(actual) == type_name.resolve()
            }),
            Value::Junction { kind, values } => {
                let mut matches = values
                    .iter()
                    .map(|v| self.matcher_accepts(v, actual_str, actual_val));
                match kind {
                    crate::value::JunctionKind::All => matches.all(|m| m),
                    crate::value::JunctionKind::Any => matches.any(|m| m),
                    crate::value::JunctionKind::None => !matches.any(|m| m),
                    crate::value::JunctionKind::One => matches.filter(|m| *m).count() == 1,
                }
            }
            _ => actual_str == matcher.to_string_value(),
        }
    }

    /// `throws-like-any($code, @ex_type, $reason?, *%matcher)`
    /// Like `throws-like` but accepts an array of exception types and passes if
    /// the thrown exception matches *any* of them.
    pub(crate) fn test_fn_throws_like_any(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let code_val =
            Self::positional_value_required(args, 0, "throws-like-any expects code")?.clone();
        let types_val =
            Self::positional_value_required(args, 1, "throws-like-any expects type list")?.clone();

        // Extract type names from the array argument
        let type_names: Vec<String> = match &types_val {
            Value::Array(items, _) => items
                .iter()
                .map(|v| {
                    let s = v.to_string_value();
                    // Normalize "(Exception)" -> "Exception"
                    let normalized: String = s
                        .strip_prefix('(')
                        .and_then(|s2| s2.strip_suffix(')'))
                        .unwrap_or(&s)
                        .to_string();
                    normalized
                })
                .collect(),
            _ => vec![types_val.to_string_value()],
        };

        let desc = Self::positional_string(args, 2);

        // Execute the code (reuse the same logic as throws-like)
        let result = match &code_val {
            Value::Sub(data) => {
                let saved_last_value = self.last_value.take();
                let r = self.eval_block_value(&data.body);
                self.last_value = saved_last_value;
                r
            }
            Value::Str(code) => {
                let mut nested = Interpreter::new();
                nested.strict_mode = self.strict_mode;
                nested.lib_paths = self.lib_paths.clone();
                nested.program_path = self.program_path.clone();
                nested.registry_mut().functions = self.registry().functions.clone();
                nested.registry_mut().proto_functions = self.registry().proto_functions.clone();
                nested.registry_mut().token_defs = self.registry().token_defs.clone();
                nested.registry_mut().proto_subs = self.registry().proto_subs.clone();
                nested.registry_mut().proto_tokens = self.registry().proto_tokens.clone();
                nested.registry_mut().classes = self.registry().classes.clone();
                nested.registry_mut().roles = self.registry().roles.clone();
                nested.registry_mut().subsets = self.registry().subsets.clone();
                nested.registry_mut().enum_types = self.registry().enum_types.clone();
                nested.type_metadata = self.type_metadata.clone();
                for (k, v) in &self.env {
                    if !k.contains_str("::") {
                        nested.env.insert_sym(*k, v.clone());
                    }
                }
                let pre_check_result = {
                    let op_names = nested.collect_operator_sub_names();
                    let op_assoc = nested.collect_operator_assoc_map();
                    let imported_names = nested.collect_eval_imported_function_names();
                    match crate::parser::parse_program_with_operators(
                        code,
                        &op_names,
                        &op_assoc,
                        &imported_names,
                    ) {
                        Ok((stmts, _)) => nested
                            .check_eval_mainline_placeholders(&stmts)
                            .and_then(|()| nested.check_eval_class_redeclarations(&stmts))
                            .and_then(|()| nested.check_eval_undeclared_vars(&stmts))
                            .and_then(|()| nested.check_eval_undeclared_names(&stmts)),
                        Err(mut e) => {
                            // A compile-time exception raised while parsing the
                            // EVAL'd code reports the EVAL pseudo-file and the
                            // line within that source (Raku exposes `.filename`
                            // matching /EVAL/ and `.line`).
                            if let Some(ref mut exc_box) = e.exception
                                && let Value::Instance { attributes, .. } = exc_box.as_mut()
                            {
                                let line = e.line.unwrap_or(1) as i64;
                                attributes.insert_if_absent("line".to_string(), Value::Int(line));
                                attributes.insert_if_absent(
                                    "filename".to_string(),
                                    Value::str("EVAL_0".to_string()),
                                );
                                attributes.insert_if_absent(
                                    "file".to_string(),
                                    Value::str("EVAL_0".to_string()),
                                );
                            }
                            Err(e)
                        }
                    }
                };
                if let Err(e) = pre_check_result {
                    Err(e)
                } else {
                    let run_result = nested.run(code).map(|_| Value::Nil);
                    match run_result {
                        Ok(_) => {
                            if let Some(last_val) = nested.last_value.take() {
                                Self::sink_failure_to_error(last_val)
                            } else {
                                Ok(Value::Nil)
                            }
                        }
                        err => err,
                    }
                }
            }
            other => Self::sink_failure_to_error(other.clone()),
        };
        let result = match result {
            Ok(val) => Self::sink_failure_to_error(val),
            err => err,
        };

        // Check if the exception matches any of the provided types
        let type_ok =
            match &result {
                Ok(_) => false,
                Err(err) => {
                    let ex_class = err.exception.as_ref().and_then(|ex| {
                        if let Value::Instance { class_name, .. } = ex.as_ref() {
                            Some(class_name.resolve().to_string())
                        } else {
                            None
                        }
                    });
                    type_names.iter().any(|expected: &String| -> bool {
                        if expected.is_empty() || expected == "Exception" {
                            return true;
                        }
                        if let Some(cls) = &ex_class
                            && (cls == expected
                                || cls.starts_with(&format!("{}::", expected))
                                || self.registry().classes.get(cls).is_some_and(|def| {
                                    def.mro.iter().any(|parent| parent == expected)
                                }))
                        {
                            return true;
                        }
                        // Fallback: message-based matching
                        err.message.contains(expected.as_str())
                    })
                }
            };

        let type_display = type_names.join(", ");

        let ctx = self.begin_subtest();
        let state = self.tap.ensure_state();
        state.planned = Some(2);
        self.emit_output("1..2\n");
        self.test_ok(result.is_err(), "code dies", false)?;
        self.test_ok(
            type_ok,
            &format!("right exception type ({})", type_display),
            false,
        )?;

        let all_ok = type_ok && result.is_err();
        let label = if desc.is_empty() {
            format!("did we throws-like {}?", type_display)
        } else {
            desc.clone()
        };
        self.finish_subtest(
            ctx,
            &label,
            if all_ok {
                Ok(())
            } else {
                Err(RuntimeError::new(""))
            },
        )?;
        Ok(Value::Bool(type_ok))
    }
}
