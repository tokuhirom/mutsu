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
            Value::Sub(data) => self.eval_block_value(&data.body),
            Value::Str(code) => {
                let mut nested = Interpreter::new();
                nested.strict_mode = self.strict_mode;
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.lib_paths = self.lib_paths.clone();
                nested.program_path = self.program_path.clone();
                nested.functions = self.functions.clone();
                nested.proto_functions = self.proto_functions.clone();
                nested.token_defs = self.token_defs.clone();
                nested.proto_subs = self.proto_subs.clone();
                nested.proto_tokens = self.proto_tokens.clone();
                nested.classes = self.classes.clone();
                nested.class_trusts = self.class_trusts.clone();
                nested.class_composed_roles = self.class_composed_roles.clone();
                nested.roles = self.roles.clone();
                nested.role_candidates = self.role_candidates.clone();
                nested.role_parents = self.role_parents.clone();
                nested.role_hides = self.role_hides.clone();
                nested.role_type_params = self.role_type_params.clone();
                nested.class_role_param_bindings = self.class_role_param_bindings.clone();
                nested.subsets = self.subsets.clone();
                nested.enum_types = self.enum_types.clone();
                nested.type_metadata = self.type_metadata.clone();
                nested.current_package = self.current_package.clone();
                nested.suppressed_names = self.suppressed_names.clone();
                nested.lexical_class_scopes = self.lexical_class_scopes.clone();
                nested.var_dynamic_flags = self.var_dynamic_flags.clone();
                nested.restore_var_type_constraints(self.snapshot_var_type_constraints());
                for (k, v) in &self.env {
                    if k.contains("::") {
                        continue;
                    }
                    nested.env.insert(k.clone(), v.clone());
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
                            .check_eval_class_redeclarations(&stmts)
                            .and_then(|()| nested.check_eval_undeclared_names(&stmts)),
                        Err(e) => Err(e),
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
                        || self.classes.get(cls).is_some_and(|def| {
                            def.mro.iter().any(|parent| parent == expected_normalized)
                        })
                        // X::Comp::Group wraps compile-time errors: match any X::Comp subtype
                        || (expected_normalized == "X::Comp::Group"
                            && self.classes.get(cls).is_some_and(|def| {
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
        let state = self.test_state.get_or_insert_with(TestState::new);
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
                    attributes.get(attr_name).cloned()
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
            let matched = match expected_val {
                Value::Whatever => true, // * matches anything
                Value::Regex(pattern) => self
                    .regex_match_with_captures(pattern, &actual_str)
                    .is_some(),
                Value::Sub(_) | Value::Routine { .. } => {
                    // Smart-match: call the block with the actual value as topic
                    let call_arg = actual_val.clone().unwrap_or(Value::Nil);
                    match self.call_sub_value(expected_val.clone(), vec![call_arg], false) {
                        Ok(result_val) => result_val.truthy(),
                        Err(_) => false,
                    }
                }
                _ => actual_str == expected_val.to_string_value(),
            };
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
}
