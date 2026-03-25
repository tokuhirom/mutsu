use super::super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn test_fn_fails_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val = Self::positional_value_required(args, 0, "fails-like expects code")?.clone();
        let expected =
            Self::positional_value_required(args, 1, "fails-like expects type")?.to_string_value();
        let desc = Self::positional_string(args, 2);
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

        let expected_normalized = expected
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .unwrap_or(&expected)
            .to_string();

        // Save $_ so the block evaluation doesn't leak Failure values
        // into the caller's topic variable
        let saved_topic = self.env.get("_").cloned();
        let saved_dollar_topic = self.env.get("$_").cloned();
        let result = match &code_val {
            Value::Sub(data) => self.eval_block_value(&data.body),
            Value::Str(code) => {
                let mut nested = Interpreter::new();
                nested.strict_mode = self.strict_mode;
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.lib_paths = self.lib_paths.clone();
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
                nested.var_dynamic_flags = self.var_dynamic_flags.clone();
                for (k, v) in &self.env {
                    if k.contains("::") {
                        continue;
                    }
                    if matches!(v, Value::Sub(_) | Value::Routine { .. }) {
                        continue;
                    }
                    nested.env.insert(k.clone(), v.clone());
                }
                nested.eval_eval_string(code)
            }
            _ => Ok(Value::Nil),
        };
        // Restore $_ to prevent leaking Failure values from the block
        if let Some(topic) = saved_topic {
            self.env.insert("_".to_string(), topic);
        } else {
            self.env.remove("_");
        }
        if let Some(topic) = saved_dollar_topic {
            self.env.insert("$_".to_string(), topic);
        } else {
            self.env.remove("$_");
        }

        let is_failure_like = |value: &Value| {
            matches!(value, Value::Instance { class_name, .. } if class_name == "Failure")
                || matches!(value, Value::Mixin(_, mixins) if mixins.contains_key("Failure"))
        };

        // Determine if the result is a Failure, and if so, sink it to get the error
        let (is_failure, sink_result) = match result {
            Ok(value) => {
                if is_failure_like(&value) {
                    let sink = self.call_method_with_values(value, "sink", vec![]);
                    (true, Some(sink))
                } else {
                    (false, None)
                }
            }
            Err(err) => {
                // Code threw directly (not via a Failure) — report as
                // "expected code to fail but it threw <type> instead"
                let ex_class = err
                    .exception
                    .as_ref()
                    .and_then(|ex| {
                        if let Value::Instance { class_name, .. } = ex.as_ref() {
                            Some(class_name.resolve())
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| "X::AdHoc".to_string());
                let label = if desc.is_empty() {
                    format!("did we fails-like {}?", expected_normalized)
                } else {
                    desc.clone()
                };
                let fail_msg = format!("expected code to fail but it threw {} instead", ex_class);
                // Emit as a single failing subtest
                let ctx = self.begin_subtest();
                let state = self.test_state.get_or_insert_with(TestState::new);
                state.planned = Some(2);
                self.emit_output("1..2\n");
                self.test_ok(false, &fail_msg, false)?;
                // Skip the second test since the first failed
                {
                    let state = self.test_state.get_or_insert_with(TestState::new);
                    state.next_ran();
                }
                let ran = self.test_state.as_ref().map(|s| s.ran).unwrap_or(0);
                self.emit_output(&format!("ok {} - # SKIP {}\n", ran, fail_msg));
                self.finish_subtest(ctx, &label, Err(RuntimeError::new("")))?;
                return Ok(Value::Bool(false));
            }
        };

        let label = if desc.is_empty() {
            format!("did we fails-like {}?", expected_normalized)
        } else {
            desc.clone()
        };

        // Outer subtest: "code returned a Failure" + inner "Failure threw when sunk"
        let outer_ctx = self.begin_subtest();
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.planned = Some(2);
        self.emit_output("1..2\n");

        // Test 1: code returned a Failure
        if !is_failure {
            self.test_ok(false, "code returned a Failure", false)?;
        } else {
            self.test_ok(true, "code returned a Failure", false)?;
        }

        // Test 2: inner subtest "Failure threw when sunk"
        let inner_ctx = self.begin_subtest();

        let (code_died, type_ok, exception_val) = if !is_failure {
            // Not a failure — the inner subtest also fails
            (false, false, None)
        } else if let Some(sink) = sink_result {
            match sink {
                Err(err) => {
                    let ex_class = err.exception.as_ref().and_then(|ex| {
                        if let Value::Instance { class_name, .. } = ex.as_ref() {
                            Some(class_name.resolve())
                        } else {
                            None
                        }
                    });
                    let type_matched =
                        if expected_normalized.is_empty() || expected_normalized == "Exception" {
                            true
                        } else if let Some(cls) = &ex_class {
                            cls == &expected_normalized
                                || cls.starts_with(&format!("{}::", expected_normalized))
                                || (cls == "X::AdHoc" && err.message.contains(&expected_normalized))
                        } else if expected_normalized == "X::AdHoc" {
                            true
                        } else {
                            err.message.contains(&expected_normalized)
                        };
                    (
                        true,
                        type_matched,
                        err.exception.as_ref().map(|e| e.as_ref().clone()),
                    )
                }
                Ok(_) => (false, false, None),
            }
        } else {
            (false, false, None)
        };

        // Determine which named matchers to check
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

        let inner_total = 2 + named_checks.len();
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.planned = Some(inner_total);
        self.emit_output(&format!("1..{}\n", inner_total));

        self.test_ok(code_died, "code dies", false)?;

        if !code_died {
            // Skip remaining tests
            {
                let state = self.test_state.get_or_insert_with(TestState::new);
                state.next_ran();
            }
            let ran = self.test_state.as_ref().map(|s| s.ran).unwrap_or(0);
            self.emit_output(&format!(
                "ok {} - # SKIP Code did not die, can not check exception\n",
                ran
            ));
        } else {
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
                let actual_str = actual_val
                    .as_ref()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let matched = match expected_val {
                    Value::Whatever => true,
                    Value::Regex(pattern) => self
                        .regex_match_with_captures(pattern, &actual_str)
                        .is_some(),
                    Value::Sub(_) | Value::Routine { .. } => {
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
        }

        let inner_ok = code_died && type_ok;
        self.finish_subtest(
            inner_ctx,
            "Failure threw when sunk",
            if inner_ok {
                Ok(())
            } else {
                Err(RuntimeError::new(""))
            },
        )?;

        let all_ok = is_failure && inner_ok;
        self.finish_subtest(
            outer_ctx,
            &label,
            if all_ok {
                Ok(())
            } else {
                Err(RuntimeError::new(""))
            },
        )?;
        Ok(Value::Bool(all_ok))
    }
}
