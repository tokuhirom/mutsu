use super::super::*;
use crate::symbol::Symbol;

/// Restore an environment key to its saved value, or remove it if there was none.
fn restore_env_key(
    env: &mut std::collections::HashMap<String, Value>,
    key: &str,
    saved: Option<Value>,
) {
    match saved {
        Some(v) => {
            env.insert(key.to_string(), v);
        }
        None => {
            env.remove(key);
        }
    }
}

impl Interpreter {
    pub(crate) fn test_fn_lives_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let block = Self::positional_value_required(args, 0, "lives-ok expects block")?.clone();
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let ok = match &block {
            Value::Sub(data) => {
                self.push_caller_env();
                let saved_topic = self.env.get("$_").cloned();
                let saved_bare = self.env.get("_").cloned();
                let result = self.eval_block_value(&data.body).is_ok();
                // Restore both "$_" and "_" (bare name used by the VM) to
                // prevent the block from leaking its topic to the caller.
                restore_env_key(&mut self.env, "$_", saved_topic);
                restore_env_key(&mut self.env, "_", saved_bare);
                self.pop_caller_env();
                result
            }
            _ => true,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    pub(crate) fn test_fn_dies_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let block = Self::positional_value_required(args, 0, "dies-ok expects block")?.clone();
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let ok = match &block {
            Value::Sub(data) => {
                self.push_caller_env();
                let saved_topic = self.env.get("$_").cloned();
                let saved_bare = self.env.get("_").cloned();
                let result = self.eval_block_value(&data.body);
                let died = match &result {
                    Err(_) => true,
                    Ok(val) => {
                        // A Failure value in sink context should throw
                        Self::is_failure_value(val)
                    }
                };
                restore_env_key(&mut self.env, "$_", saved_topic);
                restore_env_key(&mut self.env, "_", saved_bare);
                self.pop_caller_env();
                died
            }
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    pub(crate) fn test_fn_isa_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // isa-ok uses the first non-named-pair arg as value.
        // We need special handling because positional_value filters out ALL Pairs,
        // but the first arg could be a Pair value being tested.
        let (value, type_name, desc) = Self::extract_isa_ok_args(args);
        let todo = Self::named_bool(args, "todo");
        let mut ok = value.isa_check(&type_name) || self.type_matches_value(&type_name, value);
        // For Package values, also check full MRO (handles grammar/class inheritance)
        if !ok && let Value::Package(name) = value {
            let mro = self.class_mro(&name.resolve());
            ok = mro.contains(&type_name);
        }
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    /// Extract (value, type_name, desc) for isa-ok from raw args.
    /// The first arg is always the value (even if it's a Pair).
    /// Named Pairs (like :todo) are excluded from positional counting
    /// only after the first 3 positional args are consumed.
    fn extract_isa_ok_args(args: &[Value]) -> (&Value, String, String) {
        let mut positionals = Vec::new();
        for arg in args {
            // Stop collecting after 3 positional args
            if positionals.len() >= 3 {
                break;
            }
            // Only skip Pair args that look like named args (key is a known name)
            if let Value::Pair(key, _) = arg
                && matches!(key.as_str(), "todo")
            {
                continue;
            }
            positionals.push(arg);
        }
        let value = positionals.first().copied().unwrap_or(&Value::Nil);
        let type_name = match positionals.get(1) {
            Some(Value::Package(name)) => name.resolve(),
            Some(Value::Nil) => "Nil".to_string(),
            Some(Value::Str(s)) => s.to_string(),
            Some(Value::Instance { class_name, .. }) => class_name.resolve(),
            Some(v) => {
                // For defined values (e.g., isa-ok $val, 3), extract the type
                // name from the value's type. This matches Raku's behavior.
                crate::value::types::what_type_name(v)
            }
            None => String::new(),
        };
        let desc = positionals
            .get(2)
            .map(|v| v.to_string_value())
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| format!("The object is-a '{}'", type_name));
        (value, type_name, desc)
    }

    pub(crate) fn test_fn_force_todo(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut ranges = Vec::new();
        for arg in Self::positional_values(args) {
            match arg {
                Value::Int(i) if *i > 0 => {
                    let n = *i as usize;
                    ranges.push(TodoRange {
                        start: n,
                        end: n,
                        reason: String::new(),
                    });
                }
                Value::Range(a, b) => {
                    let start = (*a).min(*b).max(1) as usize;
                    let end = (*a).max(*b).max(1) as usize;
                    ranges.push(TodoRange {
                        start,
                        end,
                        reason: String::new(),
                    });
                }
                _ => {}
            }
        }
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.force_todo.extend(ranges);
        Ok(Value::Nil)
    }

    pub(crate) fn test_fn_eval_lives_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val = Self::positional_value_required(args, 0, "eval-lives-ok expects code")?;
        let desc = Self::positional_string(args, 1);
        let code = match code_val {
            Value::Str(s) => s.to_string(),
            _ => String::new(),
        };
        let mut nested = Interpreter::new();
        nested.strict_mode = self.strict_mode;
        if let Some(Value::Int(pid)) = self.env.get("*PID") {
            nested.set_pid(pid.saturating_add(1));
        }
        nested.lib_paths = self.lib_paths.clone();
        nested.program_path = self.program_path.clone();
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
        let eval_result = nested.eval_eval_string(&code);
        let ok = eval_result.is_ok();
        let eval_err_msg = match &eval_result {
            Err(e) => {
                let msg = if e.code.is_some_and(|c| c.is_parse()) {
                    format!("Unable to parse expression; {}", e.message)
                } else {
                    e.message.clone()
                };
                Some(msg)
            }
            Ok(_) => None,
        };
        if ok {
            self.sync_eval_definition_state(&nested);
        }
        for raw in nested.output.lines() {
            let line = raw.trim_start();
            let (assert_ok, rest) = if let Some(rest) = line.strip_prefix("ok ") {
                (true, rest)
            } else if let Some(rest) = line.strip_prefix("not ok ") {
                (false, rest)
            } else {
                continue;
            };
            // Keep TODO failures internal to eval-lives-ok canaries.
            let todo = line.contains("# TODO");
            if !assert_ok && todo {
                continue;
            }
            let desc = rest
                .split_once(" - ")
                .map(|(_, text)| text)
                .unwrap_or("")
                .split_once(" #")
                .map(|(text, _)| text)
                .unwrap_or_else(|| rest.split_once(' ').map(|(_, text)| text).unwrap_or(""))
                .trim()
                .to_string();
            self.test_ok(assert_ok, &desc, todo)?;
        }
        self.test_ok(ok, &desc, false)?;
        if !ok && let Some(err_msg) = eval_err_msg {
            // Emit error details to stderr as diag, matching Raku behavior
            let diag = format!("# Error: {}\n", err_msg);
            self.stderr_output.push_str(&diag);
            if self.immediate_stdout {
                eprint!("{}", diag);
            }
        }
        Ok(Value::Bool(ok))
    }

    pub(crate) fn test_fn_eval_dies_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val = Self::positional_value_required(args, 0, "eval-dies-ok expects code")?;
        let desc = Self::positional_string(args, 1);
        let code = match code_val {
            Value::Str(s) => s.to_string(),
            _ => String::new(),
        };
        let mut nested = Interpreter::new();
        nested.strict_mode = self.strict_mode;
        if let Some(Value::Int(pid)) = self.env.get("*PID") {
            nested.set_pid(pid.saturating_add(1));
        }
        nested.lib_paths = self.lib_paths.clone();
        nested.program_path = self.program_path.clone();
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
        let ok = nested.eval_eval_string(&code).is_err();
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    pub(crate) fn test_fn_warns_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "warns-like expects code")?;
        let test_pattern = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);
        let warn_message = match program_val {
            Value::Str(program) => {
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.set_program_path("<warns-like>");
                let _ = nested.run(program);
                nested.warn_output.clone()
            }
            Value::Sub(data) => {
                let saved_warn = std::mem::take(&mut self.warn_output);
                self.push_caller_env();
                let _ = self.eval_block_value(&data.body);
                self.pop_caller_env();
                let warn_message = self.warn_output.clone();
                self.warn_output = saved_warn;
                warn_message
            }
            Value::WeakSub(weak) => {
                let Some(data) = weak.upgrade() else {
                    return Err(RuntimeError::new("warns-like expects live callable"));
                };
                let saved_warn = std::mem::take(&mut self.warn_output);
                self.push_caller_env();
                let _ = self.eval_block_value(&data.body);
                self.pop_caller_env();
                let warn_message = self.warn_output.clone();
                self.warn_output = saved_warn;
                warn_message
            }
            _ => {
                return Err(RuntimeError::new(
                    "warns-like expects string code or callable",
                ));
            }
        };
        let did_warn = !warn_message.is_empty();
        let label = if desc.is_empty() {
            "warns-like".to_string()
        } else {
            desc
        };
        let ctx = self.begin_subtest();
        let test_state = self.test_state.as_mut().unwrap();
        test_state.planned = Some(2);
        self.emit_output("1..2\n");
        // Test 1: code threw a warning
        self.test_ok(did_warn, "code threw a warning", false)?;
        // Test 2: warning message matches test pattern
        let msg_val = Value::str(warn_message.trim_end().to_string());
        let matched = self.smart_match(&msg_val, &test_pattern);
        self.test_ok(matched, "warning message passes test", false)?;
        self.finish_subtest(ctx, &label, Ok(()))?;
        Ok(Value::Bool(did_warn && matched))
    }

    pub(crate) fn test_fn_doesnt_warn(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "doesn't-warn expects code")?;
        let desc = Self::positional_string(args, 1);
        match program_val {
            Value::Str(s) => {
                let program = s.to_string();
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.set_program_path("<doesn't-warn>");
                let _ = nested.run(&program);
                let warn_message = nested.warn_output.clone();
                let did_warn = !warn_message.is_empty();
                if did_warn {
                    let diag_msg = format!(
                        "code must not warn but it produced a warning: {}",
                        warn_message.trim_end()
                    );
                    self.emit_output(&format!("# {}\n", diag_msg));
                }
                self.test_ok(!did_warn, &desc, false)?;
                Ok(Value::Bool(!did_warn))
            }
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                let saved_warn = std::mem::take(&mut self.warn_output);
                let _ = self.call_sub_value(program_val.clone(), vec![], false);
                let warn_message = std::mem::replace(&mut self.warn_output, saved_warn);
                let did_warn = !warn_message.is_empty();
                if did_warn {
                    let diag_msg = format!(
                        "code must not warn but it produced a warning: {}",
                        warn_message.trim_end()
                    );
                    self.emit_output(&format!("# {}\n", diag_msg));
                }
                self.test_ok(!did_warn, &desc, false)?;
                Ok(Value::Bool(!did_warn))
            }
            _ => Err(RuntimeError::new(
                "doesn't-warn expects string code or callable",
            )),
        }
    }

    pub(crate) fn test_fn_use_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let module = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        let desc = format!("{} module can be use-d ok", module);
        let mut found = false;
        let module_file = module.replace("::", "/");
        for lib_path in &self.lib_paths.clone() {
            for ext in &[".rakumod", ".pm6", ".pm"] {
                let full = format!("{}/{}{}", lib_path, module_file, ext);
                if std::path::Path::new(&full).exists() {
                    found = true;
                    break;
                }
            }
            if found {
                break;
            }
        }
        self.test_ok(found, &desc, todo)?;
        Ok(Value::Bool(found))
    }

    pub(crate) fn test_fn_does_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let role_val = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or_else(|| Value::Package(Symbol::intern(&Self::positional_string(args, 1))));
        let role_name = match &role_val {
            Value::Package(name) => name.resolve(),
            other => other.to_string_value(),
        };
        let desc = {
            let explicit = Self::positional_string(args, 2);
            if explicit.is_empty() {
                format!("The object does '{}'", role_name)
            } else {
                explicit
            }
        };
        let todo = Self::named_bool(args, "todo");
        let ok = self.smart_match(&value, &role_val);
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    pub(crate) fn test_fn_can_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let method_name = Self::positional_string(args, 1);
        let desc = {
            let explicit = Self::positional_string(args, 2);
            if explicit.is_empty() {
                format!("The object can '{}'", method_name)
            } else {
                explicit
            }
        };
        let todo = Self::named_bool(args, "todo");
        let ok = self.value_can_method(&value, &method_name);
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }
}
