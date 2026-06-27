use super::*;
use crate::runtime::types::unwrap_varref_value;

impl Interpreter {
    /// Lightweight compiled function call that avoids the heavyweight frame
    /// management (push_call_frame/env clone, Sub value creation, block/routine
    /// push, callable_id lookup). Binds parameters directly to locals slots
    /// without touching the env HashMap, maximizing performance for hot loops.
    pub(super) fn call_compiled_function_light(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        self.record_cf_deprecation(cf);
        // Save caller locals and create callee locals
        let saved_locals = std::mem::take(&mut self.locals);

        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller. Param / alias / @_ env writes below
        // land in a fresh map and are discarded by dropping the overlay on return
        // (callee-local names) or merged overlay-only (captured-outer writes),
        // replacing the per-key save/restore the `modified_env_keys` list did.
        let parent = self.env().clone();
        let caller_env = std::mem::replace(self.env_mut(), crate::env::Env::scoped_child(parent));

        let num_locals = cf.code.locals.len();
        self.locals = vec![Value::Nil; num_locals];

        // Bind parameters directly to locals slots and the overlay env.
        let mut positional_idx = 0usize;
        for pd in &cf.param_defs {
            let param_name = &pd.name;
            let value = if pd.named {
                // Named parameter: search args for matching Pair
                let match_key = pd
                    .name
                    .strip_prefix('@')
                    .or_else(|| pd.name.strip_prefix('%'))
                    .unwrap_or(&pd.name);
                let match_key = match_key
                    .strip_prefix('!')
                    .or_else(|| match_key.strip_prefix('.'))
                    .unwrap_or(match_key);

                let mut found_val: Option<Value> = None;

                // Try matching the param name directly
                for arg in args.iter().rev() {
                    let arg = unwrap_varref_value(arg.clone());
                    if let Value::Pair(key, val) = arg
                        && key == match_key
                    {
                        found_val = Some(*val.clone());
                        break;
                    }
                }

                // Try alias matching via sub_signature (e.g. :color(:$colour))
                if found_val.is_none()
                    && let Some(ref sub_params) = pd.sub_signature
                {
                    for sub_pd in sub_params {
                        if found_val.is_some() {
                            break;
                        }
                        if !sub_pd.named {
                            continue;
                        }
                        let inner_key = sub_pd.name.strip_prefix(':').unwrap_or(&sub_pd.name);
                        for arg in args.iter().rev() {
                            let arg = unwrap_varref_value(arg.clone());
                            if let Value::Pair(key, val) = arg
                                && key == inner_key
                            {
                                found_val = Some(*val.clone());
                                break;
                            }
                        }
                    }
                }
                // Try outer_sub_signature aliases
                if found_val.is_none()
                    && let Some(ref outer) = pd.outer_sub_signature
                {
                    for outer_pd in outer {
                        if found_val.is_some() {
                            break;
                        }
                        let outer_name = outer_pd
                            .name
                            .trim_start_matches(|c: char| "$@%&".contains(c));
                        for arg in args.iter().rev() {
                            let arg = unwrap_varref_value(arg.clone());
                            if let Value::Pair(key, val) = arg
                                && key == outer_name
                            {
                                found_val = Some(*val.clone());
                                break;
                            }
                        }
                    }
                }

                if let Some(v) = found_val {
                    // If there's a sub_signature (rename), also bind inner params.
                    // e.g. :color(:$colour) — bind both "color" and "colour".
                    if let Some(ref sub_params) = pd.sub_signature {
                        for sub_pd in sub_params {
                            let sub_name = &sub_pd.name;
                            if let Some(slot) = cf.code.locals.iter().position(|n| n == sub_name) {
                                self.locals[slot] = v.clone();
                            }
                            self.env_mut().insert(sub_name.clone(), v.clone());
                        }
                    }
                    Some(v)
                } else if pd.required {
                    self.set_env(caller_env);
                    self.locals = saved_locals;
                    // Missing required named parameter is a runtime X::AdHoc in
                    // Raku (see binding.rs); carry the typed exception so it does
                    // not fall back to the bare "Exception" default.
                    return Err(RuntimeError::typed_msg(
                        "X::AdHoc",
                        format!("Required named parameter '{}' not passed", param_name),
                    ));
                } else {
                    None
                }
            } else {
                // Positional parameter: skip Pair args (they are named)
                while positional_idx < args.len() {
                    let unwrapped = unwrap_varref_value(args[positional_idx].clone());
                    if !matches!(&unwrapped, Value::Pair(..)) {
                        break;
                    }
                    positional_idx += 1;
                }
                if positional_idx < args.len() {
                    let val = unwrap_varref_value(args[positional_idx].clone());
                    positional_idx += 1;
                    Some(val)
                } else if pd.required {
                    self.set_env(caller_env);
                    self.locals = saved_locals;
                    return Err(RuntimeError::new(format!(
                        "Too few positionals passed; expected {} arguments but got {}",
                        cf.param_defs.iter().filter(|p| !p.named).count(),
                        args.iter()
                            .filter(|a| !matches!(a, Value::Pair(..)))
                            .count()
                    )));
                } else {
                    None
                }
            };

            // Set both locals slot and env for the param.
            // Locals are needed for GetLocal (fast path), env is needed for
            // closures that capture the variable and for GetLocal's Nil
            // fallback check (which errors on undeclared variables).
            let bound_val = value.unwrap_or(Value::Nil);
            if let Some(slot) = cf.code.locals.iter().position(|n| n == param_name) {
                self.locals[slot] = bound_val.clone();
            }
            self.env_mut().insert(param_name.clone(), bound_val);
        }

        // Mark parameters as readonly (by default, params are immutable in Raku).
        // Save existing readonly state so we can restore it after the call.
        let saved_readonly = self.save_readonly_vars();
        for pd in &cf.param_defs {
            if !pd.name.is_empty()
                && !pd.sigilless
                && !pd.name.starts_with('!')
                && !pd.name.starts_with('.')
            {
                let has_mutable_trait = pd
                    .traits
                    .iter()
                    .any(|t| t == "rw" || t == "copy" || t == "raw");
                if !has_mutable_trait {
                    self.mark_readonly(&pd.name);
                }
            }
        }

        // Set @_ only if the function body uses it (has @_ in locals)
        if cf.code.locals.iter().any(|n| n == "@_") {
            let plain_args: Vec<Value> = args
                .iter()
                .filter(|a| !matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)))
                .map(|a| unwrap_varref_value(a.clone()))
                .collect();
            self.env_mut()
                .insert("@_".to_string(), Value::array(plain_args));
        }

        // Handle legacy placeholder params (e.g. $^a, $^b from cf.params)
        if cf.param_defs.is_empty() && !cf.params.is_empty() {
            let mut placeholder_pos = 0usize;
            let plain_args: Vec<Value> = args
                .iter()
                .filter(|a| !matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)))
                .map(|a| unwrap_varref_value(a.clone()))
                .collect();
            for param in &cf.params {
                if placeholder_pos < plain_args.len() {
                    let val = plain_args[placeholder_pos].clone();
                    placeholder_pos += 1;
                    // Set in locals
                    if let Some(slot) = cf.code.locals.iter().position(|n| n == param) {
                        self.locals[slot] = val.clone();
                    }
                    // Also set in (overlay) env for the placeholder and its alias
                    self.env_mut().insert(param.clone(), val.clone());
                    // Create de-careted alias: ^foo -> foo, ^k -> k
                    if let Some(bare) = param.strip_prefix('^') {
                        let bare = bare.to_string();
                        if let Some(slot) = cf.code.locals.iter().position(|n| *n == bare) {
                            self.locals[slot] = val.clone();
                        }
                        self.env_mut().insert(bare, val);
                    }
                }
            }
        }

        // For any locals not yet set from params, try to initialize from env
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if matches!(self.locals[i], Value::Nil)
                && let Some(val) = self.env().get(local_name)
            {
                self.locals[i] = val.clone();
            }
        }

        let saved_stack_depth = self.stack.len();
        let let_mark = self.let_saves_len();

        // Execute the function body
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(e) if e.return_value.is_some() => {
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.resolve_let_saves_on_success(let_mark, true);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail() => {
                    fail_bypass = true;
                    let failure = self.fail_error_to_failure_value(&e);
                    loan_env!(self, restore_let_saves(let_mark));
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
            }
            if self.is_halted() {
                break;
            }
        }

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Restore locals
        self.locals = saved_locals;

        // Restore readonly vars
        self.restore_readonly_vars(saved_readonly);

        // Restore the caller env, merging the overlay (the callee's own writes)
        // back: a write to a captured outer variable (not a declared local /
        // param of this function) persists in the caller; the function's
        // params/locals/aliases are dropped with the overlay. This replaces the
        // per-key `modified_env_keys` save/restore.
        {
            let local_names: std::collections::HashSet<&str> =
                if let Some(ref declared) = cf.declared_locals {
                    declared.iter().map(|s| s.as_str()).collect()
                } else {
                    cf.code.locals.iter().map(|s| s.as_str()).collect()
                };
            let scoped = std::mem::replace(self.env_mut(), caller_env);
            for (k, v) in scoped.overlay_iter() {
                if *k == "_"
                    || *k == "@_"
                    || *k == "%_"
                    || *k == "__mutsu_callable_id"
                    || k.with_str(|s| s.starts_with('?'))
                {
                    continue;
                }
                if !k.with_str(|s| local_names.contains(s)) {
                    self.env_mut().insert_sym(*k, v.clone());
                }
            }
        }

        // Slice F (env<->locals coherence): record the captured-outer variables
        // this body writes so the call-site op writes them straight through to the
        // caller's local slots (see `call_compiled_function_positional_light`).
        for sym in &cf.code.free_var_writes {
            sym.with_str(|fname| {
                if fname != "_" && fname != "@_" && fname != "%_" {
                    self.pending_rw_writeback_sources.push(fname.to_string());
                }
            });
        }

        match result {
            Ok(()) if fail_bypass => Ok(ret_val),
            Ok(()) => {
                if let Some(v) = explicit_return {
                    Ok(v)
                } else {
                    Ok(ret_val)
                }
            }
            Err(e) => Err(e),
        }
    }

    /// A return value that may *be* (or carry) a routine declared inside the
    /// callee body — i.e. a `my sub` that escaped by being returned. When the
    /// body declares an inner routine and returns one of these, its registry
    /// entry must survive the call so it stays callable by name (e.g. `my &bar
    /// := producer()` then `bar(...)`). Any other return value means the inner
    /// routine did not escape via the return slot and can be cleaned up.
    pub(super) fn return_value_escapes_routine(v: &Value) -> bool {
        matches!(
            v,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } | Value::Mixin(..)
        )
    }
}
