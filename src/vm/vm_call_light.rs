use super::*;

impl Interpreter {
    pub(super) fn call_compiled_function_positional_light(
        &mut self,
        cf: &CompiledFunction,
        args: &[Value],
        compiled_fns: &HashMap<String, CompiledFunction>,
        func_name: &str,
    ) -> Result<Value, RuntimeError> {
        self.record_cf_deprecation(cf);
        let param_slots = cf.param_local_slots.as_ref().unwrap();
        let positional_count = param_slots.len();
        let actual_count = args.len();

        // Every positional-light-eligible parameter is a mandatory positional
        // (no default, optional `?`, or slurpy -- see
        // `is_positional_light_call_eligible`), so any shortfall is a "too few
        // positionals" arity error. Report it as a typed X::TypeCheck::Argument
        // carrying objname/signature/arguments, matching the interpreter path.
        if actual_count < positional_count {
            let msg = format!(
                "Too few positionals passed; expected {} arguments but got {}",
                positional_count, actual_count
            );
            return Err(RuntimeError::typed(
                "X::TypeCheck::Argument",
                Self::type_check_argument_attrs(func_name, &cf.param_defs, args, msg),
            ));
        }

        let saved_locals = std::mem::take(&mut self.locals);

        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller. Param / local env writes land in a
        // fresh map and are discarded by dropping the overlay on return (for
        // callee-local names) or merged overlay-only (for captured-outer writes).
        // This replaces the previous name-keyed save/restore juggling
        // (saved_env_locals / saved_param_env) with a single O(callee-writes)
        // merge -- the function's own params/locals never pollute the caller env.
        let parent = self.env().clone();
        let caller_env = std::mem::replace(self.env_mut(), crate::env::Env::scoped_child(parent));

        let num_locals = cf.code.locals.len();
        self.locals.clear();
        self.locals.resize(num_locals, Value::Nil);

        // Read-through to the caller (parent tier) for the initial value of a
        // local that shadows a same-named caller variable, matching the prior
        // env().get() semantics.
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }

        let saved_readonly = self.save_readonly_vars();
        // Bind params to slots. Also write the param into the overlay when a
        // name-based reader needs it (reflective access anywhere / GetGlobal /
        // closure capture via needs_env_sync), or when it is `Nil` (the GetLocal
        // handler treats a `Nil` slot as possibly-undeclared and verifies via
        // env.contains_key). The overlay write is born-owned (no caller-env fork)
        // and is dropped on return, so no per-param save/restore is needed.
        let write_all_params = crate::opcode::reflective_name_access_possible();
        for (param_idx, slot) in param_slots.iter().enumerate() {
            if param_idx < actual_count {
                let val = crate::runtime::types::unwrap_varref_value(args[param_idx].clone());
                if let Some(ref tc) = cf.param_defs[param_idx].type_constraint
                    && !Self::fast_type_check(&val, tc)
                {
                    self.restore_readonly_vars(saved_readonly);
                    self.set_env(caller_env);
                    self.locals = saved_locals;
                    {
                        let param_name = &cf.param_defs[param_idx].name;
                        let got = runtime::value_type_name(&val);
                        let msg = format!(
                            "Type check failed in binding ${}: expected {}, got {}",
                            param_name, tc, got
                        );
                        let mut attrs =
                            Self::type_check_argument_attrs(func_name, &cf.param_defs, args, msg);
                        attrs.insert("expected".to_string(), Value::str(tc.to_string()));
                        attrs.insert("got".to_string(), Value::str(got.to_string()));
                        return Err(RuntimeError::typed("X::TypeCheck::Argument", attrs));
                    }
                }
                let param_name = &cf.param_defs[param_idx].name;
                self.locals[*slot] = val.clone();
                let needs_env = write_all_params
                    || matches!(val, Value::Nil)
                    || cf.code.needs_env_sync.get(*slot).copied().unwrap_or(true);
                if needs_env {
                    self.env_mut().insert(param_name.clone(), val);
                }
                self.mark_readonly(&cf.param_defs[param_idx].name);
            }
        }
        // Bind-time param values that a name-based reader can observe were
        // already written into the (born-owned) overlay env above when
        // `needs_env` held. A slot-only param (read solely via GetLocal) never
        // reaches env, so no env mirror is needed here. The bind value is already
        // coherent; any later reassignment in the body writes through to env via
        // the SetLocal path (`flush_local_to_env`).

        let saved_stack_depth = self.stack.len();
        let let_mark = self.let_saves_len();
        // Run the body under the routine's declaring package (set after the
        // arity/type-check early returns above, which run under the caller's
        // package). Restored after the env merge below.
        let saved_package = self.enter_routine_package(cf);

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

        // Natural fall-through completion (no explicit return / fail / error
        // break arm): restore any `temp` bindings the body introduced so a
        // `sub f { temp $x = ... }` with no explicit return does not leak the
        // temporized value into the caller's scope.
        if result.is_ok() && explicit_return.is_none() && !fail_bypass {
            self.resolve_let_saves_on_success(let_mark, true);
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

        self.locals = saved_locals;
        self.restore_readonly_vars(saved_readonly);

        // Restore the caller env and merge the overlay (the callee's own writes)
        // back: a write to a captured outer variable (not a declared local of
        // this function) persists in the caller; the function's params/locals are
        // dropped with the overlay. This replaces the prior per-name save/restore.
        {
            let local_names: std::collections::HashSet<&str> =
                if let Some(ref declared) = cf.declared_locals {
                    declared.iter().map(|s| s.as_str()).collect()
                } else {
                    cf.code.locals.iter().map(|s| s.as_str()).collect()
                };
            let scoped = std::mem::replace(self.env_mut(), caller_env);
            for (k, v) in scoped.overlay_iter() {
                // The callee's private topic / arg array / routine id, and the
                // per-frame contextual vars (`?LINE`/`?FILE`/...), must not
                // propagate to the caller (which retains its own). Skipping the
                // `?`-prefixed ones also avoids spurious `env_dirty` churn from
                // the per-statement `?LINE` write on every recursive call.
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
        self.leave_routine_package(saved_package);

        // Return type check (if specified). Allows type objects, Nil, and Failure through.
        if result.is_ok()
            && let Some(ref rt) = cf.return_type
        {
            let check_val = explicit_return.as_ref().unwrap_or(&ret_val);
            if !Self::light_return_type_check(check_val, rt) {
                return Err(RuntimeError::new(format!(
                    "Type check failed for return value; expected {}, got {}",
                    rt,
                    runtime::value_type_name(check_val)
                )));
            }
        }

        // Slice F (env<->locals coherence): record the captured-outer variables
        // this body writes so the call-site op writes their new env values straight
        // through to the caller's local slots, dropping the dependency on the
        // reverse `sync_locals_from_env` pull. Mirrors the fast-call (#3317) and
        // named-dispatch (#3323) paths: `sub take($n) { $seen = $n }` writes its
        // enclosing `$seen` by name. `free_var_writes` is empty for a pure body
        // (no cost); the topic is excluded as a per-call alias.
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

    /// Check if a type name is one of the basic types that fast_type_check handles.
    pub(super) fn is_fast_type_name(name: &str) -> bool {
        matches!(
            name,
            "Int" | "Str" | "Num" | "Bool" | "Rat" | "Any" | "Mu" | "Cool"
        )
    }

    /// Return type check that handles type objects, Nil, and Failure passthrough.
    fn light_return_type_check(val: &Value, type_name: &str) -> bool {
        // Nil and Failure always pass return type checks
        if matches!(val, Value::Nil) {
            return true;
        }
        if let Value::Instance { class_name, .. } = val
            && class_name.resolve() == "Failure"
        {
            return true;
        }
        // Type objects (Package values) that match the return type pass
        if let Value::Package(sym) = val {
            return sym.resolve() == type_name;
        }
        Self::fast_type_check(val, type_name)
    }

    /// Fast type check for common types.
    fn fast_type_check(val: &Value, type_name: &str) -> bool {
        match type_name {
            "Int" => matches!(val, Value::Int(_) | Value::BigInt(_)),
            "Str" => matches!(val, Value::Str(_)),
            "Num" => matches!(val, Value::Num(_)),
            "Bool" => matches!(val, Value::Bool(_)),
            "Rat" => matches!(val, Value::Rat(_, _)),
            "Any" | "Mu" | "Cool" => true,
            _ => {
                let actual = runtime::value_type_name(val);
                actual == type_name
            }
        }
    }
}
