use super::*;

const ATTR_ALIAS_META_PREFIX: &str = "__mutsu_attr_alias::";

impl VM {
    /// Call a compiled method body (MethodDef with compiled_code).
    /// Mirrors `Interpreter::run_instance_method_resolved` but executes bytecode.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn call_compiled_method(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_name: &str,
        method_def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        mut attributes: HashMap<String, Value>,
        args: Vec<Value>,
        invocant: Option<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        // Build the base (self) value
        let mut base = if let Some(inv) = invocant {
            inv
        } else if attributes.is_empty() {
            Value::Package(crate::symbol::Symbol::intern(receiver_class_name))
        } else {
            Value::make_instance(
                crate::symbol::Symbol::intern(receiver_class_name),
                attributes.clone(),
            )
        };

        self.push_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;

        // Clear var_bindings so attribute aliases from outer interpreter-level
        // method calls don't leak into compiled method locals (e.g. `x → !x`
        // from run_instance_method_resolved shadowing a local parameter `x`).
        let saved_var_bindings = self.interpreter.take_var_bindings();

        self.interpreter.push_method_class(owner_class.to_string());

        // Detect role context: use the pre-computed role_origin stored on the
        // MethodDef (set during role composition) instead of expensive fingerprint
        // matching on every call.
        let role_context = if self.interpreter.is_role(owner_class) {
            Some(owner_class.to_string())
        } else {
            method_def
                .original_role
                .as_ref()
                .or(method_def.role_origin.as_ref())
                .cloned()
        };

        // Set ::?CLASS / ::?ROLE
        self.interpreter.env_mut().insert(
            "?CLASS".to_string(),
            Value::Package(crate::symbol::Symbol::intern(owner_class)),
        );
        if let Some(role_name) = role_context {
            self.interpreter.env_mut().insert(
                "?ROLE".to_string(),
                Value::Package(crate::symbol::Symbol::intern(&role_name)),
            );
        } else {
            self.interpreter.env_mut().remove("?ROLE");
        }

        // Set self and __ANON_STATE__ (used by `$.foo` desugaring inside methods)
        self.interpreter
            .env_mut()
            .insert("self".to_string(), base.clone());
        self.interpreter
            .env_mut()
            .insert("__ANON_STATE__".to_string(), base.clone());

        // In Raku, methods do NOT set $_ to the invocant by default.
        // $_ in a method body is Any unless the invocant is explicitly named $_
        // (e.g. `method foo ($_: ) { ... }`). The invocant binding loop below
        // will set $_ back to self if the invocant param is named "_".
        self.interpreter.env_mut().insert(
            "_".to_string(),
            Value::Package(crate::symbol::Symbol::intern("Any")),
        );

        // Raku: $! is scoped per routine — fresh Nil on entry
        self.interpreter
            .env_mut()
            .insert("!".to_string(), Value::Nil);

        // Assign a unique callable ID for this method invocation so that
        // non-local returns from blocks defined inside this method can target it.
        let method_callable_id = crate::value::next_instance_id();
        self.interpreter.env_mut().insert(
            "__mutsu_callable_id".to_string(),
            Value::Int(method_callable_id as i64),
        );

        // Role param bindings
        if let Some(role_bindings) = self
            .interpreter
            .class_role_param_bindings(owner_class)
            .cloned()
        {
            for (name, value) in &role_bindings {
                self.interpreter
                    .env_mut()
                    .insert(name.clone(), value.clone());
            }
        } else if let Some(role_bindings) = self
            .interpreter
            .class_role_param_bindings(receiver_class_name)
            .cloned()
        {
            for (name, value) in &role_bindings {
                self.interpreter
                    .env_mut()
                    .insert(name.clone(), value.clone());
            }
        }

        // Skip invocant param, bind remaining
        let mut bind_params = Vec::new();
        let mut bind_param_defs = Vec::new();
        for (idx, param_name) in method_def.params.iter().enumerate() {
            let is_invocant = method_def
                .param_defs
                .get(idx)
                .map(|pd| pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"))
                .unwrap_or(false);
            if is_invocant {
                if let Some(pd) = method_def.param_defs.get(idx)
                    && let Some(constraint) = &pd.type_constraint
                {
                    if let Some(captured_name) = constraint.strip_prefix("::") {
                        self.interpreter.bind_type_capture(captured_name, &base);
                    } else {
                        let coercion_target = if let Some(open) = constraint.find('(') {
                            if constraint.ends_with(')') && open > 0 {
                                Some(&constraint[..open])
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        let expected = coercion_target.unwrap_or(constraint.as_str());
                        if coercion_target.is_some() {
                            let mut candidate = self
                                .interpreter
                                .try_coerce_value_for_constraint(constraint, base.clone())
                                .unwrap_or_else(|_| base.clone());
                            if !self.interpreter.type_matches_value(expected, &candidate)
                                && let Ok(coerced) = self.try_compiled_method_or_interpret(
                                    base.clone(),
                                    expected,
                                    vec![],
                                )
                            {
                                candidate = coerced;
                            }
                            if self.interpreter.type_matches_value(expected, &candidate) {
                                base = candidate;
                                self.interpreter
                                    .env_mut()
                                    .insert("self".to_string(), base.clone());
                            }
                        } else if !self.interpreter.type_matches_value(constraint, &base)
                            && let Ok(coerced) = self
                                .interpreter
                                .try_coerce_value_for_constraint(constraint, base.clone())
                        {
                            base = coerced;
                            self.interpreter
                                .env_mut()
                                .insert("self".to_string(), base.clone());
                        }
                        if !self.interpreter.type_matches_value(expected, &base) {
                            self.interpreter.restore_var_bindings(saved_var_bindings);
                            self.interpreter.pop_method_class();
                            self.stack.truncate(saved_stack_depth);
                            let frame = self.pop_call_frame();
                            *self.interpreter.env_mut() = frame.saved_env;
                            // :D/:U smiley mismatch → X::Parameter::InvalidConcreteness
                            if constraint.ends_with(":D") || constraint.ends_with(":U") {
                                let (base_type, _) =
                                    crate::runtime::types::strip_type_smiley(constraint);
                                let should_be_concrete = constraint.ends_with(":D");
                                return Err(RuntimeError::parameter_invalid_concreteness(
                                    base_type,
                                    base_type,
                                    method_name,
                                    &format!("${}", param_name),
                                    should_be_concrete,
                                    pd.is_invocant,
                                ));
                            }
                            return Err(RuntimeError::new(format!(
                                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {}, got {}",
                                param_name,
                                constraint,
                                crate::runtime::value_type_name(&base)
                            )));
                        }
                    }
                }
                self.interpreter
                    .env_mut()
                    .insert(param_name.clone(), base.clone());
                continue;
            }
            bind_params.push(param_name.clone());
            if let Some(pd) = method_def.param_defs.get(idx) {
                bind_param_defs.push(pd.clone());
            }
        }

        // Bind attributes
        for (attr_name, attr_val) in &attributes {
            // Skip class-qualified private attribute entries (ClassName\0attrName)
            // — these are handled below when binding $!attr for the owner class.
            if attr_name.contains('\0') {
                continue;
            }
            if let Some(actual_attr) = attr_name.strip_prefix(ATTR_ALIAS_META_PREFIX) {
                if let Value::Str(source_name) = attr_val {
                    // Set up bidirectional alias: !x ↔ alias_name
                    self.interpreter.env_mut().insert(
                        format!("__mutsu_sigilless_alias::!{}", actual_attr),
                        Value::str(source_name.to_string()),
                    );
                    self.interpreter.env_mut().insert(
                        format!("__mutsu_sigilless_readonly::!{}", actual_attr),
                        Value::Bool(false),
                    );
                    // Reverse alias: alias_name → !attr so writing to $x updates $!x
                    self.interpreter.env_mut().insert(
                        format!("__mutsu_sigilless_alias::{}", source_name),
                        Value::str(format!("!{}", actual_attr)),
                    );
                    self.interpreter.env_mut().insert(
                        format!("__mutsu_sigilless_readonly::{}", source_name),
                        Value::Bool(false),
                    );
                    // Also set up the alias name with the current attribute value
                    if let Some(attr_value) = attributes.get(actual_attr) {
                        self.interpreter
                            .env_mut()
                            .insert(source_name.to_string(), attr_value.clone());
                    }
                }
                continue;
            }
            // For private attributes ($!attr), prefer the class-qualified value
            // from the method's owner class. This ensures that when Parent and Child
            // both declare `$!priv`, Parent's method gets Parent's value.
            let qualified_key = format!("{}\0{}", owner_class, attr_name);
            let private_val = attributes.get(&qualified_key).unwrap_or(attr_val);
            self.interpreter
                .env_mut()
                .insert(format!("!{}", attr_name), private_val.clone());
            self.interpreter
                .env_mut()
                .insert(format!(".{}", attr_name), attr_val.clone());
            match private_val {
                Value::Array(..) => {
                    self.interpreter
                        .env_mut()
                        .insert(format!("@!{}", attr_name), private_val.clone());
                    self.interpreter
                        .env_mut()
                        .insert(format!("@.{}", attr_name), attr_val.clone());
                }
                Value::Hash(..) => {
                    self.interpreter
                        .env_mut()
                        .insert(format!("%!{}", attr_name), private_val.clone());
                    self.interpreter
                        .env_mut()
                        .insert(format!("%.{}", attr_name), attr_val.clone());
                }
                _ => {}
            }
        }

        // Register `is default(...)` values for attribute variables so that
        // .VAR.default returns the correct value inside methods.
        for attr_name in attributes.keys() {
            if attr_name.contains('\0') || attr_name.starts_with(ATTR_ALIAS_META_PREFIX) {
                continue;
            }
            // Check both the owner class and the receiver class for defaults
            let default_val = self
                .interpreter
                .class_attribute_default(owner_class, attr_name)
                .or_else(|| {
                    self.interpreter
                        .class_attribute_default(receiver_class_name, attr_name)
                })
                .cloned();
            if let Some(def) = default_val {
                // Register for $!attr and $.attr variable names
                self.interpreter
                    .set_var_default(&format!("!{}", attr_name), def.clone());
                self.interpreter
                    .set_var_default(&format!(".{}", attr_name), def);
            }
        }

        // Bind method parameters
        let rw_bindings =
            match self
                .interpreter
                .bind_function_args_values(&bind_param_defs, &bind_params, &args)
            {
                Ok(bindings) => bindings,
                Err(e) => {
                    self.interpreter.restore_var_bindings(saved_var_bindings);
                    self.interpreter.pop_method_class();
                    self.stack.truncate(saved_stack_depth);
                    let frame = self.pop_call_frame();
                    *self.interpreter.env_mut() = frame.saved_env;
                    return Err(e);
                }
            };

        // Initialize locals from env
        self.locals = vec![Value::Nil; cc.locals.len()];
        for (i, local_name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values
        for (slot, key) in &cc.state_locals {
            if let Some(val) = self.interpreter.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        // Execute bytecode
        let let_mark = self.interpreter.let_saves_len();
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        while ip < cc.ops.len() {
            match self.exec_one(cc, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(mut e) if e.is_leave => {
                    let routine_key = format!("{}::{}", owner_class, method_name);
                    let matches_frame = if let Some(_target_id) = e.leave_callable_id {
                        // Methods don't have callable IDs, so this won't match
                        false
                    } else if let Some(target_routine) = e.leave_routine.as_ref() {
                        target_routine == &routine_key
                    } else {
                        e.label.is_none()
                    };
                    if matches_frame {
                        e.is_leave = false;
                        e.is_last = false;
                        let ret_val = e.return_value.unwrap_or(Value::Nil);
                        explicit_return = Some(ret_val.clone());
                        self.stack.truncate(saved_stack_depth);
                        self.stack.push(ret_val);
                        self.interpreter.discard_let_saves(let_mark);
                        result = Ok(());
                        break;
                    }
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
                Err(e) if e.return_value.is_some() => {
                    // Non-local return: if the signal targets a specific callable,
                    // only catch it if this method is the target.
                    if let Some(target_id) = e.return_target_callable_id
                        && target_id != method_callable_id
                    {
                        self.interpreter.restore_let_saves(let_mark);
                        result = Err(e);
                        break;
                    }
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.interpreter.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
                    let failure = self.interpreter.fail_error_to_failure_value(&e);
                    self.interpreter.restore_let_saves(let_mark);
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                // Implicit return from env "_"
                self.interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil)
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Sync state variables back
        for (slot, key) in &cc.state_locals {
            let local_name = &cc.locals[*slot];
            let val = self
                .interpreter
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.interpreter.set_state_var(key.clone(), val);
        }

        // Sync locals back to env
        for (i, local_name) in cc.locals.iter().enumerate() {
            if !local_name.is_empty() {
                self.interpreter
                    .env_mut()
                    .insert(local_name.clone(), self.locals[i].clone());
            }
        }

        writeback_attributes(self.interpreter.env(), &mut attributes);
        // Collect keys introduced by the method frame so they don't bleed
        // back into the caller's env (e.g. method param `$g` vs caller `$g`).
        let mut method_local_keys: HashSet<String> = HashSet::from_iter([
            "self".to_string(),
            "__ANON_STATE__".to_string(),
            "?CLASS".to_string(),
            "?ROLE".to_string(),
            "_".to_string(),
        ]);
        for p in &method_def.params {
            method_local_keys.insert(p.clone());
        }
        for attr_name in attributes.keys() {
            // Skip class-qualified private attribute keys
            if attr_name.contains('\0') {
                continue;
            }
            if let Some(actual_attr) = attr_name.strip_prefix(ATTR_ALIAS_META_PREFIX) {
                // Add the alias name itself to method_local_keys
                if let Some(Value::Str(alias_name)) = attributes.get(attr_name) {
                    method_local_keys.insert(alias_name.to_string());
                    method_local_keys.insert(actual_attr.to_string());
                }
                continue;
            }
            method_local_keys.insert(format!("!{}", attr_name));
            method_local_keys.insert(format!(".{}", attr_name));
            method_local_keys.insert(format!("@!{}", attr_name));
            method_local_keys.insert(format!("@.{}", attr_name));
            method_local_keys.insert(format!("%!{}", attr_name));
            method_local_keys.insert(format!("%.{}", attr_name));
        }
        for local_name in &cc.locals {
            if !local_name.is_empty() {
                method_local_keys.insert(local_name.clone());
            }
        }
        // Collect `is rw` param writeback values before the env merge
        // filters out method-local keys (params). The param value may exist
        // under its bare name or a package-qualified name (e.g. "C::x")
        // because the compiled method body qualifies variables with the class
        // package.
        let rw_writeback: Vec<(String, Value)> = rw_bindings
            .iter()
            .filter_map(|(param_name, source_name)| {
                self.interpreter
                    .env()
                    .get(param_name)
                    .cloned()
                    .or_else(|| {
                        let qualified = format!("{}::{}", owner_class, param_name);
                        self.interpreter.env().get(&qualified).cloned()
                    })
                    .map(|val| (source_name.clone(), val))
            })
            .collect();

        let mut merged_env = merge_method_env(
            &self.call_frames.last().unwrap().saved_env,
            self.interpreter.env(),
            &method_local_keys,
        );

        // Apply `is rw` writebacks to the merged env so changes propagate
        // back to the caller's variables.
        for (source_name, val) in &rw_writeback {
            merged_env.insert(source_name.clone(), val.clone());
        }

        // Merge var_bindings: keep any new bindings set during method execution
        // (e.g. from $CALLER:: rebinding), then restore original bindings for
        // keys not touched during execution.
        let method_var_bindings = self.interpreter.take_var_bindings();
        let mut restored_bindings = saved_var_bindings;
        for (k, v) in method_var_bindings {
            restored_bindings.insert(k, v);
        }
        self.interpreter.restore_var_bindings(restored_bindings);

        self.interpreter.pop_method_class();
        let _frame = self.pop_call_frame();
        *self.interpreter.env_mut() = merged_env;

        let final_result = match result {
            Ok(()) => Ok(explicit_return.unwrap_or(ret_val)),
            Err(e) => Err(e),
        };

        // Apply return type spec (e.g. `--> 5` returns literal 5 from empty body)
        let final_result = if let Some(ref return_spec) = method_def.return_type {
            let effective_return_spec = self.interpreter.resolved_type_capture_name(return_spec);
            self.interpreter
                .finalize_return_with_spec(final_result, Some(effective_return_spec.as_str()))
        } else {
            match final_result {
                Ok(v) => Ok(v),
                Err(e) if e.return_value.is_some() && e.return_target_callable_id.is_none() => {
                    Ok(e.return_value.unwrap())
                }
                Err(e) => Err(e),
            }
        };

        // Adjust return value if it's the same instance (update attributes)
        final_result.map(|v| {
            let adjusted = match (&base, &v) {
                (
                    Value::Instance {
                        class_name,
                        id: base_id,
                        ..
                    },
                    Value::Instance { id: ret_id, .. },
                ) if base_id == ret_id => {
                    Value::make_instance_with_id(*class_name, attributes.clone(), *base_id)
                }
                _ => v,
            };
            (adjusted, attributes)
        })
    }
}

/// Write back attribute values from env after method execution.
///
/// Compares original attribute values with private (`!name`) and public (`.name`)
/// env entries, preferring public when private is unchanged and public differs.
fn writeback_attributes(env: &HashMap<String, Value>, attributes: &mut HashMap<String, Value>) {
    for attr_name in attributes.keys().cloned().collect::<Vec<_>>() {
        if attr_name.starts_with(ATTR_ALIAS_META_PREFIX) {
            continue;
        }
        // Skip class-qualified private attribute keys (ClassName\0attrName).
        // These are written back separately below.
        if attr_name.contains('\0') {
            continue;
        }
        let original = attributes.get(&attr_name).cloned().unwrap_or(Value::Nil);
        let env_key = format!("!{}", attr_name);
        let public_env_key = format!(".{}", attr_name);
        let env_array_private_key = format!("@!{}", attr_name);
        let env_array_public_key = format!("@.{}", attr_name);
        let env_hash_private_key = format!("%!{}", attr_name);
        let env_hash_public_key = format!("%.{}", attr_name);
        let env_private = env.get(&env_key).cloned();
        let env_public = env.get(&public_env_key).cloned();
        let env_array_private = env.get(&env_array_private_key).cloned();
        let env_array_public = env.get(&env_array_public_key).cloned();
        let env_hash_private = env.get(&env_hash_private_key).cloned();
        let env_hash_public = env.get(&env_hash_public_key).cloned();
        if let (Some(private_val), Some(public_val)) = (&env_array_private, &env_array_public) {
            if *private_val == original && *public_val != original {
                attributes.insert(attr_name.clone(), public_val.clone());
            } else {
                attributes.insert(attr_name.clone(), private_val.clone());
            }
            continue;
        }
        if let (Some(private_val), Some(public_val)) = (&env_hash_private, &env_hash_public) {
            if *private_val == original && *public_val != original {
                attributes.insert(attr_name.clone(), public_val.clone());
            } else {
                attributes.insert(attr_name.clone(), private_val.clone());
            }
            continue;
        }
        if let (Some(private_val), Some(public_val)) = (&env_private, &env_public) {
            if *private_val == original && *public_val != original {
                attributes.insert(attr_name.clone(), public_val.clone());
            } else {
                attributes.insert(attr_name.clone(), private_val.clone());
            }
            continue;
        }
        if let Some(val) = env_array_private.or(env_hash_private).or(env_private) {
            attributes.insert(attr_name.clone(), val);
            continue;
        }
        if let Some(val) = env_array_public.or(env_hash_public).or(env_public) {
            attributes.insert(attr_name.clone(), val);
        }
        let alias_env_key = format!("__mutsu_sigilless_alias::!{}", attr_name);
        if let Some(Value::Str(alias_name)) = env.get(&alias_env_key) {
            attributes.insert(
                format!("{}{}", ATTR_ALIAS_META_PREFIX, attr_name),
                Value::str(alias_name.to_string()),
            );
        }
    }
}

/// Merge method env back into the saved (caller) env.
///
/// Carries forward values for keys that existed in the saved env, plus any
/// dynamic/global keys (`&`-prefixed, `__mutsu_method_value::`-prefixed).
fn merge_method_env(saved: &Env, current: &Env, method_local_keys: &HashSet<String>) -> Env {
    let mut merged = saved.clone();
    for (k, v) in current.iter() {
        // Skip keys that were introduced by the method frame (params, self,
        // attributes, locals) — these must not leak back into the caller.
        if method_local_keys.contains(k) {
            continue;
        }
        if saved.contains_key(k) {
            merged.insert(k.clone(), v.clone());
        }
        if (k.starts_with('&') && !k.starts_with("&?"))
            || k.starts_with("__mutsu_method_value::")
            || k.starts_with("__mutsu_sigilless_alias::!")
            || k.starts_with("__mutsu_predictive_seq_iter::")
        {
            merged.insert(k.clone(), v.clone());
        }
    }
    merged
}
