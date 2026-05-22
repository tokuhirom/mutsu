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
        // Check for `is DEPRECATED` trait on the method
        if let Some(ref msg) = method_def.deprecated_message {
            let cl = self.interpreter.env().get("?LINE").and_then(|v| match v {
                Value::Int(i) => Some(*i),
                _ => None,
            });
            self.interpreter.check_deprecation_for_method_with_line(
                method_name,
                owner_class,
                msg,
                cl,
            );
        }
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

        // Pre-compute whether we can skip the expensive env merge on exit.
        let has_rw_params = method_def
            .param_defs
            .iter()
            .any(|pd| pd.traits.iter().any(|t| t == "rw"));
        let can_skip_merge = !has_rw_params && !cc.has_env_writes;

        // Fast path: bypass env entirely and populate locals directly from
        // source data. Avoids the ~12μs Arc::make_mut deep clone that the
        // first env_mut() call triggers.
        if !has_rw_params {
            let has_invocant_constraint = method_def.param_defs.iter().any(|pd| {
                (pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"))
                    && pd.type_constraint.is_some()
            });
            let has_attr_aliases = attributes
                .keys()
                .any(|k| k.starts_with(ATTR_ALIAS_META_PREFIX));
            let has_role_bindings = self
                .interpreter
                .class_role_param_bindings(owner_class)
                .is_some()
                || self
                    .interpreter
                    .class_role_param_bindings(receiver_class_name)
                    .is_some();
            let has_complex_params = method_def.param_defs.iter().any(|pd| {
                if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") {
                    return false;
                }
                // Allow the implicit %_ slurpy that every method gets
                if pd.slurpy && pd.name == "%_" {
                    return false;
                }
                pd.slurpy
                    || pd.double_slurpy
                    || pd.named
                    || pd.where_constraint.is_some()
                    || pd.sub_signature.is_some()
                    || pd.outer_sub_signature.is_some()
                    || pd.code_signature.is_some()
            });
            // If a default expr needs evaluation and args are missing, fall back
            let needs_default_eval = {
                let mut pos = 0;
                method_def.param_defs.iter().any(|pd| {
                    if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") {
                        return false;
                    }
                    let result = pos >= args.len() && pd.default.is_some();
                    pos += 1;
                    result
                })
            };

            if !has_invocant_constraint
                && !has_attr_aliases
                && !has_role_bindings
                && !has_complex_params
                && !needs_default_eval
                && !cc.may_capture_outer_vars
            {
                return self.call_compiled_method_fast(
                    receiver_class_name,
                    owner_class,
                    method_name,
                    method_def,
                    cc,
                    attributes,
                    args,
                    base,
                    compiled_fns,
                    can_skip_merge,
                );
            }
        }

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

        // Set current_package so class-scoped subs are found during method execution.
        // Only change package if the class has subs declared in its body.
        let saved_package = self.interpreter.current_package().to_string();
        if self.interpreter.has_class_scoped_subs(receiver_class_name) {
            self.interpreter
                .set_current_package(receiver_class_name.to_string());
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
                            self.interpreter.set_current_package(saved_package.clone());
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
                    self.interpreter.set_current_package(saved_package.clone());
                    self.stack.truncate(saved_stack_depth);
                    let frame = self.pop_call_frame();
                    *self.interpreter.env_mut() = frame.saved_env;
                    return Err(e);
                }
            };

        // Initialize locals from env
        self.locals = vec![Value::Nil; cc.locals.len()];
        self.locals_dirty_slots = vec![false; cc.locals.len()];
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

        // Push routine_stack so &?ROUTINE can find the current method
        self.interpreter.push_method_routine_with_location(
            owner_class.to_string(),
            method_name.to_string(),
            self.current_source_line(),
            self.current_source_file(),
        );

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

        if can_skip_merge {
            // Sync only attribute-related locals to env for writeback.
            // Full locals→env sync is unnecessary since we skip the merge,
            // but attribute values must be written back for instance updates.
            for (i, local_name) in cc.locals.iter().enumerate() {
                if local_name.starts_with('.') || local_name.starts_with('!') {
                    self.interpreter
                        .env_mut()
                        .insert(local_name.clone(), self.locals[i].clone());
                }
            }
            writeback_attributes(self.interpreter.env(), &mut attributes);

            let method_var_bindings = self.interpreter.take_var_bindings();
            let mut restored_bindings = saved_var_bindings;
            for (k, v) in method_var_bindings {
                restored_bindings.insert(k, v);
            }
            self.interpreter.restore_var_bindings(restored_bindings);

            self.interpreter.pop_routine();
            self.interpreter.pop_method_class();
        } else {
            // Sync locals back to env
            for (i, local_name) in cc.locals.iter().enumerate() {
                if !local_name.is_empty() {
                    self.interpreter
                        .env_mut()
                        .insert(local_name.clone(), self.locals[i].clone());
                }
            }

            writeback_attributes(self.interpreter.env(), &mut attributes);
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
                if attr_name.contains('\0') {
                    continue;
                }
                if let Some(actual_attr) = attr_name.strip_prefix(ATTR_ALIAS_META_PREFIX) {
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

            for (source_name, val) in &rw_writeback {
                merged_env.insert(source_name.clone(), val.clone());
            }

            let method_var_bindings = self.interpreter.take_var_bindings();
            let mut restored_bindings = saved_var_bindings;
            for (k, v) in method_var_bindings {
                restored_bindings.insert(k, v);
            }
            self.interpreter.restore_var_bindings(restored_bindings);

            self.interpreter.pop_routine();
            self.interpreter.pop_method_class();
            self.interpreter.set_current_package(saved_package.clone());
            let _frame = self.pop_call_frame();
            *self.interpreter.env_mut() = merged_env;
        }

        if can_skip_merge {
            self.interpreter.set_current_package(saved_package);
            let frame = self.pop_call_frame();
            *self.interpreter.env_mut() = frame.saved_env;
        }

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

    /// Fast path for read-only compiled methods. Bypasses all env_mut() calls
    /// to avoid the ~12μs Arc::make_mut deep clone. Populates locals directly
    /// from source data (attributes, args, special variables).
    #[allow(clippy::too_many_arguments)]
    fn call_compiled_method_fast(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_name: &str,
        method_def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        mut attributes: HashMap<String, Value>,
        args: Vec<Value>,
        base: Value,
        compiled_fns: &HashMap<String, CompiledFunction>,
        can_skip_merge: bool,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        if let Some(ref msg) = method_def.deprecated_message {
            self.interpreter.check_deprecation_for_method_with_line(
                method_name,
                owner_class,
                msg,
                None,
            );
        }

        self.push_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        let saved_var_bindings = self.interpreter.take_var_bindings();
        self.interpreter.push_method_class(owner_class.to_string());

        let saved_package = self.interpreter.current_package().to_string();
        if self.interpreter.has_class_scoped_subs(receiver_class_name) {
            self.interpreter
                .set_current_package(receiver_class_name.to_string());
        }

        // Compute role context without touching env
        let role_context: Option<String> = if self.interpreter.is_role(owner_class) {
            Some(owner_class.to_string())
        } else {
            method_def
                .original_role
                .as_ref()
                .or(method_def.role_origin.as_ref())
                .cloned()
        };

        // Build param name → arg value mapping (skip invocant)
        let mut param_values: Vec<(&str, Value)> = Vec::new();
        let mut arg_idx = 0;
        for (idx, param_name) in method_def.params.iter().enumerate() {
            let pd = method_def.param_defs.get(idx);
            let is_invocant = pd
                .map(|pd| pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"))
                .unwrap_or(false);
            if is_invocant {
                param_values.push((param_name, base.clone()));
                continue;
            }
            if arg_idx < args.len() {
                let val = args[arg_idx].clone();
                if let Some(ref constraint) = pd.and_then(|pd| pd.type_constraint.as_ref())
                    && !self.interpreter.type_matches_value(constraint, &val)
                {
                    // Type mismatch — fall back to slow path for proper error
                    self.interpreter.restore_var_bindings(saved_var_bindings);
                    self.interpreter.pop_method_class();
                    self.interpreter.set_current_package(saved_package);
                    self.stack.truncate(saved_stack_depth);
                    let frame = self.pop_call_frame();
                    self.interpreter.set_env(frame.saved_env);
                    return Err(RuntimeError::new(format!(
                        "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {}, got {}",
                        param_name,
                        constraint,
                        crate::runtime::value_type_name(&val)
                    )));
                }
                param_values.push((param_name, val));
                arg_idx += 1;
            } else if pd.map(|pd| pd.optional_marker).unwrap_or(false) {
                param_values.push((param_name, Value::Nil));
            }
        }

        // Build class value for ?CLASS
        let class_val = Value::Package(crate::symbol::Symbol::intern(owner_class));
        let method_callable_id = crate::value::next_instance_id();
        let any_val = Value::Package(crate::symbol::Symbol::intern("Any"));

        // Build a small method env for $!attr reads (GetGlobal needs env).
        // Instead of cloning the full outer env (~100 entries, ~12μs), build
        // a fresh HashMap with just method-specific entries (~20 entries).
        {
            let capacity = 10 + attributes.len() * 4;
            let mut method_env = HashMap::with_capacity(capacity);
            method_env.insert("self".to_string(), base.clone());
            method_env.insert("__ANON_STATE__".to_string(), base.clone());
            method_env.insert("?CLASS".to_string(), class_val.clone());
            method_env.insert("_".to_string(), any_val.clone());
            method_env.insert("!".to_string(), Value::Nil);
            method_env.insert(
                "__mutsu_callable_id".to_string(),
                Value::Int(method_callable_id as i64),
            );
            if let Some(ref role_name) = role_context {
                method_env.insert(
                    "?ROLE".to_string(),
                    Value::Package(crate::symbol::Symbol::intern(role_name)),
                );
            }
            for (attr_name, attr_val) in &attributes {
                if attr_name.contains('\0') || attr_name.starts_with(ATTR_ALIAS_META_PREFIX) {
                    continue;
                }
                let qualified_key = format!("{}\0{}", owner_class, attr_name);
                let private_val = attributes.get(&qualified_key).unwrap_or(attr_val);
                method_env.insert(format!("!{}", attr_name), private_val.clone());
                method_env.insert(format!(".{}", attr_name), attr_val.clone());
                match private_val {
                    Value::Array(..) => {
                        method_env.insert(format!("@!{}", attr_name), private_val.clone());
                        method_env.insert(format!("@.{}", attr_name), attr_val.clone());
                    }
                    Value::Hash(..) => {
                        method_env.insert(format!("%!{}", attr_name), private_val.clone());
                        method_env.insert(format!("%.{}", attr_name), attr_val.clone());
                    }
                    _ => {}
                }
            }
            // Add parameter bindings
            for (param_name, param_val) in &param_values {
                method_env.insert(param_name.to_string(), param_val.clone());
            }
            // Copy dynamic variables ($*FOO), caller-visible keys, and
            // other entries from the outer env that the method might need.
            for (k, v) in self.interpreter.env().iter() {
                if k.starts_with('*')
                    || k.starts_with("$*")
                    || k.starts_with("@*")
                    || k.starts_with("%*")
                    || k.starts_with('&')
                    || k.starts_with("?LINE")
                    || k.starts_with("?FILE")
                    || k == "/"
                {
                    method_env.insert(k.clone(), v.clone());
                }
            }
            self.interpreter.set_env(crate::env::Env::from(method_env));
        }

        // Populate locals directly
        self.locals = vec![Value::Nil; cc.locals.len()];
        self.locals_dirty_slots = vec![false; cc.locals.len()];

        for (i, local_name) in cc.locals.iter().enumerate() {
            self.locals[i] = match local_name.as_str() {
                "self" | "__ANON_STATE__" => base.clone(),
                "?CLASS" => class_val.clone(),
                "?ROLE" => {
                    if let Some(ref role_name) = role_context {
                        Value::Package(crate::symbol::Symbol::intern(role_name))
                    } else {
                        Value::Nil
                    }
                }
                "_" => any_val.clone(),
                "!" => Value::Nil,
                "__mutsu_callable_id" => Value::Int(method_callable_id as i64),
                name => {
                    // Check params first
                    if let Some((_, val)) = param_values.iter().find(|(n, _)| *n == name) {
                        val.clone()
                    }
                    // Private attribute: !attr_name
                    else if let Some(attr_name) = name.strip_prefix('!') {
                        let qualified_key = format!("{}\0{}", owner_class, attr_name);
                        attributes
                            .get(&qualified_key)
                            .or_else(|| attributes.get(attr_name))
                            .cloned()
                            .unwrap_or(Value::Nil)
                    }
                    // Public attribute: .attr_name
                    else if let Some(attr_name) = name.strip_prefix('.') {
                        attributes.get(attr_name).cloned().unwrap_or(Value::Nil)
                    }
                    // Array/hash attributes: @!name, @.name, %!name, %.name
                    else if ((name.starts_with("@!") || name.starts_with("@."))
                        || (name.starts_with("%!") || name.starts_with("%.")))
                        && name.len() > 2
                    {
                        attributes.get(&name[2..]).cloned().unwrap_or(Value::Nil)
                    }
                    // Outer env (read-only, no deep clone)
                    else if let Some(val) = self.interpreter.env().get(name) {
                        val.clone()
                    } else {
                        Value::Nil
                    }
                }
            };
        }

        // Load persisted state variable values
        for (slot, key) in &cc.state_locals {
            if let Some(val) = self.interpreter.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        // Register `is default(...)` values for attribute variables
        for attr_name in attributes.keys() {
            if attr_name.contains('\0') || attr_name.starts_with(ATTR_ALIAS_META_PREFIX) {
                continue;
            }
            let default_val = self
                .interpreter
                .class_attribute_default(owner_class, attr_name)
                .or_else(|| {
                    self.interpreter
                        .class_attribute_default(receiver_class_name, attr_name)
                })
                .cloned();
            if let Some(def) = default_val {
                self.interpreter
                    .set_var_default(&format!("!{}", attr_name), def.clone());
                self.interpreter
                    .set_var_default(&format!(".{}", attr_name), def);
            }
        }

        self.interpreter.push_method_routine_with_location(
            owner_class.to_string(),
            method_name.to_string(),
            self.current_source_line(),
            self.current_source_file(),
        );

        // Execute bytecode (same as slow path)
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
                Value::Nil
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Sync state variables
        for (slot, key) in &cc.state_locals {
            let val = self.locals[*slot].clone();
            self.interpreter.set_state_var(key.clone(), val);
        }

        if can_skip_merge {
            // Writeback attributes directly from locals (no env involved)
            writeback_attributes_from_locals(cc, &self.locals, &mut attributes, owner_class);
        } else {
            // Non-can_skip_merge: AssignExpr may have written attribute
            // values to env. Sync locals→env first, then use env-based writeback.
            for (i, local_name) in cc.locals.iter().enumerate() {
                if local_name.starts_with('.')
                    || local_name.starts_with('!')
                    || local_name.starts_with("@!")
                    || local_name.starts_with("@.")
                    || local_name.starts_with("%!")
                    || local_name.starts_with("%.")
                {
                    self.interpreter
                        .env_mut()
                        .insert(local_name.clone(), self.locals[i].clone());
                }
            }
            writeback_attributes(self.interpreter.env(), &mut attributes);
        }

        let method_var_bindings = self.interpreter.take_var_bindings();
        let mut restored_bindings = saved_var_bindings;
        for (k, v) in method_var_bindings {
            restored_bindings.insert(k, v);
        }
        self.interpreter.restore_var_bindings(restored_bindings);

        self.interpreter.pop_routine();
        self.interpreter.pop_method_class();
        self.interpreter.set_current_package(saved_package);

        if can_skip_merge {
            // No env writes possible — just restore saved env
            let frame = self.pop_call_frame();
            self.interpreter.set_env(frame.saved_env);
        } else {
            // Inner calls may have modified env (globals, dynamics).
            // Check if env was actually changed; if so, merge the changes
            // into the saved env before restoring.
            let frame = self.pop_call_frame();
            if self.interpreter.env().ptr_eq(&frame.saved_env) {
                self.interpreter.set_env(frame.saved_env);
            } else {
                // Build method_local_keys set (keys that should NOT leak to caller)
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
                    if attr_name.contains('\0') || attr_name.starts_with(ATTR_ALIAS_META_PREFIX) {
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
                let merged =
                    merge_method_env(&frame.saved_env, self.interpreter.env(), &method_local_keys);
                self.interpreter.set_env(merged);
            }
        }

        let final_result = match result {
            Ok(()) => Ok(explicit_return.unwrap_or(ret_val)),
            Err(e) => Err(e),
        };

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

/// Write back attribute values from locals after fast-path method execution.
/// Reads directly from the locals array instead of going through env.
fn writeback_attributes_from_locals(
    cc: &CompiledCode,
    locals: &[Value],
    attributes: &mut HashMap<String, Value>,
    owner_class: &str,
) {
    for attr_name in attributes.keys().cloned().collect::<Vec<_>>() {
        if attr_name.starts_with(ATTR_ALIAS_META_PREFIX) || attr_name.contains('\0') {
            continue;
        }
        let original = attributes.get(&attr_name).cloned().unwrap_or(Value::Nil);

        let private_key = format!("!{}", attr_name);
        let public_key = format!(".{}", attr_name);
        let private_val = cc
            .locals
            .iter()
            .rposition(|n| n == &private_key)
            .map(|i| locals[i].clone());
        let public_val = cc
            .locals
            .iter()
            .rposition(|n| n == &public_key)
            .map(|i| locals[i].clone());

        // Also check array/hash variants
        let arr_private_key = format!("@!{}", attr_name);
        let arr_public_key = format!("@.{}", attr_name);
        let hash_private_key = format!("%!{}", attr_name);
        let hash_public_key = format!("%.{}", attr_name);
        let arr_private_val = cc
            .locals
            .iter()
            .rposition(|n| n == &arr_private_key)
            .map(|i| locals[i].clone());
        let arr_public_val = cc
            .locals
            .iter()
            .rposition(|n| n == &arr_public_key)
            .map(|i| locals[i].clone());
        let hash_private_val = cc
            .locals
            .iter()
            .rposition(|n| n == &hash_private_key)
            .map(|i| locals[i].clone());
        let hash_public_val = cc
            .locals
            .iter()
            .rposition(|n| n == &hash_public_key)
            .map(|i| locals[i].clone());

        if let (Some(priv_val), Some(pub_val)) = (&arr_private_val, &arr_public_val) {
            if *priv_val == original && *pub_val != original {
                attributes.insert(attr_name.clone(), pub_val.clone());
            } else {
                attributes.insert(attr_name.clone(), priv_val.clone());
            }
            continue;
        }
        if let (Some(priv_val), Some(pub_val)) = (&hash_private_val, &hash_public_val) {
            if *priv_val == original && *pub_val != original {
                attributes.insert(attr_name.clone(), pub_val.clone());
            } else {
                attributes.insert(attr_name.clone(), priv_val.clone());
            }
            continue;
        }
        if let (Some(priv_val), Some(pub_val)) = (&private_val, &public_val) {
            if *priv_val == original && *pub_val != original {
                attributes.insert(attr_name.clone(), pub_val.clone());
            } else {
                attributes.insert(attr_name.clone(), priv_val.clone());
            }
            continue;
        }
        if let Some(val) = arr_private_val.or(hash_private_val).or(private_val) {
            attributes.insert(attr_name.clone(), val);
            continue;
        }
        if let Some(val) = arr_public_val.or(hash_public_val).or(public_val) {
            attributes.insert(attr_name, val);
        }
    }

    // Write back class-qualified private attribute entries
    for attr_name in attributes.keys().cloned().collect::<Vec<_>>() {
        if !attr_name.contains('\0') {
            continue;
        }
        if let Some(bare_attr) = attr_name.strip_prefix(&format!("{}\0", owner_class)) {
            let private_key = format!("!{}", bare_attr);
            if let Some(idx) = cc.locals.iter().rposition(|n| n == &private_key) {
                attributes.insert(attr_name, locals[idx].clone());
            }
        }
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
