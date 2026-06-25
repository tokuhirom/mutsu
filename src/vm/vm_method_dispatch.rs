use super::*;

pub(super) const ATTR_ALIAS_META_PREFIX: &str = "__mutsu_attr_alias::";

impl Interpreter {
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
    ) -> Result<(Value, HashMap<String, Value>, bool), RuntimeError> {
        // Slice F: the rw-writeback source list is drained by the CallMethod /
        // CallMethodMut op right after this dispatch returns, so it must hold
        // only this call's sources. Clear any leftover from a sibling whose call
        // site did not drain; nested calls in the body self-drain via their own
        // call-site ops before this frame records its own sources.
        self.pending_rw_writeback_sources.clear();
        // Check for `is DEPRECATED` trait on the method
        if let Some(ref msg) = method_def.deprecated_message {
            let cl = self.env().get("?LINE").and_then(|v| match v {
                Value::Int(i) => Some(*i),
                _ => None,
            });
            loan_env!(
                self,
                check_deprecation_for_method_with_line(method_name, owner_class, msg, cl,)
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
        // `is raw` is included alongside `is rw`: an `is raw` param bound to an
        // lvalue argument also writes back to the caller (`bind_function_args_values`
        // adds it to `rw_bindings`), and that writeback lives only on the merge
        // path — so a raw-param method must not take the fast path / skip the merge
        // (mirrors how the sub dispatch treats rw and raw alike).
        let has_rw_params = method_def
            .param_defs
            .iter()
            .any(|pd| pd.traits.iter().any(|t| t == "rw" || t == "raw"));
        // A plain (non-`is copy`) `@`/`%` positional param binds the caller's
        // container by alias (Raku readonly-container semantics): `.push`,
        // element assign, and whole-container `=` all propagate to the caller.
        // mutsu realizes this via the same exit-time writeback as `is rw`
        // (`bind_function_args_values` adds it to `rw_bindings`,
        // `apply_rw_bindings_to_env`/the merge writes it back). That writeback
        // lives only on the full path's `else` (merge) branch — so a method with
        // such a param must NOT take the fast path, and must NOT be allowed to
        // `can_skip_merge` (a `.push` mutation is not a static env write, so
        // `cc.has_env_writes` can be false while the container still changed).
        // Subs already route through their writeback; only the method fast path
        // dropped this. Excludes slurpy/named/`is copy`/attr-twigil params.
        let has_aliasable_container_params = method_def.param_defs.iter().any(|pd| {
            !pd.is_invocant
                && !pd.traits.iter().any(|t| t == "invocant")
                && !pd.slurpy
                && !pd.double_slurpy
                && !pd.named
                && !pd.traits.iter().any(|t| t == "copy")
                && pd.sub_signature.is_none()
                && pd.name.len() > 1
                && (pd.name.starts_with('@') || pd.name.starts_with('%'))
                && !pd.name[1..].starts_with(['!', '.'])
        });
        // Slice 2d (method follow-up): an array/hash variable passed into a
        // plain readonly scalar `$` param shares the caller's container in Raku
        // (`method m($n) { $n.push }` mutates the caller's `@z`). The promotion
        // + rw-writeback lives only on the slow `bind_function_args_values`
        // path, so such a call must avoid the fast path and the merge skip —
        // mirroring how `has_aliasable_container_params` handles `@`/`%` params.
        let shares_scalar_container =
            self.method_shares_container_into_scalar_param(method_def, &args);
        let can_skip_merge = !has_rw_params
            && !has_aliasable_container_params
            && !shares_scalar_container
            && !cc.has_env_writes;

        // Fast path: bypass env entirely and populate locals directly from
        // source data. Avoids the ~12μs Arc::make_mut deep clone that the
        // first env_mut() call triggers.
        if !has_rw_params && !has_aliasable_container_params && !shares_scalar_container {
            let has_invocant_constraint = method_def.param_defs.iter().any(|pd| {
                (pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"))
                    && pd.type_constraint.is_some()
            });
            let has_attr_aliases = attributes
                .keys()
                .any(|k| k.starts_with(ATTR_ALIAS_META_PREFIX));
            let has_role_bindings = self.class_role_param_bindings(owner_class).is_some()
                || self
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
                // An attributive parameter (`$!x`/`@!a`) binds straight to an
                // attribute, i.e. it mutates `self` — so it is not read-only and
                // must take the full path (which mirrors it into the shared cell
                // and writes it back). The read-only fast path drops the write.
                if Self::attr_twigil_base(&pd.name).is_some() {
                    return true;
                }
                pd.slurpy
                    || pd.double_slurpy
                    || pd.named
                    || pd.where_constraint.is_some()
                    || pd.sub_signature.is_some()
                    || pd.outer_sub_signature.is_some()
                    || pd.code_signature.is_some()
                    || pd
                        .type_constraint
                        .as_ref()
                        .is_some_and(|tc| tc.contains('('))
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
            // Count positional params to check for arg count mismatch
            let positional_count = method_def
                .param_defs
                .iter()
                .filter(|pd| {
                    !pd.is_invocant
                        && !pd.traits.iter().any(|t| t == "invocant")
                        && !pd.slurpy
                        && !pd.double_slurpy
                        && !pd.named
                })
                .count();
            let has_arg_mismatch = args.len() > positional_count;

            if !has_invocant_constraint
                && !has_attr_aliases
                && !has_role_bindings
                && !has_complex_params
                && !needs_default_eval
                && !has_arg_mismatch
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

        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller (gated on no inner closures) so the
        // method's `self`/`?CLASS`/param/attr/local env setup writes land in a
        // fresh map instead of forking the caller env. `frame.saved_env` holds the
        // flat caller for restoration; the can_skip_merge path drops the overlay
        // via set_env(saved_env) and the merge path's `merge_method_env` iterates
        // it overlay-only (the method's own writes). No closure/thread body runs
        // under the overlay.
        if cc.closure_compiled_codes.is_empty() {
            let parent = self.env().clone();
            self.set_env(crate::env::Env::scoped_child(parent));
        }

        // Clear var_bindings so attribute aliases from outer interpreter-level
        // method calls don't leak into compiled method locals (e.g. `x → !x`
        // from run_instance_method_resolved shadowing a local parameter `x`).
        let saved_var_bindings = self.take_var_bindings();

        self.push_method_class(owner_class.to_string());

        // Detect role context: use the pre-computed role_origin stored on the
        // MethodDef (set during role composition) instead of expensive fingerprint
        // matching on every call.
        let role_context = if self.is_role(owner_class) {
            Some(owner_class.to_string())
        } else {
            method_def
                .original_role
                .as_ref()
                .or(method_def.role_origin.as_ref())
                .cloned()
        };

        // Set ::?CLASS / ::?ROLE
        self.env_mut().insert(
            "?CLASS".to_string(),
            Value::Package(crate::symbol::Symbol::intern(owner_class)),
        );
        if let Some(role_name) = role_context {
            self.env_mut().insert(
                "?ROLE".to_string(),
                Value::Package(crate::symbol::Symbol::intern(&role_name)),
            );
        } else {
            self.env_mut().remove("?ROLE");
        }

        // Set current_package so class-scoped subs are found during method execution.
        // Only change package if the class has subs declared in its body.
        let saved_package = self.current_package().to_string();
        if self.has_class_scoped_subs(receiver_class_name) {
            self.set_current_package(receiver_class_name.to_string());
        }

        // Set self and __ANON_STATE__ (used by `$.foo` desugaring inside methods)
        self.env_mut().insert("self".to_string(), base.clone());
        self.env_mut()
            .insert("__ANON_STATE__".to_string(), base.clone());

        // In Raku, methods do NOT set $_ to the invocant by default.
        // $_ in a method body is Any unless the invocant is explicitly named $_
        // (e.g. `method foo ($_: ) { ... }`). The invocant binding loop below
        // will set $_ back to self if the invocant param is named "_".
        self.env_mut().insert(
            "_".to_string(),
            Value::Package(crate::symbol::Symbol::intern("Any")),
        );

        // Raku: $! is scoped per routine — fresh Nil on entry
        self.env_mut().insert("!".to_string(), Value::Nil);

        // Assign a unique callable ID for this method invocation so that
        // non-local returns from blocks defined inside this method can target it.
        let method_callable_id = crate::value::next_instance_id();
        self.env_mut().insert(
            "__mutsu_callable_id".to_string(),
            Value::Int(method_callable_id as i64),
        );

        // Role param bindings
        if let Some(role_bindings) = self.class_role_param_bindings(owner_class) {
            for (name, value) in &role_bindings {
                self.env_mut().insert(name.clone(), value.clone());
            }
        } else if let Some(role_bindings) = self.class_role_param_bindings(receiver_class_name) {
            for (name, value) in &role_bindings {
                self.env_mut().insert(name.clone(), value.clone());
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
                        loan_env!(self, bind_type_capture(captured_name, &base));
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
                            let mut candidate = loan_env!(
                                self,
                                try_coerce_value_for_constraint(constraint, base.clone())
                            )
                            .unwrap_or_else(|_| base.clone());
                            if !self.type_matches_value(expected, &candidate)
                                && let Ok(coerced) = self.try_compiled_method_or_interpret(
                                    base.clone(),
                                    expected,
                                    vec![],
                                )
                            {
                                candidate = coerced;
                            }
                            if self.type_matches_value(expected, &candidate) {
                                base = candidate;
                                self.env_mut().insert("self".to_string(), base.clone());
                            }
                        } else if !self.type_matches_value(constraint, &base)
                            && let Ok(coerced) = loan_env!(
                                self,
                                try_coerce_value_for_constraint(constraint, base.clone())
                            )
                        {
                            base = coerced;
                            self.env_mut().insert("self".to_string(), base.clone());
                        }
                        if !self.type_matches_value(expected, &base) {
                            self.restore_var_bindings(saved_var_bindings);
                            self.pop_method_class();
                            self.set_current_package(saved_package.clone());
                            self.stack.truncate(saved_stack_depth);
                            let frame = self.pop_call_frame();
                            *self.env_mut() = frame.saved_env;
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
                            return Err(RuntimeError::typecheck_binding_parameter(
                                param_name,
                                constraint,
                                crate::runtime::value_type_name(&base),
                                None,
                            ));
                        }
                    }
                }
                self.env_mut().insert(param_name.clone(), base.clone());
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
                    // A sigilless attribute is in play — enable the cell-direct
                    // routing's alias-table lookup (Phase 3 Stage 2c (ii)).
                    self.sigilless_attrs_active = true;
                    // Set up bidirectional alias: !x ↔ alias_name
                    self.env_mut().insert(
                        format!("__mutsu_sigilless_alias::!{}", actual_attr),
                        Value::str(source_name.to_string()),
                    );
                    self.env_mut().insert(
                        format!("__mutsu_sigilless_readonly::!{}", actual_attr),
                        Value::Bool(false),
                    );
                    // Reverse alias: alias_name → !attr so writing to $x updates $!x
                    self.env_mut().insert(
                        format!("__mutsu_sigilless_alias::{}", source_name),
                        Value::str(format!("!{}", actual_attr)),
                    );
                    self.env_mut().insert(
                        format!("__mutsu_sigilless_readonly::{}", source_name),
                        Value::Bool(false),
                    );
                    // Also set up the alias name with the current attribute value
                    if let Some(attr_value) = attributes.get(actual_attr) {
                        self.env_mut()
                            .insert(source_name.to_string(), attr_value.clone());
                    }
                }
                continue;
            }
            // Phase 3 Stage 2c (iii-b): the attribute value env copies (`!attr`,
            // `.attr`, `@!attr`, …) that materialize used to insert here are no
            // longer needed — every read of `$!x`/`$.x`/`@!a`/`%!h`/sigilless `$x`
            // goes cell-direct (Stage 2a/2b/2c (ii)), and the exit reconcile reads
            // the cell (Stage 2c (iii-a)). Only the sigilless alias table above is
            // still required.
        }

        // Register `is default(...)` values for attribute variables so that
        // .VAR.default returns the correct value inside methods.
        for attr_name in attributes.keys() {
            if attr_name.contains('\0') || attr_name.starts_with(ATTR_ALIAS_META_PREFIX) {
                continue;
            }
            // Check both the owner class and the receiver class for defaults
            let default_val = self
                .class_attribute_default(owner_class, attr_name)
                .or_else(|| self.class_attribute_default(receiver_class_name, attr_name));
            if let Some(def) = default_val {
                // Register for $!attr and $.attr variable names
                self.set_var_default(&format!("!{}", attr_name), def.clone());
                self.set_var_default(&format!(".{}", attr_name), def.clone());
                // Also register for @!attr/@.attr and %!attr/%.attr so
                // .VAR.default works on array/hash attributes.
                self.set_var_default(&format!("@!{}", attr_name), def.clone());
                self.set_var_default(&format!("@.{}", attr_name), def.clone());
                self.set_var_default(&format!("%!{}", attr_name), def.clone());
                self.set_var_default(&format!("%.{}", attr_name), def);
            }
        }

        // Bind method parameters
        let rw_bindings = match loan_env!(
            self,
            bind_function_args_values(&bind_param_defs, &bind_params, &args)
        ) {
            Ok(bindings) => bindings,
            Err(e) => {
                self.restore_var_bindings(saved_var_bindings);
                self.pop_method_class();
                self.set_current_package(saved_package.clone());
                self.stack.truncate(saved_stack_depth);
                let frame = self.pop_call_frame();
                *self.env_mut() = frame.saved_env;
                return Err(e);
            }
        };

        // Initialize locals from env
        self.locals = vec![Value::Nil; cc.locals.len()];
        for (i, local_name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Phase 3 Stage 2c (i): an attributive parameter (`method m($!x)` /
        // `method m(@!a)`) binds straight to the attribute. The binding above
        // writes only env/locals; mirror it into `self`'s shared cell now so the
        // method body's cell-direct reads of `$!x` see the parameter value (not
        // the stale entry value) and the mutation is visible to every alias. This
        // removes the attributive-param case from the exit-time reconcile.
        self.mirror_attributive_params_to_cell(cc, method_def);
        // Load persisted state variable values
        for (slot, key) in &cc.state_locals {
            if let Some(val) = self.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        // Push routine_stack so &?ROUTINE can find the current method
        self.push_method_routine_with_location(
            owner_class.to_string(),
            method_name.to_string(),
            self.current_source_line(),
            self.current_source_file(),
        );

        // Execute bytecode
        let let_mark = self.let_saves_len();
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
                        self.discard_let_saves(let_mark);
                        result = Ok(());
                        break;
                    }
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
                Err(e) if e.return_value.is_some() => {
                    // Non-local return: if the signal targets a specific callable,
                    // only catch it if this method is the target.
                    if let Some(target_id) = e.return_target_callable_id
                        && target_id != method_callable_id
                    {
                        loan_env!(self, restore_let_saves(let_mark));
                        result = Err(e);
                        break;
                    }
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
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
                // Implicit return from env "_"
                self.env().get("_").cloned().unwrap_or(Value::Nil)
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Sync state variables back
        for (slot, key) in &cc.state_locals {
            let local_name = &cc.locals[*slot];
            let val = self
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            loan_env!(self, set_state_var(key.clone(), val));
        }

        let attrs_adjusted;
        if can_skip_merge {
            // Phase 3 Stage 2: all attributes (scalar/array/hash) are reconciled
            // against the live cell + local/env writes before the env is torn
            // down. The cell-direct reads + per-op mirrors make the cell the
            // single source; the legacy attribute writeback is gone.
            attrs_adjusted = self.reconcile_attrs(&base, cc, &mut attributes);

            let method_var_bindings = self.take_var_bindings();
            let mut restored_bindings = saved_var_bindings;
            for (k, v) in method_var_bindings {
                restored_bindings.insert(k, v);
            }
            self.restore_var_bindings(restored_bindings);

            self.pop_routine();
            self.pop_method_class();
        } else {
            // Sync locals back to env
            for (i, local_name) in cc.locals.iter().enumerate() {
                if !local_name.is_empty() {
                    {
                        let __v = self.locals[i].clone();
                        self.env_mut().insert(local_name.clone(), __v);
                    }
                }
            }

            // Phase 3 Stage 2: reconcile all attributes against the live cell +
            // local/env writes before the env is merged away.
            attrs_adjusted = self.reconcile_attrs(&base, cc, &mut attributes);
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
                    self.env()
                        .get(param_name)
                        .cloned()
                        .or_else(|| {
                            let qualified = format!("{}::{}", owner_class, param_name);
                            self.env().get(&qualified).cloned()
                        })
                        .map(|val| (source_name.clone(), val))
                })
                .collect();

            // Take sole ownership of caller + callee envs (pop the frame for the
            // saved caller env, take the live callee env) so merge_method_env can
            // mutate the caller env in place without a deep copy.
            let frame = self.pop_call_frame();
            let current_env = self.take_env();
            let (mut merged_env, wrote_caller, changed_caller_locals) =
                merge_method_env(frame.saved_env, current_env, &method_local_keys);
            // Precise dirty signal (Slice 6.3): the caller only needs an
            // env->locals re-sync when this method actually merged a
            // caller-visible write (captured-outer var, global, &sub) or wrote
            // back an `is rw` param. A method that merged nothing leaves the
            // caller's slots coherent -> stays "pure" so the opcode skips the
            // env_dirty mark (no per-call locals pull).
            self.method_dispatch_pure = !wrote_caller && rw_writeback.is_empty();

            // Slice F (env<->locals coherence): a method that mutated a captured
            // outer lexical (`method bar { $Foo++ }` closing over an outer-block
            // `$Foo`) merged the new value into `merged_env` (which becomes
            // `self.env` below). Record those changed caller-visible names so the
            // CallMethod / CallMethodMut op — which holds the caller's `code` —
            // writes each value straight through to the caller's local slot,
            // dropping the dependency on the reverse `sync_locals_from_env` pull.
            for k in &changed_caller_locals {
                self.pending_rw_writeback_sources
                    .push(k.resolve().to_string());
            }

            for (source_name, val) in &rw_writeback {
                merged_env.insert(source_name.clone(), val.clone());
            }
            // Slice F: record the caller-source names this `is rw` method writeback
            // touched (the value now lives in `merged_env`, which becomes
            // `self.env` below) so the CallMethod / CallMethodMut op — which holds
            // the caller's `code` — can write each value straight through to the
            // caller's local slot, dropping the dependency on the reverse
            // `sync_locals_from_env` pull. See `apply_pending_rw_writeback`.
            if !rw_writeback.is_empty() {
                self.pending_rw_writeback_sources
                    .extend(rw_writeback.iter().map(|(source, _)| source.clone()));
            }

            let method_var_bindings = self.take_var_bindings();
            let mut restored_bindings = saved_var_bindings;
            for (k, v) in method_var_bindings {
                restored_bindings.insert(k, v);
            }
            self.restore_var_bindings(restored_bindings);

            self.pop_routine();
            self.pop_method_class();
            self.set_current_package(saved_package.clone());
            *self.env_mut() = merged_env;
        }

        if can_skip_merge {
            // No env writes possible -> the caller's slots stay coherent (pure).
            self.method_dispatch_pure = true;
            self.set_current_package(saved_package);
            let frame = self.pop_call_frame();
            *self.env_mut() = frame.saved_env;
        }

        let final_result = match result {
            Ok(()) => Ok(explicit_return.unwrap_or(ret_val)),
            Err(e) => Err(e),
        };

        // Apply return type spec (e.g. `--> 5` returns literal 5 from empty body)
        let final_result = if let Some(ref return_spec) = method_def.return_type {
            let effective_return_spec = loan_env!(self, resolved_type_capture_name(return_spec));
            self.finalize_return_with_spec(final_result, Some(effective_return_spec.as_str()))
        } else {
            match final_result {
                Ok(v) => Ok(v),
                Err(e) if e.return_value.is_some() && e.return_target_callable_id.is_none() => {
                    Ok(e.return_value.unwrap())
                }
                Err(e) => Err(e),
            }
        };

        // Adjust return value if it's the same instance (update attributes).
        // Only commit the reconcile snapshot when it was adjusted beyond the
        // cell contents (`:=` recovery): an unadjusted snapshot is already what
        // the cell holds, and writing it back would race with concurrent
        // cell-CAS / cell-direct writes from other threads (lost updates).
        final_result.map(|v| {
            let adjusted = match (&base, &v) {
                (
                    Value::Instance {
                        class_name,
                        attributes: base_attrs,
                        id: base_id,
                    },
                    Value::Instance { id: ret_id, .. },
                ) if base_id == ret_id => {
                    if attrs_adjusted {
                        Value::write_back_sharing(
                            base_attrs,
                            *class_name,
                            attributes.clone(),
                            *base_id,
                        )
                    } else {
                        Value::instance_sharing_cell(base_attrs, *class_name, *base_id)
                    }
                }
                _ => v,
            };
            (adjusted, attributes, attrs_adjusted)
        })
    }

    /// Phase 3 Stage 2c (iii): the shared attribute cell is the source of truth
    /// for ordinary attribute writes, so the post-method attribute map is its
    /// current contents.
    ///
    /// Every plain in-method attribute change now lands in the cell at write time:
    /// cell-direct scalar writes (`$!x = v`, Stage 2a), per-op mirrored array/hash
    /// mutations (`@!a.push`, `%!h<k>=`, Stage 2b), attributive params
    /// (`method m($!s)`, Stage 2c (i)), and sigilless aliases (`has $x`, Stage 2c
    /// (ii)); cross-frame and nested-frame mutations are visible through the same
    /// shared cell. The former entry-snapshot-vs-env value reconcile is gone.
    ///
    /// The one exception is a `:=`-bound attribute (`$!x := $outer`), which aliases
    /// an external `ContainerRef` held in env/locals rather than the cell. The
    /// cell-direct write path does not carry that binding, so recover it from
    /// env/locals here and let it win, keeping the alias alive past method exit.
    ///
    /// Returns `true` when the map was adjusted beyond the raw cell snapshot
    /// (a `:=` ContainerRef was recovered). When `false`, the cell's current
    /// contents are authoritative and `attributes` is left as the (stale) entry
    /// snapshot — committing a snapshot back would be a no-op at best, and a
    /// lost-update race at worst: a concurrent cell-CAS / cell-direct write
    /// from another thread between snapshot and commit would be clobbered by
    /// the stale whole-map write. Callers skip the commit then, and the rare
    /// consumer that needs the post-method map (proxy_fetch) re-snapshots the
    /// live cell on demand instead — so the common exit pays no `to_map()`
    /// (full attr-map clone) at all.
    fn reconcile_attrs(
        &self,
        base: &Value,
        code: &CompiledCode,
        attributes: &mut HashMap<String, Value>,
    ) -> bool {
        let Value::Instance {
            attributes: cell, ..
        } = base
        else {
            return false;
        };
        // Cheap pre-check: a `:=` attr override can only be observed as a
        // ContainerRef value in THIS frame's locals or env overlay (the bind
        // op writes it there). No ContainerRef anywhere -> skip the per-attr
        // candidate scan below (7 `format!`s + env lookups per attribute key,
        // ~5% of method-heavy profiles) entirely. Deliberately overlay-only:
        // a caller-frame ContainerRef (e.g. a boxed loop capture that happens
        // to share an attribute's bare name) is NOT a binding made by this
        // method and must not be adopted as one.
        let frame_has_container_ref = self
            .locals
            .iter()
            .any(|v| matches!(v, Value::ContainerRef(_)))
            || self
                .env()
                .overlay_iter()
                .any(|(_, v)| matches!(v, Value::ContainerRef(_)));
        if !frame_has_container_ref {
            return false;
        }
        // Scan for `:=`-bound attributes: their authoritative value is the
        // shared ContainerRef in env/locals, not the cell. Key iteration runs
        // under the cell's read guard (attr_env_or_local touches only
        // env/locals, never the cell), so no map clone is materialized when —
        // as in the overwhelmingly common case — no `:=` binding exists.
        let mut overrides: Vec<(String, Value)> = Vec::new();
        {
            let cell_map = cell.as_map();
            for k in cell_map.keys() {
                if k.starts_with(ATTR_ALIAS_META_PREFIX) {
                    continue;
                }
                let bare = k.rsplit('\0').next().unwrap_or(k);
                // Sigilless (`has $x` → bare `x`) and twigil (`$!x`/`@.x`/…) keys
                // may hold the `:=` ContainerRef alias.
                let candidates = [
                    bare.to_string(),
                    format!("!{}", bare),
                    format!(".{}", bare),
                    format!("@!{}", bare),
                    format!("@.{}", bare),
                    format!("%!{}", bare),
                    format!("%.{}", bare),
                ];
                for key in candidates {
                    if let Some(v) = self.attr_env_or_local(code, &key)
                        && v.is_container_ref()
                    {
                        overrides.push((k.clone(), v));
                        break;
                    }
                }
            }
        }
        if overrides.is_empty() {
            return false;
        }
        *attributes = cell.to_map();
        for (k, v) in overrides {
            attributes.insert(k, v);
        }
        true
    }

    /// Read the current value of `name` from the method's local slot if present,
    /// else from env.
    fn attr_env_or_local(&self, code: &CompiledCode, name: &str) -> Option<Value> {
        if let Some(slot) = code.locals.iter().position(|n| n == name) {
            return Some(self.locals[slot].clone());
        }
        self.env().get(name).cloned()
    }

    /// Phase 3 Stage 2c (i): mirror attributive parameters (`$!x`/`@!a`/`%!h`)
    /// into `self`'s shared attribute cell right after argument binding. The
    /// parameter binding writes the value to env/locals keyed by the attribute
    /// twigil name (`!x`, `@!a`, …); this pushes it through to the cell so that
    /// cell-direct body reads and cross-frame aliases observe it, taking the
    /// attributive-param case out of the exit-time `reconcile_attrs`.
    fn mirror_attributive_params_to_cell(
        &self,
        code: &CompiledCode,
        method_def: &crate::runtime::MethodDef,
    ) {
        for pd in &method_def.param_defs {
            if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") {
                continue;
            }
            if Self::attr_twigil_base(&pd.name).is_some() {
                self.mirror_attr_value_to_cell_by_name(code, &pd.name);
            }
        }
    }

    /// Mark a compiled method's `$` scalar parameters read-only, mirroring
    /// `bind_function_args_values`. Routine params are read-only by default
    /// unless `is rw`/`is copy`/`is raw`; `@`/`%` params and attribute-binding
    /// params (`$!x`/`$.x`) stay writable. Sigilless (`\x`) params are raw
    /// aliases. The light-frame fast path skips the slow path's readonly
    /// snapshot, so each newly-marked name is recorded on the current call frame
    /// and dropped in `pop_call_frame`.
    fn mark_fast_method_params_readonly(&mut self, method_def: &crate::runtime::MethodDef) {
        for pd in &method_def.param_defs {
            if pd.name.is_empty()
                || pd.name == "__type_only__"
                || pd.name == "__subsig__"
                || pd.sigilless
                || pd.is_invocant
                || pd.name.starts_with('!')
                || pd.name.starts_with('.')
                || pd.name.starts_with('@')
                || pd.name.starts_with('%')
            {
                continue;
            }
            if pd
                .traits
                .iter()
                .any(|t| t == "rw" || t == "copy" || t == "raw")
            {
                continue;
            }
            if self.readonly_vars().contains(pd.name.as_str()) {
                continue; // already read-only (e.g. caller-owned same name)
            }
            self.mark_readonly(&pd.name);
            if let Some(frame) = self.call_frames.last_mut() {
                frame.readonly_added.push(pd.name.clone());
            }
        }
    }

    /// Fast path for read-only compiled methods. Bypasses all env_mut() calls
    /// to avoid the ~12μs Arc::make_mut deep clone. Populates locals directly
    /// from source data (attributes, args, special variables).
    #[allow(clippy::too_many_arguments)]
    pub(super) fn call_compiled_method_fast(
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
    ) -> Result<(Value, HashMap<String, Value>, bool), RuntimeError> {
        if let Some(ref msg) = method_def.deprecated_message {
            loan_env!(
                self,
                check_deprecation_for_method_with_line(method_name, owner_class, msg, None,)
            );
        }

        self.push_light_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller so the `self`/`?CLASS`/param/attr
        // env writes below land in a fresh map (strong_count 1) instead of
        // `make_mut`-forking the inherited caller env — this removes the per-
        // method-call env deep copy (the bench-class cost). Reads fall through to
        // the parent; on return `set_env(saved_env)` (can_skip_merge) drops the
        // overlay and `merge_method_env` iterates it overlay-only (the method's
        // own writes, which is exactly what the merge needs). Gated to methods
        // with no inner closures so no closure/thread/gather body captures or
        // iterates the scoped env for a full lexical view.
        let use_scoped = cc.closure_compiled_codes.is_empty();
        if use_scoped {
            let parent = self.env().clone();
            let scoped = crate::env::Env::scoped_child(parent);
            self.set_env(scoped);
        }
        let saved_var_bindings = self.take_var_bindings();
        self.push_method_class(owner_class.to_string());

        let saved_package = self.current_package().to_string();
        if self.has_class_scoped_subs(receiver_class_name) {
            self.set_current_package(receiver_class_name.to_string());
        }

        // Compute role context without touching env
        let role_context: Option<String> = if self.is_role(owner_class) {
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
                if let Some(constraint) = pd.and_then(|pd| pd.type_constraint.as_ref())
                    && !self.type_matches_value(constraint, &val)
                {
                    // Type mismatch — fall back to slow path for proper error
                    self.restore_var_bindings(saved_var_bindings);
                    self.pop_method_class();
                    self.set_current_package(saved_package);
                    self.stack.truncate(saved_stack_depth);
                    let frame = self.pop_call_frame();
                    self.set_env(frame.saved_env);
                    return Err(RuntimeError::typecheck_binding_parameter(
                        param_name,
                        constraint,
                        crate::runtime::value_type_name(&val),
                        None,
                    ));
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

        // For can_skip_merge methods with no closures, reduce env inserts.
        // Only insert ?CLASS/?ROLE (used by ::?CLASS/::?ROLE resolution and
        // GetGlobal in non-compiled paths). Skip self, params, attrs, and other
        // method-local vars since all reads go through GetLocal.
        let skip_env_setup = can_skip_merge && cc.closure_compiled_codes.is_empty();
        if skip_env_setup {
            let env = self.env_mut();
            env.insert("self".to_string(), base.clone());
            env.insert("__ANON_STATE__".to_string(), base.clone());
            env.insert("?CLASS".to_string(), class_val.clone());
            env.insert("_".to_string(), any_val.clone());
            if let Some(ref role_name) = role_context {
                env.insert(
                    "?ROLE".to_string(),
                    Value::Package(crate::symbol::Symbol::intern(role_name)),
                );
            } else {
                env.remove("?ROLE");
            }
            for (param_name, param_val) in &param_values {
                env.insert(param_name.to_string(), param_val.clone());
            }
        } else {
            let env = self.env_mut();
            env.insert("self".to_string(), base.clone());
            env.insert("__ANON_STATE__".to_string(), base.clone());
            env.insert("?CLASS".to_string(), class_val.clone());
            env.insert("_".to_string(), any_val.clone());
            env.insert("!".to_string(), Value::Nil);
            env.insert(
                "__mutsu_callable_id".to_string(),
                Value::Int(method_callable_id as i64),
            );
            if let Some(ref role_name) = role_context {
                env.insert(
                    "?ROLE".to_string(),
                    Value::Package(crate::symbol::Symbol::intern(role_name)),
                );
            } else {
                env.remove("?ROLE");
            }
            // Array/hash attribute env copies are no longer materialized here:
            // reads are cell-direct (Stage 2b) and the mutating ops refresh
            // env/locals from the live cell pre-op (`array_hash_attr_env_snapshot`),
            // so a closure capturing this env resolves `@!a`/`%!h` through the
            // captured `self` instead of a stale env snapshot.
            for (param_name, param_val) in &param_values {
                env.insert(param_name.to_string(), param_val.clone());
            }
        }

        // Bind the method's implicit `*%_` slurpy (a Hash of the leftover named
        // args, empty when none). The compiler adds a `*%_` param to every method;
        // the slow `bind_function_args_values` path fills it, but this fast path
        // binds positionals by index and skips slurpy params, leaving `%_` as
        // `Any` -- so `|%_` would splat a stray positional. Compute it here and let
        // the locals-init loop fill the `%_` slot.
        let implicit_named_slurpy: Option<Value> = if method_def
            .param_defs
            .iter()
            .any(|pd| pd.slurpy && pd.name == "%_")
        {
            Some(Self::implicit_method_named_slurpy(
                &method_def.param_defs,
                &args,
            ))
        } else {
            None
        };

        // Bind the implicit `*%_` into env so a `%_` read finds it there first
        // (its method local is stored sigiled as "%_", which the bare-name local
        // lookup -- it strips the sigil and searches "_" -- can't resolve, falling
        // back to the topic `$_`).
        if let Some(slurpy) = &implicit_named_slurpy {
            self.env_mut().insert("%_".to_string(), slurpy.clone());
        }

        // Raku: routine parameters are read-only by default (`method m($x) { $x = 5 }`
        // dies). The slow path does this in `bind_function_args_values`; the fast
        // path binds params directly, so mark `$` scalar params read-only here.
        self.mark_fast_method_params_readonly(method_def);

        // Populate locals directly
        self.locals = vec![Value::Nil; cc.locals.len()];

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
                "!" => Value::Nil,
                "__mutsu_callable_id" => Value::Int(method_callable_id as i64),
                name => {
                    // Check params first (handles $_ invocant binding too)
                    if let Some((_, val)) = param_values.iter().find(|(n, _)| *n == name) {
                        val.clone()
                    } else if name == "_" {
                        any_val.clone()
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
                    else if let Some(val) = self.env().get(name) {
                        val.clone()
                    } else {
                        Value::Nil
                    }
                }
            };
        }

        // Load persisted state variable values
        for (slot, key) in &cc.state_locals {
            if let Some(val) = self.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        // Register `is default(...)` values for attribute variables
        for attr_name in attributes.keys() {
            if attr_name.contains('\0') || attr_name.starts_with(ATTR_ALIAS_META_PREFIX) {
                continue;
            }
            let default_val = self
                .class_attribute_default(owner_class, attr_name)
                .or_else(|| self.class_attribute_default(receiver_class_name, attr_name));
            if let Some(def) = default_val {
                self.set_var_default(&format!("!{}", attr_name), def.clone());
                self.set_var_default(&format!(".{}", attr_name), def.clone());
                self.set_var_default(&format!("@!{}", attr_name), def.clone());
                self.set_var_default(&format!("@.{}", attr_name), def.clone());
                self.set_var_default(&format!("%!{}", attr_name), def.clone());
                self.set_var_default(&format!("%.{}", attr_name), def);
            }
        }

        self.push_method_routine_with_location(
            owner_class.to_string(),
            method_name.to_string(),
            self.current_source_line(),
            self.current_source_file(),
        );

        // Execute bytecode (same as slow path)
        let let_mark = self.let_saves_len();
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
                        self.discard_let_saves(let_mark);
                        result = Ok(());
                        break;
                    }
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
                Err(e) if e.return_value.is_some() => {
                    if let Some(target_id) = e.return_target_callable_id
                        && target_id != method_callable_id
                    {
                        loan_env!(self, restore_let_saves(let_mark));
                        result = Err(e);
                        break;
                    }
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
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

        // Sync state variables
        for (slot, key) in &cc.state_locals {
            let val = self.locals[*slot].clone();
            loan_env!(self, set_state_var(key.clone(), val));
        }

        if !can_skip_merge {
            // Non-can_skip_merge: AssignExpr may have written attribute values to
            // env. Sync locals→env first so the merge/reconcile sees them.
            for (i, local_name) in cc.locals.iter().enumerate() {
                if local_name.starts_with('.')
                    || local_name.starts_with('!')
                    || local_name.starts_with("@!")
                    || local_name.starts_with("@.")
                    || local_name.starts_with("%!")
                    || local_name.starts_with("%.")
                {
                    {
                        let __v = self.locals[i].clone();
                        self.env_mut().insert(local_name.clone(), __v);
                    }
                }
            }
        }
        // Phase 3 Stage 2: reconcile all attributes against the live cell +
        // local/env writes before the env is torn down.
        let attrs_adjusted = self.reconcile_attrs(&base, cc, &mut attributes);

        let method_var_bindings = self.take_var_bindings();
        let mut restored_bindings = saved_var_bindings;
        for (k, v) in method_var_bindings {
            restored_bindings.insert(k, v);
        }
        self.restore_var_bindings(restored_bindings);

        self.pop_routine();
        self.pop_method_class();
        self.set_current_package(saved_package);

        if can_skip_merge {
            // No env writes possible — just restore saved env (pure: caller
            // slots stay coherent, opcode skips the env_dirty mark).
            self.method_dispatch_pure = true;
            let frame = self.pop_call_frame();
            self.set_env(frame.saved_env);
        } else {
            // Inner calls may have modified env (globals, dynamics).
            // Check if env was actually changed; if so, merge the changes
            // into the saved env before restoring.
            let frame = self.pop_call_frame();
            if self.env().ptr_eq(&frame.saved_env) {
                // Env object unchanged -> nothing merged back (pure).
                self.method_dispatch_pure = true;
                self.set_env(frame.saved_env);
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
                // Own both envs (frame already popped above; take the live callee
                // env) so the merge mutates the caller env in place, no deep copy.
                let current_env = self.take_env();
                let (merged, wrote_caller, changed_caller_locals) =
                    merge_method_env(frame.saved_env, current_env, &method_local_keys);
                // Precise dirty signal (Slice 6.3): re-sync the caller's locals
                // only when the method merged a caller-visible write.
                self.method_dispatch_pure = !wrote_caller;
                // Slice F: write captured-outer mutations straight through to the
                // caller's local slot (see the primary merge path above).
                for k in &changed_caller_locals {
                    self.pending_rw_writeback_sources
                        .push(k.resolve().to_string());
                }
                self.set_env(merged);
            }
        }

        let final_result = match result {
            Ok(()) => Ok(explicit_return.unwrap_or(ret_val)),
            Err(e) => Err(e),
        };

        let final_result = if let Some(ref return_spec) = method_def.return_type {
            let effective_return_spec = loan_env!(self, resolved_type_capture_name(return_spec));
            self.finalize_return_with_spec(final_result, Some(effective_return_spec.as_str()))
        } else {
            match final_result {
                Ok(v) => Ok(v),
                Err(e) if e.return_value.is_some() && e.return_target_callable_id.is_none() => {
                    Ok(e.return_value.unwrap())
                }
                Err(e) => Err(e),
            }
        };

        // Only commit the reconcile snapshot when `:=` recovery adjusted it
        // beyond the cell contents (see `call_compiled_method` exit).
        final_result.map(|v| {
            let adjusted = match (&base, &v) {
                (
                    Value::Instance {
                        class_name,
                        attributes: base_attrs,
                        id: base_id,
                    },
                    Value::Instance { id: ret_id, .. },
                ) if base_id == ret_id => {
                    if attrs_adjusted {
                        Value::write_back_sharing(
                            base_attrs,
                            *class_name,
                            attributes.clone(),
                            *base_id,
                        )
                    } else {
                        Value::instance_sharing_cell(base_attrs, *class_name, *base_id)
                    }
                }
                _ => v,
            };
            (adjusted, attributes, attrs_adjusted)
        })
    }
}

/// True if `name` could be the name of a caller compiled-local slot (and so a
/// change to it could make a caller slot stale). Excludes names that are never
/// stored as a local slot — compile-time/location vars (`?...`), pod (`=...`),
/// dynamic and pseudo-global names (anything containing `*`, e.g. `$*OUT`,
/// `*PERL`), and `__mutsu_` internal plumbing. Used by `merge_method_env` to keep
/// the env_dirty signal precise. Conservative: an ordinary lexical (`$x`, `@y`,
/// bare param, `!attr`) returns true so it is always re-synced when it changes.
fn could_name_caller_local(name: &str) -> bool {
    !(name.starts_with('?')
        || name.starts_with('=')
        || name.contains('*')
        || name.starts_with("__mutsu_"))
}

/// O(1), conservative "is this value provably unchanged?" check used by
/// `merge_method_env` to avoid flipping `env_dirty` for value-identical copies
/// (e.g. globals pulled in by a nested call's overlay flatten). Returns `true`
/// only when equality is cheap to prove: Arc-pointer identity for the heap
/// container/string types, value compare for the immutable scalars, and id
/// compare for instances. Any case it cannot cheaply prove returns `false`, so
/// the caller treats it as a possible change (a redundant pull at worst).
pub(crate) fn cheaply_unchanged(old: &Value, new: &Value) -> bool {
    match (old, new) {
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Num(a), Value::Num(b)) => a.to_bits() == b.to_bits(),
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Rat(an, ad), Value::Rat(bn, bd)) => an == bn && ad == bd,
        (Value::Package(a), Value::Package(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        (Value::Str(a), Value::Str(b)) => Arc::ptr_eq(a, b),
        (Value::Array(a, _), Value::Array(b, _)) => Arc::ptr_eq(a, b),
        (Value::Hash(a), Value::Hash(b)) => Arc::ptr_eq(a, b),
        // A `ContainerRef` cell is the shared-identity primitive of the
        // single-store design: `my @a := @b` installs the *same* cell into both
        // the env entry and the local slot for both names. Without this arm the
        // generic `_ => false` reported every same-cell pair as "changed", so the
        // reverse-sync survey over-counted bound containers as effective stale
        // (and `merge_method_env` over-signalled env_dirty for a method that
        // returned the same cell). Same Arc => genuinely unchanged; distinct Arcs
        // stay "changed" (conservative).
        (Value::ContainerRef(a), Value::ContainerRef(b)) => Arc::ptr_eq(a, b),
        (Value::Instance { id: a, .. }, Value::Instance { id: b, .. }) => a == b,
        _ => false,
    }
}

/// Merge the callee method frame's caller-visible overlay writes back into the
/// saved caller env. Returns the merged env and a flag that is `true` iff the
/// merge changed a value that could alias a caller compiled-local slot — the
/// precise signal (Slice 6.3) that the caller must re-sync its locals. A method
/// whose merge changed no such value leaves the caller's slots coherent, so no
/// env->locals pull is needed.
///
/// Takes ownership of both `saved` (the caller env) and `current` (the callee's
/// scoped overlay env). The callee's overlay is collected into a small `writes`
/// list, then `current` is dropped: this releases the callee's parent-chain
/// `Arc<Env>`, which (under the multi-tier overlay) shares `saved`'s overlay map.
/// With that reference gone, `saved` is the sole owner of its overlay, so the
/// in-place inserts mutate it without an `Arc::make_mut` deep copy -- the
/// per-nested-call O(env) cost measured in PR #2683. `saved` is consumed (the
/// caller discards it in favor of the returned env), so mutating it directly is
/// safe and avoids the extra `saved.clone()`.
fn merge_method_env(
    mut saved: Env,
    current: Env,
    method_local_keys: &HashSet<String>,
) -> (Env, bool, Vec<Symbol>) {
    let writes: Vec<(Symbol, Value)> = current
        .overlay_iter()
        .filter_map(|(k, v)| {
            // Skip keys introduced by the method frame (params, self, attributes,
            // locals) -- these must not leak back into the caller.
            if k.with_str(|s| method_local_keys.contains(s)) {
                return None;
            }
            let keep = saved.contains_key_sym(*k)
                || (k.starts_with("&") && !k.starts_with("&?"))
                || k.starts_with("__mutsu_method_value::")
                || k.starts_with("__mutsu_sigilless_alias::!")
                || k.starts_with("__mutsu_predictive_seq_iter::");
            keep.then(|| (*k, v.clone()))
        })
        .collect();
    drop(current);
    // Precise dirty signal (Slice 6.3): a merged write only obliges the caller to
    // re-pull its local slots if it actually *changes* a value the caller can see.
    // A nested method call flattens the scoped overlay, which copies every parent
    // lexical/global (`@*ARGS`, `%*ENV`, `?LINE`, type names, ...) into the callee
    // overlay; those copies are kept by the merge but are value-identical to the
    // caller's, so they must not flip env_dirty. `cheaply_unchanged` proves
    // equality in O(1) (Arc-ptr identity for containers/Str, scalar compare for
    // immutables); anything it cannot prove unchanged counts as dirty
    // (conservative -> at worst a redundant pull, never a stale read).
    // Collect the keys whose merged value can name a caller compiled-local slot
    // and actually changed. `wrote` (the precise env_dirty signal) is just
    // "this list is non-empty"; the list itself drives the Slice F call-site
    // write-through so the caller's slot stays coherent without the reverse pull.
    let mut changed_caller_locals: Vec<Symbol> = Vec::new();
    for (k, v) in &writes {
        // Only a key that can name a caller compiled-local slot obliges a pull:
        // `sync_locals_from_env` iterates exactly `code.locals`. Compile-time
        // location vars (`?LINE`/`?FILE`/`?CLASS`...), pod (`=...`), dynamic /
        // global names (`$*...`, `*PERL`, ...) and `__mutsu_` plumbing are never
        // local slots, so a change to them never makes a caller slot stale. This
        // matters because `?LINE` legitimately differs between the caller line
        // and the method-body line, which would otherwise flip env_dirty on every
        // multi-line nested method call.
        if !k.with_str(could_name_caller_local) {
            continue;
        }
        let changed = match saved.get_sym(*k) {
            Some(old) => !cheaply_unchanged(old, v),
            None => true,
        };
        if changed {
            changed_caller_locals.push(*k);
        }
    }
    let wrote = !changed_caller_locals.is_empty();
    for (k, v) in writes {
        saved.insert_sym(k, v);
    }
    (saved, wrote, changed_caller_locals)
}

#[cfg(test)]
mod cheaply_unchanged_tests {
    use super::cheaply_unchanged;
    use crate::value::Value;
    use std::sync::{Arc, Mutex};

    #[test]
    fn container_ref_same_cell_is_unchanged() {
        // The single-store identity primitive: `my @a := @b` installs one shared
        // cell into both the env entry and the local slot. Comparing those two
        // (same Arc) must report "unchanged" so the reverse-sync survey does not
        // count a bound container as effective stale, and `merge_method_env` does
        // not over-signal env_dirty for a method returning the same cell.
        let cell = Arc::new(Mutex::new(Value::array(vec![Value::Int(1)])));
        let a = Value::ContainerRef(cell.clone());
        let b = Value::ContainerRef(cell);
        assert!(cheaply_unchanged(&a, &b));
    }

    #[test]
    fn container_ref_distinct_cells_are_changed() {
        // Distinct cells (even with equal contents) stay "changed" — the test is
        // conservative Arc identity, never deep value equality.
        let a = Value::ContainerRef(Arc::new(Mutex::new(Value::array(vec![Value::Int(1)]))));
        let b = Value::ContainerRef(Arc::new(Mutex::new(Value::array(vec![Value::Int(1)]))));
        assert!(!cheaply_unchanged(&a, &b));
    }
}
