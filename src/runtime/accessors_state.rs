//! Callable signature/composition and interpreter state accessors:
//! `our`/`state`/once vars, wrap chains, method/multi/proto dispatch frames.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn callable_signature(&self, callable: &Value) -> (Vec<String>, Vec<ParamDef>) {
        match callable {
            Value::Sub(data) => (data.params.clone(), data.param_defs.clone()),
            Value::Routine { name, .. } => {
                if let Some(def) = self.resolve_function(&name.resolve()) {
                    return (def.params.clone(), def.param_defs.clone());
                }
                if let Some(arity) = Self::inferred_operator_arity(&name.resolve()) {
                    let params = (0..arity).map(|i| format!("arg{}", i)).collect();
                    return (params, Vec::new());
                }
                // Well-known 0-arity terms, and slurpy (`*@args`) builtins whose
                // required-positional arity is 0, should report no parameters
                // (raku: `&warn.arity == 0`). Mustache's logger keys on
                // `&warn.?arity == 2`, so a spurious arity of 1 sent it down the
                // wrong (2-arg) branch.
                if matches!(
                    name.resolve().as_ref(),
                    "rand"
                        | "now"
                        | "time"
                        | "warn"
                        | "note"
                        | "say"
                        | "print"
                        | "put"
                        | "die"
                        | "fail"
                ) {
                    return (Vec::new(), Vec::new());
                }
                (vec!["arg0".to_string()], Vec::new())
            }
            _ => (vec!["arg0".to_string()], Vec::new()),
        }
    }

    pub(crate) fn infix_associativity(&self, full_name: &str) -> Option<String> {
        let fq = format!("{}::{}", self.current_package(), full_name);
        self.operator_assoc
            .get(&fq)
            .cloned()
            .or_else(|| self.operator_assoc.get(full_name).cloned())
            .or_else(|| {
                let global = format!("GLOBAL::{}", full_name);
                self.operator_assoc.get(&global).cloned()
            })
    }

    pub(crate) fn call_user_routine_direct(
        &mut self,
        full_name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(def) = self.resolve_function_with_alias(full_name, &args) {
            return self.call_function_def(&def, &args);
        }
        if let Some(err) = self.take_pending_dispatch_error() {
            return Err(err);
        }
        let env_name = format!("&{}", full_name);
        if let Some(callable) = self.env.get(&env_name).cloned() {
            return self.eval_call_on_value(callable, args);
        }
        // Try stripping package prefix (e.g., "Main::foo" -> "foo")
        // when the function is in the current or GLOBAL package.
        if let Some(pos) = full_name.rfind("::") {
            let short_name = &full_name[pos + 2..];
            if let Some(def) = self.resolve_function_with_alias(short_name, &args) {
                return self.call_function_def(&def, &args);
            }
            let env_short = format!("&{}", short_name);
            if let Some(callable) = self.env.get(&env_short).cloned() {
                return self.eval_call_on_value(callable, args);
            }
        }
        let suggestions = self.suggest_routine_names(full_name);
        Err(RuntimeError::undeclared_routine_symbols(
            full_name,
            format!("Unknown function: {}", full_name),
            suggestions,
        ))
    }

    pub(crate) fn compose_callables(&self, left: Value, right: Value) -> Value {
        use std::sync::atomic::{AtomicU64, Ordering};

        static COMPOSE_ID: AtomicU64 = AtomicU64::new(1_000_000);
        let id = COMPOSE_ID.fetch_add(1, Ordering::Relaxed);

        let (mut params, param_defs) = self.callable_signature(&right);
        if params.is_empty() {
            if !param_defs.is_empty() {
                params = param_defs.iter().map(|pd| pd.name.clone()).collect();
            } else {
                params = vec!["arg0".to_string()];
            }
        }

        let left_return_type = self.callable_return_type(&left);
        let mut env = crate::env::Env::new();
        env.insert("__mutsu_compose_left".to_string(), left);
        env.insert("__mutsu_compose_right".to_string(), right);
        if let Some(rt) = left_return_type {
            env.insert("__mutsu_return_type".to_string(), Value::str(rt));
        }

        Value::make_sub_with_id(
            Symbol::intern(""),
            Symbol::intern("<composed>"),
            params,
            param_defs,
            Vec::new(),
            false,
            env,
            id,
        )
    }

    pub(crate) fn env_mut(&mut self) -> &mut Env {
        &mut self.env
    }

    /// Get a cloned copy of the persisted closure env for a given closure id.
    pub(crate) fn get_closure_env_override(&self, id: u64) -> Option<crate::env::Env> {
        self.closure_env_overrides.get(&id).cloned()
    }

    /// Check whether a sub has an active (non-empty) wrap chain.
    pub(crate) fn has_wrap_chain(&self, sub_id: u64) -> bool {
        self.wrap_chains.get(&sub_id).is_some_and(|c| !c.is_empty())
    }

    /// Check whether we're already inside a wrap dispatch for a given sub.
    pub(crate) fn is_wrap_dispatching(&self, sub_id: u64) -> bool {
        self.wrap_dispatch_stack.iter().any(|f| f.sub_id == sub_id)
    }

    /// Find the sub_id and Sub value for a function name that has an active wrap chain.
    /// Returns the sub_id if a wrap chain exists for the given function name.
    pub(crate) fn wrap_sub_id_for_name(&self, name: &str) -> Option<u64> {
        for (sub_id, sub_name) in &self.wrap_sub_names {
            if sub_name == name && self.has_wrap_chain(*sub_id) {
                return Some(*sub_id);
            }
        }
        None
    }

    /// Get the original wrapped Sub value for a function name.
    /// Returns the Sub value stored when wrap was called, preserving the original sub_id.
    pub(crate) fn get_wrapped_sub(&self, name: &str) -> Option<Value> {
        self.wrap_name_to_sub.get(name).cloned()
    }

    pub(crate) fn get_our_var(&self, key: &str) -> Option<&Value> {
        self.our_vars.get(key)
    }

    pub(crate) fn our_vars_iter(&self) -> impl Iterator<Item = (&String, &Value)> {
        self.our_vars.iter()
    }

    pub(crate) fn set_our_var(&mut self, key: String, value: Value) {
        self.our_vars.insert(key, value);
    }

    pub(crate) fn get_state_var(&self, key: &str) -> Option<&Value> {
        self.state_vars.get(key)
    }

    pub(crate) fn set_state_var(&mut self, key: String, value: Value) {
        self.state_vars.insert(key, value);
    }

    /// Track C: get-or-create a shared `ContainerRef` cell for a `state` variable
    /// in `shared_vars` (the cross-thread store), keyed by `key`. Used while a
    /// thread is running so that concurrent calls to the same routine — e.g.
    /// `await (^3).map: { start f() }` where `f` has `state $n` — share one live
    /// cell instead of each thread initializing its own snapshot. The get-or-init
    /// is atomic under the `shared_vars` write lock, so the first caller seeds the
    /// cell (from `initial`) and the rest observe it. Returns the cell value.
    pub(crate) fn get_or_init_shared_state_cell(&self, key: &str, initial: Value) -> Value {
        let mut sv = self.shared_vars.write().unwrap();
        if let Some(existing) = sv.get(key)
            && matches!(existing, Value::ContainerRef(_))
        {
            return existing.clone();
        }
        let cell = initial.into_container_ref();
        sv.insert(key.to_string(), cell.clone());
        cell
    }

    /// Read per-closure-instance captured-variable state (hot closure-call path).
    pub(crate) fn get_closure_captured_state(&self, id: u64, name: Symbol) -> Option<&Value> {
        self.closure_captured_state.get(&(id, name))
    }

    /// Persist per-closure-instance captured-variable state (hot closure-call path).
    pub(crate) fn set_closure_captured_state(&mut self, id: u64, name: Symbol, value: Value) {
        self.closure_captured_state.insert((id, name), value);
    }

    pub(crate) fn current_once_scope(&self) -> Option<u64> {
        // When inside a closure call, __mutsu_callable_id identifies the
        // specific closure clone.  This must take priority over the
        // once_scope_stack so that `once` blocks inside closures called from
        // EVAL (where the stack depth > 1) still get a per-clone scope.
        match self.env.get("__mutsu_callable_id") {
            Some(Value::Int(id)) if *id >= 0 => Some(*id as u64),
            _ => self.once_scope_stack.last().copied(),
        }
    }

    pub(crate) fn push_once_scope(&mut self, scope: u64) {
        self.once_scope_stack.push(scope);
    }

    pub(crate) fn pop_once_scope(&mut self) {
        self.once_scope_stack.pop();
    }

    pub(crate) fn next_once_scope_id(&mut self) -> u64 {
        let scope = self.next_once_scope_id;
        self.next_once_scope_id += 1;
        scope
    }

    pub(crate) fn get_once_value(&self, key: &str) -> Option<&Value> {
        self.once_values.get(key)
    }

    pub(crate) fn set_once_value(&mut self, key: String, value: Value) {
        self.once_values.insert(key, value);
    }

    pub(crate) fn when_matched(&self) -> bool {
        self.when_matched
    }

    pub(crate) fn set_when_matched(&mut self, v: bool) {
        self.when_matched = v;
    }

    pub(crate) fn is_role(&self, name: &str) -> bool {
        self.registry().roles.contains_key(name)
    }

    pub(crate) fn push_method_class(&mut self, class_name: String) {
        self.method_class_stack.push(class_name);
    }

    pub(crate) fn pop_method_class(&mut self) {
        self.method_class_stack.pop();
    }

    pub(crate) fn method_class_stack_top(&self) -> Option<String> {
        self.method_class_stack.last().cloned()
    }

    /// Set up a method dispatch frame for nextsame/callsame support.
    /// Returns true if a frame was pushed (caller must call pop_method_dispatch).
    /// Also pushes a samewith context unconditionally for samewith() support.
    pub(crate) fn push_method_dispatch_frame(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: &[Value],
        invocant: Value,
    ) -> bool {
        // Always push samewith context so samewith() can find the method name/invocant
        self.samewith_context_stack
            .push((method_name.to_string(), Some(invocant.clone())));
        let all_candidates = self.resolve_all_methods_with_owner(receiver_class, method_name, args);
        // Fast path: with zero or one candidate there is nothing to defer to, so no
        // dispatch frame is ever pushed (the single candidate is the chosen one and
        // gets skipped, leaving `remaining` empty). Returning early here avoids the
        // per-call `function_body_fingerprint` work below — which Debug-traverses the
        // whole method body AST to derive a candidate identity — for the overwhelmingly
        // common single-method case. Mirrors `push_multi_dispatch_frame`'s `<= 1` guard.
        if all_candidates.len() <= 1 {
            return false;
        }
        // Identify the chosen candidate and skip exactly that one
        let chosen = self.resolve_method_with_owner(receiver_class, method_name, args);
        let chosen_fp = chosen.as_ref().map(|(_, def)| {
            crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body)
        });
        let mut remaining: Vec<(String, super::MethodDef)> = Vec::new();
        let mut skipped_chosen = false;
        for (owner, def) in all_candidates {
            let fp = crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
            if !skipped_chosen && Some(fp) == chosen_fp {
                skipped_chosen = true;
                continue;
            }
            if self.should_skip_defer_method_candidate(receiver_class, &owner) {
                continue;
            }
            remaining.push((owner, def));
        }
        let pushed = !remaining.is_empty();
        if pushed {
            let rw_params = chosen
                .as_ref()
                .map(|(_, def)| {
                    super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs)
                })
                .unwrap_or_default();
            self.method_dispatch_stack.push(super::MethodDispatchFrame {
                receiver_class: receiver_class.to_string(),
                invocant,
                args: args.to_vec(),
                remaining,
                rw_params,
            });
        }
        pushed
    }

    pub(crate) fn is_inside_wrap_dispatch(&self) -> bool {
        !self.wrap_dispatch_stack.is_empty()
    }

    pub(crate) fn has_any_wrap_chains(&self) -> bool {
        !self.method_wrap_chains.is_empty()
    }

    pub(crate) fn push_wrap_dispatch_frame(&mut self, frame: super::WrapDispatchFrame) {
        self.wrap_dispatch_stack.push(frame);
    }

    pub(crate) fn pop_wrap_dispatch_frame(&mut self) {
        self.wrap_dispatch_stack.pop();
    }

    /// Get method-level wrap chain for a specific candidate.
    pub(crate) fn get_method_wrap_chain(
        &self,
        class_name: &str,
        method_name: &str,
        candidate_idx: usize,
    ) -> Option<&Vec<(u64, Value)>> {
        let key = (
            class_name.to_string(),
            method_name.to_string(),
            candidate_idx,
        );
        self.method_wrap_chains.get(&key).filter(|c| !c.is_empty())
    }

    /// Find the candidate index for a method definition in its class.
    pub(crate) fn find_method_candidate_index(
        &self,
        class_name: &str,
        method_name: &str,
        method_def: &super::MethodDef,
    ) -> Option<usize> {
        // No user-code re-entry here, so a let-bound guard is safe.
        let registry = self.registry();
        let class_def = registry.classes.get(class_name)?;
        let defs = class_def.methods.get(method_name)?;
        let target_fp = crate::ast::function_body_fingerprint(
            &method_def.params,
            &method_def.param_defs,
            &method_def.body,
        );
        defs.iter().position(|d| {
            crate::ast::function_body_fingerprint(&d.params, &d.param_defs, &d.body) == target_fp
        })
    }

    /// Pop a method dispatch frame (must only be called if push returned true).
    pub(crate) fn pop_method_dispatch(&mut self) {
        self.method_dispatch_stack.pop();
    }

    /// Pop the samewith context pushed by push_method_dispatch_frame.
    /// Must always be called after push_method_dispatch_frame, regardless of its return value.
    pub(crate) fn pop_method_samewith_context(&mut self) {
        self.samewith_context_stack.pop();
    }

    /// Push a multi dispatch frame for callsame/nextsame/callwith/nextwith support.
    /// Returns true if a frame was pushed (i.e. there are remaining candidates).
    pub(crate) fn push_multi_dispatch_frame(&mut self, name: &str, args: &[Value]) -> bool {
        // Collect ALL multi candidates regardless of arg matching. This is
        // needed because callwith() can re-dispatch with different args, so
        // candidates that don't match the original args may match the new ones.
        let all_candidates = self.resolve_all_multi_candidates(name);
        if all_candidates.len() <= 1 {
            return false;
        }
        // Identify the candidate currently being called by the DETERMINISTIC
        // dispatch winner (the same resolver the interpreter's inline frame uses),
        // NOT a HashMap-ordered first match. `resolve_all_matching_candidates` is
        // HashMap-ordered, so its `.first()` is not reliably the winner: when the
        // narrowest candidate is declared after a broader one, callsame/nextsame
        // would redispatch to the wrong (or the same) candidate, flaking ~50% of
        // the time with the process hash seed. The winner is excluded from
        // `remaining` so redispatch targets the OTHER candidates.
        let saved_err = self.take_pending_dispatch_error();
        let current_fp = self.resolve_function_with_alias(name, args).map(|def| {
            crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body)
        });
        if let Some(err) = saved_err {
            self.set_pending_dispatch_error(err);
        }
        let remaining: Vec<std::sync::Arc<super::FunctionDef>> = all_candidates
            .into_iter()
            .filter(|c| {
                let fp = crate::ast::function_body_fingerprint(&c.params, &c.param_defs, &c.body);
                Some(fp) != current_fp
            })
            .collect();
        let pushed = !remaining.is_empty();
        if pushed {
            // Capture the FIRST (winning) candidate's scalar rw params so a
            // nextsame+rw redispatch can chain the rw value through it (§D).
            let rw_params = self
                .resolve_function_with_alias(name, args)
                .map(|def| {
                    super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs)
                })
                .unwrap_or_default();
            self.multi_dispatch_stack
                .push((name.to_string(), remaining, args.to_vec(), rw_params));
        }
        pushed
    }

    /// Pop a multi dispatch frame (must only be called if push returned true).
    pub(crate) fn pop_multi_dispatch(&mut self) {
        self.multi_dispatch_stack.pop();
    }

    /// Push a proto-sub dispatch frame so a compiled proto body's `{*}`
    /// (`__PROTO_DISPATCH__`) can read the original proto args when it
    /// redispatches to the winning multi candidate (ledger §D).
    pub(crate) fn push_proto_dispatch_frame(&mut self, name: String, args: Vec<Value>) {
        self.proto_dispatch_stack.push((name, args, None));
    }

    /// Pop the proto-sub dispatch frame pushed by `push_proto_dispatch_frame`.
    pub(crate) fn pop_proto_dispatch_frame(&mut self) {
        self.proto_dispatch_stack.pop();
    }

    /// Clone of the current proto-dispatch frame `(name, args, method_ctx)`, read
    /// by the VM-native `{*}` redispatch handler. `None` outside a proto body.
    #[allow(clippy::type_complexity)]
    pub(crate) fn proto_dispatch_last(
        &self,
    ) -> Option<(String, Vec<Value>, Option<super::ProtoMethodCtx>)> {
        self.proto_dispatch_stack.last().cloned()
    }

    /// Push a samewith context for a multi sub dispatch.
    pub(crate) fn push_samewith_context(&mut self, name: &str, invocant: Option<Value>) {
        self.samewith_context_stack
            .push((name.to_string(), invocant));
    }

    /// Pop a samewith context.
    pub(crate) fn pop_samewith_context(&mut self) {
        self.samewith_context_stack.pop();
    }

    #[allow(dead_code)]
    pub(crate) fn class_composed_roles(&self, class_name: &str) -> Option<Vec<String>> {
        self.registry()
            .class_composed_roles
            .get(class_name)
            .cloned()
    }

    #[allow(dead_code)]
    pub(crate) fn get_role_def(&self, role_name: &str) -> Option<super::RoleDef> {
        self.registry().roles.get(role_name).cloned()
    }

    pub(crate) fn class_role_param_bindings(
        &self,
        class_name: &str,
    ) -> Option<HashMap<String, Value>> {
        self.registry()
            .class_role_param_bindings
            .get(class_name)
            .cloned()
    }
}
