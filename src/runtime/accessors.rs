use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn should_hide_from_my_global_stash(&self, key: &str) -> bool {
        if key.starts_with('$')
            || key.starts_with('@')
            || key.starts_with('%')
            || key.starts_with('&')
        {
            return false;
        }
        self.need_hidden_classes.contains(key)
            || key
                .strip_prefix("GLOBAL::")
                .is_some_and(|name| self.need_hidden_classes.contains(name))
            || key
                .rsplit_once("::")
                .is_some_and(|(_, short)| self.need_hidden_classes.contains(short))
            || self
                .need_hidden_classes
                .iter()
                .any(|name| key.ends_with(&format!("::{name}")))
    }

    fn normalize_categorical_operator_name(name: &str) -> String {
        // Keep categorical operator names as-written. Parenthesized operators
        // like infix:<(==)> are distinct from infix:<==>.
        name.to_string()
    }

    fn inferred_operator_arity(name: &str) -> Option<usize> {
        if name.starts_with("infix:<") && name.ends_with('>') {
            return Some(2);
        }
        if (name.starts_with("prefix:<") || name.starts_with("postfix:<")) && name.ends_with('>') {
            return Some(1);
        }
        None
    }

    fn callable_return_type_inner(callable: &Value) -> Option<String> {
        match callable {
            Value::Sub(data) => match data.env.get("__mutsu_return_type") {
                Some(Value::Str(rt)) => Some(rt.to_string()),
                _ => None,
            },
            _ => None,
        }
    }

    pub(crate) fn callable_return_type(&self, callable: &Value) -> Option<String> {
        Self::callable_return_type_inner(callable)
    }

    pub(crate) fn routine_return_spec_by_name(&self, name: &str) -> Option<String> {
        let code_key = format!("&{}", name);
        for key in [code_key.as_str(), name] {
            if let Some(Value::Sub(data)) = self.env.get(key)
                && let Some(Value::Str(spec)) = data.env.get("__mutsu_return_type")
            {
                return Some(spec.to_string());
            }
        }
        // Also check the FunctionDef registry
        if let Some(def) = self.resolve_function(name)
            && let Some(ref rt) = def.return_type
        {
            return Some(rt.clone());
        }
        None
    }

    fn return_spec_scalar_name(spec: &str) -> Option<String> {
        let s = spec.trim();
        let rest = s.strip_prefix('$')?;
        if rest.is_empty()
            || !rest
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
        {
            return None;
        }
        Some(rest.to_string())
    }

    pub(crate) fn is_definite_return_spec(&self, spec: &str) -> bool {
        let s = spec.trim();
        if s.is_empty() {
            return false;
        }
        // Coercion types like Str(Numeric:D) or Foo:D() are type constraints, not definite returns
        if s.contains('(') && s.ends_with(')') {
            return false;
        }
        if Self::return_spec_scalar_name(s).is_some() {
            return true;
        }
        if matches!(s, "Nil" | "True" | "False" | "Empty" | "pi") {
            return true;
        }
        if s.starts_with('\"')
            || s.starts_with('\'')
            || s.chars().next().is_some_and(|c| c.is_ascii_digit())
            || (s.starts_with('-') && s[1..].chars().next().is_some_and(|c| c.is_ascii_digit()))
        {
            return true;
        }
        if crate::runtime::utils::is_known_type_constraint(s) || self.has_type(s) {
            return false;
        }
        if let Some(base) = s.strip_suffix(":D").or_else(|| s.strip_suffix(":U"))
            && (crate::runtime::utils::is_known_type_constraint(base) || self.has_type(base))
        {
            return false;
        }
        if s.chars().next().is_some_and(|c| c.is_ascii_uppercase())
            && s.chars()
                .all(|c| c.is_ascii_alphanumeric() || c == ':' || c == '_')
        {
            return false;
        }
        true
    }

    fn failure_value_to_error(exception: &Value) -> RuntimeError {
        let message = if let Value::Instance { attributes, .. } = exception {
            attributes
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| "Died".to_string())
        } else {
            "Died".to_string()
        };
        let mut err = RuntimeError::new(&message);
        err.exception = Some(Box::new(exception.clone()));
        err
    }

    pub(crate) fn failure_to_runtime_error_if_unhandled(
        &self,
        value: &Value,
    ) -> Option<RuntimeError> {
        let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
        else {
            return None;
        };
        if class_name != "Failure" {
            return None;
        }
        if value.is_failure_handled() {
            return None;
        }
        if let Some(exception) = attributes.get("exception") {
            return Some(Self::failure_value_to_error(exception));
        }
        Some(RuntimeError::new("Failed"))
    }

    pub(crate) fn fail_error_to_failure_value(&self, err: &RuntimeError) -> Value {
        let exception = err.exception.as_deref().cloned().unwrap_or_else(|| {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(err.message.clone()));
            Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
        });
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        // When UNDO phasers ran for this fail, the Failure is marked as handled
        // (Raku semantics: UNDO acts as a handler, so sinking the Failure won't throw).
        failure_attrs.insert("handled".to_string(), Value::Bool(err.fail_handled));
        let failure = Value::make_instance(Symbol::intern("Failure"), failure_attrs);
        // Register in the pending failure registry for $!.pending support
        if let Value::Instance { id, .. } = &failure {
            crate::value::register_pending_failure(*id, failure.clone());
        }
        failure
    }

    fn malformed_return_value_error(&self, value: &Value) -> RuntimeError {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
            && class_name == "Failure"
            && let Some(ex) = attributes.get("exception")
        {
            return Self::failure_value_to_error(ex);
        }
        RuntimeError::new("Malformed return value")
    }

    fn evaluate_definite_return_value(&mut self, spec: &str) -> Result<Value, RuntimeError> {
        if let Some(name) = Self::return_spec_scalar_name(spec) {
            return Ok(self.env.get(&name).cloned().unwrap_or(Value::Nil));
        }
        self.eval_eval_string(spec.trim())
    }

    fn sink_for_definite_return(&mut self, value: &Value) -> Result<(), RuntimeError> {
        match value {
            Value::LazyList(list) => {
                let items = self.force_lazy_list(list)?;
                for item in &items {
                    self.sink_for_definite_return(item)?;
                }
                Ok(())
            }
            _ if value.as_list_items().is_some() => {
                for item in value.as_list_items().unwrap().iter() {
                    self.sink_for_definite_return(item)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub(crate) fn prepare_definite_return_slot(&mut self, return_spec: Option<&str>) {
        let Some(spec) = return_spec else {
            return;
        };
        if !self.is_definite_return_spec(spec) {
            return;
        }
        if let Some(name) = Self::return_spec_scalar_name(spec) {
            self.env.insert(name, Value::Nil);
        }
    }

    pub(crate) fn finalize_return_with_spec(
        &mut self,
        result: Result<Value, RuntimeError>,
        return_spec: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        let Some(spec) = return_spec else {
            return match result {
                Ok(v) => Ok(v),
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                Err(e) => Err(e),
            };
        };
        if !self.is_definite_return_spec(spec) {
            let value = match result {
                Ok(v) => v,
                Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
                Err(e) => return Err(e),
            };
            return self.enforce_return_type_constraint(spec, value);
        }

        match result {
            Ok(v) => {
                self.sink_for_definite_return(&v)?;
                self.evaluate_definite_return_value(spec)
            }
            Err(e) if e.return_value.is_some() => {
                let explicit = e.return_value.unwrap();
                if !matches!(explicit, Value::Nil) {
                    return Err(self.malformed_return_value_error(&explicit));
                }
                self.evaluate_definite_return_value(spec)
            }
            Err(e) => Err(e),
        }
    }

    /// Create an X::TypeCheck::Return exception
    fn throw_type_check_return(&self, got: &Value) -> RuntimeError {
        let msg = format!(
            "Type check failed for return value; expected a type object coercion, got {}",
            super::utils::value_type_name(got)
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert("got".to_string(), got.clone());
        let exception = Value::make_instance(Symbol::intern("X::TypeCheck::Return"), attrs);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(exception));
        err
    }

    /// Create an X::Coerce::Impossible exception
    fn throw_coerce_impossible(&self, target: &str, got: &Value) -> RuntimeError {
        let msg = format!(
            "Impossible coercion from '{}' into '{}': no acceptable coercion method found",
            super::utils::value_type_name(got),
            target
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let exception = Value::make_instance(Symbol::intern("X::Coerce::Impossible"), attrs);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(exception));
        err
    }

    /// Check if a value is a Failure instance
    pub(crate) fn is_failure_value(value: &Value) -> bool {
        matches!(value, Value::Instance { class_name, .. } if class_name == "Failure")
    }

    /// Enforce a return type constraint on a return value.
    /// Handles coercion types like Str(Numeric:D), Foo:D(), and subset types.
    fn enforce_return_type_constraint(
        &mut self,
        spec: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let resolved_spec = self.resolved_type_capture_name(spec);
        let spec = resolved_spec.as_str();
        // Nil and Failure pass through unconditionally
        if matches!(value, Value::Nil) || Self::is_failure_value(&value) {
            return Ok(value);
        }

        // Check if this is a subset type
        if let Some(subset) = self.subsets.get(spec).cloned() {
            // For subsets with coercion base types, coerce first then check predicate
            let coerced = if subset.base.contains('(') && subset.base.ends_with(')') {
                self.enforce_coercion_return(&subset.base, value)?
            } else {
                // Non-coercion subset: just check the base type
                if !self.type_matches_value(&subset.base, &value) {
                    return Err(self.throw_type_check_return(&value));
                }
                value
            };
            // Check the where predicate if present
            if let Some(ref pred) = subset.predicate {
                let pred_clone = pred.clone();
                if !self.check_where_constraint(&pred_clone, &coerced) {
                    return Err(self.throw_type_check_return(&coerced));
                }
            }
            return Ok(coerced);
        }

        // Check if this is a coercion type like Str(Numeric:D) or Foo:D()
        if spec.contains('(') && spec.ends_with(')') {
            return self.enforce_coercion_return(spec, value);
        }

        // Plain type constraint (e.g., Str, Int:D)
        if !self.type_matches_value(spec, &value) {
            return Err(self.throw_type_check_return(&value));
        }
        Ok(value)
    }

    /// Enforce a coercion return type like Str(Numeric:D) or Foo:D().
    fn enforce_coercion_return(&mut self, spec: &str, value: Value) -> Result<Value, RuntimeError> {
        // Parse the coercion type: strip smiley from the whole spec first
        let (spec_no_smiley, outer_smiley) = super::types::strip_type_smiley(spec);
        let (full_target, source) = if let Some(open) = spec_no_smiley.find('(')
            && spec_no_smiley.ends_with(')')
        {
            let target = &spec_no_smiley[..open];
            let src = &spec_no_smiley[open + 1..spec_no_smiley.len() - 1];
            (target, if src.is_empty() { None } else { Some(src) })
        } else if let Some(open) = spec.find('(')
            && spec.ends_with(')')
        {
            let target = &spec[..open];
            let src = &spec[open + 1..spec.len() - 1];
            (target, if src.is_empty() { None } else { Some(src) })
        } else {
            return Ok(value);
        };

        // Determine definite requirement from target smiley
        let (base_target, target_smiley) = super::types::strip_type_smiley(full_target);
        let requires_definite = target_smiley == Some(":D") || outer_smiley == Some(":D");

        // If there's a source constraint, check it
        if let Some(src) = source {
            // The value must match the source type, OR already be of the target type
            if !self.type_matches_value(src, &value)
                && !self.type_matches_value(base_target, &value)
            {
                return Err(self.throw_type_check_return(&value));
            }
        }

        // If value already matches target type, return it directly
        if self.type_matches_value(full_target, &value) {
            return Ok(value);
        }

        // Try to coerce the value
        let coerced = self.try_coerce_value_for_return(base_target, value.clone())?;

        // Check if coercion produced a valid result
        if requires_definite && !super::types::value_is_defined(&coerced) {
            return Err(self.throw_coerce_impossible(spec, &value));
        }

        // Check if the coerced value actually matches the target type
        if !self.type_matches_value(base_target, &coerced) {
            return Err(self.throw_coerce_impossible(spec, &value));
        }

        // For subset targets, check the where constraint
        if let Some(subset) = self.subsets.get(base_target).cloned()
            && let Some(ref pred) = subset.predicate
        {
            let pred_clone = pred.clone();
            if !self.check_where_constraint(&pred_clone, &coerced) {
                return Err(self.throw_coerce_impossible(spec, &value));
            }
        }

        Ok(coerced)
    }

    /// Try to coerce a value for a return type constraint.
    /// For subset types, coerces to the subset's base type first.
    fn try_coerce_value_for_return(
        &mut self,
        target: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        // If target is a subset, coerce to the subset's base type
        if let Some(subset) = self.subsets.get(target).cloned() {
            let (base, _) = super::types::strip_type_smiley(&subset.base);
            return self.try_coerce_value_for_constraint(&format!("{}()", base), value);
        }
        self.try_coerce_value_for_constraint(&format!("{}()", target), value)
    }

    /// Check a where constraint against a value
    fn check_where_constraint(&mut self, pred: &Expr, value: &Value) -> bool {
        // Evaluate the predicate expression to get a callable value
        let pred_val = match self.eval_block_value(&[Stmt::Expr(pred.clone())]) {
            Ok(v) => v,
            Err(_) => return false,
        };
        // Call the predicate with the value
        match self.call_sub_value(pred_val, vec![value.clone()], false) {
            Ok(result) => result.truthy(),
            Err(_) => false,
        }
    }

    pub(crate) fn callable_signature(&self, callable: &Value) -> (Vec<String>, Vec<ParamDef>) {
        match callable {
            Value::Sub(data) => (data.params.clone(), data.param_defs.clone()),
            Value::Routine { name, .. } => {
                if let Some(def) = self.resolve_function(&name.resolve()) {
                    return (def.params, def.param_defs);
                }
                if let Some(arity) = Self::inferred_operator_arity(&name.resolve()) {
                    let params = (0..arity).map(|i| format!("arg{}", i)).collect();
                    return (params, Vec::new());
                }
                // Well-known 0-arity terms should report as having no parameters
                if matches!(name.resolve().as_ref(), "rand" | "now" | "time") {
                    return (Vec::new(), Vec::new());
                }
                (vec!["arg0".to_string()], Vec::new())
            }
            _ => (vec!["arg0".to_string()], Vec::new()),
        }
    }

    pub(crate) fn infix_associativity(&self, full_name: &str) -> Option<String> {
        let fq = format!("{}::{}", self.current_package, full_name);
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
        Err(RuntimeError::new(format!(
            "X::Undeclared::Symbols: Unknown function: {}",
            full_name
        )))
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

    pub(crate) fn set_our_var(&mut self, key: String, value: Value) {
        self.our_vars.insert(key, value);
    }

    pub(crate) fn get_state_var(&self, key: &str) -> Option<&Value> {
        self.state_vars.get(key)
    }

    pub(crate) fn set_state_var(&mut self, key: String, value: Value) {
        self.state_vars.insert(key, value);
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
        self.roles.contains_key(name)
    }

    pub(crate) fn push_method_class(&mut self, class_name: String) {
        self.method_class_stack.push(class_name);
    }

    pub(crate) fn pop_method_class(&mut self) {
        self.method_class_stack.pop();
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
            self.method_dispatch_stack.push(super::MethodDispatchFrame {
                receiver_class: receiver_class.to_string(),
                invocant,
                args: args.to_vec(),
                remaining,
            });
        }
        pushed
    }

    pub(crate) fn is_inside_wrap_dispatch(&self) -> bool {
        !self.wrap_dispatch_stack.is_empty()
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
        let class_def = self.classes.get(class_name)?;
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
        // Collect matching candidates (used to identify the current winner)
        let matching_candidates = self.resolve_all_matching_candidates(name, args);
        // Also collect ALL multi candidates regardless of arg matching.
        // This is needed because callwith() can re-dispatch with different args,
        // so candidates that don't match the original args may match the new ones.
        let all_candidates = self.resolve_all_multi_candidates(name);
        if matching_candidates.is_empty() && all_candidates.len() <= 1 {
            return false;
        }
        // The first matching candidate is the one currently being called;
        // remaining are all OTHER candidates (matching or not).
        let current_fp = matching_candidates.first().map(|def| {
            crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body)
        });
        let remaining: Vec<super::FunctionDef> = all_candidates
            .into_iter()
            .filter(|c| {
                let fp = crate::ast::function_body_fingerprint(&c.params, &c.param_defs, &c.body);
                Some(fp) != current_fp
            })
            .collect();
        let pushed = !remaining.is_empty();
        if pushed {
            self.multi_dispatch_stack
                .push((name.to_string(), remaining, args.to_vec()));
        }
        pushed
    }

    /// Pop a multi dispatch frame (must only be called if push returned true).
    pub(crate) fn pop_multi_dispatch(&mut self) {
        self.multi_dispatch_stack.pop();
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
    pub(crate) fn class_composed_roles(&self, class_name: &str) -> Option<&Vec<String>> {
        self.class_composed_roles.get(class_name)
    }

    #[allow(dead_code)]
    pub(crate) fn get_role_def(&self, role_name: &str) -> Option<&super::RoleDef> {
        self.roles.get(role_name)
    }

    pub(crate) fn class_role_param_bindings(
        &self,
        class_name: &str,
    ) -> Option<&HashMap<String, Value>> {
        self.class_role_param_bindings.get(class_name)
    }

    /// Compile all uncompiled method bodies in a method map.
    fn compile_methods_for_map(
        methods: &mut HashMap<String, Vec<super::MethodDef>>,
        package_name: &str,
    ) {
        let mut to_compile = Vec::new();
        for (method_name, overloads) in methods.iter() {
            for (idx, def) in overloads.iter().enumerate() {
                if def.compiled_code.is_none() && !def.body.is_empty() {
                    let mut compiler = crate::compiler::Compiler::new();
                    // For $?PACKAGE: use the original role name if this method
                    // was composed from a role, otherwise use the class/package name.
                    let method_package = def
                        .original_role
                        .as_deref()
                        .or(def.role_origin.as_deref())
                        .unwrap_or(package_name);
                    compiler.set_current_package(method_package.to_string());
                    let cc = compiler.compile_routine_closure_body(
                        &def.params,
                        &def.param_defs,
                        &def.body,
                    );
                    to_compile.push((method_name.clone(), idx, std::sync::Arc::new(cc)));
                }
            }
        }
        for (method_name, idx, arc_cc) in to_compile {
            if let Some(overloads) = methods.get_mut(&method_name)
                && let Some(def) = overloads.get_mut(idx)
            {
                def.compiled_code = Some(arc_cc);
            }
        }
    }

    /// Check all methods in a class for assignment to native-typed read-only
    /// parameters. Returns the first error found, or None.
    pub(crate) fn check_class_native_readonly_param_errors(
        &self,
        class_name: &str,
    ) -> Option<crate::value::RuntimeError> {
        let class_def = self.classes.get(class_name)?;
        for overloads in class_def.methods.values() {
            for def in overloads {
                if let Some(err_val) =
                    crate::compiler::Compiler::check_native_readonly_param_assignment(
                        &def.param_defs,
                        &def.body,
                    )
                {
                    let msg = if let Value::Instance { attributes, .. } = &err_val {
                        attributes
                            .get("message")
                            .map(|v| v.to_string_value())
                            .unwrap_or_else(|| "Cannot assign to readonly variable".to_string())
                    } else {
                        "Cannot assign to readonly variable".to_string()
                    };
                    let mut err = crate::value::RuntimeError::new(msg);
                    err.exception = Some(Box::new(err_val));
                    return Some(err);
                }
            }
        }
        None
    }

    /// Compile method bodies for a given class using the bytecode compiler.
    pub(crate) fn compile_class_methods(&mut self, class_name: &str) {
        if let Some(class_def) = self.classes.get_mut(class_name) {
            Self::compile_methods_for_map(&mut class_def.methods, class_name);
        }
    }

    /// Compile method bodies for a given role.
    pub(crate) fn compile_role_methods(&mut self, role_name: &str) {
        if let Some(role_def) = self.roles.get_mut(role_name) {
            Self::compile_methods_for_map(&mut role_def.methods, role_name);
        }
    }

    pub(crate) fn smart_match_values(&mut self, left: &Value, right: &Value) -> bool {
        self.smart_match(left, right)
    }

    pub(crate) fn eval_sequence_values(
        &mut self,
        left: Value,
        right: Value,
        exclude_end: bool,
    ) -> Result<Value, RuntimeError> {
        self.eval_sequence(left, right, exclude_end)
    }

    pub(crate) fn resolve_code_var(&self, name: &str) -> Value {
        let normalized_name = Self::normalize_categorical_operator_name(name);
        if (normalized_name.starts_with("infix:<")
            || normalized_name.starts_with("prefix:<")
            || normalized_name.starts_with("postfix:<"))
            && normalized_name.ends_with('>')
        {
            return Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(&normalized_name),
                is_regex: false,
            };
        }
        // Handle package-qualified names: strip pseudo-package prefixes and
        // resolve the bare function name.
        let bare_name = Self::strip_pseudo_packages(name);
        let has_packages = bare_name != name;
        let lookup_name = bare_name.strip_prefix('*').unwrap_or(bare_name);
        if bare_name == "?ROUTINE" {
            // Skip pointy-block entries to find the enclosing routine
            let entry = self
                .routine_stack
                .iter()
                .rev()
                .find(|(_, name)| name != "<pointy-block>");
            if let Some((package, routine)) = entry {
                return Value::Routine {
                    package: Symbol::intern(package),
                    name: Symbol::intern(routine),
                    is_regex: false,
                };
            }
            return Value::Nil;
        }
        // When SETTING:: (or similar) pseudo-packages are present, resolve to
        // the builtin directly — these refer to the outer setting scope, not
        // user-defined overrides.
        // When pseudo-package qualifiers are present (e.g. SETTING::), resolve
        // to the builtin directly, bypassing user-defined overrides.
        if has_packages && Self::is_builtin_function(lookup_name) {
            return Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(lookup_name),
                is_regex: false,
            };
        }
        // For &-sigil private attribute access (e.g. &!m), the attribute
        // value is stored in env as "!m" (not "&!m"), so check directly.
        if bare_name.starts_with('!')
            && let Some(val) = self.env.get(bare_name)
        {
            return val.clone();
        }
        // Check if stored as a variable first (my &f = ...)
        let var_key = format!("&{}", bare_name);
        if let Some(val) = self.env.get(&var_key) {
            // Upgrade WeakSub references (e.g., &?BLOCK) to strong Sub
            if let Value::WeakSub(weak) = val {
                return match weak.upgrade() {
                    Some(strong) => Value::Sub(strong),
                    None => Value::Nil,
                };
            }
            return val.clone();
        }
        // `return` is a control-flow keyword that also resolves as &return
        // so that it can be rebound (proxied return pattern).
        if bare_name == "return" {
            return Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern("return"),
                is_regex: false,
            };
        }
        // Look up as a function reference (including multi subs).
        // When pseudo-packages are present (e.g. OUR::, GLOBAL::), also check
        // our_scoped_functions for subs defined in EVAL that don't leak into
        // the regular functions map.
        let def = self.resolve_function(lookup_name).or_else(|| {
            if has_packages {
                let fq = format!("{}::{}", self.current_package, lookup_name);
                self.our_scoped_functions
                    .get(&Symbol::intern(&fq))
                    .cloned()
                    .or_else(|| {
                        let global_fq = format!("GLOBAL::{}", lookup_name);
                        self.our_scoped_functions
                            .get(&Symbol::intern(&global_fq))
                            .cloned()
                    })
            } else {
                None
            }
        });
        let is_multi = if def.is_none() {
            // Check if there are multi-dispatch variants (stored with arity/type suffixes)
            let prefix_local = format!("{}::{}/", self.current_package, lookup_name);
            let prefix_global = format!("GLOBAL::{}/", lookup_name);
            self.functions.keys().any(|k| {
                let ks = k.resolve();
                ks.starts_with(&prefix_local) || ks.starts_with(&prefix_global)
            })
        } else {
            false
        };
        if is_multi {
            // Multi subs should resolve via the dispatcher at call time
            Value::Routine {
                package: Symbol::intern(&self.current_package),
                name: Symbol::intern(lookup_name),
                is_regex: false,
            }
        } else if self.has_proto(lookup_name)
            || self.resolve_token_defs(lookup_name).is_some()
            || self.has_proto_token(lookup_name)
        {
            Value::Routine {
                package: Symbol::intern(&self.current_package),
                name: Symbol::intern(lookup_name),
                is_regex: self.resolve_token_defs(lookup_name).is_some()
                    || self.has_proto_token(lookup_name),
            }
        } else if let Some(def) = def {
            let mut captured_env = self.env.clone();
            if let Some(ref return_type) = def.return_type {
                captured_env.insert(
                    "__mutsu_return_type".to_string(),
                    Value::str(return_type.clone()),
                );
            }
            if def.is_method {
                captured_env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from("Method"),
                );
            }
            let empty_sig = def.empty_sig;
            let mut sub_val = Value::make_sub(
                def.package,
                def.name,
                def.params,
                def.param_defs,
                def.body,
                def.is_rw,
                captured_env,
            );
            // Preserve empty_sig from the FunctionDef so that arity checks
            // (e.g. sort rejecting 0-arity callables) work correctly.
            if empty_sig && let Value::Sub(ref data) = sub_val {
                let mut new_data = (**data).clone();
                new_data.empty_sig = true;
                sub_val = Value::Sub(Arc::new(new_data));
            }
            sub_val
        } else if Self::is_builtin_function(lookup_name) {
            Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(lookup_name),
                is_regex: false,
            }
        } else if bare_name.starts_with('*') {
            // Dynamic code vars (&*foo) can point to routines that are resolved
            // at call time (including builtins not listed in is_builtin_function).
            Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(lookup_name),
                is_regex: false,
            }
        } else {
            Value::Nil
        }
    }

    /// Strip pseudo-package prefixes (SETTING::, OUTER::, CALLER::, CORE::, etc.)
    /// from a qualified name and return the final bare function name.
    pub(crate) fn strip_pseudo_packages(name: &str) -> &str {
        let pseudo = [
            "SETTING", "CALLER", "OUTER", "CORE", "GLOBAL", "MY", "OUR", "DYNAMIC", "UNIT",
        ];
        let mut rest = name;
        loop {
            let mut found = false;
            for pkg in &pseudo {
                if let Some(after) = rest.strip_prefix(pkg)
                    && let Some(after) = after.strip_prefix("::")
                {
                    rest = after;
                    found = true;
                    break;
                }
            }
            if !found {
                break;
            }
        }
        rest
    }

    pub(crate) fn routine_stack_top(&self) -> Option<&(String, String)> {
        self.routine_stack.last()
    }

    pub(crate) fn routine_stack(&self) -> &[(String, String)] {
        &self.routine_stack
    }

    pub(crate) fn push_routine(&mut self, package: String, name: String) {
        self.routine_stack.push((package, name));
    }

    pub(crate) fn pop_routine(&mut self) {
        self.routine_stack.pop();
    }

    pub(crate) fn block_stack_top(&self) -> Option<&Value> {
        self.block_stack.last()
    }

    pub(crate) fn push_block(&mut self, val: Value) {
        self.block_stack.push(val);
    }

    pub(crate) fn pop_block(&mut self) {
        self.block_stack.pop();
    }

    /// Stringify a value, calling the `.Str` method for Instance and Package types.
    pub(crate) fn stringify_value(&mut self, value: Value) -> Result<String, RuntimeError> {
        match &value {
            Value::Instance { .. } | Value::Package(_) => {
                let result = self.call_method_with_values(value, "Str", vec![])?;
                Ok(result.to_string_value())
            }
            _ => Ok(value.to_string_value()),
        }
    }

    /// Check if a value can respond to a given method name.
    pub(crate) fn value_can_method(&mut self, value: &Value, method: &str) -> bool {
        // Check builtin 0-arg method (covers most built-in methods)
        if crate::builtins::native_method_0arg(value, crate::symbol::Symbol::intern(method))
            .is_some()
        {
            return true;
        }
        // For instances, check class methods
        if let Value::Instance { class_name, .. } = value
            && self.class_has_method(&class_name.resolve(), method)
        {
            return true;
        }
        // Universal methods available on all values
        matches!(
            method,
            "WHAT"
                | "say"
                | "print"
                | "put"
                | "gist"
                | "Str"
                | "Int"
                | "Num"
                | "Bool"
                | "Numeric"
                | "Real"
                | "so"
                | "not"
                | "defined"
                | "isa"
                | "can"
                | "does"
                | "ACCEPTS"
                | "raku"
                | "perl"
                | "clone"
                | "new"
        )
    }

    pub(crate) fn take_value(&mut self, val: Value) -> Result<(), RuntimeError> {
        if let Some(items) = self.gather_items.last_mut() {
            items.push(val);
            if let Some(Some(limit)) = self.gather_take_limits.last()
                && items.len() >= *limit
            {
                return Err(RuntimeError::new(
                    "__mutsu_lazy_gather_take_limit_reached__",
                ));
            }
        }
        Ok(())
    }

    pub(crate) fn gather_items_len(&self) -> usize {
        self.gather_items.len()
    }

    pub(crate) fn push_gather_items(&mut self, items: Vec<Value>) {
        self.gather_items.push(items);
    }

    pub(crate) fn pop_gather_items(&mut self) -> Option<Vec<Value>> {
        self.gather_items.pop()
    }

    pub(crate) fn push_gather_take_limit(&mut self, limit: Option<usize>) {
        self.gather_take_limits.push(limit);
    }

    pub(crate) fn pop_gather_take_limit(&mut self) {
        self.gather_take_limits.pop();
    }

    pub(crate) fn current_package(&self) -> &str {
        &self.current_package
    }

    pub(crate) fn set_current_package(&mut self, pkg: String) {
        self.current_package = pkg;
    }

    fn stash_symbol_key_from_env_tail(rest: &str) -> String {
        if rest.starts_with('$')
            || rest.starts_with('@')
            || rest.starts_with('%')
            || rest.starts_with('&')
        {
            return rest.to_string();
        }
        if rest.contains("::") || rest.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
            return rest.to_string();
        }
        format!("${rest}")
    }

    fn stash_member_tail<'a>(key: &'a str, package: &str) -> Option<&'a str> {
        let package = package.trim_end_matches("::");
        if package == "GLOBAL" {
            return Some(key);
        }
        let direct = format!("{package}::");
        if let Some(rest) = key.strip_prefix(&direct) {
            return Some(rest);
        }
        let needle = format!("::{package}::");
        if let Some(idx) = key.rfind(&needle) {
            let start = idx + needle.len();
            return Some(&key[start..]);
        }
        None
    }

    fn make_stash_instance(package: &str, symbols: HashMap<String, Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(package.to_string()));
        attrs.insert("symbols".to_string(), Value::hash(symbols));
        Value::make_instance(Symbol::intern("Stash"), attrs)
    }

    fn package_export_tag_parts(package: &str) -> Option<(&str, &str)> {
        let (module, rest) = package.split_once("::EXPORT::")?;
        if module.is_empty() || rest.is_empty() || rest.contains("::") {
            return None;
        }
        Some((module, rest))
    }

    fn package_export_module(package: &str) -> Option<&str> {
        package.strip_suffix("::EXPORT")
    }

    fn qualify_stash_name(package: &str, symbol: &str) -> String {
        let package = package.trim_end_matches("::");
        if package.is_empty() || package == "GLOBAL" {
            symbol.to_string()
        } else {
            format!("{package}::{symbol}")
        }
    }

    fn normalize_stash_package(package: &str) -> String {
        let trimmed = package.trim_end_matches("::");
        if let Some(inner) = trimmed
            .strip_prefix("GLOBAL[")
            .and_then(|s| s.strip_suffix(']'))
        {
            inner.to_string()
        } else {
            trimmed.to_string()
        }
    }

    fn has_package_members(&self, package: &str) -> bool {
        let prefix = format!("{package}::");
        self.env.keys().any(|k| k.starts_with(&prefix))
            || self
                .functions
                .keys()
                .any(|k| k.resolve().starts_with(&prefix))
            || self.classes.keys().any(|k| k.starts_with(&prefix))
            || self.exported_subs.contains_key(package)
            || self.exported_vars.contains_key(package)
    }

    fn stash_lookup_symbol(stash: &Value, key: &str) -> Option<Value> {
        let Value::Instance {
            class_name,
            attributes,
            ..
        } = stash
        else {
            return None;
        };
        if class_name != "Stash" {
            return None;
        }
        let Value::Hash(symbols) = attributes.get("symbols")? else {
            return None;
        };
        if let Some(value) = symbols.get(key) {
            return Some(value.clone());
        }
        if !key.starts_with('$')
            && !key.starts_with('@')
            && !key.starts_with('%')
            && !key.starts_with('&')
        {
            let scalar = format!("${key}");
            if let Some(value) = symbols.get(&scalar) {
                return Some(value.clone());
            }
        }
        None
    }

    fn no_such_symbol_failure(name: &str) -> Value {
        let mut ex_attrs = HashMap::new();
        ex_attrs.insert(
            "message".to_string(),
            Value::str(format!("No such symbol '{name}'")),
        );
        let exception = Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs);
        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::Bool(false));
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    pub(crate) fn resolve_indirect_type_name(&self, name: &str) -> Value {
        if name.is_empty() {
            return Self::no_such_symbol_failure(name);
        }
        if let Some(code_name) = name.strip_prefix('&') {
            let val = self.resolve_code_var(code_name);
            // When the code variable is not found via ::('&name'), return a
            // Failure (like Raku's X::NoSuchSymbol) so that attempting to use
            // the result throws an exception.
            if matches!(val, Value::Nil) {
                return Self::no_such_symbol_failure(name);
            }
            return val;
        }
        // Scalars are stored without the `$` sigil in the env; strip it for lookup.
        if let Some(bare) = name.strip_prefix('$')
            && let Some(value) = self.env.get(bare)
            && !matches!(value, Value::Nil)
        {
            return value.clone();
        }
        if let Some(value) = self.env.get(name)
            && !matches!(value, Value::Nil)
        {
            return value.clone();
        }
        // Fallback: check persistent `our`-scoped variables (constants, `our` decls)
        // which may have been removed from the lexical env by block-scope restoration.
        if let Some(bare) = name.strip_prefix('$')
            && let Some(value) = self.our_vars.get(bare)
            && !matches!(value, Value::Nil)
        {
            return value.clone();
        }
        if let Some(value) = self.our_vars.get(name)
            && !matches!(value, Value::Nil)
        {
            return value.clone();
        }
        // Look up well-known numerical constants
        match name {
            "e" | "\u{1D452}" => return Value::Num(std::f64::consts::E),
            "pi" => return Value::Num(std::f64::consts::PI),
            "tau" | "\u{03C4}" => return Value::Num(std::f64::consts::TAU),
            _ => {}
        }
        if !self.method_class_stack.is_empty() && self.loaded_modules.contains(name) {
            return Value::Package(Symbol::intern(name));
        }

        // Check if the full compound name (e.g. "IO::Path") is a known type
        // before splitting on "::".
        if name.contains("::")
            && (crate::runtime::utils::is_known_compound_type(name)
                || self.has_class(name)
                || self.is_role(name))
        {
            return Value::Package(Symbol::intern(name));
        }

        let mut parts = name.split("::").filter(|part| !part.is_empty());
        let Some(first) = parts.next() else {
            return Self::no_such_symbol_failure(name);
        };

        let mut current = if let Some(value) = self.env.get(first)
            && !matches!(value, Value::Nil)
        {
            value.clone()
        } else if crate::runtime::utils::is_known_type_constraint(first)
            || (name.contains("::")
                && (self.has_package_members(first)
                    || self.has_class(first)
                    || self.is_role(first)))
        {
            Value::Package(Symbol::intern(first))
        } else {
            return Self::no_such_symbol_failure(name);
        };

        for part in parts {
            current = match &current {
                Value::Package(package) => {
                    let stash = self.package_stash_value(&package.resolve());
                    if let Some(value) = Self::stash_lookup_symbol(&stash, part) {
                        value
                    } else {
                        return Self::no_such_symbol_failure(name);
                    }
                }
                Value::Instance { class_name, .. } if class_name == "Stash" => {
                    if let Some(value) = Self::stash_lookup_symbol(&current, part) {
                        value
                    } else {
                        return Self::no_such_symbol_failure(name);
                    }
                }
                _ => return Self::no_such_symbol_failure(name),
            };
        }

        current
    }

    pub(crate) fn package_stash_value(&self, package: &str) -> Value {
        let package_name = Self::normalize_stash_package(package);

        // PROCESS:: pseudo-package: exposes process-level dynamic variables
        // like $*PROGRAM, $*PID, %*ENV, @*ARGS, etc.
        // PROCESS::<$PROGRAM> looks up key "$PROGRAM" in the stash.
        if package_name == "PROCESS" {
            let mut symbols: HashMap<String, Value> = HashMap::new();
            for (key, val) in self.env.iter() {
                // Dynamic variables stored as "*NAME" map to "$NAME" in the stash
                if let Some(name) = key.strip_prefix('*')
                    && !name.contains("::")
                {
                    symbols.insert(format!("${name}"), val.clone());
                }
                // Hash dynamic variables stored as "%*NAME" map to "%NAME" in the stash
                if let Some(name) = key.strip_prefix("%*")
                    && !name.contains("::")
                {
                    symbols.insert(format!("%{name}"), val.clone());
                }
                // Array dynamic variables stored as "@*NAME" map to "@NAME" in the stash
                if let Some(name) = key.strip_prefix("@*")
                    && !name.contains("::")
                {
                    symbols.insert(format!("@{name}"), val.clone());
                }
            }
            return Self::make_stash_instance(package, symbols);
        }

        if let Some((module, tag)) = Self::package_export_tag_parts(package) {
            let mut symbols: HashMap<String, Value> = HashMap::new();
            if let Some(subs) = self.exported_subs.get(module) {
                for (name, tags) in subs {
                    if tag != "ALL" && !tags.contains(tag) {
                        continue;
                    }
                    let fq = format!("{module}::{name}");
                    symbols.insert(format!("&{name}"), self.resolve_code_var(&fq));
                }
            }
            if let Some(vars) = self.exported_vars.get(module) {
                for (name, tags) in vars {
                    if tag != "ALL" && !tags.contains(tag) {
                        continue;
                    }
                    let fq = format!("{module}::{name}");
                    let val = self
                        .env
                        .get(&fq)
                        .cloned()
                        .or_else(|| self.env.get(name).cloned())
                        .unwrap_or(Value::Nil);
                    symbols.insert(name.clone(), val);
                }
            }
            return Self::make_stash_instance(package, symbols);
        }

        if let Some(module) = Self::package_export_module(&package_name) {
            let mut tags = std::collections::BTreeSet::new();
            if let Some(subs) = self.exported_subs.get(module) {
                for tagset in subs.values() {
                    tags.extend(tagset.iter().cloned());
                }
            }
            if let Some(vars) = self.exported_vars.get(module) {
                for tagset in vars.values() {
                    tags.extend(tagset.iter().cloned());
                }
            }
            let mut symbols: HashMap<String, Value> = HashMap::new();
            for tag in tags {
                symbols.insert(
                    tag.clone(),
                    Value::Package(Symbol::intern(&Self::qualify_stash_name(
                        &package_name,
                        &tag,
                    ))),
                );
            }
            return Self::make_stash_instance(package, symbols);
        }

        let mut symbols: HashMap<String, Value> = HashMap::new();

        for (key, val) in self.env.iter() {
            if key.starts_with("__mutsu_callable_id::") {
                continue;
            }
            if (package_name == "MY" || package_name == "GLOBAL")
                && self.should_hide_from_my_global_stash(key)
            {
                continue;
            }
            // Skip my-scoped items (they should not appear in the package stash)
            if self.is_my_scoped_package_item(key) {
                continue;
            }
            if let Some(rest) = Self::stash_member_tail(key, &package_name) {
                let stash_key = Self::stash_symbol_key_from_env_tail(rest);
                symbols.insert(stash_key, val.clone());
            }
        }

        for (key, def) in &self.functions {
            let key_s = key.resolve();
            let Some(rest) = Self::stash_member_tail(&key_s, &package_name) else {
                continue;
            };
            let base = rest.split('/').next().unwrap_or(rest);
            if base.is_empty() || base.contains("::") || base.contains(':') {
                continue;
            }
            // Skip my-scoped subs (they should not appear in the package stash)
            let fq_base = format!("{}::{}", package_name, base);
            if self.is_my_scoped_package_item(&fq_base) {
                continue;
            }
            symbols
                .entry(format!("&{base}"))
                .or_insert_with(|| Value::Routine {
                    package: def.package,
                    name: def.name,
                    is_regex: false,
                });
        }

        for class_name in self.classes.keys() {
            let class_short = class_name
                .rsplit_once("::")
                .map(|(_, short)| short)
                .unwrap_or(class_name.as_str());
            if (package_name == "MY" || package_name == "GLOBAL")
                && (self.need_hidden_classes.contains(class_name)
                    || self.need_hidden_classes.contains(class_short))
            {
                continue;
            }
            // Skip my-scoped classes (they should not appear in the package stash)
            if self.is_my_scoped_package_item(class_name) {
                continue;
            }
            let Some(rest) = Self::stash_member_tail(class_name, &package_name) else {
                continue;
            };
            if rest.is_empty() {
                continue;
            }
            if let Some((head, _)) = rest.split_once("::") {
                symbols.entry(head.to_string()).or_insert_with(|| {
                    Value::Package(Symbol::intern(&Self::qualify_stash_name(
                        &package_name,
                        head,
                    )))
                });
                continue;
            }
            symbols.entry(rest.to_string()).or_insert_with(|| {
                Value::Package(Symbol::intern(&Self::qualify_stash_name(
                    &package_name,
                    rest,
                )))
            });
        }
        for role_name in self.roles.keys() {
            let Some(rest) = Self::stash_member_tail(role_name, &package_name) else {
                continue;
            };
            if rest.is_empty() {
                continue;
            }
            if let Some((head, _)) = rest.split_once("::") {
                symbols.entry(head.to_string()).or_insert_with(|| {
                    Value::Package(Symbol::intern(&Self::qualify_stash_name(
                        &package_name,
                        head,
                    )))
                });
                continue;
            }
            symbols.entry(rest.to_string()).or_insert_with(|| {
                Value::Package(Symbol::intern(&Self::qualify_stash_name(
                    &package_name,
                    rest,
                )))
            });
        }

        if self.exported_subs.contains_key(&package_name)
            || self.exported_vars.contains_key(&package_name)
        {
            symbols.entry("EXPORT".to_string()).or_insert_with(|| {
                Value::Package(Symbol::intern(&Self::qualify_stash_name(
                    &package_name,
                    "EXPORT",
                )))
            });
        }

        Self::make_stash_instance(package, symbols)
    }

    pub(crate) fn push_end_phaser(&mut self, body: Vec<Stmt>) {
        let captured_env = self.env.clone();
        self.end_phasers.push((body, captured_env));
    }

    pub(crate) fn snapshot_routine_registry(&self) -> RoutineRegistrySnapshot {
        (
            self.functions.clone(),
            self.proto_functions.clone(),
            self.token_defs.clone(),
            self.proto_subs.clone(),
            self.proto_tokens.clone(),
        )
    }

    pub(crate) fn restore_routine_registry(&mut self, snapshot: RoutineRegistrySnapshot) {
        self.restore_routine_registry_impl(snapshot, false);
    }

    pub(crate) fn restore_routine_registry_eval(&mut self, snapshot: RoutineRegistrySnapshot) {
        self.restore_routine_registry_impl(snapshot, true);
    }

    fn restore_routine_registry_impl(&mut self, snapshot: RoutineRegistrySnapshot, is_eval: bool) {
        let (functions, proto_functions, token_defs, proto_subs, proto_tokens) = snapshot;
        // Collect our-scoped functions that were newly added during this block
        // (not present in the snapshot) that need to persist after scope restoration.
        // Only preserve functions whose package matches the current package (i.e., not
        // functions from loaded modules with different packages).
        let current_pkg = self.current_package.clone();
        let mut new_our: Vec<(Symbol, FunctionDef)> = Vec::new();
        for (key, def) in &self.our_scoped_functions {
            if !functions.contains_key(key) && def.package.resolve() == current_pkg {
                new_our.push((*key, def.clone()));
            }
        }
        self.functions = functions;
        self.proto_functions = proto_functions;
        self.token_defs = token_defs;
        self.proto_subs = proto_subs;
        self.proto_tokens = proto_tokens;
        // Re-apply only newly added our-scoped functions so they survive block scope exit.
        // In EVAL context, our-scoped functions should NOT leak into the outer lexical
        // scope — they remain accessible only via OUR:: pseudo-package resolution.
        if !is_eval {
            for (key, def) in new_our {
                self.functions.insert(key, def);
            }
        }
    }

    pub(crate) fn push_block_scope_depth(&mut self) {
        self.block_scope_depth += 1;
    }

    pub(crate) fn pop_block_scope_depth(&mut self) {
        self.block_scope_depth = self.block_scope_depth.saturating_sub(1);
    }

    /// Push a saved variable value for `let`/`temp` scope management.
    /// `is_temp`: true for `temp` (always restore), false for `let` (restore on failure only).
    pub(crate) fn let_saves_push(&mut self, name: String, value: Value, is_temp: bool) {
        self.let_saves.push((name, value, is_temp));
    }

    /// Current length of let_saves stack (used as a mark).
    pub(crate) fn let_saves_len(&self) -> usize {
        self.let_saves.len()
    }

    /// Restore all variables from let_saves starting at `mark`, then truncate.
    pub(crate) fn restore_let_saves(&mut self, mark: usize) {
        for i in (mark..self.let_saves.len()).rev() {
            let (name, old_val, _is_temp) = self.let_saves[i].clone();
            self.env.insert(name, old_val);
        }
        self.let_saves.truncate(mark);
    }

    /// On successful block exit: restore `temp` saves, discard `let` saves.
    /// For `let`, only restore if the block returned an unsuccessful value.
    pub(crate) fn resolve_let_saves_on_success(&mut self, mark: usize, success: bool) {
        for i in (mark..self.let_saves.len()).rev() {
            let (ref name, ref old_val, is_temp) = self.let_saves[i];
            // temp: always restore; let: restore only if block was unsuccessful
            if is_temp || !success {
                self.env.insert(name.clone(), old_val.clone());
            }
        }
        self.let_saves.truncate(mark);
    }

    /// Discard let_saves from `mark` without restoring (block succeeded).
    pub(crate) fn discard_let_saves(&mut self, mark: usize) {
        self.let_saves.truncate(mark);
    }
}
