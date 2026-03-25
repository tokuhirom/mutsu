use super::*;
use crate::symbol::Symbol;

mod args_matching;
mod binding;
mod coercion;
mod roles;
mod signature;
mod type_matching;
mod type_matching_static;
mod type_registry;

// Re-export public items from submodules
pub(crate) use coercion::{coerce_impossible_error, parse_coercion_type};
pub(in crate::runtime) use signature::{
    bind_named_rename_sub_signature, bind_sub_signature_from_value, flatten_into_slurpy,
    indexed_varref_from_value, make_varref_value, sigilless_alias_key, sigilless_readonly_key,
    sub_signature_matches_value, sub_signature_target_from_remaining_args, unwrap_varref_value,
    varref_from_value, wrap_native_int_for_binding,
};
// Internal re-exports used by submodules via `use super::*`
use coercion::is_coercion_constraint;
use signature::code_signature_matches_value;

const TEST_CALLSITE_LINE_KEY: &str = "__mutsu_test_callsite_line";

#[inline]
fn is_internal_named_arg(arg: &Value) -> bool {
    match arg {
        Value::Pair(key, _) => key == TEST_CALLSITE_LINE_KEY,
        Value::ValuePair(key, _) => {
            matches!(key.as_ref(), Value::Str(s) if s.as_str() == TEST_CALLSITE_LINE_KEY)
        }
        _ => false,
    }
}

pub(super) fn language_version_is_6e_or_newer(version: &str) -> bool {
    version.starts_with("6.e")
}

pub(super) fn predicate_requires_defined(predicate: &Expr) -> bool {
    match predicate {
        Expr::MethodCall {
            target, name, args, ..
        } => {
            args.is_empty()
                && name.resolve() == "defined"
                && matches!(target.as_ref(), Expr::Var(v) if v == "_")
        }
        Expr::AnonSub { body, .. } => {
            body.len() == 1
                && matches!(
                    &body[0],
                    Stmt::Expr(expr) if predicate_requires_defined(expr)
                )
        }
        _ => false,
    }
}

/// Strip a type smiley suffix (:U, :D, :_) from a constraint string.
/// Returns (base_type, smiley) where smiley is Some(":U"), Some(":D"), Some(":_") or None.
pub(crate) fn strip_type_smiley(constraint: &str) -> (&str, Option<&str>) {
    if let Some(base) = constraint.strip_suffix(":U") {
        (base, Some(":U"))
    } else if let Some(base) = constraint.strip_suffix(":D") {
        (base, Some(":D"))
    } else if let Some(base) = constraint.strip_suffix(":_") {
        (base, Some(":_"))
    } else {
        (constraint, None)
    }
}

/// Check if a value is "defined" in the Raku sense.
/// Type objects (Package) are undefined; concrete values and instances are defined.
pub(crate) fn value_is_defined(value: &Value) -> bool {
    match value {
        Value::Nil | Value::Package(_) => false,
        Value::Slip(items) if items.is_empty() => false,
        Value::Instance { class_name, .. } if class_name == "Failure" => false,
        _ => true,
    }
}

impl Interpreter {
    fn resolve_sigilless_alias_source_name(&self, source_name: &str) -> String {
        let mut resolved = source_name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(resolved.clone()) {
            let key = sigilless_alias_key(&resolved);
            let Some(Value::Str(next)) = self.env.get(&key) else {
                break;
            };
            resolved = next.to_string();
        }
        resolved
    }

    /// Save the current readonly_vars set (call before function body execution).
    pub(crate) fn save_readonly_vars(&self) -> HashSet<String> {
        self.readonly_vars.clone()
    }

    /// Restore the readonly_vars set (call after function body execution).
    pub(crate) fn restore_readonly_vars(&mut self, saved: HashSet<String>) {
        self.readonly_vars = saved;
    }

    /// Get mutable access to readonly_vars set.
    pub(crate) fn readonly_vars_mut(&mut self) -> &mut HashSet<String> {
        &mut self.readonly_vars
    }

    /// Mark a variable as readonly.
    pub(crate) fn mark_readonly(&mut self, name: &str) {
        self.readonly_vars.insert(name.to_string());
    }

    /// Check if a variable is readonly and return an error if so.
    /// Returns Ok(()) if writable, Err with X::Multi::NoMatch for increment/decrement,
    /// or Err with a generic message for assignment.
    pub(crate) fn check_readonly_for_modify(&self, name: &str) -> Result<(), RuntimeError> {
        if self.readonly_vars.contains(name) {
            let msg = format!("Cannot assign to a readonly variable ({}) or a value", name);
            let mut err = RuntimeError::new(msg.clone());
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Assignment::RO"),
                attrs,
            )));
            return Err(err);
        }
        Ok(())
    }

    /// Check if a variable is readonly for increment/decrement operations.
    /// Returns Err with X::Multi::NoMatch (like Raku's postfix:<++> dispatch failure).
    pub(crate) fn check_readonly_for_increment(&self, name: &str) -> Result<(), RuntimeError> {
        if self.readonly_vars.contains(name) {
            let msg = format!(
                "Cannot resolve caller postfix:<++>({}); the parameter requires mutable arguments",
                name
            );
            let mut err = RuntimeError::new(msg.clone());
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Multi::NoMatch"),
                attrs,
            )));
            return Err(err);
        }
        Ok(())
    }

    pub(crate) fn apply_rw_bindings_to_env(
        &self,
        rw_bindings: &[(String, String)],
        target_env: &mut crate::env::Env,
    ) {
        for (param_name, source_name) in rw_bindings {
            if let Some(updated) = self.env.get(param_name).cloned() {
                target_env.insert(source_name.clone(), updated);
            }
        }
    }

    pub(in crate::runtime) fn bind_param_value(&mut self, name: &str, value: Value) {
        self.env.insert(name.to_string(), value.clone());
        // Extract attribute name from twigil params: $!x -> "x", @!types -> "types", %!h -> "h"
        let attr_name = if let Some(a) = name.strip_prefix('!') {
            Some(a)
        } else {
            // Handle sigil+twigil: @!types, %!h, @.items, %.pairs
            name.strip_prefix("@!")
                .or_else(|| name.strip_prefix("%!"))
                .or_else(|| name.strip_prefix("@."))
                .or_else(|| name.strip_prefix("%."))
        };
        if let Some(attr_name) = attr_name {
            // Also set the canonical !attr env key so write-back finds it
            self.env.insert(format!("!{}", attr_name), value.clone());
            self.env.insert(format!(".{}", attr_name), value.clone());
            if let Some(Value::Instance {
                class_name,
                attributes,
                id,
            }) = self.env.get("self").cloned()
            {
                let mut updated_attrs = (*attributes).clone();
                updated_attrs.insert(attr_name.to_string(), value.clone());
                self.env.insert(
                    "self".to_string(),
                    Value::make_instance_with_id(class_name, updated_attrs, id),
                );
            }
        }
        if matches!(
            value,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ) && !name.starts_with('&')
        {
            self.env.insert(format!("&{}", name), value.clone());
        }
        // Placeholder variables also create a non-twigil alias:
        // $^foo -> $foo, &^c -> &c
        if let Some(bare) = name.strip_prefix("&^") {
            self.env.insert(format!("&{}", bare), value);
        } else if let Some(bare) = name.strip_prefix('^') {
            self.env.insert(bare.to_string(), value);
        }
    }

    pub(crate) fn captured_type_object(value: &Value) -> Value {
        match value {
            Value::Package(name) => Value::Package(*name),
            Value::ParametricRole { .. } => value.clone(),
            Value::Instance { class_name, .. } => Value::Package(*class_name),
            Value::Nil => Value::Package(Symbol::intern("Any")),
            _ => Value::Package(Symbol::intern(super::value_type_name(value))),
        }
    }

    fn type_capture_marker_key(name: &str) -> String {
        format!("__type_capture__{}", name)
    }

    pub(crate) fn bind_type_capture(&mut self, name: &str, value: &Value) {
        self.env
            .insert(name.to_string(), Self::captured_type_object(value));
        self.env
            .insert(Self::type_capture_marker_key(name), Value::Bool(true));
    }

    pub(crate) fn has_type_capture_binding(&self, name: &str) -> bool {
        matches!(
            self.env.get(&Self::type_capture_marker_key(name)),
            Some(Value::Bool(true))
        )
    }

    pub(in crate::runtime) fn optional_type_object_name(constraint: &str) -> String {
        let mut end = constraint.len();
        for (idx, ch) in constraint.char_indices() {
            if ch == '[' || ch == '(' || ch == ':' {
                end = idx;
                break;
            }
        }
        constraint[..end].to_string()
    }

    pub(in crate::runtime) fn missing_optional_param_value(pd: &ParamDef) -> Value {
        if pd.name.starts_with('@') {
            return Value::real_array(Vec::new());
        }
        if pd.name.starts_with('%') {
            return Value::hash(std::collections::HashMap::new());
        }
        if let Some(constraint) = &pd.type_constraint {
            return Value::Package(Symbol::intern(&Self::optional_type_object_name(constraint)));
        }
        Value::Nil
    }

    pub(crate) fn nominal_type_object_name_for_constraint(&self, constraint: &str) -> String {
        let (base, _) = strip_type_smiley(constraint);
        if let Some((target, _source)) = parse_coercion_type(base) {
            return self.nominal_type_object_name_for_constraint(target);
        }
        let base_name = Self::optional_type_object_name(base);
        if let Some(captured_name) = base_name.strip_prefix("::")
            && let Some(Value::Package(bound)) = self.env.get(captured_name)
        {
            let resolved = bound.resolve();
            if resolved != captured_name {
                return self.nominal_type_object_name_for_constraint(&resolved);
            }
        }
        if let Some(Value::Package(bound)) = self.env.get(&base_name) {
            let resolved = bound.resolve();
            if resolved != base_name {
                return self.nominal_type_object_name_for_constraint(&resolved);
            }
        }
        if let Some(subset) = self.subsets.get(&base_name)
            && subset.base != base_name
        {
            return self.nominal_type_object_name_for_constraint(&subset.base);
        }
        base_name
    }

    pub(in crate::runtime) fn resolve_constraint_alias(&self, constraint: &str) -> String {
        if let Some(captured) = constraint.strip_prefix("::")
            && let Some(Value::Package(name)) = self.env.get(captured)
        {
            return name.resolve();
        }
        if let Some(Value::Package(name)) = self.env.get(constraint) {
            return name.resolve();
        }
        constraint.to_string()
    }

    fn checked_default_param_value(
        &mut self,
        pd: &ParamDef,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let value = self.materialize_default_parametric_role(value)?;
        if let Some(constraint) = &pd.type_constraint
            && !constraint.starts_with("::")
            && !self.type_matches_value(constraint, &value)
        {
            return Err(RuntimeError::new(format!(
                "X::Parameter::Default::TypeCheck: Type check failed for default value of parameter '{}'; expected {}, got {}",
                pd.name,
                constraint,
                super::value_type_name(&value)
            )));
        }
        Ok(value)
    }

    pub(super) fn materialize_default_parametric_role(
        &mut self,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let Value::Package(role_name) = &value else {
            return Ok(value);
        };
        let role_name = role_name.resolve();
        let Some(candidates) = self.role_candidates.get(&role_name).cloned() else {
            return Ok(value);
        };
        let Some(candidate) = candidates.into_iter().find(|candidate| {
            !candidate.type_param_defs.is_empty()
                && candidate
                    .type_param_defs
                    .iter()
                    .all(|pd| pd.default.is_some() || pd.optional_marker)
        }) else {
            return Ok(value);
        };
        let param_names = candidate
            .type_param_defs
            .iter()
            .map(|pd| pd.name.clone())
            .collect::<Vec<_>>();
        let saved_env = self.env.clone();
        if let Err(err) =
            self.bind_function_args_values(&candidate.type_param_defs, &param_names, &[])
        {
            self.env = saved_env;
            return Err(err);
        }
        let type_args = candidate
            .type_param_defs
            .iter()
            .filter_map(|pd| {
                if let Some(captured) = pd
                    .type_constraint
                    .as_deref()
                    .and_then(|constraint| constraint.strip_prefix("::"))
                {
                    self.env.get(captured).cloned()
                } else {
                    self.env.get(&pd.name).cloned()
                }
            })
            .collect::<Vec<_>>();
        self.env = saved_env;
        Ok(Value::ParametricRole {
            base_name: Symbol::intern(&role_name),
            type_args,
        })
    }

    pub(in crate::runtime) fn parse_generic_constraint(constraint: &str) -> Option<(&str, &str)> {
        let open = constraint.find('[')?;
        if open == 0 || !constraint.ends_with(']') {
            return None;
        }
        let base = &constraint[..open];
        let inner = &constraint[open + 1..constraint.len() - 1];
        if base.is_empty() || inner.is_empty() {
            return None;
        }
        Some((base, inner))
    }
}
