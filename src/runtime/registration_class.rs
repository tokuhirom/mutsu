use super::*;
use crate::ast::{HandleSpec, ParamDef};
use crate::symbol::Symbol;

type ResolvedRoleCandidate = (RoleDef, Vec<String>, Vec<Value>);

fn type_value_name(value: &Value) -> String {
    match value {
        Value::Package(name) => name.resolve(),
        Value::ParametricRole {
            base_name,
            type_args,
        } => format!(
            "{}[{}]",
            base_name.resolve(),
            type_args
                .iter()
                .map(type_value_name)
                .collect::<Vec<_>>()
                .join(",")
        ),
        other => other
            .to_string_value()
            .trim_start_matches('(')
            .trim_end_matches(')')
            .to_string(),
    }
}

fn builtin_role_def() -> RoleDef {
    RoleDef {
        attributes: Vec::new(),
        methods: HashMap::new(),
        is_stub_role: false,
        is_hidden: false,
        captured_env: None,
        wildcard_handles: Vec::new(),
        role_id: 0,
        attribute_conflicts: Vec::new(),
        deferred_body_stmts: Vec::new(),
    }
}

/// Intermediate representation for resolved handle specs.
enum ResolvedHandle {
    /// Forward `exposed` method to `target` method on the object in `attr_var`.
    Method {
        exposed: String,
        target: String,
        attr_var: String,
    },
    /// Regex-based delegation: forward methods matching `pattern`.
    Regex { attr_var: String, pattern: String },
    /// Wildcard delegation: forward all unknown methods.
    WildcardHandle(String),
}

/// Apply resolved handles to methods map and wildcard handles.
fn apply_resolved_handles(
    handles: &[ResolvedHandle],
    methods: &mut HashMap<String, Vec<MethodDef>>,
    wildcard_handles: &mut Vec<String>,
) {
    for handle in handles {
        match handle {
            ResolvedHandle::Method {
                exposed,
                target,
                attr_var,
            } => {
                methods
                    .entry(exposed.clone())
                    .or_default()
                    .push(make_delegation_method(attr_var, target));
            }
            ResolvedHandle::Regex { attr_var, pattern } => {
                wildcard_handles.push(format!("{}:regex:{}", attr_var, pattern));
            }
            ResolvedHandle::WildcardHandle(attr_var) => {
                wildcard_handles.push(attr_var.clone());
            }
        }
    }
}

/// Create the slurpy `*@_` parameter used by delegation forwarding methods.
/// This ensures the method matches any number of positional/named arguments.
fn delegation_slurpy_param() -> ParamDef {
    ParamDef {
        name: "@_".to_string(),
        default: None,
        multi_invocant: false,
        required: false,
        named: false,
        slurpy: true,
        double_slurpy: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
    }
}

/// Create a delegation MethodDef that forwards method calls to `target_method`
/// on the object in `attr_var_name`.
fn make_delegation_method(attr_var_name: &str, target_method: &str) -> MethodDef {
    MethodDef {
        params: vec!["@_".to_string(), "%_".to_string()],
        param_defs: vec![delegation_slurpy_param(), delegation_double_slurpy_param()],
        body: std::sync::Arc::new(Vec::new()),
        is_rw: false,
        is_private: false,
        is_multi: false,
        is_my: false,
        role_origin: None,
        original_role: None,
        return_type: None,
        compiled_code: None,
        delegation: Some((attr_var_name.to_string(), target_method.to_string())),
        is_default: false,
        deprecated_message: None,
    }
}

/// Create the double-slurpy `**@_` parameter for named arg forwarding.
fn delegation_double_slurpy_param() -> ParamDef {
    ParamDef {
        name: "%_".to_string(),
        default: None,
        multi_invocant: false,
        required: false,
        named: false,
        slurpy: true,
        double_slurpy: true,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
    }
}

pub(crate) struct ClassDeclModifiers<'a> {
    pub(crate) class_is_rw: bool,
    pub(crate) is_hidden: bool,
    pub(crate) hidden_parents: &'a [String],
    pub(crate) does_parents: &'a [String],
}

fn parse_role_type_args(input: &str) -> Vec<String> {
    split_balanced_comma_list(input)
}

fn looks_like_type_arg_expr(input: &str) -> bool {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return false;
    }
    trimmed.chars().all(|ch| {
        ch.is_ascii_alphanumeric()
            || matches!(
                ch,
                ':' | '?' | '_' | '[' | ']' | '(' | ')' | ',' | ' ' | '\t'
            )
    })
}

fn should_treat_role_arg_as_type_expr(input: &str) -> bool {
    let trimmed = input.trim();
    looks_like_type_arg_expr(trimmed)
        && (trimmed.contains(':') || trimmed.contains('(') || trimmed.contains("::"))
}

/// Substitute type parameters in a method definition.
/// E.g., if type_subs = [("T", "Str:D(Numeric)")], then any param with
/// type_constraint "T" becomes "Str:D(Numeric)".
fn substitute_type_params_in_method(
    method: &MethodDef,
    type_subs: &[(String, String)],
) -> MethodDef {
    fn replace_type_name(type_name: &str, type_subs: &[(String, String)]) -> String {
        for (param_name, replacement) in type_subs {
            if type_name == param_name {
                return replacement.clone();
            }
        }
        type_name.to_string()
    }

    fn substitute_param_def(pd: &ParamDef, type_subs: &[(String, String)]) -> ParamDef {
        let mut new_pd = pd.clone();
        if let Some(tc) = &new_pd.type_constraint {
            new_pd.type_constraint = Some(replace_type_name(tc, type_subs));
        }
        if let Some(sub) = &new_pd.sub_signature {
            new_pd.sub_signature = Some(
                sub.iter()
                    .map(|p| substitute_param_def(p, type_subs))
                    .collect(),
            );
        }
        if let Some(outer) = &new_pd.outer_sub_signature {
            new_pd.outer_sub_signature = Some(
                outer
                    .iter()
                    .map(|p| substitute_param_def(p, type_subs))
                    .collect(),
            );
        }
        if let Some((sig_params, sig_ret)) = &new_pd.code_signature {
            let next_params = sig_params
                .iter()
                .map(|p| substitute_param_def(p, type_subs))
                .collect();
            let next_ret = sig_ret.as_ref().map(|r| replace_type_name(r, type_subs));
            new_pd.code_signature = Some((next_params, next_ret));
        }
        new_pd
    }

    let new_param_defs = method
        .param_defs
        .iter()
        .map(|pd| substitute_param_def(pd, type_subs))
        .collect();
    MethodDef {
        params: method.params.clone(),
        param_defs: new_param_defs,
        body: method.body.clone(),
        is_rw: method.is_rw,
        is_private: method.is_private,
        is_multi: method.is_multi,
        is_my: method.is_my,
        role_origin: method.role_origin.clone(),
        original_role: method.original_role.clone(),
        return_type: method.return_type.clone(),
        compiled_code: method.compiled_code.clone(),
        delegation: method.delegation.clone(),
        is_default: method.is_default,
        deprecated_message: method.deprecated_message.clone(),
    }
}

impl Interpreter {
    /// Apply `handles` specifications to a class definition.
    /// For type-based handles, collects method names from the referenced type
    /// first, then applies them without holding borrows on self.
    fn apply_handle_specs(
        &self,
        specs: &[HandleSpec],
        attr_var_name: &str,
        class_def: &mut ClassDef,
    ) {
        let resolved = self.resolve_handle_specs_to_names(specs, attr_var_name);
        apply_resolved_handles(
            &resolved,
            &mut class_def.methods,
            &mut class_def.wildcard_handles,
        );
    }

    /// Apply `handles` specifications to a role definition.
    fn apply_handle_specs_to_role(
        &self,
        specs: &[HandleSpec],
        attr_var_name: &str,
        role_def: &mut RoleDef,
    ) {
        let resolved = self.resolve_handle_specs_to_names(specs, attr_var_name);
        apply_resolved_handles(
            &resolved,
            &mut role_def.methods,
            &mut role_def.wildcard_handles,
        );
    }

    /// Resolve handle specs to concrete (exposed_name, target_method, attr_var_name) tuples
    /// or wildcard/regex entries. This step only reads from self (immutable borrow).
    fn resolve_handle_specs_to_names(
        &self,
        specs: &[HandleSpec],
        attr_var_name: &str,
    ) -> Vec<ResolvedHandle> {
        let mut result = Vec::new();
        for spec in specs {
            match spec {
                HandleSpec::Name(name) => {
                    // Check if the name refers to a known class or role (type delegation)
                    let type_methods = self.collect_type_method_names(name);
                    if !type_methods.is_empty() {
                        for method_name in type_methods {
                            result.push(ResolvedHandle::Method {
                                exposed: method_name.clone(),
                                target: method_name,
                                attr_var: attr_var_name.to_string(),
                            });
                        }
                    } else {
                        result.push(ResolvedHandle::Method {
                            exposed: name.clone(),
                            target: name.clone(),
                            attr_var: attr_var_name.to_string(),
                        });
                    }
                }
                HandleSpec::Rename { exposed, target } => {
                    result.push(ResolvedHandle::Method {
                        exposed: exposed.clone(),
                        target: target.clone(),
                        attr_var: attr_var_name.to_string(),
                    });
                }
                HandleSpec::Type(type_name) => {
                    for method_name in self.collect_type_method_names(type_name) {
                        result.push(ResolvedHandle::Method {
                            exposed: method_name.clone(),
                            target: method_name,
                            attr_var: attr_var_name.to_string(),
                        });
                    }
                }
                HandleSpec::Regex(pattern) => {
                    result.push(ResolvedHandle::Regex {
                        attr_var: attr_var_name.to_string(),
                        pattern: pattern.clone(),
                    });
                }
                HandleSpec::Wildcard => {
                    result.push(ResolvedHandle::WildcardHandle(attr_var_name.to_string()));
                }
            }
        }
        result
    }

    /// Collect method names from a class or role by name.
    fn collect_type_method_names(&self, type_name: &str) -> Vec<String> {
        let mut names = Vec::new();
        if let Some(class_def) = self.classes.get(type_name) {
            names.extend(class_def.methods.keys().cloned());
        } else if let Some(role_def) = self.roles.get(type_name) {
            names.extend(role_def.methods.keys().cloned());
            // Also include methods from composed roles
            if let Some(composed) = self.class_composed_roles.get(type_name) {
                for role_name in composed {
                    if let Some(rd) = self.roles.get(role_name) {
                        for key in rd.methods.keys() {
                            if !names.contains(key) {
                                names.push(key.clone());
                            }
                        }
                    }
                }
            }
        }
        names
    }

    fn eval_role_arg_values(&mut self, arg_exprs: &[String]) -> Result<Vec<Value>, RuntimeError> {
        let mut values = Vec::with_capacity(arg_exprs.len());
        for expr in arg_exprs {
            if expr.trim_start().starts_with("::") {
                return Err(RuntimeError::new(
                    "X::Syntax::Malformed: cannot use ::T in role application".to_string(),
                ));
            }
            if should_treat_role_arg_as_type_expr(expr) {
                values.push(Value::Package(Symbol::intern(expr.trim())));
                continue;
            }
            match crate::parse_dispatch::parse_source(expr)
                .and_then(|(stmts, _)| self.eval_block_value(&stmts))
            {
                Ok(value) => values.push(value),
                Err(_) if looks_like_type_arg_expr(expr) => {
                    values.push(Value::Package(Symbol::intern(expr.trim())));
                }
                Err(err) => return Err(err),
            }
        }
        Ok(values)
    }

    fn role_constraint_specificity(&self, constraint: Option<&str>) -> i32 {
        let Some(constraint) = constraint else {
            return 0;
        };
        if constraint.starts_with("::") {
            return 1;
        }
        if constraint == "Any" || constraint == "Mu" {
            return 2;
        }
        if let Some(def) = self.classes.get(constraint) {
            return 10 + def.parents.len() as i32;
        }
        if self.roles.contains_key(constraint) {
            return 9;
        }
        8
    }

    fn role_candidate_specificity_score(&self, param_defs: &[ParamDef]) -> i32 {
        let mut score = 0i32;
        for pd in param_defs.iter().filter(|pd| !pd.named) {
            score += self.role_constraint_specificity(pd.type_constraint.as_deref());
            if pd.where_constraint.is_some() {
                score += 20;
            }
            if pd.literal_value.is_some() {
                score += 30;
            }
        }
        score
    }

    fn role_candidate_arity_ok(&self, args: &[Value], param_defs: &[ParamDef]) -> bool {
        if param_defs.is_empty() {
            return args.is_empty();
        }
        let positional_arg_count = args
            .iter()
            .filter(|arg| !matches!(arg, Value::Pair(..)))
            .count();
        let positional_params: Vec<&ParamDef> = param_defs.iter().filter(|pd| !pd.named).collect();
        let has_positional_slurpy = positional_params
            .iter()
            .any(|pd| pd.slurpy && !pd.name.starts_with('%'));
        let required = positional_params
            .iter()
            .filter(|pd| !pd.slurpy && pd.default.is_none() && !pd.optional_marker)
            .count();
        if positional_arg_count < required {
            return false;
        }
        if !has_positional_slurpy && positional_arg_count > positional_params.len() {
            return false;
        }
        true
    }

    fn resolve_role_candidate(
        &mut self,
        parent: &str,
    ) -> Result<Option<ResolvedRoleCandidate>, RuntimeError> {
        let parent = self.resolve_declared_type_name(parent);
        if let Some(bracket_start) = parent.find('[') {
            let args_str = &parent[bracket_start + 1..parent.len() - 1];
            let arg_exprs = parse_role_type_args(args_str);
            if arg_exprs
                .iter()
                .any(|expr| expr.trim_start().starts_with("::"))
            {
                return Err(RuntimeError::new(
                    "X::Syntax::Malformed: cannot use ::T in role application".to_string(),
                ));
            }
        }

        let base_role_name = if let Some(bracket) = parent.find('[') {
            &parent[..bracket]
        } else {
            &parent
        };
        let Some(candidates) = self.role_candidates.get(base_role_name).cloned() else {
            if let Some(role) = self.roles.get(base_role_name).cloned() {
                return Ok(Some((role, Vec::new(), Vec::new())));
            }
            if matches!(base_role_name, "Positional" | "Associative" | "Callable") {
                return Ok(Some((builtin_role_def(), Vec::new(), Vec::new())));
            }
            return Ok(None);
        };

        let arg_exprs = if let Some(bracket_start) = parent.find('[') {
            let args_str = &parent[bracket_start + 1..parent.len() - 1];
            parse_role_type_args(args_str)
        } else {
            Vec::new()
        };
        let arg_values = self.eval_role_arg_values(&arg_exprs)?;

        let mut matches: Vec<(RoleCandidateDef, i32, usize)> = candidates
            .into_iter()
            .enumerate()
            .filter_map(|(idx, candidate)| {
                let candidate_param_names = candidate
                    .type_param_defs
                    .iter()
                    .map(|pd| pd.name.clone())
                    .collect::<Vec<_>>();
                let ok = if self.role_candidate_arity_ok(&arg_values, &candidate.type_param_defs) {
                    let saved_env = self.env.clone();
                    let ok = self
                        .bind_function_args_values(
                            &candidate.type_param_defs,
                            &candidate_param_names,
                            &arg_values,
                        )
                        .is_ok();
                    self.env = saved_env;
                    ok
                } else {
                    false
                };
                if ok {
                    Some((
                        candidate.clone(),
                        self.role_candidate_specificity_score(&candidate.type_param_defs),
                        idx,
                    ))
                } else {
                    None
                }
            })
            .collect();

        if matches.is_empty() {
            return Err(RuntimeError::typed_msg(
                "X::Role::Parametric::NoSuchCandidate",
                "No matching candidate found for the parametric role",
            ));
        }

        matches.sort_by(|a, b| b.1.cmp(&a.1).then(b.2.cmp(&a.2)));
        let selected = matches.remove(0).0;
        Ok(Some((selected.role_def, selected.type_params, arg_values)))
    }

    fn resolve_declared_type_name(&self, name: &str) -> String {
        let (base, suffix) = if let Some(bracket) = name.find('[') {
            (&name[..bracket], &name[bracket..])
        } else {
            (name, "")
        };
        let lookup = base.strip_prefix("::").unwrap_or(base);
        if let Value::Package(pkg) = self.resolve_indirect_type_name(lookup) {
            return format!("{}{}", pkg.resolve(), suffix);
        }
        if let Some(Value::Package(pkg)) = self.env.get(lookup) {
            return format!("{}{}", pkg.resolve(), suffix);
        }
        name.to_string()
    }

    pub(crate) fn register_class_decl(
        &mut self,
        name: &str,
        parents: &[String],
        modifiers: ClassDeclModifiers<'_>,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        self.clear_private_zeroarg_method_cache();
        let ClassDeclModifiers {
            class_is_rw,
            is_hidden,
            hidden_parents,
            does_parents,
        } = modifiers;
        let prev_class = self.classes.get(name).cloned();
        let prev_hidden = self.hidden_classes.contains(name);
        let prev_hidden_defer = self.hidden_defer_parents.get(name).cloned();
        let prev_composed_roles = self.class_composed_roles.get(name).cloned();
        let prev_role_param_bindings = self.class_role_param_bindings.get(name).cloned();
        // Clear `is Type` trait entries for this class (they'll be re-populated from the body).
        self.class_attribute_is_types
            .retain(|(cn, _), _| cn != name);

        let restore_previous_state = |this: &mut Self| {
            if let Some(class_def) = prev_class.clone() {
                this.classes.insert(name.to_string(), class_def);
            } else {
                this.classes.remove(name);
            }
            if prev_hidden {
                this.hidden_classes.insert(name.to_string());
            } else {
                this.hidden_classes.remove(name);
            }
            if let Some(hidden) = prev_hidden_defer.clone() {
                this.hidden_defer_parents.insert(name.to_string(), hidden);
            } else {
                this.hidden_defer_parents.remove(name);
            }
            if let Some(composed) = prev_composed_roles.clone() {
                this.class_composed_roles.insert(name.to_string(), composed);
            } else {
                this.class_composed_roles.remove(name);
            }
            if let Some(bindings) = prev_role_param_bindings.clone() {
                this.class_role_param_bindings
                    .insert(name.to_string(), bindings);
            } else {
                this.class_role_param_bindings.remove(name);
            }
        };

        // TODO: Detect X::Redeclaration when a class redefines a role in the same scope.
        // Currently disabled because the role registry is global (not lexically scoped),
        // so `my role B { ... }` in one block leaks and causes false positives when
        // `class B` is defined in a different scope (e.g., EVAL).

        // Detect stub body: `class Foo { ... }` — body is a stub operator call
        // Filter SetLine annotations which don't affect the stub nature.
        let body_no_setline: Vec<_> = body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        let is_stub_body = body_no_setline.len() == 1
            && matches!(body_no_setline[0], Stmt::Expr(Expr::Call { name: fn_name, .. })
                if *fn_name == "__mutsu_stub_die" || *fn_name == "__mutsu_stub_warn");

        // If this is a stub registration but the class already exists and is
        // NOT a stub (i.e., it was already filled in by a hoisted real
        // declaration), skip the stub registration to avoid overwriting the
        // real class definition.
        if is_stub_body && self.classes.contains_key(name) && !self.class_stubs.contains(name) {
            return Ok(());
        }

        // Validate that all parent classes exist
        // Allow inheriting from built-in types that may not be in the classes HashMap
        const BUILTIN_TYPES: &[&str] = &[
            "Mu",
            "Any",
            "Cool",
            "Int",
            "Num",
            "Str",
            "Bool",
            "Rat",
            "FatRat",
            "Complex",
            "Array",
            "Hash",
            "List",
            "Map",
            "Set",
            "Bag",
            "Mix",
            "Range",
            "Pair",
            "IO",
            "IO::Path",
            "IO::Handle",
            "Regex",
            "Match",
            "Junction",
            "Exception",
            "Failure",
            "Version",
            "Nil",
            "Block",
            "Code",
            "Routine",
            "Sub",
            "Method",
            "Seq",
            "Slip",
            "Whatever",
            "WhateverCode",
            "HyperWhatever",
            "Callable",
            "Numeric",
            "Real",
            "Stringy",
            "Positional",
            "Associative",
            "Order",
            "Endian",
            "Proc",
            "Proc::Async",
            "Supply",
            "Supplier",
            "Setty",
            "Baggy",
            "Mixy",
            "Date",
            "DateTime",
            "Grammar",
            "Proxy",
            "Stash",
        ];
        for parent in parents {
            let resolved_parent_name = self.resolve_declared_type_name(parent);
            // Strip type arguments for validation (e.g., "R[Str:D(Numeric)]" -> "R")
            let base_parent = if let Some(bracket) = resolved_parent_name.find('[') {
                &resolved_parent_name[..bracket]
            } else {
                resolved_parent_name.as_str()
            };
            // Strip leading `::` for comparison (e.g., `is ::F` refers to `F`)
            let resolved_parent = base_parent.strip_prefix("::").unwrap_or(base_parent);
            if resolved_parent == name {
                return Err(RuntimeError::new(format!(
                    "X::Inheritance::SelfInherit: class '{}' cannot inherit from itself",
                    name
                )));
            }
            if !self.classes.contains_key(base_parent)
                && !BUILTIN_TYPES.contains(&base_parent)
                && !self.roles.contains_key(base_parent)
                && !self.enum_types.contains_key(base_parent)
            {
                // Use X::InvalidType for `does` parents, X::Inheritance::UnknownParent
                // for `is` parents.
                if does_parents.contains(parent) {
                    return Err(RuntimeError::new(format!(
                        "X::InvalidType: Invalid typename '{}'",
                        resolved_parent_name
                    )));
                }
                return Err(RuntimeError::new(format!(
                    "X::Inheritance::UnknownParent: class '{}' specifies unknown parent class '{}'",
                    name, resolved_parent_name
                )));
            }
            // Check that `does` targets are actually roles, not classes
            if does_parents.contains(parent)
                && self.classes.contains_key(resolved_parent)
                && !self.roles.contains_key(resolved_parent)
                && !BUILTIN_TYPES.contains(&resolved_parent)
            {
                return Err(RuntimeError::new(format!(
                    "'{}' cannot compose '{}' because it is not a role",
                    name, resolved_parent
                )));
            }
            // Check if parent is a stub (not yet composed)
            if self.class_stubs.contains(resolved_parent) {
                let message = format!(
                    "'{}' cannot inherit from '{}' because '{}' isn't composed yet (maybe it is stubbed)",
                    name, resolved_parent, resolved_parent
                );
                let mut attrs = HashMap::new();
                attrs.insert("child-name".to_string(), Value::str(name.to_string()));
                attrs.insert(
                    "parent-name".to_string(),
                    Value::str(resolved_parent.to_string()),
                );
                attrs.insert("message".to_string(), Value::str(message.clone()));
                let ex = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Inheritance::NotComposed"),
                    attrs,
                );
                let mut err =
                    RuntimeError::new(format!("X::Inheritance::NotComposed: {}", message));
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
        }
        let mut class_def = ClassDef {
            parents: parents.to_vec(),
            attributes: Vec::new(),
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            methods: HashMap::new(),
            native_methods: HashSet::new(),
            mro: Vec::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: HashMap::new(),
        };
        if is_hidden {
            self.hidden_classes.insert(name.to_string());
        } else {
            self.hidden_classes.remove(name);
        }
        if hidden_parents.is_empty() {
            self.hidden_defer_parents.remove(name);
        } else {
            self.hidden_defer_parents
                .insert(name.to_string(), hidden_parents.iter().cloned().collect());
        }
        // Compose roles listed in the parents (from "does Role" or "is Role" in class header)
        let mut composed_roles_list = Vec::new();
        let mut punned_roles = Vec::new();
        let mut hidden_punned_role_bases: HashSet<String> = HashSet::new();
        let mut class_role_param_bindings: HashMap<String, Value> = HashMap::new();
        for parent in parents {
            let resolved_parent_name = self.resolve_declared_type_name(parent);
            let base_role_name = resolved_parent_name
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(resolved_parent_name.as_str());
            if let Some((role, role_param_names, role_arg_values)) =
                self.resolve_role_candidate(&resolved_parent_name)?
            {
                if role.is_stub_role {
                    return Err(RuntimeError::typed_msg(
                        "X::Role::Parametric::NoSuchCandidate",
                        "No matching candidate found for the parametric role",
                    ));
                }
                // Check for attribute conflicts detected during role composition
                if let Some((attr_name, role_a, role_b)) = role.attribute_conflicts.first() {
                    return Err(RuntimeError::new(format!(
                        "Attribute '$!{}' conflicts in role '{}' composition: declared in both '{}' and '{}'",
                        attr_name, base_role_name, role_a, role_b
                    )));
                }
                // Check if this role was specified via `is` (punning) vs `does` (composition)
                let is_punned = !does_parents.contains(parent);
                if is_punned {
                    punned_roles.push(resolved_parent_name.clone());
                    if role.is_hidden {
                        hidden_punned_role_bases.insert(base_role_name.to_string());
                    }
                }
                composed_roles_list.push(resolved_parent_name.clone());
                // Collect type parameter substitutions for method type constraints.
                let type_subs: Vec<(String, String)> = role_param_names
                    .iter()
                    .zip(role_arg_values.iter())
                    .map(|(p, v)| (p.clone(), type_value_name(v)))
                    .collect();
                for (p, v) in role_param_names.iter().zip(role_arg_values.iter()) {
                    class_role_param_bindings.insert(p.clone(), v.clone());
                }
                for attr in &role.attributes {
                    if !class_def.attributes.iter().any(|(n, ..)| n == &attr.0) {
                        class_def.attributes.push(attr.clone());
                    }
                }
                for (mname, overloads) in &role.methods {
                    // Skip methods declared with `my` scope -- they are role-private
                    // and should not be composed into consuming classes.
                    let non_my_overloads: Vec<&MethodDef> =
                        overloads.iter().filter(|md| !md.is_my).collect();
                    if non_my_overloads.is_empty() {
                        continue;
                    }
                    let composed: Vec<MethodDef> = if type_subs.is_empty() {
                        non_my_overloads
                            .into_iter()
                            .map(|md| {
                                let mut method = md.clone();
                                if method.original_role.is_none() {
                                    method.original_role = method.role_origin.clone();
                                }
                                method.role_origin = Some(base_role_name.to_string());
                                method
                            })
                            .collect()
                    } else {
                        non_my_overloads
                            .into_iter()
                            .map(|md| {
                                let mut method = substitute_type_params_in_method(md, &type_subs);
                                if method.original_role.is_none() {
                                    method.original_role = method.role_origin.clone();
                                }
                                method.role_origin = Some(base_role_name.to_string());
                                method
                            })
                            .collect()
                    };
                    class_def
                        .methods
                        .entry(mname.clone())
                        .or_default()
                        .extend(composed);
                }
                // Transfer wildcard handles from role to class
                for wh in &role.wildcard_handles {
                    if !class_def.wildcard_handles.contains(wh) {
                        class_def.wildcard_handles.push(wh.clone());
                    }
                }
                let role_param_values: HashMap<String, Value> = role_param_names
                    .iter()
                    .cloned()
                    .zip(role_arg_values.iter().cloned())
                    .collect();
                // Execute deferred body statements from parameterized roles
                // with concrete type parameter bindings. These statements
                // (e.g., `my T $v .= new;`) may create closure variables that
                // are referenced by composed methods, so we must keep their
                // effects on the env (only clean up the type capture markers).
                if !role.deferred_body_stmts.is_empty() {
                    // Bind type parameters as type captures
                    for (param_name, param_value) in &role_param_values {
                        self.bind_type_capture(param_name, param_value);
                    }
                    for stmt in &role.deferred_body_stmts {
                        self.run_block_raw(std::slice::from_ref(stmt))?;
                    }
                    // Remove type capture markers (but keep the variables
                    // created by the deferred stmts for method closures)
                    for param_name in role_param_values.keys() {
                        self.env.remove(&format!("__type_capture__{}", param_name));
                        // Don't remove the param name itself - methods may need it
                    }
                }
                if let Some(parent_specs) = self.role_parents.get(base_role_name).cloned() {
                    for parent_spec in parent_specs {
                        let resolved_parent = if let Some(v) = role_param_values.get(&parent_spec) {
                            type_value_name(v)
                        } else if let Some((pbase, _)) = parent_spec.split_once('[') {
                            let p_args_str = &parent_spec[pbase.len() + 1..parent_spec.len() - 1];
                            let p_args = parse_role_type_args(p_args_str)
                                .into_iter()
                                .map(|arg| {
                                    role_param_values
                                        .get(&arg)
                                        .map(|v| match v {
                                            Value::Package(name) => name.resolve(),
                                            other => other
                                                .to_string_value()
                                                .trim_start_matches('(')
                                                .trim_end_matches(')')
                                                .to_string(),
                                        })
                                        .unwrap_or(arg)
                                })
                                .collect::<Vec<_>>();
                            format!("{pbase}[{}]", p_args.join(","))
                        } else {
                            parent_spec.clone()
                        };
                        let parent_base = resolved_parent
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(resolved_parent.as_str());
                        if let Some(parent_role) = self.roles.get(parent_base).cloned() {
                            if !composed_roles_list.contains(&resolved_parent) {
                                composed_roles_list.push(resolved_parent.clone());
                            }
                            let parent_type_subs: Vec<(String, String)> =
                                if let Some(parent_tps) = self.role_type_params.get(parent_base) {
                                    if let Some(bracket_start) = resolved_parent.find('[') {
                                        let args_str = &resolved_parent
                                            [bracket_start + 1..resolved_parent.len() - 1];
                                        let args = parse_role_type_args(args_str);
                                        parent_tps
                                            .iter()
                                            .zip(args.iter())
                                            .map(|(p, a)| (p.clone(), a.clone()))
                                            .collect()
                                    } else {
                                        Vec::new()
                                    }
                                } else {
                                    Vec::new()
                                };
                            for attr in &parent_role.attributes {
                                if !class_def.attributes.iter().any(|(n, ..)| n == &attr.0) {
                                    class_def.attributes.push(attr.clone());
                                }
                            }
                            for (mname, overloads) in &parent_role.methods {
                                // Skip methods declared with `my` scope -- role-private
                                let non_my_overloads: Vec<&MethodDef> =
                                    overloads.iter().filter(|md| !md.is_my).collect();
                                if non_my_overloads.is_empty() {
                                    continue;
                                }
                                let composed: Vec<MethodDef> = if parent_type_subs.is_empty() {
                                    non_my_overloads
                                        .into_iter()
                                        .map(|md| {
                                            let mut method = md.clone();
                                            if method.original_role.is_none() {
                                                method.original_role = method.role_origin.clone();
                                            }
                                            method.role_origin = Some(parent_base.to_string());
                                            method
                                        })
                                        .collect()
                                } else {
                                    non_my_overloads
                                        .into_iter()
                                        .map(|md| {
                                            let mut method = substitute_type_params_in_method(
                                                md,
                                                &parent_type_subs,
                                            );
                                            if method.original_role.is_none() {
                                                method.original_role = method.role_origin.clone();
                                            }
                                            method.role_origin = Some(parent_base.to_string());
                                            method
                                        })
                                        .collect()
                                };
                                class_def
                                    .methods
                                    .entry(mname.clone())
                                    .or_default()
                                    .extend(composed);
                            }
                        } else if self.classes.contains_key(parent_base)
                            && !class_def.parents.iter().any(|p| p == &resolved_parent)
                        {
                            class_def.parents.push(resolved_parent.clone());
                        }
                    }
                }
            } else if does_parents.contains(parent) && self.enum_types.contains_key(base_role_name)
            {
                // Enum used as a role via `does`: record it for method dispatch
                self.class_enum_roles
                    .entry(name.to_string())
                    .or_default()
                    .push(base_role_name.to_string());
            }
        }
        if class_role_param_bindings.is_empty() {
            self.class_role_param_bindings.remove(name);
        } else {
            self.class_role_param_bindings
                .insert(name.to_string(), class_role_param_bindings);
        }
        // Handle role punning: `is Role` creates a punned class from the role
        for punned_role in &punned_roles {
            let base_role = punned_role
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(punned_role.as_str());
            // Create a punned class entry if one doesn't already exist
            if !self.classes.contains_key(punned_role.as_str())
                && !self.classes.contains_key(base_role)
            {
                // Collect class parents and composed roles recursively from role hierarchy
                let mut punned_class_parents = Vec::new();
                let mut punned_composed_roles = Vec::new();
                let mut role_stack = vec![base_role.to_string()];
                let mut seen_roles = HashSet::new();
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if let Some(rparents) = self.role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.roles.contains_key(rp_base) {
                                // It's a role - add as composed role and recurse
                                if !punned_composed_roles.contains(&rp) {
                                    punned_composed_roles.push(rp.clone());
                                }
                                role_stack.push(rp_base.to_string());
                            } else if self.classes.contains_key(rp_base)
                                && !punned_class_parents.contains(&rp)
                            {
                                // It's a class - add as parent
                                punned_class_parents.push(rp);
                            }
                        }
                    }
                }
                let punned_class = ClassDef {
                    parents: punned_class_parents,
                    attributes: Vec::new(),
                    attribute_types: HashMap::new(),
                    attribute_smileys: HashMap::new(),
                    methods: HashMap::new(),
                    native_methods: HashSet::new(),
                    mro: Vec::new(),
                    wildcard_handles: Vec::new(),
                    alias_attributes: HashSet::new(),
                    class_level_attrs: HashMap::new(),
                };
                self.classes.insert(base_role.to_string(), punned_class);
                if !punned_composed_roles.is_empty() {
                    self.class_composed_roles
                        .insert(base_role.to_string(), punned_composed_roles);
                }
                // Propagate hidden status from role to punned class
                if let Some(role_def) = self.roles.get(base_role)
                    && role_def.is_hidden
                {
                    self.hidden_classes.insert(base_role.to_string());
                }
                if hidden_punned_role_bases.contains(base_role) {
                    self.hidden_classes.insert(base_role.to_string());
                }
                // Recompute MRO for the punned class
                let mro = self.class_mro(base_role);
                if let Some(cd) = self.classes.get_mut(base_role) {
                    cd.mro = mro;
                }
            }
            if hidden_punned_role_bases.contains(base_role) {
                self.hidden_classes.insert(base_role.to_string());
            }
        }
        // Clear stale composed roles from previous registration
        self.class_composed_roles.remove(name);
        if !composed_roles_list.is_empty() {
            // Propagate role parent classes to the class (recursively through sub-roles)
            // When a role `R is C1` is composed into a class, C1 becomes a parent
            {
                let mut role_stack: Vec<String> = composed_roles_list
                    .iter()
                    .map(|r| {
                        r.split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(r.as_str())
                            .to_string()
                    })
                    .collect();
                let mut seen_roles = HashSet::new();
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if let Some(rparents) = self.role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.roles.contains_key(rp_base) {
                                // It's a sub-role, recurse
                                role_stack.push(rp_base.to_string());
                            } else if self.classes.contains_key(rp_base)
                                && !class_def.parents.contains(&rp)
                            {
                                class_def.parents.push(rp);
                            }
                        }
                    }
                }
            }
            self.class_composed_roles
                .insert(name.to_string(), composed_roles_list.clone());
            // Propagate `hides` from composed roles (and sub-roles) to the class
            {
                let mut role_stack: Vec<String> = composed_roles_list
                    .iter()
                    .map(|r| {
                        r.split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(r.as_str())
                            .to_string()
                    })
                    .collect();
                let mut seen_roles = HashSet::new();
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if let Some(hides_list) = self.role_hides.get(&role_name).cloned() {
                        for hidden in hides_list {
                            self.hidden_defer_parents
                                .entry(name.to_string())
                                .or_default()
                                .insert(hidden);
                        }
                    }
                    // Recurse into sub-roles
                    if let Some(rparents) = self.role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.roles.contains_key(rp_base) {
                                role_stack.push(rp_base.to_string());
                            }
                        }
                    }
                }
            }
        }
        for stmt in body {
            if let Stmt::TrustsDecl {
                name: trusted_class,
            } = stmt
            {
                self.class_trusts
                    .entry(name.to_string())
                    .or_default()
                    .insert(trusted_class.resolve());
            }
        }
        // Make the class visible while its body executes so introspection calls
        // like `A.^add_method(...)` inside the declaration can resolve `A`.
        self.classes.insert(name.to_string(), class_def.clone());
        if is_stub_body {
            self.class_stubs.insert(name.to_string());
            self.classes.insert(name.to_string(), class_def);
            let mut stack = Vec::new();
            let _ = self.compute_class_mro(name, &mut stack)?;
            return Ok(());
        }
        // Clear stub status now that the class has a real body.
        self.class_stubs.remove(name);
        // Also clear package stub status (for `package Foo { ... }; class Foo { }`)
        self.package_stubs.remove(name);
        let saved_package = self.current_package.clone();
        let saved_env = self.env.clone();
        self.current_package = name.to_string();
        self.env
            .insert("?CLASS".to_string(), Value::Package(Symbol::intern(name)));
        // Flatten SyntheticBlock (from `has ($a, $b)` list form) so inner
        // HasDecl statements are processed at the top level.
        let flattened_body: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        // Pre-scan: collect attribute names declared directly in this class body.
        // Combined with role-composed attributes already in class_def.attributes,
        // this gives the full set of attributes valid for $!attr access.
        let mut class_own_attrs: HashSet<String> = class_def
            .attributes
            .iter()
            .map(|(n, ..)| n.clone())
            .collect();
        for stmt in &flattened_body {
            if let Stmt::HasDecl {
                name: attr_name,
                is_our,
                is_my,
                ..
            } = stmt
                && !*is_our
                && !*is_my
            {
                class_own_attrs.insert(attr_name.resolve());
            }
        }
        for stmt in flattened_body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    handles,
                    is_rw,
                    is_readonly,
                    type_constraint,
                    type_smiley,
                    is_required,
                    sigil,
                    where_constraint,
                    is_alias,
                    is_our,
                    is_my,
                    is_default,
                    is_type,
                } => {
                    let attr_name_str = attr_name.resolve();

                    // Handle class-level attributes (our $.x / my $.x)
                    if *is_our || *is_my {
                        // Evaluate the default value if present
                        let initial_value = if let Some(expr) = default {
                            self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                        } else {
                            Value::Nil
                        };
                        class_def
                            .class_level_attrs
                            .insert(attr_name_str.clone(), initial_value);
                        // Skip per-instance attribute registration
                        continue;
                    }

                    // Check for duplicate attribute from role composition
                    if class_def
                        .attributes
                        .iter()
                        .any(|(n, ..)| n == &attr_name_str)
                    {
                        self.current_package = saved_package;
                        self.env = saved_env;
                        return Err(RuntimeError::new(format!(
                            "X::Comp::Trait::Duplicate: attribute '{}' already exists in class '{}' (possibly from role composition)",
                            attr_name_str, name,
                        )));
                    }
                    let effective_is_rw = !*is_readonly && (*is_rw || (class_is_rw && *is_public));
                    class_def.attributes.push((
                        attr_name_str.clone(),
                        *is_public,
                        default.clone(),
                        effective_is_rw,
                        is_required.clone(),
                        *sigil,
                        where_constraint.as_ref().map(|wc| wc.as_ref().clone()),
                    ));
                    // Store `is default(...)` trait value for this attribute.
                    // When is_default is set, the evaluated value is stored for
                    // .VAR.default and Nil-restore behavior.
                    // When only `default` is set (from `is default(X)` without `= value`),
                    // also store it as the is_default trait value.
                    if let Some(is_default_expr) = is_default {
                        if let Ok(val) =
                            self.eval_block_value(&[Stmt::Expr(is_default_expr.clone())])
                        {
                            self.class_attribute_defaults
                                .insert((name.to_string(), attr_name_str.clone()), val);
                        }
                    } else if default.is_some() {
                        // No explicit `is default(X)`, but there IS a `default` expr.
                        // This means either `has $.a = expr` or `has $.a is default(expr)` without `= value`.
                        // We can't distinguish here, so we DON'T set class_attribute_defaults
                        // (it would be wrong for `has $.a = 42` — Nil should give (Any), not 42).
                    }
                    if *is_alias {
                        class_def.alias_attributes.insert(attr_name_str.clone());
                    }
                    if let Some(tc) = type_constraint {
                        // Resolve ::?CLASS to the current class name
                        let resolved_tc = tc.replace("::?CLASS", name);
                        class_def
                            .attribute_types
                            .insert(attr_name_str.clone(), resolved_tc);
                    }
                    if let Some(ts) = type_smiley {
                        class_def
                            .attribute_smileys
                            .insert(attr_name_str.clone(), ts.clone());
                    }
                    if let Some(it) = is_type {
                        self.class_attribute_is_types
                            .insert((name.to_string(), attr_name_str.clone()), it.clone());
                    }
                    let attr_var_name = if *is_public {
                        format!(".{}", attr_name_str)
                    } else {
                        format!("!{}", attr_name_str)
                    };
                    self.apply_handle_specs(handles, &attr_var_name, &mut class_def);
                }
                Stmt::MethodDecl {
                    name: method_name,
                    name_expr,
                    params: _,
                    param_defs,
                    body: method_body,
                    multi,
                    is_rw,
                    is_private,
                    is_our,
                    is_my,
                    return_type,
                    is_default_candidate,
                    deprecated_message,
                } => {
                    self.validate_private_access_in_stmts(name, method_body)?;
                    Self::validate_attr_declared_in_class(&class_own_attrs, method_body)?;
                    let resolved_method_name = if let Some(expr) = name_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                            .to_string_value()
                    } else {
                        method_name.resolve()
                    };
                    let effective_param_defs =
                        Self::effective_method_param_defs(param_defs, is_hidden);
                    let effective_params: Vec<String> = effective_param_defs
                        .iter()
                        .map(|p| p.name.clone())
                        .collect();
                    let def = MethodDef {
                        params: effective_params.clone(),
                        param_defs: effective_param_defs.clone(),
                        body: std::sync::Arc::new(method_body.clone()),
                        is_rw: *is_rw,
                        is_private: *is_private,
                        is_multi: *multi,
                        is_my: *is_my,
                        role_origin: None,
                        original_role: None,
                        return_type: return_type.clone(),
                        compiled_code: None,
                        delegation: None,
                        is_default: *is_default_candidate,
                        deprecated_message: deprecated_message.clone(),
                    };
                    if *multi {
                        class_def
                            .methods
                            .entry(resolved_method_name.clone())
                            .or_default()
                            .push(def);
                    } else {
                        // Check for duplicate non-multi method definition.
                        // Only error if the existing method was defined in
                        // this class (not composed from a role).
                        if let Some(existing) = class_def.methods.get(&resolved_method_name) {
                            let all_from_role = existing.iter().all(|m| m.role_origin.is_some());
                            if !all_from_role {
                                return Err(RuntimeError::new(format!(
                                    "Package '{}' already has a method '{}' (did you mean to declare a multi method?)",
                                    name, resolved_method_name
                                )));
                            }
                        }
                        class_def
                            .methods
                            .insert(resolved_method_name.clone(), vec![def]);
                    }
                    // `our method` also registers as a package-scoped sub
                    if *is_our {
                        let qualified_name = format!("{}::{}", name, resolved_method_name);
                        let has_explicit_invocant = effective_param_defs
                            .iter()
                            .any(|p| p.is_invocant || p.traits.iter().any(|t| t == "invocant"));
                        let (our_params, our_param_defs) = if has_explicit_invocant {
                            (
                                effective_params.clone(),
                                effective_param_defs
                                    .iter()
                                    .filter(|p| p.name.as_str() != "self")
                                    .cloned()
                                    .collect(),
                            )
                        } else {
                            // Prepend "self" as first param so the first argument
                            // gets bound as `self` when calling this as a function.
                            let mut our_params = vec!["self".to_string()];
                            our_params.extend(
                                effective_params
                                    .iter()
                                    .filter(|p| p.as_str() != "self")
                                    .cloned(),
                            );
                            let self_param = crate::ast::ParamDef {
                                name: "self".to_string(),
                                default: None,
                                multi_invocant: true,
                                required: false,
                                named: false,
                                slurpy: false,
                                double_slurpy: false,
                                sigilless: false,
                                type_constraint: None,
                                literal_value: None,
                                sub_signature: None,
                                where_constraint: None,
                                traits: Vec::new(),
                                optional_marker: false,
                                outer_sub_signature: None,
                                code_signature: None,
                                is_invocant: false,
                                shape_constraints: None,
                            };
                            let mut our_param_defs = vec![self_param];
                            our_param_defs.extend(
                                effective_param_defs
                                    .iter()
                                    .filter(|p| {
                                        !(p.is_invocant || p.traits.iter().any(|t| t == "invocant"))
                                    })
                                    .cloned(),
                            );
                            (our_params, our_param_defs)
                        };
                        let func_def = crate::ast::FunctionDef {
                            package: Symbol::intern(name),
                            name: Symbol::intern(&resolved_method_name),
                            params: our_params,
                            param_defs: our_param_defs,
                            body: method_body.clone(),
                            is_test_assertion: false,
                            is_rw: *is_rw,
                            is_raw: false,
                            is_method: true,
                            empty_sig: false,
                            return_type: None,
                            is_default: *is_default_candidate,
                            deprecated_message: None,
                        };
                        self.functions
                            .insert(Symbol::intern(&qualified_name), func_def);
                    }
                }
                Stmt::DoesDecl { name: role_name } => {
                    let role_name_str = role_name.resolve();
                    if !self.roles.contains_key(&role_name_str)
                        && matches!(
                            role_name_str.as_str(),
                            "Real"
                                | "Numeric"
                                | "Cool"
                                | "Any"
                                | "Mu"
                                | "Positional"
                                | "Associative"
                        )
                    {
                        if !class_def.parents.iter().any(|p| p == &role_name_str) {
                            class_def.parents.insert(0, role_name_str.clone());
                            class_def.mro.clear();
                        }
                        continue;
                    }
                    let role = self.roles.get(&role_name_str).cloned().ok_or_else(|| {
                        RuntimeError::new(format!("Unknown role: {}", role_name_str))
                    })?;
                    if role.is_stub_role {
                        return Err(RuntimeError::typed_msg(
                            "X::Role::Parametric::NoSuchCandidate",
                            "No matching candidate found for the parametric role",
                        ));
                    }
                    for attr in &role.attributes {
                        if !class_def.attributes.iter().any(|(n, ..)| n == &attr.0) {
                            class_def.attributes.push(attr.clone());
                        }
                    }
                    for (mname, overloads) in role.methods {
                        let composed: Vec<MethodDef> = overloads
                            .into_iter()
                            .filter(|md| !md.is_my)
                            .map(|mut md| {
                                if md.original_role.is_none() {
                                    md.original_role = md.role_origin.clone();
                                }
                                md.role_origin = Some(role_name_str.clone());
                                md
                            })
                            .collect();
                        if composed.is_empty() {
                            continue;
                        }
                        class_def.methods.entry(mname).or_default().extend(composed);
                    }
                    // Transfer wildcard handles from role to class
                    for wh in &role.wildcard_handles {
                        if !class_def.wildcard_handles.contains(wh) {
                            class_def.wildcard_handles.push(wh.clone());
                        }
                    }
                    if !class_def.parents.iter().any(|p| p == &role_name_str) {
                        // Keep role composition visible in MRO introspection.
                        class_def.parents.insert(0, role_name_str.clone());
                        class_def.mro.clear();
                    }
                    // Transfer role's own parents (from `is` declarations) to the class
                    if let Some(rparents) = self.role_parents.get(&role_name_str).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.classes.contains_key(rp_base)
                                && !class_def.parents.iter().any(|p| p == &rp)
                            {
                                class_def.parents.push(rp.clone());
                                class_def.mro.clear();
                            }
                        }
                    }
                }
                Stmt::TrustsDecl {
                    name: trusted_class,
                } => {
                    self.class_trusts
                        .entry(name.to_string())
                        .or_default()
                        .insert(trusted_class.resolve());
                }
                // our &baz ::= &bar  — alias a method under a new name
                Stmt::VarDecl {
                    name: var_name,
                    expr: Expr::CodeVar(source_name),
                    ..
                } if var_name.starts_with('&') => {
                    let alias = var_name.trim_start_matches('&').to_string();
                    if let Some(overloads) = class_def.methods.get(source_name).cloned() {
                        class_def.methods.insert(alias, overloads);
                    }
                    // Also execute the statement so the code variable is set
                    self.classes.insert(name.to_string(), class_def.clone());
                    self.run_block_raw(std::slice::from_ref(stmt))?;
                    for outer_name in saved_env.keys() {
                        let class_scoped_name = format!("{}::{}", name, outer_name);
                        if let Some(updated) = self.env.get(&class_scoped_name).cloned() {
                            self.env.insert(outer_name.clone(), updated);
                        }
                    }
                    if let Some(updated) = self.classes.get(name).cloned() {
                        class_def = updated;
                    }
                }
                _ => {
                    self.classes.insert(name.to_string(), class_def.clone());
                    self.run_block_raw(std::slice::from_ref(stmt))?;
                    for outer_name in saved_env.keys() {
                        let class_scoped_name = format!("{}::{}", name, outer_name);
                        if let Some(updated) = self.env.get(&class_scoped_name).cloned() {
                            self.env.insert(outer_name.clone(), updated);
                        }
                    }
                    if let Some(updated) = self.classes.get(name).cloned() {
                        class_def = updated;
                    }
                }
            }
            self.classes.insert(name.to_string(), class_def.clone());
        }
        self.current_package = saved_package;
        if let Err(err) = self.resolve_class_stub_requirements(name, &mut class_def) {
            restore_previous_state(self);
            return Err(err);
        }
        if let Err(err) = self.detect_unresolved_role_method_conflicts(name, &class_def) {
            restore_previous_state(self);
            return Err(err);
        }
        self.classes.insert(name.to_string(), class_def);
        let mut stack = Vec::new();
        if let Err(err) = self.compute_class_mro(name, &mut stack) {
            restore_previous_state(self);
            return Err(err);
        }
        // Validate that all self!method() calls reference existing private methods
        if let Err(err) = self.validate_private_method_existence(name) {
            restore_previous_state(self);
            return Err(err);
        }
        Ok(())
    }

    /// Augment an existing class by adding methods (and attributes) from the body.
    /// This implements `augment class ClassName { ... }` (monkey-patching).
    pub(crate) fn augment_class(&mut self, name: &str, body: &[Stmt]) -> Result<(), RuntimeError> {
        self.clear_private_zeroarg_method_cache();
        // Check if the class exists (user-defined or builtin)
        let is_builtin = !self.classes.contains_key(name);
        if is_builtin {
            // For builtin types, create a minimal class def so we can add methods
            const BUILTIN_TYPES: &[&str] = &[
                "Mu",
                "Any",
                "Cool",
                "Int",
                "Num",
                "Str",
                "Bool",
                "Rat",
                "FatRat",
                "Complex",
                "Array",
                "Hash",
                "List",
                "Map",
                "Set",
                "Bag",
                "Mix",
                "Range",
                "Pair",
                "IO",
                "IO::Path",
                "IO::Handle",
                "Regex",
                "Match",
                "Junction",
                "Exception",
                "Failure",
                "Version",
                "Nil",
                "Block",
                "Code",
                "Routine",
                "Sub",
                "Method",
                "Seq",
                "Slip",
                "Whatever",
                "WhateverCode",
                "HyperWhatever",
                "Callable",
                "Numeric",
                "Real",
                "Stringy",
                "Positional",
                "Associative",
                "Order",
                "Endian",
                "Proc",
            ];
            if !BUILTIN_TYPES.contains(&name) {
                return Err(RuntimeError::new(format!(
                    "You tried to augment class {}, but it does not exist",
                    name
                )));
            }
            // Create a minimal entry for the builtin type
            self.classes.entry(name.to_string()).or_default();
        }

        let saved_package = self.current_package.clone();
        self.current_package = name.to_string();

        // Process body statements and add methods/attributes to the existing class
        let flattened_body: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        for stmt in flattened_body {
            match stmt {
                Stmt::MethodDecl {
                    name: method_name,
                    name_expr,
                    param_defs,
                    body: method_body,
                    multi,
                    is_rw,
                    is_private,
                    is_my,
                    return_type,
                    is_default_candidate,
                    ..
                } => {
                    let resolved_method_name = if let Some(expr) = name_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                            .to_string_value()
                    } else {
                        method_name.resolve()
                    };
                    let effective_param_defs = Self::effective_method_param_defs(param_defs, false);
                    let effective_params: Vec<String> = effective_param_defs
                        .iter()
                        .map(|p| p.name.clone())
                        .collect();
                    let def = MethodDef {
                        params: effective_params,
                        param_defs: effective_param_defs,
                        body: std::sync::Arc::new(method_body.clone()),
                        is_rw: *is_rw,
                        is_private: *is_private,
                        is_multi: *multi,
                        is_my: *is_my,
                        role_origin: None,
                        original_role: None,
                        return_type: return_type.clone(),
                        compiled_code: None,
                        delegation: None,
                        is_default: *is_default_candidate,
                        deprecated_message: None,
                    };
                    if let Some(class_def) = self.classes.get_mut(name) {
                        if *multi {
                            class_def
                                .methods
                                .entry(resolved_method_name)
                                .or_default()
                                .push(def);
                        } else {
                            // Check for duplicate non-multi method definition.
                            // Only error if the existing method was defined in
                            // this class (not composed from a role).
                            if let Some(existing) = class_def.methods.get(&resolved_method_name) {
                                let all_from_role =
                                    existing.iter().all(|m| m.role_origin.is_some());
                                if !all_from_role {
                                    return Err(RuntimeError::new(format!(
                                        "Package '{}' already has a method '{}' (did you mean to declare a multi method?)",
                                        name, resolved_method_name
                                    )));
                                }
                            }
                            class_def.methods.insert(resolved_method_name, vec![def]);
                        }
                    }
                }
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    is_rw,
                    is_readonly: _,
                    type_constraint,
                    type_smiley: _,
                    is_required,
                    sigil,
                    handles,
                    where_constraint,
                    is_alias,
                    is_our: _,
                    is_my: _,
                    is_default: _,
                    is_type: _,
                } => {
                    let attr_name_str = attr_name.resolve();
                    let attr_var_name = if *is_public {
                        format!(".{}", attr_name_str)
                    } else {
                        format!("!{}", attr_name_str)
                    };
                    // Resolve handles before taking mutable borrow on class_def
                    let resolved = self.resolve_handle_specs_to_names(handles, &attr_var_name);
                    if let Some(class_def) = self.classes.get_mut(name) {
                        class_def.attributes.push((
                            attr_name_str.clone(),
                            *is_public,
                            default.clone(),
                            *is_rw,
                            is_required.clone(),
                            *sigil,
                            where_constraint.as_ref().map(|wc| wc.as_ref().clone()),
                        ));
                        if *is_alias {
                            class_def.alias_attributes.insert(attr_name_str.clone());
                        }
                        if let Some(tc) = type_constraint {
                            class_def
                                .attribute_types
                                .insert(attr_name_str.clone(), tc.clone());
                        }
                        apply_resolved_handles(
                            &resolved,
                            &mut class_def.methods,
                            &mut class_def.wildcard_handles,
                        );
                    }
                }
                // Execute other statements in the class body (e.g., sub declarations)
                other => {
                    let _ = self.eval_block_value(std::slice::from_ref(other));
                }
            }
        }

        self.current_package = saved_package;
        Ok(())
    }

    pub(crate) fn register_role_decl(
        &mut self,
        name: &str,
        type_params: &[String],
        type_param_defs: &[ParamDef],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        self.clear_private_zeroarg_method_cache();

        // Check for our-scoped declarations inside the role body.
        // In Raku, class/subset/enum/constant/role are implicitly our-scoped,
        // and explicit `our sub/method/variable` are also forbidden inside roles.
        let check_body: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        for stmt in &check_body {
            let declaration = match stmt {
                Stmt::ClassDecl { .. } => Some("class"),
                Stmt::SubsetDecl { .. } => Some("subset"),
                Stmt::EnumDecl { .. } => Some("enum"),
                Stmt::RoleDecl { .. } => Some("role"),
                Stmt::VarDecl {
                    is_our: true,
                    custom_traits,
                    ..
                } => {
                    if custom_traits.iter().any(|(t, _)| t == "__constant") {
                        Some("constant")
                    } else {
                        Some("variable")
                    }
                }
                Stmt::SubDecl { custom_traits, .. }
                    if custom_traits.iter().any(|t| t == "__our_scoped") =>
                {
                    Some("sub")
                }
                Stmt::MethodDecl { is_our: true, .. } => Some("method"),
                _ => None,
            };
            if let Some(decl) = declaration {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("declaration".to_string(), Value::str(decl.to_string()));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Cannot declare our-scoped {} inside of a role",
                        decl
                    )),
                );
                return Err(RuntimeError::typed("X::Declaration::OurScopeInRole", attrs));
            }
        }

        for param_def in type_param_defs {
            if param_def.name == "__type_only__"
                && let Some(type_name) = param_def.type_constraint.as_deref()
                && !type_name.starts_with("::")
                && !self.is_resolvable_type(type_name)
            {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("type".to_string(), Value::str(type_name.to_string()));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Invalid type '{}' used in role parameter list",
                        type_name
                    )),
                );
                return Err(RuntimeError::typed("X::Parameter::InvalidType", attrs));
            }
        }

        // Clean up stale punned class entry for this role name.
        self.classes.remove(name);
        self.hidden_classes.remove(name);
        self.class_composed_roles.remove(name);
        // When registering a parametric variant of an existing non-parametric role
        // (forming a role group), save the non-parametric role's parents so we can
        // restore them after the parametric variant adds its own parents.
        let prev_parents = if !type_params.is_empty()
            && self
                .roles
                .get(name)
                .is_some_and(|existing| !existing.is_stub_role)
        {
            self.role_parents.get(name).cloned()
        } else {
            None
        };
        self.role_parents.remove(name);
        self.role_hides.remove(name);
        let mut role_def = RoleDef {
            attributes: Vec::new(),
            methods: HashMap::new(),
            is_stub_role: false,
            is_hidden: false,
            captured_env: None,
            wildcard_handles: Vec::new(),
            role_id: super::next_role_id(),
            attribute_conflicts: Vec::new(),
            deferred_body_stmts: Vec::new(),
        };
        let is_parametric = !type_params.is_empty();
        let flattened_body: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        for stmt in flattened_body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    handles,
                    is_rw,
                    is_readonly: _,
                    type_constraint: _,
                    type_smiley: _,
                    is_required,
                    sigil,
                    where_constraint,
                    is_alias: _,
                    is_our: _,
                    is_my: _,
                    is_default: _,
                    is_type: _,
                } => {
                    let attr_name_str = attr_name.resolve();
                    // Check if this attribute already exists from a composed role
                    if let Some(existing) = role_def
                        .attributes
                        .iter()
                        .find(|(n, ..)| n == &attr_name_str)
                    {
                        // The attribute already exists from a parent role composition.
                        // Record the conflict; the existing one came from a composed role.
                        // We need to figure out which role contributed it.
                        let parent_role = self
                            .role_parents
                            .get(name)
                            .and_then(|parents| {
                                parents.iter().find(|p| {
                                    let base =
                                        p.split_once('[').map(|(b, _)| b).unwrap_or(p.as_str());
                                    self.roles.get(base).is_some_and(|r| {
                                        r.attributes.iter().any(|(n, ..)| n == &attr_name_str)
                                    })
                                })
                            })
                            .cloned()
                            .unwrap_or_else(|| "unknown".to_string());
                        let _ = existing;
                        role_def.attribute_conflicts.push((
                            attr_name_str.clone(),
                            name.to_string(),
                            parent_role,
                        ));
                    }
                    role_def.attributes.push((
                        attr_name_str.clone(),
                        *is_public,
                        default.clone(),
                        *is_rw,
                        is_required.clone(),
                        *sigil,
                        where_constraint.as_ref().map(|wc| wc.as_ref().clone()),
                    ));
                    let attr_var_name = if *is_public {
                        format!(".{}", attr_name_str)
                    } else {
                        format!("!{}", attr_name_str)
                    };
                    self.apply_handle_specs_to_role(handles, &attr_var_name, &mut role_def);
                }
                Stmt::DoesDecl { name: role_name } => {
                    if *role_name == "__mutsu_role_hidden__" {
                        role_def.is_hidden = true;
                        continue;
                    }
                    let role_name_str = role_name.resolve();
                    if let Some(hidden_name) = role_name_str.strip_prefix("__mutsu_role_hides__") {
                        // Track hidden class relationship for this role
                        self.role_hides
                            .entry(name.to_string())
                            .or_default()
                            .push(hidden_name.to_string());
                        continue;
                    }
                    if self.classes.contains_key(&role_name_str) {
                        self.role_parents
                            .entry(name.to_string())
                            .or_default()
                            .push(role_name_str);
                        continue;
                    }
                    let base_role_name = role_name_str
                        .split_once('[')
                        .map(|(b, _)| b)
                        .unwrap_or(role_name_str.as_str());
                    if type_params.iter().any(|tp| tp == base_role_name)
                        || (!self.roles.contains_key(base_role_name)
                            && matches!(
                                base_role_name,
                                "Real"
                                    | "Numeric"
                                    | "Cool"
                                    | "Any"
                                    | "Mu"
                                    | "Positional"
                                    | "Associative"
                            ))
                    {
                        self.role_parents
                            .entry(name.to_string())
                            .or_default()
                            .push(role_name_str);
                        continue;
                    }
                    let role = self.roles.get(base_role_name).cloned().ok_or_else(|| {
                        RuntimeError::new(format!("Unknown role: {}", role_name_str))
                    })?;
                    if role.is_stub_role {
                        return Err(RuntimeError::typed_msg(
                            "X::Role::Parametric::NoSuchCandidate",
                            "No matching candidate found for the parametric role",
                        ));
                    }
                    self.role_parents
                        .entry(name.to_string())
                        .or_default()
                        .push(role_name_str.clone());
                    let type_subs: Vec<(String, String)> = if let Some(parent_type_params) =
                        self.role_type_params.get(base_role_name)
                    {
                        if let Some(bracket_start) = role_name_str.find('[') {
                            let args_str =
                                &role_name_str[bracket_start + 1..role_name_str.len() - 1];
                            let type_args = parse_role_type_args(args_str);
                            parent_type_params
                                .iter()
                                .zip(type_args.iter())
                                .map(|(p, a)| (p.clone(), a.clone()))
                                .collect()
                        } else {
                            Vec::new()
                        }
                    } else {
                        Vec::new()
                    };
                    for attr in &role.attributes {
                        if role_def.attributes.iter().any(|(n, ..)| n == &attr.0) {
                            // Attribute conflict: declared in both current role and parent role
                            role_def.attribute_conflicts.push((
                                attr.0.clone(),
                                name.to_string(),
                                base_role_name.to_string(),
                            ));
                        } else {
                            role_def.attributes.push(attr.clone());
                        }
                    }
                    for (mname, overloads) in role.methods {
                        // Skip methods declared with `my` scope -- role-private
                        let non_my_overloads: Vec<MethodDef> =
                            overloads.into_iter().filter(|md| !md.is_my).collect();
                        if non_my_overloads.is_empty() {
                            continue;
                        }
                        let composed: Vec<MethodDef> = if type_subs.is_empty() {
                            non_my_overloads
                                .into_iter()
                                .map(|mut md| {
                                    if md.original_role.is_none() {
                                        md.original_role = md.role_origin.clone();
                                    }
                                    md.role_origin = Some(base_role_name.to_string());
                                    md
                                })
                                .collect()
                        } else {
                            non_my_overloads
                                .iter()
                                .map(|md| {
                                    let mut method =
                                        substitute_type_params_in_method(md, &type_subs);
                                    if method.original_role.is_none() {
                                        method.original_role = method.role_origin.clone();
                                    }
                                    method.role_origin = Some(base_role_name.to_string());
                                    method
                                })
                                .collect()
                        };
                        role_def.methods.entry(mname).or_default().extend(composed);
                    }
                }
                Stmt::MethodDecl {
                    name: method_name,
                    name_expr,
                    params,
                    param_defs,
                    body: method_body,
                    multi,
                    is_rw,
                    is_private,
                    is_our: _,
                    is_my,
                    return_type,
                    is_default_candidate,
                    deprecated_message: _,
                } => {
                    if *multi
                        && (param_defs.iter().any(|pd| {
                            pd.type_constraint
                                .as_deref()
                                .is_some_and(|tc| tc.contains("?CLASS"))
                        }) || return_type
                            .as_deref()
                            .is_some_and(|rt| rt.contains("?CLASS")))
                    {
                        return Err(RuntimeError::typed_msg(
                            "X::Role::Unimplemented::Multi",
                            "Unimplemented multi method from role",
                        ));
                    }
                    let resolved_method_name = if let Some(expr) = name_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                            .to_string_value()
                    } else {
                        method_name.resolve()
                    };
                    let def = MethodDef {
                        params: params.clone(),
                        param_defs: param_defs.clone(),
                        body: std::sync::Arc::new(method_body.clone()),
                        is_rw: *is_rw,
                        is_private: *is_private,
                        is_multi: *multi,
                        is_my: *is_my,
                        role_origin: None,
                        original_role: None,
                        return_type: return_type.clone(),
                        compiled_code: None,
                        delegation: None,
                        is_default: *is_default_candidate,
                        deprecated_message: None,
                    };
                    if *multi {
                        role_def
                            .methods
                            .entry(resolved_method_name)
                            .or_default()
                            .push(def);
                    } else {
                        role_def.methods.insert(resolved_method_name, vec![def]);
                    }
                }
                Stmt::Expr(Expr::Call { name, .. })
                    if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn" =>
                {
                    role_def.is_stub_role = true;
                }
                Stmt::SetLine(_) => {
                    // Skip source line annotations
                }
                _ => {
                    if is_parametric {
                        // Defer non-method/non-attribute statements until composition
                        // time so they can be re-evaluated with concrete type bindings.
                        role_def.deferred_body_stmts.push(stmt.clone());
                    } else {
                        self.run_block_raw(std::slice::from_ref(stmt))?;
                    }
                }
            }
        }
        // Capture the current environment for anonymous roles so that attribute
        // defaults referencing closure variables can be evaluated later.
        let has_expr_default = role_def
            .attributes
            .iter()
            .any(|(_, _, default, ..)| default.is_some());
        if has_expr_default {
            role_def.captured_env = Some((*self.env).clone());
        }
        // Capture the parents that were added during this registration
        // (these are the parents specific to this candidate).
        let candidate_parents = self.role_parents.get(name).cloned().unwrap_or_default();
        self.role_candidates
            .entry(name.to_string())
            .or_default()
            .push(RoleCandidateDef {
                type_params: type_params.to_vec(),
                type_param_defs: type_param_defs.to_vec(),
                role_def: role_def.clone(),
                parents: candidate_parents,
            });
        if self
            .roles
            .get(name)
            .is_none_or(|existing| existing.is_stub_role || type_params.is_empty())
        {
            self.roles.insert(name.to_string(), role_def);
        }
        if !type_params.is_empty() && !self.role_type_params.contains_key(name) {
            self.role_type_params
                .insert(name.to_string(), type_params.to_vec());
        }
        // When a parametric variant was registered over an existing non-parametric
        // role (forming a role group), merge the previous parents back into
        // role_parents so that role_parent_args_for can find all candidates' parents.
        if let Some(prev) = prev_parents {
            let current = self.role_parents.entry(name.to_string()).or_default();
            for p in prev {
                if !current.contains(&p) {
                    current.push(p);
                }
            }
        }
        Ok(())
    }

    pub(crate) fn register_subset_decl(
        &mut self,
        name: &str,
        base: &str,
        predicate: Option<&Expr>,
        version: &str,
    ) {
        // When the predicate is `* ~~ <expr>` (Whatever on LHS of SmartMatch),
        // the parser doesn't wrap it as WhateverCode (to avoid breaking other
        // smartmatch semantics). Convert it here to a Lambda so the subset
        // check correctly evaluates `$_ ~~ <expr>` against the candidate value.
        let predicate = predicate.map(|pred| {
            if let Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::SmartMatch,
                right,
            } = pred
                && matches!(left.as_ref(), Expr::Whatever)
            {
                return Expr::Lambda {
                    param: "_".to_string(),
                    body: vec![Stmt::Expr(Expr::Binary {
                        left: Box::new(Expr::Var("_".to_string())),
                        op: crate::token_kind::TokenKind::SmartMatch,
                        right: right.clone(),
                    })],
                };
            }
            pred.clone()
        });
        self.subsets.insert(
            name.to_string(),
            SubsetDef {
                base: base.to_string(),
                predicate,
                version: version.to_string(),
            },
        );
        self.env
            .insert(name.to_string(), Value::Package(Symbol::intern(name)));
    }

    pub(crate) fn register_cunion_class(&mut self, name: &str) {
        self.clear_private_zeroarg_method_cache();
        self.cunion_classes.insert(name.to_string());
    }

    /// Construct a CUnion instance: all native-int fields share the same
    /// underlying bytes (little-endian), so setting one field also sets the
    /// lower bits of the others.
    pub(crate) fn construct_cunion_instance(
        &mut self,
        class_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::native_types::native_int_bounds;

        // Collect class attributes with their types
        let class_attrs = self.collect_class_attributes(class_name);

        // Parse named args
        let mut named_args: HashMap<String, Value> = HashMap::new();
        for arg in args {
            if let Value::Pair(key, value) = arg {
                named_args.insert(key.clone(), *value.clone());
            }
        }

        // Find the widest provided value and convert to bytes
        let mut max_bytes = 0u32;
        let mut raw_value: u64 = 0;

        for (attr_name, _is_public, _default, _is_rw, _, _, _) in &class_attrs {
            if let Some(val) = named_args.get(attr_name) {
                let int_val = match val {
                    Value::Int(i) => *i as u64,
                    Value::BigInt(n) => {
                        use num_traits::ToPrimitive;
                        n.to_u64().unwrap_or(0)
                    }
                    _ => 0,
                };
                raw_value = int_val;
                // Find the type of this attribute to determine byte width
                if let Some(type_constraint) = self.get_attr_type_constraint(class_name, attr_name)
                {
                    let byte_width = Self::native_type_byte_width(&type_constraint);
                    if byte_width > max_bytes {
                        max_bytes = byte_width;
                    }
                }
            }
        }

        // Build attributes from shared bytes
        let bytes = raw_value.to_le_bytes();
        let mut attrs = HashMap::new();
        for (attr_name, _is_public, default, _is_rw, _, _, _) in &class_attrs {
            if let Some(type_constraint) = self.get_attr_type_constraint(class_name, attr_name) {
                let byte_width = Self::native_type_byte_width(&type_constraint);
                let val = match byte_width {
                    1 => Value::Int(bytes[0] as i64),
                    2 => Value::Int(u16::from_le_bytes([bytes[0], bytes[1]]) as i64),
                    4 => Value::Int(
                        u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as i64
                    ),
                    8 => {
                        let v = u64::from_le_bytes(bytes);
                        if let Some((_min, _max)) = native_int_bounds(&type_constraint) {
                            if type_constraint.starts_with('u') || type_constraint == "byte" {
                                // Unsigned: store as BigInt if needed
                                if v > i64::MAX as u64 {
                                    Value::bigint(num_bigint::BigInt::from(v as u128))
                                } else {
                                    Value::Int(v as i64)
                                }
                            } else {
                                // Signed: reinterpret as i64
                                Value::Int(v as i64)
                            }
                        } else {
                            Value::Int(v as i64)
                        }
                    }
                    _ => {
                        if let Some(v) = named_args.get(attr_name) {
                            v.clone()
                        } else if let Some(expr) = default {
                            self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                        } else {
                            Value::Int(0)
                        }
                    }
                };
                attrs.insert(attr_name.clone(), val);
            } else if let Some(v) = named_args.get(attr_name) {
                attrs.insert(attr_name.clone(), v.clone());
            } else if let Some(expr) = default {
                attrs.insert(
                    attr_name.clone(),
                    self.eval_block_value(&[Stmt::Expr(expr.clone())])?,
                );
            } else {
                attrs.insert(attr_name.clone(), Value::Int(0));
            }
        }

        Ok(Value::make_instance(Symbol::intern(class_name), attrs))
    }

    /// Get the type constraint for a class attribute, searching MRO.
    pub(super) fn get_attr_type_constraint(
        &self,
        class_name: &str,
        attr_name: &str,
    ) -> Option<String> {
        if let Some(class_def) = self.classes.get(class_name) {
            for (name, _is_public, _default, _is_rw, _, _, _) in &class_def.attributes {
                if name == attr_name {
                    return class_def.attribute_types.get(attr_name).cloned();
                }
            }
        }
        None
    }

    fn native_type_byte_width(type_name: &str) -> u32 {
        match type_name {
            "uint8" | "int8" | "byte" => 1,
            "uint16" | "int16" => 2,
            "uint32" | "int32" => 4,
            "uint64" | "int64" | "uint" | "int" => 8,
            _ => 0,
        }
    }

    /// Auto-pun a role into a class so it can be instantiated (e.g. via COERCE or new).
    /// If the role is already registered as a class, this is a no-op.
    pub(crate) fn ensure_role_punned_to_class(&mut self, role_name: &str) {
        if self.classes.contains_key(role_name) {
            return;
        }
        self.clear_private_zeroarg_method_cache();
        let role_def = match self.roles.get(role_name) {
            Some(r) => r.clone(),
            None => return,
        };
        // Collect attributes and methods from the role itself and all composed parent roles
        let mut all_attributes = role_def.attributes.clone();
        let mut all_methods: HashMap<String, Vec<MethodDef>> = role_def.methods.clone();
        let mut composed_roles_list = vec![role_name.to_string()];
        if let Some(parent_names) = self.role_parents.get(role_name).cloned() {
            let mut role_stack: Vec<String> = parent_names;
            while let Some(parent_role_name) = role_stack.pop() {
                if !composed_roles_list.contains(&parent_role_name) {
                    composed_roles_list.push(parent_role_name.clone());
                    if let Some(parent_role) = self.roles.get(&parent_role_name).cloned() {
                        for attr in &parent_role.attributes {
                            // ClassAttributeDef is a tuple; field 0 is the attribute name
                            if !all_attributes.iter().any(|a| a.0 == attr.0) {
                                all_attributes.push(attr.clone());
                            }
                        }
                        for (method_name, method_defs) in &parent_role.methods {
                            all_methods
                                .entry(method_name.clone())
                                .or_default()
                                .extend(method_defs.clone());
                        }
                    }
                    // Also recurse into grandparent roles
                    if let Some(grandparents) = self.role_parents.get(&parent_role_name).cloned() {
                        for gp_name in &grandparents {
                            if !composed_roles_list.contains(gp_name) {
                                role_stack.push(gp_name.clone());
                            }
                        }
                    }
                }
            }
        }
        let punned_class = ClassDef {
            parents: Vec::new(),
            attributes: all_attributes,
            attribute_types: HashMap::new(),
            attribute_smileys: HashMap::new(),
            methods: all_methods,
            native_methods: HashSet::new(),
            mro: vec![role_name.to_string(), "Any".to_string(), "Mu".to_string()],
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: HashMap::new(),
        };
        self.classes.insert(role_name.to_string(), punned_class);
        // Register the role and its composed roles
        self.class_composed_roles
            .insert(role_name.to_string(), composed_roles_list);
    }

    /// Validate that all `$!attr` references in method bodies refer to attributes
    /// declared directly in the current class (not inherited from a parent).
    /// In Raku, private attributes are scoped to the declaring class.
    fn validate_attr_declared_in_class(
        class_own_attrs: &HashSet<String>,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        for stmt in stmts {
            Self::validate_attr_in_stmt(class_own_attrs, stmt)?;
        }
        Ok(())
    }

    fn validate_attr_in_stmt(
        class_own_attrs: &HashSet<String>,
        stmt: &Stmt,
    ) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e) => {
                Self::validate_attr_in_expr(class_own_attrs, e)?;
            }
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                Self::validate_attr_in_expr(class_own_attrs, expr)?;
            }
            Stmt::Say(exprs) | Stmt::Put(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
                for e in exprs {
                    Self::validate_attr_in_expr(class_own_attrs, e)?;
                }
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                Self::validate_attr_in_expr(class_own_attrs, cond)?;
                Self::validate_attr_declared_in_class(class_own_attrs, then_branch)?;
                Self::validate_attr_declared_in_class(class_own_attrs, else_branch)?;
            }
            Stmt::While { cond, body, .. } => {
                Self::validate_attr_in_expr(class_own_attrs, cond)?;
                Self::validate_attr_declared_in_class(class_own_attrs, body)?;
            }
            Stmt::For { iterable, body, .. } => {
                Self::validate_attr_in_expr(class_own_attrs, iterable)?;
                Self::validate_attr_declared_in_class(class_own_attrs, body)?;
            }
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                ..
            } => {
                if let Some(init) = init.as_ref() {
                    Self::validate_attr_in_stmt(class_own_attrs, init)?;
                }
                if let Some(cond) = cond.as_ref() {
                    Self::validate_attr_in_expr(class_own_attrs, cond)?;
                }
                if let Some(step) = step.as_ref() {
                    Self::validate_attr_in_expr(class_own_attrs, step)?;
                }
                Self::validate_attr_declared_in_class(class_own_attrs, body)?;
            }
            Stmt::Given { topic, body, .. } => {
                Self::validate_attr_in_expr(class_own_attrs, topic)?;
                Self::validate_attr_declared_in_class(class_own_attrs, body)?;
            }
            Stmt::When { cond, body, .. } => {
                Self::validate_attr_in_expr(class_own_attrs, cond)?;
                Self::validate_attr_declared_in_class(class_own_attrs, body)?;
            }
            Stmt::Default(body) => {
                Self::validate_attr_declared_in_class(class_own_attrs, body)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn validate_attr_in_expr(
        class_own_attrs: &HashSet<String>,
        expr: &Expr,
    ) -> Result<(), RuntimeError> {
        match expr {
            Expr::Var(name) => {
                if let Some(attr_name) = name.strip_prefix('!') {
                    // $! by itself is the error variable, not an attribute
                    if !attr_name.is_empty() && !class_own_attrs.contains(attr_name) {
                        let mut attrs = HashMap::new();
                        attrs.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
                        let ex =
                            Value::make_instance(Symbol::intern("X::Attribute::Undeclared"), attrs);
                        let mut err = RuntimeError::new(format!(
                            "Attribute $!{} not declared in class",
                            attr_name
                        ));
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                }
            }
            Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
                Self::validate_attr_in_expr(class_own_attrs, target)?;
                for arg in args {
                    Self::validate_attr_in_expr(class_own_attrs, arg)?;
                }
            }
            Expr::Call { args, .. }
            | Expr::ArrayLiteral(args)
            | Expr::BracketArray(args, _)
            | Expr::StringInterpolation(args) => {
                for arg in args {
                    Self::validate_attr_in_expr(class_own_attrs, arg)?;
                }
            }
            Expr::Unary { expr, .. }
            | Expr::PostfixOp { expr, .. }
            | Expr::Reduction { expr, .. } => {
                Self::validate_attr_in_expr(class_own_attrs, expr)?;
            }
            Expr::Binary { left, right, .. }
            | Expr::MetaOp { left, right, .. }
            | Expr::HyperOp { left, right, .. } => {
                Self::validate_attr_in_expr(class_own_attrs, left)?;
                Self::validate_attr_in_expr(class_own_attrs, right)?;
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                Self::validate_attr_in_expr(class_own_attrs, cond)?;
                Self::validate_attr_in_expr(class_own_attrs, then_expr)?;
                Self::validate_attr_in_expr(class_own_attrs, else_expr)?;
            }
            Expr::Index { target, index } => {
                Self::validate_attr_in_expr(class_own_attrs, target)?;
                Self::validate_attr_in_expr(class_own_attrs, index)?;
            }
            Expr::IndexAssign {
                target,
                index,
                value,
            } => {
                Self::validate_attr_in_expr(class_own_attrs, target)?;
                Self::validate_attr_in_expr(class_own_attrs, index)?;
                Self::validate_attr_in_expr(class_own_attrs, value)?;
            }
            Expr::AssignExpr { expr, .. } => {
                Self::validate_attr_in_expr(class_own_attrs, expr)?;
            }
            Expr::DoBlock { body, .. }
            | Expr::Block(body)
            | Expr::Gather(body)
            | Expr::AnonSub { body, .. }
            | Expr::AnonSubParams { body, .. }
            | Expr::Lambda { body, .. } => {
                Self::validate_attr_declared_in_class(class_own_attrs, body)?;
            }
            Expr::Try { body, catch } => {
                Self::validate_attr_declared_in_class(class_own_attrs, body)?;
                if let Some(catch) = catch.as_ref() {
                    Self::validate_attr_declared_in_class(class_own_attrs, catch)?;
                }
            }
            Expr::DoStmt(stmt) => {
                Self::validate_attr_in_stmt(class_own_attrs, stmt)?;
            }
            _ => {}
        }
        Ok(())
    }

    /// Store a specific language version as type metadata for ^language-revision.
    pub(crate) fn store_language_revision_from_version(&mut self, name: &str, version: &str) {
        let revision = language_revision_letter(version);
        let meta = self.type_metadata.entry(name.to_string()).or_default();
        meta.insert("language-revision".to_string(), Value::str(revision));
    }
}

/// Extract the language revision letter from a version string like "6.c", "6.d", "6.e".
fn language_revision_letter(version: &str) -> String {
    // Version is like "6.c", "6.d", "6.e" — extract the letter after the dot
    if let Some(letter) = version.strip_prefix("6.") {
        letter.chars().next().unwrap_or('c').to_string()
    } else {
        "c".to_string()
    }
}
