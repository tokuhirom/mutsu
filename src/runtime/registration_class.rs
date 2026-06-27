use super::*;
use crate::ast::{HandleSpec, ParamDef};

pub(super) type ResolvedRoleCandidate = (RoleDef, Vec<String>, Vec<Value>);

/// Whether a built-in type is a concrete (non-composable) class that cannot be
/// `does`-composed by a class — `class B does Int {}` is
/// X::Composition::NotComposable. This is a denylist of the concrete value/object
/// types; everything else built-in that is `does`-able is a role
/// (Real/Numeric/Positional/Baggy/Setty/Stringy/...) and composes fine. A
/// denylist is used (rather than an allowlist of composable roles) so new or
/// uncommon built-in roles like Baggy/Setty/Mixy are not wrongly rejected.
pub(super) fn is_non_composable_builtin(name: &str) -> bool {
    matches!(
        name,
        "Int" | "UInt" | "Str" | "Num" | "Rat" | "FatRat" | "Complex" | "Cool" | "Any" | "Mu"
    )
}

pub(super) fn type_value_name(value: &Value) -> String {
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

pub(super) fn builtin_role_def() -> RoleDef {
    RoleDef {
        attributes: Vec::new(),
        methods: HashMap::new(),
        is_stub_role: false,
        is_hidden: false,
        is_rw: false,
        captured_env: None,
        wildcard_handles: Vec::new(),
        role_id: 0,
        attribute_conflicts: Vec::new(),
        own_attribute_names: HashSet::new(),
        deferred_body_stmts: Vec::new(),
        deferred_custom_traits: Vec::new(),
    }
}

/// Intermediate representation for resolved handle specs.
pub(crate) enum ResolvedHandle {
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
pub(super) fn apply_resolved_handles(
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
        onearg: false,
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
pub(super) fn make_delegation_method(attr_var_name: &str, target_method: &str) -> MethodDef {
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
        is_submethod: false,
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
        onearg: false,
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
    pub(crate) is_lexical: bool,
    pub(crate) hidden_parents: &'a [String],
    pub(crate) does_parents: &'a [String],
    /// Language version of the class being declared (e.g. "6.c", "6.d", "6.e").
    /// Used to determine whether submethods from composed roles should be included.
    pub(crate) language_version: &'a str,
}

pub(super) fn parse_role_type_args(input: &str) -> Vec<String> {
    split_balanced_comma_list(input)
}

pub(super) fn looks_like_type_arg_expr(input: &str) -> bool {
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

pub(super) fn should_treat_role_arg_as_type_expr(input: &str) -> bool {
    let trimmed = input.trim();
    // Colonpair syntax like `:a(1)` or `:foo(42)` is a named argument, not a type.
    if trimmed.starts_with(':')
        && trimmed
            .chars()
            .nth(1)
            .is_some_and(|c| c.is_ascii_lowercase())
    {
        return false;
    }
    looks_like_type_arg_expr(trimmed)
        && (trimmed.contains(':') || trimmed.contains('(') || trimmed.contains("::"))
}

/// Substitute type parameters in a method definition.
/// E.g., if type_subs = [("T", "Str:D(Numeric)")], then any param with
/// type_constraint "T" becomes "Str:D(Numeric)".
pub(super) fn substitute_type_params_in_method(
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
        is_submethod: method.is_submethod,
    }
}

/// Context threaded through the `$!attr` declaration validators so that an
/// undeclared private attribute can be reported as a fully-populated
/// `X::Attribute::Undeclared` (with `package-name`/`package-kind`).
pub(crate) struct AttrValidationCtx<'a> {
    pub(crate) attrs: &'a HashSet<String>,
    pub(crate) pkg_name: &'a str,
    pub(crate) pkg_kind: &'a str,
}

/// Extract the language revision letter from a version string like "6.c", "6.d", "6.e".
pub(super) fn language_revision_letter(version: &str) -> String {
    // Version is like "6.c", "6.d", "6.e" — extract the letter after the dot
    if let Some(letter) = version.strip_prefix("6.") {
        letter.chars().next().unwrap_or('c').to_string()
    } else {
        "c".to_string()
    }
}

impl Interpreter {
    /// Apply `handles` specifications to a class definition.
    /// For type-based handles, collects method names from the referenced type
    /// first, then applies them without holding borrows on self.
    pub(crate) fn apply_handle_specs(
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
    pub(crate) fn apply_handle_specs_to_role(
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
    pub(crate) fn resolve_handle_specs_to_names(
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
        if let Some(class_def) = self.registry().classes.get(type_name) {
            names.extend(class_def.methods.keys().cloned());
        } else if let Some(role_def) = self.registry().roles.get(type_name) {
            names.extend(role_def.methods.keys().cloned());
            // Also include methods from composed roles
            if let Some(composed) = self.registry().class_composed_roles.get(type_name) {
                for role_name in composed {
                    if let Some(rd) = self.registry().roles.get(role_name) {
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

    pub(crate) fn resolve_declared_type_name(&self, name: &str) -> String {
        let (base, suffix) = if let Some(bracket) = name.find('[') {
            (&name[..bracket], &name[bracket..])
        } else {
            (name, "")
        };
        let lookup = base.strip_prefix("::").unwrap_or(base);
        // Well-known builtin parent types should not be resolved to a
        // package-scoped variant (e.g. "Grammar" → "HTTP::Parser::Grammar").
        if !lookup.contains("::")
            && matches!(
                lookup,
                "Any"
                    | "Cool"
                    | "Mu"
                    | "Grammar"
                    | "Match"
                    | "Int"
                    | "Str"
                    | "Num"
                    | "Rat"
                    | "Bool"
                    | "IO"
                    | "Exception"
                    | "Stash"
            )
        {
            return format!("{}{}", lookup, suffix);
        }
        if let Value::Package(pkg) = self.resolve_indirect_type_name(lookup) {
            return format!("{}{}", pkg.resolve(), suffix);
        }
        if let Some(Value::Package(pkg)) = self.env.get(lookup) {
            return format!("{}{}", pkg.resolve(), suffix);
        }
        // Fallback: when a compile-time pre-qualified name like `M::C1` cannot
        // be resolved (e.g. because `C1` lives outside module `M`), try the
        // bare suffix (`C1`).  This handles cross-package parents in classes
        // declared inside a `unit module`/`unit class` body.
        if lookup.contains("::") {
            let bare = lookup.rsplit_once("::").map(|(_, b)| b).unwrap_or(lookup);
            // Single guard for all four lookups (avoids stacking read guards).
            let needs_bare = {
                let registry = self.registry();
                !registry.classes.contains_key(lookup)
                    && !registry.roles.contains_key(lookup)
                    && (registry.classes.contains_key(bare) || registry.roles.contains_key(bare))
            };
            if needs_bare {
                return format!("{}{}", bare, suffix);
            }
            if let Value::Package(pkg) = self.resolve_indirect_type_name(bare) {
                let resolved = pkg.resolve();
                if self.registry().classes.contains_key(resolved.as_str())
                    || self.registry().roles.contains_key(resolved.as_str())
                {
                    return format!("{}{}", resolved, suffix);
                }
            }
        }
        name.to_string()
    }
}
