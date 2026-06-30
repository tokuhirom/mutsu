use super::registration_class::{
    AttrValidationCtx, is_non_composable_builtin, language_revision_letter, make_delegation_method,
    parse_role_type_args, substitute_type_params_in_method, type_value_name,
};
use super::*;
use crate::ast::{HandleSpec, ParamDef, PhaserKind};
use crate::symbol::Symbol;

impl Interpreter {
    /// Recursively surface `has`-attribute declarations nested inside a sub/block
    /// within a class body (`class C { sub f { has $.x } }`). Descends into
    /// `sub`/block bodies but NOT into a nested `class`/`role` (which owns its own
    /// attribute scope). Collects only `HasDecl` statements.
    fn collect_nested_class_has_decls<'a>(stmts: &'a [Stmt], out: &mut Vec<&'a Stmt>) {
        for s in stmts {
            match s {
                // Own attribute scope / already collected at the top level.
                Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::HasDecl { .. } => {}
                Stmt::SubDecl { body, .. } => {
                    for inner in body {
                        if matches!(inner, Stmt::HasDecl { .. }) {
                            out.push(inner);
                        }
                    }
                    Self::collect_nested_class_has_decls(body, out);
                }
                _ => {}
            }
        }
    }

    pub(crate) fn register_class_decl(
        &mut self,
        name: &str,
        parents: &[String],
        modifiers: ClassDeclModifiers<'_>,
        body: &[Stmt],
    ) -> Result<Vec<String>, RuntimeError> {
        self.clear_private_zeroarg_method_cache();
        // Mark this as a user-declared class so its collected attribute list is
        // authoritative for accessor resolution (undeclared `.name` -> NotFound).
        self.user_declared_classes.insert(name.to_string());
        let ClassDeclModifiers {
            class_is_rw,
            is_hidden,
            is_lexical,
            hidden_parents,
            does_parents,
            language_version: class_language_version,
        } = modifiers;
        let class_lang_rev = language_revision_letter(class_language_version);
        // Normalize parent names: strip leading `::` (indirect name lookup syntax).
        // `is ::Foo` means the same as `is Foo` in Raku.
        let strip_colons = |s: &str| s.strip_prefix("::").unwrap_or(s).to_string();
        let parents: Vec<String> = parents.iter().map(|p| strip_colons(p)).collect();
        let parents = parents.as_slice();
        let does_parents: Vec<String> = does_parents.iter().map(|p| strip_colons(p)).collect();
        let does_parents = does_parents.as_slice();
        let hidden_parents: Vec<String> = hidden_parents.iter().map(|p| strip_colons(p)).collect();
        let hidden_parents = hidden_parents.as_slice();
        // Snapshot the previous registry state for this class under a single read
        // guard (all values are owned/cloned, no re-entry) so a redefinition can be
        // rolled back if the new body fails.
        let (
            prev_class,
            prev_hidden,
            prev_hidden_defer,
            prev_composed_roles,
            prev_role_param_bindings,
        ) = {
            let reg = self.registry();
            (
                reg.classes.get(name).cloned(),
                reg.hidden_classes.contains(name),
                reg.hidden_defer_parents.get(name).cloned(),
                reg.class_composed_roles.get(name).cloned(),
                reg.class_role_param_bindings.get(name).cloned(),
            )
        };
        // Clear `is Type` trait entries for this class (they'll be re-populated from the body).
        self.registry_mut()
            .class_attribute_is_types
            .retain(|(cn, _), _| cn != name);

        // Rollback writes are purely registry mutations with no user-code
        // re-entry, so they take a single write guard for the whole block.
        let restore_previous_state = |this: &mut Self| {
            let mut reg = this.registry_mut();
            if let Some(class_def) = prev_class.clone() {
                reg.classes.insert(name.to_string(), class_def);
            } else {
                reg.classes.remove(name);
            }
            if prev_hidden {
                reg.hidden_classes.insert(name.to_string());
            } else {
                reg.hidden_classes.remove(name);
            }
            if let Some(hidden) = prev_hidden_defer.clone() {
                reg.hidden_defer_parents.insert(name.to_string(), hidden);
            } else {
                reg.hidden_defer_parents.remove(name);
            }
            if let Some(composed) = prev_composed_roles.clone() {
                reg.class_composed_roles.insert(name.to_string(), composed);
            } else {
                reg.class_composed_roles.remove(name);
            }
            if let Some(bindings) = prev_role_param_bindings.clone() {
                reg.class_role_param_bindings
                    .insert(name.to_string(), bindings);
            } else {
                reg.class_role_param_bindings.remove(name);
            }
        };

        // Detect X::Redeclaration when a class redefines a role in the same scope.
        // Only check user-declared roles (not pre-registered builtins like Iterator).
        // Lexical classes (`my class`) are allowed to shadow outer role names.
        if !is_lexical && self.registry().user_declared_roles.contains(name) {
            // Only report redeclaration for non-stub class bodies
            let body_non_stub: Vec<_> = body
                .iter()
                .filter(|s| !matches!(s, Stmt::SetLine(_)))
                .collect();
            let class_is_stub = body_non_stub.len() == 1
                && matches!(body_non_stub[0], Stmt::Expr(Expr::Call { name: fn_name, .. })
                    if *fn_name == "__mutsu_stub_die" || *fn_name == "__mutsu_stub_warn");
            if !class_is_stub {
                return Err(RuntimeError::redeclaration("symbol", name));
            }
        }

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
        if is_stub_body
            && self.registry().classes.contains_key(name)
            && !self.registry().class_stubs.contains(name)
        {
            return Ok(Vec::new());
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
            "SetHash",
            "BagHash",
            "MixHash",
            "Range",
            "Pair",
            "IO",
            "IO::Path",
            "IO::Handle",
            "IO::Spec",
            "IO::Spec::Unix",
            "IO::Spec::Win32",
            "IO::Spec::Cygwin",
            "IO::Spec::QNX",
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
            "Capture",
            "Grammar",
            "Parameter",
            "Proxy",
            "Signature",
            "Stash",
            "Metamodel::ClassHOW",
            "Perl6::Metamodel::ClassHOW",
        ];
        let mut deferred_custom_traits: Vec<String> = Vec::new();
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
            if !self.registry().classes.contains_key(base_parent)
                && !BUILTIN_TYPES.contains(&base_parent)
                && !self.registry().roles.contains_key(base_parent)
                && !self.registry().enum_types.contains_key(base_parent)
            {
                // Use X::InvalidType for `does`/`hides` parents,
                // X::Inheritance::UnknownParent for `is` parents.
                if does_parents.contains(parent) || hidden_parents.contains(parent) {
                    return Err(RuntimeError::new(format!(
                        "X::InvalidType: Invalid typename '{}'",
                        resolved_parent_name
                    )));
                }
                // If trait_mod:<is> is defined, defer unknown lowercase parents
                // to custom trait dispatch instead of erroring.
                if (self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>"))
                    && resolved_parent
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_ascii_lowercase())
                {
                    deferred_custom_traits.push(resolved_parent_name.to_string());
                    continue;
                }
                // A name that is declared as a `package` (or module) exists but
                // does not support inheritance: `package A {}; class B is A {}`
                // is X::Inheritance::Unsupported, not an unknown-parent error.
                if self.chain_declared_packages.contains(base_parent)
                    || self
                        .chain_declared_packages
                        .contains(resolved_parent_name.as_str())
                {
                    let msg = format!(
                        "{} does not support inheritance, so {} cannot inherit from it",
                        resolved_parent_name, name
                    );
                    let mut attrs = HashMap::new();
                    attrs.insert("child-typename".to_string(), Value::str(name.to_string()));
                    attrs.insert(
                        "parent".to_string(),
                        Value::Package(crate::symbol::Symbol::intern(
                            resolved_parent_name.as_str(),
                        )),
                    );
                    attrs.insert("message".to_string(), Value::str(msg));
                    return Err(RuntimeError::typed("X::Inheritance::Unsupported", attrs));
                }
                {
                    // Suggest close known type names (Did-you-mean).
                    let suggestions = self.suggest_type_names(resolved_parent_name.as_str());
                    let mut msg = format!(
                        "'{}' cannot inherit from '{}' because it is unknown.",
                        name, resolved_parent_name
                    );
                    if suggestions.len() == 1 {
                        msg.push_str(&format!("\nDid you mean '{}'?", suggestions[0]));
                    } else if suggestions.len() > 1 {
                        msg.push_str("\nDid you mean one of these?\n");
                        for s in &suggestions {
                            msg.push_str(&format!("    '{}'\n", s));
                        }
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert("child-name".to_string(), Value::str(name.to_string()));
                    attrs.insert("child".to_string(), Value::str(name.to_string()));
                    attrs.insert(
                        "parent-name".to_string(),
                        Value::str(resolved_parent_name.to_string()),
                    );
                    attrs.insert(
                        "parent".to_string(),
                        Value::str(resolved_parent_name.to_string()),
                    );
                    attrs.insert(
                        "suggestions".to_string(),
                        Value::array(suggestions.into_iter().map(Value::str).collect()),
                    );
                    attrs.insert("message".to_string(), Value::str(msg));
                    return Err(RuntimeError::typed("X::Inheritance::UnknownParent", attrs));
                }
            }
            // A `does` target that is a non-composable built-in concrete class
            // (Int, Str, Num, Cool, Any, Mu, ...) — as opposed to a composable
            // built-in role (Real, Numeric, Positional, Iterable, ...) — raises
            // X::Composition::NotComposable.
            if does_parents.contains(parent)
                && !self.registry().roles.contains_key(resolved_parent)
                && BUILTIN_TYPES.contains(&resolved_parent)
                && is_non_composable_builtin(resolved_parent)
            {
                let msg = format!(
                    "{} is not composable, so {} cannot compose it",
                    resolved_parent, name
                );
                let mut attrs = HashMap::new();
                attrs.insert("target-name".to_string(), Value::str(name.to_string()));
                attrs.insert(
                    "composer".to_string(),
                    Value::Package(crate::symbol::Symbol::intern(resolved_parent)),
                );
                attrs.insert("message".to_string(), Value::str(msg.clone()));
                let ex = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Composition::NotComposable"),
                    attrs,
                );
                let mut err = RuntimeError::new(&msg);
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
            // Check that `does` targets are actually roles, not classes
            if does_parents.contains(parent)
                && self.registry().classes.contains_key(resolved_parent)
                && !self.registry().roles.contains_key(resolved_parent)
                && !BUILTIN_TYPES.contains(&resolved_parent)
            {
                return Err(RuntimeError::new(format!(
                    "'{}' cannot compose '{}' because it is not a role",
                    name, resolved_parent
                )));
            }
            // Check if parent is a stub (not yet composed)
            if self.registry().class_stubs.contains(resolved_parent) {
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
            attribute_built: HashMap::new(),
            methods: HashMap::new(),
            native_methods: HashSet::new(),
            mro: Vec::new(),
            wildcard_handles: Vec::new(),
            alias_attributes: HashSet::new(),
            class_level_attrs: HashMap::new(),
        };
        if is_hidden {
            self.registry_mut().hidden_classes.insert(name.to_string());
        } else {
            self.registry_mut().hidden_classes.remove(name);
        }
        if hidden_parents.is_empty() {
            self.registry_mut().hidden_defer_parents.remove(name);
        } else {
            self.registry_mut()
                .hidden_defer_parents
                .insert(name.to_string(), hidden_parents.iter().cloned().collect());
        }
        // Compose roles listed in the parents (from "does Role" or "is Role" in class header)
        let mut composed_roles_list = Vec::new();
        // The DIRECTLY-declared role parents (one per class-header `does`/`is`
        // role), captured before any transitive sub-role concretizations are
        // flattened into `composed_roles_list`. Used for qualified-call
        // concretization resolution (see `class_direct_composed_roles`).
        let mut direct_composed_roles: Vec<String> = Vec::new();
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
                direct_composed_roles.push(resolved_parent_name.clone());
                // Look up the role's language revision for submethod composition rules.
                let role_lang_rev = self
                    .type_metadata
                    .get(base_role_name)
                    .and_then(|m| m.get("language-revision"))
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "c".to_string());
                // Submethods from roles are only composed when the class is 6.c
                // AND the role is also 6.c. In 6.d+, submethods are never composed
                // from roles.
                let compose_submethods = class_lang_rev == "c" && role_lang_rev == "c";
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
                // Carry each composed-role attribute's deferred `is default(...)`
                // expression onto the consuming class so it can be evaluated at
                // construction with this class's type-param bindings in scope.
                let role_default_exprs: Vec<(String, crate::ast::Expr)> = self
                    .registry()
                    .role_attribute_default_exprs
                    .iter()
                    .filter(|((r, _), _)| r == base_role_name)
                    .map(|((_, attr), expr)| (attr.clone(), expr.clone()))
                    .collect();
                for (attr, expr) in role_default_exprs {
                    self.registry_mut()
                        .class_attribute_default_exprs
                        .entry((name.to_string(), attr))
                        .or_insert(expr);
                }
                for (mname, overloads) in &role.methods {
                    // Skip methods declared with `my` scope -- they are role-private
                    // and should not be composed into consuming classes.
                    // Submethods (is_submethod=true) ARE composed only when both
                    // the class and role share 6.c language revision.
                    let non_my_overloads: Vec<&MethodDef> = overloads
                        .iter()
                        .filter(|md| !md.is_my || (md.is_submethod && compose_submethods))
                        .collect();
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
                    // Run a nested TYPE declaration (`my class CR2`) in the role
                    // body with the ROLE as the current package so it is named
                    // `R2::CR2`, not the composing class. Only type declarations get
                    // the role package — a lexical `sub`/`my $x` keeps the outer
                    // package so a bare reference from a role method still resolves.
                    let saved_body_pkg = self.current_package().to_string();
                    for stmt in &role.deferred_body_stmts {
                        let is_type_decl =
                            matches!(stmt, Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. });
                        if is_type_decl {
                            self.set_current_package(base_role_name.to_string());
                        }
                        let r = self.run_block_raw(std::slice::from_ref(stmt));
                        if is_type_decl {
                            self.set_current_package(saved_body_pkg.clone());
                        }
                        r?;
                    }
                    // Remove type capture markers (but keep the variables
                    // created by the deferred stmts for method closures)
                    for param_name in role_param_values.keys() {
                        self.env.remove(&format!("__type_capture__{}", param_name));
                        // Don't remove the param name itself - methods may need it
                    }
                }
                if let Some(parent_specs) =
                    self.registry().role_parents.get(base_role_name).cloned()
                {
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
                        if let Some(parent_role) = self.registry().roles.get(parent_base).cloned() {
                            if !composed_roles_list.contains(&resolved_parent) {
                                composed_roles_list.push(resolved_parent.clone());
                            }
                            let parent_type_subs: Vec<(String, String)> = if let Some(parent_tps) =
                                self.registry().role_type_params.get(parent_base)
                            {
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
                                // If the composing role (base_role_name, e.g. R2) defines
                                // this method itself, it has already resolved the same-named
                                // method it inherits from its parent role (parent_base, e.g.
                                // R1). Do not re-propagate the parent's copy into the consumer
                                // as an independent candidate -- doing so would create a spurious
                                // X::Role::Composition::Conflict. The parent role's method is
                                // still reachable via a qualified call (self.R1::method).
                                let resolved_by_composing_role =
                                    role.methods.get(mname).is_some_and(|defs| {
                                        defs.iter().any(|d| d.role_origin.is_none() && !d.is_my)
                                    });
                                if resolved_by_composing_role {
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
                        } else if self.registry().classes.contains_key(parent_base)
                            && !class_def.parents.iter().any(|p| p == &resolved_parent)
                        {
                            class_def.parents.push(resolved_parent.clone());
                        }
                    }
                }
            } else if does_parents.contains(parent)
                && self.registry().enum_types.contains_key(base_role_name)
            {
                // Enum used as a role via `does`: record it for method dispatch
                self.registry_mut()
                    .class_enum_roles
                    .entry(name.to_string())
                    .or_default()
                    .push(base_role_name.to_string());
            } else if does_parents.contains(parent)
                && BUILTIN_TYPES.contains(&base_role_name)
                && !self.registry().roles.contains_key(base_role_name)
                && !composed_roles_list.contains(&resolved_parent_name)
            {
                // Built-in type used as a role via `does` (e.g., `does Numeric`,
                // `does Real`): record in composed_roles_list so that role-based
                // method dispatch (e.g., .Numeric on type objects) works correctly.
                composed_roles_list.push(resolved_parent_name.clone());
            }
        }
        if class_role_param_bindings.is_empty() {
            self.registry_mut().class_role_param_bindings.remove(name);
        } else {
            self.registry_mut()
                .class_role_param_bindings
                .insert(name.to_string(), class_role_param_bindings);
        }
        // Handle role punning: `is Role` creates a punned class from the role
        for punned_role in &punned_roles {
            let base_role = punned_role
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(punned_role.as_str());
            // Create a punned class entry if one doesn't already exist
            if !self.registry().classes.contains_key(punned_role.as_str())
                && !self.registry().classes.contains_key(base_role)
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
                    if let Some(rparents) = self.registry().role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.registry().roles.contains_key(rp_base) {
                                // It's a role - add as composed role and recurse
                                if !punned_composed_roles.contains(&rp) {
                                    punned_composed_roles.push(rp.clone());
                                }
                                role_stack.push(rp_base.to_string());
                            } else if self.registry().classes.contains_key(rp_base)
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
                    attribute_built: HashMap::new(),
                    methods: HashMap::new(),
                    native_methods: HashSet::new(),
                    mro: Vec::new(),
                    wildcard_handles: Vec::new(),
                    alias_attributes: HashSet::new(),
                    class_level_attrs: HashMap::new(),
                };
                self.registry_mut()
                    .classes
                    .insert(base_role.to_string(), punned_class);
                if !punned_composed_roles.is_empty() {
                    self.registry_mut()
                        .class_composed_roles
                        .insert(base_role.to_string(), punned_composed_roles);
                }
                // Propagate hidden status from role to punned class
                // Read the flag under the guard, drop it, then write (read->write on
                // the same lock would deadlock).
                let base_role_is_hidden = self
                    .registry()
                    .roles
                    .get(base_role)
                    .is_some_and(|role_def| role_def.is_hidden);
                if base_role_is_hidden {
                    self.registry_mut()
                        .hidden_classes
                        .insert(base_role.to_string());
                }
                if hidden_punned_role_bases.contains(base_role) {
                    self.registry_mut()
                        .hidden_classes
                        .insert(base_role.to_string());
                }
                // Recompute MRO for the punned class
                let mro = self.class_mro(base_role);
                if let Some(cd) = self.registry_mut().classes.get_mut(base_role) {
                    cd.mro = mro;
                }
            }
            if hidden_punned_role_bases.contains(base_role) {
                self.registry_mut()
                    .hidden_classes
                    .insert(base_role.to_string());
            }
        }
        // Clear stale composed roles from previous registration
        self.registry_mut().class_composed_roles.remove(name);
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
                    if let Some(rparents) = self.registry().role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.registry().roles.contains_key(rp_base) {
                                // It's a sub-role, recurse
                                role_stack.push(rp_base.to_string());
                            } else if self.registry().classes.contains_key(rp_base)
                                && !class_def.parents.contains(&rp)
                            {
                                class_def.parents.push(rp);
                            }
                        }
                    }
                }
            }
            self.registry_mut()
                .class_composed_roles
                .insert(name.to_string(), composed_roles_list.clone());
            self.registry_mut()
                .class_direct_composed_roles
                .insert(name.to_string(), direct_composed_roles.clone());
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
                    // Hoist the clone to a `let` so the read guard drops before the
                    // registry_mut write below (read->write on the same lock deadlocks).
                    let hides_list = self.registry().role_hides.get(&role_name).cloned();
                    if let Some(hides_list) = hides_list {
                        for hidden in hides_list {
                            self.registry_mut()
                                .hidden_defer_parents
                                .entry(name.to_string())
                                .or_default()
                                .insert(hidden);
                        }
                    }
                    // Recurse into sub-roles
                    if let Some(rparents) = self.registry().role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.registry().roles.contains_key(rp_base) {
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
                self.registry_mut()
                    .class_trusts
                    .entry(name.to_string())
                    .or_default()
                    .insert(trusted_class.resolve());
            }
        }
        // Make the class visible while its body executes so introspection calls
        // like `A.^add_method(...)` inside the declaration can resolve `A`.
        // Clear stale method wrap chains from a previous class with the same name.
        self.method_wrap_chains.retain(|(cls, _, _), _| cls != name);
        self.registry_mut()
            .classes
            .insert(name.to_string(), class_def.clone());
        if is_stub_body {
            self.registry_mut().class_stubs.insert(name.to_string());
            self.registry_mut()
                .classes
                .insert(name.to_string(), class_def);
            let mut stack = Vec::new();
            let _ = self.compute_class_mro(name, &mut stack)?;
            return Ok(deferred_custom_traits);
        }
        // Clear stub status now that the class has a real body (also clears
        // package stub status for `package Foo { ... }; class Foo { }`).
        {
            let mut reg = self.registry_mut();
            reg.class_stubs.remove(name);
            reg.package_stubs.remove(name);
        }
        let saved_package = self.current_package();
        let saved_env = self.env.clone();
        self.set_current_package(name.to_string());
        self.env
            .insert("?CLASS".to_string(), Value::Package(Symbol::intern(name)));
        // Flatten SyntheticBlock (from `has ($a, $b)` list form) so inner
        // HasDecl statements are processed at the top level.
        let mut flattened_body: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        // An attribute can also be declared inside a nested sub/block within the
        // class body (`class C { sub f { has $.x } }`) — Rakudo still registers it
        // on the class. Surface those `has` declarations so the main loop below
        // registers the attribute (the nested sub itself is still processed
        // normally; its body's `has` is a compile-time declaration).
        Self::collect_nested_class_has_decls(body, &mut flattened_body);
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
        let class_attr_ctx = AttrValidationCtx {
            attrs: &class_own_attrs,
            pkg_name: name,
            pkg_kind: "class",
        };
        let saved_functions_keys: HashSet<String> = self
            .registry()
            .functions
            .keys()
            .map(|k| k.resolve())
            .collect();
        // LEAVE phasers declared at class-body scope (e.g. via
        // `my $x will leave { ... }`) must fire when the class body is left,
        // i.e. once all body statements have been processed. Collect them here
        // and run them (LIFO) after the loop instead of executing them inline.
        let mut class_leave_phasers: Vec<Vec<Stmt>> = Vec::new();
        for stmt in flattened_body {
            match stmt {
                Stmt::Phaser {
                    kind: PhaserKind::Leave,
                    body,
                } => {
                    class_leave_phasers.push(body.clone());
                }
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
                    deprecated_message,
                    is_built,
                    unknown_traits,
                } => {
                    let attr_name_str = attr_name.resolve();

                    // Handle unknown traits. If a user-defined `trait_mod:<is>`
                    // (or `trait_mod:<will>`, etc.) can handle the trait, dispatch
                    // to it with an Attribute introspection object; otherwise raise
                    // X::Comp::Trait::Unknown. Kept in a separate method so its
                    // locals don't inflate this already-large function's frame.
                    if !unknown_traits.is_empty()
                        && let Err(err) = self.apply_attribute_traits(
                            unknown_traits,
                            &attr_name_str,
                            *sigil,
                            *is_public,
                            name,
                            type_constraint.as_deref(),
                        )
                    {
                        self.set_current_package(saved_package);
                        self.env = saved_env;
                        return Err(err);
                    }

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
                        self.set_current_package(saved_package);
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
                            // Type-check the default value against the attribute's type
                            // constraint. For an object hash (`%.a{KeyType}`) the
                            // constraint is `ValueType{KeyType}`; the `is default`
                            // value is an *element* default, so check it against the
                            // value type only.
                            if let Some(tc) = type_constraint {
                                let tc = tc
                                    .split_once('{')
                                    .map(|(value_tc, _)| value_tc)
                                    .unwrap_or(tc.as_str());
                                let type_ok = if matches!(val, Value::Nil) {
                                    // Nil is only valid for untyped or Nil-accepting attributes
                                    tc == "Any" || tc == "Mu" || tc.contains("Nil")
                                } else {
                                    self.type_matches_value(tc, &val)
                                };
                                if !type_ok {
                                    let mut attrs = std::collections::HashMap::new();
                                    attrs.insert(
                                        "message".to_string(),
                                        Value::str(format!(
                                            "Type check failed in assignment to attribute; expected {} but got {}",
                                            tc, super::utils::value_type_name(&val)
                                        )),
                                    );
                                    attrs.insert(
                                        "expected".to_string(),
                                        Value::Package(crate::symbol::Symbol::intern(tc)),
                                    );
                                    attrs.insert(
                                        "got".to_string(),
                                        if matches!(val, Value::Nil) {
                                            Value::Nil
                                        } else {
                                            val.clone()
                                        },
                                    );
                                    let err = Value::make_instance(
                                        crate::symbol::Symbol::intern(
                                            "X::TypeCheck::Attribute::Default",
                                        ),
                                        attrs,
                                    );
                                    let mut runtime_err = RuntimeError::new(format!(
                                        "X::TypeCheck::Attribute::Default: Type check failed for default value of attribute '{}'; expected {}, got {}",
                                        attr_name_str,
                                        tc,
                                        super::utils::value_type_name(&val)
                                    ));
                                    runtime_err.exception = Some(Box::new(err));
                                    self.set_current_package(saved_package);
                                    self.env = saved_env;
                                    return Err(runtime_err);
                                }
                            }
                            self.registry_mut()
                                .class_attribute_defaults
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
                    if let Some(built) = is_built {
                        class_def
                            .attribute_built
                            .insert(attr_name_str.clone(), *built);
                    }
                    if let Some(it) = is_type {
                        self.registry_mut()
                            .class_attribute_is_types
                            .insert((name.to_string(), attr_name_str.clone()), it.clone());
                    }
                    if let Some(dm) = deprecated_message {
                        self.registry_mut()
                            .class_attribute_deprecated
                            .insert((name.to_string(), attr_name_str.clone()), dm.clone());
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
                    is_submethod,
                    our_variable_form,
                    return_type,
                    is_default_candidate,
                    deprecated_message,
                    handles: method_handles,
                    custom_traits: method_custom_traits,
                    is_export: method_is_export,
                    export_tags: method_export_tags,
                } => {
                    self.validate_private_access_in_stmts(name, method_body)?;
                    Self::validate_attr_declared_in_class(&class_attr_ctx, method_body)?;
                    // In BUILD/TWEAK submethods, :$!attr parameters must refer
                    // to declared attributes; reject undeclared ones with
                    // X::Attribute::Undeclared.
                    {
                        let mn = method_name.resolve();
                        if mn == "BUILD" || mn == "TWEAK" {
                            for pd in param_defs {
                                if pd.name.starts_with('!') && pd.name != "!" {
                                    let attr_name = &pd.name[1..]; // strip '!'
                                    if !class_own_attrs.contains(attr_name) {
                                        let err = Self::undeclared_attr_error(
                                            &class_attr_ctx,
                                            attr_name,
                                            "!",
                                        );
                                        self.set_current_package(saved_package);
                                        self.env = saved_env;
                                        return Err(err);
                                    }
                                }
                            }
                        }
                    }
                    let resolved_method_name = if let Some(expr) = name_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                            .to_string_value()
                    } else {
                        method_name.resolve()
                    };
                    let mut effective_param_defs =
                        Self::effective_method_param_defs(param_defs, is_hidden);
                    // Auto-detect @_ usage in methods without explicit signatures
                    if param_defs.is_empty() {
                        let (use_positional, _) = Self::auto_signature_uses(method_body);
                        if use_positional && !effective_param_defs.iter().any(|pd| pd.name == "@_")
                        {
                            // Insert @_ slurpy before the named %_ slurpy (if any)
                            let insert_pos = effective_param_defs
                                .iter()
                                .position(|pd| pd.name.starts_with('%') && pd.slurpy)
                                .unwrap_or(effective_param_defs.len());
                            effective_param_defs.insert(
                                insert_pos,
                                ParamDef {
                                    name: "@_".to_string(),
                                    default: None,
                                    multi_invocant: true,
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
                                },
                            );
                        }
                    }
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
                        // Use is_submethod for the MethodDef is_my flag, which
                        // controls inheritance filtering (submethods not inherited).
                        // `my method` and `our method` are NOT added to the method
                        // table at all — they are only registered as functions.
                        is_my: *is_submethod,
                        role_origin: None,
                        original_role: None,
                        return_type: return_type.clone(),
                        compiled_code: None,
                        delegation: None,
                        is_default: *is_default_candidate,
                        deprecated_message: deprecated_message.clone(),
                        is_submethod: *is_submethod,
                    };
                    // `my method` and `our method` are NOT part of the class
                    // method table — they are only callable as functions.
                    // Submethods (is_submethod=true) DO go in the table even
                    // though they also have is_my=true from the parser.
                    // `my method` and `our method` are NOT part of the class
                    // method table — they are only callable as functions.
                    // Submethods (is_submethod=true) DO go in the table even
                    // though they also have is_my=true from the parser.
                    // The `our &name = method name(...)` variable form
                    // (our_variable_form=true) keeps the method in the table.
                    let is_lexical_only = *is_my && !*is_submethod;
                    let is_our_only = *is_our && !*our_variable_form;
                    if !is_lexical_only && !is_our_only {
                        if *multi {
                            class_def
                                .methods
                                .entry(resolved_method_name.clone())
                                .or_default()
                                .push(def);
                        } else {
                            // Check for duplicate non-multi method definition.
                            // Only error if the existing method was defined in
                            // this class (not composed from a role) AND shares the
                            // same privacy: a private `method !foo` and a public
                            // `method foo` live in separate namespaces and do not
                            // collide (they are stored together but dispatch filters
                            // on `is_private`).
                            let new_is_private = def.is_private;
                            if let Some(existing) = class_def.methods.get(&resolved_method_name) {
                                let conflicts = existing.iter().any(|m| {
                                    m.role_origin.is_none() && m.is_private == new_is_private
                                });
                                if conflicts {
                                    return Err(RuntimeError::new(format!(
                                        "Package '{}' already has a method '{}' (did you mean to declare a multi method?)",
                                        name, resolved_method_name
                                    )));
                                }
                            }
                            // A non-multi method replaces prior same-privacy
                            // candidates but must preserve methods of the OTHER
                            // privacy stored under the same name.
                            let entry = class_def
                                .methods
                                .entry(resolved_method_name.clone())
                                .or_default();
                            entry.retain(|m| m.is_private != new_is_private);
                            entry.push(def);
                        }
                    }
                    // A method declared `is export` is recorded as an export of
                    // the enclosing class so that `import ClassName` succeeds
                    // (and exposes the method's sub-form name). This is mainly
                    // used by operator methods such as `method infix:<as> is
                    // export`, whose sub-form is importable.
                    if *method_is_export && !self.suppress_exports {
                        let tags = if method_export_tags.is_empty() {
                            vec!["DEFAULT".to_string()]
                        } else {
                            method_export_tags.clone()
                        };
                        self.register_exported_var(
                            name.to_string(),
                            format!("&{}", resolved_method_name),
                            tags,
                        );
                    }
                    // Apply custom trait_mod:<is> for each non-builtin trait on methods
                    if !method_custom_traits.is_empty() {
                        let has_trait_mod = self.has_proto("trait_mod:<is>")
                            || self.has_multi_candidates("trait_mod:<is>");
                        if has_trait_mod {
                            for (trait_name, trait_arg) in method_custom_traits {
                                let mut trait_env = self.env.clone();
                                // Add method lookup markers so .wrap stores in
                                // method_wrap_chains (keyed by class+method).
                                trait_env.insert(
                                    "__mutsu_lookup_class".to_string(),
                                    Value::str(name.to_string()),
                                );
                                trait_env.insert(
                                    "__mutsu_lookup_method".to_string(),
                                    Value::str(resolved_method_name.clone()),
                                );
                                trait_env.insert(
                                    "__mutsu_lookup_candidate_idx".to_string(),
                                    Value::Int(0),
                                );
                                let sub_val = Value::make_sub(
                                    Symbol::intern(name),
                                    Symbol::intern(&resolved_method_name),
                                    effective_params.clone(),
                                    effective_param_defs.clone(),
                                    method_body.to_vec(),
                                    *is_rw,
                                    trait_env,
                                );
                                let trait_arg_val = if let Some(arg_expr) = trait_arg {
                                    Some(self.eval_block_value(&[crate::ast::Stmt::Expr(
                                        arg_expr.clone(),
                                    )])?)
                                } else {
                                    None
                                };
                                let type_obj = self.resolve_type_object(trait_name);
                                let mut args = vec![sub_val];
                                if let Some(type_val) = type_obj {
                                    args.push(type_val);
                                    if let Some(arg_val) = trait_arg_val {
                                        args.push(arg_val);
                                    }
                                    let _ = self.call_function("trait_mod:<is>", args);
                                } else {
                                    let named_val = if let Some(arg_val) = trait_arg_val {
                                        Value::Pair(trait_name.clone(), Box::new(arg_val))
                                    } else {
                                        Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)))
                                    };
                                    args.push(named_val);
                                    let _ = self.call_function("trait_mod:<is>", args);
                                }
                            }
                        }
                    }
                    // `handles` on a method: synthesize forwarder methods that
                    // delegate to the return value of this method. E.g.
                    //   method Str() handles 'uc' { 'x' }
                    // registers a `uc` method that calls `self.Str.uc(|@_)`.
                    if !method_handles.is_empty() {
                        // Encode "method-based delegation" by prefixing the
                        // source method name with `&`; the delegation dispatch
                        // sites recognize this prefix and invoke the named
                        // method on self to obtain the delegate.
                        let source_attr_marker = format!("&{}", resolved_method_name);
                        for spec in method_handles {
                            match spec {
                                HandleSpec::Name(target) => {
                                    class_def
                                        .methods
                                        .entry(target.clone())
                                        .or_default()
                                        .push(make_delegation_method(&source_attr_marker, target));
                                }
                                HandleSpec::Rename { exposed, target } => {
                                    class_def
                                        .methods
                                        .entry(exposed.clone())
                                        .or_default()
                                        .push(make_delegation_method(&source_attr_marker, target));
                                }
                                HandleSpec::Wildcard => {
                                    class_def.wildcard_handles.push(source_attr_marker.clone());
                                }
                                HandleSpec::Regex(pattern) => {
                                    class_def
                                        .wildcard_handles
                                        .push(format!("{}:regex:{}", source_attr_marker, pattern));
                                }
                                HandleSpec::Type(_) => {
                                    // Method-based delegation via a type name
                                    // is not yet supported; fall through.
                                }
                            }
                        }
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
                        self.registry_mut().functions.insert(
                            Symbol::intern(&qualified_name),
                            std::sync::Arc::new(func_def),
                        );
                    }
                    // `my method` registers as a lexically-scoped function
                    // (callable as `name(invocant)` inside the class body)
                    if *is_my {
                        let has_explicit_invocant = effective_param_defs
                            .iter()
                            .any(|p| p.is_invocant || p.traits.iter().any(|t| t == "invocant"));
                        let (my_params, my_param_defs) = if has_explicit_invocant {
                            (
                                effective_params.clone(),
                                effective_param_defs
                                    .iter()
                                    .filter(|p| p.name.as_str() != "self")
                                    .cloned()
                                    .collect(),
                            )
                        } else {
                            let mut my_params = vec!["self".to_string()];
                            my_params.extend(
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
                            };
                            let mut my_param_defs = vec![self_param];
                            my_param_defs.extend(
                                effective_param_defs
                                    .iter()
                                    .filter(|p| {
                                        !(p.is_invocant || p.traits.iter().any(|t| t == "invocant"))
                                    })
                                    .cloned(),
                            );
                            (my_params, my_param_defs)
                        };
                        let func_def = crate::ast::FunctionDef {
                            package: Symbol::intern(name),
                            name: Symbol::intern(&resolved_method_name),
                            params: my_params,
                            param_defs: my_param_defs,
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
                        // Register under the short name (lexical scope)
                        self.registry_mut().functions.insert(
                            Symbol::intern(&resolved_method_name),
                            std::sync::Arc::new(func_def.clone()),
                        );
                        // Also register under the qualified name for consistency
                        let qualified_name = format!("{}::{}", name, resolved_method_name);
                        self.registry_mut().functions.insert(
                            Symbol::intern(&qualified_name),
                            std::sync::Arc::new(func_def),
                        );
                        // Mark as my-scoped so it doesn't appear in the package stash
                        self.mark_my_scoped_package_item(qualified_name);
                    }
                }
                Stmt::DoesDecl { name: role_name } => {
                    let role_name_str = role_name.resolve();
                    if !self.registry().roles.contains_key(&role_name_str)
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
                    let role = self
                        .registry()
                        .roles
                        .get(&role_name_str)
                        .cloned()
                        .ok_or_else(|| {
                            RuntimeError::new(format!("Unknown role: {}", role_name_str))
                        })?;
                    if role.is_stub_role {
                        return Err(RuntimeError::typed_msg(
                            "X::Role::Parametric::NoSuchCandidate",
                            "No matching candidate found for the parametric role",
                        ));
                    }
                    // Look up the role's language revision for submethod composition rules.
                    let role_lang_rev_does = self
                        .type_metadata
                        .get(&role_name_str)
                        .and_then(|m| m.get("language-revision"))
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "c".to_string());
                    let compose_submethods_does =
                        class_lang_rev == "c" && role_lang_rev_does == "c";
                    for attr in &role.attributes {
                        if !class_def.attributes.iter().any(|(n, ..)| n == &attr.0) {
                            class_def.attributes.push(attr.clone());
                        }
                    }
                    for (mname, overloads) in role.methods {
                        let composed: Vec<MethodDef> = overloads
                            .into_iter()
                            .filter(|md| !md.is_my || (md.is_submethod && compose_submethods_does))
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
                    if let Some(rparents) =
                        self.registry().role_parents.get(&role_name_str).cloned()
                    {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.registry().classes.contains_key(rp_base)
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
                    self.registry_mut()
                        .class_trusts
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
                    self.registry_mut()
                        .classes
                        .insert(name.to_string(), class_def.clone());
                    self.run_block_raw(std::slice::from_ref(stmt))?;
                    for outer_name in saved_env.keys() {
                        let class_scoped_name = format!("{}::{}", name, outer_name);
                        if let Some(updated) = self.env.get(&class_scoped_name).cloned() {
                            self.env.insert_sym(*outer_name, updated);
                        }
                    }
                    if let Some(updated) = self.registry().classes.get(name).cloned() {
                        class_def = updated;
                    }
                }
                Stmt::ProtoDecl {
                    name: proto_name,
                    param_defs,
                    body: proto_body,
                    is_method: true,
                    ..
                } => {
                    let method_name = proto_name.resolve();
                    let effective_param_defs = Self::effective_method_param_defs(param_defs, false);
                    let effective_params: Vec<String> = effective_param_defs
                        .iter()
                        .map(|p| p.name.clone())
                        .collect();
                    let fdef = FunctionDef {
                        package: Symbol::intern(name),
                        name: *proto_name,
                        params: effective_params,
                        param_defs: effective_param_defs,
                        body: proto_body.clone(),
                        is_test_assertion: false,
                        is_rw: false,
                        is_raw: false,
                        is_method: true,
                        empty_sig: false,
                        return_type: None,
                        is_default: false,
                        deprecated_message: None,
                    };
                    self.registry_mut()
                        .proto_methods
                        .insert((name.to_string(), method_name), fdef);
                }
                _ => {
                    // BEGIN phasers and EVAL calls in class bodies may fail
                    // (e.g. `BEGIN EVAL q[has $.x]` or `EVAL q[has $.x]`).
                    // Swallow errors from these so the class still registers.
                    let is_swallowable = matches!(
                        stmt,
                        Stmt::Phaser {
                            kind: PhaserKind::Begin,
                            ..
                        }
                    ) || matches!(
                        stmt,
                        Stmt::Call { name: fn_name, .. }
                            if fn_name.resolve() == "EVAL"
                    ) || matches!(
                        stmt,
                        Stmt::Expr(Expr::Call { name: fn_name, .. })
                            if fn_name.resolve() == "EVAL"
                    );
                    self.registry_mut()
                        .classes
                        .insert(name.to_string(), class_def.clone());
                    let result = self.run_block_raw(std::slice::from_ref(stmt));
                    if let Err(e) = result {
                        if !is_swallowable {
                            return Err(e);
                        }
                    } else {
                        for outer_name in saved_env.keys() {
                            let class_scoped_name = format!("{}::{}", name, outer_name);
                            if let Some(updated) = self.env.get(&class_scoped_name).cloned() {
                                self.env.insert_sym(*outer_name, updated);
                            }
                        }
                    }
                    if let Some(updated) = self.registry().classes.get(name).cloned() {
                        class_def = updated;
                    }
                }
            }
            // Check if any new functions were registered under the class package
            // during body processing (e.g., class-scoped subs).
            // Only count functions that are actual subs (not methods, which are
            // registered via MethodDecl and stored in the class methods table).
            if let Stmt::SubDecl { name: sub_name, .. } = stmt {
                let fq = format!("{}::{}", name, sub_name);
                if self.registry().functions.contains_key(&Symbol::intern(&fq))
                    && !saved_functions_keys.contains(&fq)
                {
                    self.registry_mut()
                        .class_subs
                        .entry(name.to_string())
                        .or_default()
                        .insert(fq, Value::Bool(true));
                }
            }
            self.registry_mut()
                .classes
                .insert(name.to_string(), class_def.clone());
        }
        // Fire class-body LEAVE phasers in LIFO order now that the body scope
        // is being left. They run while the class package/env are still active
        // so their bodies can see body-scoped variables; writes to outer
        // variables persist because the class-body env is not rolled back on
        // the success path.
        for body in class_leave_phasers.iter().rev() {
            self.run_block_raw(body)?;
            for outer_name in saved_env.keys() {
                let class_scoped_name = format!("{}::{}", name, outer_name);
                if let Some(updated) = self.env.get(&class_scoped_name).cloned() {
                    self.env.insert_sym(*outer_name, updated);
                }
            }
        }
        self.set_current_package(saved_package);
        if let Err(err) = self.resolve_class_stub_requirements(name, &mut class_def) {
            restore_previous_state(self);
            return Err(err);
        }
        if let Err(err) = self.detect_unresolved_role_method_conflicts(name, &class_def) {
            restore_previous_state(self);
            return Err(err);
        }
        self.registry_mut()
            .classes
            .insert(name.to_string(), class_def);
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
        Ok(deferred_custom_traits)
    }
}
