use super::*;
use crate::symbol::Symbol;

type ResolvedRoleCandidate = (RoleDef, Vec<String>, Vec<Value>);

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
        return_type: method.return_type.clone(),
        compiled_code: method.compiled_code.clone(),
    }
}

impl Interpreter {
    fn eval_role_arg_values(&mut self, arg_exprs: &[String]) -> Result<Vec<Value>, RuntimeError> {
        let mut values = Vec::with_capacity(arg_exprs.len());
        for expr in arg_exprs {
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
        let base_role_name = if let Some(bracket) = parent.find('[') {
            &parent[..bracket]
        } else {
            parent
        };
        let Some(candidates) = self.role_candidates.get(base_role_name).cloned() else {
            if let Some(role) = self.roles.get(base_role_name).cloned() {
                return Ok(Some((role, Vec::new(), Vec::new())));
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
            return Err(RuntimeError::new("X::Role::Parametric::NoSuchCandidate"));
        }

        matches.sort_by(|a, b| b.1.cmp(&a.1).then(b.2.cmp(&a.2)));
        let selected = matches.remove(0).0;
        Ok(Some((selected.role_def, selected.type_params, arg_values)))
    }

    pub(crate) fn register_class_decl(
        &mut self,
        name: &str,
        parents: &[String],
        is_hidden: bool,
        hidden_parents: &[String],
        does_parents: &[String],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let prev_class = self.classes.get(name).cloned();
        let prev_hidden = self.hidden_classes.contains(name);
        let prev_hidden_defer = self.hidden_defer_parents.get(name).cloned();
        let prev_composed_roles = self.class_composed_roles.get(name).cloned();
        let prev_role_param_bindings = self.class_role_param_bindings.get(name).cloned();

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
            "Grammar",
            "Proxy",
        ];
        for parent in parents {
            // Strip type arguments for validation (e.g., "R[Str:D(Numeric)]" -> "R")
            let base_parent = if let Some(bracket) = parent.find('[') {
                &parent[..bracket]
            } else {
                parent.as_str()
            };
            if base_parent == name {
                return Err(RuntimeError::new(format!(
                    "X::Inheritance::SelfInherit: class '{}' cannot inherit from itself",
                    name
                )));
            }
            if !self.classes.contains_key(base_parent)
                && !BUILTIN_TYPES.contains(&base_parent)
                && !self.roles.contains_key(base_parent)
            {
                return Err(RuntimeError::new(format!(
                    "X::Inheritance::UnknownParent: class '{}' specifies unknown parent class '{}'",
                    name, parent
                )));
            }
        }
        let mut class_def = ClassDef {
            parents: parents.to_vec(),
            attributes: Vec::new(),
            attribute_types: HashMap::new(),
            methods: HashMap::new(),
            native_methods: HashSet::new(),
            mro: Vec::new(),
            wildcard_handles: Vec::new(),
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
            let base_role_name = parent
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(parent.as_str());
            if let Some((role, role_param_names, role_arg_values)) =
                self.resolve_role_candidate(parent)?
            {
                if role.is_stub_role {
                    return Err(RuntimeError::new("X::Role::Parametric::NoSuchCandidate"));
                }
                // Check if this role was specified via `is` (punning) vs `does` (composition)
                let is_punned = !does_parents.contains(parent);
                if is_punned {
                    punned_roles.push(parent.clone());
                    if role.is_hidden {
                        hidden_punned_role_bases.insert(base_role_name.to_string());
                    }
                }
                composed_roles_list.push(parent.clone());
                // Collect type parameter substitutions for method type constraints.
                let type_subs: Vec<(String, String)> = role_param_names
                    .iter()
                    .zip(role_arg_values.iter())
                    .map(|(p, v)| {
                        let value_name = match v {
                            Value::Package(name) => name.resolve(),
                            other => other
                                .to_string_value()
                                .trim_start_matches('(')
                                .trim_end_matches(')')
                                .to_string(),
                        };
                        (p.clone(), value_name)
                    })
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
                    let composed: Vec<MethodDef> = if type_subs.is_empty() {
                        overloads.clone()
                    } else {
                        overloads
                            .iter()
                            .map(|md| substitute_type_params_in_method(md, &type_subs))
                            .collect()
                    };
                    class_def
                        .methods
                        .entry(mname.clone())
                        .or_default()
                        .extend(composed);
                }
                let role_param_values: HashMap<String, Value> = role_param_names
                    .iter()
                    .cloned()
                    .zip(role_arg_values.iter().cloned())
                    .collect();
                if let Some(parent_specs) = self.role_parents.get(base_role_name).cloned() {
                    for parent_spec in parent_specs {
                        let resolved_parent = if let Some(v) = role_param_values.get(&parent_spec) {
                            match v {
                                Value::Package(name) => name.resolve(),
                                other => other
                                    .to_string_value()
                                    .trim_start_matches('(')
                                    .trim_end_matches(')')
                                    .to_string(),
                            }
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
                                let composed: Vec<MethodDef> = if parent_type_subs.is_empty() {
                                    overloads.clone()
                                } else {
                                    overloads
                                        .iter()
                                        .map(|md| {
                                            substitute_type_params_in_method(md, &parent_type_subs)
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
                    methods: HashMap::new(),
                    native_methods: HashSet::new(),
                    mro: Vec::new(),
                    wildcard_handles: Vec::new(),
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
        // Detect stub class: `class Foo { ... }` — body is a stub operator call.
        // Register the class but skip body execution.
        let is_stub = body.len() == 1
            && matches!(&body[0], Stmt::Expr(Expr::Call { name: fn_name, .. })
                if *fn_name == "__mutsu_stub_die");
        // Make the class visible while its body executes so introspection calls
        // like `A.^add_method(...)` inside the declaration can resolve `A`.
        self.classes.insert(name.to_string(), class_def.clone());
        if is_stub {
            self.classes.insert(name.to_string(), class_def);
            let mut stack = Vec::new();
            let _ = self.compute_class_mro(name, &mut stack)?;
            return Ok(());
        }
        let saved_package = self.current_package.clone();
        let saved_env = self.env.clone();
        self.current_package = name.to_string();
        for stmt in body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    handles,
                    is_rw,
                    type_constraint,
                    is_required,
                    sigil,
                    where_constraint: _,
                } => {
                    let attr_name_str = attr_name.resolve();
                    class_def.attributes.push((
                        attr_name_str.clone(),
                        *is_public,
                        default.clone(),
                        *is_rw,
                        is_required.clone(),
                        *sigil,
                    ));
                    if let Some(tc) = type_constraint {
                        class_def
                            .attribute_types
                            .insert(attr_name_str.clone(), tc.clone());
                    }
                    let attr_var_name = if *is_public {
                        format!(".{}", attr_name_str)
                    } else {
                        format!("!{}", attr_name_str)
                    };
                    for handle_name in handles {
                        if handle_name == "*" {
                            // Wildcard delegation: forward unknown methods to this attribute
                            class_def.wildcard_handles.push(attr_var_name.clone());
                        } else {
                            class_def
                                .methods
                                .entry(handle_name.clone())
                                .or_default()
                                .push(MethodDef {
                                    params: Vec::new(),
                                    param_defs: Vec::new(),
                                    body: vec![Stmt::Expr(Expr::MethodCall {
                                        target: Box::new(Expr::Var(attr_var_name.clone())),
                                        name: Symbol::intern(handle_name),
                                        args: Vec::new(),
                                        modifier: None,
                                        quoted: false,
                                    })],
                                    is_rw: false,
                                    is_private: false,
                                    return_type: None,
                                    compiled_code: None,
                                });
                        }
                    }
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
                    return_type,
                } => {
                    self.validate_private_access_in_stmts(name, method_body)?;
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
                        body: method_body.clone(),
                        is_rw: *is_rw,
                        is_private: *is_private,
                        return_type: return_type.clone(),
                        compiled_code: None,
                    };
                    if *multi {
                        class_def
                            .methods
                            .entry(resolved_method_name.clone())
                            .or_default()
                            .push(def);
                    } else {
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
                            "Real" | "Numeric" | "Cool" | "Any" | "Mu"
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
                        return Err(RuntimeError::new("X::Role::Parametric::NoSuchCandidate"));
                    }
                    for attr in &role.attributes {
                        if !class_def.attributes.iter().any(|(n, ..)| n == &attr.0) {
                            class_def.attributes.push(attr.clone());
                        }
                    }
                    for (mname, overloads) in role.methods {
                        class_def
                            .methods
                            .entry(mname)
                            .or_default()
                            .extend(overloads);
                    }
                    if !class_def.parents.iter().any(|p| p == &role_name_str) {
                        // Keep role composition visible in MRO introspection.
                        class_def.parents.insert(0, role_name_str);
                        class_def.mro.clear();
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
        self.classes.insert(name.to_string(), class_def);
        let mut stack = Vec::new();
        if let Err(err) = self.compute_class_mro(name, &mut stack) {
            restore_previous_state(self);
            return Err(err);
        }
        Ok(())
    }

    /// Augment an existing class by adding methods (and attributes) from the body.
    /// This implements `augment class ClassName { ... }` (monkey-patching).
    pub(crate) fn augment_class(&mut self, name: &str, body: &[Stmt]) -> Result<(), RuntimeError> {
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
        for stmt in body {
            match stmt {
                Stmt::MethodDecl {
                    name: method_name,
                    name_expr,
                    param_defs,
                    body: method_body,
                    multi,
                    is_rw,
                    is_private,
                    return_type,
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
                        body: method_body.clone(),
                        is_rw: *is_rw,
                        is_private: *is_private,
                        return_type: return_type.clone(),
                        compiled_code: None,
                    };
                    if let Some(class_def) = self.classes.get_mut(name) {
                        if *multi {
                            class_def
                                .methods
                                .entry(resolved_method_name)
                                .or_default()
                                .push(def);
                        } else {
                            class_def.methods.insert(resolved_method_name, vec![def]);
                        }
                    }
                }
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    is_rw,
                    type_constraint,
                    is_required,
                    sigil,
                    handles,
                    where_constraint: _,
                } => {
                    let attr_name_str = attr_name.resolve();
                    if let Some(class_def) = self.classes.get_mut(name) {
                        class_def.attributes.push((
                            attr_name_str.clone(),
                            *is_public,
                            default.clone(),
                            *is_rw,
                            is_required.clone(),
                            *sigil,
                        ));
                        if let Some(tc) = type_constraint {
                            class_def
                                .attribute_types
                                .insert(attr_name_str.clone(), tc.clone());
                        }
                        let attr_var_name = if *is_public {
                            format!(".{}", attr_name_str)
                        } else {
                            format!("!{}", attr_name_str)
                        };
                        for handle_name in handles {
                            if handle_name == "*" {
                                class_def.wildcard_handles.push(attr_var_name.clone());
                            } else {
                                class_def
                                    .methods
                                    .entry(handle_name.clone())
                                    .or_default()
                                    .push(MethodDef {
                                        params: Vec::new(),
                                        param_defs: Vec::new(),
                                        body: vec![Stmt::Expr(Expr::MethodCall {
                                            target: Box::new(Expr::Var(attr_var_name.clone())),
                                            name: Symbol::intern(handle_name),
                                            args: Vec::new(),
                                            modifier: None,
                                            quoted: false,
                                        })],
                                        is_rw: false,
                                        is_private: false,
                                        return_type: None,
                                        compiled_code: None,
                                    });
                            }
                        }
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
        // Clean up stale punned class entry for this role name.
        self.classes.remove(name);
        self.hidden_classes.remove(name);
        self.class_composed_roles.remove(name);
        self.role_parents.remove(name);
        self.role_hides.remove(name);
        let mut role_def = RoleDef {
            attributes: Vec::new(),
            methods: HashMap::new(),
            is_stub_role: false,
            is_hidden: false,
        };
        for stmt in body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    handles,
                    is_rw,
                    type_constraint: _,
                    is_required,
                    sigil,
                    where_constraint: _,
                } => {
                    let attr_name_str = attr_name.resolve();
                    role_def.attributes.push((
                        attr_name_str.clone(),
                        *is_public,
                        default.clone(),
                        *is_rw,
                        is_required.clone(),
                        *sigil,
                    ));
                    let attr_var_name = if *is_public {
                        format!(".{}", attr_name_str)
                    } else {
                        format!("!{}", attr_name_str)
                    };
                    for handle_name in handles {
                        role_def
                            .methods
                            .entry(handle_name.clone())
                            .or_default()
                            .push(MethodDef {
                                params: Vec::new(),
                                param_defs: Vec::new(),
                                body: vec![Stmt::Expr(Expr::MethodCall {
                                    target: Box::new(Expr::Var(attr_var_name.clone())),
                                    name: Symbol::intern(handle_name),
                                    args: Vec::new(),
                                    modifier: None,
                                    quoted: false,
                                })],
                                is_rw: false,
                                is_private: false,
                                return_type: None,
                                compiled_code: None,
                            });
                    }
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
                            && matches!(base_role_name, "Real" | "Numeric" | "Cool" | "Any" | "Mu"))
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
                        return Err(RuntimeError::new("X::Role::Parametric::NoSuchCandidate"));
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
                        if !role_def.attributes.iter().any(|(n, ..)| n == &attr.0) {
                            role_def.attributes.push(attr.clone());
                        }
                    }
                    for (mname, overloads) in role.methods {
                        let composed: Vec<MethodDef> = if type_subs.is_empty() {
                            overloads
                        } else {
                            overloads
                                .iter()
                                .map(|md| substitute_type_params_in_method(md, &type_subs))
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
                    return_type,
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
                        return Err(RuntimeError::new("X::Role::Unimplemented::Multi"));
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
                        body: method_body.clone(),
                        is_rw: *is_rw,
                        is_private: *is_private,
                        return_type: return_type.clone(),
                        compiled_code: None,
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
                _ => {
                    self.run_block_raw(std::slice::from_ref(stmt))?;
                }
            }
        }
        self.role_candidates
            .entry(name.to_string())
            .or_default()
            .push(RoleCandidateDef {
                type_params: type_params.to_vec(),
                type_param_defs: type_param_defs.to_vec(),
                role_def: role_def.clone(),
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
        Ok(())
    }

    pub(crate) fn register_subset_decl(
        &mut self,
        name: &str,
        base: &str,
        predicate: Option<&Expr>,
    ) {
        self.subsets.insert(
            name.to_string(),
            SubsetDef {
                base: base.to_string(),
                predicate: predicate.cloned(),
            },
        );
        self.env
            .insert(name.to_string(), Value::Package(Symbol::intern(name)));
    }

    pub(crate) fn register_cunion_class(&mut self, name: &str) {
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

        for (attr_name, _is_public, _default, _is_rw, _, _) in &class_attrs {
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
        for (attr_name, _is_public, default, _is_rw, _, _) in &class_attrs {
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
    fn get_attr_type_constraint(&self, class_name: &str, attr_name: &str) -> Option<String> {
        if let Some(class_def) = self.classes.get(class_name) {
            for (name, _is_public, _default, _is_rw, _, _) in &class_def.attributes {
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
}
