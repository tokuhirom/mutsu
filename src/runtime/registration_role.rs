//! `register_role_decl` — the AST registration walker for `role`
//! declarations — and parametric-role candidate resolution. ADR-0019 D0
//! split the former single ~920-line walker into named pass functions with
//! explicit inputs (see `registration_role_decl.rs`,
//! `registration_role_body.rs`, and `registration_role_method.rs`); this
//! file keeps the orchestrating entry point and candidate resolution.

use super::registration_class::{
    ResolvedRoleCandidate, builtin_role_def, looks_like_type_arg_expr, parse_role_type_args,
    should_treat_role_arg_as_type_expr,
};
use super::registration_role_decl::RoleDeclCx;
use super::*;
use crate::ast::ParamDef;
use crate::symbol::Symbol;

impl Interpreter {
    fn eval_role_arg_values(&mut self, arg_exprs: &[String]) -> Result<Vec<Value>, RuntimeError> {
        let mut values = Vec::with_capacity(arg_exprs.len());
        for expr in arg_exprs {
            if expr.trim_start().starts_with("::") {
                return Err(RuntimeError::new(
                    "X::Syntax::Malformed: cannot use ::T in role application".to_string(),
                ));
            }
            if should_treat_role_arg_as_type_expr(expr) {
                // A plain qualified name (`Type::Connect`, no brackets/parens)
                // may be an *enum value* rather than a type — an enum value used
                // as a role type argument (`class C does Packet[Type::Connect]`)
                // must bind to a typed param (`role Packet[Type $t]`) as the value,
                // not as a `Package` type object (which would fail the type check
                // and yield "No matching candidate found for the parametric role").
                // Try evaluating it; use the result only if it is an enum value.
                let trimmed = expr.trim();
                if !trimmed.contains(['[', '(', ' '])
                    && trimmed.contains("::")
                    && let Ok(value) = crate::parse_dispatch::parse_source(expr)
                        .and_then(|(stmts, _)| self.eval_block_value(&stmts))
                    && matches!(value.view(), ValueView::Enum { .. })
                {
                    values.push(value);
                    continue;
                }
                values.push(Value::package(Symbol::intern(trimmed)));
                continue;
            }
            // A bare block-literal argument (`R[{ .<id> // die }]`) must bind to a
            // `&callable` param as the Block itself. Evaluated as a *statement*, a
            // bare `{ ... }` is a block that mutsu immediately *executes* (yielding
            // its body's value, or dying), so `role R[&f]; class A does R[{ 1 }]`
            // saw an `Int`, not a `Callable`, and matched no candidate. Wrap it in
            // parens to force expression context, where `{ ... }` is a Block term.
            let expr_owned;
            let eval_expr = {
                let t = expr.trim();
                if t.starts_with('{') && t.ends_with('}') {
                    expr_owned = format!("({t})");
                    expr_owned.as_str()
                } else {
                    expr.as_str()
                }
            };
            match crate::parse_dispatch::parse_source(eval_expr)
                .and_then(|(stmts, _)| self.eval_block_value(&stmts))
            {
                Ok(value) => values.push(Self::namify_reparsed_colonpair_role_arg(expr, value)),
                Err(_) if looks_like_type_arg_expr(expr) => {
                    values.push(Value::package(Symbol::intern(expr.trim())));
                }
                Err(err) => return Err(err),
            }
        }
        Ok(values)
    }

    /// A role type/parameter argument (`R[:a(1)]`) is re-parsed and evaluated
    /// as a standalone source string, outside any argument-list AST node —
    /// so the compiler's call-site named-ness detection (ADR-0021 I3, keyed
    /// off `is_named_arg_expr`) never sees it, and a genuine colonpair
    /// argument compiles through the data-minting default (positional)
    /// instead of the named flavour `role_candidate_arity_ok` expects for a
    /// `:$name`-shaped role parameter. `R[:a(1)]` IS argument-list syntax
    /// conceptually (this is exactly the "internal runtime argument
    /// synthesis" case the ADR carves out as correct to mint named on
    /// purpose) — restore the named flavour when the source was a genuine
    /// bareword colonpair (`:name(...)`/`:name`/`:!name`, not `::Type`).
    fn namify_reparsed_colonpair_role_arg(source: &str, value: Value) -> Value {
        let trimmed = source.trim_start();
        if !trimmed.starts_with(':') || trimmed.starts_with("::") {
            return value;
        }
        match value.view() {
            ValueView::ValuePair(key, val) => match key.view() {
                ValueView::Str(s) => Value::pair(s.to_string(), val.clone()),
                _ => value,
            },
            _ => value,
        }
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
        if let Some(def) = self.registry().classes.get(constraint) {
            return 10 + def.parents.len() as i32;
        }
        if self.registry().roles.contains_key(constraint) {
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
            .filter(|arg| !arg.is_string_pair_value())
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

    pub(crate) fn resolve_role_candidate(
        &mut self,
        parent: &str,
    ) -> Result<Option<ResolvedRoleCandidate>, RuntimeError> {
        self.resolve_role_candidate_with_args(parent, None)
    }

    /// [`Self::resolve_role_candidate`], but with the bracket arguments
    /// already evaluated (ADR-0019 D4-3) instead of re-parsing them out of
    /// the concatenated `parent` string. `pre_args` is `None` for every
    /// call site that has no precompiled chunk for this parent — a
    /// computed/dynamic role application (`resolve_role_candidate`'s
    /// existing callers), or a bracket whose content did not parse as a
    /// clean expression list (D4-1) — and behaves exactly as before.
    pub(crate) fn resolve_role_candidate_with_args(
        &mut self,
        parent: &str,
        pre_args: Option<&[Value]>,
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
        let Some(candidates) = self.registry().role_candidates.get(base_role_name).cloned() else {
            if let Some(role) = self.registry().roles.get(base_role_name).cloned() {
                return Ok(Some((role, Vec::new(), Vec::new())));
            }
            if matches!(base_role_name, "Positional" | "Associative" | "Callable") {
                return Ok(Some((builtin_role_def(), Vec::new(), Vec::new())));
            }
            return Ok(None);
        };

        let arg_values = if let Some(pre_args) = pre_args {
            pre_args.to_vec()
        } else {
            let arg_exprs = if let Some(bracket_start) = parent.find('[') {
                let args_str = &parent[bracket_start + 1..parent.len() - 1];
                parse_role_type_args(args_str)
            } else {
                Vec::new()
            };
            self.eval_role_arg_values(&arg_exprs)?
        };

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
        // Properly bind args (handling named params, defaults, etc.) and extract
        // resolved values per param name, instead of using raw positional zip.
        let resolved_values = if !selected.type_param_defs.is_empty() {
            let saved_env = self.env.clone();
            let candidate_param_names: Vec<String> = selected
                .type_param_defs
                .iter()
                .map(|pd| pd.name.clone())
                .collect();
            let _ = self.bind_function_args_values(
                &selected.type_param_defs,
                &candidate_param_names,
                &arg_values,
            );
            let mut resolved = Vec::with_capacity(selected.type_params.len());
            for (i, param_name) in selected.type_params.iter().enumerate() {
                // `type_params` are sigil-less (`f`), but `bind_function_args_values`
                // stores a callable param under its full name (`&f`) — only `$`/`@`/`%`
                // sigils are stripped. Fall back to the ParamDef's own name so a
                // `role R[&f]` param resolves to its bound Callable, not `Nil`.
                let value = self
                    .env
                    .get(param_name)
                    .or_else(|| {
                        selected
                            .type_param_defs
                            .get(i)
                            .and_then(|pd| self.env.get(&pd.name))
                    })
                    .cloned()
                    .unwrap_or(Value::NIL);
                resolved.push(value);
            }
            self.env = saved_env;
            resolved
        } else {
            arg_values
        };
        Ok(Some((
            selected.role_def,
            selected.type_params,
            resolved_values,
        )))
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_role_decl(
        &mut self,
        name: &str,
        type_params: &[String],
        type_param_defs: &[ParamDef],
        body: &[Stmt],
        role_is_rw: bool,
        language_version: &str,
        own_attribute_names: &[Symbol],
        body_used_modules: &[String],
        body_declared_types: &[String],
        attr_decls: &[(Symbol, crate::opcode::CompiledAttrDecl)],
        method_name_chunks: &[Option<crate::opcode::CompiledDeclExpr>],
        method_decls: &[crate::opcode::CompiledMethodDecl],
        is_stub_body: bool,
        our_scope_violation: Option<&str>,
        parent_ops: &[crate::opcode::RoleParentOp],
    ) -> Result<(), RuntimeError> {
        self.clear_private_zeroarg_method_cache();

        if let Some(decl) = our_scope_violation {
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
        self.check_role_type_param_validity(type_param_defs)?;

        // If this is a stub declaration (body is `...`, `!!!`, or `???`)
        // and a real (non-stub) role already exists under this name, treat
        // the stub as a forward declaration / no-op — don't register a new
        // stub candidate that would shadow the real one.
        if is_stub_body
            && type_params.is_empty()
            && self
                .registry()
                .roles
                .get(name)
                .is_some_and(|existing| !existing.is_stub_role)
        {
            return Ok(());
        }
        let prev_parents = self.reset_role_registration_state(name, type_params);
        let role_def = RoleDef {
            attributes: Vec::new(),
            methods: HashMap::new(),
            // A yada-body forward declaration (`role Foo { ... }`) is a stub role:
            // mark it so the real definition that follows replaces it instead of
            // being treated as a second, conflicting role of the same name (which
            // made transitively-composed methods like `add_route` look like a
            // cross-role X::Role::Composition::Conflict).
            is_stub_role: is_stub_body,
            is_hidden: false,
            is_rw: role_is_rw,
            captured_env: None,
            wildcard_handles: Vec::new(),
            role_id: super::next_role_id(),
            attribute_conflicts: Vec::new(),
            own_attribute_names: HashSet::new(),
            deferred_body_stmts: Vec::new(),
            deferred_custom_traits: Vec::new(),
        };
        let mut cx = RoleDeclCx {
            name,
            type_params,
            role_is_rw,
            is_parametric: !type_params.is_empty(),
            role_def,
            role_own_attrs: own_attribute_names.iter().map(|s| s.resolve()).collect(),
            body_used_modules: body_used_modules.iter().cloned().collect(),
            body_declared_types: body_declared_types.iter().cloned().collect(),
            attr_decls,
            method_name_chunks,
            method_decls,
            method_name_chunk_idx: 0,
            parent_ops,
            parent_op_idx: 0,
        };
        self.walk_role_body(body, &mut cx)?;
        self.finish_role_registration(
            name,
            type_params,
            type_param_defs,
            language_version,
            cx.role_def,
            prev_parents,
        );
        Ok(())
    }
}
