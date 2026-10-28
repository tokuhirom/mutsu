//! The eager lexical-type pass of role declaration: `my class`/`my
//! grammar`/`my enum`/exported `my constant`… declared directly in a role
//! body are registered when the role is declared, not only at composition.

use super::*;

impl Interpreter {
    /// Fix the current candidate's nominal signature types while its nested
    /// declarations are known, as class method registration already does.
    // Cost: O(s + p * d), s = method metadata/retained AST size,
    // p = signature types, d = declaring package depth.
    pub(crate) fn resolve_role_signature_types(
        &mut self,
        owner: &str,
        role_id: u64,
    ) -> Result<(), RuntimeError> {
        let methods = self
            .registry()
            .role_candidates
            .get(owner)
            .and_then(|candidates| {
                candidates
                    .iter()
                    .find(|candidate| candidate.role_def.role_id == role_id)
            })
            .map(|candidate| {
                (
                    candidate.role_def.methods.clone(),
                    candidate.type_params.clone(),
                )
            });
        let Some((mut methods, type_params)) = methods else {
            return Ok(());
        };
        for overloads in methods.values_mut() {
            for method in overloads {
                self.resolve_owned_param_types(owner, &mut std::sync::Arc::make_mut(&mut method.param_defs)[..], &type_params, 0)?;
                if let Some(return_type) = method.return_type.as_mut() {
                    *return_type =
                        self.resolve_role_owned_type_name(owner, return_type, &type_params);
                }
            }
        }
        let mut registry = self.registry_mut();
        if let Some(role) = registry.roles.get_mut(owner)
            && role.role_id == role_id
        {
            role.methods = methods.clone();
        }
        if let Some(candidates) = registry.role_candidates.get_mut(owner) {
            for candidate in candidates {
                if candidate.role_def.role_id == role_id {
                    candidate.role_def.methods = methods.clone();
                }
            }
        }
        Ok(())
    }

    // Cost: O(p * d), p = signature types, d = declaring package depth.
    fn resolve_owned_param_types(
        &self,
        owner: &str,
        params: &mut [crate::ast::ParamDef],
        type_params: &[String],
        depth: usize,
    ) -> Result<(), RuntimeError> {
        const MAX_SIGNATURE_DEPTH: usize = 128;
        if depth >= MAX_SIGNATURE_DEPTH {
            return Err(RuntimeError::new(
                "Signature nesting exceeds the interpreter limit (128)",
            ));
        }
        for param in params {
            if let Some(constraint) = param.type_constraint.as_mut() {
                *constraint = self.resolve_role_owned_type_name(owner, constraint, type_params);
            }
            if let Some(params) = param.sub_signature.as_mut() {
                self.resolve_owned_param_types(owner, params, type_params, depth + 1)?;
            }
            if let Some(params) = param.outer_sub_signature.as_mut() {
                self.resolve_owned_param_types(owner, params, type_params, depth + 1)?;
            }
            if let Some((params, return_type)) = param.code_signature.as_mut() {
                self.resolve_owned_param_types(owner, params, type_params, depth + 1)?;
                if let Some(return_type) = return_type {
                    *return_type =
                        self.resolve_role_owned_type_name(owner, return_type, type_params);
                }
            }
        }
        Ok(())
    }

    // Cost: O(n + p), n = type spelling length, p = role type parameters.
    fn resolve_role_owned_type_name(&self, owner: &str, name: &str, params: &[String]) -> String {
        let (base, _) = crate::runtime::types::strip_type_smiley(name);
        if params.iter().any(|param| param == base) {
            name.to_string()
        } else {
            self.resolve_method_type_name(owner, name)
        }
    }

    /// Save the name bindings a role's nested type declarations will replace.
    /// Registry definitions keep their declaration-site identities; only the
    /// temporary aliases used while running the body are restored afterwards.
    // Cost: O(s), s = declared names in the body.
    pub(crate) fn save_role_type_bindings(
        &self,
        owner: &str,
        ops: &[crate::opcode::DeferredBodyOp],
    ) -> Vec<(Symbol, Option<Value>)> {
        let mut keys = HashSet::new();
        for op in ops {
            if !matches!(op.kind, crate::opcode::DeferredBodyOpKind::TypeDecl) {
                continue;
            }
            for name in &op.declared_vars {
                keys.insert(*name);
                let short = crate::qualified::unqualified_part(*name);
                keys.insert(short);
                keys.insert(crate::term_names::lexical_type_key_sym(short));
                if !crate::qualified::is_qualified(*name) {
                    keys.insert(crate::qualified::qualified(Symbol::intern(owner), *name));
                }
            }
        }
        keys.into_iter()
            .map(|key| (key, self.env().get_sym(key).cloned()))
            .collect()
    }

    // Cost: O(s), s = saved name bindings.
    pub(crate) fn restore_role_type_bindings(&mut self, saved: Vec<(Symbol, Option<Value>)>) {
        for (key, value) in saved {
            match value {
                Some(value) => {
                    self.env_mut().insert_sym(key, value);
                }
                None => {
                    self.env_mut().remove_sym(key);
                }
            }
        }
    }

    /// Register every lexical TYPE declaration (`my class`, `my grammar`, `my
    /// role`, `my token`/`rule`/`regex`, `my enum`) declared directly in a
    /// role body when the role is declared, rather than waiting for a later
    /// composition — the type-declaration counterpart of
    /// [`Self::register_role_body_exported_subs`], for the same reason.
    ///
    /// A `my class`/`my grammar` in a role body is, like a plain `sub`, a
    /// lexical declaration of the role's own compilation unit: Rakudo makes
    /// it resolvable the moment the role loads, with no class ever composing
    /// it. `Date::Calendar::Strftime`'s private `_strftime` helper parses
    /// with a `my grammar prt-format {...}` and builds results with a `my
    /// class re-format {...}`, both declared alongside it in the role body —
    /// without this, `prt-format`/`re-format` stayed undeclared (resolving
    /// as a bare `Str`, `No such method 'parse'...`) for exactly the same
    /// reason `_strftime` itself used to be unresolvable (#8950).
    ///
    /// In a **parameterized** role a declaration is run here only when it
    /// does not mention any of the role's parameters
    /// ([`stmt_mentions_role_params`]). A declaration that does (`role
    /// R[::T] { my class C { has T $x } }`) cannot be evaluated before
    /// composition binds `T` to a concrete type — re-evaluating the body with
    /// real bindings is exactly what `run_composed_role_deferred_body` does
    /// at every composition site, and this eager pass must not pre-empt that
    /// with an unbound `T`. One that does not (`unit role V6[::K]; my class
    /// Foo is export { }`, `my constant X is export = 5`) means the same thing
    /// under every parameterization, so, as in a non-parameterized role, it
    /// is declared once here — importable the moment the role loads (#10244)
    /// — and a later composition re-running it is idempotent. Rakudo agrees:
    /// `role R[::T] { my class C {} }` has one `R::C` shared by `R[Int]` and
    /// `R[Str]`; because it already exists when a composition re-runs the
    /// body, the composition-time `R::C[Int]` renaming (reserved for classes
    /// the composition itself creates) leaves it alone.
    ///
    /// Rakudo goes further and declares even a parameter-mentioning
    /// declaration eagerly, *generically* over the unbound `T`; mutsu has no
    /// generic-type representation for that, so such a declaration stays
    /// composition-only.
    ///
    /// Only declarative statement kinds are eagerly run — never a plain
    /// statement or expression, whose side effects must not replay both here
    /// and at composition.
    pub(crate) fn register_role_body_lexical_types(
        &mut self,
        role_name: &str,
        deferred_body_ops: &[crate::opcode::DeferredBodyOp],
        type_params: &[String],
    ) -> Result<(), RuntimeError> {
        self.register_role_body_lexical_types_where(
            role_name,
            deferred_body_ops,
            type_params,
            |_| true,
        )
    }

    /// The nested `my role`/`my class` declarations a role body names as its
    /// own parents (`unit role L; my role Common {...}; also does Common;` in
    /// PDF::Attributes::Layout), registered *before* the role's parent walk
    /// resolves them — Rakudo has declared them by the time the `also does`
    /// is compiled. The full [`Self::register_role_body_lexical_types`] pass
    /// runs after the role is registered, as before.
    ///
    /// Cost: O(s * p), s = body statements, p = parent ops.
    pub(crate) fn register_role_body_parent_types(
        &mut self,
        role_name: &str,
        deferred_body_ops: &[crate::opcode::DeferredBodyOp],
        type_params: &[String],
        parent_ops: &[crate::opcode::RoleParentOp],
    ) -> Result<(), RuntimeError> {
        let names_parent = |stmt: &Stmt| match stmt {
            Stmt::ClassDecl { name, .. } | Stmt::RoleDecl { name, .. } => parent_ops
                .iter()
                .any(|op| !op.hides && !op.hidden && op.name == *name),
            _ => false,
        };
        if !deferred_body_ops.iter().any(|op| names_parent(&op.raw)) {
            return Ok(());
        }
        self.register_role_body_lexical_types_where(
            role_name,
            deferred_body_ops,
            type_params,
            names_parent,
        )
    }

    fn register_role_body_lexical_types_where(
        &mut self,
        role_name: &str,
        deferred_body_ops: &[crate::opcode::DeferredBodyOp],
        type_params: &[String],
        wanted: impl Fn(&Stmt) -> bool,
    ) -> Result<(), RuntimeError> {
        let parameterized = !type_params.is_empty();
        // The routines the body declares: they are registered after this
        // pass (or only at composition), so a constant that calls one waits
        // for composition, as before.
        let body_routines: Vec<String> = deferred_body_ops
            .iter()
            .filter_map(|op| match &op.raw {
                Stmt::SubDecl { name, .. } | Stmt::ProtoDecl { name, .. } => Some(name.resolve()),
                _ => None,
            })
            .collect();
        let saved_package = self.current_package().to_string();
        let saved_types = self.save_role_type_bindings(role_name, deferred_body_ops);
        let mut declared_types = HashSet::new();
        self.set_current_package(role_name.to_string());
        for op in deferred_body_ops {
            if !wanted(&op.raw) {
                continue;
            }
            let eager = match &op.raw {
                // An exported `my constant`/`my $x` is a compile-time
                // declaration too (#9981), and so is every `constant`: Rakudo
                // initializes it when the body is compiled, so a `sub` of the
                // role body exported to an importer that never composes the
                // role reads it (CSS::Units' `%Dimension`, #13279).
                Stmt::VarDecl {
                    is_export: true, ..
                }
                | Stmt::EnumDecl { .. }
                | Stmt::ClassDecl { .. }
                | Stmt::RoleDecl { .. }
                | Stmt::TokenDecl { .. }
                | Stmt::SubsetDecl { .. } => {
                    !parameterized || !stmt_mentions_role_params(&op.raw, type_params)
                }
                Stmt::VarDecl { constant, .. } if is_constant_decl(*constant) => {
                    (!parameterized || !stmt_mentions_role_params(&op.raw, type_params))
                        && !stmt_mentions_role_params(&op.raw, &body_routines)
                }
                // A sigilless or bind declaration arrives as the group
                // `role_body_plan` keeps whole.
                Stmt::SyntheticBlock(inner)
                    if crate::ast::scope_members(inner).any(|s| {
                        match s {
                            Stmt::VarDecl {
                                is_export,
                                constant,
                                ..
                            } => {
                                *is_export
                                    || (is_constant_decl(*constant)
                                        && !stmt_mentions_role_params(s, &body_routines))
                            }
                            _ => false,
                        }
                    }) =>
                {
                    !parameterized || !stmt_mentions_role_params(&op.raw, type_params)
                }
                _ => false,
            };
            if !eager {
                continue;
            }
            if let Err(error) = self.run_block_raw(std::slice::from_ref(&op.raw)) {
                self.restore_role_type_bindings(saved_types);
                self.set_current_package(saved_package);
                return Err(error);
            }
            if matches!(op.kind, crate::opcode::DeferredBodyOpKind::TypeDecl) {
                Self::collect_role_body_declared_names(
                    std::slice::from_ref(op),
                    &mut declared_types,
                );
            }
            // A lexical `my class C is export` records its tags on the node (see
            // `class_decl`); publish it the way an exported subset is.
            if let Stmt::ClassDecl {
                name,
                export_tags: Some(tags),
                ..
            } = &op.raw
                && !self.module.suppress_exports
            {
                let tags = tags.clone();
                let (pkg, short) = match crate::qualified::package_parent(*name) {
                    Some(parent) => (
                        parent.resolve().to_string(),
                        crate::qualified::unqualified_part(*name)
                            .resolve()
                            .to_string(),
                    ),
                    None => (role_name.to_string(), name.resolve().to_string()),
                };
                self.register_exported_var(pkg, short, tags);
            }
        }
        self.persist_role_body_lexicals(role_name, &declared_types);
        self.restore_role_type_bindings(saved_types);
        self.set_current_package(saved_package);
        Ok(())
    }
}

/// Whether `stmt` mentions any of a parameterized role's parameters
/// (`params`: the bare names of `::T` captures and `$n` value parameters).
///
/// Conservative by construction: every identifier the typed AST visitor
/// (ADR-0137) reports — variable and type names, type constraints (`T:D`,
/// `Array[T]`), source text compiled later — is inspected, and one containing
/// a parameter's name as a whole word counts as a mention. A false positive
/// only defers a declaration to composition (the behavior before #10244).
/// String literals are data, not mentions: `my constant X is export = "T"`
/// does not depend on `T`.
///
/// Cost: O(n), n = size of `stmt`'s AST (times the parameter count).
fn stmt_mentions_role_params(stmt: &Stmt, params: &[String]) -> bool {
    use crate::ast_visit::{NameKind, Visit, contains_word, walk_expr, walk_stmt};

    struct Mentions<'a> {
        params: Vec<&'a str>,
        found: bool,
    }
    impl<'ast> Visit<'ast> for Mentions<'_> {
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            if !self.found {
                walk_stmt(self, stmt);
            }
        }
        fn visit_expr(&mut self, expr: &'ast Expr) {
            if !self.found {
                walk_expr(self, expr);
            }
        }
        fn visit_name(&mut self, name: &str, _kind: NameKind) {
            self.found |= self.params.iter().any(|p| contains_word(name, p));
        }
    }

    let mut m = Mentions {
        params: params
            .iter()
            .map(|p| p.as_str())
            .filter(|p| !p.is_empty())
            .collect(),
        found: false,
    };
    m.visit_stmt(stmt);
    m.found
}

/// Whether a `VarDecl` is a `constant` declaration.
// Cost: O(1).
fn is_constant_decl(constant: Option<crate::ast::ConstantSigil>) -> bool {
    constant.is_some()
}
