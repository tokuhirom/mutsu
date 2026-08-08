//! Lowering a source declaration into its typed declaration plan (ADR-0019).
//!
//! A declaration carries expressions of its own, separate from any routine body:
//! the computed name of `sub ::($name) {...}` and the argument of each custom
//! trait (`is native(LIB)`). They are compiled here, once, into child chunks the
//! VM runs through its normal re-entrant bytecode entry at registration time.
use super::*;

impl Compiler {
    /// Lower a declaration-time expression to its own bytecode chunk (ADR-0019 C5).
    ///
    /// This replaces compiling the expression at every registration through
    /// `Interpreter::compile_block_value`, and it is deliberately compiled the
    /// same way that helper compiled it: a standalone unit with no local slots,
    /// so every variable it names resolves through the environment the
    /// declaration registers in. The package and distribution come from the
    /// declaration's own lexical position rather than from whatever routine
    /// frame happens to be live when registration runs.
    pub(crate) fn compile_decl_expr(&self, expr: &Expr) -> crate::opcode::CompiledDeclExpr {
        self.compile_decl_expr_inner(expr, false)
    }

    fn compile_decl_expr_inner(
        &self,
        expr: &Expr,
        mint_named_pair: bool,
    ) -> crate::opcode::CompiledDeclExpr {
        let mut chunk_compiler = Compiler::new();
        chunk_compiler.is_routine = self.is_routine;
        chunk_compiler.lexically_in_routine = self.lexically_in_routine;
        chunk_compiler.enclosing_package = Some(
            self.enclosing_package
                .clone()
                .unwrap_or_else(|| self.current_package.clone()),
        );
        chunk_compiler.set_current_package(self.current_package.clone());
        chunk_compiler.current_distribution = self.current_distribution.clone();
        chunk_compiler.last_source_line = self.last_source_line;
        // ADR-0021 I2/I3: a bareword-keyed fat-arrow (or colonpair, same AST
        // shape) written directly as a declaration-time trait/role argument
        // (`is foo(:bar(1))`, `role B does A[:a(1)]`) mints the named
        // flavour — this chunk is conceptually an argument list, even
        // though it's compiled as a standalone top-level statement.
        if mint_named_pair
            && matches!(expr, Expr::Binary { op, .. } if *op == crate::token_kind::TokenKind::FatArrow)
        {
            chunk_compiler.mint_named_pair = true;
        }
        let body = [Stmt::Expr(expr.clone())];
        let (code, fns) = chunk_compiler.compile(&body);
        crate::opcode::CompiledDeclExpr {
            code: std::sync::Arc::new(code),
            fns: std::sync::Arc::new(fns),
        }
    }

    /// Lower one custom trait's argument. A constant needs no chunk: it is
    /// already the value registration will use.
    fn compile_decl_trait_arg(&self, expr: &Expr) -> crate::opcode::DeclTraitArg {
        match expr {
            Expr::Literal(value) => crate::opcode::DeclTraitArg::Literal(value.clone()),
            _ => crate::opcode::DeclTraitArg::Compiled(self.compile_decl_expr_inner(expr, true)),
        }
    }

    /// Precompile the runtime-resolved-name chunk of each top-level `method`/
    /// `submethod` declaration in a class or role body (ADR-0019 D3-1), one
    /// entry per method encountered after `SyntheticBlock` flattening —
    /// mirroring `run_class_body`/`walk_role_body`'s own flattening exactly,
    /// so registration can read a chunk by position instead of recompiling
    /// `name_expr` from raw AST on every registration. A method's fallback
    /// `name: Symbol` is not a reliable key here (unlike an attribute's
    /// unique name): an indirect declaration with a non-literal expression
    /// falls back to a shared placeholder, and ordinary multi methods
    /// legitimately share a literal name — position is the only key both
    /// sides can agree on.
    fn compile_method_name_chunks(
        &self,
        body: &[Stmt],
    ) -> Vec<Option<crate::opcode::CompiledDeclExpr>> {
        body.iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .filter_map(|stmt| match stmt {
                Stmt::MethodDecl { name_expr, .. } => {
                    Some(name_expr.as_ref().map(|e| self.compile_decl_expr(e)))
                }
                _ => None,
            })
            .collect()
    }

    /// Lower a declaration's custom-trait arguments, index-aligned with its
    /// `custom_traits` list.
    fn compile_decl_trait_args(
        &self,
        custom_traits: &[(String, Option<Expr>)],
    ) -> Vec<Option<crate::opcode::DeclTraitArg>> {
        custom_traits
            .iter()
            .map(|(_, arg)| arg.as_ref().map(|e| self.compile_decl_trait_arg(e)))
            .collect()
    }

    /// Lower a `SubDecl` into the declaration-plan pool, compiling its
    /// declaration-time expressions (computed name, custom-trait arguments) into
    /// child chunks first.
    pub(crate) fn add_sub_decl_plan(&mut self, stmt: &Stmt) -> u32 {
        let Stmt::SubDecl {
            name_expr,
            custom_traits,
            ..
        } = stmt
        else {
            panic!("add_sub_decl_plan expects SubDecl");
        };
        let name_chunk = name_expr.as_ref().map(|e| self.compile_decl_expr(e));
        let trait_args = self.compile_decl_trait_args(custom_traits);
        self.code.add_sub_decl_plan(stmt, name_chunk, trait_args)
    }

    /// [`Self::add_sub_decl_plan`] for a class declaration.
    pub(crate) fn add_class_decl_plan(&mut self, stmt: &Stmt) -> u32 {
        let Stmt::ClassDecl {
            name,
            name_expr,
            custom_traits,
            body,
            parent_args,
            is_hidden,
            ..
        } = stmt
        else {
            panic!("add_class_decl_plan expects ClassDecl");
        };
        let name_chunk = name_expr.as_ref().map(|e| self.compile_decl_expr(e));
        let trait_args = self.compile_decl_trait_args(custom_traits);
        let attr_decls = self.compile_class_attr_decls(body);
        let method_name_chunks = self.compile_method_name_chunks(body);
        let parent_arg_chunks = self.compile_parent_arg_chunks(parent_args);
        // ADR-0019 D3-8a: only a statically-named class (`name_expr` absent —
        // a `class ::($n) {...}` has no compile-time-known package to key
        // method bodies under) gets its methods' bodies compiled here; the
        // computed-name case leaves every entry `None` and keeps using the
        // registration-time throwaway compile. A `__hoisted` forward-reference
        // shell (`hoist_type_decl_shells`) carries a full copy of every method
        // body too, but only the SOURCE-ORDER declaration's plan is ever the
        // one D3-8b/c would install from (the shell's own `RegisterDecl` is
        // superseded at runtime by the real one) — mirrors the sub side,
        // where only the source-order site compiles the body. Skip the
        // (otherwise-redundant) compile there.
        let is_hoisted_shell = custom_traits.iter().any(|(t, _)| t == "__hoisted");
        let package_name = if name_expr.is_none() && !is_hoisted_shell {
            Some(self.qualified_class_decl_name(&name.resolve()))
        } else {
            None
        };
        // Class-body methods auto-detect a bare `@_` read the way
        // `class_body_method_decl` does (`apply_auto_positional_slurpy:
        // true`); `is_hidden` gates the implicit `*%_` the same way too.
        let method_compiled_keys =
            self.compile_method_body_keys(body, package_name.as_deref(), *is_hidden, true);
        self.code.add_class_decl_plan(
            stmt,
            name_chunk,
            trait_args,
            attr_decls,
            method_name_chunks,
            parent_arg_chunks,
            method_compiled_keys,
        )
    }

    /// Compile each top-level `method`/`submethod` declaration's body to
    /// main-pass bytecode (ADR-0019 D3-8a), in the same `SyntheticBlock`-
    /// flattened order [`Self::compile_method_name_chunks`] and
    /// `compile_method_decls` (`opcode.rs`) already walk, so the returned
    /// vec shares their position cursor. `package_name` is `None` when the
    /// declaring class/role's own name is computed — no static package to
    /// key bodies under, so every entry stays `None`. A method whose OWN
    /// name is computed (`method ::($n) {...}`) also stays `None` regardless
    /// of the package, mirroring D3-1's `method_name_chunks` fallback for
    /// the same case.
    fn compile_method_body_keys(
        &mut self,
        body: &[Stmt],
        package_name: Option<&str>,
        is_hidden: bool,
        apply_auto_positional_slurpy: bool,
    ) -> Vec<Option<Symbol>> {
        let flattened: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        let Some(package_name) = package_name else {
            return flattened
                .iter()
                .filter(|stmt| matches!(stmt, Stmt::MethodDecl { .. }))
                .map(|_| None)
                .collect();
        };
        let package_name = package_name.to_string();
        let mut keys = Vec::new();
        for stmt in flattened {
            let Stmt::MethodDecl {
                name,
                name_expr,
                param_defs,
                body,
                is_rw,
                return_type,
                ..
            } = stmt
            else {
                continue;
            };
            if name_expr.is_some() {
                keys.push(None);
                continue;
            }
            keys.push(self.compile_method_body(
                &package_name,
                &name.resolve(),
                param_defs,
                body,
                is_hidden,
                apply_auto_positional_slurpy,
                *is_rw,
                return_type.as_ref(),
            ));
        }
        keys
    }

    /// Lower each parent/role bracket argument list to declaration-trait-arg
    /// chunks (ADR-0019 D4-2), keyed by the same concatenated parent string
    /// `parents`/`does_parents`/`hidden_parents` already use as a registry
    /// lookup key. No consumer reads this yet (D4-3).
    fn compile_parent_arg_chunks(
        &self,
        parent_args: &[(String, Vec<Expr>)],
    ) -> Vec<(String, Vec<crate::opcode::DeclTraitArg>)> {
        parent_args
            .iter()
            .map(|(key, args)| {
                (
                    key.clone(),
                    args.iter()
                        .map(|e| self.compile_decl_trait_arg(e))
                        .collect(),
                )
            })
            .collect()
    }

    /// Precompile a full `CompiledAttrDecl` for each attribute a class body
    /// declares directly in its own body (ADR-0019 D2b remainder), keyed by
    /// attribute name so `class_body_has_decl` can look one up without
    /// depending on its registration-time walk visiting attributes in
    /// exactly this order. Mirrors `class_own_attribute_names`'s eligibility
    /// (`our`/`my` class-level attributes are excluded — see
    /// `run_class_body`'s early `SkipTail` return before the per-instance
    /// attribute is registered) and traversal (SyntheticBlock-flattened top
    /// level plus `has` nested directly inside a body `sub`, recursively) —
    /// same helper shape as `class_own_attribute_names`/
    /// `collect_nested_has_decl_names`, which the earlier
    /// `collect_attr_is_default_chunks` did NOT share, double-pushing a
    /// nested-sub `has ... is default` (once from the `SubDecl` arm's direct
    /// loop, once from its own recursive call re-matching the same
    /// statement). This mirrors the registration-side non-recursive-repeat
    /// exactly to avoid that trap, harmless as it was under
    /// first-match-wins name-keyed lookup.
    fn compile_class_attr_decls(
        &self,
        body: &[Stmt],
    ) -> Vec<(Symbol, crate::opcode::CompiledAttrDecl)> {
        let mut out: Vec<(Symbol, crate::opcode::CompiledAttrDecl)> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .filter_map(|stmt| match stmt {
                Stmt::HasDecl {
                    name,
                    is_our,
                    is_my,
                    ..
                } if !*is_our && !*is_my => Some((*name, self.compile_class_attr_decl(stmt))),
                _ => None,
            })
            .collect();
        self.collect_nested_class_attr_decls(body, &mut out);
        out
    }

    fn collect_nested_class_attr_decls(
        &self,
        stmts: &[Stmt],
        out: &mut Vec<(Symbol, crate::opcode::CompiledAttrDecl)>,
    ) {
        for s in stmts {
            match s {
                Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::HasDecl { .. } => {}
                Stmt::SubDecl { body, .. } => {
                    for inner in body {
                        if let Stmt::HasDecl {
                            name,
                            is_our,
                            is_my,
                            ..
                        } = inner
                            && !*is_our
                            && !*is_my
                        {
                            out.push((*name, self.compile_class_attr_decl(inner)));
                        }
                    }
                    self.collect_nested_class_attr_decls(body, out);
                }
                _ => {}
            }
        }
    }

    /// Build one attribute's typed descriptor, precompiling its `is
    /// default(...)`, `default`, and `where_constraint` trait/expr arguments
    /// (ADR-0019 D2c-1/D2c-4) inline instead of leaving them as raw `Expr`s
    /// for a registration-time `eval_block_value`/`.as_expr()` call.
    fn compile_class_attr_decl(&self, stmt: &Stmt) -> crate::opcode::CompiledAttrDecl {
        crate::opcode::CompiledAttrDecl::from_stmt(stmt, self.compile_attr_decl_chunks(stmt))
    }

    /// Build the `AttrDeclChunks` override for one `Stmt::HasDecl`, shared by
    /// the class and role attribute-descriptor collectors.
    fn compile_attr_decl_chunks(&self, stmt: &Stmt) -> crate::opcode::AttrDeclChunks {
        let Stmt::HasDecl {
            default,
            where_constraint,
            is_default,
            ..
        } = stmt
        else {
            unreachable!("compile_attr_decl_chunks called on a non-HasDecl statement");
        };
        crate::opcode::AttrDeclChunks {
            is_default: is_default.as_ref().map(|e| self.compile_decl_trait_arg(e)),
            default: default.as_ref().map(|e| self.compile_decl_trait_arg(e)),
            where_constraint: where_constraint
                .as_deref()
                .map(|e| self.compile_decl_trait_arg(e)),
        }
    }

    /// [`Self::add_sub_decl_plan`] for a role declaration. A role's name is
    /// always compile-time known, so only its trait arguments are lowered.
    pub(crate) fn add_role_decl_plan(&mut self, stmt: &Stmt) -> u32 {
        let Stmt::RoleDecl {
            name,
            custom_traits,
            body,
            ..
        } = stmt
        else {
            panic!("add_role_decl_plan expects RoleDecl");
        };
        let trait_args = self.compile_decl_trait_args(custom_traits);
        let attr_decls = self.compile_role_attr_decls(body);
        let method_name_chunks = self.compile_method_name_chunks(body);
        let parent_ops = self.compile_role_parent_ops(body);
        // ADR-0019 D3-8a: `role_body_method_decl` always passes `is_hidden:
        // false` and never auto-detects a bare `@_` read (unlike the class
        // walker) — see its doc comment — so mirror both here exactly. Skip
        // the (otherwise fully redundant — the role hoist shell keeps the
        // whole original body, unlike the class shell) compile for a
        // `__hoisted` forward-reference shell, mirroring `add_class_decl_plan`.
        let is_hoisted_shell = custom_traits.iter().any(|(t, _)| t == "__hoisted");
        let method_compiled_keys = if is_hoisted_shell {
            self.compile_method_body_keys(body, None, false, false)
        } else {
            let package_name = self.qualified_role_decl_name(&name.resolve());
            self.compile_method_body_keys(body, Some(&package_name), false, false)
        };
        self.code.add_role_decl_plan(
            stmt,
            trait_args,
            attr_decls,
            method_name_chunks,
            parent_ops,
            method_compiled_keys,
        )
    }

    /// Precompile each `does`/`hides`/`is hidden` clause of a role's own body
    /// into a typed [`crate::opcode::RoleParentOp`] (ADR-0019 D7-3), one per
    /// `DoesDecl` statement in source order after the same `SyntheticBlock`
    /// flatten `compile_role_attr_decls` uses — `walk_role_body` flattens
    /// identically, so the runtime cursor and this vec stay aligned. The
    /// parser folds `does`/`is Parent`/`hides` clauses from the role header
    /// (and the `is hidden` trait) into synthetic `DoesDecl` statements
    /// prepended to the body; a `hides Parent` clause contributes two
    /// statements (the plain parent, then the `__mutsu_role_hides__` marker),
    /// which this precompute turns into two typed ops rather than collapsing
    /// them, preserving the walk's exact statement-by-statement shape.
    fn compile_role_parent_ops(&self, body: &[Stmt]) -> Vec<crate::opcode::RoleParentOp> {
        body.iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .filter_map(|stmt| match stmt {
                Stmt::DoesDecl { name, args } => {
                    let name_str = name.resolve();
                    if name_str == "__mutsu_role_hidden__" {
                        return Some(crate::opcode::RoleParentOp {
                            name: Symbol::intern(""),
                            hides: false,
                            hidden: true,
                            args: None,
                        });
                    }
                    if let Some(hidden_name) = name_str.strip_prefix("__mutsu_role_hides__") {
                        return Some(crate::opcode::RoleParentOp {
                            name: Symbol::intern(hidden_name),
                            hides: true,
                            hidden: false,
                            args: None,
                        });
                    }
                    Some(crate::opcode::RoleParentOp {
                        name: *name,
                        hides: false,
                        hidden: false,
                        args: args.as_ref().map(|exprs| {
                            exprs
                                .iter()
                                .map(|e| self.compile_decl_trait_arg(e))
                                .collect()
                        }),
                    })
                }
                _ => None,
            })
            .collect()
    }

    /// Precompile a full `CompiledAttrDecl` for each attribute a role body
    /// declares (ADR-0019 D2b remainder), keyed by attribute name — see
    /// `compile_class_attr_decls`. Mirrors `role_body_prescan`'s single-level
    /// `SyntheticBlock` flatten with no nested-sub surfacing (roles have none
    /// — `walk_role_body`'s own comment confirms it) and includes class-level
    /// (`our`/`my`) attributes, unlike the class side: `role_body_has_decl`
    /// handles both kinds through the same arm. Precompiles `is default(...)`/
    /// `default`/`where_constraint` the same way the class side does
    /// (ADR-0019 D2c-4) — role attribute defaults reference the role's type
    /// parameters as ordinary env variables bound before evaluation, not as
    /// AST substitution, so a single compile-time chunk is sound regardless
    /// of which concrete class later composes this role (confirmed by the
    /// D2c research pass).
    fn compile_role_attr_decls(
        &self,
        body: &[Stmt],
    ) -> Vec<(Symbol, crate::opcode::CompiledAttrDecl)> {
        body.iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .filter_map(|stmt| match stmt {
                Stmt::HasDecl { name, .. } => Some((
                    *name,
                    crate::opcode::CompiledAttrDecl::from_stmt(
                        stmt,
                        self.compile_attr_decl_chunks(stmt),
                    ),
                )),
                _ => None,
            })
            .collect()
    }
}
