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
            _ => crate::opcode::DeclTraitArg::Compiled(self.compile_decl_expr(expr)),
        }
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
            name_expr,
            custom_traits,
            body,
            ..
        } = stmt
        else {
            panic!("add_class_decl_plan expects ClassDecl");
        };
        let name_chunk = name_expr.as_ref().map(|e| self.compile_decl_expr(e));
        let trait_args = self.compile_decl_trait_args(custom_traits);
        let is_default_chunks = self.compile_attr_is_default_chunks(body);
        self.code
            .add_class_decl_plan(stmt, name_chunk, trait_args, is_default_chunks)
    }

    /// Precompile the `is default(...)` trait argument of each of a class
    /// body's own attributes into a child chunk (ADR-0019 D2c), keyed by
    /// attribute name so `class_body_has_decl` can look one up without
    /// depending on its registration-time walk visiting attributes in
    /// exactly this order. Mirrors `class_body_has_decl`'s eligibility
    /// (`our`/`my` class-level attributes never reach the `is_default` arm —
    /// see `run_class_body`'s early `SkipTail` return) and
    /// `collect_nested_class_has_decls`'s traversal (SyntheticBlock-flattened
    /// top level plus `has` nested directly inside a body `sub`, recursively).
    fn compile_attr_is_default_chunks(
        &self,
        body: &[Stmt],
    ) -> Vec<(Symbol, crate::opcode::DeclTraitArg)> {
        let mut out = Vec::new();
        self.collect_attr_is_default_chunks(body, &mut out);
        out
    }

    fn collect_attr_is_default_chunks(
        &self,
        body: &[Stmt],
        out: &mut Vec<(Symbol, crate::opcode::DeclTraitArg)>,
    ) {
        for stmt in body {
            match stmt {
                Stmt::SyntheticBlock(inner) => self.collect_attr_is_default_chunks(inner, out),
                Stmt::HasDecl {
                    name,
                    is_our,
                    is_my,
                    is_default: Some(expr),
                    ..
                } if !*is_our && !*is_my => {
                    out.push((*name, self.compile_decl_trait_arg(expr)));
                }
                Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::HasDecl { .. } => {}
                Stmt::SubDecl { body: inner, .. } => {
                    for s in inner {
                        if let Stmt::HasDecl {
                            name,
                            is_our,
                            is_my,
                            is_default: Some(expr),
                            ..
                        } = s
                            && !*is_our
                            && !*is_my
                        {
                            out.push((*name, self.compile_decl_trait_arg(expr)));
                        }
                    }
                    self.collect_attr_is_default_chunks(inner, out);
                }
                _ => {}
            }
        }
    }

    /// [`Self::add_sub_decl_plan`] for a role declaration. A role's name is
    /// always compile-time known, so only its trait arguments are lowered.
    pub(crate) fn add_role_decl_plan(&mut self, stmt: &Stmt) -> u32 {
        let Stmt::RoleDecl { custom_traits, .. } = stmt else {
            panic!("add_role_decl_plan expects RoleDecl");
        };
        let trait_args = self.compile_decl_trait_args(custom_traits);
        self.code.add_role_decl_plan(stmt, trait_args)
    }
}
