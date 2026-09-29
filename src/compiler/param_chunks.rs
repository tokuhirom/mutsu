//! Precompiling a signature's parameter expressions (ADR-0132).
//!
//! A `where` clause, a non-literal default and a shape dimension are evaluated
//! by the binder on every call. They used to reach it as AST and were compiled
//! there each time (`eval_block_value(&[Stmt::Expr(expr.clone())])`). They are
//! compiled here instead, once, when the routine or closure owning the
//! signature is compiled, and stored in the `ParamCode` slot every clone of the
//! parse node shares.
use super::*;

impl Compiler {
    /// Compile the signature-time expressions of `param_defs` (recursively
    /// through sub-signatures) into their `ParamCode` slots.
    ///
    /// `self` is the compiler of the scope that DECLARES the routine: the
    /// chunks are standalone units rooted there, like the declaration-time
    /// chunks of ADR-0019 C5, so every name they read resolves through the env
    /// the binder prepares (the routine's captures, the parameters bound so
    /// far, `$_`). They are compiled as running inside a routine, as the
    /// binder runs them.
    pub(super) fn attach_param_chunks(&self, param_defs: &[crate::ast::ParamDef]) {
        self.attach_param_chunks_in_package(param_defs, None);
    }

    /// [`Compiler::attach_param_chunks`] for a method, whose signature belongs
    /// to `package` (the class being declared) rather than to this compiler's
    /// own current package.
    pub(super) fn attach_param_chunks_in_package(
        &self,
        param_defs: &[crate::ast::ParamDef],
        package: Option<&str>,
    ) {
        if !param_defs.iter().any(Self::signature_needs_chunks) {
            return;
        }
        let mut sigilless = Vec::new();
        Self::collect_signature_sigilless(param_defs, &mut sigilless);
        self.attach_param_chunks_with(param_defs, &sigilless, package);
    }

    fn signature_needs_chunks(pd: &crate::ast::ParamDef) -> bool {
        (!pd.code.is_filled() && Self::param_has_evaluated_exprs(pd))
            || [&pd.sub_signature, &pd.outer_sub_signature]
                .into_iter()
                .flatten()
                .any(|nested| nested.iter().any(Self::signature_needs_chunks))
    }

    fn attach_param_chunks_with(
        &self,
        param_defs: &[crate::ast::ParamDef],
        sigilless: &[String],
        package: Option<&str>,
    ) {
        for pd in param_defs {
            if !pd.code.is_filled() && Self::param_has_evaluated_exprs(pd) {
                pd.code.fill(|| crate::ast::ParamChunks {
                    where_chunk: pd
                        .where_constraint
                        .as_deref()
                        .map(|w| self.compile_param_chunk(&Self::where_chunk_body(w), sigilless, package)),
                    default_chunk: pd
                        .default
                        .as_ref()
                        .filter(|d| !Self::is_bound_without_evaluation(d))
                        .map(|d| self.compile_param_chunk(&[Stmt::Expr(d.clone())], sigilless, package)),
                    shape_chunks: pd
                        .shape_constraints
                        .as_deref()
                        .unwrap_or_default()
                        .iter()
                        .map(|dim| {
                            (!matches!(dim, Expr::Whatever | Expr::HyperWhatever)).then(|| {
                                self.compile_param_chunk(&[Stmt::Expr(dim.clone())], sigilless, package)
                            })
                        })
                        .collect(),
                });
            }
            for nested in [&pd.sub_signature, &pd.outer_sub_signature]
                .into_iter()
                .flatten()
            {
                self.attach_param_chunks_with(nested, sigilless, package);
            }
        }
    }

    /// The statements the binder evaluates for a `where` clause: a block's own
    /// statements (its placeholders are bound in the env first), otherwise the
    /// clause as one expression statement — a value the binder smartmatches
    /// against, or a truthy result for a `.method` on the topic.
    pub(crate) fn where_chunk_body(where_expr: &Expr) -> Vec<Stmt> {
        match where_expr {
            Expr::AnonSub { body, .. } => body.clone(),
            expr => vec![Stmt::Expr(expr.clone())],
        }
    }

    /// A default the binder binds as-is, never evaluating it
    /// (`Interpreter::eval_param_default`'s immutable-scalar-literal shortcut).
    fn is_bound_without_evaluation(default: &Expr) -> bool {
        matches!(default, Expr::Literal(v)
            if crate::opcode::CompiledFunction::is_immutable_scalar_literal(v))
    }

    fn param_has_evaluated_exprs(pd: &crate::ast::ParamDef) -> bool {
        pd.where_constraint.is_some()
            || pd
                .default
                .as_ref()
                .is_some_and(|d| !Self::is_bound_without_evaluation(d))
            || pd.shape_constraints.as_ref().is_some_and(|s| !s.is_empty())
    }

    /// Every sigilless name the signature binds (`\x`), so a later parameter's
    /// expression reads `x` as that lexical rather than as a bareword call.
    fn collect_signature_sigilless(param_defs: &[crate::ast::ParamDef], out: &mut Vec<String>) {
        for pd in param_defs {
            if pd.sigilless && !pd.name.is_empty() {
                out.push(pd.name.clone());
            }
            for nested in [&pd.sub_signature, &pd.outer_sub_signature]
                .into_iter()
                .flatten()
            {
                Self::collect_signature_sigilless(nested, out);
            }
        }
    }

    fn compile_param_chunk(
        &self,
        body: &[Stmt],
        sigilless: &[String],
        package: Option<&str>,
    ) -> crate::opcode::CompiledDeclExpr {
        let mut chunk_compiler = self.new_decl_chunk_compiler();
        if let Some(package) = package {
            chunk_compiler.enclosing_package = Some(package.to_string());
            chunk_compiler.set_current_package(package.to_string());
        }
        chunk_compiler.is_routine = true;
        chunk_compiler.lexically_in_routine = true;
        chunk_compiler.seed_enclosing_sigilless(sigilless);
        chunk_compiler
            .enclosing_sigilless
            .extend(self.sigilless_locals.iter().cloned());
        chunk_compiler
            .enclosing_sigilless
            .extend(self.enclosing_sigilless.iter().cloned());
        let (code, fns) = chunk_compiler.compile(body);
        crate::opcode::CompiledDeclExpr {
            code: std::sync::Arc::new(code),
            fns: std::sync::Arc::new(fns),
        }
    }
}
