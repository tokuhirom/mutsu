//! Publish expression-position methods without changing the source AST.

use super::*;
use crate::ast_visit::Visit;

impl Compiler {
    /// Class/role plans share a compiler-owned body view. A named method
    /// initializer remains in its variable declaration and also contributes
    /// the method-table declaration that Raku installs at composition time.
    pub(super) fn with_expression_methods(stmt: &Stmt) -> Option<Stmt> {
        let body = match stmt {
            Stmt::ClassDecl { body, .. } | Stmt::RoleDecl { body, .. } => body,
            _ => return None,
        };
        #[derive(Default)]
        struct Methods(Vec<Stmt>);
        impl<'ast> Visit<'ast> for Methods {
            fn visit_stmt(&mut self, stmt: &'ast Stmt) {
                if matches!(
                    stmt,
                    Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::Package { .. }
                ) {
                    return;
                }
                crate::ast_visit::walk_stmt(self, stmt);
            }

            fn visit_expr(&mut self, expr: &'ast Expr) {
                if let Expr::DoStmt(stmt) = expr
                    && matches!(
                        stmt.as_ref(),
                        Stmt::MethodDecl {
                            is_my: false,
                            is_our: false,
                            ..
                        }
                    )
                {
                    self.0.push((**stmt).clone());
                }
                crate::ast_visit::walk_expr(self, expr);
            }
        }
        let mut methods = Methods::default();
        for item in body {
            methods.visit_stmt(item);
        }
        if methods.0.is_empty() {
            return None;
        }
        let mut result = stmt.clone();
        match &mut result {
            Stmt::ClassDecl { body, .. } | Stmt::RoleDecl { body, .. } => {
                body.splice(0..0, methods.0);
            }
            _ => unreachable!(),
        }
        Some(result)
    }

    /// The expression's Method has its own routine identity and state. It
    /// does not introduce a lexical routine under the method's public name.
    pub(super) fn compile_named_method_value(&mut self, stmt: &Stmt) {
        let Stmt::MethodDecl {
            name,
            params,
            param_defs,
            body,
            return_type,
            is_rw,
            is_raw,
            is_submethod,
            multi,
            ..
        } = stmt
        else {
            unreachable!()
        };
        let effective =
            crate::method_signature_shared::effective_method_param_defs(param_defs, false);
        let (_, mut defs) =
            crate::runtime::registration_class_body_method_forms::method_sub_form_params(
                params, &effective,
            );
        if let Some(receiver) = defs.first_mut() {
            receiver.is_invocant = true;
            if receiver.type_constraint.is_none() {
                receiver.type_constraint = self.static_class.clone();
            }
        }
        let params = defs.iter().map(|p| p.name.clone()).collect::<Vec<_>>();
        let declarator = if *is_submethod {
            crate::ast::RoutineDeclarator::Submethod
        } else {
            crate::ast::RoutineDeclarator::Method
        };
        let traits = if *multi {
            vec![(crate::ast::MULTI_CANDIDATE_VALUE_MARKER.to_string(), None)]
        } else {
            Vec::new()
        };
        self.compile_expr_anon_sub_params(
            &params,
            &defs,
            return_type,
            body,
            *is_rw,
            *is_raw,
            &traits,
            false,
            declarator,
            *name,
        );
    }
}
