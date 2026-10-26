//! Publish expression-position methods without changing the source AST.

use super::*;
use crate::ast_visit::{Visit, VisitMut};

impl Compiler {
    /// Class/role plans share a compiler-owned body view. A named method
    /// initializer remains in its variable declaration and also contributes
    /// the method-table declaration that Raku installs at composition time.
    pub(super) fn with_expression_methods(stmt: &Stmt) -> Option<Stmt> {
        if !matches!(
            stmt,
            Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::AugmentClass { .. }
        ) {
            return None;
        }
        let mut result = stmt.clone();
        let body = match &mut result {
            Stmt::ClassDecl { body, .. }
            | Stmt::RoleDecl { body, .. }
            | Stmt::AugmentClass { body, .. } => body,
            _ => unreachable!(),
        };
        #[derive(Default)]
        struct Mark {
            sites: std::collections::HashSet<Symbol>,
        }
        impl VisitMut for Mark {
            fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
                if !matches!(
                    stmt,
                    Stmt::ClassDecl { .. }
                        | Stmt::RoleDecl { .. }
                        | Stmt::Package { .. }
                        | Stmt::AugmentClass { .. }
                ) {
                    crate::ast_visit::walk_stmt_mut(self, stmt);
                }
            }
            fn visit_expr_mut(&mut self, expr: &mut Expr) {
                if let Expr::DoStmt(stmt) = expr
                    && let Stmt::MethodDecl {
                        value_prototype,
                        is_my: false,
                        is_our: false,
                        ..
                    } = stmt.as_mut()
                    && value_prototype.is_none()
                {
                    let key = Symbol::intern(&format!(
                        "__METHOD_VALUE#{}__",
                        super::compile_session::mint()
                    ));
                    *value_prototype = Some(key);
                    self.sites.insert(key);
                }
                crate::ast_visit::walk_expr_mut(self, expr);
            }
        }
        let mut mark = Mark::default();
        for item in body.iter_mut() {
            mark.visit_stmt_mut(item);
        }
        if mark.sites.is_empty() {
            return None;
        }
        struct Methods<'a> {
            sites: &'a std::collections::HashSet<Symbol>,
            decls: Vec<Stmt>,
        }
        impl<'ast> Visit<'ast> for Methods<'_> {
            fn visit_stmt(&mut self, stmt: &'ast Stmt) {
                if !matches!(
                    stmt,
                    Stmt::ClassDecl { .. }
                        | Stmt::RoleDecl { .. }
                        | Stmt::Package { .. }
                        | Stmt::AugmentClass { .. }
                ) {
                    crate::ast_visit::walk_stmt(self, stmt);
                }
            }
            fn visit_expr(&mut self, expr: &'ast Expr) {
                if let Expr::DoStmt(stmt) = expr
                    && let Stmt::MethodDecl {
                        value_prototype: Some(site),
                        ..
                    } = stmt.as_ref()
                    && self.sites.contains(site)
                {
                    self.decls.push((**stmt).clone());
                }
                crate::ast_visit::walk_expr(self, expr);
            }
        }
        let mut methods = Methods {
            sites: &mark.sites,
            decls: Vec::new(),
        };
        for item in body.iter() {
            methods.visit_stmt(item);
        }
        body.splice(0..0, methods.decls);
        Some(result)
    }

    /// The expression's Method has its own routine identity and state. It
    /// does not introduce a lexical routine under the method's public name.
    pub(super) fn compile_named_method_value(&mut self, stmt: &Stmt) {
        let Stmt::MethodDecl {
            value_prototype,
            name,
            param_defs,
            body,
            return_type,
            is_rw,
            is_raw,
            is_submethod,
            multi,
            custom_traits,
            ..
        } = stmt
        else {
            unreachable!()
        };
        let effective = crate::method_signature_shared::effective_method_param_defs(
            param_defs,
            self.static_class_hidden,
        );
        let declared_receiver = param_defs.first().is_some_and(|p| p.is_invocant);
        let (mut defs, aliases) = crate::parser::method_literal_params(effective);
        if let Some(receiver) = defs.first_mut() {
            receiver.is_invocant = true;
            if !declared_receiver && receiver.type_constraint.is_none() {
                receiver.type_constraint = self.static_class.clone();
            }
        }
        let params = defs.iter().map(|p| p.name.clone()).collect::<Vec<_>>();
        let declarator = if *is_submethod {
            crate::ast::RoutineDeclarator::Submethod
        } else {
            crate::ast::RoutineDeclarator::Method
        };
        let body_expr = crate::parser::anon_method_expr(Vec::new(), None, body.clone(), declarator);
        let Expr::AnonSubParams { body, .. } =
            crate::parser::bind_invocant_aliases(body_expr, &aliases)
        else {
            unreachable!()
        };
        let mut traits = if value_prototype.is_none() {
            custom_traits.clone()
        } else {
            Vec::new()
        };
        if *multi {
            traits.push((crate::ast::MULTI_CANDIDATE_VALUE_MARKER.to_string(), None));
        }
        self.compile_expr_anon_sub_params(
            &params,
            &defs,
            return_type,
            &body,
            *is_rw,
            *is_raw,
            &traits,
            false,
            declarator,
            *name,
            *value_prototype,
        );
    }
}
