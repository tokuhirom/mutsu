//! Compiling a `subset ... where PRED` declaration (and the anonymous subset
//! a `my $x where PRED` declaration desugars to).

use super::*;
use crate::ast_visit::{Visit, walk_expr};

/// Whether an expression names a type (a bare `Foo`), which resolves through
/// the declaring scope's lexical imports and aliases.
struct TypeNameScan(bool);

impl<'ast> Visit<'ast> for TypeNameScan {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if matches!(expr, Expr::BareWord(_)) {
            self.0 = true;
        }
        walk_expr(self, expr);
    }
}

impl Compiler {
    /// Emit the registration of `stmt` (a `Stmt::SubsetDecl`).
    ///
    /// A `where` block that refers to outer lexicals is a closure over its
    /// declaring scope in Raku: `my $n = 0; subset C where { $n++; True }`
    /// counts every check in `$n`. So such a predicate is built here, as a
    /// closure value capturing those lexicals, and handed to the registration;
    /// the type check calls it (#10868). A predicate with no free lexical
    /// (`where * > 0`, `where { $_ %% 2 }`) depends on nothing but its
    /// argument and keeps the inline check that binds the topic and runs the
    /// body without a call.
    pub(super) fn emit_register_subset(&mut self, stmt: Stmt) {
        let (with_closure, with_refinement) = match &stmt {
            Stmt::SubsetDecl {
                predicate: Some(pred),
                ..
            } => {
                let is_closure = Self::is_closure_predicate(pred);
                // A bare type name in the predicate (`where $_ ~~ Resolution`)
                // may be an import of the declaring scope, which the checker
                // -- running in the caller's scope -- cannot see; close over
                // the declaration scope for it as well.
                let mut names = TypeNameScan(false);
                names.visit_expr(pred);
                let with_closure = is_closure
                    && (names.0 || !self.decl_time_expr_free_var_syms(pred).is_empty());
                // `.^refinement` needs the predicate as a callable. A code
                // literal is that already; anything else (`where /a/`) is
                // wrapped as a block that smartmatches the value, as rakudo
                // does. The registry keeps the value past this frame: it
                // escapes, so the lexicals it writes get shared cells.
                let callable = if is_closure {
                    pred.clone()
                } else {
                    Expr::Lambda {
                        param: "_".to_string(),
                        body: vec![Stmt::Expr(Expr::Binary {
                            left: Box::new(Expr::Var("_".to_string())),
                            op: crate::token_kind::TokenKind::SmartMatch,
                            right: Box::new(pred.clone()),
                        })],
                        is_whatever_code: false,
                        param_sigilless: false,
                    }
                };
                self.with_escape(true, |c| c.compile_expr(&callable));
                (with_closure, true)
            }
            _ => (false, false),
        };
        let idx = self.code.add_stmt(stmt);
        self.code.emit(OpCode::RegisterSubset {
            idx,
            with_closure,
            with_refinement,
        });
    }

    /// A predicate written as a code literal, which evaluates to the callable
    /// the type check invokes (as opposed to a value it smartmatches against).
    fn is_closure_predicate(pred: &Expr) -> bool {
        matches!(
            pred,
            Expr::AnonSub { .. }
                | Expr::AnonSubParams { .. }
                | Expr::Lambda { .. }
                | Expr::WhateverCurry(_)
        )
    }
}
