//! Compiling a `subset ... where PRED` declaration (and the anonymous subset
//! a `my $x where PRED` declaration desugars to).

use super::*;

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
        let with_closure = match &stmt {
            Stmt::SubsetDecl {
                predicate: Some(pred),
                ..
            } if Self::is_closure_predicate(pred)
                && !self.decl_time_expr_free_var_syms(pred).is_empty() =>
            {
                // The registry keeps the closure past this frame: it escapes,
                // so the lexicals it writes get shared cells.
                self.with_escape(true, |c| c.compile_expr(pred));
                true
            }
            _ => false,
        };
        let idx = self.code.add_stmt(stmt);
        self.code.emit(OpCode::RegisterSubset { idx, with_closure });
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
