//! The assignment-form substitution RHS (`s[pat] = EXPR`, `S[pat] = EXPR`),
//! compiled as a thunk closure the substitution op calls once per match.

use super::*;
use crate::symbol::Symbol;

impl Compiler {
    /// Push the closure of an assignment-form substitution's RHS
    /// (`s[pat] = EXPR`), which the `Subst` / `NonDestructiveSubst` op pops
    /// and calls once per match. EXPR is a thunk of the enclosing block, not
    /// a Block of its own: a placeholder or an anonymous `state` (`$++`) in
    /// it was declared in the enclosing scope when EXPR was parsed, so the
    /// closure's only parameter is the per-match `$/` (its placeholders stay
    /// the enclosing block's free variables) and, like a pointy block, it
    /// binds no `$_` of its own and is no `return` boundary.
    pub(super) fn compile_subst_replacement_thunk(&mut self, thunk: Option<&Expr>) {
        let Some(expr) = thunk else {
            return;
        };
        let body = vec![Stmt::Expr(expr.clone())];
        // The one parameter is the match, bound as the thunk's own `$/`: a
        // closure captures its free variables when it is built (before the
        // first match), so an enclosing `$/` read by name would be the stale
        // match that preceded the substitution.
        let params = vec!["/".to_string()];
        let param_defs = Vec::new();
        let mut compiled = self.compile_closure_body(&params, &param_defs, &body);
        compiled.is_pointy_block = true;
        let esc = self.escaping_position;
        let cc_idx = self.add_closure_code_baked(compiled, esc);
        let idx = self.code.add_stmt(Stmt::SubDecl {
            name: Symbol::intern(""),
            name_expr: None,
            params,
            param_defs,
            return_type: None,
            associativity: None,
            precedence_trait: None,
            signature_alternates: Vec::new(),
            body,
            multi: false,
            is_rw: false,
            is_raw: false,
            is_export: false,
            export_tags: Vec::new(),
            is_test_assertion: false,
            supersede: false,
            custom_traits: Vec::new(),
        });
        self.code
            .emit(OpCode::MakeAnonSubParams(idx, Some(cc_idx), false));
    }
}
