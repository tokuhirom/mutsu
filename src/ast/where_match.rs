//! Whether a parameter's `where` clause can set the routine's `$/`.

use super::{Expr, ParamDef};
use crate::ast_visit::{Visit, walk_expr};
use crate::token_kind::TokenKind;

/// Finds a regex match (`m/.../`) or a smartmatch (`$x ~~ $regex`) anywhere
/// in a `where` clause; either one stores its result in `$/`.
#[derive(Default)]
struct SetsMatchVar {
    found: bool,
}

impl<'ast> Visit<'ast> for SetsMatchVar {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found {
            return;
        }
        match expr {
            Expr::MatchRegex(_)
            | Expr::MatchRegexTree { .. }
            | Expr::MatchRegexDynamicAdverbs { .. }
            | Expr::Binary {
                op: TokenKind::SmartMatch,
                ..
            } => self.found = true,
            _ => walk_expr(self, expr),
        }
    }
}

impl ParamDef {
    /// Whether the `where` clause may set `$/` -- a regex match (`where
    /// m/.../`) or a smartmatch (`where .uc ~~ $re`), at any depth -- whose
    /// match is the `$/` the routine's body sees (`$<year>`, `$0`). Such a
    /// clause runs at bind time even when multi resolution already verified
    /// it, because resolution ran it in a scope that discarded `$/`.
    // Cost: O(n), n = number of nodes in the `where` clause (early exit on
    // the first match).
    pub(crate) fn where_is_regex_match(&self) -> bool {
        self.where_constraint.as_deref().is_some_and(|expr| {
            let mut scan = SetsMatchVar::default();
            scan.visit_expr(expr);
            scan.found
        })
    }
}
