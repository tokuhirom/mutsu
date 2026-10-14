//! Whether a WhateverCode body mutates one of its placeholders.
//!
//! A WhateverCode parameter binds `is raw` only when its body writes through
//! the placeholder or depends on its container identity (`*++`, `* =:= $x`,
//! `*.=foo`); S02 "WhateverCode parameters are rw" needs that container. A
//! body that only reads it binds the value, which keeps a read-only `* + *`
//! on the light closure call (`vm_closure_light_call.rs` declines a raw
//! parameter) and avoids a topic-`_` writeback leak in `for`/grep frames.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::Visit;
use crate::token_kind::TokenKind;

/// Whether `body` mutates the placeholder variable `name` (`_` for a
/// single-`*` lambda, `__wc_N` for a numbered one) or depends on its
/// container identity.
///
/// The topic `_` is shadowed by every nested code object, so those are not
/// entered for it. A `__wc_N` name is generated and unique to this
/// WhateverCode, so a nested block naming it refers to the same parameter
/// and is scanned.
// Cost: O(n), n = size of `body` (outside nested code objects when `name` is `_`).
pub(crate) fn body_mutates_placeholder(body: &[Stmt], name: &str) -> bool {
    let mut scan = PlaceholderMutationScan {
        name,
        enter_code_objects: name != "_",
        found: false,
    };
    crate::ast_visit::walk_stmts(&mut scan, body);
    scan.found
}

struct PlaceholderMutationScan<'n> {
    name: &'n str,
    enter_code_objects: bool,
    found: bool,
}

impl PlaceholderMutationScan<'_> {
    fn is_placeholder(&self, e: &Expr) -> bool {
        matches!(e, Expr::Var(n) if n == self.name)
    }

    /// Whether `e` denotes the placeholder itself, seen through the operator
    /// chain that carries it (a prefix/postfix, an infix operand, a
    /// method-call invocant). A value-path spine, not a subtree search: an
    /// argument mentioning the placeholder does not make the expression it.
    fn refs_placeholder(&self, e: &Expr) -> bool {
        if self.is_placeholder(e) {
            return true;
        }
        match e {
            Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => self.refs_placeholder(expr),
            Expr::Binary { left, right, .. } => {
                self.refs_placeholder(left) || self.refs_placeholder(right)
            }
            Expr::MethodCall { target, .. } => self.refs_placeholder(target),
            _ => false,
        }
    }
}

impl<'ast> Visit<'ast> for PlaceholderMutationScan<'_> {
    fn visit_expr(&mut self, e: &'ast Expr) {
        if self.found || (!self.enter_code_objects && crate::ast::scope_scan::is_code_object(e)) {
            return;
        }
        self.found = match e {
            // `*++` / `*--` / `++*` / `--*`
            Expr::PostfixOp {
                op: TokenKind::PlusPlus | TokenKind::MinusMinus,
                expr,
            }
            | Expr::Unary {
                op: TokenKind::PlusPlus | TokenKind::MinusMinus,
                expr,
                ..
            } => self.refs_placeholder(expr),
            // `* =:= $x` — container identity needs the same container.
            Expr::Binary {
                op: TokenKind::Ident(op),
                left,
                right,
                ..
            } if op == "=:=" => self.refs_placeholder(left) || self.refs_placeholder(right),
            // `*.=foo` — mutating method-assign on the placeholder.
            Expr::MethodCall {
                target,
                modifier: Some('='),
                ..
            } => self.refs_placeholder(target),
            // `* += *` — a compound assignment through a numbered placeholder.
            // Not for the topic `_`: treating its assignment as a mutation
            // broke shaped-array `map(* *= 2)` writeback (99e7273ef), so the
            // single-star scan keeps its narrower set.
            Expr::AssignExpr { name, .. } => self.name != "_" && name == self.name,
            _ => false,
        };
        if !self.found {
            crate::ast_visit::walk_expr(self, e);
        }
    }
}
