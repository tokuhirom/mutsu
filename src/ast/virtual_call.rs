//! The virtual-accessor-call check for attribute initializers, over the typed
//! AST visitor (ADR-0137).

use super::scope_scan::is_scope_declaration;
use super::{Expr, RoutineDeclarator, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt};
use crate::regex_tree::RegexNode;

struct FirstVirtualCall {
    found: Option<String>,
}

impl<'ast> Visit<'ast> for FirstVirtualCall {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        // A method, and the methods of a nested package, rebind the invocant;
        // a `sub` does not (rakudo rejects `has $.x = sub { $.y }`).
        let rebinds_invocant = is_scope_declaration(stmt) && !matches!(stmt, Stmt::SubDecl { .. });
        if self.found.is_none() && !rebinds_invocant {
            walk_stmt(self, stmt);
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        // An anonymous method rebinds the invocant; every other closure (a
        // block, a pointy block, a `sub`, a WhateverCode) still runs against
        // the partially-constructed object, as rakudo's check agrees.
        let rebinds_invocant = matches!(
            expr,
            Expr::AnonSubParams {
                declarator: RoutineDeclarator::Method | RoutineDeclarator::Submethod,
                ..
            }
        );
        if self.found.is_none() && !rebinds_invocant {
            walk_expr(self, expr);
        }
    }

    // A regex is not an attribute-initializer expression mutsu checks.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        let sigil = match kind {
            NameKind::Var => '$',
            NameKind::ArrayVar => '@',
            NameKind::HashVar => '%',
            _ => return,
        };
        if self.found.is_none() && name.starts_with('.') {
            self.found = Some(format!("{sigil}{name}"));
        }
    }
}

/// Find the first virtual accessor call (`$.attr` / `@.attr` / `%.attr`) used in
/// an attribute initializer expression. Such a call dereferences the
/// partially-constructed invocant and is X::Syntax::VirtualCall. Descends into
/// every closure that keeps the invocant (blocks, pointy blocks, subs,
/// WhateverCodes) but stops at methods and package declarations, which rebind
/// it.
// Cost: O(n), n = size of `expr`'s subtree.
pub(crate) fn first_virtual_call_in_expr(expr: &Expr) -> Option<String> {
    let mut v = FirstVirtualCall { found: None };
    v.visit_expr(expr);
    v.found
}
