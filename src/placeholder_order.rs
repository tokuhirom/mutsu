//! Ordering / cross-scope checks for bare `$name` vs placeholder `$^name`
//! uses within the same lexical block.
//!
//! A placeholder parameter (`$^name`) declares its block's `$name` under the
//! *plain* name, so:
//!  - a bare `$name` written **before** the `$^name` that declares it, in the
//!    SAME block, is `X::Placeholder::NonPlaceholder` (if `$name` also
//!    already exists in an outer scope) or `X::Undeclared` (otherwise) —
//!    `bare_precedes_placeholder` below;
//!  - a bare `$name` in a block that has no `$^name` of its own, but where a
//!    STRICTLY NESTED block (an `if`/`for`/`given` BLOCK body, `whenever`, or
//!    a closure) does use `$^name`, is *also* `X::Undeclared` — the inner
//!    block owns that placeholder; it does not leak outward —
//!    `bare_name_shadowed_by_nested_placeholder` below.
//!
//! Both checks need the same notion of "this block's own placeholder scope"
//! that `collect_placeholders_shallow` uses to build a block's own signature,
//! so both walk it through the same typed-visitor scope walk
//! (`crate::ast::placeholders::walk_stmt_placeholder_scope` /
//! `walk_expr_placeholder_scope`, ADR-0137).

use crate::ast::placeholders::{walk_expr_placeholder_scope, walk_stmt_placeholder_scope};
use crate::ast::{Expr, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_stmts};
use crate::regex_tree::RegexNode;

/// Check if a bare variable reference (`$name` or `$name = ...`) appears
/// before the corresponding placeholder variable (`$^name`) in source order,
/// within this block's own placeholder scope (see module docs).
///
/// This is a single left-to-right walk across all of `stmts`, not two
/// independent whole-statement containment checks: `$b + $^b` in ONE
/// statement has the placeholder appear textually after the bare use, so
/// [`OrderCheck`] threads a running "have we passed the placeholder yet" flag
/// through the walk itself, which follows source order (left-then-right for
/// `Expr::Binary`; a statement modifier's statement before its condition).
// Cost: O(n), n = size of the block's own placeholder scope.
pub(crate) fn bare_precedes_placeholder(stmts: &[Stmt], bare_name: &str) -> bool {
    let ph_name = format!("^{bare_name}");
    let mut state = OrderCheck {
        bare_name,
        ph_name: &ph_name,
        ph_seen: false,
        bare_before: false,
    };
    for stmt in stmts {
        state.visit_stmt(stmt);
        if state.bare_before {
            return true;
        }
    }
    false
}

/// The order-sensitive walk of [`bare_precedes_placeholder`].
struct OrderCheck<'a> {
    bare_name: &'a str,
    ph_name: &'a str,
    ph_seen: bool,
    bare_before: bool,
}

impl<'ast> Visit<'ast> for OrderCheck<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.bare_before {
            return;
        }
        // A statement modifier is written before its condition or list
        // (`say $^b if $b` mentions `$^b` first), so its statement is
        // visited first. It opens no block, so both halves are this scope's.
        match stmt {
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                is_statement_modifier: true,
                ..
            } => {
                walk_stmts(self, then_branch);
                walk_stmts(self, else_branch);
                self.visit_expr(cond);
            }
            Stmt::While {
                cond: header,
                body,
                is_statement_modifier: true,
                ..
            }
            | Stmt::For {
                iterable: header,
                body,
                is_statement_modifier: true,
                ..
            }
            | Stmt::Given {
                topic: header,
                body,
                is_statement_modifier: true,
                ..
            } => {
                walk_stmts(self, body);
                self.visit_expr(header);
            }
            _ => walk_stmt_placeholder_scope(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !self.bare_before {
            walk_expr_placeholder_scope(self, expr);
        }
    }

    // A regex is a code object of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        // A scalar mention, or an assignment target (itself a reference).
        if self.bare_before || !matches!(kind, NameKind::Var | NameKind::AssignTarget) {
            return;
        }
        if name == self.ph_name {
            self.ph_seen = true;
        } else if name == self.bare_name && !self.ph_seen {
            self.bare_before = true;
        }
    }
}

/// Find a bare name that is referenced in `body`'s own placeholder scope
/// but is declared as a placeholder (`$^name`) only in a block STRICTLY
/// NESTED inside `body` — e.g. `{ for 1 { $^b }; say $b }`: the inner `for`
/// block owns `$^b`, so it does not make `$b` this block's parameter, and the
/// outer `$b` was never declared. `own_placeholders` is `body`'s own
/// placeholder list (from `collect_placeholders_shallow`); a name already in
/// it is handled by `bare_precedes_placeholder`'s same-scope ordering check
/// instead.
///
/// Returns the first such bare name found (undecorated, no `$` sigil).
// Cost: O(p * n), p = placeholders nested in `body`, n = size of `body`.
pub(crate) fn bare_name_shadowed_by_nested_placeholder(
    body: &[Stmt],
    own_placeholders: &[String],
) -> Option<String> {
    for ph in crate::ast::collect_placeholders(body) {
        // Only scalar placeholders (`^name`, no `@`/`%`/`&` prefix) share a
        // plain name with a bare `$name` use.
        let Some(bare_name) = ph.strip_prefix('^') else {
            continue;
        };
        if own_placeholders.iter().any(|p| p == &ph) {
            continue;
        }
        let mut probe = BareReference {
            bare_name,
            found: false,
        };
        for stmt in body {
            probe.visit_stmt(stmt);
        }
        if probe.found {
            return Some(bare_name.to_string());
        }
    }
    None
}

/// Whether a bare variable (`$name`, or an assignment target named `name`) is
/// referenced within this block's own placeholder scope.
struct BareReference<'a> {
    bare_name: &'a str,
    found: bool,
}

impl<'ast> Visit<'ast> for BareReference<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if !self.found {
            walk_stmt_placeholder_scope(self, stmt);
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !self.found {
            walk_expr_placeholder_scope(self, expr);
        }
    }

    // A regex is a code object of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if matches!(kind, NameKind::Var | NameKind::AssignTarget) && name == self.bare_name {
            self.found = true;
        }
    }
}
