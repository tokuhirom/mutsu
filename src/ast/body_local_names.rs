//! The names a routine body binds locally, over the typed AST visitor
//! (ADR-0137).

use super::{Expr, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt};
use crate::compiler::scope_scan::is_scope_declaration;
use crate::regex_tree::RegexNode;
use std::collections::HashSet;

/// Collects every `my`-declared name and every block-parameter name (`for`
/// / `whenever` parameters, an `if`/`with` binding) in a routine's own frame.
struct LocalNames<'a> {
    scalars_only: bool,
    out: &'a mut HashSet<String>,
}

impl LocalNames<'_> {
    fn add(&mut self, name: &str) {
        let bare = name.strip_prefix('\\').unwrap_or(name);
        if bare.is_empty()
            || (self.scalars_only && (bare.starts_with('@') || bare.starts_with('%')))
        {
            return;
        }
        self.out.insert(bare.to_string());
    }
}

impl<'ast> Visit<'ast> for LocalNames<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        // A nested routine or package body is a scope of its own.
        if !is_scope_declaration(stmt) {
            walk_stmt(self, stmt);
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        // A closure is a scope of its own. A bare `{}` value is walked: the
        // parser lowers some condition-position blocks to it.
        if !matches!(
            expr,
            Expr::Lambda { .. } | Expr::AnonSub { .. } | Expr::AnonSubParams { .. }
        ) {
            walk_expr(self, expr);
        }
    }

    // A regex code block is a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if matches!(kind, NameKind::VarDecl | NameKind::BlockParam) {
            self.add(name);
        }
    }
}

/// Collect every `my`-declared lexical name (scalars, arrays, hashes) and
/// every block-parameter name that a body introduces, through the control-flow
/// constructs, phasers and expressions whose code runs in the *same* env scope
/// (`for`/`while`/`loop`/`if`/blocks/`given`/`when`/`gather`/`do`, a `my` in a
/// condition such as `next unless my @x = ...`), but NOT into nested
/// `sub`/method/closure bodies (those are separate scopes).
///
/// Used to seed `CompiledCode::env_only_decls` so the method-dispatch return
/// merge treats a `my @x` declared inside a deferred body (e.g. a `gather` block,
/// stashed in `stmt_pool` and run by-name against the method env) as method-local
/// and does not leak it into a same-named caller lexical across (self-)recursion.
// Cost: O(n), n = size of `stmts`' subtree outside nested routines and closures.
pub(crate) fn collect_all_my_decl_names(stmts: &[Stmt], out: &mut HashSet<String>) {
    let mut v = LocalNames {
        scalars_only: false,
        out,
    };
    for stmt in stmts {
        v.visit_stmt(stmt);
    }
}

/// Collect the scalar names a routine body binds *locally* — `for`/`whenever`
/// pointy parameters, `if`/`with` bindings and every nested `my` declaration,
/// including one in a condition (`if (my $d = ...)`) — so the interpreter's
/// return env merge does not write them back over a same-named *caller*
/// lexical. Without this, a routine that recurses into a same-named `for` loop
/// and early-returns (e.g. Zef's `system-collapse`) leaks its inner loop
/// parameter's last value into the caller, and Text::CSV's `csv()`, whose
/// `if (my $file = %args<file>:delete)` clobbered the caller's `$file`.
/// Scalar names only (matching the caller's scalar-writeback filter); `@`/`%`
/// binders are handled by the Array/Hash writeback path.
// Cost: O(n), n = size of `stmts`' subtree outside nested routines and closures.
pub(crate) fn collect_routine_body_local_names(stmts: &[Stmt], out: &mut HashSet<String>) {
    let mut v = LocalNames {
        scalars_only: true,
        out,
    };
    for stmt in stmts {
        v.visit_stmt(stmt);
    }
}
