//! A code variable a closure names before the block declares it (#18248,
//! ADR-12529 phase 3).
//!
//! In Raku a `my` lexical lives in its scope's pad from the scope's entry, so
//! a closure created before the declaration of `&f` still names that `&f`:
//!
//! ```raku
//! sub t() { my &g = -> { f() }; my &f = -> { 100 }; g() }   # 100
//! ```
//!
//! mutsu captures a closure's free variables when the closure is created, and
//! a `my &f` compiled after it is a fresh binding the capture never saw. The
//! call only worked because a closure frame that missed a name walked on into
//! its *callers* and usually found the creating frame there, which is the
//! dynamic-scope leak ADR-12529 retires.
//!
//! The case that already works is a declaration before the closure and an
//! assignment after it (`my &f; my &g = -> { f() }; &f = -> { 100 }`): the
//! closure captures the binding and sees the later store. So this pass gives
//! the forward case that shape. For a plain `my &f = init` that a closure in
//! an earlier statement of the same statement list calls or names, or a
//! closure in `init` itself does (a recursive `my &encode = -> $v { ...
//! encode($x) ... }`, CBOR::Simple's encoder), it
//! declares `my &f` at the start of the list and turns the declaration
//! statement into the assignment `&f = init`. Only a plain declaration
//! qualifies: no type, trait, binding, `where`, `state`/`our`/`dynamic` or
//! export. The unit's own top-level list is left as it is: its lexicals are
//! the program scope, which a closure reaches without a caller walk.

use crate::ast::{AssignOp, Expr, Stmt};
use crate::ast_visit::{Visit, VisitMut, walk_expr, walk_stmt, walk_stmts_mut};

/// Rewrite every statement list nested in `stmts` (not `stmts` itself, the
/// unit's top level).
// Cost: O(n * d), n = AST nodes, d = forward-declared code variables per list
// (usually 0; each candidate rescans the statements before it).
pub(crate) fn hoist_nested(stmts: &mut [Stmt]) {
    walk_stmts_mut(&mut Hoist, stmts);
}

struct Hoist;

impl VisitMut for Hoist {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        walk_stmts_mut(self, body);
        hoist_list(body);
    }
}

/// The name of a plain `my &name = init` declaration, the only shape the
/// rewrite touches.
// Cost: O(1).
fn plain_code_var_decl(stmt: &Stmt) -> Option<&str> {
    match stmt {
        Stmt::VarDecl {
            name,
            anonymous: false,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            custom_traits,
            has_initializer: true,
            constant: None,
            bind: None,
            where_constraint: None,
            ..
        } if name.starts_with('&') && name.len() > 1 && custom_traits.is_empty() => {
            Some(name.as_str())
        }
        _ => None,
    }
}

// Cost: O(s * c), s = statements, c = candidates in the list.
fn hoist_list(body: &mut Vec<Stmt>) {
    let mut forward: Vec<(usize, String)> = Vec::new();
    for (i, stmt) in body.iter().enumerate() {
        let Some(name) = plain_code_var_decl(stmt) else {
            continue;
        };
        let earlier = &body[..i];
        if declares(earlier, name) {
            continue;
        }
        let mut scan = ClosureNames {
            bare: &name[1..],
            amp: name,
            closure_depth: 0,
            found: false,
        };
        scan.visit_stmts(earlier);
        // The declaration's own initializer can create the closure that names
        // it: a recursive `my &encode = -> $v { ... encode($x) ... }`.
        if !scan.found
            && let Stmt::VarDecl { expr, .. } = stmt
        {
            scan.visit_expr(expr);
        }
        if scan.found {
            forward.push((i, name.to_string()));
        }
    }
    if forward.is_empty() {
        return;
    }
    let mut decls = Vec::with_capacity(forward.len());
    for (i, name) in &forward {
        let Stmt::VarDecl { expr, .. } = &mut body[*i] else {
            continue;
        };
        let init = std::mem::replace(expr, Expr::Literal(crate::value::Value::NIL));
        let mut decl = body[*i].clone();
        if let Stmt::VarDecl {
            has_initializer, ..
        } = &mut decl
        {
            *has_initializer = false;
        }
        decls.push(decl);
        body[*i] = Stmt::Assign {
            name: name.clone(),
            expr: init,
            op: AssignOp::Assign,
            target_is_sigilless: false,
        };
    }
    body.splice(0..0, decls);
}

/// Whether one of `stmts` (at the list's own level) declares `name`.
// Cost: O(s).
fn declares(stmts: &[Stmt], name: &str) -> bool {
    stmts
        .iter()
        .any(|s| matches!(s, Stmt::VarDecl { name: n, .. } if n == name))
}

/// Whether a closure in the visited statements calls `bare(...)` or names
/// `&bare`.
struct ClosureNames<'a> {
    bare: &'a str,
    amp: &'a str,
    closure_depth: usize,
    found: bool,
}

impl<'ast> Visit<'ast> for ClosureNames<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        if self.closure_depth > 0
            && let Stmt::Call { name, .. } = stmt
            && name.with_str(|n| n == self.bare)
        {
            self.found = true;
            return;
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found {
            return;
        }
        match expr {
            Expr::Call { name, .. }
                if self.closure_depth > 0 && name.with_str(|n| n == self.bare) =>
            {
                self.found = true;
            }
            Expr::CodeVar(name)
                if self.closure_depth > 0 && (name == self.bare || name == self.amp) =>
            {
                self.found = true;
            }
            Expr::AnonSub { .. }
            | Expr::AnonSubParams { .. }
            | Expr::Lambda { .. }
            | Expr::Block(_) => {
                self.closure_depth += 1;
                walk_expr(self, expr);
                self.closure_depth -= 1;
            }
            _ => walk_expr(self, expr),
        }
    }
}
