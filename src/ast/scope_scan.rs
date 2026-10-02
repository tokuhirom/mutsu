//! Scope boundaries shared by the compile-time AST analyses (ADR-0137).
//!
//! Many analyses ask a question about the code that runs in *one* block's own
//! scope: "does this block declare a `state`?", "can a `when` in it reach this
//! block's succeed barrier?", "does it declare a block-local `my`?". They walk
//! the typed visitor ([`crate::ast_visit::Visit`]) and stop where a construct
//! opens a scope of its own. The two predicates and the statement walk here
//! are that stop, written once, so every such analysis agrees on where a
//! scope ends.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_stmt, walk_stmts};

/// An expression that is a code object of its own: a pointy block, a `sub {}`
/// or a bare block value. Its body runs when the object is called, under its
/// own frame, `$_`, `state` and `let`/`temp` scope.
// Cost: O(1).
pub(crate) fn is_code_object(e: &Expr) -> bool {
    matches!(
        e,
        Expr::Lambda { .. } | Expr::AnonSub { .. } | Expr::AnonSubParams { .. } | Expr::Block(_)
    )
}

/// An expression whose body runs in a block of its own: a code object (see
/// [`is_code_object`]), `do {}`, `try {}`, `gather {}`, `once {}` or a phaser
/// used as a value.
// Cost: O(1).
pub(crate) fn opens_own_scope(e: &Expr) -> bool {
    is_code_object(e)
        || matches!(
            e,
            Expr::DoBlock { .. }
                | Expr::Try { .. }
                | Expr::Gather(_)
                | Expr::Once { .. }
                | Expr::PhaserExpr { .. }
        )
}

/// A declaration whose body is a routine or package scope of its own.
// Cost: O(1).
pub(crate) fn is_scope_declaration(s: &Stmt) -> bool {
    matches!(
        s,
        Stmt::SubDecl { .. }
            | Stmt::MethodDecl { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ProtoDecl { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::Package { .. }
            | Stmt::PackageRuntimeBody { .. }
            | Stmt::AugmentClass { .. }
    )
}

/// Walks only the header of a control statement — the parts that run in the
/// enclosing scope whatever form the statement takes: an `if`/`while`
/// condition, a `for` iterable, a `given` topic, a `when` matcher, a C-style
/// loop's init/cond/step, a `whenever` supply. Not the body. Any other
/// statement has no header and is not walked.
// Cost: O(n), n = size of the header.
pub(crate) fn walk_control_header<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, stmt: &'ast Stmt) {
    match stmt {
        Stmt::If { cond, .. }
        | Stmt::While { cond, .. }
        | Stmt::Given { topic: cond, .. }
        | Stmt::When { cond, .. } => v.visit_expr(cond),
        Stmt::For { iterable, .. } => v.visit_expr(iterable),
        Stmt::Whenever { supply, .. } => v.visit_expr(supply),
        Stmt::Loop {
            init, cond, step, ..
        } => {
            if let Some(init) = init {
                v.visit_stmt(init);
            }
            for e in [cond, step].into_iter().flatten() {
                v.visit_expr(e);
            }
        }
        _ => {}
    }
}

/// Walks the parts of `stmt` that run in the enclosing block's own scope.
///
/// A control statement's header (an `if` condition, a loop's iterable or
/// condition, a C-style loop's init/cond/step, a `given` topic, a `when`
/// matcher, a `whenever` supply) runs in the enclosing scope; its body is a
/// block of its own and is not entered — except for a statement-modifier form
/// (`... if $c`, `... for @a`, `... given $x`), which opens no block. Blocks,
/// phasers, `CATCH`/`CONTROL`, scope declarations and the compile-time or
/// thunked declarations (`enum`, `subset`, `has`, `use`) are not entered at
/// all. Every other statement is walked in full.
// Cost: O(n), n = size of the part of `stmt` that runs in the enclosing scope.
pub(crate) fn walk_stmt_own_scope<'ast, V: Visit<'ast> + ?Sized>(v: &mut V, stmt: &'ast Stmt) {
    match stmt {
        Stmt::If {
            then_branch,
            else_branch,
            is_statement_modifier,
            ..
        } => {
            walk_control_header(v, stmt);
            if *is_statement_modifier {
                walk_stmts(v, then_branch);
                walk_stmts(v, else_branch);
            }
        }
        Stmt::While {
            body,
            is_statement_modifier,
            ..
        }
        | Stmt::For {
            body,
            is_statement_modifier,
            ..
        }
        | Stmt::Given {
            body,
            is_statement_modifier,
            ..
        }
        | Stmt::When {
            body,
            is_statement_modifier,
            ..
        } => {
            walk_control_header(v, stmt);
            if *is_statement_modifier {
                walk_stmts(v, body);
            }
        }
        Stmt::Loop { .. } | Stmt::Whenever { .. } => walk_control_header(v, stmt),
        Stmt::Label { stmt, .. } => v.visit_stmt(stmt),
        // Bodies that are blocks of their own.
        Stmt::Block(_)
        | Stmt::React { .. }
        | Stmt::Default(_)
        | Stmt::Catch(_)
        | Stmt::Control(_)
        | Stmt::Phaser { .. }
        | Stmt::DocPhaser(_)
        | Stmt::NestedMethodCapture { .. } => {}
        // Compile-time declarations, and the thunks they carry (a `subset`
        // predicate, an attribute default), do not run in this scope.
        Stmt::EnumDecl { .. }
        | Stmt::SubsetDecl { .. }
        | Stmt::HasDecl { .. }
        | Stmt::Use { .. }
        | Stmt::No { .. }
        | Stmt::Need { .. }
        | Stmt::Import { .. } => {}
        s if is_scope_declaration(s) => {}
        _ => walk_stmt(v, stmt),
    }
}
