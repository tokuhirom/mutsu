//! The shared pieces of the `with` / `without` / `orwith` BLOCK desugar.
//!
//! `with X { BODY }` has no dedicated runtime representation: the parser lowers
//! it to the conditional
//!
//! ```text
//! if (my $__with_tmp_N = X).defined { given <topic> { BODY } }
//! ```
//!
//! (the condition negated for `without`), tagging both halves so the source
//! keyword survives -- see `Stmt::If`'s and `Stmt::Given`'s `with_kind`.
//!
//! The RakuAST lowerer has to rebuild exactly that shape when it is handed a
//! hand-written `RakuAST::Statement::With`, so the parts both sides need live
//! here rather than being spelled twice and drifting apart.

use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use std::sync::atomic::{AtomicUsize, Ordering};

static WITH_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// A fresh name for the hidden temp holding the once-evaluated condition.
///
/// The condition must be evaluated exactly once: for `with Failure.new { }`,
/// evaluating it twice would `.defined`-test a different object than the one
/// `$_` receives.
pub(crate) fn next_tmp_name() -> String {
    format!(
        "__with_tmp_{}",
        WITH_COUNTER.fetch_add(1, Ordering::Relaxed)
    )
}

/// `(my $tmp = COND).defined`, negated for `without`.
///
/// The `DoStmt(VarDecl)` declares the temp in the current scope and evaluates
/// to the condition's value, so the body can topicalize on the temp instead of
/// re-evaluating the source expression. For an lvalue condition, the temp is a
/// binding to that lvalue: this preserves `with $x`/`with @a` writeback while
/// still evaluating an effectful subscript only once.
pub(crate) fn defined_condition(negated: bool, tmp_name: &str, cond_expr: Expr) -> Expr {
    let decl = Stmt::VarDecl {
        anonymous: false,
        name: tmp_name.to_string(),
        expr: cond_expr,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        has_initializer: false,
        constant: None,
        bind: None,
        where_constraint: None,
    };
    let init = Expr::DoStmt(Box::new(decl));
    let defined = Expr::MethodCall {
        target: Box::new(init),
        name: Symbol::intern("defined"),
        args: Vec::new(),
        modifier: None,
        quoted: false,
        sugar: false,
    };
    if negated {
        Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(defined),
            word: false,
        }
    } else {
        defined
    }
}

/// Recover the simple element source from the hidden condition used by a
/// `with`/`without` scaffold. The condition evaluates the source once; the
/// compiler uses this metadata to tag that already-evaluated element for the
/// topicalizing `given`, instead of compiling the subscript a second time.
pub(crate) fn condition_element_source(cond: &Expr) -> Option<(String, Vec<bool>)> {
    let cond = match cond {
        Expr::Unary { expr, .. } => expr.as_ref(),
        other => other,
    };
    let Expr::MethodCall { target, name, .. } = cond else {
        return None;
    };
    if name.resolve() != "defined" {
        return None;
    }
    let Expr::DoStmt(stmt) = target.as_ref() else {
        return None;
    };
    let decl = match stmt.as_ref() {
        Stmt::VarDecl { name, expr, .. } if name.starts_with("__with_tmp_") => expr,
        _ => return None,
    };
    let mut path = Vec::new();
    let root = decl.index_path(&mut path);
    let container = element_source_root_key(root)?;
    let positionals = path.iter().map(|(_, positional)| *positional).collect();
    Some((container, positionals))
}

/// The container key a `with`/`without` element source is recorded under: a
/// plain lexical variable, not an attribute (`$!x`, `$.x`) or a compile-time
/// `%?` hash, whose element writes do not go through a named lexical. The
/// compiler's matching side (`compile_expr_index`) must agree, so both call
/// this.
// Cost: O(|name|).
pub(crate) fn element_source_root_key(root: &Expr) -> Option<String> {
    match root {
        Expr::Var(name) | Expr::ArrayVar(name) if name.starts_with(['!', '.']) => None,
        Expr::HashVar(name) if name.starts_with(['!', '.', '?']) => None,
        other => other.container_var_key(),
    }
}
