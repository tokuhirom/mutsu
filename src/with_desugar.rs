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

use crate::ast::{Expr, GivenWithKind, Stmt, WithBlockKind};
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
        name: tmp_name.to_string(),
        expr: cond_expr,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    };
    let init = Expr::DoStmt(Box::new(decl));
    let defined = Expr::MethodCall {
        target: Box::new(init),
        name: Symbol::intern("defined"),
        args: Vec::new(),
        modifier: None,
        quoted: false,
    };
    if negated {
        Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(defined),
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
    let mut positionals = Vec::new();
    let root = flatten_index_source(decl, &mut positionals)?;
    let container = match root {
        Expr::Var(name) if !name.starts_with(['!', '.']) => name.clone(),
        Expr::ArrayVar(name) if !name.starts_with(['!', '.']) => format!("@{name}"),
        Expr::HashVar(name) if !name.starts_with(['!', '.', '?']) => format!("%{name}"),
        _ => return None,
    };
    Some((container, positionals))
}

/// Flatten a chained index expression to its root and record the source-order
/// positional/hash nature of each subscript. The expressions themselves stay
/// in the AST; the compiler uses this shape to evaluate them once before the
/// VM records the complete lvalue path.
fn flatten_index_source<'a>(expr: &'a Expr, positionals: &mut Vec<bool>) -> Option<&'a Expr> {
    match expr {
        Expr::Index {
            target,
            is_positional,
            index: _,
        } => {
            let root = flatten_index_source(target, positionals)?;
            positionals.push(*is_positional);
            Some(root)
        }
        _ if positionals.is_empty() => Some(expr),
        _ => None,
    }
}

/// The topic the parameterless block form's scaffold `given` runs on.
///
/// An lvalue condition is represented by a bound once-evaluated temp, so it
/// topicalizes on that temp and mutations through `$_` still write back to the
/// source. A literal topicalizes on the literal so `$_` keeps `given`'s
/// read-only semantics; anything else uses the once-evaluated temp.
/// Mirrors the routing in `parser::stmt::control::with_stmt` for the case with
/// no pointy parameter -- keep the two in step.
pub(crate) fn body_topic(cond_expr: &Expr, tmp_var: &Expr) -> Expr {
    if matches!(
        cond_expr,
        Expr::Literal(_) | Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_)
    ) {
        cond_expr.clone()
    } else {
        tmp_var.clone()
    }
}

/// The topicalizing `given` a `with`-family block body runs under, tagged so
/// the RakuAST converter can render it as the block's `implicit-topic` flag
/// rather than as a source-level `given`.
pub(crate) fn topic_given(topic: Expr, body: Vec<Stmt>) -> Stmt {
    Stmt::Given {
        topic,
        body,
        is_statement_modifier: false,
        with_kind: Some(GivenWithKind::BlockTopic),
    }
}

/// The conditional a `with` / `without` block form lowers to, given an already
/// built `else` branch (an `orwith`/`elsif` chain, a topicalized `else` block,
/// or nothing).
pub(crate) fn with_conditional(
    kind: WithBlockKind,
    tmp_name: &str,
    cond_expr: Expr,
    body: Vec<Stmt>,
    else_branch: Vec<Stmt>,
) -> Stmt {
    let tmp_var = Expr::Var(tmp_name.to_string());
    let topic = body_topic(&cond_expr, &tmp_var);
    Stmt::If {
        cond: defined_condition(kind == WithBlockKind::Without, tmp_name, cond_expr),
        then_branch: vec![topic_given(topic, body)],
        else_branch,
        binding_var: None,
        is_statement_modifier: false,
        is_unless: false,
        with_kind: Some(kind),
    }
}

/// The conditional one `orwith` clause lowers to: like `with`, but the
/// condition is tested directly (the preceding clause already evaluated it into
/// its own temp, and an `orwith` expression is evaluated only when reached).
pub(crate) fn orwith_conditional(cond_expr: Expr, body: Vec<Stmt>, else_branch: Vec<Stmt>) -> Stmt {
    let cond = Expr::MethodCall {
        target: Box::new(cond_expr.clone()),
        name: Symbol::intern("defined"),
        args: Vec::new(),
        modifier: None,
        quoted: false,
    };
    Stmt::If {
        cond,
        then_branch: vec![topic_given(cond_expr, body)],
        else_branch,
        binding_var: None,
        is_statement_modifier: false,
        is_unless: false,
        with_kind: Some(WithBlockKind::Orwith),
    }
}
