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
/// re-evaluating the source expression.
pub(crate) fn defined_condition(negated: bool, tmp_name: &str, cond_expr: Expr) -> Expr {
    let init = Expr::DoStmt(Box::new(Stmt::VarDecl {
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
    }));
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

/// The topic the parameterless block form's scaffold `given` runs on.
///
/// An lvalue condition topicalizes on the *source* so mutations through `$_`
/// write back to it, and a literal topicalizes on the literal so `$_` keeps
/// `given`'s read-only semantics; anything else uses the once-evaluated temp.
/// Mirrors the routing in `parser::stmt::control::with_stmt` for the case with
/// no pointy parameter -- keep the two in step.
pub(crate) fn body_topic(cond_expr: &Expr, tmp_var: &Expr) -> Expr {
    let is_element_lvalue = matches!(
        cond_expr,
        Expr::Index { target, .. }
            if matches!(
                target.as_ref(),
                Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_)
            )
    );
    let is_lvalue = matches!(
        cond_expr,
        Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_)
    ) || is_element_lvalue;
    if is_lvalue || matches!(cond_expr, Expr::Literal(_)) {
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
