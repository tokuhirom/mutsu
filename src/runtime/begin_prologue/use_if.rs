//! The BEGIN-time evaluation of a conditional `use` (ADR-0134 §2.1.6).

use crate::ast::{Expr, Stmt};

/// The prefix of the unit-level slots conditional `use`s read their evaluated
/// `:if` values from. Each conditional `use` gets its own slot, numbered in
/// prologue order, so the value stays readable after the prologue: the
/// undeclared-routine check of #10331 reads it there.
const IF_CONDITION_SLOT: &str = "__begin_use_if";

/// The name of the next conditional `use`'s slot in `prologue`.
// Cost: O(p), p = statements in the prologue so far.
pub(super) fn next_if_condition_slot(prologue: &[Stmt]) -> String {
    let taken = prologue
        .iter()
        .filter(|stmt| matches!(stmt, Stmt::VarDecl { name, .. } if name.starts_with(IF_CONDITION_SLOT)))
        .count();
    format!("{IF_CONDITION_SLOT}_{taken}")
}

/// The slot a prologue-evaluated conditional `use` reads (its rewritten
/// `condition`), or `None` for a condition the prologue did not evaluate.
// Cost: O(1).
pub(crate) fn if_condition_slot(condition: &Expr) -> Option<&str> {
    match condition {
        Expr::Var(name) if name.starts_with(IF_CONDITION_SLOT) => Some(name),
        _ => None,
    }
}

/// `use Foo:if(EXPR)` under the `if` pragma evaluates `EXPR` as a BEGIN-time
/// effect (ADR-0134 §2.1.6). Running in the prologue, it sees lexicals in
/// their static state, so a condition that only a run-time assignment would
/// define is undefined here. That is the rakudo `if` module's compile error.
/// The value is stored in `slot`, which the `use` then reads.
pub(super) fn if_condition_check(condition: Expr, slot: &str) -> Vec<Stmt> {
    let slot_var = || Expr::Var(slot.to_string());
    vec![
        Stmt::VarDecl {
            name: slot.to_string(),
            expr: condition,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: vec![],
            custom_traits: vec![("__has_initializer".to_string(), None)],
            where_constraint: None,
        },
        Stmt::If {
            cond: Expr::Unary {
                op: crate::token_kind::TokenKind::Bang,
                expr: Box::new(Expr::MethodCall {
                    target: Box::new(slot_var()),
                    name: crate::symbol::Symbol::intern("defined"),
                    args: vec![],
                    modifier: None,
                    quoted: false, sugar: false,
                }),
            },
            then_branch: vec![Stmt::Die(Expr::Literal(crate::value::Value::str(
                "Did not provide compile-time-value for :if adverb in use statement".to_string(),
            )))],
            else_branch: vec![],
            binding_var: None,
            is_statement_modifier: true,
            is_unless: false,
            with_kind: None,
        },
    ]
}
