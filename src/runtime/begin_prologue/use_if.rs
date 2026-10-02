//! The BEGIN-time evaluation of a conditional `use` (ADR-0134 §2.1.6).

use crate::ast::{Expr, Stmt};

/// The unit-level slot a conditional `use` reads its evaluated `:if` value from.
pub(super) const IF_CONDITION_SLOT: &str = "__begin_use_if";

/// `use Foo:if(EXPR)` under the `if` pragma evaluates `EXPR` as a BEGIN-time
/// effect (ADR-0134 §2.1.6). Running in the prologue, it sees lexicals in
/// their static state, so a condition that only a run-time assignment would
/// define is undefined here. That is the rakudo `if` module's compile error.
/// Each conditional `use` stores its value in the same slot just before the
/// `use` reads it, so one slot serves them all.
pub(super) fn if_condition_check(condition: Expr) -> Vec<Stmt> {
    let slot = || Expr::Var(IF_CONDITION_SLOT.to_string());
    vec![
        Stmt::VarDecl {
            name: IF_CONDITION_SLOT.to_string(),
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
                    target: Box::new(slot()),
                    name: crate::symbol::Symbol::intern("defined"),
                    args: vec![],
                    modifier: None,
                    quoted: false,
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
