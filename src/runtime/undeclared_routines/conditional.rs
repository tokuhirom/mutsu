//! The undeclared-routine check across conditional `use`s and import-free
//! pragmas (ADR-0134 §2.1.6, #10331).
//!
//! A unit that `use`s a module is normally not judged at all: the walker
//! cannot see what the module imports. Two kinds of `use` are let through.
//!
//! - A pragma that imports no routines (`use if`, `use lib`, `use strict`,
//!   ...) changes nothing the check depends on.
//! - A top-level conditional `use Foo:if(EXPR)` imports either the names the
//!   parse-time scan registered for it (`Stmt::Use::if_imports`) or, when
//!   `EXPR` is False, nothing at all. `EXPR` is a BEGIN-time value: it is
//!   known only once the prologue has run, after this check. So the names
//!   the statement registered do not count as declared here, and a call left
//!   unexplained becomes a guard placed right after the prologue: it raises
//!   the "Undeclared routine" error unless one of the unit's conditional
//!   `use`s was loaded. Any of them guards every call, because the parse-time
//!   scan of a module need not list every name it imports (a `sub EXPORT`
//!   hook); the check only ever errs on the side of not reporting.

use crate::ast::{Expr, Stmt, UndeclaredRoutineCall};

/// A top-level conditional `use` whose condition the BEGIN prologue
/// evaluated into `slot`.
pub(super) struct ConditionalUse {
    pub(super) slot: String,
    pub(super) imports: Vec<String>,
}

/// The conditional `use` that `stmt` is, when the check can defer to its
/// condition: the prologue evaluated the condition into a slot, and the `use`
/// passes no arguments to the module's `sub EXPORT` (which could compute any
/// name from them).
// Cost: O(i), i = names the `use` imported at parse time.
pub(super) fn conditional_use(stmt: &Stmt) -> Option<ConditionalUse> {
    let Stmt::Use {
        condition: Some(condition),
        arg: None,
        if_imports,
        ..
    } = stmt
    else {
        return None;
    };
    let slot = crate::runtime::begin_prologue::if_condition_slot(condition)?;
    Some(ConditionalUse {
        slot: slot.to_string(),
        imports: if_imports.clone(),
    })
}

/// Whether a `use` of `module` imports no routines, so the check can go on
/// judging the unit. These are the pragmas: `if` (whose `sub EXPORT` returns
/// an empty map, after enabling the `:if` adverb), `lib`, the `MONKEY`
/// family, and the positional pragmas mutsu applies as run-time state, except
/// `experimental`, whose `:macros` adds a declarator, and a language version
/// (`use v6.e.PREVIEW` brings in routines such as `nano`).
// Cost: O(1).
pub(super) fn imports_no_routines(module: &str) -> bool {
    matches!(module, "if" | "lib")
        || module.starts_with("MONKEY")
        || (module != "experimental"
            && !module.starts_with("v6")
            && crate::runtime::begin_prologue::is_known_pragma_name(module))
}

/// The guard raising `call`'s error unless one of the conditions in `slots`
/// was true: `if !slot0 { if !slot1 { ... raise } }`.
// Cost: O(s), s = number of slots.
pub(super) fn guard(call: UndeclaredRoutineCall, slots: &[String]) -> Stmt {
    slots
        .iter()
        .rev()
        .fold(Stmt::UndeclaredRoutine(Box::new(call)), |inner, slot| {
            Stmt::If {
                cond: Expr::Unary {
                    op: crate::token_kind::TokenKind::Bang,
                    expr: Box::new(Expr::Var(slot.clone())), word: false,
                },
                then_branch: vec![inner],
                else_branch: vec![],
                binding_var: None,
                is_statement_modifier: false,
                is_unless: false,
                with_kind: None,
            }
        })
}
