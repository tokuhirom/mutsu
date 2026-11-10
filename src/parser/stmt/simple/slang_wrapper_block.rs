//! The routine a slang's `statementlist` wrapper sees as `$*BLOCK`
//! (ADR-16888 §2.6).
//!
//! A routine declarator announces its name and signature just before it parses
//! the body; the statement list that body starts takes the announcement and hands
//! it to the slang host with the wrapper hook. The lists nested in the body find
//! no announcement and run in a bare block's scope, as rakudo's `<block>` gives
//! them.

use crate::ast::ParamDef;
use crate::rakuast::slang_block_scope::{RoutineKind, routine_scope_node};
use crate::value::Value;
use std::cell::RefCell;

thread_local! {
    static ROUTINE_BODY: RefCell<Option<Value>> = const { RefCell::new(None) };
}

/// Clears an announcement no list took when the body's parse ends, however it
/// ends (a failed parse backtracks past it), so it cannot reach an unrelated list.
pub(in crate::parser) struct RoutineBodyGuard;

impl Drop for RoutineBodyGuard {
    fn drop(&mut self) {
        ROUTINE_BODY.with(|b| b.borrow_mut().take());
    }
}

/// The body about to be parsed belongs to the routine `name` with `params`;
/// hold the guard while it is parsed. Costs one flag test when no slang wraps
/// `statementlist`.
// Cost: O(k) without a wrapper, k = registered wrappers; else O(p), p = parameters.
pub(in crate::parser) fn announce_routine_body(
    kind: RoutineKind,
    name: &str,
    params: &[ParamDef],
) -> RoutineBodyGuard {
    if super::slang_hooks::statementlist_wrapped() {
        let node = routine_scope_node(kind, name, params);
        ROUTINE_BODY.with(|b| *b.borrow_mut() = node);
    }
    RoutineBodyGuard
}

/// The routine announced for the list being entered, if any; clears it so the
/// lists nested in the body do not see it.
// Cost: O(1).
pub(in crate::parser) fn take_routine_body() -> Option<Value> {
    ROUTINE_BODY.with(|b| b.borrow_mut().take())
}
