//! A slang's `routine-declarator` after a `multi` / `proto` / `only` keyword
//! (#16888, EEL 0.3's `multi pattern` / `proto pattern`).
//!
//! rakudo's `multi-declarator` sets `$*MULTINESS` and parses `<declarator>`
//! after its keyword, so a slang's `routine-declarator:sym<...>` production
//! competes there exactly as it does at the start of a statement. The core
//! `method-def` / `routine-def` the production calls then declares a candidate,
//! a dispatcher or an `only` routine by that multiness. The Rust parser keeps the
//! multiness here for the duration of the production; the core shims take it.

use super::*;
use std::cell::Cell;

/// The declarator keyword in force for a slang routine production.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Multiness {
    Multi,
    Proto,
    Only,
}

thread_local! {
    static MULTINESS: Cell<Option<Multiness>> = const { Cell::new(None) };
}

/// Restores the multiness in force before a production ran, however it ends.
struct MultinessGuard(Option<Multiness>);

impl Drop for MultinessGuard {
    fn drop(&mut self) {
        MULTINESS.with(|m| m.set(self.0));
    }
}

/// The multiness the next core `method-def` / `routine-def` declares with;
/// cleared so the routines nested in its body are not candidates too.
// Cost: O(1).
pub(crate) fn take_slang_multiness() -> Option<Multiness> {
    MULTINESS.with(|m| m.take())
}

/// The multiness in force, as a memo key component: a hook answer at one
/// position differs by the keyword in front of it.
// Cost: O(1).
pub(super) fn slang_multiness_key() -> u64 {
    match MULTINESS.with(|m| m.get()) {
        None => 0,
        Some(Multiness::Multi) => 1,
        Some(Multiness::Proto) => 2,
        Some(Multiness::Only) => 3,
    }
}

/// Try the slang's `routine-declarator` productions after a `multi`, `proto` or
/// `only` keyword at the start of `input`, ahead of the built-in declarators.
/// `None` when no keyword is there or no production matches.
// Cost: O(1) with no hook registered; otherwise as the slang's production.
pub(in crate::parser) fn multi_declarator_hook(input: &str) -> Option<PResult<'_, Stmt>> {
    if !super::slang_hooks::has_routine_declarator_hooks() {
        return None;
    }
    let (after, multiness) = [
        ("multi", Multiness::Multi),
        ("proto", Multiness::Proto),
        ("only", Multiness::Only),
    ]
    .into_iter()
    .find_map(|(kw, m)| crate::parser::stmt::keyword(kw, input).map(|r| (r, m)))?;
    let (after, _) = crate::parser::helpers::ws1(after).ok()?;
    let _guard = MultinessGuard(MULTINESS.with(|m| m.replace(Some(multiness))));
    super::slang_hooks::routine_declarator_hook(after)
}
