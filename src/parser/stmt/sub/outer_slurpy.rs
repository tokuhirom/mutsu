//! Which implicit-slurpy spellings (`@_` / `%_`) an enclosing routine has
//! declared as real lexicals while its body is being parsed.
//!
//! A `@_` / `%_` inside a block is a placeholder only when no enclosing scope
//! declares that name: `sub f(@a, *%_) { @a.map(-> $x { to-json $x, |%_ }) }`
//! reads `f`'s `%_` (JSON::Fast::Hyper), and a method's implicit `*%_` is
//! visible the same way. The pointy-block placeholder check runs at parse time,
//! so the parser keeps this small stack of what the routines around the current
//! position declare.

use std::cell::RefCell;

use crate::ast::ParamDef;

thread_local! {
    static OUTER_IMPLICIT_SLURPIES: RefCell<Vec<&'static str>> = const { RefCell::new(Vec::new()) };
}

/// Pops what [`enter_routine_body`] pushed when the routine body ends.
pub(crate) struct RoutineBodyScope {
    depth: usize,
}

impl Drop for RoutineBodyScope {
    fn drop(&mut self) {
        OUTER_IMPLICIT_SLURPIES.with(|s| s.borrow_mut().truncate(self.depth));
    }
}

/// Record the implicit-slurpy names a routine body about to be parsed makes
/// visible: each `@_` / `%_` its signature declares, plus `%_` for a method
/// (rakudo gives every method an implicit `*%_`, never a `*@_`).
// Cost: O(p), p = parameters of the routine.
pub(crate) fn enter_routine_body(param_defs: &[ParamDef], is_method: bool) -> RoutineBodyScope {
    OUTER_IMPLICIT_SLURPIES.with(|s| {
        let mut s = s.borrow_mut();
        let depth = s.len();
        for pd in param_defs {
            match pd.name.as_str() {
                "@_" => s.push("@_"),
                "%_" => s.push("%_"),
                _ => {}
            }
        }
        if is_method {
            s.push("%_");
        }
        RoutineBodyScope { depth }
    })
}

/// Whether `name` is `@_` / `%_` declared by a routine enclosing the current
/// parse position.
// Cost: O(d), d = implicit slurpies declared by enclosing routines (tiny).
pub(crate) fn outer_declares_implicit_slurpy(name: &str) -> bool {
    matches!(name, "@_" | "%_")
        && OUTER_IMPLICIT_SLURPIES.with(|s| s.borrow().iter().any(|n| *n == name))
}
