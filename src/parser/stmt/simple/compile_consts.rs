use super::*;

/// Register a compile-time constant value (for resolving `<<$x>>` in operator names).
pub(crate) fn register_compile_time_constant(name: &str, value: String) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current
            .compile_time_constants
            .insert(name.to_string(), value);
    });
}

/// Look up a compile-time constant by name.
pub(crate) fn lookup_compile_time_constant(name: &str) -> Option<String> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        for scope in scopes.iter().rev() {
            if let Some(value) = scope.compile_time_constants.get(name) {
                return Some(value.clone());
            }
        }
        None
    })
}

/// Check if a name was declared as a test assertion sub in any enclosing scope.
pub(crate) fn is_user_test_assertion_sub(name: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        scopes
            .iter()
            .rev()
            .any(|scope| scope.test_assertion_subs.contains(name))
    })
}

/// Check if the callable should carry test assertion caller-site metadata.
pub(crate) fn is_test_assertion_callable(name: &str) -> bool {
    TEST_ASSERTION_EXPORTS.contains(&name) || is_user_test_assertion_sub(name)
}

/// Push a new lexical scope (called when entering a `{ }` block).
pub(crate) fn push_scope() {
    SCOPES.with(|s| {
        let mut inherited = s.borrow().last().cloned().unwrap_or_default();
        // Routine-body-ness is a property of ONE scope, not of everything
        // beneath it — a `map { }` block inside a sub body is a per-call block,
        // not part of the routine's own (cloned-once) frame.
        inherited.is_routine_body = false;
        s.borrow_mut().push(inherited);
    });
}

/// Mark the current (innermost) scope as a routine body. Called by the
/// routine-declaration parsers right after they push the body's scope.
pub(crate) fn mark_current_scope_routine_body() {
    SCOPES.with(|s| {
        if let Some(current) = s.borrow_mut().last_mut() {
            current.is_routine_body = true;
        }
    });
}

/// Whether an anonymous state variable (`$++` / `++$`) minted at the current
/// parse position is PER-CALL: lexically inside a nested block that is itself
/// lexically inside a routine. Such a `$` belongs to a block clone the routine
/// re-makes on every call, so its counter restarts per call; a `$` directly in
/// a routine body (cloned once, at registration) or anywhere at the mainline
/// keeps counting. The classification is baked into the variable's NAME
/// (`__ANON_STATE_PC_<id>__`), so every later compilation of the same AST —
/// including runtime re-compiles of block bodies — agrees on it by
/// construction (the key-stability hazard that sank the per-chunk attempt,
/// PR #5885).
pub(crate) fn anon_state_is_per_call() -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        for (blocks_skipped, scope) in scopes.iter().rev().enumerate() {
            if scope.is_routine_body {
                return blocks_skipped > 0;
            }
        }
        false
    })
}

/// Pop the current lexical scope (called when leaving a `{ }` block).
pub(crate) fn pop_scope() {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        if scopes.len() > 1 {
            scopes.pop();
        }
    });
}

/// Enable the `no worries` lexical pragma in the current scope, suppressing
/// compiler "Potential difficulties" warnings for the rest of this scope and
/// any nested scopes.
pub(crate) fn suppress_worries() {
    SCOPES.with(|s| {
        if let Some(current) = s.borrow_mut().last_mut() {
            current.worries_suppressed = true;
        }
    });
}

/// Returns true when `no worries` is in effect in the current (innermost) scope.
pub(crate) fn worries_suppressed() -> bool {
    SCOPES.with(|s| {
        s.borrow()
            .last()
            .map(|scope| scope.worries_suppressed)
            .unwrap_or(false)
    })
}

/// Check if a function name was registered via `use` module import.
/// Searches all scopes from innermost to outermost.
pub(crate) fn is_imported_function(name: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        scopes
            .iter()
            .rev()
            .any(|scope| scope.imported_functions.contains(name))
    })
}

const TEST_ASSERTION_EXPORTS: &[&str] = &[
    "ok",
    "nok",
    "is",
    "isnt",
    "is-deeply",
    "is-approx",
    "cmp-ok",
    "isa-ok",
    "does-ok",
    "can-ok",
    "lives-ok",
    "dies-ok",
    "exits-ok",
    "eval-lives-ok",
    "eval-dies-ok",
    "throws-like",
    "fails-like",
    "pass",
    "flunk",
    "use-ok",
];
