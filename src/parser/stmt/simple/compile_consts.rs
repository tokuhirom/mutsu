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
        let inherited = s.borrow().last().cloned().unwrap_or_default();
        s.borrow_mut().push(inherited);
    });
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
    "eval-lives-ok",
    "eval-dies-ok",
    "throws-like",
    "fails-like",
    "pass",
    "flunk",
    "use-ok",
    "tap-ok",
];
