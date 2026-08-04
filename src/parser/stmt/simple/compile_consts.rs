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
        // Pending anonymous-state declarations belong to the scope that minted
        // them; a nested block starts with none of its own.
        inherited.anon_states = Vec::new();
        s.borrow_mut().push(inherited);
    });
}

/// Record an anonymous state variable (`$`) as minted directly in the current
/// scope, so `take_anon_state_decls` can turn it into an implicit `state`
/// declaration when the enclosing block's statement list is complete.
pub(crate) fn record_anon_state_name(name: &str) {
    SCOPES.with(|s| {
        if let Some(current) = s.borrow_mut().last_mut() {
            current.anon_states.push(name.to_string());
        }
    });
}

/// How many anonymous-state names the current scope has pending, so a caller can
/// tell which ones a nested parse added (see `current_scope_anon_state_names_from`).
pub(crate) fn current_scope_anon_state_count() -> usize {
    SCOPES.with(|s| s.borrow().last().map_or(0, |c| c.anon_states.len()))
}

/// The current scope's pending anonymous-state names from index `from` on.
pub(crate) fn current_scope_anon_state_names_from(from: usize) -> Vec<String> {
    SCOPES.with(|s| {
        s.borrow()
            .last()
            .map(|c| c.anon_states.get(from..).unwrap_or_default().to_vec())
            .unwrap_or_default()
    })
}

/// Take the current scope's pending anonymous-state names and turn each into an
/// implicit `state $__ANON_STATE_<id>__;` declaration.
///
/// In Raku a bare `$` IS a `state` variable of the block it appears in, so the
/// cell belongs to that block's *clone*: re-entering the enclosing scope
/// re-clones the block literal and the counter restarts (`for ^2 { say (map {
/// ++$ }, ^3).join(",") }` prints `1,2,3` twice), while iterations of one
/// execution share it (`for ^3 { print ++$ }` prints `1 2 3`). Declaring the
/// minted name as a real `state` hands all of that to the existing state
/// machinery — per-closure `scoped_state_key`, and `reset_state_locals_in_range`
/// for inline loop bodies — instead of a parallel mechanism that has to
/// re-derive clone identity from the routine stack.
pub(crate) fn take_anon_state_decls() -> Vec<crate::ast::Stmt> {
    let names = SCOPES.with(|s| {
        s.borrow_mut()
            .last_mut()
            .map(|current| std::mem::take(&mut current.anon_states))
            .unwrap_or_default()
    });
    names
        .into_iter()
        .map(|name| crate::ast::Stmt::VarDecl {
            name,
            expr: crate::ast::Expr::Literal(crate::value::Value::NIL),
            type_constraint: None,
            is_state: true,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: None,
        })
        .collect()
}

/// Prepend this scope's implicit anonymous-state `state` declarations to the
/// block's statement list. Call immediately before the matching `pop_scope`.
pub(crate) fn prepend_anon_state_decls(stmts: &mut Vec<crate::ast::Stmt>) {
    let decls = take_anon_state_decls();
    if !decls.is_empty() {
        stmts.splice(0..0, decls);
    }
}

/// [`prepend_anon_state_decls`] applied to a block parser's result, for the call
/// sites that hold a `PResult<'_, Vec<Stmt>>` between `push_scope` and
/// `pop_scope`. A failed parse drops the pending names with the popped scope.
pub(crate) fn finish_block_anon_states<T, E>(result: &mut Result<(T, Vec<crate::ast::Stmt>), E>) {
    if let Ok((_, stmts)) = result {
        prepend_anon_state_decls(stmts);
    }
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
