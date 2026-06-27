use super::*;

pub(crate) fn set_attributes_pragma(smiley: &str) {
    ATTRIBUTES_PRAGMA.with(|v| {
        *v.borrow_mut() = smiley.to_string();
    });
}

pub(crate) fn current_attributes_pragma() -> String {
    ATTRIBUTES_PRAGMA.with(|v| v.borrow().clone())
}

/// Set operator sub names to pre-register after scope reset (for EVAL).
pub(crate) fn set_eval_operator_preseed(names: Vec<String>) {
    EVAL_OPERATOR_PRESEED.with(|preseed| {
        *preseed.borrow_mut() = names;
    });
}

pub(crate) fn set_eval_operator_assoc_preseed(assoc: HashMap<String, String>) {
    EVAL_OPERATOR_ASSOC_PRESEED.with(|preseed| {
        *preseed.borrow_mut() = assoc;
    });
}

pub(crate) fn set_eval_imported_function_preseed(names: Vec<String>) {
    EVAL_IMPORTED_FUNCTION_PRESEED.with(|preseed| {
        *preseed.borrow_mut() = names;
    });
}

pub(crate) fn set_eval_user_sub_preseed(names: Vec<String>) {
    EVAL_USER_SUB_PRESEED.with(|preseed| {
        *preseed.borrow_mut() = names;
    });
}

/// Check if a name was declared as a user sub in any enclosing scope.
pub(crate) fn is_user_declared_sub(name: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        scopes
            .iter()
            .rev()
            .any(|scope| scope.user_subs.contains(name))
    })
}

/// Register a user-declared type name (class, role, grammar, enum).
pub(crate) fn register_user_type(name: &str) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current.user_types.insert(name.to_string());
    });
}

/// Check if a name was declared as a user type (class, role, grammar, enum)
/// in any enclosing scope.
pub(crate) fn is_user_declared_type(name: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        scopes
            .iter()
            .rev()
            .any(|scope| scope.user_types.contains(name))
    })
}
