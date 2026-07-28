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

/// Strip the `GLOBAL` pseudo-package from the head of a declared package name.
/// `package GLOBAL::X::Foo` installs `X::Foo`; `GLOBAL` is not part of the
/// composed name.
fn strip_global_prefix(name: &str) -> &str {
    name.strip_prefix("GLOBAL::").unwrap_or(name)
}

/// Push `name` as the enclosing package path while a package-like declarator's
/// body is parsed. The returned guard pops it again, including on the error
/// paths out of the body parser.
#[must_use]
pub(crate) fn push_package_path(name: &str) -> PackagePathGuard {
    PACKAGE_PATH.with(|p| {
        p.borrow_mut().push(strip_global_prefix(name).to_string());
    });
    PackagePathGuard
}

pub(crate) struct PackagePathGuard;

impl Drop for PackagePathGuard {
    fn drop(&mut self) {
        PACKAGE_PATH.with(|p| {
            p.borrow_mut().pop();
        });
    }
}

/// Clear the package path. Called from `reset_user_subs` so a parse aborted
/// mid-body cannot leak a stale prefix into the next parse.
pub(crate) fn reset_package_path() {
    PACKAGE_PATH.with(|p| p.borrow_mut().clear());
}

/// The `::`-joined path of the package-like declarators currently being parsed,
/// or `None` at the top level.
fn current_package_prefix() -> Option<String> {
    PACKAGE_PATH.with(|p| {
        let path = p.borrow();
        if path.is_empty() {
            None
        } else {
            Some(path.join("::"))
        }
    })
}

/// Register a type name exactly as given, without composing it with the
/// enclosing package path. Used for names that are already fully composed
/// (e.g. those harvested from a `use`d module).
pub(crate) fn register_user_type_verbatim(name: &str) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current.user_types.insert(name.to_string());
    });
}

/// Register a user-declared type name (class, role, grammar, enum).
pub(crate) fn register_user_type(name: &str) {
    register_user_type_verbatim(name);
    // A declaration nested inside `package`/`module`/`class`/`role` is installed
    // under its composed name, and stays visible after the enclosing body ends.
    // Register that spelling in the outermost scope so it outlives the body's
    // lexical scope, matching where Raku installs the package-scoped name.
    if let Some(prefix) = current_package_prefix() {
        let composed = format!("{}::{}", prefix, name);
        SCOPES.with(|s| {
            let mut scopes = s.borrow_mut();
            let outermost = scopes
                .first_mut()
                .expect("scope stack should never be empty");
            outermost.user_types.insert(composed);
        });
    }
}

/// Register the name of one value of a user-declared enum.
///
/// Registered in the **outermost** scope, like the composed spelling of a
/// package-nested type: an `enum` declared inside a class or module body
/// installs its values as package-scoped symbols that outlive the body, and a
/// `use`d module's exported enum values must be visible for the rest of the
/// importing file.
pub(crate) fn register_user_enum_value(name: &str) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let outermost = scopes
            .first_mut()
            .expect("scope stack should never be empty");
        outermost.user_enum_values.insert(name.to_string());
    });
}

/// Whether `name` is a value of a user-declared enum — the user-declared twin of
/// [`is_builtin_enum_value`](crate::runtime::utils::is_builtin_enum_value).
///
/// An enum value takes no arguments, so a bare occurrence of it is a *complete*
/// term. That is what lets it stand unparenthesized in a ternary's then-branch
/// (`$t ~~ Blob ?? MYSQL_TYPE_BLOB !! MYSQL_TYPE_STRING`, from
/// `DBDish::mysql`), where a bareword is otherwise assumed to be a listop head
/// that gobbled the `!!`.
pub(crate) fn is_user_declared_enum_value(name: &str) -> bool {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        scopes
            .iter()
            .rev()
            .any(|scope| scope.user_enum_values.contains(name))
    })
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
