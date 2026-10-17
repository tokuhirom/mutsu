//! Built-in methods whose invocant must be concrete (`:D:`), called on a
//! *type object*: `X::NYI.throw`, `IO::Path.e`. Rakudo rejects these with
//! `X::Parameter::InvalidConcreteness` from the invocant's signature binding;
//! without this check mutsu would report "No such method" (the native row only
//! serves instances) or fall through to a generic `Cool`/`Any` method.
//! Shared by the CallMethod and CallMethodMut dispatch paths (a bareword-target
//! method call like `X::NYI.throw` or `IO::Path.e` compiles to CallMethodMut).

use super::*;

/// The built-in `IO::Path` type objects: `IO::Path` and its per-OS subclasses.
/// (`IO::Path::Parts` is a separate class, not a subclass.)
fn is_builtin_io_path_type(name: &str) -> bool {
    matches!(
        name,
        "IO::Path" | "IO::Path::Unix" | "IO::Path::Win32" | "IO::Path::Cygwin" | "IO::Path::QNX"
    )
}

/// `X::Parameter::InvalidConcreteness` for numeric *context* (not an explicit
/// method call) on a built-in `IO::Path` type object: `IO::Path + 1`,
/// `+IO::Path`, `IO::Path * 2`: `IO::Path.Numeric`'s invocant is `:D` (its row
/// is `RowFlags::DEFINITE`, which refuses the explicit method call), see #9629.
// Cost: O(1).
pub(super) fn io_path_numeric_concreteness_error(type_name: &str) -> Option<RuntimeError> {
    if is_builtin_io_path_type(type_name) {
        Some(RuntimeError::parameter_invalid_concreteness(
            "IO::Path", type_name, "Numeric", "", true, true,
        ))
    } else {
        None
    }
}

impl Interpreter {
    /// `X::Parameter::InvalidConcreteness` for a `:D:`-invocant built-in
    /// method called on a type object, or `None` when the call is fine.
    ///
    /// - `fail`/`die`/`throw`/`rethrow`/`resume` (no args) on an Exception
    ///   type object (`X::NYI.throw`).
    ///
    /// - `Int` on a built-in `IO::Path` type object, which is numeric context.
    ///
    /// `IO::Path`'s own `:D` methods (`IO::Path.e`) are refused by their rows
    /// (`RowFlags::DEFINITE`).
    // Cost: O(d), d = the class's parent-chain depth.
    pub(super) fn type_object_concreteness_error(
        &self,
        method: &str,
        args: &[Value],
        target: &Value,
    ) -> Option<RuntimeError> {
        let ValueView::Package(type_name) = target.view() else {
            return None;
        };
        let name = type_name.resolve();
        // `Mu.Int` is `self.Numeric.Int`: numeric context, which a built-in
        // `IO::Path` type object refuses.
        if method == "Int" && args.is_empty() {
            return io_path_numeric_concreteness_error(&name);
        }
        if !matches!(method, "fail" | "die" | "throw" | "rethrow" | "resume") || !args.is_empty() {
            return None;
        }
        let is_exc = name == "Exception"
            || name.starts_with("X::")
            || name.starts_with("CX::")
            || self.class_inherits_from_exception(&name);
        is_exc.then(|| {
            RuntimeError::parameter_invalid_concreteness("Exception", &name, method, "", true, true)
        })
    }
}
