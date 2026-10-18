//! Numeric context on a built-in `IO::Path` type object (`IO::Path.Int`,
//! `IO::Path + 1`): Rakudo rejects it with `X::Parameter::InvalidConcreteness`
//! from `IO::Path.Numeric`'s `:D` invocant. A `:D:`-invocant method called
//! explicitly is refused by its row (`RowFlags::DEFINITE`). Shared by the
//! CallMethod and CallMethodMut dispatch paths.

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
    /// `X::Parameter::InvalidConcreteness` for `Int` on a built-in `IO::Path`
    /// type object, which is numeric context (`Mu.Int` is
    /// `self.Numeric.Int`), or `None` when the call is fine.
    ///
    /// Every other `:D:`-invocant built-in method (`IO::Path.e`,
    /// `X::NYI.throw`) is refused by its row (`RowFlags::DEFINITE`).
    // Cost: O(1).
    pub(super) fn type_object_concreteness_error(
        &self,
        method: &str,
        args: &[Value],
        target: &Value,
    ) -> Option<RuntimeError> {
        let ValueView::Package(type_name) = target.view() else {
            return None;
        };
        if method == "Int" && args.is_empty() {
            return io_path_numeric_concreteness_error(type_name.as_str());
        }
        None
    }
}
