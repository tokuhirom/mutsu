//! Built-in methods whose invocant must be concrete (`:D:`), called on a
//! *type object*: `X::NYI.throw`, `IO::Path.e`. Rakudo rejects these with
//! `X::Parameter::InvalidConcreteness` from the invocant's signature binding;
//! without this check mutsu would report "No such method" (the native row only
//! serves instances) or fall through to a generic `Cool`/`Any` method.
//! Shared by the CallMethod and CallMethodMut dispatch paths (a bareword-target
//! method call like `X::NYI.throw` or `IO::Path.e` compiles to CallMethodMut).

use super::*;

/// `IO::Path` methods declared `IO::Path:D:` in Rakudo — the ones for which
/// `raku -e 'IO::Path.<name>'` raises `X::Parameter::InvalidConcreteness`.
/// Deliberately excluded: the `multi` ones (`absolute`, `extension`, `parent`,
/// `slurp`, `spurt`, `chdir`, `dir`: `X::Multi::NoMatch`), the ones that die
/// on an attribute lookup (`X::AdHoc`), and the ones that answer on the type
/// object (`IO`, `Bool`, `Str`, `gist`, `raku`).
const IO_PATH_CONCRETE_METHODS: &[&str] = &[
    "accessed", "add", "basename", "changed", "cleanup", "comb", "d", "dirname", "e", "f", "Int",
    "l", "lines", "mkdir", "mode", "modified", "move", "Numeric", "open", "pred", "r", "relative",
    "resolve", "rmdir", "rw", "rwx", "s", "split", "succ", "unlink", "volume", "w", "watch",
    "words", "x", "z",
];

/// The built-in `IO::Path` type objects: `IO::Path` and its per-OS subclasses.
/// (`IO::Path::Parts` is a separate class, not a subclass.)
fn is_builtin_io_path_type(name: &str) -> bool {
    matches!(
        name,
        "IO::Path" | "IO::Path::Unix" | "IO::Path::Win32" | "IO::Path::Cygwin" | "IO::Path::QNX"
    )
}

impl Interpreter {
    /// `X::Parameter::InvalidConcreteness` for a `:D:`-invocant built-in
    /// method called on a type object, or `None` when the call is fine.
    ///
    /// - `fail`/`die`/`throw`/`rethrow`/`resume` (no args) on an Exception
    ///   type object (`X::NYI.throw`).
    /// - An `IO::Path:D:` method on a built-in `IO::Path` type object
    ///   (`IO::Path.e`; `Nil.IO` / `Str.IO` return that type object).
    // Cost: O(1) for IO::Path (a fixed ~36-name table); O(d) for the
    // Exception case, d = the class's parent-chain depth.
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
        if is_builtin_io_path_type(&name) && IO_PATH_CONCRETE_METHODS.contains(&method) {
            return Some(RuntimeError::parameter_invalid_concreteness(
                "IO::Path", &name, method, "", true, true,
            ));
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
