//! What the interpreter still needs to know about `Test` after the native TAP
//! provider was retired (#7566).
//!
//! `use Test` loads rakudo's own `Test.rakumod` (vendored to
//! `modules/Rakudo-Core/lib/`), so every assertion is an ordinary imported Raku
//! sub and there is no Rust implementation of `ok`/`is`/`plan` left. What
//! remains here are three predicates that are *about* the module rather than
//! part of it:
//!
//! * [`Interpreter::is_test_function_name`] — a static name list, so the
//!   analysis frontend (ADR-0065) and the undeclared-routine diagnostic can
//!   recognise a `Test` / `Test::Util` routine in a unit they never run;
//! * [`Interpreter::test_module_loaded`] / [`Interpreter::test_mode_active`] —
//!   the TAP-state gates the runtime consults for output buffering, thread
//!   cloning and bare-word dispatch.

use super::*;

impl Interpreter {
    /// Returns true when the Test module has been loaded (plan or test
    /// state exists), indicating that test function names should be resolved
    /// as function calls rather than bare words.
    pub(crate) fn test_mode_active(&self) -> bool {
        self.tap.active()
    }

    /// True when a `Test` (or `Test::*`) module is loaded. Unlike
    /// [`Self::test_mode_active`] it is already true for the very first test
    /// call (`plan`), before any `TestState` exists.
    pub(crate) fn test_module_loaded(&self) -> bool {
        self.loaded_modules.contains("Test")
            || self.loaded_modules.iter().any(|m| m.starts_with("Test::"))
    }

    /// Whether `name` is a routine `Test` or roast's `Test::Util` provides.
    ///
    /// Nothing dispatches on this any more — it is a *recognition* list. The
    /// static known-routine tables (`runtime/undeclared_routines.rs`,
    /// `crate::analysis`) use it so a file that calls `is-deeply` is not
    /// reported as calling an undeclared routine when no module was loaded,
    /// and the bare-word resolver uses it to route a hyphenated zero-argument
    /// call (`done-testing`, `make-temp-dir`) through function dispatch.
    pub(crate) fn is_test_function_name(name: &str) -> bool {
        matches!(
            name,
            "ok" | "nok"
                | "diag"
                | "pass"
                | "flunk"
                | "is"
                | "isnt"
                | "plan"
                | "done-testing"
                | "skip"
                | "skip-rest"
                | "bail-out"
                | "cmp-ok"
                | "like"
                | "unlike"
                | "is-deeply"
                | "is-approx"
                | "lives-ok"
                | "dies-ok"
                | "exits-ok"
                | "isa-ok"
                | "force_todo"
                | "force-todo"
                | "eval-lives-ok"
                | "eval-dies-ok"
                | "throws-like"
                | "fails-like"
                | "use-ok"
                | "does-ok"
                | "can-ok"
                | "todo"
                | "subtest"
                | "warns-like"
        )
    }
}
