//! `Cool`'s string routines (`lc $x`, `substr($x, 0, 2)`, ...) are defined in
//! rakudo as `sub lc(\x) { x.lc }`: the routine form is the method form. A
//! class that declares (or composes a role that declares) its own `lc` is
//! therefore reached through `lc($obj)` too -- Red's `Column` answers
//! `lc .email` with a `Lowercase` AST node, not a lowercased `Str`.
//!
//! The native routine arms stringify their argument, so without this the
//! routine form silently bypassed the object's own method.

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

/// The `Cool` string routines that rakudo defines as `x.NAME(|rest)`.
const METHOD_FORWARDING_ROUTINES: &[&str] = &[
    "lc",
    "uc",
    "fc",
    "tc",
    "tclc",
    "wordcase",
    "flip",
    "trim",
    "trim-leading",
    "trim-trailing",
    "chomp",
    "chop",
    "ord",
    "ords",
    "lines",
    "words",
    "substr",
    "substr-rw",
    "samecase",
    "samemark",
    "samespace",
];

/// Cool routines outside [`METHOD_FORWARDING_ROUTINES`] whose routine form is
/// also `x.NAME` in rakudo, so an unhandled `Failure` argument is thrown.
const FAILURE_THROWING_ROUTINES: &[&str] = &[
    "chars", "chr", "sqrt", "abs", "floor", "ceiling", "round", "truncate", "sign", "exp",
    "log", "log2", "log10", "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh",
    "cis", "polymod", "expmod", "is-prime", "lsb", "msb",
];

impl Interpreter {
    /// `NAME($obj, ...)` for a user object with its own `NAME` method: the
    /// method answers, as `$obj.NAME(...)`. `None` leaves the routine to the
    /// native arms (every other receiver, every other routine).
    // Cost: O(1) for a name outside the list or a non-object first argument;
    // otherwise one memoized method probe, then the method call itself.
    pub(crate) fn forward_cool_routine_to_method(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // Cost: O(len(name)) -- one hash probe; every builtin call asks.
        static ROUTINES: std::sync::OnceLock<rustc_hash::FxHashSet<&'static str>> =
            std::sync::OnceLock::new();
        let forwards = ROUTINES
            .get_or_init(|| METHOD_FORWARDING_ROUTINES.iter().copied().collect())
            .contains(name);
        if !forwards && !FAILURE_THROWING_ROUTINES.contains(&name) {
            return None;
        }
        let first = args.first()?;
        // `lc(Failure)` is `Failure.lc`: an unhandled Failure explodes with its
        // exception instead of the routine stringifying its message.
        if let Some(err) = self.failure_to_runtime_error_if_unhandled(first) {
            return Some(Err(err));
        }
        if !forwards {
            return None;
        }
        let class = match first.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => return None,
        };
        if !self.has_user_method_including_role(&class, name) {
            return None;
        }
        Some(self.call_method_with_values(first.clone(), name, args[1..].to_vec()))
    }
}
