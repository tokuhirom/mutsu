//! Which instance classes are built-in `Supply` objects.
//!
//! `Proc::Async.stdout` / `.stderr` answer a `Proc::Async::Pipe`, rakudo's
//! core subclass of `Supply` (`.^mro` is `Proc::Async::Pipe Supply Any Mu`).
//! It is the same supply object underneath, so the code that recognizes a
//! supply instance asks this predicate instead of comparing the class name
//! with `"Supply"`, which a pipe does not have.

use crate::symbol::Symbol;

/// The class of a `Proc::Async` output pipe.
pub(crate) const PROC_ASYNC_PIPE: &str = "Proc::Async::Pipe";

/// Whether an instance of `class` is a built-in supply: `Supply` itself or a
/// `Proc::Async::Pipe`.
// Cost: O(1), two symbol compares.
pub(crate) fn is_supply_class(class: Symbol) -> bool {
    class == crate::sym!("Supply") || class == crate::sym!(PROC_ASYNC_PIPE)
}
