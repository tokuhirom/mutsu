//! The yada-yada stub operators, `...`, `!!!` and `???`.
//!
//! The parser spells each as a call of a reserved marker routine whose only
//! arguments are the message the source wrote (none when it wrote none, so
//! the RakuAST converter can tell `...` from `... "Stub code executed"`). A
//! routine whose body is nothing but one of these is a *stub* (`.yada`, a
//! role's required method, a forward declaration), so every place that asks
//! "is this a stub" goes through [`is_marker`].

/// `...`: fails with `X::StubCode` (a Failure to the caller).
pub(crate) const FAIL: &str = "__mutsu_stub_die";
/// `!!!`: dies with `X::StubCode`.
pub(crate) const DIE: &str = "__mutsu_stub_fatal";
/// `???`: warns.
pub(crate) const WARN: &str = "__mutsu_stub_warn";

/// The message a stub without one carries.
pub(crate) const DEFAULT_MESSAGE: &str = "Stub code executed";

/// Whether `name` is one of the stub markers.
// Cost: O(1).
pub(crate) fn is_marker(name: &str) -> bool {
    name == FAIL || name == DIE || name == WARN
}
