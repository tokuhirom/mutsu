//! `Metamodel::DefiniteHOW` — the metaclass of a definiteness-constrained type
//! object (`Int:D`, `Any:U`).
//!
//! Per ADR-0069 a constrained type object is an ordinary type object whose
//! interned name carries the smiley (`Package("Int:D")`), and its metaclass is
//! derived from that name rather than stored. This module implements the two
//! metamethods `DefiniteHOW` adds over `ClassHOW`:
//!
//! * `base_type` — the unconstrained type the smiley was applied to (`Int`).
//! * `definite`  — `1` for `:D`, `0` for `:U`.
//!
//! Both throw `X::Method::NotFound` on an unconstrained type, matching Rakudo,
//! where the methods live on `DefiniteHOW` and nowhere else.

use super::*;

impl Interpreter {
    /// Handle a `DefiniteHOW`-only metamethod. Returns `None` when `method` is
    /// not one of them, so the caller falls through to the `ClassHOW` arms.
    pub(super) fn dispatch_definitehow_method(
        &self,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "base_type" | "definite") {
            return None;
        }
        let target = args.first()?;
        let name = match target.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => return Some(Err(Self::definitehow_method_not_found(method))),
        };
        let (base, smiley) = crate::runtime::types::strip_type_smiley(&name);
        let Some(smiley) = smiley else {
            // `Int.^base_type` / `Int.^definite`: the receiver's metaclass is
            // `ClassHOW`, which has neither method.
            return Some(Err(Self::definitehow_method_not_found(method)));
        };
        if smiley == ":_" {
            // `:_` asserts nothing, so Rakudo folds `Any:_` back to plain `Any`
            // — a `ClassHOW`, with neither method.
            return Some(Err(Self::definitehow_method_not_found(method)));
        }
        Some(Ok(match method {
            "base_type" => Value::package(Symbol::intern(base)),
            _ => Value::int(i64::from(smiley == ":D")),
        }))
    }

    fn definitehow_method_not_found(method: &str) -> RuntimeError {
        RuntimeError::new(format!(
            "X::Method::NotFound: No such method '{method}' for invocant of type \
             'Perl6::Metamodel::ClassHOW'"
        ))
    }
}
