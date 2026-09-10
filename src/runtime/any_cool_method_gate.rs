//! `Any` is not a `Cool`, so it does not answer `Cool`'s methods.
//!
//! `uc`, `chars`, `comb`, `abs`, `substr`, ... are declared on `Cool`, which
//! `Any` does not inherit from -- it is the other way round (`Cool` is a
//! subclass of `Any`). Calling one of them on an undefined `Any` is therefore
//! `X::Method::NotFound` in raku:
//!
//! ```text
//! my $x; say $x.comb(/\w+/);   # No such method 'comb' for invocant of type 'Any'
//! ```
//!
//! mutsu's by-name native dispatch did not model that boundary for a type
//! object: it recognizes the method NAME, stringifies the receiver -- and a
//! type object stringifies to its gist -- then answers out of `"(Any)"`. The
//! result is not an error but a *wrong value* that travels on, which is worse
//! than a missing method: `$undefined.comb(/\w+/)` answered `("Any",)`, and
//! `@words.push("said", $t.comb(/\w+/))` put it into the array (#7773).
//!
//! This is ADR-0051's P4 rule ("an unresolved `Cool`-only method throws")
//! applied to the one receiver shape its gates did not cover: they are reached
//! only for a `ValueView::Instance`, so the `Any`/`Mu` type object walked
//! straight past them into the cascades.

use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};
use std::collections::HashSet;
use std::sync::OnceLock;

/// `Cool` names raku ALSO declares on `Any`, which must NOT be gated.
///
/// Both are `proto`s on `Any` whose candidates merely refuse an *undefined*
/// invocant, so raku answers `X::Multi::NoMatch` (`split`) or a
/// use-of-uninitialized warning (`match`) for them -- never
/// `X::Method::NotFound`. Reporting a missing method here would swap one wrong
/// answer for another, so they keep the behaviour they have.
const ALSO_DECLARED_ON_ANY: &[&str] = &["match", "split"];

/// The `Cool` rows an `Any`/`Mu` invocant does not inherit, derived from the
/// same catalog `.^can` answers from (`builtins::native_method_row`) so the two
/// cannot drift apart -- `Any.^can("comb")` was already empty while the call
/// itself succeeded, which is exactly the disagreement fixed here.
///
/// Verified against rakudo v2026.07 on 2026-09-10: `Any.^can` is empty for
/// every name this yields.
fn cool_rows_absent_from_any() -> &'static HashSet<&'static str> {
    static NAMES: OnceLock<HashSet<&'static str>> = OnceLock::new();
    NAMES.get_or_init(|| {
        use crate::builtins::builtin_type_methods::builtin_type_method_names;
        let inherited: HashSet<&'static str> = builtin_type_method_names("Any")
            .into_iter()
            .chain(builtin_type_method_names("Mu"))
            .chain(ALSO_DECLARED_ON_ANY.iter().copied())
            .collect();
        builtin_type_method_names("Cool")
            .into_iter()
            .filter(|name| !inherited.contains(name))
            .collect()
    })
}

/// The structured `X::Method::NotFound` for a `Cool`-only method called on the
/// `Any` or `Mu` type object, or `None` when the call is not gated.
///
/// Only those two type objects qualify. Every other undefined receiver either
/// IS a `Cool` (`Str`, `Int`, `Nil`, ... -- raku runs the method and warns
/// about the uninitialized value) or has its own method surface.
pub(crate) fn cool_method_not_found(
    target: &Value,
    method: &str,
) -> Option<crate::value::RuntimeError> {
    let owner = undefined_owner(target)?;
    // Two independently raku-verified sources of "resolvable only through
    // `Cool`": ADR-0051's hand-curated list, and the generated row catalog.
    // Neither subsumes the other -- the curated list carries names with no
    // `Cool` row of their own (`bytes`, `lazy`, `race`, `Date`), the catalog
    // carries rows the list predates (`printf`, `is-prime`, the native-int
    // coercion family) -- and `Any.^can` is empty for every name in either.
    (Interpreter::cool_only_builtin_method(method) || cool_rows_absent_from_any().contains(method))
        .then(|| super::methods_signature_errors::make_method_not_found_error(method, owner, false))
}

/// `"Any"`/`"Mu"` when `target` is one of those two type objects.
fn undefined_owner(target: &Value) -> Option<&'static str> {
    match target.view() {
        ValueView::Package(name) if name == "Any" => Some("Any"),
        ValueView::Package(name) if name == "Mu" => Some("Mu"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::Symbol;

    fn any() -> Value {
        Value::package(Symbol::intern("Any"))
    }

    fn mu() -> Value {
        Value::package(Symbol::intern("Mu"))
    }

    #[test]
    fn cool_only_methods_are_missing_on_any() {
        for name in [
            // string
            "comb",
            "uc",
            "chars",
            "substr",
            "trim",
            "words",
            "flip",
            // numeric
            "abs",
            "sqrt",
            "floor",
            "is-prime",
            "printf", // coercions
            "IO",
            "Num",
            "Rat",
            "Version",
            "NFC",
            "int8",
            // curated-list-only names with no `Cool` row of their own
            "bytes",
            "lazy",
            "race",
            "DateTime",
            "parse-base",
        ] {
            let err = cool_method_not_found(&any(), name);
            assert!(err.is_some(), "{name} should be absent from Any");
            // A "Did you mean ...?" suffix may follow, exactly as raku's does.
            let message = err.unwrap().message.to_string();
            let expected = format!("No such method '{name}' for invocant of type 'Any'");
            assert!(
                message.starts_with(&expected),
                "unexpected message for {name}: {message}"
            );
        }
        assert!(cool_method_not_found(&mu(), "uc").is_some());
    }

    #[test]
    fn methods_any_itself_declares_are_not_gated() {
        // `Any`/`Mu`'s own surface, and the two `Cool` names raku shares with
        // `Any` (see `ALSO_DECLARED_ON_ANY`).
        for name in [
            "gist",
            "raku",
            "Str",
            "Bool",
            "Int",
            "Numeric",
            "defined",
            "elems",
            "list",
            "so",
            "WHAT",
            "WHICH",
            "match",
            "split",
            "no-such-method",
        ] {
            assert!(
                cool_method_not_found(&any(), name).is_none(),
                "{name} should still resolve on Any"
            );
        }
    }

    #[test]
    fn a_defined_receiver_is_never_gated() {
        assert!(cool_method_not_found(&Value::str_from("abc"), "uc").is_none());
        assert!(cool_method_not_found(&Value::int(2), "abs").is_none());
        // `Nil` and `Str:U` ARE `Cool`: raku runs the method and warns.
        assert!(cool_method_not_found(&Value::NIL, "comb").is_none());
        assert!(cool_method_not_found(&Value::package(Symbol::intern("Str")), "uc").is_none());
    }
}
