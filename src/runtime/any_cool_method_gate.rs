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

use crate::value::{Value, ValueView};
use rustc_hash::FxHashSet;
use std::sync::OnceLock;

/// `Cool` names raku ALSO declares on `Any`, which must NOT be gated.
///
/// Both are `proto`s on `Any` whose candidates merely refuse an *undefined*
/// invocant, so raku answers `X::Multi::NoMatch` (`split`) or a
/// use-of-uninitialized warning (`match`) for them -- never
/// `X::Method::NotFound`. Reporting a missing method here would swap one wrong
/// answer for another, so they keep the behaviour they have.
const ALSO_DECLARED_ON_ANY: &[&str] = &["match", "split"];

/// Names raku declares on a `Cool` *subtype* (`Str`, `Int`, `List`, ...) but
/// not on `Cool` itself, which the receiver-class-blind arity cascades
/// nevertheless answer by coercing the receiver: `succ`/`pred` (`Str`, `Int`,
/// ...), `base`/`polymod` (`Int`, `Num`, `Rat`), `parse-base`/`samespace`/
/// `Date`/`DateTime`/`bytes` (`Str`, `Blob`), `lazy`/`hyper`/`race` (the
/// `Iterable` family). raku v2026.07 answers `Cool.^can` and `Any.^can` with
/// nothing for every one of them (verified 2026-09-27).
const COOL_SUBTYPE_ONLY: &[&str] = &[
    "Date",
    "DateTime",
    "base",
    "bytes",
    "hyper",
    "lazy",
    "parse-base",
    "polymod",
    "pred",
    "race",
    "samespace",
    "succ",
];

/// The builtin method names an `Any`/`Mu` invocant -- or an instance of a
/// class with no `Cool` ancestor -- does not resolve (ADR-0051 P5, which
/// retired the 94-name hand-maintained list this replaces): every `Cool` row
/// of the catalog `.^can` answers from (`builtins::native_method_row`) that
/// `Any`/`Mu` do not also carry, plus [`COOL_SUBTYPE_ONLY`]. Reading the same
/// rows keeps the gate and `.^can` from drifting apart -- `Any.^can("comb")`
/// was already empty while the call itself succeeded (#7773).
///
/// Verified against rakudo v2026.07 on 2026-09-10: `Any.^can` is empty for
/// every name this yields.
fn cool_only_name(method: &str) -> bool {
    use crate::builtins::builtin_type_methods::builtin_type_method_listed as listed;
    COOL_SUBTYPE_ONLY.contains(&method)
        || (listed("Cool", method)
            && !listed("Any", method)
            && !listed("Mu", method)
            && !ALSO_DECLARED_ON_ANY.contains(&method))
}

/// The builtin method names `Any` declares and `Mu` does not: the `Mu` type
/// object is no `Any`, so it does not resolve them (`Mu.elems`, `Mu.List`,
/// `Mu.first` are `X::Method::NotFound` in Rakudo, #12976).
///
/// The coercions `Any` declares for the container types (`Any.List`, `Any.Hash`)
/// are not in the catalog `.^can` reads (`Any.^can('List')` is empty), so they
/// are listed here.
const ANY_COERCIONS: &[&str] = &[
    "Array", "Bag", "BagHash", "Hash", "List", "Map", "Mix", "MixHash", "Seq", "Set", "SetHash",
    "Slip", "Supply",
];

/// Names the `Any` catalog lists that `Mu` answers all the same
/// (`Mu.iterator` is the empty iterator, `Mu.ACCEPTS` is identity).
const ALSO_ON_MU: &[&str] = &["ACCEPTS", "iterator"];

fn any_only_names() -> &'static FxHashSet<&'static str> {
    static NAMES: OnceLock<FxHashSet<&'static str>> = OnceLock::new();
    NAMES.get_or_init(|| {
        use crate::builtins::builtin_type_methods::builtin_type_method_names;
        let mu: FxHashSet<&'static str> = builtin_type_method_names("Mu").into_iter().collect();
        builtin_type_method_names("Any")
            .into_iter()
            .chain(ANY_COERCIONS.iter().copied())
            .filter(|name| !mu.contains(name) && !ALSO_ON_MU.contains(name))
            .collect()
    })
}

/// Whether `method` is resolvable only through `Cool` or one of its subtypes
/// (see [`cool_only_name`]).
// Cost: O(k + |method|), k = the 12 names of `COOL_SUBTYPE_ONLY`; up to three
// per-owner hash probes, so no whole-catalog set is built on the first call.
pub(crate) fn is_cool_only_method(method: &str) -> bool {
    cool_only_name(method)
}

/// Whether `method` is one of the [`COOL_SUBTYPE_ONLY`] names: declared on
/// particular `Cool` subtypes (or the `Iterable` family) but on neither `Cool`
/// nor `Any`. The receiver-blind arity cascades answer these for any value by
/// coercing it (`"10".base(2)`, `5.lazy`, `$/.succ`), so every receiver shape
/// -- not only an `Instance` -- must be asked whether its own ancestry
/// declares the method before a cascade may answer (#9948).
// Cost: O(k), k = the 12 names of `COOL_SUBTYPE_ONLY`.
pub(crate) fn is_cool_subtype_only_method(method: &str) -> bool {
    COOL_SUBTYPE_ONLY.contains(&method)
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
    (is_cool_only_method(method) || (owner == "Mu" && any_only_names().contains(method)))
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

/// The `Cool` names Rakudo's `Blob` role declares for itself: `chars` and
/// `codes` raise `X::Buf::AsStr` (verified against rakudo v2026.07: of the
/// `Cool`-only names, a `Buf` or `Blob` `.can` these two alone, 2026-10-13).
const BLOB_DECLARED_COOL_NAMES: &[&str] = &["chars", "codes"];

/// The `Cool` text and search methods a `Pair`, `Capture`, `Version` or
/// quanthash does not resolve (verified against rakudo v2026.07 on each of
/// them, 2026-10-14): [`crate::value::COOL_TEXT_FOR_CLOSED_SHAPES`] and the
/// argument-taking names of a `Match`, without `fmt` and `split`, which `Any`
/// declares.
const NON_COOL_TEXT: &[&str] = &[
    "words", "codes", "lines", "trim", "trim-leading", "trim-trailing", "flip", "chomp", "chop",
    "comb", "uc", "lc", "fc", "tc", "tclc", "wordcase", "encode", "NFC", "NFD", "NFKC", "NFKD",
    "ord", "ords", "uniname", "uninames", "uniprop", "uniprops", "unival", "univals", "unimatch",
    "samecase", "samemark", "indent", "contains", "starts-with", "ends-with", "index", "rindex",
    "substr", "subst", "trans", "indices", "chars",
];

/// The type name to report a `Cool`-only method under when `target` is not a
/// `Cool`, so it does not answer `contains`, `starts-with`, `substr`, ...: a
/// buffer (a `Blob`/`Buf` instance or type object, an alias such as `utf8`, a
/// user class composing a buffer role; #13313), or a `Pair`, `Capture`,
/// `Version` or one of the quanthashes (#13035). `None` for any other receiver
/// or method. The caller still lets a method the receiver's own class declares
/// (or its `FALLBACK`) answer first.
// Cost: O(|method|) for the name lookups, O(1) for the shape probe.
pub(crate) fn buffer_refuses_cool_method(target: &Value, method: &str) -> Option<String> {
    use crate::value::DispatchShape as Shape;
    // The shape and name tests come first: `is_cool_only_method` builds a set
    // from the whole method-table snapshot on its first call, which a program
    // calling a method on a plain `Str` or `Int` must not pay for (#14878).
    // A code object (`Sub`, `Block`, ...) is no `Cool` either (#13035).
    if matches!(target.view(), ValueView::Sub(..) | ValueView::Routine { .. }) {
        return (NON_COOL_TEXT.contains(&method) && is_cool_only_method(method))
            .then(|| crate::value::types::what_type_name(target));
    }
    let receiver = crate::builtins::method_table::Receiver::of_settled(target)?;
    let refused = match receiver.shape {
        Shape::Blob | Shape::Buf => !BLOB_DECLARED_COOL_NAMES.contains(&method),
        // Only the text and search methods: `fmt` and `split` exist on `Any`,
        // and the other `Cool` names (`reverse`, `invert`, `Int`, ...) are
        // also answered by an ancestor these shapes have, which the catalog
        // `is_cool_only_method` reads does not list.
        Shape::Pair
        | Shape::Capture
        | Shape::Version
        | Shape::Set
        | Shape::SetHash
        | Shape::Bag
        | Shape::BagHash
        | Shape::Mix
        | Shape::MixHash => NON_COOL_TEXT.contains(&method),
        _ => false,
    };
    (refused && is_cool_only_method(method)).then(|| crate::value::types::what_type_name(target))
}

/// Whether `method` is `fmt` on a `Capture` or `Version`: `Any` declares the
/// `proto` but no candidate takes these receivers, so Rakudo answers
/// `X::Multi::NoMatch` (not `X::Method::NotFound`) whatever the arguments
/// (#13477).
// Cost: O(1).
pub(crate) fn fmt_has_no_candidate(target: &Value, method: &str) -> bool {
    method == "fmt" && matches!(target.view(), ValueView::Capture { .. } | ValueView::Version { .. })
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
            // `COOL_SUBTYPE_ONLY` names with no `Cool` row of their own
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
