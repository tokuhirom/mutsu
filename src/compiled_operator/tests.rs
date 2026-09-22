//! Unit tests for [`crate::compiled_operator`]: the lowering must decode a
//! statically spelled operator into exactly the shape the VM used to compute
//! per execution.

use crate::compiled_operator::*;

#[test]
fn plain_operator_lowers_to_itself() {
    let spec = ReductionSpec::lower("+");
    assert!(!spec.scan && !spec.negate && !spec.shortcircuit && !spec.reverse);
    assert_eq!(spec.base_str(), "+");
}

#[test]
fn scan_marker_is_structural() {
    let spec = ReductionSpec::lower("\\~");
    assert!(spec.scan);
    assert_eq!(spec.base_str(), "~");
}

#[test]
fn negation_only_splits_a_known_base_operator() {
    let negated = ReductionSpec::lower("!after");
    assert!(negated.negate);
    assert_eq!(negated.base_str(), "after");

    // `!=` is its own operator, not a negated `=`.
    let ne = ReductionSpec::lower("!=");
    assert!(!ne.negate);
    assert_eq!(ne.base_str(), "!=");

    // `!nosuchop` is not a negation either: the remainder is unknown, so
    // the whole spelling stays the base operator and is resolved as a
    // user-defined infix at run time.
    let custom = ReductionSpec::lower("!nosuchop");
    assert!(!custom.negate);
    assert_eq!(custom.base_str(), "!nosuchop");
}

#[test]
fn unicode_aliases_are_canonicalized_by_the_compiler() {
    for (alias, ascii) in [
        ("\u{2218}", "o"),
        ("\u{00D7}", "*"),
        ("\u{00F7}", "/"),
        ("\u{2212}", "-"),
        ("\u{2264}", "<="),
        ("\u{2265}", ">="),
        ("\u{2260}", "!="),
    ] {
        assert_eq!(ReductionSpec::lower(alias).base_str(), ascii, "{alias}");
    }
}

#[test]
fn reverse_prefixes_fold_to_a_parity() {
    let one = ReductionSpec::lower("R/");
    assert!(one.reverse);
    assert_eq!(one.base_str(), "/");

    let two = ReductionSpec::lower("RR/");
    assert!(!two.reverse, "two reversals cancel");
    assert_eq!(two.base_str(), "/");

    // `R&foo` is undecidable at compile time: whether `&foo` names a
    // callable is a runtime question, so the `R` survives for the VM.
    let callable = ReductionSpec::lower("R&foo");
    assert!(!callable.reverse);
    assert_eq!(callable.base_str(), "R&foo");
}

#[test]
fn scan_negation_and_reverse_compose() {
    let spec = ReductionSpec::lower("\\!eqv");
    assert!(spec.scan && spec.negate);
    assert_eq!(spec.base_str(), "eqv");

    let spec = ReductionSpec::lower("\\R-");
    assert!(spec.scan && spec.reverse);
    assert_eq!(spec.base_str(), "-");
}

#[test]
fn thunked_shortcircuit_marker_is_structural() {
    let spec = ReductionSpec::lower("_sc_&&");
    assert!(spec.shortcircuit && !spec.scan);
    assert_eq!(spec.base_str(), "&&");

    let spec = ReductionSpec::lower("\\_sc_orelse");
    assert!(spec.shortcircuit && spec.scan);
    assert_eq!(spec.base_str(), "orelse");
}

#[test]
fn an_unknown_infix_stays_a_custom_symbol() {
    let spec = ReductionSpec::lower("my-op");
    assert!(!spec.scan && !spec.negate && !spec.shortcircuit && !spec.reverse);
    assert_eq!(spec.base_str(), "my-op");
    assert!(!is_builtin_infix(spec.base_str()));
}

#[test]
fn meta_kinds_round_trip() {
    for (text, kind) in [
        ("reduce", MetaKind::Reduce),
        ("R", MetaKind::Reverse),
        ("X", MetaKind::Cross),
        ("Z", MetaKind::Zip),
        ("!", MetaKind::Negate),
    ] {
        assert_eq!(MetaKind::lower(text), Some(kind));
        assert_eq!(kind.as_str(), text);
    }
    assert_eq!(MetaKind::lower("nope"), None);
}

#[test]
fn hyper_delimiters_strip_both_ascii_and_unicode() {
    assert_eq!(strip_hyper_delimiters(">>+<<"), Some("+"));
    assert_eq!(strip_hyper_delimiters("\u{00AB}*\u{00BB}"), Some("*"));
    assert_eq!(strip_hyper_delimiters(">><<"), None);
    assert_eq!(strip_hyper_delimiters("+"), None);
}

#[test]
fn infix_names_are_memoized_per_operator() {
    let a = infix_names("+");
    let b = infix_names("+");
    assert_eq!(a.infix, b.infix);
    assert_eq!(a.infix.as_str(), "infix:<+>");
    assert_eq!(a.amp_infix.as_str(), "&infix:<+>");
    assert_eq!(a.amp_op.as_str(), "&+");
    // A repeat lookup must not intern anything — not even its own key.
    let before = crate::symbol::intern_calls();
    let _ = infix_names("+");
    assert_eq!(crate::symbol::intern_calls(), before);
}
