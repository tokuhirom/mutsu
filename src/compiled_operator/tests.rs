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

/// The layers an operator decodes to, outermost first, plus its leaf — the
/// whole observable content of an [`InfixShape`].
fn shape_of(op: &str) -> (Vec<MetaLayer>, String, String) {
    let shape = InfixShape::lower(op);
    let mut cursor = shape.as_ref();
    let mut layers = Vec::new();
    while let Some((layer, inner)) = cursor.split_first() {
        layers.push(layer);
        cursor = inner;
    }
    (
        layers,
        cursor.leaf().to_string(),
        cursor.canonical().to_string(),
    )
}

#[test]
fn a_plain_operator_has_no_meta_layers() {
    let (layers, leaf, canonical) = shape_of("+");
    assert!(layers.is_empty());
    assert_eq!(leaf, "+");
    assert_eq!(canonical, "+");
}

#[test]
fn meta_layers_decode_outermost_first() {
    assert_eq!(shape_of("R-").0, vec![MetaLayer::Reverse]);
    assert_eq!(shape_of("Z+").0, vec![MetaLayer::Zip]);
    assert_eq!(shape_of("Z").0, vec![MetaLayer::ZipTuple]);
    assert_eq!(
        shape_of("RZ-").0,
        vec![MetaLayer::Reverse, MetaLayer::Zip],
        "R wraps the Z, and both survive"
    );
    assert_eq!(shape_of("RZ-").1, "-");
    assert_eq!(
        shape_of("RR+").0,
        vec![MetaLayer::Reverse, MetaLayer::Reverse],
        "the VM cancels a double reverse by applying it twice"
    );
}

#[test]
fn the_reduction_bracket_is_transparent() {
    // `[op]` as an INNER operator is `op` applied once, so it records no layer
    // (the identity `MetaKind::Reduce` documents).
    assert_eq!(shape_of("[+]").0, Vec::new());
    assert_eq!(shape_of("[+]").1, "+");
    assert_eq!(shape_of("[R+]").0, vec![MetaLayer::Reverse]);
    // An unclosed or empty bracket is not one: it stays part of the leaf.
    assert_eq!(shape_of("[]").1, "[]");
    assert_eq!(shape_of("[+").1, "[+");
}

#[test]
fn hyper_delimiters_carry_their_dwim_direction() {
    assert_eq!(
        shape_of(">>+<<").0,
        vec![MetaLayer::Hyper {
            dwim_left: false,
            dwim_right: false
        }]
    );
    assert_eq!(
        shape_of("<<+>>").0,
        vec![MetaLayer::Hyper {
            dwim_left: true,
            dwim_right: true
        }]
    );
    assert_eq!(
        shape_of(">>+>>").0,
        vec![MetaLayer::Hyper {
            dwim_left: false,
            dwim_right: true
        }]
    );
    assert_eq!(
        shape_of("\u{00AB}+\u{00AB}").0,
        vec![MetaLayer::Hyper {
            dwim_left: true,
            dwim_right: false
        }],
        "the Unicode delimiters carry the same directions"
    );
}

#[test]
fn the_leaf_keeps_its_spelling_and_offers_the_canonical_one() {
    let (layers, leaf, canonical) = shape_of("Z\u{00D7}");
    assert_eq!(layers, vec![MetaLayer::Zip]);
    assert_eq!(
        leaf, "\u{00D7}",
        "the source spelling, for a user infix lookup"
    );
    assert_eq!(
        canonical, "*",
        "and the ASCII spelling the tables are keyed by"
    );
}

#[test]
fn as_plain_only_answers_for_an_unwrapped_leaf() {
    assert_eq!(InfixShape::lower("=").as_ref().as_plain(), Some("="));
    assert_eq!(InfixShape::lower("Z=").as_ref().as_plain(), None);
    assert_eq!(InfixShape::lower("R~~").as_ref().as_plain(), None);
}

#[test]
fn an_unknown_infix_decodes_to_a_bare_leaf() {
    let (layers, leaf, _) = shape_of("my-op");
    assert!(layers.is_empty());
    assert_eq!(leaf, "my-op");
    assert!(!is_builtin_infix(&leaf));
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
