//! Tests of `term_origins`.

use super::*;

fn sp(from: u32, to: u32) -> SrcSpan {
    SrcSpan { from, to }
}

#[test]
fn notes_belong_to_the_innermost_statement() {
    // `f({ $x }, $x)`: the closure's statement 3..5 sits inside 0..12.
    let out = bucket_by_statement(
        vec![(1, sp(9, 11)), (1, sp(3, 5))],
        vec![sp(0, 12), sp(3, 5)],
    );
    assert_eq!(
        out,
        vec![(sp(3, 5), 1, sp(3, 5)), (sp(0, 12), 1, sp(9, 11))]
    );
}

#[test]
fn a_term_read_again_is_noted_once() {
    // The same fingerprint at the same range, read by several parsers.
    let out = bucket_by_statement(
        vec![(1, sp(3, 5)), (1, sp(3, 5)), (2, sp(3, 5)), (1, sp(3, 5))],
        vec![sp(0, 12)],
    );
    assert_eq!(
        out,
        vec![(sp(0, 12), 1, sp(3, 5)), (sp(0, 12), 2, sp(3, 5))]
    );
}

#[test]
fn is_new_skips_only_the_term_just_noted() {
    let text = "1 + 2";
    let (a, b) = (&text[0..1], &text[1..]);
    let one = Expr::Literal(crate::value::Value::int(1));
    let two = Expr::Literal(crate::value::Value::int(2));
    assert!(is_new(a, b, &one));
    assert!(!is_new(a, b, &one));
    assert!(!is_new(a, b, &two), "same range and kind: the same term");
    assert!(is_new(b, a, &one), "another range is another term");
}

#[test]
fn a_surplus_note_attaches_nothing_for_its_fingerprint() {
    let stmt = sp(0, 20);
    // One term of fingerprint 7, but an abandoned alternative noted a
    // second range for it: neither is attached.
    let ranges = matched_ranges(
        vec![(7, sp(2, 4)), (7, sp(10, 12)), (8, sp(14, 16))],
        &[(stmt, 7), (stmt, 8)],
        vec![stmt],
    );
    assert!(!ranges.contains_key(&key(stmt, 7)));
    assert_eq!(ranges[&key(stmt, 8)], [sp(14, 16)]);
}

#[test]
fn a_copied_or_synthesized_term_leaves_the_range_to_the_first() {
    let stmt = sp(0, 20);
    // `$x += 1` keeps two copies of `$x`, read once.
    let ranges = matched_ranges(vec![(7, sp(0, 2))], &[(stmt, 7), (stmt, 7)], vec![stmt]);
    assert_eq!(ranges[&key(stmt, 7)], [sp(0, 2)]);
}

#[test]
fn equal_terms_take_their_ranges_in_source_order() {
    let stmt = sp(0, 20);
    let ranges = matched_ranges(
        vec![(7, sp(10, 12)), (7, sp(2, 4)), (7, sp(2, 4))],
        &[(stmt, 7), (stmt, 7)],
        vec![stmt],
    );
    assert_eq!(ranges[&key(stmt, 7)], [sp(2, 4), sp(10, 12)]);
}

#[test]
fn a_new_parse_does_not_inherit_the_last_term() {
    // A fragment parsed from a fresh buffer can get the address of a buffer
    // an earlier parse read from: the term it reads at the same offset is a
    // new one (#18506).
    let expr = Expr::Whatever;
    let text = "nf";
    let (input, rest) = (text, &text[2..]);
    {
        let _outer = begin(false);
        assert!(is_new(input, rest, &expr));
        assert!(!is_new(input, rest, &expr), "the same term passes once");
        {
            let _fragment = begin(false);
            assert!(is_new(input, rest, &expr), "a new parse starts afresh");
        }
        assert!(
            !is_new(input, rest, &expr),
            "the enclosing parse's last term is restored"
        );
    }
}
