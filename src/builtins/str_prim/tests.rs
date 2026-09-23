//! Expected values are rakudo's, measured with `use nqp` (see
//! `t/nqp/nqp-str-prim-parity.t`, which pins the same answers end to end).

use super::*;

fn s(x: &str) -> Value {
    Value::str_from(x)
}

/// `a`, `é` (e + U+301), `x`, `\r\n`, `Y`, a regional-indicator flag, `z`:
/// seven graphemes, ten codepoints.
const MIXED: &str = "ae\u{301}x\r\nY\u{1F1EF}\u{1F1F5}z";

#[test]
fn chars_counts_graphemes() {
    assert_eq!(chars(&s(MIXED)), 7);
    assert_eq!(chars(&s("")), 0);
    assert_eq!(chars(&s("abc")), 3);
}

#[test]
fn nqp_substr_follows_moarvm_argument_rules() {
    let v = s("abcdef");
    let sub = |f, w| nqp_substr(&v, f, w).map(|x| x.to_string_value());
    assert_eq!(sub(-2, Some(1)).unwrap(), "e");
    assert_eq!(sub(2, Some(-1)).unwrap(), "cdef");
    assert_eq!(sub(4, Some(99)).unwrap(), "ef");
    assert_eq!(sub(7, Some(1)).unwrap(), "");
    assert_eq!(sub(-1, None).unwrap(), "f");
    assert!(sub(-10, Some(2)).is_err());
    let m = s(MIXED);
    assert_eq!(
        nqp_substr(&m, 3, Some(1)).unwrap().to_string_value(),
        "\r\n"
    );
}

#[test]
fn index_and_rindex_are_grapheme_positions() {
    let m = s(MIXED);
    assert_eq!(nqp_index(&m, "Y", 0, Fold::Exact), 4);
    assert_eq!(nqp_index(&m, "e", 0, Fold::Exact), -1);
    assert_eq!(nqp_index(&m, "", 3, Fold::Exact), 3);
    assert_eq!(nqp_index(&m, "", 99, Fold::Exact), -1);
    assert_eq!(nqp_rindex(&m, "x", None).unwrap(), 2);
    assert_eq!(nqp_rindex(&m, "", None).unwrap(), 7);
    let v = s("abcdef");
    assert_eq!(nqp_rindex(&v, "c", Some(-1)).unwrap(), 2);
    assert_eq!(nqp_rindex(&v, "", Some(99)).unwrap(), -1);
    assert!(nqp_rindex(&v, "c", Some(99)).is_err());
}

#[test]
fn folded_search_uses_full_case_fold_and_boundaries() {
    assert_eq!(nqp_index(&s("STRASSE"), "ß", 0, Fold::Case), 4);
    assert_eq!(nqp_index(&s("straße"), "SS", 0, Fold::Case), 4);
    assert_eq!(nqp_index(&s("aİb"), "i", 0, Fold::Case), -1);
    assert_eq!(nqp_index(&s("café"), "E", 0, Fold::CaseMark), 3);
    assert_eq!(nqp_index(&s("AbC\u{301}d"), "c\u{301}", 0, Fold::Case), 2);
    assert_eq!(nqp_index(&s("AbC\u{301}d"), "C", 0, Fold::Mark), 2);
    assert_eq!(nqp_index(&s("Hello"), "LL", 1, Fold::Case), 2);
}

#[test]
fn eqat_compares_whole_graphemes() {
    let m = s(MIXED);
    assert!(nqp_eqat(&m, "x\r\n", 2, Fold::Exact));
    assert!(!nqp_eqat(&m, "x\r", 2, Fold::Exact));
    assert!(!nqp_eqat(&m, "e", 1, Fold::Exact));
    let v = s("abcdef");
    assert!(nqp_eqat(&v, "", 6, Fold::Exact));
    assert!(!nqp_eqat(&v, "", 7, Fold::Exact));
    assert!(nqp_eqat(&v, "ab", -6, Fold::Exact));
    assert!(nqp_eqat(&s("STRASSE"), "ß", 4, Fold::Case));
    assert!(nqp_eqat(&s("straße"), "SS", 4, Fold::Case));
    assert!(!nqp_eqat(&s("STRASSE"), "ße", 5, Fold::Case));
}

#[test]
fn ordat_is_the_first_nfc_codepoint_of_a_grapheme() {
    let m = s(MIXED);
    assert_eq!(nqp_ordat(&m, 1), 0xE9);
    assert_eq!(nqp_ordat(&m, 3), 13);
    assert_eq!(nqp_ordat(&m, 5), 0x1F1EF);
    assert_eq!(nqp_ordat(&m, 99), -1);
    assert_eq!(nqp_ordat(&m, -1), -1);
}

#[test]
fn find_char_scans_graphemes() {
    let m = s(MIXED);
    assert_eq!(find_char(&m, 0, 99, |c| c == '\r'), 3);
    assert_eq!(find_char(&m, 0, 3, |c| c == '\r'), 3);
    assert_eq!(find_char(&m, 4, 1, |c| c == 'Y'), 4);
}

#[test]
fn building_renormalizes() {
    assert_eq!(concat("e", "\u{301}").to_string_value(), "\u{E9}");
    assert_eq!(flip("abc").to_string_value(), "cba");
    assert_eq!(
        flip(MIXED).to_string_value(),
        "z\u{1F1EF}\u{1F1F5}Y\r\nx\u{E9}a"
    );
    assert_eq!(repeat("ab", 3).unwrap().to_string_value(), "ababab");
    assert_eq!(chars(&repeat("\u{301}", 2).unwrap()), 1);
    assert_eq!(graphemes(&s(MIXED)).len(), 7);
}
