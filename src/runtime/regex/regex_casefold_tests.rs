//! Unit tests for [`super::casefold_iter`], in their own file so the module
//! they cover stays inside the repository's 500-line limit.

use super::*;

/// The general definition `casefold_iter` shortcuts for ASCII.
fn upper_then_lower(c: char) -> Vec<char> {
    c.to_uppercase()
        .collect::<String>()
        .to_lowercase()
        .chars()
        .collect()
}

#[test]
fn ascii_fast_path_matches_the_general_fold() {
    for c in (0u8..=0x7f).map(char::from) {
        assert_eq!(casefold_iter(c).collect::<Vec<_>>(), upper_then_lower(c), "{c:?}");
    }
}

#[test]
fn non_ascii_folds_are_unchanged() {
    // Multi-character folds, a fold into ASCII (KELVIN SIGN), and plain letters.
    for c in ['ß', 'ﬁ', '\u{212A}', 'Σ', 'ς', 'É', 'İ', 'ǅ'] {
        assert_eq!(casefold_iter(c).collect::<Vec<_>>(), upper_then_lower(c), "{c:?}");
        assert_eq!(casefold_char(c), upper_then_lower(c), "{c:?}");
    }
    assert!(casefold_eq("STRASSE".chars(), "straße".chars()));
    assert!(casefold_eq("\u{212A}elvin".chars(), "KELVIN".chars()));
    assert!(!casefold_eq("yes".chars(), "yet".chars()));
}
