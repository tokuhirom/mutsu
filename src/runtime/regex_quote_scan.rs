//! Which regex quote literal a position of a pattern text sits inside.
//!
//! The interpolation pre-pass (`interpolate_regex_scalars`) must not splice a
//! `$var` written inside a single-quoted literal, and treats one inside a
//! double-quoted literal by qq-string rules, so it asks this at every `<`,
//! `$` and `@` of a pattern. Asked from the start of the text each time, that
//! was quadratic in the pattern length; [`RegexQuoteCursor`] carries the
//! state forward instead.

use super::regex_parse::regex_single_quote_closes;

/// The opening quote of the regex string literal (`'...'`, `"..."` and their
/// Unicode variants) that char position `pos` sits inside, if any. Tracks both
/// quote families so a `'` inside a `"..."` region (`"it's" $x`), or a `"`
/// inside a `'...'` region, is not mistaken for an opener.
// Cost: O(pos).
fn open_regex_quote_at(chars: &[char], pos: usize) -> Option<char> {
    RegexQuoteCursor::new().open_at(chars, pos)
}

/// [`open_regex_quote_at`] for a scanner that asks about increasing positions
/// of one text: the quote state is carried forward from the previous query
/// instead of being recomputed from the start, so a left-to-right pass that
/// asks at every `<` and `$` of a pattern is linear in its length, not
/// quadratic (the interpolation pre-pass re-ran on every match attempt of a
/// parameterized grammar rule, ADR-12917).
pub(super) struct RegexQuoteCursor {
    pos: usize,
    open: Option<char>,
    escaped: bool,
}

impl RegexQuoteCursor {
    pub(super) fn new() -> Self {
        Self {
            pos: 0,
            open: None,
            escaped: false,
        }
    }

    /// The quote open at char position `pos` of `chars`, which must be the
    /// text every earlier query of this cursor asked about. A position
    /// before the previous one restarts the scan.
    // Cost: O(pos - previous pos) amortized; O(pos) after a restart.
    pub(super) fn open_at(&mut self, chars: &[char], pos: usize) -> Option<char> {
        if pos < self.pos {
            *self = Self::new();
        }
        let end = pos.min(chars.len());
        for &ch in &chars[self.pos.min(end)..end] {
            if self.escaped {
                self.escaped = false;
                continue;
            }
            if ch == '\\' && self.open != Some('\u{FF62}') {
                self.escaped = true;
                continue;
            }
            match self.open {
                Some(o) => {
                    let closes = if is_double_quote_opener(o) {
                        matches!(ch, '"' | '\u{201D}')
                    } else {
                        regex_single_quote_closes(o, ch)
                    };
                    if closes {
                        self.open = None;
                    }
                }
                None => {
                    if matches!(ch, '\'' | '\u{2018}' | '\u{201A}' | '\u{FF62}')
                        || is_double_quote_opener(ch)
                    {
                        self.open = Some(ch);
                    }
                }
            }
        }
        self.pos = pos;
        self.open
    }

    /// Whether `pos` sits inside any regex quote literal.
    // Cost: as [`Self::open_at`].
    pub(super) fn inside_any(&mut self, chars: &[char], pos: usize) -> bool {
        self.open_at(chars, pos).is_some()
    }

    /// Whether `pos` sits inside a single-quoted regex literal (`'...'`),
    /// where `$` is not interpolated.
    // Cost: as [`Self::open_at`].
    pub(super) fn inside_single(&mut self, chars: &[char], pos: usize) -> bool {
        self.open_at(chars, pos)
            .is_some_and(|o| !is_double_quote_opener(o))
    }

    /// Whether `pos` sits inside a double-quoted regex literal (`"..."`):
    /// a `$var.method(...)` chain there is a qq-string interpolation rather
    /// than a bare scalar followed by a match-any `.`.
    // Cost: as [`Self::open_at`].
    pub(super) fn inside_double(&mut self, chars: &[char], pos: usize) -> bool {
        self.open_at(chars, pos).is_some_and(is_double_quote_opener)
    }
}

fn is_double_quote_opener(ch: char) -> bool {
    matches!(ch, '"' | '\u{201C}' | '\u{201E}')
}

/// Whether char position `pos` sits inside any regex quote literal
/// (`'...'`, `"..."` or a Unicode variant). A scanner asking at several
/// increasing positions uses a [`RegexQuoteCursor`] instead.
// Cost: O(pos).
pub(super) fn is_inside_regex_quote_literal(chars: &[char], pos: usize) -> bool {
    open_regex_quote_at(chars, pos).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The cursor answers every position exactly as a scan from the start
    /// does, including after a backwards query restarts it.
    #[test]
    fn cursor_matches_a_fresh_scan_at_every_position() {
        for text in [
            r#"<.space> '$x' "it's $y" <[ ' ]> \' $z"#,
            "‘a $b’ „c $d“ ｢e \\ $f｣ 'g\\'h' $i",
            r#""unterminated $x <y>"#,
        ] {
            let chars: Vec<char> = text.chars().collect();
            let mut cursor = RegexQuoteCursor::new();
            for pos in 0..=chars.len() + 1 {
                assert_eq!(cursor.open_at(&chars, pos), open_regex_quote_at(&chars, pos));
            }
            for pos in (0..=chars.len()).rev().step_by(3) {
                let mut fresh = RegexQuoteCursor::new();
                assert_eq!(cursor.open_at(&chars, pos), fresh.open_at(&chars, pos));
            }
        }
    }

    #[test]
    fn cursor_classifies_quote_families() {
        let chars: Vec<char> = r#"a 'b' "c" d"#.chars().collect();
        let mut cursor = RegexQuoteCursor::new();
        assert!(!cursor.inside_any(&chars, 1));
        assert!(cursor.inside_single(&chars, 3));
        assert!(!cursor.inside_double(&chars, 3));
        assert!(cursor.inside_double(&chars, 7));
        assert!(!cursor.inside_single(&chars, 7));
        assert!(!cursor.inside_any(&chars, 10));
    }
}
