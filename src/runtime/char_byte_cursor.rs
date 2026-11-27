//! Char positions to byte offsets over one walk of a subject.
//!
//! The regex engine reports positions as char indices into a
//! [`MatchTarget`](crate::runtime::MatchTarget)'s char vector, while the
//! pieces a walk builds (`split`'s fields, `subst`'s kept text) are byte
//! ranges of the subject's UTF-8. A walk only moves forward, so one cursor
//! converts every position in O(n) total instead of collecting a second
//! `Vec<char>` of the subject to slice by char.

/// Converts ascending char positions into byte offsets of `text`.
pub(crate) struct CharByteCursor<'a> {
    text: &'a str,
    ascii: bool,
    char_pos: usize,
    byte_pos: usize,
}

impl<'a> CharByteCursor<'a> {
    // Cost: O(n), n = bytes of `text` (the ASCII check).
    pub(crate) fn new(text: &'a str) -> Self {
        CharByteCursor {
            text,
            ascii: text.is_ascii(),
            char_pos: 0,
            byte_pos: 0,
        }
    }

    /// The byte offset of char position `pos` (clamped to the end of the
    /// text). Positions must not decrease between calls.
    // Cost: O(1) amortized over a walk (O(1) for an ASCII subject).
    pub(crate) fn byte_of(&mut self, pos: usize) -> usize {
        if self.ascii {
            return pos.min(self.text.len());
        }
        debug_assert!(pos >= self.char_pos, "walk positions went backwards");
        let mut chars = self.text[self.byte_pos..].chars();
        while self.char_pos < pos {
            let Some(ch) = chars.next() else { break };
            self.byte_pos += ch.len_utf8();
            self.char_pos += 1;
        }
        self.byte_pos
    }
}

#[cfg(test)]
mod tests {
    use super::CharByteCursor;

    #[test]
    fn ascii_positions_are_byte_offsets() {
        let mut c = CharByteCursor::new("alpha beta");
        assert_eq!(c.byte_of(0), 0);
        assert_eq!(c.byte_of(6), 6);
        assert_eq!(c.byte_of(99), 10);
    }

    #[test]
    fn multibyte_positions_walk_forward() {
        let text = "aé日b";
        let mut c = CharByteCursor::new(text);
        assert_eq!(c.byte_of(1), 1);
        assert_eq!(c.byte_of(1), 1);
        assert_eq!(c.byte_of(2), 3);
        assert_eq!(c.byte_of(3), 6);
        assert_eq!(c.byte_of(4), 7);
        assert_eq!(c.byte_of(5), 7);
        assert_eq!(&text[3..6], "日");
    }
}
