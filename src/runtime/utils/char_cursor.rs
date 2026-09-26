//! Forward char-index -> byte-offset mapping over one string.
//!
//! The regex engine reports spans as char (codepoint) indices, while the
//! output of a substitution is assembled from byte slices of the subject.
//! Converting every span by counting chars from the start of the string
//! made a substitution with r matches over an n-char subject pay O(n*r) for
//! the conversion alone (#9143). The spans of one
//! substitution are ascending, so a single cursor that only ever walks
//! forward converts all of them in O(n + r).

/// Maps ascending char indices of `text` to byte offsets in one forward pass.
pub(crate) struct CharByteCursor<'a> {
    text: &'a str,
    ascii: bool,
    /// Char index of `byte`.
    char_pos: usize,
    byte: usize,
}

impl<'a> CharByteCursor<'a> {
    // Cost: O(n) once for the ASCII test, n = bytes of `text`.
    pub(crate) fn new(text: &'a str) -> Self {
        CharByteCursor {
            text,
            ascii: text.is_ascii(),
            char_pos: 0,
            byte: 0,
        }
    }

    /// The byte offset of char index `idx`, or `text.len()` past the end.
    /// Asking for an index behind the cursor restarts it from the beginning,
    /// so an out-of-order caller stays correct and only loses the
    /// amortization.
    // Cost: O(1) for an ASCII subject; otherwise amortized O(chars advanced),
    // O(n) in total over an ascending sequence of indices, n = chars of `text`.
    pub(crate) fn byte_of(&mut self, idx: usize) -> usize {
        if self.ascii {
            return idx.min(self.text.len());
        }
        if idx < self.char_pos {
            self.char_pos = 0;
            self.byte = 0;
        }
        let mut chars = self.text[self.byte..].chars();
        while self.char_pos < idx {
            match chars.next() {
                Some(c) => {
                    self.byte += c.len_utf8();
                    self.char_pos += 1;
                }
                None => break,
            }
        }
        self.byte
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn reference(text: &str, idx: usize) -> usize {
        text.char_indices().nth(idx).map_or(text.len(), |(b, _)| b)
    }

    #[test]
    fn matches_a_full_rescan() {
        for text in ["", "abc", "aあbいc", "ああ"] {
            let n = text.chars().count();
            let mut cursor = CharByteCursor::new(text);
            for idx in 0..=n + 2 {
                assert_eq!(cursor.byte_of(idx), reference(text, idx));
            }
            // Out of order restarts correctly.
            let mut cursor = CharByteCursor::new(text);
            for idx in (0..=n + 1).rev() {
                assert_eq!(cursor.byte_of(idx), reference(text, idx));
            }
        }
    }
}
