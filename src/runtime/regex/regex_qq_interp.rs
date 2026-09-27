//! Matching [`RegexAtom::QqInterp`]: a `"..."` atom whose compiled qq thunk
//! result is read from `env` when the atom is reached, not spliced into the
//! pattern text before the parse (see the variant's doc comment).

use super::super::*;

impl Interpreter {
    /// The end of the qq thunk result installed under `key` when it matches
    /// literally at `pos`: `Some(Some(end))` on a match, `Some(None)` on a
    /// mismatch, and `None` when no result is installed (the caller matches
    /// the atom's fallback instead).
    // Cost: O(r), r = the result's length.
    pub(super) fn match_qq_interp_result(
        &self,
        key: Symbol,
        chars: &[char],
        pos: usize,
        ignore_case: bool,
    ) -> Option<Option<usize>> {
        let result = self.env.get_sym(key)?;
        let ValueView::Str(text) = result.view() else {
            return None;
        };
        let mut end = pos;
        for want in text.chars() {
            let Some(&got) = chars.get(end) else {
                return Some(None);
            };
            let same = got == want || (ignore_case && got.to_lowercase().eq(want.to_lowercase()));
            if !same {
                return Some(None);
            }
            end += 1;
        }
        Some(Some(end))
    }
}
