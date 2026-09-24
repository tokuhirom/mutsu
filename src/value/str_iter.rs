//! Native string iterators behind the lazy `Seq`s that `Str.comb`, `.lines`
//! and `.words` return.
//!
//! This follows Rakudo. In the setting, `Str.comb` is
//! `Seq.new(<iterator class>.new(self))`, and every matcher-less `comb`,
//! `comb(Int)`, `comb(Str)`, `lines` and `words` form has a dedicated
//! `Iterator` class holding `$!str` and a `$!pos` cursor. Its `pull-one` cuts
//! the next piece with `substr` (plus `findcclass` / `index` for lines and
//! needles), `count-only` counts the rest without building any strings, and
//! `sink-all` just moves the cursor to the end. So `.comb.head(3)` costs
//! O(prefix), and `.lines.elems` allocates nothing.
//!
//! [`StrIterSpec`] is that iterator. It is a byte cursor over the shared
//! `Arc<String>` payload of the invocant, so building one copies nothing. It
//! needs no interpreter state, which means any layer can drive it: the
//! `Seq` holding it (`SeqSource::StrIter`) is cut on its first read by
//! whoever reads it, so every existing consumer keeps seeing an ordinary
//! eager `Seq`, and only a consuming `.head(n)` / `.first` on an unread one
//! stops after the prefix.

use crate::value::Value;
use std::sync::Arc;
use unicode_segmentation::{GraphemeCursor, UnicodeSegmentation};

/// What a [`StrIterSpec`] cuts its string into.
#[derive(Debug, Clone)]
pub(crate) enum StrIterMode {
    /// `.comb`, `.comb(1)` and `.comb("")`: one grapheme per element.
    Graphemes,
    /// `.comb($n)`: runs of `n` graphemes (`n >= 2`; 1 is `Graphemes`), and
    /// a shorter last run.
    Chunks(usize),
    /// `.comb($needle)` with a non-empty needle: each non-overlapping
    /// occurrence of the needle, left to right.
    Needle(Arc<String>),
    /// `.lines`: each line, with the `\n` / `\r\n` / `\r` separator dropped
    /// when `chomp` is set. A trailing separator does not start an empty
    /// last line.
    Lines { chomp: bool },
    /// `.words`: each maximal run of non-whitespace characters.
    Words,
}

/// Cursor state of one lazy `Str` iterator (see the module docs).
#[derive(Debug, Clone)]
pub(crate) struct StrIterSpec {
    /// The invocant's payload, shared with the `Str` value it came from.
    text: Arc<String>,
    /// Byte offset of the first unread byte (always a char boundary).
    pos: usize,
    mode: StrIterMode,
    /// Elements still allowed by a `$limit` argument; `None` means no limit.
    remaining: Option<usize>,
}

impl StrIterSpec {
    // Cost: O(1), the payload is shared, not copied.
    pub(crate) fn new(text: Arc<String>, mode: StrIterMode, limit: Option<usize>) -> Self {
        Self {
            text,
            pos: 0,
            mode,
            remaining: limit,
        }
    }

    /// Byte offset just past the grapheme that starts at `from`.
    // Cost: O(g), g = bytes of that grapheme.
    fn grapheme_end(&self, from: usize) -> usize {
        let len = self.text.len();
        // The chunk handed to the cursor is the whole string, starting at 0,
        // so it never asks for pre-context (regional-indicator pairs and the
        // like see everything before `from`).
        let mut cursor = GraphemeCursor::new(from, len, true);
        match cursor.next_boundary(&self.text, 0) {
            Ok(Some(end)) => end,
            _ => self.text[from..]
                .graphemes(true)
                .next()
                .map_or(len, |g| from + g.len()),
        }
    }

    /// Advance past the next element and return its byte range, or `None`
    /// once the string (or the limit) is used up.
    // Cost: O(k), k = bytes the cursor moves over (the element plus any
    // separator or whitespace skipped before the next one).
    fn advance(&mut self) -> Option<(usize, usize)> {
        if self.remaining == Some(0) {
            return None;
        }
        let len = self.text.len();
        let range = match &self.mode {
            StrIterMode::Graphemes => {
                if self.pos >= len {
                    return None;
                }
                let end = self.grapheme_end(self.pos);
                let range = (self.pos, end);
                self.pos = end;
                range
            }
            StrIterMode::Chunks(n) => {
                if self.pos >= len {
                    return None;
                }
                let start = self.pos;
                let mut end = start;
                for _ in 0..*n {
                    if end >= len {
                        break;
                    }
                    end = self.grapheme_end(end);
                }
                self.pos = end;
                (start, end)
            }
            StrIterMode::Needle(needle) => {
                if self.pos > len {
                    return None;
                }
                match self.text[self.pos..].find(needle.as_str()) {
                    Some(off) => {
                        let start = self.pos + off;
                        let end = start + needle.len();
                        self.pos = end;
                        (start, end)
                    }
                    None => {
                        // Past the end, so a later call stops at once instead
                        // of searching the tail again.
                        self.pos = len + 1;
                        return None;
                    }
                }
            }
            StrIterMode::Lines { chomp } => {
                if self.pos >= len {
                    return None;
                }
                let bytes = self.text.as_bytes();
                let start = self.pos;
                match bytes[start..]
                    .iter()
                    .position(|&b| b == b'\n' || b == b'\r')
                {
                    Some(off) => {
                        let sep_at = start + off;
                        let sep_len =
                            if bytes[sep_at] == b'\r' && bytes.get(sep_at + 1) == Some(&b'\n') {
                                2
                            } else {
                                1
                            };
                        self.pos = sep_at + sep_len;
                        (start, if *chomp { sep_at } else { self.pos })
                    }
                    None => {
                        self.pos = len;
                        (start, len)
                    }
                }
            }
            StrIterMode::Words => {
                let rest = &self.text[self.pos..];
                let skip = rest
                    .char_indices()
                    .find(|(_, c)| !c.is_whitespace())
                    .map(|(i, _)| i)?;
                let start = self.pos + skip;
                let end = self.text[start..]
                    .char_indices()
                    .find(|(_, c)| c.is_whitespace())
                    .map_or(len, |(i, _)| start + i);
                self.pos = end;
                (start, end)
            }
        };
        if let Some(r) = self.remaining.as_mut() {
            *r -= 1;
        }
        Some(range)
    }

    /// Rakudo's `pull-one`: the next element, or `None` at the end.
    // Cost: O(k), k = bytes the cursor moves over, plus the copy of the element.
    pub(crate) fn pull_one(&mut self) -> Option<Value> {
        let (start, end) = self.advance()?;
        Some(Value::str(self.text[start..end].to_string()))
    }

    /// Append up to `n` more elements to `out` (Rakudo's `push-exactly`;
    /// `usize::MAX` is `push-all`).
    // Cost: O(k), k = bytes the cursor moves over, plus the copies of the
    // pushed elements.
    pub(crate) fn push_up_to(&mut self, out: &mut Vec<Value>, n: usize) {
        for _ in 0..n {
            match self.pull_one() {
                Some(v) => out.push(v),
                None => return,
            }
        }
    }

    /// Rakudo's `count-only`: how many elements are left. The count is taken
    /// on a copy of the cursor and builds no strings.
    // Cost: O(r), r = bytes left after the cursor.
    pub(crate) fn count_only(&self) -> usize {
        let mut probe = self.clone();
        let mut count = 0usize;
        while probe.advance().is_some() {
            count += 1;
        }
        count
    }
}

/// The invocant's payload as a shared `Arc<String>`: a `Str` hands out its
/// own `Arc` (no copy), anything else is stringified once.
// Cost: O(1) for a `Str`; O(n) to stringify anything else.
fn shared_text(target: &Value) -> Arc<String> {
    match target.view() {
        crate::value::ValueView::Str(s) => Arc::clone(&s),
        _ => Arc::new(target.to_string_value()),
    }
}

/// The `Seq` that `Str.comb` / `.lines` / `.words` return: a deferred body
/// ([`crate::value::SeqSource::StrIter`]) holding a [`StrIterSpec`] over
/// `target`'s string. Nothing is cut until the Seq is read, and a consuming
/// `.head(n)` / `.first` cuts only the prefix it needs.
// Cost: O(1) for a `Str` invocant.
pub(crate) fn str_iter_seq(target: &Value, mode: StrIterMode, limit: Option<usize>) -> Value {
    let spec = StrIterSpec::new(shared_text(target), mode, limit);
    Value::seq_deferred(crate::value::SeqSource::StrIter(spec))
}

/// How many elements `str_iter_seq(target, mode, limit)` would produce,
/// counted without building any of them (`.lines(:count)`).
// Cost: O(n), n = bytes of the string.
pub(crate) fn str_iter_count(target: &Value, mode: StrIterMode, limit: Option<usize>) -> usize {
    StrIterSpec::new(shared_text(target), mode, limit).count_only()
}

/// Read the `$limit` argument of `lines` / `words` / `comb`: `Some(Some(n))`
/// caps the Seq at `n` elements, `Some(None)` means no cap (`*`, `Inf`), and
/// `None` means the value is not a limit this layer understands.
// Cost: O(1).
pub(crate) fn parse_limit(arg: &Value) -> Option<Option<usize>> {
    use crate::value::ValueView;
    match arg.view() {
        ValueView::Int(i) => Some(Some(i.max(0) as usize)),
        ValueView::BigInt(bi) => {
            use num_traits::{Signed, ToPrimitive};
            if bi.is_negative() {
                Some(Some(0))
            } else {
                Some(Some(bi.to_usize().unwrap_or(usize::MAX)))
            }
        }
        ValueView::Whatever => Some(None),
        ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => Some(None),
        ValueView::Num(f) if f >= 0.0 => Some(Some(f as usize)),
        ValueView::Rat(n, d) if d == 0 && n > 0 => Some(None),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn all(text: &str, mode: StrIterMode, limit: Option<usize>) -> Vec<String> {
        let mut it = StrIterSpec::new(Arc::new(text.to_string()), mode, limit);
        let mut out = Vec::new();
        while let Some(v) = it.pull_one() {
            out.push(v.to_string_value());
        }
        out
    }

    #[test]
    fn graphemes_keep_clusters_whole() {
        assert_eq!(
            all(
                "ae\u{301}\u{1F1EF}\u{1F1F5}\r\n",
                StrIterMode::Graphemes,
                None
            ),
            vec!["a", "e\u{301}", "\u{1F1EF}\u{1F1F5}", "\r\n"]
        );
    }

    #[test]
    fn chunks_and_limit() {
        assert_eq!(
            all("abcde", StrIterMode::Chunks(2), None),
            vec!["ab", "cd", "e"]
        );
        assert_eq!(
            all("abcde", StrIterMode::Chunks(2), Some(2)),
            vec!["ab", "cd"]
        );
        assert!(all("abc", StrIterMode::Graphemes, Some(0)).is_empty());
    }

    #[test]
    fn needle_is_non_overlapping() {
        let needle = StrIterMode::Needle(Arc::new("aa".to_string()));
        assert_eq!(all("aaaaa", needle, None), vec!["aa", "aa"]);
    }

    #[test]
    fn lines_match_split_lines() {
        for text in ["a\nb\r\nc\rd", "a\n", "\n\n", "", "x"] {
            let expected = crate::builtins::split_lines_with_chomp(text, true);
            assert_eq!(
                all(text, StrIterMode::Lines { chomp: true }, None),
                expected
            );
            let raw = crate::builtins::split_lines_with_chomp(text, false);
            assert_eq!(all(text, StrIterMode::Lines { chomp: false }, None), raw);
        }
    }

    #[test]
    fn words_match_split_whitespace() {
        let text = "  foo\tbar \u{3000}baz  ";
        let expected: Vec<&str> = text.split_whitespace().collect();
        assert_eq!(all(text, StrIterMode::Words, None), expected);
    }

    #[test]
    fn count_only_does_not_consume() {
        let mut it = StrIterSpec::new(Arc::new("a b c".to_string()), StrIterMode::Words, None);
        it.pull_one();
        assert_eq!(it.count_only(), 2);
        assert_eq!(it.pull_one().map(|v| v.to_string_value()), Some("b".into()));
    }
}
