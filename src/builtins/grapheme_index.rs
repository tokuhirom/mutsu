//! A cached grapheme -> byte position index for `Str` payloads (#9140).
//!
//! Raku strings are indexed by grapheme, Rust strings by byte. Converting one
//! to the other used to mean segmenting the whole string on every call
//! (`string_pos::grapheme_units`), so any "walk a string by index" loop —
//! `for ^$s.chars -> $i { $s.substr($i, 1) }` — was O(n^2). Rakudo stores a
//! string as a fixed-width grapheme array, which makes `.chars` O(1) and
//! `.substr($i, $k)` O(k).
//!
//! [`GraphemeIndex`] recovers that: it is built in one O(n) pass and then
//! answers `.chars` in O(1) and grapheme <-> byte conversions in O(STRIDE).
//!
//! * A **flat** string (ASCII with no `\r\n`) has one byte per grapheme, so
//!   the index is just the flag — nothing is stored.
//! * Otherwise it keeps the byte offset of every [`STRIDE`]-th grapheme; a
//!   conversion jumps to the nearest checkpoint and segments at most
//!   `STRIDE - 1` graphemes from there. Segmenting from a checkpoint yields the
//!   same units as segmenting the whole string, because a checkpoint is a
//!   grapheme boundary and UAX #29 never looks back across one.
//!
//! [`with_str_index`] caches the index per `Str` allocation, so repeated calls
//! on the same string pay the build once. The cache holds a [`Weak`] to the
//! payload: the allocation (and therefore its address, the cache key) cannot
//! be reused while the entry lives, and a `Str` payload is never mutated
//! behind a live `Weak` — the in-place append path (`str_appended_nfc`) uses
//! `Arc::get_mut`, which refuses when a `Weak` exists and copies instead.

use crate::value::{Value, ValueView};
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Weak};
use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

/// Graphemes between two stored checkpoints.
const STRIDE: usize = 32;

/// Strings shorter than this are indexed on the spot rather than cached: the
/// build is cheaper than a cache probe, and not holding a `Weak` keeps short
/// accumulators on the in-place append path.
const CACHE_MIN_BYTES: usize = 256;

/// Cached indexes per thread.
const CACHE_SLOTS: usize = 8;

#[derive(Debug)]
pub(crate) struct GraphemeIndex {
    /// One byte per grapheme: byte offsets are grapheme offsets.
    flat: bool,
    /// Number of graphemes.
    len: usize,
    /// `marks[k]` is the byte offset of grapheme `k * STRIDE` (non-flat only).
    marks: Vec<usize>,
}

fn is_utf8_c8_payload(g: &str) -> bool {
    g == "x"
}

fn is_hex_digit(g: &str) -> bool {
    g.len() == 1 && g.as_bytes()[0].is_ascii_hexdigit()
}

/// The units a positional string method indexes, from a grapheme boundary on:
/// extended grapheme clusters, except that a utf8-c8 synthetic
/// (`SYNTHETIC_MARKER x H H`) is one unit. Yields `(byte offset, unit)` with
/// offsets relative to the full string the iterator was started in.
pub(crate) struct Units<'a> {
    s: &'a str,
    inner: GraphemeIndices<'a>,
    base: usize,
    peeked: [Option<(usize, &'a str)>; 3],
}

impl<'a> Units<'a> {
    /// Units of `s` starting at byte `from`, which must be a unit boundary.
    pub(crate) fn from(s: &'a str, from: usize) -> Self {
        Units {
            s,
            inner: s[from..].grapheme_indices(true),
            base: from,
            peeked: [None, None, None],
        }
    }

    fn pull(&mut self) -> Option<(usize, &'a str)> {
        if let Some(g) = self.peeked[0].take() {
            self.peeked.rotate_left(1);
            return Some(g);
        }
        self.inner.next().map(|(i, g)| (i + self.base, g))
    }

    fn peek(&mut self, k: usize) -> Option<(usize, &'a str)> {
        for slot in 0..=k {
            if self.peeked[slot].is_none() {
                self.peeked[slot] = self.inner.next().map(|(i, g)| (i + self.base, g));
                self.peeked[slot]?;
            }
        }
        self.peeked[k]
    }
}

impl<'a> Iterator for Units<'a> {
    type Item = (usize, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        let (start, g) = self.pull()?;
        if g == crate::runtime::utf8_c8::SYNTHETIC_MARKER_STR
            && self.peek(0).is_some_and(|(_, x)| is_utf8_c8_payload(x))
            && self.peek(1).is_some_and(|(_, h)| is_hex_digit(h))
            && let Some((last, h2)) = self.peek(2)
            && is_hex_digit(h2)
        {
            self.pull();
            self.pull();
            self.pull();
            return Some((start, &self.s[start..last + h2.len()]));
        }
        Some((start, g))
    }
}

/// True when `s` has one grapheme per byte. ASCII guarantees one byte per
/// codepoint; the only ASCII sequence that merges two codepoints into one
/// grapheme is `\r\n`.
#[inline]
pub(crate) fn is_flat_ascii(s: &str) -> bool {
    s.is_ascii() && !s.contains("\r\n")
}

impl GraphemeIndex {
    pub(crate) fn build(s: &str) -> Self {
        if is_flat_ascii(s) {
            return GraphemeIndex {
                flat: true,
                len: s.len(),
                marks: Vec::new(),
            };
        }
        let mut marks = Vec::with_capacity(s.len() / (STRIDE * 2) + 1);
        let mut len = 0;
        for (off, _) in Units::from(s, 0) {
            if len % STRIDE == 0 {
                marks.push(off);
            }
            len += 1;
        }
        GraphemeIndex {
            flat: false,
            len,
            marks,
        }
    }

    /// Number of graphemes (`.chars`).
    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.len
    }

    /// Byte offset at which grapheme `g` starts; `s.len()` for `g >= len`.
    pub(crate) fn byte_at(&self, s: &str, g: usize) -> usize {
        if g >= self.len {
            return s.len();
        }
        if self.flat {
            return g;
        }
        let k = g / STRIDE;
        let skip = g - k * STRIDE;
        Units::from(s, self.marks[k])
            .nth(skip)
            .map_or(s.len(), |(off, _)| off)
    }

    /// Byte range of the `count` graphemes starting at grapheme `start`
    /// (both clamped to the string).
    pub(crate) fn byte_range(&self, s: &str, start: usize, count: usize) -> (usize, usize) {
        let b0 = self.byte_at(s, start);
        let end = start.saturating_add(count);
        if self.flat || end >= self.len {
            return (b0, self.byte_at(s, end));
        }
        // Walk forward from `start` when that is shorter than from a checkpoint.
        if count <= STRIDE {
            let b1 = Units::from(s, b0)
                .nth(count)
                .map_or(s.len(), |(off, _)| off);
            return (b0, b1);
        }
        (b0, self.byte_at(s, end))
    }

    /// The grapheme index of byte offset `byte`, or `None` when `byte` does
    /// not fall on a unit boundary. `byte == s.len()` maps to `len`.
    pub(crate) fn grapheme_at_boundary(&self, s: &str, byte: usize) -> Option<usize> {
        if byte >= s.len() {
            return (byte == s.len()).then_some(self.len);
        }
        if self.flat {
            return Some(byte);
        }
        // The last checkpoint at or before `byte`.
        let k = self.marks.partition_point(|&m| m <= byte) - 1;
        let mut g = k * STRIDE;
        for (off, _) in Units::from(s, self.marks[k]) {
            if off == byte {
                return Some(g);
            }
            if off > byte {
                return None;
            }
            g += 1;
        }
        None
    }

    /// The grapheme index containing byte offset `byte` (rounding down to the
    /// start of the grapheme it falls inside).
    pub(crate) fn grapheme_at(&self, s: &str, byte: usize) -> usize {
        if byte >= s.len() {
            return self.len;
        }
        if self.flat {
            return byte;
        }
        let k = self.marks.partition_point(|&m| m <= byte) - 1;
        let mut g = k * STRIDE;
        for (off, unit) in Units::from(s, self.marks[k]) {
            if byte < off + unit.len() {
                return g;
            }
            g += 1;
        }
        self.len
    }
}

type Slot = (Weak<String>, Rc<GraphemeIndex>);

thread_local! {
    static CACHE: RefCell<Vec<Slot>> = const { RefCell::new(Vec::new()) };
}

/// The index of an `Arc<String>` payload, from the per-thread cache.
pub(crate) fn index_of_arc(arc: &Arc<String>) -> Rc<GraphemeIndex> {
    if arc.len() < CACHE_MIN_BYTES {
        return Rc::new(GraphemeIndex::build(arc));
    }
    let ptr = Arc::as_ptr(arc);
    let hit = CACHE.with(|c| {
        let mut c = c.borrow_mut();
        let pos = c
            .iter()
            .position(|(w, _)| w.as_ptr() == ptr && w.strong_count() > 0)?;
        // Move to front: the most recently used string is probed first.
        let slot = c.remove(pos);
        let idx = slot.1.clone();
        c.insert(0, slot);
        Some(idx)
    });
    if let Some(idx) = hit {
        return idx;
    }
    let idx = Rc::new(GraphemeIndex::build(arc));
    CACHE.with(|c| {
        let mut c = c.borrow_mut();
        // Drop entries whose string is gone, then the least recently used.
        c.retain(|(w, _)| w.strong_count() > 0);
        if c.len() >= CACHE_SLOTS {
            c.pop();
        }
        c.insert(0, (Arc::downgrade(arc), idx.clone()));
    });
    idx
}

/// Run `f` over `v`'s string form and its grapheme index. A `Str` value is
/// borrowed (no copy) and its index comes from the cache; anything else is
/// stringified and indexed on the spot.
pub(crate) fn with_str_index<R>(v: &Value, f: impl FnOnce(&str, &GraphemeIndex) -> R) -> R {
    if let ValueView::Str(arc) = v.view() {
        let idx = index_of_arc(&arc);
        return f(arc.as_str(), &idx);
    }
    let s = v.to_string_value();
    let idx = GraphemeIndex::build(&s);
    f(&s, &idx)
}

/// `v`'s string form and its grapheme index, owned — for callers that need
/// both across a call back into the interpreter.
pub(crate) fn str_and_index(v: &Value) -> (Arc<String>, Rc<GraphemeIndex>) {
    if let ValueView::Str(arc) = v.view() {
        let arc: Arc<String> = Arc::clone(&arc);
        let idx = index_of_arc(&arc);
        return (arc, idx);
    }
    let s = v.to_string_value();
    let idx = Rc::new(GraphemeIndex::build(&s));
    (Arc::new(s), idx)
}

/// Run `f` over `v`'s string form, borrowing a `Str` payload instead of
/// copying it.
pub(crate) fn with_str<R>(v: &Value, f: impl FnOnce(&str) -> R) -> R {
    if let ValueView::Str(arc) = v.view() {
        return f(arc.as_str());
    }
    f(&v.to_string_value())
}

/// Byte offset of the first occurrence of `needle` in `text` at or after byte
/// `from` that starts and ends on grapheme boundaries — the hit `.index`
/// reports. A byte-level hit inside a grapheme (`"q\x[301]".index("q")`) is
/// skipped, as in Rakudo, whose strings are grapheme arrays.
pub(crate) fn find_graphemes(
    text: &str,
    idx: &GraphemeIndex,
    from: usize,
    needle: &str,
) -> Option<usize> {
    let mut at = from;
    loop {
        let b = at + text[at..].find(needle)?;
        if idx.grapheme_at_boundary(text, b).is_some()
            && idx.grapheme_at_boundary(text, b + needle.len()).is_some()
        {
            return Some(b);
        }
        // Resume just past this hit's first char.
        at = b + text[b..].chars().next().map_or(1, char::len_utf8);
        if at > text.len() {
            return None;
        }
    }
}

/// Byte offset of the last occurrence of `needle` that starts at or before
/// byte `max_start` and starts and ends on grapheme boundaries (`.rindex`).
pub(crate) fn rfind_graphemes(
    text: &str,
    idx: &GraphemeIndex,
    max_start: usize,
    needle: &str,
) -> Option<usize> {
    let mut limit =
        text.floor_char_boundary(max_start.saturating_add(needle.len()).min(text.len()));
    loop {
        let b = text[..limit].rfind(needle)?;
        if idx.grapheme_at_boundary(text, b).is_some()
            && idx.grapheme_at_boundary(text, b + needle.len()).is_some()
        {
            return Some(b);
        }
        // Any earlier hit ends before this one does.
        match (b + needle.len()).checked_sub(1) {
            Some(l) => limit = text.floor_char_boundary(l),
            None => return None,
        }
    }
}

/// The graphemes of `text` that `substr-eq($needle, $start)` compares against
/// `needle`: as many as `needle` has, starting at grapheme `start`. `None`
/// when `start` is past the end.
pub(crate) fn substr_eq_window<'a>(
    text: &'a str,
    idx: &GraphemeIndex,
    start: usize,
    needle: &str,
) -> Option<&'a str> {
    if start > idx.len() {
        return None;
    }
    let count = crate::builtins::string_pos::grapheme_len(needle);
    let (b0, b1) = idx.byte_range(text, start, count);
    Some(&text[b0..b1])
}

/// The suffix of `text` from grapheme `start` on (`contains`/`index` with a
/// position). `None` when `start` is past the end.
pub(crate) fn suffix_from<'a>(text: &'a str, idx: &GraphemeIndex, start: usize) -> Option<&'a str> {
    (start <= idx.len()).then(|| &text[idx.byte_at(text, start)..])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins::string_pos::grapheme_units;

    fn check_against_units(s: &str) {
        let idx = GraphemeIndex::build(s);
        let units = grapheme_units(s);
        assert_eq!(idx.len(), units.len(), "len of {s:?}");
        let mut off = 0;
        for (g, u) in units.iter().enumerate() {
            assert_eq!(idx.byte_at(s, g), off, "byte_at({g}) of {s:?}");
            assert_eq!(idx.grapheme_at_boundary(s, off), Some(g));
            for b in off..off + u.len() {
                assert_eq!(idx.grapheme_at(s, b), g);
                if b > off {
                    assert_eq!(idx.grapheme_at_boundary(s, b), None);
                }
            }
            off += u.len();
        }
        assert_eq!(idx.byte_at(s, units.len()), s.len());
        assert_eq!(idx.grapheme_at_boundary(s, s.len()), Some(units.len()));
        for start in 0..=units.len() {
            for count in [0, 1, 2, STRIDE, STRIDE + 3, usize::MAX] {
                let end = start.saturating_add(count).min(units.len());
                let want = units[start..end].concat();
                let (b0, b1) = idx.byte_range(s, start, count);
                assert_eq!(&s[b0..b1], want, "range {start}+{count} of {s:?}");
            }
        }
    }

    #[test]
    fn flat_ascii() {
        check_against_units("");
        check_against_units("hello world");
        check_against_units(&"abc".repeat(50));
    }

    #[test]
    fn crlf_and_combining_marks() {
        check_against_units(&"a\r\nb".repeat(40));
        check_against_units(&"q\u{301}xあ\u{1F1EF}\u{1F1F5}".repeat(30));
        check_against_units(&"👨\u{200D}👩\u{200D}👧!".repeat(25));
    }

    #[test]
    fn utf8_c8_synthetic_is_one_unit() {
        let syn = format!("{}xFE", crate::runtime::utf8_c8::SYNTHETIC_MARKER_STR);
        let s = format!("a{syn}b").repeat(20);
        check_against_units(&s);
        assert_eq!(GraphemeIndex::build(&s).len(), 60);
    }

    #[test]
    fn cache_returns_same_index_while_string_lives() {
        let arc = Arc::new("あ".repeat(200));
        let a = index_of_arc(&arc);
        let b = index_of_arc(&arc);
        assert!(Rc::ptr_eq(&a, &b));
        assert_eq!(a.len(), 200);
    }
}
