//! The one implementation of each `Str` primitive (ADR-0117).
//!
//! Rakudo's `Str` methods are thin wrappers over the MoarVM string ops:
//! `.chars` is `nqp::chars`, `.substr` is `nqp::substr`, `.index(:i)` is
//! `nqp::indexic`, `.starts-with` is `nqp::eqat`, `.flip` is `nqp::flip`,
//! infix `x` is `nqp::x`, and so on. There is exactly one routine per
//! primitive, so the method and the op cannot disagree.
//!
//! mutsu used to keep three independent copies instead: the `Str` methods
//! (grapheme-indexed, on `grapheme_index`), the `nqp::` ops
//! (codepoint-indexed, on their own `Vec<char>` memo) and TRIR's typed string
//! ops (codepoint-indexed again, on a third memo). They drifted: `nqp::chars`
//! counted `\r\n` as two, `nqp::index` reported codepoint offsets,
//! `nqp::flip` reversed codepoints, and `.index(:i)` lowercased where
//! `nqp::indexic` foldcased -- every one a divergence from rakudo.
//!
//! So this module is the single home. Every position is a **grapheme** index
//! (MoarVM strings are NFG, so `nqp::chars("\r\n")` is 1), resolved through
//! the cached [`GraphemeIndex`]; the `Str` method, the `nqp::` op, the VM
//! opcode and the TRIR op all call the routine here and differ only in how
//! they report an edge case (a `Failure` vs. `-1` vs. a native int).
//! `scripts/check-prims.sh` keeps new copies from growing back.

mod build;
mod fold;
mod strands;

pub(crate) use build::{Normal, concat, flip, normalize, repeat};
pub(crate) use fold::{Fold, eq_at, find};
pub(crate) use strands::Joiner;

use crate::builtins::grapheme_index::{GraphemeIndex, Units, with_str_index};
use crate::value::{RuntimeError, Value};

/// The number of graphemes in `v`'s string form (`.chars`, `nqp::chars`).
///
/// Cost: O(1) amortized for a cached `Str` (the index is built once in O(n)
/// and cached per payload, see `grapheme_index`); O(n) otherwise.
pub(crate) fn chars(v: &Value) -> usize {
    with_str_index(v, |_, idx| idx.len())
}

/// `text[start .. start + count]` in graphemes, both clamped to the string.
///
/// Cost: O(count) amortized once `idx` is built (see `GraphemeIndex::byte_range`).
pub(crate) fn slice<'a>(text: &'a str, idx: &GraphemeIndex, start: usize, count: usize) -> &'a str {
    let (b0, b1) = idx.byte_range(text, start, count);
    &text[b0..b1]
}

/// `nqp::substr($s, $from, $want)` with MoarVM's argument rules: a negative
/// `$from` counts from the end, a negative (or absent) `$want` means "to the
/// end", a range past either end is clamped, and only a window whose *end*
/// lands before the start of the string is an error.
///
/// Cost: O(k) amortized, k = graphemes returned.
pub(crate) fn nqp_substr(v: &Value, from: i64, want: Option<i64>) -> Result<Value, RuntimeError> {
    with_str_index(v, |text, idx| {
        let len = idx.len() as i64;
        let start = if from < 0 { len + from } else { from };
        let end = match want {
            Some(w) if w >= 0 => start.saturating_add(w),
            _ => len,
        };
        if end < 0 {
            return Err(RuntimeError::new(format!(
                "Substring end ({end}) cannot be less than 0"
            )));
        }
        let start = start.clamp(0, len) as usize;
        let end = end.clamp(0, len) as usize;
        if end <= start {
            return Ok(Value::str(String::new()));
        }
        Ok(Value::str(slice(text, idx, start, end - start).to_string()))
    })
}

/// The grapheme index of the first occurrence of `needle` at or after
/// grapheme `from`, compared under `fold`. A hit must start and end on
/// grapheme boundaries (`"q\x[301]".index("q")` is no hit). An empty needle
/// matches at `from`. `None` when `from` is past the end.
///
/// Cost: O((n - from) * m) worst case, n = graphemes of `text`, m = chars of
/// `needle`, plus an O(n - from) fold of the haystack under a non-exact `fold`.
pub(crate) fn index(
    text: &str,
    idx: &GraphemeIndex,
    from: usize,
    needle: &str,
    fold: Fold,
) -> Option<usize> {
    if from > idx.len() {
        return None;
    }
    if needle.is_empty() {
        return Some(from);
    }
    if fold.is_exact() {
        let at = idx.byte_at(text, from);
        return crate::builtins::grapheme_index::find_graphemes(text, idx, at, needle)
            .map(|b| idx.grapheme_at(text, b));
    }
    find(text, idx, from, needle, fold)
}

/// The grapheme index of the last occurrence of `needle` that starts at or
/// before grapheme `max_start` (both ends on grapheme boundaries). An empty
/// needle matches at `max_start` clamped to the length.
///
/// Cost: O((max_start - p) * m) worst case, p = the hit, m = chars of `needle`.
pub(crate) fn rindex(
    text: &str,
    idx: &GraphemeIndex,
    max_start: usize,
    needle: &str,
) -> Option<usize> {
    let max_start = max_start.min(idx.len());
    if needle.is_empty() {
        return Some(max_start);
    }
    let byte = idx.byte_at(text, max_start);
    crate::builtins::grapheme_index::rfind_graphemes(text, idx, byte, needle)
        .map(|b| idx.grapheme_at(text, b))
}

/// `nqp::index` / `nqp::indexic` / `nqp::indexim` / `nqp::indexicim`: the hit
/// as an int, **-1** when absent or when `$from` is outside `0..chars`.
///
/// Cost: see [`index`].
pub(crate) fn nqp_index(v: &Value, needle: &str, from: i64, fold: Fold) -> i64 {
    with_str_index(v, |text, idx| {
        let Ok(from) = usize::try_from(from) else {
            return -1;
        };
        index(text, idx, from, needle, fold).map_or(-1, |g| g as i64)
    })
}

/// `nqp::rindex($s, $needle, $from?)`: -1 when absent. A negative (or absent)
/// `$from` searches from the end; one past the end dies, as in MoarVM --
/// except for an empty needle, which just answers -1.
///
/// Cost: see [`rindex`].
pub(crate) fn nqp_rindex(v: &Value, needle: &str, from: Option<i64>) -> Result<i64, RuntimeError> {
    with_str_index(v, |text, idx| {
        let len = idx.len();
        let max_start = match from {
            Some(f) if f >= 0 => f as usize,
            _ => len,
        };
        if max_start > len {
            if needle.is_empty() {
                return Ok(-1);
            }
            return Err(RuntimeError::new(format!(
                "index start offset ({max_start}) out of range (0..{len})"
            )));
        }
        Ok(rindex(text, idx, max_start, needle).map_or(-1, |g| g as i64))
    })
}

/// `nqp::eqat` / `nqp::eqatic` (and the `Str` methods built on them): whether
/// `needle` occurs at exactly grapheme `pos`, under `fold`. A negative `pos`
/// counts from the end.
///
/// Cost: O(m) amortized, m = chars of `needle`.
pub(crate) fn nqp_eqat(v: &Value, needle: &str, pos: i64, fold: Fold) -> bool {
    with_str_index(v, |text, idx| {
        let len = idx.len() as i64;
        let pos = if pos < 0 { pos + len } else { pos };
        usize::try_from(pos).is_ok_and(|p| eq_at(text, idx, p, needle, fold))
    })
}

/// `.contains($needle, $from, :i, :m)`: whether [`index`] finds `needle` at
/// or after grapheme `from`. A junction of needles answers a junction.
///
/// Cost: see [`index`], once per needle.
pub(crate) fn contains(
    text: &str,
    idx: &GraphemeIndex,
    from: usize,
    needle: &Value,
    fold: Fold,
) -> Value {
    if let crate::value::ValueView::Junction { kind, values } = needle.view() {
        let mapped = values
            .iter()
            .map(|v| contains(text, idx, from, v, fold))
            .collect::<Vec<_>>();
        return Value::junction(kind, mapped);
    }
    let needle = needle.to_string_value();
    Value::truth(index(text, idx, from, &needle, fold).is_some())
}

/// `.starts-with` / `.ends-with` (with their `:i`/`:m` adverbs): `needle`
/// at grapheme 0, or at `chars - needle.chars`, exactly as Rakudo calls
/// `nqp::eqat`/`nqp::eqatic` for them.
///
/// Cost: O(m) amortized, m = chars of `needle`.
pub(crate) fn affix_matches(v: &Value, needle: &str, is_prefix: bool, fold: Fold) -> bool {
    with_str_index(v, |text, idx| {
        if is_prefix {
            return eq_at(text, idx, 0, needle, fold);
        }
        let n = crate::builtins::string_pos::grapheme_len(needle);
        idx.len()
            .checked_sub(n)
            .is_some_and(|pos| eq_at(text, idx, pos, needle, fold))
    })
}

/// The codepoint `nqp::ordat` / `nqp::iscclass` see at grapheme `g`: the
/// grapheme's first codepoint in NFC (a synthetic grapheme answers its base,
/// `"\r\n"` answers `\r`). `None` past the end.
///
/// Cost: O(1) amortized for a flat string, O(STRIDE) otherwise.
pub(crate) fn char_at(text: &str, idx: &GraphemeIndex, g: usize) -> Option<char> {
    if g >= idx.len() {
        return None;
    }
    let b = idx.byte_at(text, g);
    if idx.is_flat() {
        return Some(text.as_bytes()[b] as char);
    }
    Units::from(text, b).next().map(|(_, unit)| unit_char(unit))
}

/// A grapheme's first codepoint in NFC. A single-byte unit is its own
/// answer; anything longer may recompose (`"e\x[301]"` answers U+E9).
fn unit_char(unit: &str) -> char {
    if unit.len() == 1 {
        return unit.as_bytes()[0] as char;
    }
    use unicode_normalization::UnicodeNormalization;
    unit.nfc().next().unwrap_or('\0')
}

/// `nqp::ordat($s, $pos)`: [`char_at`] as an int, -1 outside the string.
pub(crate) fn nqp_ordat(v: &Value, pos: i64) -> i64 {
    with_str_index(v, |text, idx| {
        usize::try_from(pos)
            .ok()
            .and_then(|p| char_at(text, idx, p))
            .map_or(-1, |c| c as i64)
    })
}

/// The first grapheme in `from .. from + count` (clamped) whose [`char_at`]
/// satisfies `want`, or the window's end when none does -- the contract of
/// `nqp::findcclass` / `nqp::findnotcclass`.
///
/// Cost: O(d), d = graphemes scanned.
pub(crate) fn find_char(
    v: &Value,
    from: i64,
    count: i64,
    mut want: impl FnMut(char) -> bool,
) -> usize {
    with_str_index(v, |text, idx| {
        let len = idx.len();
        let start = (from.max(0) as usize).min(len);
        let end = start.saturating_add(count.max(0) as usize).min(len);
        // A flat string has one ASCII byte per grapheme, so the window is a
        // byte range and each byte is its own `unit_char`. Segmenting it
        // would build a UAX #29 cursor per grapheme to learn exactly that.
        if idx.is_flat() {
            return text.as_bytes()[start..end]
                .iter()
                .position(|&b| want(b as char))
                .map_or(end, |i| start + i);
        }
        let b = idx.byte_at(text, start);
        for (g, (_, unit)) in (start..end).zip(Units::from(text, b)) {
            if want(unit_char(unit)) {
                return g;
            }
        }
        end
    })
}

/// Each grapheme of `s` as the codepoint [`char_at`] reports for it, so a
/// scanner can index the result by grapheme position (`nqp::radix`).
///
/// Cost: O(n), n = chars of `s`.
pub(crate) fn grapheme_base_chars(s: &str) -> Vec<char> {
    if crate::builtins::grapheme_index::is_flat_ascii(s) {
        return s.bytes().map(char::from).collect();
    }
    Units::from(s, 0).map(|(_, u)| unit_char(u)).collect()
}

/// `v`'s graphemes as separate strings (`nqp::split("", $s)`, `.comb`).
///
/// Cost: O(n), n = chars of `v`.
pub(crate) fn graphemes(v: &Value) -> Vec<Value> {
    crate::builtins::grapheme_index::with_str(v, |s| {
        Units::from(s, 0)
            .map(|(_, u)| Value::str(u.to_string()))
            .collect()
    })
}

#[cfg(test)]
#[path = "tests.rs"]
mod tests;
