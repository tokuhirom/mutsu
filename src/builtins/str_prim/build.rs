//! The string-building primitives: `~` / `nqp::concat`, `x` / `nqp::x` and
//! `.flip` / `nqp::flip`. Each result is renormalized to NFC, because a
//! combining mark at one operand's edge may compose with its neighbour --
//! MoarVM strings are NFG, so `nqp::concat("e", "\x[301]")` is one grapheme.

use unicode_normalization::UnicodeNormalization;

use std::sync::Arc;

use crate::value::{RuntimeError, StrBody, Value};

/// A string built by joining pieces, in NFC.
pub(super) fn nfc_value(s: String) -> Value {
    if s.is_ascii() {
        Value::str(s)
    } else {
        Value::str(s.nfc().collect::<String>())
    }
}

/// `left ~ right` (`infix:<~>`, `nqp::concat`), both stringified.
///
/// A plain `Str` on the left appends through the same primitive as the fused
/// `ConcatAssignLocal`: the left buffer is grown in place when this value is
/// its only holder (copied otherwise), and NFC is restored by looking at the
/// suffix and a bounded window around the join rather than by renormalizing
/// the whole result (#9141).
///
/// A result of at least `STRAND_MIN_BYTES` whose left operand is shared (or
/// is itself a strand list) is built as a strand list instead (ADR-0120):
/// neither operand's characters are copied until the result is read.
///
/// Cost: O(n2) amortized when `left` is an unshared flat `Str`; O(1) when the
/// result is built as strands; else O(n1 + n2), n1, n2 = chars of the
/// operands (a small result, or a join that has to be renormalized).
pub(crate) fn concat(left: Value, right: &Value) -> Value {
    use crate::value::ValueView;
    let left = match super::strands::concat(left, right) {
        Ok(joined) => return joined,
        Err(left) => left,
    };
    if let ValueView::Str(_) = left.view() {
        if let ValueView::Str(suffix) = right.view() {
            let plan = crate::value::StrAppendPlan::for_suffix(suffix.as_str());
            return left.str_appended_nfc(&plan);
        }
        let suffix = crate::runtime::utils::coerce_to_str(right);
        let plan = crate::value::StrAppendPlan::for_suffix(&suffix);
        return left.str_appended_nfc(&plan);
    }
    let mut s = crate::runtime::utils::coerce_to_str(&left);
    s.push_str(&crate::runtime::utils::coerce_to_str(right));
    nfc_value(s)
}

/// Rakudo's cap on the size of a string, in graphemes.
const MAX_GRAPHEMES: usize = 4_294_967_295;

/// `src x n` (`infix:<x>`, its reduction and `nqp::x`). The caller has
/// already clamped or rejected a negative count.
///
/// A result of at least `STRAND_MIN_BYTES` is one repeat strand over `src`'s
/// payload (ADR-0120): nothing is written out until the result is read.
/// Rakudo's grapheme cap is checked up front, so an oversized request dies
/// with a catchable error instead of an allocation failure at first read.
///
/// Cost: O(n), n = chars of `src` (the NFC and size checks); O(n * c), c =
/// repeat count, only for a result that has to be built flat (smaller than
/// `STRAND_MIN_BYTES`, or a copy that composes with the one before it).
pub(crate) fn repeat(src: &Value, n: usize) -> Result<Value, RuntimeError> {
    if n > MAX_GRAPHEMES {
        return Err(RuntimeError::new(format!(
            "Repeat count ({n}) cannot be greater than max allowed number of graphemes {MAX_GRAPHEMES}"
        )));
    }
    let body: Arc<StrBody> = match src.view() {
        crate::value::ValueView::Str(arc) => Arc::clone(&arc),
        _ => Arc::new(StrBody::from(crate::runtime::utils::coerce_to_str(src))),
    };
    // A grapheme is at least one byte, so only a result over the cap in
    // bytes can be over it in graphemes; count them only then.
    if body.byte_len().saturating_mul(n) > MAX_GRAPHEMES {
        let graphemes = super::chars(&Value::str_arc(Arc::clone(&body)));
        if graphemes.saturating_mul(n) > MAX_GRAPHEMES {
            return Err(RuntimeError::new(format!(
                "Can't repeat string, required number of graphemes ({graphemes} * {n}) greater than max allowed of {MAX_GRAPHEMES}"
            )));
        }
    }
    if let Some(lazy) = super::strands::repeat(&body, n) {
        return Ok(lazy);
    }
    repeat_flat(&body, n)
}

/// `src x n`, written out.
///
/// Cost: O(n * c), n = chars of `src`, c = repeat count (plus an NFC pass
/// over a non-ASCII result whose copies compose).
fn repeat_flat(src: &str, n: usize) -> Result<Value, RuntimeError> {
    let total = src
        .len()
        .checked_mul(n)
        .ok_or_else(|| RuntimeError::new("Cannot repeat string: length overflow"))?;
    // Build by doubling (`extend_from_within` = one memcpy per doubling)
    // instead of `n` per-copy `push_str` calls.
    let mut buf: Vec<u8> = Vec::new();
    buf.try_reserve_exact(total).map_err(|_| {
        RuntimeError::new(format!(
            "Cannot repeat string to {total} bytes: memory allocation failed"
        ))
    })?;
    if total > 0 {
        buf.extend_from_slice(src.as_bytes());
        while buf.len() < total {
            let take = (total - buf.len()).min(buf.len());
            buf.extend_from_within(..take);
        }
    }
    // SAFETY: `buf` is `src.as_bytes()` (valid UTF-8) repeated whole times;
    // a concatenation of valid UTF-8 strings is valid UTF-8.
    let repeated = unsafe { String::from_utf8_unchecked(buf) };
    // NFC is local: when `src` is itself NFC and starts at a normalization
    // boundary, no copy can compose or reorder with the one before it, so
    // the repetition is already NFC (#9141).
    Ok(if super::strands::repeats_as_nfc(src) {
        Value::str(repeated)
    } else {
        nfc_value(repeated)
    })
}

/// `s` with its graphemes in reverse order (`.flip`, `nqp::flip`).
///
/// Cost: O(n), n = chars of `s`.
pub(crate) fn flip(s: &str) -> Value {
    if crate::builtins::grapheme_index::is_flat_ascii(s) {
        return Value::str(s.chars().rev().collect::<String>());
    }
    let units: Vec<&str> = crate::builtins::grapheme_index::Units::from(s, 0)
        .map(|(_, u)| u)
        .collect();
    nfc_value(units.into_iter().rev().collect())
}

/// A Unicode normalization form (`.NFC`/`.NFD`/`.NFKC`/`.NFKD`, and
/// `nqp::strtocodes`'s `NORMALIZE_*` modes 1..4).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Normal {
    Nfc,
    Nfd,
    Nfkc,
    Nfkd,
}

impl Normal {
    /// The form a method name (`"NFKC"`) or nqp mode (3) names.
    pub(crate) fn from_name(name: &str) -> Option<Normal> {
        Some(match name {
            "NFC" => Normal::Nfc,
            "NFD" => Normal::Nfd,
            "NFKC" => Normal::Nfkc,
            "NFKD" => Normal::Nfkd,
            _ => return None,
        })
    }

    pub(crate) fn from_nqp_mode(mode: i64) -> Option<Normal> {
        Some(match mode {
            1 => Normal::Nfc,
            2 => Normal::Nfd,
            3 => Normal::Nfkc,
            4 => Normal::Nfkd,
            _ => return None,
        })
    }
}

/// `s` in normalization form `form`. ASCII text is returned borrowed: no
/// ASCII codepoint has a decomposition, a compatibility mapping or a nonzero
/// combining class, and no canonical composition has an all-ASCII source
/// pair, so all four forms map an ASCII string to itself. (ADR-0116 D2.3:
/// the normalizer was 5.2% of a JSON::Fast record, every string of which is
/// ASCII.)
///
/// Cost: O(n), n = bytes of `s` (one ASCII scan; the normalizer only for
/// non-ASCII input).
pub(crate) fn normalize(s: &str, form: Normal) -> std::borrow::Cow<'_, str> {
    if s.is_ascii() {
        return std::borrow::Cow::Borrowed(s);
    }
    std::borrow::Cow::Owned(match form {
        Normal::Nfc => s.nfc().collect(),
        Normal::Nfd => s.nfd().collect(),
        Normal::Nfkc => s.nfkc().collect(),
        Normal::Nfkd => s.nfkd().collect(),
    })
}
