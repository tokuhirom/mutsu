//! The string-building primitives: `~` / `nqp::concat`, `x` / `nqp::x` and
//! `.flip` / `nqp::flip`. Each result is renormalized to NFC, because a
//! combining mark at one operand's edge may compose with its neighbour --
//! MoarVM strings are NFG, so `nqp::concat("e", "\x[301]")` is one grapheme.

use unicode_normalization::UnicodeNormalization;

use crate::value::{RuntimeError, Value};

/// A string built by joining pieces, in NFC.
fn nfc_value(s: String) -> Value {
    if s.is_ascii() {
        Value::str(s)
    } else {
        Value::str(s.nfc().collect::<String>())
    }
}

/// `left ~ right` for two strings (`infix:<~>`, `nqp::concat`).
///
/// Cost: O(n1 + n2), n1, n2 = chars of the operands (a non-ASCII result is
/// renormalized in full). Rakudo: amortized O(1) (strands) -- see #9141.
pub(crate) fn concat(left: &str, right: &str) -> Value {
    let mut s = String::with_capacity(left.len() + right.len());
    s.push_str(left);
    s.push_str(right);
    nfc_value(s)
}

/// `src x n` (`infix:<x>`, its reduction and `nqp::x`). The caller has
/// already clamped or rejected a negative count.
///
/// Cost: O(n * c), n = chars of `src`, c = repeat count (plus an NFC pass
/// over a non-ASCII result). Rakudo: O(1) for a flat operand (one repeat
/// strand) -- see #9147.
pub(crate) fn repeat(src: &str, n: usize) -> Result<Value, RuntimeError> {
    // Guard the allocation: `str::repeat` aborts the process via
    // `handle_alloc_error` on an absurd count (e.g. `"x" x 1e15`), which
    // `try {}` cannot recover from. Reserve fallibly first so the same
    // input yields a catchable `X::` instead.
    //
    // Best-effort only, and weaker than raku here: `try_reserve` fails only
    // if the kernel refuses the mapping (request over the ~128 TiB address
    // space, or a non-overcommitting `vm.overcommit_memory`). Under
    // `vm.overcommit_memory=1` a 91 TiB reservation succeeds and the fill
    // loop below then eats the machine. raku instead caps the *request*
    // deterministically -- "Repeat count (N) cannot be greater than max
    // allowed number of graphemes 4294967295", plus the same bound on
    // `graphemes * count` -- which is allocator-independent. mutsu should
    // adopt that cap; see the note in `Interpreter::autoviv_resize`.
    // TODO: enforce raku's 4294967295-grapheme cap before reserving.
    let total = src
        .len()
        .checked_mul(n)
        .ok_or_else(|| RuntimeError::new("Cannot repeat string: length overflow"))?;
    // Build by doubling (`extend_from_within` = one memcpy per doubling)
    // instead of `n` per-copy `push_str` calls: the roast A01-limits test
    // declares `"a" x 2**32-1` (a 4 GiB string), which must complete in
    // seconds, not minutes.
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
    // a concatenation of valid UTF-8 strings is valid UTF-8. Skipping the
    // validation scan matters at this size (multi-GiB).
    let repeated = unsafe { String::from_utf8_unchecked(buf) };
    Ok(nfc_value(repeated))
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

/// `s` in normalization form `form`.
///
/// Cost: O(n), n = codepoints of `s`.
pub(crate) fn normalize(s: &str, form: Normal) -> String {
    match form {
        Normal::Nfc => s.nfc().collect(),
        Normal::Nfd => s.nfd().collect(),
        Normal::Nfkc => s.nfkc().collect(),
        Normal::Nfkd => s.nfkd().collect(),
    }
}
