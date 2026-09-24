//! Building a `Str` result as a strand list instead of copying (ADR-0120).
//!
//! `~`, `x` and interpolation used to write out every character of their
//! result. When the result is mostly a string something else still holds --
//! the left operand of `$a ~ $b` with `$a` still live, the source of `x`, an
//! interpolated variable -- MoarVM references it as a strand instead, and so
//! does mutsu now: the result is a [`StrBody::Lazy`] that flattens once, on
//! its first read.
//!
//! A strand join is only ever taken where it cannot change the result: the
//! part on the right of every join starts at an NFC normalization boundary
//! and every part is NFC, so the concatenation is NFC as it stands. Anything
//! else returns `None` and the caller takes its eager path, which
//! renormalizes the join.

use std::sync::Arc;

use unicode_normalization::{IsNormalized, is_nfc_quick};

use crate::value::{STRAND_MIN_BYTES, StrBody, Value, has_nfc_boundary_before};

/// A `Str` part shorter than this is copied into its neighbour rather than
/// referenced as a strand of its own, so interpolating a short variable does
/// not use up the strand budget.
const SHARE_MIN_BYTES: usize = 256;

/// Whether `body` can follow anything under a strand join: it starts at a
/// normalization boundary (or is empty). Every `Str` is already NFC.
fn joins_cleanly(body: &StrBody) -> bool {
    body.first_char().is_none_or(has_nfc_boundary_before)
}

/// `left ~ right` as a strand list, when that is both valid and cheaper than
/// the in-place / copying paths. Gives `left` back when it is not.
///
/// Not taken for a small result (a copy is cheaper than the later flatten),
/// nor when `left` is a flat `Str` held by nothing else: growing that buffer
/// in place is amortized O(n2), and a strand list would have to be flattened
/// again before the next in-place append.
///
/// Cost: O(1) (at most `MAX_STRANDS` strand references copied); O(n) to
/// stringify a non-`Str` operand, n = its chars.
pub(super) fn concat(left: Value, right: &Value) -> Result<Value, Value> {
    let left_str_len = left_str_len(&left);
    let right_str_len = right.is_str_value().then(|| str_len(right));
    // One side has to be a big `Str` for there to be anything to share.
    if left_str_len.unwrap_or(0).max(right_str_len.unwrap_or(0)) < STRAND_MIN_BYTES {
        return Err(left);
    }
    let right_arc = match right.view() {
        crate::value::ValueView::Str(arc) => Arc::clone(&arc),
        _ => match stringified_nfc(right) {
            Some(s) => Arc::new(StrBody::from(s)),
            None => return Err(left),
        },
    };
    if !joins_cleanly(&right_arc) {
        return Err(left);
    }
    let mut left_arc = match left.into_str_arc() {
        Ok(arc) => arc,
        Err(left) => match stringified_nfc(&left) {
            Some(s) => Arc::new(StrBody::from(s)),
            None => return Err(left),
        },
    };
    if right_arc.byte_len() == 0 {
        return Ok(Value::str_arc(left_arc));
    }
    if left_arc.byte_len() == 0 {
        return Ok(Value::str_arc(right_arc));
    }
    let unique_flat = Arc::get_mut(&mut left_arc).is_some_and(|b| !b.is_lazy());
    if unique_flat || left_arc.byte_len() + right_arc.byte_len() < STRAND_MIN_BYTES {
        return Err(Value::str_arc(left_arc));
    }
    match StrBody::concat_strands(&[&left_arc, &right_arc]) {
        Some(body) => Ok(Value::str_arc(Arc::new(body))),
        None => Err(Value::str_arc(left_arc)),
    }
}

/// `src x n` as one repeat strand, when the result is big enough to be worth
/// it and no copy can compose with the one before it. `None` means "build it
/// flat".
///
/// Cost: O(n), n = bytes of `src`, for the NFC checks (O(1) for an ASCII
/// `src` whose payload is already a flat `Str`).
pub(super) fn repeat(src: &Arc<StrBody>, reps: usize) -> Option<Value> {
    let total = src.byte_len().checked_mul(reps)?;
    if total < STRAND_MIN_BYTES || !repeats_as_nfc(src) {
        return None;
    }
    Some(Value::str_arc(Arc::new(StrBody::repeated(src, reps))))
}

/// Whether copies of `src` placed side by side are NFC as they stand: `src`
/// is NFC (every `Str` is) and starts at a normalization boundary, so no copy
/// composes or reorders with the one before it.
pub(super) fn repeats_as_nfc(src: &str) -> bool {
    src.is_ascii()
        || (src.chars().next().is_none_or(has_nfc_boundary_before)
            && is_nfc_quick(src.chars()) == IsNormalized::Yes)
}

/// The concatenation of interpolated parts (`"$a-$b"`), built as strands
/// when it is big, referencing each big `Str` part instead of copying it.
///
/// Small parts and non-`Str` parts accumulate into flat pieces between the
/// shared ones. `finish` falls back to one flat, renormalized buffer whenever
/// a strand result is not valid or not worth it.
#[derive(Default)]
pub(crate) struct Joiner {
    /// Each part, with whether it is known to be NFC already (a `Str`
    /// payload is; a pending piece of stringified text has to be checked).
    parts: Vec<(Arc<StrBody>, bool)>,
    pending: String,
    len: usize,
}

impl Joiner {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Append text that has no shareable payload.
    ///
    /// Cost: O(n), n = bytes of `s`.
    pub(crate) fn push_str(&mut self, s: &str) {
        self.len += s.len();
        self.pending.push_str(s);
    }

    /// Append a value's string form: a big `Str` is referenced, anything
    /// else is stringified into the pending piece.
    ///
    /// Cost: O(1) for a `Str` of at least `SHARE_MIN_BYTES` bytes; otherwise
    /// O(n), n = chars of its string form.
    pub(crate) fn push_value(&mut self, v: &Value) {
        if let crate::value::ValueView::Str(arc) = v.view()
            && arc.byte_len() >= SHARE_MIN_BYTES
        {
            self.flush_pending();
            self.len += arc.byte_len();
            self.parts.push((Arc::clone(&arc), true));
            return;
        }
        match v.view() {
            crate::value::ValueView::Str(arc) => self.push_str(arc.as_str()),
            _ => self.push_str(&crate::runtime::utils::coerce_to_str(v)),
        }
    }

    fn flush_pending(&mut self) {
        if !self.pending.is_empty() {
            let piece = std::mem::take(&mut self.pending);
            self.parts.push((Arc::new(StrBody::from(piece)), false));
        }
    }

    /// The joined string, in NFC.
    ///
    /// Cost: O(k) for a strand result, k = parts; otherwise O(n), n = total
    /// bytes (copied, plus an NFC pass when it is not ASCII).
    pub(crate) fn finish(mut self) -> Value {
        if self.parts.is_empty() {
            return super::build::nfc_value(self.pending);
        }
        self.flush_pending();
        if self.len >= STRAND_MIN_BYTES && self.parts_join_cleanly() {
            let refs: Vec<&Arc<StrBody>> = self.parts.iter().map(|(p, _)| p).collect();
            if let Some(body) = StrBody::concat_strands(&refs) {
                return Value::str_arc(Arc::new(body));
            }
        }
        let mut flat = String::with_capacity(self.len);
        for (part, _) in &self.parts {
            flat.push_str(part);
        }
        super::build::nfc_value(flat)
    }

    /// Every part is NFC and every join is at a normalization boundary. The
    /// shared parts are `Str` payloads (NFC already); the pending pieces are
    /// checked here.
    fn parts_join_cleanly(&self) -> bool {
        self.parts.iter().enumerate().all(|(i, (part, known_nfc))| {
            (i == 0 || joins_cleanly(part))
                && (*known_nfc
                    || part.is_ascii()
                    || is_nfc_quick(part.chars()) == IsNormalized::Yes)
        })
    }
}

/// Bytes of a `Str` value's payload (without flattening a strand list).
fn str_len(v: &Value) -> usize {
    match v.view() {
        crate::value::ValueView::Str(arc) => arc.byte_len(),
        _ => 0,
    }
}

fn left_str_len(v: &Value) -> Option<usize> {
    v.is_str_value().then(|| str_len(v))
}

/// A non-`Str` operand's string form, when it is NFC (a stringified number
/// or type name always is); `None` sends the caller down the eager path,
/// which normalizes.
fn stringified_nfc(v: &Value) -> Option<String> {
    let s = crate::runtime::utils::coerce_to_str(v);
    (s.is_ascii() || is_nfc_quick(s.chars()) == IsNormalized::Yes).then_some(s)
}
