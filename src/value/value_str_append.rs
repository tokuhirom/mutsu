//! In-place `Str` append: the primitive behind `OpCode::ConcatAssignLocal`.
//!
//! `$s ~= 'x'` used to cost O(len($s)) per append — two full copies
//! (`coerce_to_str` then `format!`), plus an `is_ascii()` scan of the whole
//! accumulated result — which made building an n-character string O(n²)
//! (#8695). Reusing the buffer makes the same accumulation linear, but only
//! when the buffer really is this value's alone, which is what this file's
//! uniqueness check decides.

use super::value_str_append_nfc::{StrAppendPlan, append_nfc};
use super::{Value, ValueRepr};
use std::sync::Arc;

impl Value {
    /// The `Str` payload, moved out (no refcount traffic), or `self` back when
    /// this is not a `Str`.
    ///
    /// Cost: O(1).
    pub(crate) fn into_str_arc(self) -> Result<Arc<super::StrBody>, Value> {
        if self.is_str_value() {
            match self.into_repr() {
                ValueRepr::Str(arc) => Ok(arc),
                other => Err(Value::from_repr(other)),
            }
        } else {
            Err(self)
        }
    }

    /// `self ~ suffix` for a `Str`, growing `self`'s existing buffer when this
    /// `Value` is its only holder.
    ///
    /// Takes `self` **by value** on purpose: the caller must have *moved* the
    /// value out of wherever it was stored (its local slot), because a value
    /// still sitting in a slot is held twice and would copy every time. A
    /// genuinely shared buffer — `my $b = $a` aliasing it, or an env mirror of
    /// the slot — is detected by [`Arc::get_mut`] and copied instead, which is
    /// what keeps Raku's value semantics: appending to `$a` must never be
    /// visible through `$b`.
    ///
    /// **Normalizes in bounded time, not by rescanning the buffer.** The
    /// result has to stay NFC, but `Interpreter::concat_values` restores that
    /// by running NFC over the whole concatenation, which is O(len) per append
    /// and puts the quadratic cost straight back (#8725). `plan` — built by
    /// [`StrAppendPlan::for_suffix`] from the suffix alone — says whether the
    /// join can compose at all; when it cannot (every ASCII suffix, and any
    /// suffix starting at a normalization boundary) nothing but the suffix is
    /// looked at, and when it can, only a bounded window around the join is
    /// redone. `self` is assumed already NFC, which every path that produces a
    /// `Str` guarantees.
    pub(crate) fn str_appended_nfc(self, plan: &StrAppendPlan<'_>) -> Value {
        let ValueRepr::Str(mut arc) = self.into_repr() else {
            // The caller checks the tag before moving the value out; reaching
            // here would mean it stopped doing so.
            debug_assert!(false, "str_appended_nfc on a non-Str value");
            let mut seeded = String::new();
            append_nfc(&mut seeded, plan);
            return Value::str(seeded);
        };
        match Arc::get_mut(&mut arc) {
            // Unique: grow the existing allocation in place. `String::push_str`
            // reallocates geometrically, so a whole accumulation is linear —
            // that amortization IS the fix, not the saved copy alone.
            // A unique strand list is flattened first (ADR-0120 §2.6): the
            // one O(n) copy is paid once, and the appends after it are
            // in place again.
            Some(owned) => {
                append_nfc(owned.make_flat_mut(), plan);
                Value::Str(arc)
            }
            // Shared: Raku value semantics require a fresh buffer. Sized with
            // the suffix in mind, which still beats `format!`'s growth-from-
            // empty.
            None => {
                let mut copied = String::with_capacity(arc.len() + plan.suffix_len_hint());
                copied.push_str(&arc);
                append_nfc(&mut copied, plan);
                Value::str(copied)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn appended(v: Value, suffix: &str) -> Value {
        v.str_appended_nfc(&StrAppendPlan::for_suffix(suffix))
    }

    #[test]
    fn appends_in_place_when_unique() {
        let v = Value::str("ab".to_string());
        assert_eq!(appended(v, "cd").to_string_value(), "abcd");
    }

    #[test]
    fn copies_when_shared_so_the_alias_is_untouched() {
        let original = Value::str("ab".to_string());
        let alias = original.clone();
        assert_eq!(appended(original, "cd").to_string_value(), "abcd");
        // The alias still holds the pre-append text: the shared buffer was
        // copied rather than grown.
        assert_eq!(alias.to_string_value(), "ab");
    }

    #[test]
    fn repeated_appends_accumulate() {
        let mut v = Value::str(String::new());
        for _ in 0..1000 {
            v = appended(v, "x");
        }
        assert_eq!(v.to_string_value().len(), 1000);
    }

    #[test]
    fn a_non_ascii_suffix_appends_and_stays_normalized() {
        let v = Value::str("ascii".to_string());
        assert_eq!(appended(v, "\u{2603}").to_string_value(), "ascii\u{2603}");
    }

    #[test]
    fn a_composing_suffix_is_normalized_across_the_join() {
        let v = Value::str("e".to_string());
        assert_eq!(appended(v, "\u{301}").to_string_value(), "\u{e9}");
    }

    #[test]
    fn a_shared_buffer_normalizes_the_join_in_the_copy() {
        let original = Value::str("e".to_string());
        let alias = original.clone();
        assert_eq!(appended(original, "\u{301}").to_string_value(), "\u{e9}");
        assert_eq!(alias.to_string_value(), "e");
    }
}
