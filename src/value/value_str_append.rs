//! In-place `Str` append: the primitive behind `OpCode::ConcatAssignLocal`.
//!
//! `$s ~= 'x'` used to cost O(len($s)) per append — two full copies
//! (`coerce_to_str` then `format!`), plus an `is_ascii()` scan of the whole
//! accumulated result — which made building an n-character string O(n²)
//! (#8695). Reusing the buffer makes the same accumulation linear, but only
//! when the buffer really is this value's alone, which is what this file's
//! uniqueness check decides.

use super::{Value, ValueRepr};
use std::sync::Arc;

impl Value {
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
    /// **Does not normalize.** `Interpreter::concat_values` runs NFC over the
    /// whole concatenation, and that pass (like the `is_ascii()` scan that
    /// gates it) is O(len) per append — the other half of what makes an
    /// accumulation quadratic. Skipping it is only sound because the caller
    /// has established that this particular join cannot compose; the sole
    /// caller, `Interpreter::exec_concat_assign_local_op`, requires an ASCII
    /// suffix for exactly that reason (an ASCII character is a starter and is
    /// never a combining mark, so it can neither compose with the character
    /// before it nor change the normalization of anything earlier).
    pub(crate) fn str_appended_unnormalized(self, suffix: &str) -> Value {
        let ValueRepr::Str(mut arc) = self.into_repr() else {
            // The caller checks the tag before moving the value out; reaching
            // here would mean it stopped doing so.
            debug_assert!(false, "str_appended_unnormalized on a non-Str value");
            return Value::str(suffix.to_string());
        };
        match Arc::get_mut(&mut arc) {
            // Unique: grow the existing allocation in place. `String::push_str`
            // reallocates geometrically, so a whole accumulation is linear —
            // that amortization IS the fix, not the saved copy alone.
            Some(owned) => {
                owned.push_str(suffix);
                Value::Str(arc)
            }
            // Shared: Raku value semantics require a fresh buffer. Sized
            // exactly, which still beats `format!`'s growth-from-empty.
            None => {
                let mut copied = String::with_capacity(arc.len() + suffix.len());
                copied.push_str(&arc);
                copied.push_str(suffix);
                Value::Str(Arc::new(copied))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn appends_in_place_when_unique() {
        let v = Value::str("ab".to_string());
        let appended = v.str_appended_unnormalized("cd");
        assert_eq!(appended.to_string_value(), "abcd");
    }

    #[test]
    fn copies_when_shared_so_the_alias_is_untouched() {
        let original = Value::str("ab".to_string());
        let alias = original.clone();
        let appended = original.str_appended_unnormalized("cd");
        assert_eq!(appended.to_string_value(), "abcd");
        // The alias still holds the pre-append text: the shared buffer was
        // copied rather than grown.
        assert_eq!(alias.to_string_value(), "ab");
    }

    #[test]
    fn repeated_appends_accumulate() {
        let mut v = Value::str(String::new());
        for _ in 0..1000 {
            v = v.str_appended_unnormalized("x");
        }
        assert_eq!(v.to_string_value().len(), 1000);
    }
}
