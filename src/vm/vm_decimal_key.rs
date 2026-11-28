//! The decimal text of an `Int` hash key, formatted on the stack (#19289).

/// An `i64`'s decimal text in a stack buffer: the key a plain hash files an
/// `Int` under, the same text `Value::to_string_value` gives, without the heap
/// `String` (#19289). The digits come from the one `Int` to decimal routine,
/// [`crate::builtins::str_prim::IntDecimal`].
pub(super) struct DecimalKey(crate::builtins::str_prim::IntDecimal);

impl DecimalKey {
    // Cost: O(d), d <= 20 = digits of an i64.
    #[inline]
    pub(super) fn of(n: i64) -> Self {
        DecimalKey(crate::builtins::str_prim::IntDecimal::of(n))
    }

    // Cost: O(1).
    #[inline]
    pub(super) fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
