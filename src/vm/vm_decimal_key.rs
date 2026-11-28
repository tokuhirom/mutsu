//! The decimal text of an `Int` hash key, formatted on the stack (#19289).

/// An `i64`'s decimal text in a stack buffer: the key a plain hash files an
/// `Int` under, written by the same `Display` that `Value::to_string_value`
/// uses, without the heap `String` (#19289).
pub(super) struct DecimalKey {
    buf: [u8; 20],
    len: usize,
}

impl DecimalKey {
    // Cost: O(d), d <= 20 = digits of an i64.
    pub(super) fn of(n: i64) -> Self {
        use std::fmt::Write as _;
        let mut key = DecimalKey {
            buf: [0; 20],
            len: 0,
        };
        // Cannot fail: an i64 has at most 20 characters ("-9223372036854775808").
        let _ = write!(key, "{n}");
        key
    }

    // Cost: O(1).
    pub(super) fn as_str(&self) -> &str {
        // Only ASCII digits and '-' are ever written.
        std::str::from_utf8(&self.buf[..self.len]).unwrap_or("")
    }
}

impl std::fmt::Write for DecimalKey {
    // Cost: O(s), s = bytes written.
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        let end = self.len + s.len();
        if end > self.buf.len() {
            return Err(std::fmt::Error);
        }
        self.buf[self.len..end].copy_from_slice(s.as_bytes());
        self.len = end;
        Ok(())
    }
}
