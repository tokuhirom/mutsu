//! The decimal text of an `i64`, formatted on the stack: the one
//! implementation behind an `Int`'s `Str` (`Value::to_string_value`), an `Int`
//! hash key (#19289) and an `Int` piece of an interpolation. It writes the
//! same text as `i64`'s `Display` without `core::fmt` or a heap `String`. It
//! lives in the value layer so `Value` can use it; `builtins::str_prim`
//! re-exports it as the `Str` primitive (ADR-0117).

/// An `i64`'s decimal text in a stack buffer.
pub(crate) struct DecimalKey {
    buf: [u8; 20],
    start: usize,
}

impl DecimalKey {
    // Cost: O(d), d <= 20 = characters of an i64.
    #[inline]
    pub(crate) fn of(n: i64) -> Self {
        // Two digits per step, from a table of "00".."99".
        const PAIRS: &[u8; 200] = b"\
            0001020304050607080910111213141516171819\
            2021222324252627282930313233343536373839\
            4041424344454647484950515253545556575859\
            6061626364656667686970717273747576777879\
            8081828384858687888990919293949596979899";
        let mut buf = [0u8; 20];
        let mut start = buf.len();
        // `unsigned_abs` covers `i64::MIN`, whose magnitude has no `i64`.
        let mut u = n.unsigned_abs();
        while u >= 100 {
            let pair = (u % 100) as usize * 2;
            u /= 100;
            start -= 2;
            buf[start..start + 2].copy_from_slice(&PAIRS[pair..pair + 2]);
        }
        if u >= 10 {
            let pair = u as usize * 2;
            start -= 2;
            buf[start..start + 2].copy_from_slice(&PAIRS[pair..pair + 2]);
        } else {
            start -= 1;
            buf[start] = b'0' + u as u8;
        }
        if n < 0 {
            start -= 1;
            buf[start] = b'-';
        }
        DecimalKey { buf, start }
    }

    // Cost: O(1).
    #[inline]
    pub(crate) fn as_str(&self) -> &str {
        // SAFETY: `of` writes only ASCII digits and '-' into
        // `buf[start..]`, so the bytes are valid UTF-8.
        unsafe { std::str::from_utf8_unchecked(&self.buf[self.start..]) }
    }
}

#[cfg(test)]
mod tests {
    use super::DecimalKey;

    #[test]
    fn matches_display() {
        for n in [
            0,
            1,
            -1,
            9,
            10,
            -10,
            42,
            1_000_000,
            -987_654_321,
            i64::MAX,
            i64::MIN,
            i64::MIN + 1,
            99,
            100,
            101,
            -99,
            -100,
            1234567,
        ] {
            assert_eq!(DecimalKey::of(n).as_str(), n.to_string(), "{n}");
        }
    }
}
