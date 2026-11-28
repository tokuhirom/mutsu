//! The decimal text of an `i64`, written into a stack buffer.
//!
//! An `Int` becomes text whenever it is interpolated (`"key-$i"`), used as a
//! hash key (`%h{$i}`) or joined. Those all went through `core::fmt`'s
//! `Display` (a `Formatter`, `pad_integral`, a `write_str` per piece): about
//! 350 instructions for a five-digit number, more than the rest of a
//! `"key-$i"` interpolation. This writes the digits two at a time from a
//! table, the way `itoa` and MoarVM's `MVM_coerce_i_s` do, with the same
//! output as `Display` for every `i64`.

/// The decimal text of an `i64`: at most 20 bytes ("-9223372036854775808").
pub(crate) struct IntDecimal {
    buf: [u8; 20],
    start: usize,
}

const DIGIT_PAIRS: &[u8; 200] = b"0001020304050607080910111213141516171819\
2021222324252627282930313233343536373839\
4041424344454647484950515253545556575859\
6061626364656667686970717273747576777879\
8081828384858687888990919293949596979899";

impl IntDecimal {
    /// `n` in decimal, exactly as `format!("{n}")` writes it.
    ///
    /// Cost: O(d), d <= 20 = digits of `n`.
    #[inline]
    pub(crate) fn of(n: i64) -> Self {
        let mut buf = [0u8; 20];
        let mut pos = buf.len();
        // `unsigned_abs` covers `i64::MIN` without overflow.
        let mut v = n.unsigned_abs();
        while v >= 100 {
            let pair = (v % 100) as usize * 2;
            v /= 100;
            pos -= 2;
            buf[pos] = DIGIT_PAIRS[pair];
            buf[pos + 1] = DIGIT_PAIRS[pair + 1];
        }
        if v >= 10 {
            let pair = v as usize * 2;
            pos -= 2;
            buf[pos] = DIGIT_PAIRS[pair];
            buf[pos + 1] = DIGIT_PAIRS[pair + 1];
        } else {
            pos -= 1;
            buf[pos] = b'0' + v as u8;
        }
        if n < 0 {
            pos -= 1;
            buf[pos] = b'-';
        }
        IntDecimal { buf, start: pos }
    }

    /// The text.
    ///
    /// Cost: O(1).
    #[inline]
    pub(crate) fn as_str(&self) -> &str {
        // Only ASCII digits and '-' are ever written.
        std::str::from_utf8(&self.buf[self.start..]).unwrap_or("")
    }
}

#[cfg(test)]
mod tests {
    use super::IntDecimal;

    #[test]
    fn matches_display_for_edge_values() {
        let mut cases = vec![
            0i64,
            1,
            -1,
            9,
            10,
            -10,
            99,
            100,
            101,
            999,
            1000,
            12345,
            -12345,
            i64::MAX,
            i64::MIN,
            i64::MIN + 1,
            i64::MAX - 1,
        ];
        let mut p = 1i64;
        while let Some(next) = p.checked_mul(10) {
            cases.extend([p - 1, p, p + 1, -p, -(p - 1), -(p + 1)]);
            p = next;
        }
        cases.extend([p - 1, p, -p]);
        for n in cases {
            assert_eq!(IntDecimal::of(n).as_str(), n.to_string(), "{n}");
        }
    }
}
