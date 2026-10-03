//! `nqp::radix` and `nqp::radix_I`: one digit scanner, two accumulators.
//!
//! Rakudo returns an array containing the result, the number of result
//! digits, and the grapheme offset after the consumed input (`(0, 0, -1)` when
//! no digit was found). The fourth argument is a flag word: bit 0 forces a
//! negative result, bit 1 parses a leading sign, and bit 2 drops trailing
//! zeroes from the result while still consuming them. `radix` accumulates
//! into a native int that wraps on overflow, as the MoarVM operation does;
//! `radix_I` (#11495) into a big integer, so it never wraps.

use crate::value::{RuntimeError, Value};
use num_bigint::BigInt;

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

/// What the scanner found: the digit values that count toward the result
/// (bit 2 already applied), whether to negate, and the end offset.
struct Scan {
    digits: Vec<u32>,
    negative: bool,
    cursor: usize,
}

/// Scan `$str` from grapheme `$pos` for digits of `$radix` under `$flags`.
/// `Ok(None)` when no digit was found.
// Cost: O(k), k = digits consumed from $pos (plus an O(STRIDE) seek to $pos on
// a non-ASCII string; the grapheme index is cached per string).
fn scan(op: &str, args: &[Value]) -> Result<Option<(u32, Scan)>, RuntimeError> {
    let radix = iarg(args, 0);
    let radix = u32::try_from(radix)
        .ok()
        .filter(|r| (2..=36).contains(r))
        .ok_or_else(|| RuntimeError::new(format!("nqp::{op}: radix must be in 2..36")))?;
    let Some(source) = args.get(1) else {
        return Ok(None);
    };
    let pos = iarg(args, 2).max(0) as usize;
    let flags = iarg(args, 3);

    // `$pos` and the returned offset are grapheme positions, like every other
    // nqp string op. The scan walks forward from `$pos` over the cached
    // grapheme index, so it costs the digits consumed, not the whole string.
    let scanned = crate::builtins::grapheme_index::with_str_index(source, |text, idx| {
        if pos >= idx.len() {
            return None;
        }
        let mut chars = crate::builtins::str_prim::chars_from(text, idx, pos).peekable();
        let mut cursor = pos;
        let mut negative = flags & 0x01 != 0;
        if flags & 0x02 != 0
            && let Some(&sign @ ('-' | '+')) = chars.peek()
        {
            negative |= sign == '-';
            chars.next();
            cursor += 1;
        }
        // `digits` holds every digit; `kept` how many of them run up to the
        // last non-zero one, the result under flag 0x04.
        let mut digits = Vec::new();
        let mut kept = 0usize;
        let mut underscore = false;
        for ch in chars {
            if let Some(digit) = crate::builtins::parse_base::char_digit_value(ch, radix) {
                digits.push(digit);
                if digit != 0 {
                    kept = digits.len();
                }
                cursor += 1 + usize::from(underscore);
                underscore = false;
                continue;
            }
            // NQP permits a single underscore between two digits. It is
            // consumed but does not contribute to either the result or its
            // digit count; one not followed by a digit is left unconsumed.
            if ch == '_' && !digits.is_empty() && !underscore {
                underscore = true;
                continue;
            }
            break;
        }
        if digits.is_empty() {
            return None;
        }
        if flags & 0x04 != 0 {
            digits.truncate(kept);
        }
        Some(Scan {
            digits,
            negative,
            cursor,
        })
    });
    Ok(scanned.map(|s| (radix, s)))
}

fn result(value: Value, scan: &Scan) -> Value {
    Value::array(vec![
        value,
        Value::int(scan.digits.len() as i64),
        Value::int(scan.cursor as i64),
    ])
}

fn no_match() -> Value {
    Value::array(vec![Value::int(0), Value::int(0), Value::int(-1)])
}

/// `nqp::radix($radix, $str, $pos, $flags)`: the native-int form.
// Cost: O(k), k = digits consumed (see `scan`).
pub(super) fn nqp_radix(args: &[Value]) -> Result<Value, RuntimeError> {
    let Some((radix, scan)) = scan("radix", args)? else {
        return Ok(no_match());
    };
    let base = i64::from(radix);
    let mut acc = scan.digits.iter().fold(0i64, |acc, &d| {
        // Digit accumulation into a native int, wrapping as MoarVM's does.
        // native-prim: allow
        acc.wrapping_mul(base).wrapping_add(i64::from(d))
    });
    if scan.negative {
        // native-prim: allow
        acc = acc.wrapping_neg();
    }
    Ok(result(Value::int(acc), &scan))
}

/// `nqp::radix_I($radix, $str, $pos, $flags, $type)`: the big-integer form.
/// The result is an `Int` (mutsu's Int is the boxed big-integer type the
/// `$type` argument names in Rakudo).
// Cost: O(k^2) bit operations, k = digits consumed (each step multiplies the
// growing accumulator by the radix).
pub(super) fn nqp_radix_big(args: &[Value]) -> Result<Value, RuntimeError> {
    let Some((radix, scan)) = scan("radix_I", args)? else {
        return Ok(no_match());
    };
    let mut acc = scan
        .digits
        .iter()
        .fold(BigInt::from(0), |acc, &d| acc * radix + d);
    if scan.negative {
        acc = -acc;
    }
    Ok(result(Value::from_bigint(acc), &scan))
}
