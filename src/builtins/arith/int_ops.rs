//! The one implementation of Raku's integer operators (ADR-0118): `div`,
//! `+&` / `+|` / `+^`, `+<` / `+>`, and the i64 edge cases of `prefix:<->`
//! and `.abs`.
//!
//! Each used to be written separately by the VM opcode, the reduction /
//! metaop fold and the methods, and they had drifted: `[div] 7, -2` used
//! Euclidean division while `7 div -2` floored, `5.5 +& 3` answered 0 while
//! `[+&] 5.5, 3` answered 1, `-$min` (i64::MIN) became a Num, and
//! `$min div -1` panicked the interpreter. Every layer now calls these.
//!
//! Raku integers are arbitrary precision, so an i64 result that overflows is
//! promoted to a BigInt here -- never wrapped (that is the native `int` /
//! `nqp::*_i` contract, which lives in `runtime::nqp_pure`).

use num_bigint::BigInt;
use num_traits::{FromPrimitive, Zero};

use crate::value::{RuntimeError, Value, ValueView};

/// An operand of an integer operator, coerced the way Rakudo's `Int()`
/// coercion does: Int/BigInt as they are, a Num or Rational truncated toward
/// zero (at full precision, not through i64), a Str numified first, anything
/// else through `runtime::to_int`.
pub(crate) fn int_operand(v: &Value) -> Value {
    match v.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => v.clone(),
        ValueView::Num(f) if f.is_finite() => {
            if f.abs() < 9.0e18 {
                Value::int(f as i64)
            } else {
                BigInt::from_f64(f.trunc()).map_or_else(|| Value::int(0), Value::from_bigint)
            }
        }
        ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => {
            Value::from_bigint(v.to_bigint())
        }
        ValueView::Mixin(inner, _) => int_operand(inner),
        // A Str (and anything else Cool) numifies first, the way `+$x` does,
        // so `"0x10" +& 3` sees 16.
        ValueView::Str(_) | ValueView::Bool(_) => {
            let n = crate::runtime::coerce_to_numeric(v.clone());
            if matches!(n.view(), ValueView::Str(_) | ValueView::Bool(_)) {
                Value::int(crate::runtime::to_int(&n))
            } else {
                int_operand(&n)
            }
        }
        _ => Value::int(crate::runtime::to_int(v)),
    }
}

fn is_zero(v: &Value) -> bool {
    match v.view() {
        ValueView::Int(i) => i == 0,
        ValueView::BigInt(b) => b.is_zero(),
        _ => false,
    }
}

/// `$a div $b`: floored integer division of the two operands' `Int`
/// coercions. Division by zero is a soft `Failure`, as in Rakudo; the one
/// i64 case that overflows (`i64::MIN div -1`) is promoted to a BigInt.
///
/// Cost: O(1) for i64 operands; O(n^2) in the operand digits for BigInts.
pub(crate) fn int_div(left: &Value, right: &Value) -> Value {
    let (l, r) = (int_operand(left), int_operand(right));
    if is_zero(&r) {
        return RuntimeError::divide_by_zero_failure(Some(l), Some("div"));
    }
    if let (ValueView::Int(a), ValueView::Int(b)) = (l.view(), r.view()) {
        if let Some(q) = a.checked_div(b) {
            // `checked_div` truncates; step down when the signs differ and
            // the division was inexact, which is what flooring means.
            let q = if (a % b != 0) && ((a < 0) != (b < 0)) {
                q - 1
            } else {
                q
            };
            return Value::int(q);
        }
        // i64::MIN div -1: the only overflowing case.
        return Value::from_bigint(-BigInt::from(a));
    }
    Value::from_bigint(num_integer::Integer::div_floor(
        &l.to_bigint(),
        &r.to_bigint(),
    ))
}

/// Floored integer modulus of two i64s (`$a % $b` / `$a mod $b`), with the
/// divisor-sign result Raku gives. `b` must be non-zero. `i64::MIN % -1` is 0
/// rather than the overflow `i64::rem` panics on.
pub(crate) fn int_mod_i64(a: i64, b: i64) -> i64 {
    if b == -1 {
        return 0;
    }
    num_integer::Integer::mod_floor(&a, &b)
}

/// The three bitwise operators.
#[derive(Debug, Clone, Copy)]
pub(crate) enum BitOp {
    And,
    Or,
    Xor,
}

/// `$a +& $b`, `$a +| $b`, `$a +^ $b` on the operands' `Int` coercions.
///
/// Cost: O(1) for i64 operands; O(n) in the operand digits for BigInts.
pub(crate) fn int_bitop(left: &Value, right: &Value, op: BitOp) -> Value {
    let (l, r) = (int_operand(left), int_operand(right));
    if let (ValueView::Int(a), ValueView::Int(b)) = (l.view(), r.view()) {
        return Value::int(match op {
            BitOp::And => a & b,
            BitOp::Or => a | b,
            BitOp::Xor => a ^ b,
        });
    }
    let (a, b) = (l.to_bigint(), r.to_bigint());
    Value::from_bigint(match op {
        BitOp::And => a & b,
        BitOp::Or => a | b,
        BitOp::Xor => a ^ b,
    })
}

/// A shift count as an i64, saturating a BigInt count (a shift that large
/// either exhausts memory or yields 0 / -1 either way).
fn shift_count(v: &Value) -> i64 {
    match int_operand(v).view() {
        ValueView::Int(i) => i,
        ValueView::BigInt(b) => {
            if b.sign() == num_bigint::Sign::Minus {
                i64::MIN
            } else {
                i64::MAX
            }
        }
        _ => 0,
    }
}

/// `$a +< $b` (a negative count shifts right).
///
/// Cost: O(n + b) in the operand's digits and the shift count.
pub(crate) fn int_shift_left(left: &Value, right: &Value) -> Value {
    shift(int_operand(left), shift_count(right))
}

/// `$a +> $b` (a negative count shifts left). Right shifts are arithmetic:
/// a negative operand rounds toward negative infinity.
///
/// Cost: O(n + b) in the operand's digits and the shift count.
pub(crate) fn int_shift_right(left: &Value, right: &Value) -> Value {
    shift(int_operand(left), shift_count(right).saturating_neg())
}

/// Shift `v` left by `count` bits (right when negative).
fn shift(v: Value, count: i64) -> Value {
    if count >= 0 {
        if let ValueView::Int(a) = v.view()
            && count < 63
            && let Some(r) = a.checked_mul(1i64 << count)
        {
            return Value::int(r);
        }
        return Value::from_bigint(v.to_bigint() << (count as usize));
    }
    let by = count.unsigned_abs();
    if let ValueView::Int(a) = v.view() {
        return Value::int(if by >= 64 {
            if a < 0 { -1 } else { 0 }
        } else {
            a >> by
        });
    }
    Value::from_bigint(v.to_bigint() >> (by as usize))
}

/// `-$i` for an i64: `-i64::MIN` is a BigInt, not a Num.
pub(crate) fn int_negate(i: i64) -> Value {
    i.checked_neg()
        .map_or_else(|| Value::from_bigint(-BigInt::from(i)), Value::int)
}

/// `$i.abs` for an i64: `i64::MIN.abs` is a BigInt, not a wrapped negative.
pub(crate) fn int_abs(i: i64) -> Value {
    i.checked_abs()
        .map_or_else(|| Value::from_bigint(-BigInt::from(i)), Value::int)
}

/// `+^ $a` (prefix bitwise negation) on the operand's `Int` coercion.
///
/// Cost: O(1) for an i64 operand; O(n) in the operand digits for a BigInt.
pub(crate) fn int_bitneg(v: &Value) -> Value {
    let v = int_operand(v);
    match v.view() {
        ValueView::Int(i) => Value::int(!i),
        _ => Value::from_bigint(!v.to_bigint()),
    }
}

/// `$a.abs` on the operand's `Int` coercion, for any magnitude.
///
/// Cost: O(1) for an i64 operand; O(n) in the operand digits for a BigInt.
pub(crate) fn int_abs_value(v: &Value) -> Value {
    let v = int_operand(v);
    match v.view() {
        ValueView::Int(i) => int_abs(i),
        _ => Value::from_bigint(num_traits::Signed::abs(&v.to_bigint())),
    }
}

/// `$a gcd $b`: the non-negative greatest common divisor of the operands'
/// `Int` coercions (`0 gcd 0` is 0).
///
/// Cost: O(n^2) in the operand digits (Euclid on BigInts).
pub(crate) fn int_gcd(left: &Value, right: &Value) -> Value {
    let a = int_operand(left).to_bigint();
    let b = int_operand(right).to_bigint();
    Value::from_bigint(num_integer::Integer::gcd(&a, &b))
}

/// `$a lcm $b`: the non-negative least common multiple of the operands'
/// `Int` coercions; 0 when either operand is 0.
///
/// Cost: O(n^2) in the operand digits.
pub(crate) fn int_lcm(left: &Value, right: &Value) -> Value {
    let a = num_traits::Signed::abs(&int_operand(left).to_bigint());
    let b = num_traits::Signed::abs(&int_operand(right).to_bigint());
    if a.is_zero() || b.is_zero() {
        return Value::int(0);
    }
    let g = num_integer::Integer::gcd(&a, &b);
    Value::from_bigint(&a / &g * &b)
}

/// Three-way comparison of the operands' `Int` coercions, at full precision.
///
/// Cost: O(1) for i64 operands; O(n) in the operand digits for BigInts.
pub(crate) fn int_cmp(left: &Value, right: &Value) -> std::cmp::Ordering {
    let (l, r) = (int_operand(left), int_operand(right));
    if let (ValueView::Int(a), ValueView::Int(b)) = (l.view(), r.view()) {
        return a.cmp(&b);
    }
    l.to_bigint().cmp(&r.to_bigint())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(v: Value) -> String {
        v.to_string_value()
    }

    #[test]
    fn div_floors_and_promotes() {
        assert_eq!(s(int_div(&Value::int(7), &Value::int(-2))), "-4");
        assert_eq!(s(int_div(&Value::int(-7), &Value::int(2))), "-4");
        assert_eq!(s(int_div(&Value::int(7), &Value::int(2))), "3");
        assert_eq!(
            s(int_div(&Value::int(i64::MIN), &Value::int(-1))),
            "9223372036854775808"
        );
        assert_eq!(s(int_div(&Value::num(7.5), &Value::int(2))), "3");
    }

    #[test]
    fn mod_min_by_minus_one_is_zero() {
        assert_eq!(int_mod_i64(i64::MIN, -1), 0);
        assert_eq!(int_mod_i64(-7, 3), 2);
        assert_eq!(int_mod_i64(7, -3), -2);
    }

    #[test]
    fn bitops_truncate_non_integers() {
        assert_eq!(
            s(int_bitop(&Value::num(5.5), &Value::int(3), BitOp::And)),
            "1"
        );
        assert_eq!(
            s(int_bitop(&Value::num(5.5), &Value::int(2), BitOp::Or)),
            "7"
        );
    }

    #[test]
    fn shifts_promote_and_round_down() {
        assert_eq!(
            s(int_shift_left(&Value::int(1), &Value::int(64))),
            "18446744073709551616"
        );
        assert_eq!(s(int_shift_right(&Value::int(-8), &Value::int(1))), "-4");
        assert_eq!(s(int_shift_right(&Value::int(-1), &Value::int(100))), "-1");
        assert_eq!(s(int_shift_left(&Value::int(8), &Value::int(-2))), "2");
    }

    #[test]
    fn min_negate_and_abs_are_bigints() {
        assert_eq!(s(int_negate(i64::MIN)), "9223372036854775808");
        assert_eq!(s(int_abs(i64::MIN)), "9223372036854775808");
    }
}
