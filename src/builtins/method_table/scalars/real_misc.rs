//! The integer and real numeric methods that are not math functions:
//! `is-prime`, `narrow`, `conj`, `Bridge`, `lsb`, `msb`, `chr`, the native
//! integer coercions (`int8` .. `uint64`, `byte`, `int`, `uint`) and `rand`.
//!
//! Rakudo declares some of them on `Cool` (`is-prime`, `conj`, `chr`, the
//! native coercions, `rand`), where the body is `self.Numeric.METHOD`: those
//! rows read their receiver through [`numify`], the way the math rows do. The
//! rest belong to the numeric owners alone (`narrow`, `Bridge`, `lsb`,
//! `msb`), so a `Str` has no such method, as in Rakudo. `narrow`, `lsb` and
//! `msb` take a defined invocant, and every `chr` candidate does: a type
//! object is refused (`RowFlags::DEFINITE`, `RowFlags::DEFINITE_MULTI`).

use super::numify::numify;
use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

/// A zero-argument row.
macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
    ($owner:literal, $name:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: $flags,
            named: &[],
        }
    };
}

/// A one-argument row.
macro_rules! row1 {
    ($owner:literal, $name:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 1,
            handler: Handler::Pure($handler),
            flags: $flags,
            named: &[],
        }
    };
}

/// The native integer coercions `Cool` and `Int` both declare.
macro_rules! native_int_rows {
    ($owner:literal) => {
        &[
            row!($owner, "int", int, RowFlags::TYPE_OBJECT_OK),
            row!($owner, "int8", int8, RowFlags::TYPE_OBJECT_OK),
            row!($owner, "int16", int16, RowFlags::TYPE_OBJECT_OK),
            row!($owner, "int32", int32, RowFlags::TYPE_OBJECT_OK),
            row!($owner, "int64", int64, RowFlags::TYPE_OBJECT_OK),
            row!($owner, "uint", uint, RowFlags::TYPE_OBJECT_OK),
            row!($owner, "uint8", uint8, RowFlags::TYPE_OBJECT_OK),
            row!($owner, "uint16", uint16, RowFlags::TYPE_OBJECT_OK),
            row!($owner, "uint32", uint32, RowFlags::TYPE_OBJECT_OK),
            row!($owner, "uint64", uint64, RowFlags::TYPE_OBJECT_OK),
            row!($owner, "byte", byte, RowFlags::TYPE_OBJECT_OK),
        ]
    };
}

pub(super) static INT_ROWS: &[MethodRow] = &[
    row!("Int", "is-prime", is_prime, RowFlags::DEFINITE),
    row!("Int", "narrow", narrow, RowFlags::DEFINITE),
    row!("Int", "conj", conj_real),
    row!("Int", "Bridge", bridge),
    row!("Int", "lsb", lsb, RowFlags::DEFINITE),
    row!("Int", "msb", msb, RowFlags::DEFINITE),
    row!("Int", "chr", chr, RowFlags::DEFINITE_MULTI),
    row!("Int", "rand", rand, RowFlags::RANDOM),
    row!("Int", "bits", bits, RowFlags::TYPE_OBJECT_OK),
    row1!("Int", "power-up", power_up, RowFlags::DEFINITE),
    row1!("Int", "power-down", power_down, RowFlags::DEFINITE),
];
pub(super) static NUM_ROWS: &[MethodRow] = &[
    row!("Num", "is-prime", is_prime),
    row!("Num", "narrow", narrow, RowFlags::DEFINITE),
    row!("Num", "conj", conj_real),
    row!("Num", "Bridge", bridge),
    row!("Num", "rand", rand, RowFlags::RANDOM),
];
pub(super) static RAT_ROWS: &[MethodRow] = &[
    row!("Rat", "is-prime", is_prime),
    row!("Rat", "narrow", narrow, RowFlags::DEFINITE),
    row!("Rat", "conj", conj_real),
    row!("Rat", "Bridge", bridge),
    row!("Rat", "rand", rand, RowFlags::RANDOM),
];
pub(super) static FAT_RAT_ROWS: &[MethodRow] = &[
    row!("FatRat", "narrow", narrow, RowFlags::DEFINITE),
    row!("FatRat", "Bridge", bridge),
];
pub(super) static COMPLEX_ROWS: &[MethodRow] =
    &[row!("Complex", "narrow", narrow, RowFlags::DEFINITE)];
pub(super) static COOL_ROWS: &[MethodRow] = &[
    row!("Cool", "is-prime", is_prime),
    row!("Cool", "conj", conj),
    row!("Cool", "chr", chr, RowFlags::DEFINITE_MULTI),
    row!("Cool", "rand", rand, RowFlags::RANDOM),
];
pub(super) static COOL_NATIVE_INT_ROWS: &[MethodRow] = native_int_rows!("Cool");
pub(super) static INT_NATIVE_INT_ROWS: &[MethodRow] = native_int_rows!("Int");

/// `.is-prime`: the primality of the receiver's integer value (`false` for a
/// negative or non-integral one, the `X::Numeric::Real` failure for a
/// `Complex` with an imaginary part).
// Cost: O(b^3) worst case for a b-bit receiver (probabilistic primality for a
// big Int); O(sqrt(n)) at most for a word-sized one.
fn is_prime(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match numify(target) {
        Ok(number) => crate::builtins::methods_0arg::coercion::value_is_prime(&number),
        Err(failure) => Ok(failure),
    }
}

/// `.conj` of a real number: itself.
// Cost: O(1).
fn conj_real(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    super::complex::conj(target, args)
}

/// `Cool.conj`: the numified receiver's conjugate.
// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn conj(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    match numify(target) {
        Ok(number) => super::complex::conj(&number, args),
        Err(failure) => Ok(failure),
    }
}

/// Whether the relative difference of `a` and `b` is within `tol`, the
/// tolerance `.narrow` uses (`1e-15`).
fn approx(a: f64, b: f64) -> bool {
    let max = a.abs().max(b.abs());
    max == 0.0 || (a - b).abs() / max <= 1e-15
}

/// `.narrow`: the simplest numeric type that holds the value.
// Cost: O(1).
fn narrow(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(match target.view() {
        ValueView::Int(i) => Value::int(i),
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) if d != 0 && n % d == 0 => Value::int(n / d),
        ValueView::Rat(n, d) => Value::rat_raw(n, d),
        ValueView::Num(f) if f.is_finite() => {
            let rounded = f.round();
            if approx(f, rounded) {
                Value::int(rounded as i64)
            } else {
                Value::num(f)
            }
        }
        ValueView::Num(f) => Value::num(f),
        ValueView::Complex(re, im) => {
            if approx_zero(im, re) {
                // Narrow to the real part, then try narrowing that to an Int.
                let rounded = re.round();
                if re.is_finite() && approx(re, rounded) {
                    Value::int(rounded as i64)
                } else {
                    Value::num(re)
                }
            } else {
                // Drop a negligible real part.
                Value::complex(if approx_zero(re, im) { 0.0 } else { re }, im)
            }
        }
        _ => target.clone(),
    })
}

/// Whether `part` is negligible next to `other` (within `1e-15` of the larger
/// magnitude).
fn approx_zero(part: f64, other: f64) -> bool {
    let max = part.abs().max(other.abs());
    part == 0.0 || max == 0.0 || part.abs() / max <= 1e-15
}

/// `.Bridge`: a real number as a `Num`.
// Cost: O(1) (O(d) for a big Int or rational, d = limbs).
fn bridge(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::num(target.to_f64()))
}

/// `Int.bits`: `Inf`, the width of an `Int` type object (`multi method
/// bits(Int:U:)`); an instance is refused as a `:U` invocant is.
// Cost: O(1).
fn bits(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    if matches!(target.view(), ValueView::Package(_)) {
        return Ok(Value::num(f64::INFINITY));
    }
    Err(RuntimeError::parameter_invalid_concreteness(
        "Int", "Int", "bits", "self", false, true,
    ))
}

/// The exponent of `power-up`/`power-down` (`Int:D $b`) as an integer value.
// Cost: O(1).
fn exponent(args: &[Value]) -> Result<Value, RuntimeError> {
    match args {
        [b] => match b.view() {
            ValueView::Int(_) | ValueView::BigInt(_) => Ok(b.clone()),
            ValueView::Bool(flag) => Ok(Value::int(i64::from(flag))),
            _ => Err(RuntimeError::new("power exponent must be an Int")),
        },
        _ => Err(RuntimeError::new("power exponent must be an Int")),
    }
}

/// Whether the integer value `b` is below zero.
// Cost: O(1).
fn is_negative(b: &Value) -> bool {
    match b.view() {
        ValueView::Int(n) => n < 0,
        ValueView::BigInt(n) => n.sign() == num_bigint::Sign::Minus,
        _ => false,
    }
}

/// `Int.power-up($b)`: `self ** $b` for a non-negative exponent; a negative
/// one is an `X::Numeric::Overflow` failure.
// Cost: O(1) for a word-sized result; as `**` otherwise.
fn power_up(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let b = exponent(args)?;
    if is_negative(&b) {
        return Ok(RuntimeError::numeric_overflow_failure());
    }
    Ok(crate::builtins::arith::arith_pow(target.clone(), b))
}

/// `Int.power-down($b)`: `self ** $b` for a negative exponent (a `Rat`, or a
/// `Num` once the denominator leaves 64 bits); `1.0` for a zero exponent or a
/// base of one; any other positive exponent is an `X::Numeric::Underflow`
/// failure, as the power would be an integer greater than one.
// Cost: O(1) for a word-sized result; as `**` otherwise.
fn power_down(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    let b = exponent(args)?;
    let one = matches!(target.view(), ValueView::Int(1) | ValueView::Bool(true));
    if one || matches!(b.view(), ValueView::Int(0)) {
        return Ok(Value::rat_raw(1, 1));
    }
    if is_negative(&b) {
        return Ok(crate::builtins::arith::arith_pow(target.clone(), b));
    }
    Ok(RuntimeError::numeric_underflow_failure())
}

/// `Int.lsb`: the index of the least significant set bit, `Nil` for zero.
// Cost: O(1) for a word-sized receiver; O(b) for a big one, b = bits.
pub(crate) fn lsb(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(match target.view() {
        ValueView::Int(0) | ValueView::Bool(false) => Value::NIL,
        ValueView::Int(i) => Value::int(i64::from(i.unsigned_abs().trailing_zeros())),
        ValueView::Bool(true) => Value::int(0),
        ValueView::BigInt(n) => {
            if num_traits::Zero::is_zero(n.as_ref()) {
                Value::NIL
            } else {
                // `trailing_zeros` is `Some` for any non-zero value.
                Value::int(n.magnitude().trailing_zeros().unwrap_or(0) as i64)
            }
        }
        _ => return Err(RuntimeError::new("Int.lsb: receiver is not an Int")),
    })
}

/// `Int.msb`: the index of the most significant bit (the bit length of a
/// negative value's `-n - 1`), `Nil` for zero.
// Cost: O(1) for a word-sized receiver; O(b) for a big one, b = bits.
pub(crate) fn msb(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(match target.view() {
        ValueView::Int(0) | ValueView::Bool(false) => Value::NIL,
        ValueView::Int(i) if i > 0 => Value::int(i64::from(63 - i.leading_zeros())),
        ValueView::Int(-1) | ValueView::Bool(true) => Value::int(0),
        ValueView::Int(i) => {
            let m = i.unsigned_abs().saturating_sub(1);
            Value::int(i64::from(64 - m.leading_zeros()))
        }
        ValueView::BigInt(n) => {
            use num_bigint::Sign;
            if num_traits::Zero::is_zero(n.as_ref()) {
                Value::NIL
            } else if n.sign() == Sign::Minus {
                if **n == num_bigint::BigInt::from(-1i8) {
                    Value::int(0)
                } else {
                    let m = n.magnitude() - 1u8;
                    Value::int(m.bits() as i64)
                }
            } else {
                Value::int(n.magnitude().bits() as i64 - 1)
            }
        }
        _ => return Err(RuntimeError::new("Int.msb: receiver is not an Int")),
    })
}

/// `.chr`: the character with the receiver's codepoint (NFC-normalized, since
/// some codepoints decompose); an out-of-range codepoint is an error.
// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
fn chr(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let number = match numify(target) {
        Ok(number) => number,
        Err(failure) => return Ok(failure),
    };
    let out_of_bounds = |display: String, hex: String| {
        RuntimeError::new(format!(
            "Codepoint {display} (0x{hex}) is out of bounds in 'chr'"
        ))
    };
    let code = match number.view() {
        ValueView::BigInt(n) => {
            return Err(out_of_bounds(n.to_string(), format!("{:X}", &**n)));
        }
        // A Failure or a non-number: the codepoint is 0, as before.
        _ => super::coerce::int_of(&number)
            .and_then(|v| v.as_int())
            .unwrap_or_default(),
    };
    if crate::builtins::str_prim::codepoint::is_surrogate(code) {
        return Err(crate::builtins::str_prim::codepoint::surrogate_error(code));
    }
    match u32::try_from(code).ok().and_then(char::from_u32) {
        Some(ch) if code <= 0x10FFFF => {
            use unicode_normalization::UnicodeNormalization;
            Ok(Value::str(ch.to_string().nfc().collect::<String>()))
        }
        // MoarVM's own bound: a codepoint it could store but Unicode lacks.
        _ if (0x110000..1 << 31).contains(&code) => Err(RuntimeError::new(format!(
            "chr codepoint {code} (0x{code:X}) is out of bounds"
        ))),
        _ => {
            let hex = if code < 0 {
                format!("-{:X}", code.unsigned_abs())
            } else {
                format!("{code:X}")
            };
            Err(out_of_bounds(code.to_string(), hex))
        }
    }
}

macro_rules! native_int_handlers {
    ($($handler:ident: $name:literal;)*) => {
        $(
            // Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
            fn $handler(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
                // An `Instant` or `Duration` is its seconds, a `Range` its count.
                match super::numify::temporal_seconds(target)
                    .or_else(|| super::numify::numeric_range_elems(target))
                {
                    Some(seconds) => {
                        crate::value::raku_repr::native_int_coerce_method(&seconds, $name)
                    }
                    None => crate::value::raku_repr::native_int_coerce_method(target, $name),
                }
            }
        )*
    };
}

native_int_handlers! {
    int: "int";
    int8: "int8";
    int16: "int16";
    int32: "int32";
    int64: "int64";
    uint: "uint";
    uint8: "uint8";
    uint16: "uint16";
    uint32: "uint32";
    uint64: "uint64";
    byte: "byte";
}

/// `.rand`: a `Num` in `[0, value)`; a `List`, `Array` or `Hash` numifies to
/// its element count.
// Cost: O(1) (O(n) for a Str receiver, n = chars of the parse).
pub(crate) fn rand(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let max = match numify(target) {
        Ok(number) => number.to_f64(),
        Err(failure) => return Ok(failure),
    };
    Ok(Value::num(crate::builtins::rng::builtin_rand() * max))
}
