//! The integer and real numeric methods that are not math functions:
//! `is-prime`, `narrow`, `conj`, `Bridge`, `lsb`, `msb`, `chr`, the native
//! integer coercions (`int8` .. `uint64`, `byte`, `int`, `uint`) and `rand`.
//!
//! Rakudo declares some of them on `Cool` (`is-prime`, `conj`, `chr`, the
//! native coercions, `rand`), where the body is `self.Numeric.METHOD`: those
//! rows read their receiver through [`numify`], the way the math rows do. The
//! rest belong to the numeric owners alone (`narrow`, `Bridge`, `lsb`,
//! `msb`), so a `Str` has no such method, as in Rakudo.

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

/// The native integer coercions `Cool` and `Int` both declare.
macro_rules! native_int_rows {
    ($owner:literal) => {
        &[
            row!($owner, "int", int),
            row!($owner, "int8", int8),
            row!($owner, "int16", int16),
            row!($owner, "int32", int32),
            row!($owner, "int64", int64),
            row!($owner, "uint", uint),
            row!($owner, "uint8", uint8),
            row!($owner, "uint16", uint16),
            row!($owner, "uint32", uint32),
            row!($owner, "uint64", uint64),
            row!($owner, "byte", byte),
        ]
    };
}

pub(super) static INT_ROWS: &[MethodRow] = &[
    row!("Int", "is-prime", is_prime),
    row!("Int", "narrow", narrow),
    row!("Int", "conj", conj_real),
    row!("Int", "Bridge", bridge),
    row!("Int", "lsb", lsb),
    row!("Int", "msb", msb),
    row!("Int", "chr", chr),
    row!("Int", "rand", rand, RowFlags::RANDOM),
];
pub(super) static NUM_ROWS: &[MethodRow] = &[
    row!("Num", "is-prime", is_prime),
    row!("Num", "narrow", narrow),
    row!("Num", "conj", conj_real),
    row!("Num", "Bridge", bridge),
    row!("Num", "rand", rand, RowFlags::RANDOM),
];
pub(super) static RAT_ROWS: &[MethodRow] = &[
    row!("Rat", "is-prime", is_prime),
    row!("Rat", "narrow", narrow),
    row!("Rat", "conj", conj_real),
    row!("Rat", "Bridge", bridge),
    row!("Rat", "rand", rand, RowFlags::RANDOM),
];
pub(super) static FAT_RAT_ROWS: &[MethodRow] = &[
    row!("FatRat", "narrow", narrow),
    row!("FatRat", "Bridge", bridge),
];
pub(super) static COMPLEX_ROWS: &[MethodRow] = &[row!("Complex", "narrow", narrow)];
pub(super) static COOL_ROWS: &[MethodRow] = &[
    row!("Cool", "is-prime", is_prime),
    row!("Cool", "conj", conj),
    row!("Cool", "chr", chr),
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
    match u32::try_from(code).ok().and_then(char::from_u32) {
        Some(ch) if code <= 0x10FFFF => {
            use unicode_normalization::UnicodeNormalization;
            Ok(Value::str(ch.to_string().nfc().collect::<String>()))
        }
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
