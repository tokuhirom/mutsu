//! The numeric types' radix methods: `base` on `Int`, `Num`, `Rat` and
//! `FatRat`, and `base-repeating` on `Rat` (ADR-11276 §9.70, §9.104).
//!
//! `base($radix)` and `base($radix, $digits)` are two arities of one routine,
//! [`base_of`]. A bad radix is the `X::OutOfRange` `Failure` Rakudo answers. The
//! cascade arms keep the receivers the table has no shape for (an `Instant` or
//! `Duration` has rows of its own; a user subclass is an instance) and call the
//! same routines.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_narg::allomorph::out_of_range_failure;
use crate::builtins::methods_narg::base::{
    BaseDigits, f64_to_rat, rat_base_repeating, rat_to_base,
};
use crate::value::{ArrayKind, RuntimeError, Value, ValueView};

/// The two arities of `base`, owned by `$owner`. `$flags` is
/// `RowFlags::DEFINITE_MULTI` where every candidate is `:D` (`Int.base(2)`
/// finds none).
macro_rules! base_rows {
    ($owner:literal, $flags:expr) => {
        &[
            MethodRow {
                owner: $owner,
                name: "base",
                arity: 1,
                handler: Handler::Narrow(base_1),
                flags: RowFlags::ANY_ARGS.or($flags),
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "base",
                arity: 2,
                handler: Handler::Narrow(base_2),
                flags: RowFlags::ANY_ARGS.or($flags),
                named: &[],
            },
        ]
    };
}

pub(super) static INT_ROWS: &[MethodRow] = base_rows!("Int", RowFlags::DEFINITE_MULTI);
pub(super) static NUM_ROWS: &[MethodRow] = base_rows!("Num", RowFlags::NONE);
/// Rakudo declares `base` on `Rational`, which `FatRat` composes too.
pub(super) static FAT_RAT_ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "FatRat",
        name: "base",
        arity: 1,
        handler: Handler::Named(base_named),
        flags: RowFlags::ANY_ARGS.or(RowFlags::DEFINITE_MULTI),
        named: RATIONAL_BASE_NAMED,
    },
    MethodRow {
        owner: "FatRat",
        name: "base",
        arity: 2,
        handler: Handler::Named(base_named),
        flags: RowFlags::ANY_ARGS.or(RowFlags::DEFINITE_MULTI),
        named: RATIONAL_BASE_NAMED,
    },
];

/// The adverb `Rational.base` declares (ADR-11276 §9.168): every fractional
/// digit that is a trailing zero goes, and the radix point with the last one.
const RATIONAL_BASE_NAMED: &[&str] = &["no-trailing-zeroes"];

/// `Rational.base($radix, $digits?, :no-trailing-zeroes)`.
// Cost: as [`base_1`] / [`base_2`], plus O(n) to strip, n = chars rendered.
fn base_named(
    target: &Value,
    args: &[Value],
    named: crate::builtins::method_table::Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    if !named.pairs().is_empty() {
        let mut all = args.to_vec();
        all.extend_from_slice(named.pairs());
        if let Some(answer) = crate::builtins::native_base_with_options(target, &all) {
            return Some(answer);
        }
    }
    match args.len() {
        1 => base_1(target, args),
        _ => base_2(target, args),
    }
}
pub(super) static RAT_ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Rat",
        name: "base",
        arity: 1,
        handler: Handler::Named(base_named),
        flags: RowFlags::ANY_ARGS,
        named: RATIONAL_BASE_NAMED,
    },
    MethodRow {
        owner: "Rat",
        name: "base",
        arity: 2,
        handler: Handler::Named(base_named),
        flags: RowFlags::ANY_ARGS,
        named: RATIONAL_BASE_NAMED,
    },
    MethodRow {
        owner: "Rat",
        name: "base-repeating",
        arity: 0,
        handler: Handler::Narrow(base_repeating_0),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Rat",
        name: "base-repeating",
        arity: 1,
        handler: Handler::Narrow(base_repeating_1),
        flags: RowFlags::ANY_ARGS,
        named: &[],
    },
];

/// `base-repeating`, which Rakudo declares on `Rational`, for a `FatRat`.
pub(super) static FAT_RAT_REPEATING_ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "FatRat",
        name: "base-repeating",
        arity: 0,
        handler: Handler::Narrow(base_repeating_0),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "FatRat",
        name: "base-repeating",
        arity: 1,
        handler: Handler::Narrow(base_repeating_1),
        flags: RowFlags::ANY_ARGS,
        named: &[],
    },
];

fn base_1(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    base_of(target, &args[0], None)
}

fn base_2(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    base_of(target, &args[0], Some(&args[1]))
}

fn base_repeating_0(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    base_repeating_of(target, &Value::int(10))
}

fn base_repeating_1(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    base_repeating_of(target, &args[0])
}

/// A radix argument: an `Int` or an integer-valued `Str` in `2..=36`. Any other
/// is the `X::OutOfRange` `Failure` (`Err`) `.base` answers.
// Cost: O(n), n = chars of a Str radix; O(1) otherwise.
fn radix_arg(arg: &Value) -> Result<u32, Value> {
    let radix = match arg.view() {
        ValueView::Int(r) => Some(r),
        ValueView::Str(s) => s.parse::<i64>().ok(),
        _ => None,
    };
    match radix {
        Some(r) if (2..=36).contains(&r) => Ok(r as u32),
        Some(r) => Err(out_of_range_failure(&format!(
            "base argument to base out of range. Is: {r}, should be in 2..36"
        ))),
        None => Err(out_of_range_failure("base requires radix 2..36")),
    }
}

/// The fractional-digits argument of `.base($radix, $digits)`: a non-negative
/// `Int`, or `*` for as many as the number has. `None` for any other value
/// (the call takes the cascades).
fn digits_arg(arg: &Value) -> Option<Result<BaseDigits, Value>> {
    match arg.view() {
        ValueView::Int(d) if d < 0 => {
            Some(Err(out_of_range_failure("digits must be non-negative")))
        }
        ValueView::Int(d) => Some(Ok(BaseDigits::Fixed(d as u32))),
        ValueView::Whatever => Some(Ok(BaseDigits::Whatever)),
        _ => None,
    }
}

/// `.base($radix)` / `.base($radix, $digits)` of a number: its digits in that
/// radix. With no `$digits` an integer has none after the point and a
/// fraction gets as many as its denominator needs (at least six). `None` for a
/// receiver the rows do not cover (an instance), whose
/// call takes the cascades.
// Cost: O(d * r) for a fraction, d = digits produced, r = the radix digits'
// size; O(b^2) for a big Int, b = size in bits.
pub(crate) fn base_of(
    target: &Value,
    radix: &Value,
    digits: Option<&Value>,
) -> Option<Result<Value, RuntimeError>> {
    let mode = match digits {
        None => None,
        Some(arg) => match digits_arg(arg)? {
            Ok(mode) => Some(mode),
            Err(failure) => return Some(Ok(failure)),
        },
    };
    // The integer `Int` form has no fraction machinery; a `NaN` or infinite
    // `Num` fails before its radix is looked at.
    let fraction = match target.view() {
        ValueView::Int(i) => (i, 1),
        ValueView::Bool(b) => (i64::from(b), 1),
        ValueView::BigInt(_) => {
            let radix = match radix_arg(radix) {
                Ok(radix) => radix,
                Err(failure) => return Some(Ok(failure)),
            };
            let mut text = crate::builtins::int_to_base(target, radix);
            // An integer has no fraction: `$digits` only pads it with zeros.
            if let Some(BaseDigits::Fixed(digits)) = mode
                && digits > 0
            {
                text.push('.');
                text.extend(std::iter::repeat_n('0', digits as usize));
            }
            return Some(Ok(Value::str(text)));
        }
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => (n, d),
        ValueView::Num(f) if !f.is_finite() => {
            return Some(Err(RuntimeError::new(format!(
                "X::Numeric::CannotConvert: Cannot convert {} to Int",
                if f.is_nan() {
                    "NaN"
                } else if f > 0.0 {
                    "Inf"
                } else {
                    "-Inf"
                },
            ))));
        }
        ValueView::Num(f) => f64_to_rat(f),
        _ => return None,
    };
    let radix = match radix_arg(radix) {
        Ok(radix) => radix,
        Err(failure) => return Some(Ok(failure)),
    };
    let (n, d) = fraction;
    let text = match (mode, target.view()) {
        (None, ValueView::Int(_) | ValueView::Bool(_)) => {
            crate::builtins::int_to_base(&Value::int(n), radix)
        }
        (mode, _) => rat_to_base(n, d, radix, mode.unwrap_or(BaseDigits::Auto)),
    };
    Some(Ok(Value::str(text)))
}

/// `Rational.base-repeating($radix)`: the digits before the repeating part and
/// the repeating part, as a two-element list of strings, for a `Rat` and a
/// `FatRat`.
// Cost: O(d * r), d = digits until the remainder repeats (at most the
// denominator), r = the radix digits' size.
pub(crate) fn base_repeating_of(
    target: &Value,
    radix: &Value,
) -> Option<Result<Value, RuntimeError>> {
    use num_bigint::BigInt;
    let (n, d) = match target.view() {
        // Declared on `Rational` (`Rat` and `FatRat`): Rakudo has no
        // `Int`/`Num` candidate.
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => (BigInt::from(n), BigInt::from(d)),
        ValueView::BigRat(n, d) => (n.clone(), d.clone()),
        _ => return None,
    };
    let radix = match radix_arg(radix) {
        Ok(radix) => radix,
        Err(failure) => return Some(Ok(failure)),
    };
    let (non_repeating, repeating) = rat_base_repeating(&n, &d, radix);
    Some(Ok(Value::array_with_kind(
        crate::gc::Gc::new(crate::value::ArrayData::new(vec![
            Value::str(non_repeating),
            Value::str(repeating),
        ])),
        ArrayKind::List,
    )))
}
