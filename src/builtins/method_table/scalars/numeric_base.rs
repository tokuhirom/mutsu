//! The numeric types' radix methods: `base` on `Int`, `Num`, `Rat` and
//! `FatRat`, and `base-repeating` on `Rat` (ADR-11276 §9.70, §9.104).
//!
//! `base($radix)` and `base($radix, $digits)` are two arities of one routine,
//! [`base_of`]. The radix is bound first, as each owner's signature binds it
//! ([`bind_radix`]): `Int`'s candidates coerce it (`Int(Cool) $base`),
//! `Rational`'s take only an `Int:D` (any other is `X::Multi::NoMatch`), and
//! `Num`'s and `Real`'s parameter is `Int:D $base` (any other is
//! `X::TypeCheck::Binding::Parameter`). A radix out of range is the
//! `X::OutOfRange` `Failure` Rakudo answers. The
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
    if let Err(err) = bind_radix(target, &args[0]) {
        return Some(Err(err));
    }
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
    match bind_radix(target, &args[0]) {
        Ok(radix) => base_of(target, &radix, None),
        Err(err) => Some(Err(err)),
    }
}

fn base_2(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match bind_radix(target, &args[0]) {
        Ok(radix) => base_of(target, &radix, Some(&args[1])),
        Err(err) => Some(Err(err)),
    }
}

/// The `Int` an `Int:D` radix argument stands for: an `Int`, a `Bool`, an
/// `Int`-valued enum or an `IntStr`; `None` for any other value.
// Cost: O(1), a tag probe.
fn int_radix(arg: &Value) -> Option<Value> {
    match arg.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => Some(arg.clone()),
        ValueView::Bool(b) => Some(Value::int(i64::from(b))),
        ValueView::Enum {
            value: crate::value::EnumValue::Int(i),
            ..
        } => Some(Value::int(*i)),
        ValueView::Mixin(inner, _) => int_radix(&inner),
        _ => None,
    }
}

/// Bind `.base`'s radix argument the way the receiver's candidates do: the
/// `Int` radix for the rows to read, or the error binding it raises. A type
/// object and an argument of a kind the signatures do not name keep the old
/// answer (the radix as given, which [`radix_arg`] reads).
// Cost: O(n) to numify a Str radix, n = its chars; O(1) otherwise.
pub(crate) fn bind_radix(target: &Value, radix: &Value) -> Result<Value, RuntimeError> {
    if let Some(radix) = int_radix(radix) {
        return Ok(radix);
    }
    match target.view() {
        // `Int(Cool) $base`: a Str numifies, a fraction truncates.
        ValueView::Int(_) | ValueView::BigInt(_) | ValueView::Bool(_) => int_cool_radix(radix),
        ValueView::Rat(..) => rational_radix("Rat", radix),
        ValueView::FatRat(..) | ValueView::BigRat(..) => rational_radix("FatRat", radix),
        ValueView::Num(_) => bind_real_radix(radix),
        _ => Ok(radix.clone()),
    }
}

/// `Num.base` and `Real.base` (an `Instant`, a `Duration`): `Int:D $base`.
// Cost: O(t) to render a refused argument, t = its `.raku` length; O(1) otherwise.
pub(crate) fn bind_real_radix(radix: &Value) -> Result<Value, RuntimeError> {
    if let Some(radix) = int_radix(radix) {
        return Ok(radix);
    }
    if radix_is_unbound(radix) {
        return Ok(radix.clone());
    }
    Err(
        crate::runtime::utils::typecheck_binding_parameter_with_hint(
            "$base",
            "Int",
            radix,
            &crate::runtime::utils::value_short_repr(radix),
            None,
        ),
    )
}

/// Whether binding leaves `radix` to [`radix_arg`]: a type object or `Nil`,
/// whose messages differ per candidate, or a value no candidate names.
// Cost: O(1), a tag probe.
fn radix_is_unbound(radix: &Value) -> bool {
    !matches!(
        radix.view(),
        ValueView::Str(_) | ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::Num(_)
    )
}

/// `Int(Cool) $base`: a numeric Str numifies (a bad one raises
/// `X::Str::Numeric`), and a fraction truncates to its `Int`.
// Cost: O(n) for a Str radix, n = its chars; O(1) otherwise.
fn int_cool_radix(radix: &Value) -> Result<Value, RuntimeError> {
    let number = match radix.view() {
        ValueView::Str(s) => crate::value::str_numeric::parse_raku_str_to_numeric(&s)
            .ok_or_else(|| crate::builtins::methods_0arg::str_numeric_error(&s))?,
        _ => radix.clone(),
    };
    let truncated = match number.view() {
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) if d != 0 => Value::int(n / d),
        ValueView::Num(f) if f.is_finite() => Value::int(f.trunc() as i64),
        _ => number.clone(),
    };
    Ok(truncated)
}

/// `Rational.base`: every candidate takes `Int:D $base`, so any other radix has
/// no candidate.
// Cost: O(1).
fn rational_radix(type_name: &str, radix: &Value) -> Result<Value, RuntimeError> {
    if radix_is_unbound(radix) {
        return Ok(radix.clone());
    }
    Err(
        crate::runtime::methods_signature_errors::make_multi_no_match_error_detailed(
            "base",
            type_name,
            true,
            &format!("{}:D", crate::value::types::what_type_name(radix)),
            &RATIONAL_BASE_SIGNATURES.map(str::to_string),
        ),
    )
}

/// The candidates `Rational.base` lists in its dispatch failure.
const RATIONAL_BASE_SIGNATURES: [&str; 3] = [
    "(Rational:D $:: Int:D $base, *%_ --> Str:D)",
    "(Rational:D $:: Int:D $base, Whatever, *%_ --> Str:D)",
    "(Rational:D $:: Int:D $base, Int(Any) $digits, Bool:D :$no-trailing-zeroes = Bool::False, *%_ --> Str:D)",
];

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
