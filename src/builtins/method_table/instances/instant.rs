//! `Instant`'s and `Duration`'s rows (ADR-11276 §9.18).
//!
//! Both classes `do Real`, and both keep their seconds in a `value` attribute
//! (a `Rat` of nanosecond resolution for a `Duration`, an `Int`, `Rat` or `Num`
//! for an `Instant`). Every `Real` method Rakudo composes into them reads that
//! number, so a row's handler answers the question of the inner number and
//! wraps the answer only where the method keeps the type (`abs`, `succ`, `pred`).
//! The rest of `Real`'s numeric surface (`sin`, `floor`, `sign`, ...) is
//! `Cool`'s, reached through the shapes' MRO (`numify` reads the same
//! attribute).
//!
//! Only the built-in classes have a shape: an instance of a subclass has
//! another class name, so it never took the cascade arms these rows replace.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::temporal;
use crate::builtins::methods_0arg::temporal_dispatch::real_role_step;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// A zero-argument row of `owner` for `name` with a pure handler.
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

/// The rows `Instant` and `Duration` both declare.
macro_rules! real_rows {
    ($owner:literal) => {
        &[
            row!($owner, "Bool", truthiness),
            row!($owner, "Bridge", bridge),
            row!($owner, "Int", int),
            row!($owner, "Num", num),
            row!($owner, "Rat", rat),
            row!($owner, "FatRat", fat_rat),
            row!($owner, "Complex", complex),
            MethodRow {
                owner: $owner,
                name: "Rat",
                arity: 1,
                handler: Handler::Narrow(rat_with),
                flags: RowFlags::ANY_ARGS,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "FatRat",
                arity: 1,
                handler: Handler::Narrow(fat_rat_with),
                flags: RowFlags::ANY_ARGS,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "base",
                arity: 1,
                handler: Handler::Narrow(base_with),
                flags: RowFlags::ANY_ARGS,
                named: &[],
            },
            MethodRow {
                owner: $owner,
                name: "base",
                arity: 2,
                handler: Handler::Narrow(base_with_digits),
                flags: RowFlags::ANY_ARGS,
                named: &[],
            },
            row!($owner, "Numeric", itself),
            row!($owner, "Real", itself),
            row!($owner, "conj", itself),
            row!($owner, "Str", rendered),
            row!($owner, "gist", rendered),
            row!($owner, "abs", abs),
            row!($owner, "narrow", narrow),
            row!($owner, "isNaN", is_nan),
            row!($owner, "tai", tai),
            row!($owner, "succ", succ),
            row!($owner, "pred", pred),
            row!($owner, "to-nanos", to_nanos),
            row!($owner, "rand", rand, RowFlags::RANDOM),
        ]
    };
}

pub(super) static INSTANT_ROWS: &[MethodRow] = real_rows!("Instant");
pub(super) static DURATION_ROWS: &[MethodRow] = real_rows!("Duration");

/// `Instant`'s own rows.
pub(super) static INSTANT_OWN_ROWS: &[MethodRow] = &[
    row!("Instant", "raku", instant_raku),
    row!("Instant", "to-posix", to_posix),
    row!("Instant", "Date", to_date),
    row!("Instant", "DateTime", to_datetime),
    row!("Instant", "Instant", itself, RowFlags::TYPE_OBJECT_OK),
];

/// `Duration`'s own rows.
pub(super) static DURATION_OWN_ROWS: &[MethodRow] = &[row!("Duration", "raku", duration_raku)];

/// The seconds an `Instant` or `Duration` holds: its `value` attribute, or zero
/// for an instance made without one.
// Cost: O(a), a = attributes of the instance (one lookup in a copy of the map).
fn seconds(target: &Value) -> Value {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => attributes
            .as_map()
            .get("value")
            .cloned()
            .unwrap_or_else(|| {
                if class_name == "Duration" {
                    Value::num(0.0)
                } else {
                    Value::int(0)
                }
            }),
        _ => Value::int(0),
    }
}

/// `self`, for the methods `Real` answers with the value itself.
// Cost: O(1).
fn itself(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(target.clone())
}

/// `Real.Bool`: a nonzero number of seconds.
// Cost: O(1).
fn truthiness(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(seconds(target).truthy()))
}

/// A zero-argument method of the seconds, answered by the method of the same
/// name of the number they are.
// Cost: O(1) plus the number's own method.
fn of_seconds(target: &Value, method: &str) -> Result<Value, RuntimeError> {
    let number = seconds(target);
    crate::builtins::native_method_0arg(&number, Symbol::intern(method)).unwrap_or_else(|| {
        Err(RuntimeError::new(format!(
            "No such method '{method}' for invocant of type 'Real'"
        )))
    })
}

/// `Real.Bridge`: the seconds as a `Num`.
// Cost: O(1).
fn bridge(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::num(seconds(target).to_f64()))
}

/// `Real.Int`: the seconds truncated to an `Int`.
// Cost: O(1) for word-sized seconds; O(b) for big ones, b = size in bits.
fn int(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    of_seconds(target, "Int")
}

/// `Real.Num`: the seconds as a `Num`.
// Cost: O(1).
fn num(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    of_seconds(target, "Num")
}

/// `Real.Rat`: the seconds as a `Rat`.
// Cost: O(1) for word-sized seconds; O(b) for big ones, b = size in bits.
fn rat(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    of_seconds(target, "Rat")
}

/// `Real.FatRat`: the seconds as a `FatRat`.
// Cost: O(1) for word-sized seconds; O(b) for big ones, b = size in bits.
fn fat_rat(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    of_seconds(target, "FatRat")
}

/// `Real.Complex`: the seconds with a zero imaginary part.
// Cost: O(1).
fn complex(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::complex(seconds(target).to_f64(), 0.0))
}

/// A one-argument method of the seconds (`Rat($epsilon)`).
// Cost: O(1) plus the number's own method.
fn of_seconds_with(
    target: &Value,
    args: &[Value],
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    let [epsilon] = args else {
        return None;
    };
    crate::builtins::native_method_1arg(&seconds(target), Symbol::intern(method), epsilon)
}

/// `Real.base($radix)`: the seconds written in that radix.
// Cost: O(d), d = digits written.
fn base_with(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    of_seconds_with(target, args, "base")
}

/// `Real.base($radix, $digits)`: the seconds written in that radix with that many
/// fractional digits (`*` for as many as needed).
// Cost: O(d), d = digits written.
fn base_with_digits(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let [radix, digits] = args else {
        return None;
    };
    crate::builtins::native_method_2arg(&seconds(target), Symbol::intern("base"), radix, digits)
}

/// `Real.Rat($epsilon)`.
// Cost: O(1) for word-sized seconds.
fn rat_with(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    of_seconds_with(target, args, "Rat")
}

/// `Real.FatRat($epsilon)`.
// Cost: O(1) for word-sized seconds.
fn fat_rat_with(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    of_seconds_with(target, args, "FatRat")
}

/// `Real.isNaN`.
// Cost: O(1).
fn is_nan(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    of_seconds(target, "isNaN")
}

/// `tai`: the seconds themselves.
// Cost: O(1).
fn tai(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(seconds(target))
}

/// `Str` and `gist`: `Instant:<tai>` for an `Instant`, the seconds for a
/// `Duration` (`value::display`).
// Cost: O(1).
fn rendered(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str(target.to_string_value()))
}

/// The instance with its seconds replaced by `seconds`.
// Cost: O(a), a = attributes of the instance.
fn with_seconds(target: &Value, seconds: Value) -> Value {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => {
            let mut attrs = attributes.to_map();
            attrs.insert("value".to_string(), seconds);
            Value::make_instance(class_name, attrs)
        }
        _ => target.clone(),
    }
}

/// `Real.abs`: keeps the type (`Instant.abs` is an `Instant`, `Duration.abs` a
/// `Duration`), since Rakudo's is `self < 0 ?? -self !! self`.
// Cost: O(a), a = attributes of the instance.
fn abs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    match crate::builtins::method_table::real::abs_of(&seconds(target)) {
        Some(result) => Ok(with_seconds(target, result?)),
        None => Ok(target.clone()),
    }
}

/// `Real.narrow`: the seconds in their narrowest type, an `Int` when whole.
// Cost: O(1).
fn narrow(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let seconds = seconds(target);
    // Rakudo's `Duration` always holds a Rat, and every constructor here stores
    // one (`arith::tai_rat`, #11273); a Num can only come from a hand-built
    // instance, which narrows through the same Num -> Rat conversion `.Rat`
    // uses.
    let seconds = match seconds.view() {
        ValueView::Num(f) if f.is_finite() => crate::builtins::arith::real_to_rat(&seconds),
        _ => seconds.clone(),
    };
    Ok(match seconds.view() {
        ValueView::Rat(n, d) if d != 0 && n % d == 0 => Value::int(n / d),
        ValueView::Rat(n, d) => Value::rat_raw(n, d),
        _ => seconds.clone(),
    })
}

/// `Real.succ`: one second later, keeping the type.
// Cost: O(a), a = attributes of the instance.
fn succ(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    step(target, true)
}

/// `Real.pred`: one second earlier, keeping the type.
// Cost: O(a), a = attributes of the instance.
fn pred(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    step(target, false)
}

fn step(target: &Value, forward: bool) -> Result<Value, RuntimeError> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => real_role_step(class_name, attributes.to_map(), forward),
        _ => Ok(target.clone()),
    }
}

/// `Real.rand`: a `Num` below the seconds.
// Cost: O(1).
fn rand(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    crate::builtins::method_table::real_misc::rand(&seconds(target), &[])
}

/// `to-nanos`: the seconds in nanoseconds, truncated to an `Int`.
// Cost: O(1) for word-sized seconds; O(b) for big ones, b = size in bits.
fn to_nanos(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let nanos = crate::builtins::arith::arith_mul(seconds(target), Value::int(1_000_000_000));
    match crate::builtins::method_table::coerce::int_of(&nanos) {
        Some(nanos) => Ok(nanos),
        None => Ok(nanos),
    }
}

/// `Instant.to-posix`: the POSIX seconds, and whether the second is a leap one.
// Cost: O(l), l = leap seconds (a table of 28).
fn to_posix(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let tai = seconds(target).to_f64();
    let tai_int = tai.floor() as i64;
    let is_leap = temporal::LEAP_SECONDS
        .iter()
        .skip(1)
        .any(|&(threshold, cumulative)| tai_int == threshold + (cumulative - 1));
    let posix = temporal::instant_to_posix(tai);
    let posix = if posix == posix.floor() {
        Value::int(posix as i64)
    } else {
        Value::num(posix)
    };
    Ok(Value::array(vec![posix, Value::truth(is_leap)]))
}

/// `Instant.DateTime`: the UTC date and time of the instant.
// Cost: O(l), l = leap seconds (a table of 28).
fn to_datetime(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let seconds = seconds(target);
    let (tai_int, tai_frac) = match seconds.view() {
        ValueView::Rat(n, d) if d != 0 => (n / d, (n % d) as f64 / d as f64),
        _ => {
            let tai = seconds.to_f64();
            (tai.floor() as i64, tai - tai.floor())
        }
    };
    let (year, month, day, hour, minute, second) =
        temporal::instant_to_datetime_leap_aware_parts(tai_int, tai_frac, 0);
    Ok(temporal::make_datetime(
        year, month, day, hour, minute, second, 0,
    ))
}

/// `Instant.Date`: the UTC date of the instant.
// Cost: O(l), l = leap seconds (a table of 28).
fn to_date(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let posix = temporal::instant_to_posix(seconds(target).to_f64());
    let (year, month, day) = temporal::epoch_days_to_civil((posix / 86400.0).floor() as i64);
    Ok(temporal::make_date(year, month, day))
}

/// A number in the form `Instant.raku` and `Duration.raku` print: always with
/// a decimal point (`42.0`, `-400.2`), scientific notation outside the safe
/// `Rat` range, so that the text round-trips through the parser.
// Cost: O(1).
pub(crate) fn format_temporal_num(f: f64) -> String {
    if f.is_nan() {
        return "NaN".to_string();
    }
    if f.is_infinite() {
        return if f > 0.0 { "Inf" } else { "-Inf" }.to_string();
    }
    if f.abs() >= 1e18 {
        return format!("{f:e}");
    }
    let text = format!("{f}");
    if text.contains('.') {
        text
    } else {
        format!("{text}.0")
    }
}

/// `Instant.raku`: `Instant.from-posix(<posix seconds>)`.
// Cost: O(l), l = leap seconds (a table of 28).
fn instant_raku(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let posix = temporal::instant_to_posix(seconds(target).to_f64());
    Ok(Value::str(format!(
        "Instant.from-posix({})",
        format_temporal_num(posix)
    )))
}

/// `Duration.raku`: `Duration.new(<seconds>)`.
// Cost: O(1).
fn duration_raku(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str(format!(
        "Duration.new({})",
        format_temporal_num(seconds(target).to_f64())
    )))
}

/// A hand-built instance, for the tests: an `Instant` or `Duration` holding
/// `seconds`.
#[cfg(test)]
pub(crate) fn sample(class: &str, seconds: Value) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("value".to_string(), seconds);
    Value::make_instance(Symbol::intern(class), attrs)
}
