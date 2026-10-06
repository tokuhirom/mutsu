//! Date, Instant, and Duration arithmetic helpers.

use super::rat::to_big_rat_parts;
use crate::symbol::Symbol;
use crate::value::{Value, ValueView, make_big_rat_arith};
use num_traits::{FromPrimitive, Zero};

/// A Date-shaped instance always carries `year`/`month`/`day`/`days` (see
/// `make_date_with_formatter`); DateTime carries `year`/`month`/`day` too but
/// never `days` (it stores `epoch` instead), so checking for `days` already
/// distinguishes the two. Duck-typed on attributes rather than on the literal
/// class name `"Date"` so a `class Foo is Date` subclass instance (e.g. from
/// a `Date::YearDay`-style module that calls `self.Date::new(...)` from its
/// own constructor) is recognized as a Date arithmetic operand too, matching
/// Rakudo.
fn is_date_like(value: &Value) -> bool {
    match value.view() {
        ValueView::Mixin(inner, _) => is_date_like(inner),
        ValueView::Instance { attributes, .. } => {
            attributes.contains_key("days") && attributes.contains_key("year")
        }
        _ => false,
    }
}

/// Check if a value is a Date, Instant, or Duration instance (temporal operand for arithmetic).
pub(crate) fn is_temporal_operand(value: &Value) -> bool {
    is_date_like(value)
        || match value.view() {
            ValueView::Mixin(inner, _) => is_temporal_operand(inner),
            // Duck-typed like `is_date_like`: a `class G is DateTime` subclass
            // subtracts and adds exactly as `DateTime` does.
            ValueView::Instance { class_name, .. } => {
                class_name == "Instant"
                    || class_name == "Duration"
                    || instance_datetime_parts(value).is_some()
            }
            _ => false,
        }
}

pub(crate) fn instance_days(value: &Value) -> Option<i64> {
    match value.view() {
        ValueView::Instance { attributes, .. } if is_date_like(value) => {
            match attributes.as_map().get("days").map(Value::view) {
                Some(ValueView::Int(days)) => Some(days),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Build a new Date-shaped instance for an arithmetic result: clones
/// `original`'s full attribute set (preserving any custom subclass
/// attributes, e.g. a `Date::YearDay`-style formatter) and overwrites
/// `year`/`month`/`day`/`days` with the recomputed date, keeping `original`'s
/// own class name. Mirrors Rakudo's `Date::infix:<+>`/`infix:<->`, which are
/// `self.clone(:days(...))` — a clone, not a fresh plain `Date`.
pub(crate) fn rebuild_date_like(original: &Value, new_days: i64) -> Value {
    let class_name = match original.view() {
        ValueView::Instance { class_name, .. } => class_name,
        _ => Symbol::intern("Date"),
    };
    let (y, m, d) = crate::builtins::methods_0arg::temporal::epoch_days_to_civil(new_days);
    let mut attrs = match original.view() {
        ValueView::Instance { attributes, .. } => attributes.to_map(),
        _ => crate::value::AttrMap::new(),
    };
    attrs.insert("year", Value::int(y));
    attrs.insert("month", Value::int(m));
    attrs.insert("day", Value::int(d));
    attrs.insert("days", Value::int(new_days));
    Value::make_instance(class_name, attrs)
}

pub(crate) fn instance_instant_value(value: &Value) -> Option<f64> {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => attributes
            .as_map()
            .get("value")
            .and_then(crate::runtime::to_float_value),
        _ => None,
    }
}

pub(crate) fn instance_instant_raw(value: &Value) -> Option<Value> {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Instant" => attributes.as_map().get("value").cloned(),
        _ => None,
    }
}

pub(crate) fn value_sub(a: Value, b: Value) -> Value {
    let (l, r) = crate::runtime::coerce_numeric(a, b);
    if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r)) {
        return make_big_rat_arith(an * &bd - bn * &ad, ad * bd);
    }
    Value::num(
        crate::runtime::to_float_value(&l).unwrap_or(0.0)
            - crate::runtime::to_float_value(&r).unwrap_or(0.0),
    )
}

pub(crate) fn instance_duration_value(value: &Value) -> Option<f64> {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Duration" => attributes
            .as_map()
            .get("value")
            .and_then(crate::runtime::to_float_value),
        _ => None,
    }
}

/// Return the raw stored `value` of a Duration instance (a Rational), if any.
pub(crate) fn instance_duration_raw_value(value: &Value) -> Option<Value> {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Duration" => attributes.as_map().get("value").cloned(),
        _ => None,
    }
}

/// Build a Duration instance storing the given (already Rational) value.
pub(crate) fn make_duration_from_value(val: Value) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("value".to_string(), val);
    Value::make_instance(Symbol::intern("Duration"), attrs)
}

pub(crate) fn instance_datetime_parts(
    value: &Value,
) -> Option<(i64, i64, i64, i64, i64, f64, i64)> {
    match value.view() {
        ValueView::Mixin(inner, _) => instance_datetime_parts(inner),
        ValueView::Instance { attributes, .. }
            if attributes.contains_key("year")
                && attributes.contains_key("month")
                && attributes.contains_key("day")
                && attributes.contains_key("hour")
                && attributes.contains_key("minute")
                && attributes.contains_key("second")
                && attributes.contains_key("timezone") =>
        {
            Some(crate::builtins::methods_0arg::temporal::datetime_attrs(
                &(attributes).as_map(),
            ))
        }
        _ => None,
    }
}

/// Build a new DateTime-shaped instance for an arithmetic result while
/// retaining the operand's concrete class and any subclass attributes.
/// DateTime arithmetic is defined in terms of cloning the receiver, so a
/// `DateTime` subclass (for example `Interval`) must remain that subclass.
pub(crate) fn rebuild_datetime_like(
    original: &Value,
    parts: (i64, i64, i64, i64, i64, f64, i64),
) -> Value {
    let (year, month, day, hour, minute, second, timezone) = parts;
    let mixin_state = match original.view() {
        ValueView::Mixin(_, mixins) => Some(mixins.clone()),
        _ => None,
    };
    let source = match original.view() {
        ValueView::Mixin(inner, _) => inner.as_ref(),
        _ => original,
    };
    let (class_name, mut attrs) = match source.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } => (class_name, attributes.to_map()),
        _ => (Symbol::intern("DateTime"), crate::value::AttrMap::new()),
    };
    attrs.insert("year", Value::int(year));
    attrs.insert("month", Value::int(month));
    attrs.insert("day", Value::int(day));
    attrs.insert("hour", Value::int(hour));
    attrs.insert("minute", Value::int(minute));
    attrs.insert("second", Value::num(second));
    attrs.insert("timezone", Value::int(timezone));
    let epoch_days = crate::builtins::methods_0arg::temporal::civil_to_epoch_days(year, month, day);
    let epoch_secs =
        epoch_days as f64 * 86_400.0 + hour as f64 * 3_600.0 + minute as f64 * 60.0 + second
            - timezone as f64;
    attrs.insert("epoch", Value::num(epoch_secs));
    // Rakudo builds the sum from the Instant (`self.new($instant, :timezone)`),
    // which does not pass the formatter along: `~($dt + $dur)` is ISO.
    attrs.remove("formatter");
    let rebuilt = Value::make_instance(class_name, attrs);
    match mixin_state {
        Some(mixins) => Value::mixin_with_state(rebuilt, (*mixins).clone()),
        None => rebuilt,
    }
}

pub(crate) fn make_duration_value(secs: f64) -> Value {
    make_duration(secs)
}

pub(crate) fn make_duration(secs: f64) -> Value {
    make_duration_real(&Value::num(secs))
}

/// Build a Duration from a Real number of seconds, stored the way Rakudo
/// stores it: a nanosecond-truncated Rat ([`tai_rat`]).
// Cost: O(d), see [`tai_rat`].
pub(crate) fn make_duration_real(secs: &Value) -> Value {
    make_duration_from_value(tai_rat(secs))
}

/// The TAI seconds an Instant or Duration stores for the Real `secs`: a Rat
/// truncated toward zero to whole nanoseconds, as Rakudo stores them
/// (`Duration.new(1/3).tai` is `333333333/1000000000`). Inf and NaN keep
/// their degenerate Rats (`1/0`, `-1/0`, `0/0`).
// Cost: O(d), d = digits of the operand's numerator and denominator.
pub(crate) fn tai_rat(secs: &Value) -> Value {
    const NANOS: i64 = 1_000_000_000;
    let nanos = match to_big_rat_parts(secs) {
        Some((n, d)) if !d.is_zero() => (n * NANOS) / d,
        Some(_) => return super::rat::real_to_rat(secs),
        None => {
            let f = crate::runtime::to_float_value(secs).unwrap_or(0.0);
            if !f.is_finite() {
                return super::rat::real_to_rat(&Value::num(f));
            }
            match num_bigint::BigInt::from_f64((f * NANOS as f64).trunc()) {
                Some(n) => n,
                None => return super::rat::real_to_rat(&Value::num(f)),
            }
        }
    };
    crate::value::make_big_rat(nanos, num_bigint::BigInt::from(NANOS))
}

/// The TAI seconds of the POSIX timestamp `posix` (a Real), stored as
/// [`tai_rat`] stores them: `Instant.from-posix(1/3)` keeps the exact
/// fraction (to the nanosecond) instead of going through a Num.
// Cost: O(d + log L), d as for [`tai_rat`], L = leap-second table entries.
pub(crate) fn posix_to_tai(posix: &Value) -> Value {
    let f = crate::runtime::to_float_value(posix).unwrap_or(0.0);
    let leap = crate::value::temporal_core::leap_seconds_at(f);
    tai_rat(&value_add(posix.clone(), Value::int(leap)))
}

/// `a + b` over Real values, exact when both are Rat/Int (as for an Instant
/// or Duration's stored seconds), else in Num — the Real arithmetic of
/// Rakudo's `Instant`/`Duration` operators (`$a.tai + $b`).
// Cost: O(1) for native operands; O(d) for big rationals, d = digits.
pub(crate) fn value_add(a: Value, b: Value) -> Value {
    let (l, r) = crate::runtime::coerce_numeric(a, b);
    if let (Some((an, ad)), Some((bn, bd))) = (to_big_rat_parts(&l), to_big_rat_parts(&r)) {
        return make_big_rat_arith(an * &bd + bn * &ad, ad * bd);
    }
    Value::num(
        crate::runtime::to_float_value(&l).unwrap_or(0.0)
            + crate::runtime::to_float_value(&r).unwrap_or(0.0),
    )
}
