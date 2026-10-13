//! `.Range` of the numeric type objects (ADR-11276 §9.97): the values the type
//! can hold. Rakudo declares it on `Int` (`-Inf^..^Inf`, and a native integer
//! type's bounds: `int8.Range` is `-128..127`), on `Num` and on the `Rational`
//! role (`-Inf..Inf`), which `Rat` and `FatRat` compose. It is a method of the
//! type object only: an instance's `.Range` throws
//! `X::Parameter::InvalidConcreteness`.
//!
//! A native integer type object (`int8`, `uint`) and the `Rational` role have no
//! shape; they reach these rows through the type-object walk
//! (`method_table::collections::type_params::type_object_row`).

use super::{Handler, MethodRow, RowFlags};
use num_traits::ToPrimitive;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: "Range",
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::TYPE_OBJECT_OK,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Int", int_range),
    row!("Num", num_range),
    row!("Rat", rat_range),
    row!("FatRat", fat_rat_range),
    // No shape is the `Rational` role: its type object reaches the row through
    // its owner (`type_object_row`), as `Callable.of` does.
    MethodRow {
        owner: "Rational",
        name: "Range",
        arity: 0,
        handler: Handler::Narrow(rational_range),
        flags: RowFlags::TYPE_OBJECT_OK.or(RowFlags::OWNER_ONLY),
        named: &[],
    },
];

/// `-Inf..Inf`, or `-Inf^..^Inf` with both ends excluded.
// Cost: O(1).
fn infinite_range(excluded: bool) -> Value {
    Value::generic_range(
        Value::num(f64::NEG_INFINITY),
        Value::num(f64::INFINITY),
        excluded,
        excluded,
    )
}

/// The `X::Parameter::InvalidConcreteness` an instance's `.Range` throws.
// Cost: O(1).
fn instance_error(owner: &str, target: &Value) -> Option<Result<Value, RuntimeError>> {
    Some(Err(RuntimeError::parameter_invalid_concreteness(
        owner,
        crate::value::type_name::value_type_name(target),
        "Range",
        "self",
        false,
        true,
    )))
}

/// The bounds of the native integer type `name` (`int8` is `-128..127`), or
/// `None` for any other type.
// Cost: O(1).
fn native_int_range(name: &str) -> Option<Value> {
    if !crate::runtime::native_types::is_native_int_type(name) {
        return None;
    }
    let (min_big, max_big) = crate::runtime::native_types::native_int_bounds(name)?;
    let (min_i64, max_i64) = (min_big.to_i64(), max_big.to_i64());
    // `Value::range` reads `i64::MIN` / `i64::MAX` as the -Inf / Inf
    // sentinels, so the full 64-bit range needs the generic form.
    if let (Some(min), Some(max)) = (min_i64, max_i64)
        && min != i64::MIN
        && max != i64::MAX
    {
        return Some(Value::range(min, max));
    }
    let min = min_i64.map(Value::int).unwrap_or_else(|| Value::bigint(min_big));
    let max = max_i64.map(Value::int).unwrap_or_else(|| Value::bigint(max_big));
    Some(Value::generic_range(min, max, false, false))
}

/// `Int.Range`: a native integer type's bounds, `-Inf^..^Inf` for `Int` and
/// its other subtypes.
// Cost: O(1).
fn int_range(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Package(name) = target.view() else {
        return instance_error("Int", target);
    };
    Some(Ok(
        native_int_range(name.as_str()).unwrap_or_else(|| infinite_range(true))
    ))
}

/// The `Range` rows whose answer is `-Inf..Inf`, each naming its own type in
/// an instance's error.
macro_rules! real_range {
    ($fn:ident, $owner:literal) => {
        /// `.Range` of a real type with no finite bound: `-Inf..Inf`.
        // Cost: O(1).
        fn $fn(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
            match target.view() {
                ValueView::Package(_) => Some(Ok(infinite_range(false))),
                _ => instance_error($owner, target),
            }
        }
    };
}

real_range!(num_range, "Num");
real_range!(rat_range, "Rat");
real_range!(fat_rat_range, "FatRat");
real_range!(rational_range, "Rational");
