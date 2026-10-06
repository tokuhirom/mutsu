//! `Date`'s own rows (ADR-11276 §9.18): the neighbouring days and months, the
//! coercions and the renderings.
//!
//! Like the calendar rows (`dateish.rs`), the handlers take the instance's
//! attributes, and the cascade's `date_method_0arg` calls the same functions
//! for the receivers the table has no shape for (an instance of a subclass).

use super::temporal::with_attrs;
use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::temporal;
use crate::symbol::Symbol;
use crate::value::temporal_core::date_attrs;
use crate::value::{AttrMap, RuntimeError, Value};

pub(super) static ROWS: &[MethodRow] = &[
    super::narrow_row!("Date", "succ", 0, succ_row),
    super::narrow_row!("Date", "pred", 0, pred_row),
    super::narrow_row!("Date", "first-date-in-month", 0, first_date_in_month_row),
    super::narrow_row!("Date", "last-date-in-month", 0, last_date_in_month_row),
    super::narrow_row!("Date", "Date", 0, to_date_row),
    super::narrow_row!("Date", "DateTime", 0, to_datetime_row),
    super::narrow_row!("Date", "Int", 0, int_row),
    super::narrow_row!("Date", "Numeric", 0, int_row),
    super::narrow_row!("Date", "Real", 0, int_row),
    super::narrow_row!("Date", "WHICH", 0, which_row),
    super::narrow_row!("Date", "raku", 0, raku_row),
    super::narrow_row!("Date", "Str", 0, str_row),
    super::narrow_row!("Date", "gist", 0, str_row),
];

/// The date `offset` days from the instance's, keeping its formatter.
fn shifted(attributes: &AttrMap, offset: i64) -> Value {
    let (year, month, day) = date_attrs(attributes);
    let days = temporal::civil_to_epoch_days(year, month, day) + offset;
    let (year, month, day) = temporal::epoch_days_to_civil(days);
    temporal::make_date_with_formatter(year, month, day, attributes.get("formatter").cloned())
}

/// `Date.succ`: the next day.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn succ(attributes: &AttrMap) -> Value {
    shifted(attributes, 1)
}

/// `Date.pred`: the previous day.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn pred(attributes: &AttrMap) -> Value {
    shifted(attributes, -1)
}

/// `Date.first-date-in-month`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn first_date_in_month(attributes: &AttrMap) -> Value {
    let (year, month, _) = date_attrs(attributes);
    temporal::make_date_with_formatter(year, month, 1, attributes.get("formatter").cloned())
}

/// `Date.last-date-in-month`.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn last_date_in_month(attributes: &AttrMap) -> Value {
    let (year, month, _) = date_attrs(attributes);
    temporal::make_date_with_formatter(
        year,
        month,
        temporal::days_in_month(year, month),
        attributes.get("formatter").cloned(),
    )
}

/// `Date.Date`: the date itself, formatter included.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn to_date(attributes: &AttrMap) -> Value {
    let (year, month, day) = date_attrs(attributes);
    temporal::make_date_with_formatter(year, month, day, attributes.get("formatter").cloned())
}

/// `Date.DateTime`: midnight UTC of the date.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn to_datetime(attributes: &AttrMap) -> Value {
    let (year, month, day) = date_attrs(attributes);
    temporal::make_datetime(year, month, day, 0, 0, 0.0, 0)
}

/// `Date.Int` (and `Numeric`, `Real`): the day count, the Modified Julian Day
/// number of the date.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn int(attributes: &AttrMap) -> Value {
    let (year, month, day) = date_attrs(attributes);
    Value::int(temporal::daycount(year, month, day))
}

/// `Date.WHICH`: a `ValueObjAt` naming the date by its day count.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn which(attributes: &AttrMap) -> Value {
    let (year, month, day) = date_attrs(attributes);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(
        "WHICH".to_string(),
        Value::str(format!("Date|{}", temporal::daycount(year, month, day))),
    );
    Value::make_instance(Symbol::intern("ValueObjAt"), attrs)
}

/// `Date.raku`: the constructor call that rebuilds the date.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn raku(attributes: &AttrMap) -> Value {
    let (year, month, day) = date_attrs(attributes);
    Value::str(format!("Date.new({year},{month},{day})"))
}

/// `Date.Str` (and `gist`): the ISO 8601 date. A date made with a `:formatter`
/// is rendered by running that Callable, which only the interpreter can do, so
/// it answers `None` and takes the interpreter's path.
// Cost: O(a), a = attributes of the instance.
pub(crate) fn str_of(attributes: &AttrMap) -> Option<Value> {
    if attributes.contains_key("formatter") {
        return None;
    }
    let (year, month, day) = date_attrs(attributes);
    Some(Value::str(temporal::format_date(year, month, day)))
}

macro_rules! attr_row_fns {
    ($($row:ident => $handler:ident),* $(,)?) => {
        $(
            fn $row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
                with_attrs(target, $handler)
            }
        )*
    };
}

attr_row_fns! {
    succ_row => succ,
    pred_row => pred,
    first_date_in_month_row => first_date_in_month,
    last_date_in_month_row => last_date_in_month,
    to_date_row => to_date,
    to_datetime_row => to_datetime,
    int_row => int,
    which_row => which,
    raku_row => raku,
}

fn str_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        crate::value::ValueView::Instance { attributes, .. } => {
            str_of(&attributes.as_map()).map(Ok)
        }
        _ => None,
    }
}
