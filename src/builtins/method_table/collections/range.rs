//! `Range`'s rows (ADR-11276 §10, slice 3A proof rows for the `Range` shape;
//! slice 3C moves the rest of its methods).

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Range",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("excludes-min", excludes_min),
    row!("excludes-max", excludes_max),
];

/// `Range.excludes-min`: whether the lower endpoint is excluded (`^..`).
// Cost: O(1).
pub(crate) fn excludes_min(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Range(..) | ValueView::RangeExcl(..) => Some(Ok(Value::FALSE)),
        ValueView::RangeExclStart(..) | ValueView::RangeExclBoth(..) => Some(Ok(Value::TRUE)),
        ValueView::GenericRange { excl_start, .. } => Some(Ok(Value::truth(excl_start))),
        _ => None,
    }
}

/// `Range.excludes-max`: whether the upper endpoint is excluded (`..^`).
// Cost: O(1).
pub(crate) fn excludes_max(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Range(..) | ValueView::RangeExclStart(..) => Some(Ok(Value::FALSE)),
        ValueView::RangeExcl(..) | ValueView::RangeExclBoth(..) => Some(Ok(Value::TRUE)),
        ValueView::GenericRange { excl_end, .. } => Some(Ok(Value::truth(excl_end))),
        _ => None,
    }
}
