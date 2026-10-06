//! Plain `List`/`Array` representation conversions.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "List",
        name: "list",
        arity: 0,
        handler: Handler::Narrow(list),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "List",
        arity: 0,
        handler: Handler::Narrow(list_type),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "Array",
        arity: 0,
        handler: Handler::Narrow(array),
        flags: RowFlags::NONE,
        named: &[],
    },
];

fn plain_array(
    target: &Value,
) -> Option<(
    crate::gc::Gc<crate::value::ArrayData>,
    crate::value::ArrayKind,
)> {
    let ValueView::Array(items, kind) = target.view() else {
        return None;
    };
    if crate::runtime::utils::is_shaped_array(target) || kind.is_lazy() {
        return None;
    }
    Some((items.clone(), kind))
}

/// `.list` preserves an already plain positional value and removes itemization
/// from an itemized one.
// Cost: O(1), the backing positional storage is shared.
fn list(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let (items, kind) = plain_array(target)?;
    if kind.is_itemized() {
        Some(Ok(Value::array_with_kind(
            items.clone(),
            kind.decontainerize(),
        )))
    } else {
        Some(Ok(target.clone()))
    }
}

/// `.List` returns a List view, decontainerizing real Array elements.
// Cost: O(e), e = elements copied from a real Array; O(1) for a plain List.
fn list_type(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let (items, kind) = plain_array(target)?;
    if matches!(
        kind,
        crate::value::ArrayKind::List | crate::value::ArrayKind::ItemList
    ) && items.initialized.is_none()
    {
        return Some(Ok(Value::array_with_kind(
            items.clone(),
            crate::value::ArrayKind::List,
        )));
    }
    let values = items
        .iter()
        .enumerate()
        .map(|(index, value)| {
            if items.hole_at(index) {
                Value::NIL
            } else if kind.is_real_array() {
                value.clone().deitemize_element()
            } else {
                value.clone()
            }
        })
        .collect();
    Some(Ok(Value::array(values)))
}

/// `.Array` always creates a fresh real Array for a plain positional value.
// Cost: O(e), e = elements copied and itemized.
fn array(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let (items, _) = plain_array(target)?;
    Some(Ok(crate::runtime::utils::itemize_real_array_elements(
        Value::real_array(items.to_vec()),
    )))
}

pub(crate) fn dispatch(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    match method {
        "list" => list(target, &[]),
        "List" => list_type(target, &[]),
        "Array" => array(target, &[]),
        _ => None,
    }
}
