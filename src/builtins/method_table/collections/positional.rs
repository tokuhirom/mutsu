//! Plain `List`/`Array` representation conversions.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    // `.Slip`: the elements as a Slip.
    MethodRow {
        owner: "List",
        name: "Slip",
        arity: 0,
        handler: Handler::Narrow(slip),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Array",
        name: "Slip",
        arity: 0,
        handler: Handler::Narrow(slip),
        flags: RowFlags::NONE,
        named: &[],
    },
    // `Array` declares its own `List`.
    MethodRow {
        owner: "Array",
        name: "List",
        arity: 0,
        handler: Handler::Narrow(list_type),
        flags: RowFlags::NONE,
        named: &[],
    },
    // The positional subscript protocol of the list-likes.
    // `List.AT-POS` and `Array.AT-POS` have no row: `builtin_at_pos` answers them
    // with the subscript opcode itself (`@a.AT-POS(-1)` is an `X::OutOfRange`
    // failure, a typed array past its end is its type), before the native
    // cascade this handler restates. They are an interpreter row's to be.
    MethodRow {
        owner: "Range",
        name: "AT-POS",
        arity: 1,
        handler: Handler::Narrow(at_pos),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "EXISTS-POS",
        arity: 1,
        handler: Handler::Narrow(exists_pos),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Range",
        name: "EXISTS-POS",
        arity: 1,
        handler: Handler::Narrow(exists_pos),
        flags: RowFlags::NONE,
        named: &[],
    },
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

/// The index a positional-subscript argument stands for, or the answer every
/// receiver gives for an argument that is no index. A `Str` or `Rat` index is
/// coerced to an `Int`, as Rakudo's `AT-POS(Any)` candidate does
/// (`.AT-POS("1")` reads element 1). A `Str` that is not a number is declined
/// to the general path, which dies on it as Rakudo does; answering `Nil` here
/// made the reply depend on which path the call took (`.AT-POS("a", :zzz)`
/// was `Nil`, #9905). A negative index is `Nil`.
// Cost: O(d), d = chars of a Str index; O(1) otherwise.
pub(crate) fn at_pos_index(arg: &Value) -> Result<usize, Option<Result<Value, RuntimeError>>> {
    match arg.view() {
        ValueView::Int(i) if i >= 0 => Ok(i as usize),
        ValueView::Num(f) if f >= 0.0 => Ok(f as usize),
        ValueView::Rat(n, d) if d > 0 && n >= 0 => Ok((n / d) as usize),
        ValueView::Str(s) => match s.trim().parse::<f64>() {
            Ok(f) if f >= 0.0 => Ok(f as usize),
            Ok(_) => Err(Some(Ok(Value::NIL))),
            Err(_) => Err(None),
        },
        _ => Err(Some(Ok(Value::NIL))),
    }
}

/// Element `idx` of a Range or of a list, or `None` for any other receiver.
// Cost: O(1) for a list or an unbounded range; O(e) for another range
// (materialized), e = elements.
pub(crate) fn at_pos_of(target: &Value, idx: usize) -> Option<Result<Value, RuntimeError>> {
    // A Range is not array-backed; index its (possibly lazy) element
    // sequence directly.
    if crate::builtins::arith::range::range_bounds(target).is_some() {
        return Some(Ok(range_at_pos(target, idx)));
    }
    target
        .as_list_items()
        .map(|items| Ok(items.get(idx).cloned().unwrap_or(Value::NIL)))
}

/// `.AT-POS($pos)` of a list or a Range.
// Cost: see [`at_pos_of`].
pub(crate) fn at_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match at_pos_index(&args[0]) {
        Ok(idx) => at_pos_of(target, idx),
        Err(answer) => answer,
    }
}

/// `.EXISTS-POS($pos)` of a list or a Range, or `None` for a receiver that
/// is neither (`Any.EXISTS-POS` is the cascade's). An argument that is no
/// index, or a negative one, is `False` for every receiver.
// Cost: O(1) for a list; O(e) for a range other than an unbounded one,
// e = elements (counted by materializing).
pub(crate) fn exists_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let idx = match args[0].view() {
        ValueView::Int(i) => i,
        ValueView::Num(f) => f as i64,
        _ => return Some(Ok(Value::FALSE)),
    };
    if idx < 0 {
        return Some(Ok(Value::FALSE));
    }
    // On a Range, an index exists when it is below the element count. A lazy
    // (infinite) range cannot report `.elems`, so -- like raku --
    // `.EXISTS-POS` on it throws X::Cannot::Lazy.
    if crate::builtins::arith::range::range_bounds(target).is_some() {
        return match range_elem_count(target) {
            None => {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str("Cannot .elems a lazy list".to_string()),
                );
                let ex =
                    Value::make_instance(crate::symbol::Symbol::intern("X::Cannot::Lazy"), attrs);
                let mut err = RuntimeError::new("Cannot .elems a lazy list");
                err.exception = Some(Box::new(ex));
                Some(Err(err))
            }
            Some(n) => Some(Ok(Value::truth((idx as usize) < n))),
        };
    }
    // An in-range slot of a mutable array may still be a hole -- a deleted
    // element or an unassigned gap -- and raku reports those as absent. A
    // shaped array is no exception: it is fixed-size, but `my @a[3]` starts
    // with every slot unassigned, so `@a.EXISTS-POS(0)` is False until
    // something is written there.
    if let ValueView::Array(data, ..) = target.view() {
        let i = idx as usize;
        return Some(Ok(Value::truth(i < data.len() && !data.hole_at(i))));
    }
    target
        .as_list_items()
        .map(|items| Ok(Value::truth((idx as usize) < items.len())))
}

/// The 0-based `n`th element of a Range, or Nil when `idx` is past the end.
/// An infinite integer range (`a..*`) is indexed arithmetically; every other
/// range materializes its (finite) element list.
fn range_at_pos(range: &Value, idx: usize) -> Value {
    if crate::value::flat::is_infinite_range(range) {
        // Element `idx` of an unbounded range of any element type: `first +
        // idx` for a numeric start, `idx` `.succ` steps otherwise.
        let Some(first) = crate::runtime::unbounded_range::first(range) else {
            return Value::NIL;
        };
        if let Some(v) = crate::runtime::unbounded_range::nth(&first, idx) {
            return v;
        }
        let mut steps = crate::runtime::unbounded_range::Steps::new(range);
        return steps
            .as_mut()
            .and_then(|s| s.take(idx + 1).pop())
            .unwrap_or(Value::NIL);
    }
    crate::runtime::value_to_list(range)
        .get(idx)
        .cloned()
        .unwrap_or(Value::NIL)
}

/// Number of elements in a Range, or None when the range is infinite.
fn range_elem_count(range: &Value) -> Option<usize> {
    if crate::value::flat::is_infinite_range(range) {
        return None;
    }
    Some(crate::runtime::value_to_list(range).len())
}

/// `.Slip` of a plain positional value: its elements as a Slip. An array
/// hole reads as the container's `is default(...)` value (Rakudo: `.List`
/// keeps holes as `Nil`, `.Slip` uses the default); with no custom default a
/// hole, or a deleted slot's literal `Nil`, surfaces as `Any` in a real
/// array, while an immutable List keeps its `Nil` elements
/// (`(Nil,).Slip` is `slip(Nil,)`).
// Cost: O(e), e = elements.
pub(crate) fn slip(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Array(items, kind) = target.view() else {
        return None;
    };
    let vec: Vec<Value> = if let Some(def) = items.default.as_deref() {
        items
            .iter()
            .enumerate()
            .map(|(i, v)| {
                if items.hole_at(i) {
                    def.clone()
                } else {
                    v.clone()
                }
            })
            .collect()
    } else {
        items
            .iter()
            .map(|v| match v.view() {
                ValueView::Nil if kind.is_real_array() => Value::package(crate::symbol::wk::any()),
                _ => v.clone(),
            })
            .collect()
    };
    Some(Ok(Value::slip_arc(std::sync::Arc::new(vec))))
}
