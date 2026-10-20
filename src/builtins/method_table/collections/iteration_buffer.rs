//! `IterationBuffer`'s rows (ADR-11276 §9.121): `elems`, `AT-POS`, `BIND-POS`,
//! `push`, `unshift`, `append`, `prepend`, `clear`, `List`, `Slip`, `Seq` and
//! `iterator`, which Rakudo declares on `IterationBuffer`.
//!
//! A buffer is an instance keeping its elements in one attribute
//! ([`ITERATION_BUFFER_ITEMS`]). The class has no dispatch shape; the rows are
//! reached through their owner (`RowFlags::OWNER_ONLY`, `invoke_owner`) from the
//! instance dispatch, where the arms that answered them by name were, for a
//! buffer or an instance of a subclass whose class does not declare the method
//! itself (ValueList's throwing `BIND-POS`/`push`/... override them).

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::nqp_ops_list::ITERATION_BUFFER_ITEMS;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// A row of `IterationBuffer` taking `$arity` positional arguments (and, with
/// `slurpy`, any number more).
macro_rules! row {
    ($name:literal, $arity:literal, $handler:expr) => {
        row!($name, $arity, $handler, RowFlags::OWNER_ONLY)
    };
    ($name:literal, $arity:literal, $handler:expr, slurpy) => {
        row!(
            $name,
            $arity,
            $handler,
            RowFlags::OWNER_ONLY.or(RowFlags::SLURPY)
        )
    };
    ($name:literal, $arity:literal, $handler:expr, $flags:expr) => {
        MethodRow {
            owner: "IterationBuffer",
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("elems", 0, Handler::Narrow(elems)),
    row!("AT-POS", 1, Handler::Narrow(at_pos)),
    row!("BIND-POS", 2, Handler::Narrow(bind_pos)),
    row!("push", 1, Handler::Narrow(push), slurpy),
    row!("unshift", 1, Handler::Narrow(unshift), slurpy),
    row!("append", 1, Handler::Narrow(append)),
    row!("prepend", 1, Handler::Narrow(prepend)),
    row!("clear", 0, Handler::Narrow(clear)),
    row!("List", 0, Handler::Narrow(list)),
    row!("Slip", 0, Handler::Narrow(slip)),
    row!("Seq", 0, Handler::Narrow(seq)),
    row!("iterator", 0, Handler::Interp(iterator)),
];

/// The elements of the list a buffer holds.
// Cost: O(n), n = elements (copied).
fn list_items(list: Option<&Value>) -> Vec<Value> {
    match list.map(Value::view) {
        Some(ValueView::Array(values, ..)) => values.to_vec(),
        Some(ValueView::Seq(values)) => values.to_vec(),
        Some(ValueView::Slip(values)) => values.to_vec(),
        _ => Vec::new(),
    }
}

/// The receiver's elements; `None` for a receiver that is no instance.
// Cost: O(n), n = elements (copied).
fn items(target: &Value) -> Option<Vec<Value>> {
    match target.view() {
        ValueView::Instance { attributes, .. } => {
            Some(list_items(attributes.as_map().get(ITERATION_BUFFER_ITEMS)))
        }
        _ => None,
    }
}

/// Stores `new_items` as the receiver's elements, in its shared attribute
/// cell, and answers the receiver.
// Cost: O(a), a = the instance's attributes.
fn store(target: &Value, new_items: Vec<Value>) -> Value {
    let ValueView::Instance {
        class_name,
        attributes,
        id,
    } = target.view()
    else {
        return target.clone();
    };
    let mut updated = attributes.to_map();
    updated.insert(
        ITERATION_BUFFER_ITEMS.to_string(),
        Value::real_array(new_items),
    );
    Value::write_back_sharing(&attributes, class_name, updated, id)
}

/// The elements an `append`/`prepend` argument contributes: another buffer's
/// elements, or the argument as a list.
// Cost: O(n), n = elements.
fn argument_items(value: &Value) -> Vec<Value> {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "IterationBuffer" => {
            list_items(attributes.as_map().get(ITERATION_BUFFER_ITEMS))
        }
        _ => Interpreter::value_to_list(value),
    }
}

fn mu() -> Value {
    Value::package(Symbol::intern("Mu"))
}

/// `IterationBuffer.elems`.
// Cost: O(n), n = elements (the elements are copied out).
fn elems(target: &Value, _args: &[Value]) -> Answer {
    items(target).map(|items| Ok(Value::int(items.len() as i64)))
}

/// `IterationBuffer.AT-POS($i)`: the element, `Mu` past the end.
// Cost: O(n), n = elements (copied).
fn at_pos(target: &Value, args: &[Value]) -> Answer {
    let items = items(target)?;
    let index = crate::runtime::to_int(&args[0]);
    Some(Ok(usize::try_from(index)
        .ok()
        .and_then(|index| items.get(index).cloned())
        .unwrap_or_else(mu)))
}

/// `IterationBuffer.BIND-POS($i, $value)`: binds the element, growing the
/// buffer with `Mu` up to it.
// Cost: O(n), n = elements.
fn bind_pos(target: &Value, args: &[Value]) -> Answer {
    let mut items = items(target)?;
    if let Ok(index) = usize::try_from(crate::runtime::to_int(&args[0])) {
        if index >= items.len() {
            items.resize(index + 1, mu());
        }
        items[index] = args[1].clone();
    }
    store(target, items);
    Some(Ok(args[1].clone()))
}

/// `IterationBuffer.push(*@values)`: answers the last value pushed.
// Cost: O(n + a), n = elements, a = arguments.
fn push(target: &Value, args: &[Value]) -> Answer {
    let mut items = items(target)?;
    items.extend(args.iter().cloned());
    store(target, items);
    Some(Ok(args.last().cloned().unwrap_or(Value::NIL)))
}

/// `IterationBuffer.unshift(*@values)`: answers the last value given.
// Cost: O(n + a), n = elements, a = arguments.
fn unshift(target: &Value, args: &[Value]) -> Answer {
    let items = items(target)?;
    let mut prepended = args.to_vec();
    prepended.extend(items);
    store(target, prepended);
    Some(Ok(args.last().cloned().unwrap_or(Value::NIL)))
}

/// `IterationBuffer.append(@values)`: answers the buffer.
// Cost: O(n + a), n = elements, a = elements appended.
fn append(target: &Value, args: &[Value]) -> Answer {
    let mut items = items(target)?;
    items.extend(argument_items(&args[0]));
    Some(Ok(store(target, items)))
}

/// `IterationBuffer.prepend(@values)`: answers the buffer.
// Cost: O(n + a), n = elements, a = elements prepended.
fn prepend(target: &Value, args: &[Value]) -> Answer {
    let items = items(target)?;
    let mut prepended = argument_items(&args[0]);
    prepended.extend(items);
    Some(Ok(store(target, prepended)))
}

/// `IterationBuffer.clear`.
// Cost: O(a), a = the instance's attributes.
fn clear(target: &Value, _args: &[Value]) -> Answer {
    items(target)?;
    store(target, Vec::new());
    Some(Ok(Value::NIL))
}

/// `IterationBuffer.List`.
// Cost: O(n), n = elements.
fn list(target: &Value, _args: &[Value]) -> Answer {
    items(target).map(|items| Ok(Value::array(items)))
}

/// `IterationBuffer.Slip`.
// Cost: O(n), n = elements.
fn slip(target: &Value, _args: &[Value]) -> Answer {
    items(target).map(|items| Ok(Value::slip(items)))
}

/// `IterationBuffer.Seq`.
// Cost: O(n), n = elements.
fn seq(target: &Value, _args: &[Value]) -> Answer {
    items(target).map(|items| Ok(Value::seq(items)))
}

/// `IterationBuffer.iterator`: an iterator over the elements.
// Cost: O(n), n = elements.
fn iterator(i: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    let items = items(target)?;
    Some(i.call_method_with_values(Value::array(items), "iterator", vec![]))
}
