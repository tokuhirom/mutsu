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

/// A copy of the receiver's elements; `None` for a receiver that is no
/// instance.
// Cost: O(n), n = elements (copied).
fn items(target: &Value) -> Option<Vec<Value>> {
    match target.view() {
        ValueView::Instance { attributes, .. } => {
            Some(list_items(attributes.as_map().get(ITERATION_BUFFER_ITEMS)))
        }
        _ => None,
    }
}

/// Runs `f` over the receiver's element storage in place -- the same store,
/// and the same editor, as the `nqp::` list ops (`nqp::push`, `nqp::bindpos`,
/// ...) use on a buffer, so a `List` taken from the buffer earlier sees the
/// edit, as in Rakudo, where that `List`'s `$!reified` is the buffer itself.
/// `None` for a receiver that is no instance.
// Cost: O(1) plus f.
fn edit<R>(
    target: &Value,
    f: impl FnOnce(&mut Vec<Value>) -> R,
) -> Option<Result<R, RuntimeError>> {
    if !matches!(target.view(), ValueView::Instance { .. }) {
        return None;
    }
    Some(Interpreter::nqp_with_elems_mut("push", target, f))
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
// Cost: O(1).
fn elems(target: &Value, _args: &[Value]) -> Answer {
    if !matches!(target.view(), ValueView::Instance { .. }) {
        return None;
    }
    let len = Interpreter::nqp_elems_len_of(target).unwrap_or(0);
    Some(Ok(Value::int(len as i64)))
}

/// `IterationBuffer.AT-POS($i)`: the element, `Mu` past the end. A negative
/// index counts from the end, as for every positional op on the VM array
/// (`nqp::atpos`).
// Cost: O(1).
fn at_pos(target: &Value, args: &[Value]) -> Answer {
    if !matches!(target.view(), ValueView::Instance { .. }) {
        return None;
    }
    let len = Interpreter::nqp_elems_len_of(target).unwrap_or(0);
    let index =
        match crate::runtime::nqp_backing::resolve_index(crate::runtime::to_int(&args[0]), len) {
            Ok(index) => index,
            Err(e) => return Some(Err(e)),
        };
    Some(Ok(
        Interpreter::nqp_elem_at(target, index).unwrap_or_else(mu)
    ))
}

/// `IterationBuffer.BIND-POS($i, $value)`: binds the element, growing the
/// buffer with `Mu` up to it; a negative index counts from the end
/// (`nqp::bindpos`).
// Cost: O(1) amortized; O(i - n) when growing, i = index, n = elements.
fn bind_pos(target: &Value, args: &[Value]) -> Answer {
    if !matches!(target.view(), ValueView::Instance { .. }) {
        return None;
    }
    Some(crate::runtime::nqp_backing::bind_elem(
        "BIND-POS",
        target,
        crate::runtime::to_int(&args[0]),
        args[1].clone(),
        mu(),
    ))
}

/// `IterationBuffer.push(*@values)`: answers the last value pushed.
// Cost: O(a) amortized, a = arguments.
fn push(target: &Value, args: &[Value]) -> Answer {
    let pushed = edit(target, |items| items.extend(args.iter().cloned()))?;
    Some(pushed.map(|()| args.last().cloned().unwrap_or(Value::NIL)))
}

/// `IterationBuffer.unshift(*@values)`: answers the last value given.
// Cost: O(n + a), n = elements, a = arguments. MoarVM: O(a) amortized -- see #17658.
fn unshift(target: &Value, args: &[Value]) -> Answer {
    let unshifted = edit(target, |items| {
        items.splice(0..0, args.iter().cloned());
    })?;
    Some(unshifted.map(|()| args.last().cloned().unwrap_or(Value::NIL)))
}

/// `IterationBuffer.append(@values)`: answers the buffer.
// Cost: O(a) amortized, a = elements appended.
fn append(target: &Value, args: &[Value]) -> Answer {
    let appended = argument_items(&args[0]);
    let done = edit(target, |items| items.extend(appended))?;
    Some(done.map(|()| target.clone()))
}

/// `IterationBuffer.prepend(@values)`: answers the buffer.
// Cost: O(n + a), n = elements, a = elements prepended. MoarVM: O(a) amortized -- see #17658.
fn prepend(target: &Value, args: &[Value]) -> Answer {
    let prepended = argument_items(&args[0]);
    let done = edit(target, |items| {
        items.splice(0..0, prepended);
    })?;
    Some(done.map(|()| target.clone()))
}

/// `IterationBuffer.clear`.
// Cost: O(n), n = elements (dropped).
fn clear(target: &Value, _args: &[Value]) -> Answer {
    Some(edit(target, Vec::clear)?.map(|()| Value::NIL))
}

/// `IterationBuffer.List`.
// Cost: O(1) (shares the buffer's storage).
fn list(target: &Value, _args: &[Value]) -> Answer {
    // The `List` views the buffer's own storage (Rakudo's `List` is built over
    // the `IterationBuffer` as its `$!reified`), so an in-place `nqp::push` /
    // `nqp::shift` on the buffer shows through a `List` taken earlier -- the
    // sliding window of `Sequence::Generator`'s `LambdaN`.
    if let ValueView::Instance { attributes, .. } = target.view()
        && let Some(ValueView::Array(data, _)) = attributes
            .as_map()
            .get(ITERATION_BUFFER_ITEMS)
            .map(Value::view)
    {
        return Some(Ok(Value::array_with_kind(
            data.clone(),
            crate::value::ArrayKind::List,
        )));
    }
    items(target).map(|items| Ok(Value::array(items)))
}

/// `IterationBuffer.Slip`.
// Cost: O(n), n = elements (copied). Rakudo: O(1) (the Slip shares the buffer) -- see #17658.
fn slip(target: &Value, _args: &[Value]) -> Answer {
    items(target).map(|items| Ok(Value::slip(items)))
}

/// `IterationBuffer.Seq`.
// Cost: O(n), n = elements (copied). Rakudo: O(1) (the Seq iterates the buffer) -- see #17658.
fn seq(target: &Value, _args: &[Value]) -> Answer {
    items(target).map(|items| Ok(Value::seq(items)))
}

/// `IterationBuffer.iterator`: an iterator over the elements.
// Cost: O(n), n = elements.
fn iterator(i: &mut Interpreter, target: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    let items = items(target)?;
    Some(i.call_method_with_values(Value::array(items), "iterator", vec![]))
}
