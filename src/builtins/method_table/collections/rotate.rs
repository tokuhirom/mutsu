//! `List.rotate` and `Array.rotate` (ADR-11276 §9.67): not a mutator, `rotate`
//! answers a new `Seq` and leaves the receiver alone, so the row is a plain
//! `Handler::Narrow` and needs no `ReceiverPlace`.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

/// Slurpy from zero: a call with too many arguments gets Rakudo's arity error
/// from the handler instead of "No such method".
pub(super) static ROWS: &[MethodRow] = &[row("List"), row("Array")];

const fn row(owner: &'static str) -> MethodRow {
    MethodRow {
        owner,
        name: "rotate",
        arity: 0,
        handler: Handler::Narrow(rotate),
        flags: RowFlags::SLURPY,
        named: &[],
    }
}

// Cost: see [`rotate_value`].
fn rotate(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(rotate_value(target, args))
}

/// The one implementation of `rotate`, shared by the row and the cascade's arm.
// Cost: O(e), e = elements of the invocant (copied, then placed at their
// rotated index).
pub(crate) fn rotate_value(target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() > 1 {
        return Err(RuntimeError::new(format!(
            "Too many positionals passed; expected 1 or 2 arguments but got {}",
            args.len() + 1
        )));
    }
    // Multi-dim shaped arrays cannot be rotated
    if let ValueView::Array(_, kind) = target.view()
        && kind == crate::value::ArrayKind::Shaped
        && let Some(shape) = crate::runtime::utils::shaped_array_shape(target)
        && shape.len() > 1
    {
        return Err(RuntimeError::illegal_on_fixed_dimension_array("rotate"));
    }
    let items = if let Some(list_items) = target.as_list_items() {
        list_items.to_vec()
    } else {
        match target.view() {
            ValueView::Capture { positional, .. } => positional.clone(),
            ValueView::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
            // Unlike `.reverse`, `.rotate` is NOT defined on `Any` in Rakudo
            // (only on List/Array/Buf), so a non-Iterable invocant is a
            // method-not-found error, not a silent `Nil`.
            _ => {
                return Err(crate::runtime::did_you_mean::method_not_found(
                    "rotate",
                    &crate::value::types::what_type_name(&target),
                ));
            }
        }
    };
    if items.is_empty() {
        return Ok(Value::seq(Vec::new()));
    }
    let len = items.len() as i64;
    let by = match args.first() {
        Some(v) => match v.view() {
            ValueView::Int(i) => i,
            ValueView::Num(n) => n as i64,
            ValueView::Rat(n, d) if d != 0 => n / d,
            ValueView::BigRat(n, d) if d != &num_bigint::BigInt::from(0) => {
                use num_traits::ToPrimitive;
                (n / d).to_i64().unwrap_or(1)
            }
            _ => v.to_string_value().parse::<i64>().unwrap_or(1),
        },
        None => 1,
    };
    let shift = ((by % len) + len) % len;
    let mut out = vec![Value::NIL; items.len()];
    for (i, item) in items.into_iter().enumerate() {
        let dst = ((i as i64 + len - shift) % len) as usize;
        out[dst] = item;
    }
    Ok(Value::seq(out))
}
