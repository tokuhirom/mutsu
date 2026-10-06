//! Positional collection rows. List.reverse and Array.reverse share a
//! handler because Rakudo declares the method on both types; counted
//! Any.head and Any.tail use the same handlers in the table and cascade.

use super::{Handler, MethodRow, RowFlags};
use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};
use num_traits::ToPrimitive;

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "List",
        name: "elems",
        arity: 0,
        handler: Handler::Pure(elems),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "end",
        arity: 0,
        handler: Handler::Pure(end),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "reverse",
        arity: 0,
        handler: Handler::Narrow(reverse),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Array",
        name: "reverse",
        arity: 0,
        handler: Handler::Narrow(reverse),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "invert",
        arity: 0,
        handler: Handler::Narrow(invert),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "head",
        arity: 1,
        handler: Handler::Narrow(head),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "tail",
        arity: 1,
        handler: Handler::Narrow(tail),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "Bool",
        arity: 0,
        handler: Handler::Pure(bool),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "join",
        arity: 0,
        handler: Handler::Narrow(join),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "join",
        arity: 1,
        handler: Handler::Narrow(join),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "keys",
        arity: 0,
        handler: Handler::Pure(keys),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "Numeric",
        arity: 0,
        handler: Handler::Pure(elems),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "Int",
        arity: 0,
        handler: Handler::Pure(elems),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "values",
        arity: 0,
        handler: Handler::Pure(values),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "kv",
        arity: 0,
        handler: Handler::Pure(kv),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "pairs",
        arity: 0,
        handler: Handler::Pure(pairs),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "antipairs",
        arity: 0,
        handler: Handler::Pure(antipairs),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "eager",
        arity: 0,
        handler: Handler::Narrow(eager),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "item",
        arity: 0,
        handler: Handler::Narrow(item),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "sink",
        arity: 0,
        handler: Handler::Narrow(sink),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "is-lazy",
        arity: 0,
        handler: Handler::Narrow(is_lazy),
        flags: RowFlags::NONE,
        named: &[],
    },
];

fn len(target: &Value) -> i64 {
    target.as_list_items().map_or(0, |items| items.len() as i64)
}

// Cost: O(1), a length read on the reified items. Also `List.Numeric` and
// `List.Int`: a list numifies to its element count.
fn elems(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::int(len(target)))
}

// Cost: O(1), a length read on the reified items.
fn end(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::int(len(target) - 1))
}

/// `List.keys`: the lazy Seq of the list's indices, a counting iterator over
/// its live length (Rakudo's `Seq.new(Rakudo::Iterator.CountOnly...)`).
// Cost: O(1); O(1) per key pulled.
pub(crate) fn keys(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::seq_list_gen(
        crate::value::ListGen::positional(
            target.clone(),
            crate::value::PositionalMode::Keys,
            false,
        ),
        false,
    ))
}

// Cost: O(1); O(1) per value pulled.
pub(crate) fn values(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::seq_list_gen(
        crate::value::ListGen::positional(
            target.clone(),
            crate::value::PositionalMode::Values,
            false,
        ),
        false,
    ))
}

// Cost: O(1); O(1) per key/value pulled.
pub(crate) fn kv(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::seq_list_gen(
        crate::value::ListGen::positional(target.clone(), crate::value::PositionalMode::Kv, false),
        false,
    ))
}

// Cost: O(1); O(1) per pair pulled.
pub(crate) fn pairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::seq_list_gen(
        crate::value::ListGen::positional(
            target.clone(),
            crate::value::PositionalMode::Pairs,
            false,
        ),
        false,
    ))
}

// Cost: O(1); O(1) per antipair pulled.
pub(crate) fn antipairs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::seq_list_gen(
        crate::value::ListGen::positional(
            target.clone(),
            crate::value::PositionalMode::Antipairs,
            false,
        ),
        false,
    ))
}

// Cost: O(1), a plain positional value is already eager and is returned as-is.
pub(crate) fn eager(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    match target.view() {
        ValueView::Array(_, kind) if !kind.is_lazy() => Some(Ok(target.clone())),
        _ => None,
    }
}

// Cost: O(1), itemization flips the shared positional representation tag.
pub(crate) fn item(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    match target.view() {
        ValueView::Array(_, kind) if !kind.is_lazy() => Some(Ok(target.clone().item())),
        _ => None,
    }
}

// Cost: O(1), an eager positional value has nothing to pull when sunk.
pub(crate) fn sink(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    match target.view() {
        ValueView::Array(_, kind) if !kind.is_lazy() => Some(Ok(Value::NIL)),
        _ => None,
    }
}

// Cost: O(1), a plain List/Array is not lazy.
pub(crate) fn is_lazy(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    match target.view() {
        ValueView::Array(_, kind) if !kind.is_lazy() => Some(Ok(Value::FALSE)),
        _ => None,
    }
}

// Cost: O(1), an emptiness test.
fn bool(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(target.truthy()))
}

/// The counted `Any.head` implementation shared by its row and the native
/// cascade. It decomposes the receiver's own elements, independent of the
/// itemization it carries as an element of another container (ADR-0040
/// slices 1-2).
// Cost: O(k) on Array/List, k = selected elements; O(e) otherwise,
// e = receiver elements materialized before selecting a window.
pub(crate) fn head(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let [arg] = args else {
        return None;
    };
    let n: i64 = match arg.view() {
        ValueView::Int(i) => i,
        ValueView::Rat(num, den) => {
            if den == 0 {
                0
            } else {
                num / den
            }
        }
        ValueView::Num(f) => f as i64,
        ValueView::BigInt(bi) => {
            // For very large BigInts that don't fit in i64:
            // negative => treat as negative (returns empty), positive => clamp to MAX
            bi.to_i64()
                .unwrap_or(if bi.sign() == num_bigint::Sign::Minus {
                    -1
                } else {
                    i64::MAX
                })
        }
        _ => return None,
    };
    if n <= 0 {
        return Some(Ok(Value::seq(vec![])));
    }
    let n = n as usize;
    match target.view() {
        ValueView::Array(items, kind) => {
            let count = n.min(items.len());
            let values = if kind.is_immutable_list() {
                items[..count].to_vec()
            } else {
                (0..count)
                    .map(|i| {
                        target
                            .array_slot_ref(i, true)
                            .unwrap_or_else(|| items[i].clone())
                    })
                    .collect()
            };
            Some(Ok(Value::seq(values)))
        }
        ValueView::Range(a, b) => {
            let items: Vec<Value> = (a..=b).take(n).map(Value::int).collect();
            Some(Ok(Value::seq(items)))
        }
        // An unbounded range of any element type: step `n` times.
        _ if let Some(mut steps) = crate::runtime::unbounded_range::Steps::new(target) => {
            Some(Ok(Value::seq(steps.take(n))))
        }
        _ => Some(Ok(Value::seq(runtime::with_receiver_items(
            target,
            |items| items[..n.min(items.len())].to_vec(),
        )))),
    }
}

/// The counted `Any.tail` implementation shared by its row and the native
/// cascade, preserving mutable Array element cells.
// Cost: O(k) on Array/List, k = selected elements; O(e) otherwise,
// e = receiver elements materialized before selecting a window.
pub(crate) fn tail(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let [arg] = args else {
        return None;
    };
    if matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply") {
        return None;
    }
    let n = match arg.view() {
        ValueView::Int(i) if i > 0 => usize::try_from(i).unwrap_or(usize::MAX),
        ValueView::Int(_) => return Some(Ok(Value::seq(Vec::new()))),
        _ => return None,
    };
    match target.view() {
        ValueView::Array(items, kind) => {
            let start = items.len().saturating_sub(n);
            let values = if kind.is_immutable_list() {
                items[start..].to_vec()
            } else {
                (start..items.len())
                    .map(|i| {
                        target
                            .array_slot_ref(i, true)
                            .unwrap_or_else(|| items[i].clone())
                    })
                    .collect()
            };
            Some(Ok(Value::seq(values)))
        }
        _ => Some(Ok(Value::seq(runtime::with_receiver_items(
            target,
            |items| items[items.len().saturating_sub(n)..].to_vec(),
        )))),
    }
}

/// The one implementation used by the List row and the native cascade. The
/// cascade still reaches it for receivers without a List or Array dispatch
/// shape.
// Cost: O(e), e = elements copied or reached while reversing the receiver.
pub(crate) fn reverse(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Array(items, kind) => {
            // Multi-dimensional shaped arrays cannot be reversed.
            if kind == crate::value::ArrayKind::Shaped
                && let Some(shape) = crate::runtime::utils::shaped_array_shape(target)
                && shape.len() > 1
            {
                return Some(Err(RuntimeError::illegal_on_fixed_dimension_array(
                    "reverse",
                )));
            }
            let mut reversed = (**items).clone();
            reversed.reverse();
            // .reverse returns a Seq in Raku, not an Array.
            Some(Ok(Value::seq(reversed.into_items())))
        }
        ValueView::Range(a, b)
        | ValueView::RangeExcl(a, b)
        | ValueView::RangeExclStart(a, b)
        | ValueView::RangeExclBoth(a, b) => {
            if b == i64::MAX || a == i64::MIN {
                Some(Err(RuntimeError::cannot_lazy("reverse")))
            } else {
                let mut reversed = crate::runtime::utils::value_to_list(target);
                reversed.reverse();
                Some(Ok(Value::seq(reversed)))
            }
        }
        ValueView::GenericRange { end, .. } => {
            // For example, 1 .. -Inf is empty and reverses to an empty list.
            let end_is_neg_inf = matches!(
                end.as_ref().view(),
                ValueView::Num(n) if n.is_infinite() && n.is_sign_negative()
            );
            if end_is_neg_inf {
                return Some(Ok(Value::seq(Vec::new())));
            }
            if crate::builtins::methods_0arg::is_infinite_range(target) {
                return Some(Err(RuntimeError::cannot_lazy("reverse")));
            }
            let mut items = crate::runtime::utils::value_to_list(target);
            if items.len() == 1
                && matches!(
                    items.first().map(Value::view),
                    Some(ValueView::GenericRange { .. })
                )
            {
                None
            } else {
                items.reverse();
                Some(Ok(Value::seq(items)))
            }
        }
        // Any.reverse is self.list.reverse, so a non-Iterable is a one-element
        // list. String character reversal is .flip.
        ValueView::Str(_)
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Bool(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..)
        | ValueView::Pair(..)
        | ValueView::ValuePair(..) => Some(Ok(Value::seq(vec![target.clone()]))),
        ValueView::Seq(items) => {
            let mut reversed = items.to_vec();
            reversed.reverse();
            Some(Ok(Value::seq(reversed)))
        }
        ValueView::Slip(items) => {
            let mut reversed = items.to_vec();
            reversed.reverse();
            Some(Ok(Value::seq(reversed)))
        }
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
            let mut bytes = crate::value::value_buf::buf_elems_or_empty(&attributes);
            bytes.reverse();
            Some(Ok(crate::value::value_buf::make_buf(class_name, bytes)))
        }
        _ => None,
    }
}

/// The List and Array `.invert` implementation shared by the method row and
/// the native cascade. Other collection kinds keep their specialized paths.
// Cost: O(e + v), e = input elements, v = expanded values in Pair payloads.
pub(crate) fn invert(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    crate::builtins::methods_0arg::collection::invert_value(target).map(Ok)
}

/// `List.join($sep = "")` on a plain list or array, or `None` when an element
/// needs more than the pure stringification (see [`join_items`]), is a
/// `Proxy`, or is undefined: Rakudo warns for each undefined element, which
/// only the interpreter path can do (#11838).
// Cost: O(e + t), e = elements (walked once more for a Proxy, one container
// level per step), t = total chars of the result.
fn join(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    // `.join` stringifies every element, so a zero-denominator Rational among
    // them dies like its own `.Str` (GH #9621).
    if let Err(err) = crate::runtime::utils::check_str_coercion_zero_denominator(target) {
        return Some(Err(err));
    }
    // A `Proxy` element renders as its FETCHed value, which only the
    // interpreter can run (ADR-0040 §9.2); the full path resolves it first.
    if crate::runtime::Interpreter::value_has_proxy(target) {
        return None;
    }
    let sep = args.first().map(Value::to_string_value).unwrap_or_default();
    let items = join_source_items(target)?;
    if items.iter().any(|v| {
        v.with_deref(|inner| {
            matches!(
                inner.descalarize().view(),
                ValueView::Nil | ValueView::Package(_)
            )
        })
    }) {
        return None;
    }
    join_items(&items, &sep).map(Ok)
}

/// The elements `.join` reads from a list-like receiver: a hole in an array
/// with an `is default(...)` value reads as that value, not the `Any` marker
/// the slot holds. `None` for a receiver with no reified items.
// Cost: O(1) when the receiver has no default; O(e) otherwise, e = elements.
pub(crate) fn join_source_items(target: &Value) -> Option<std::borrow::Cow<'_, [Value]>> {
    if let ValueView::Array(data, _) = target.view()
        && let std::borrow::Cow::Owned(resolved) = data.items_with_default()
    {
        return Some(std::borrow::Cow::Owned(resolved));
    }
    target.as_list_items().map(std::borrow::Cow::Borrowed)
}

/// Join already-reified elements with `sep`, or `None` when an element needs
/// the interpreter to stringify it: an instance or mixin (a user `Str` may
/// apply, also when nested in an inner list), a Junction (the whole `join`
/// threads over its eigenstates), or a deferred Seq whose callback has not
/// run. The one implementation the `List.join` row and the cascade arms share.
// Cost: O(e + t), e = elements, t = total chars of the result.
pub(crate) fn join_items(items: &[Value], sep: &str) -> Option<Value> {
    if items.iter().any(|v| {
        v.with_deref(|inner| {
            let inner = inner.descalarize();
            matches!(
                inner.view(),
                ValueView::Instance { .. }
                    | ValueView::Mixin(..)
                    | ValueView::Junction { .. }
                    | ValueView::LazyList(_)
                    | ValueView::LazyThunk(_)
            ) || matches!(inner.view(), ValueView::Seq(s) if s.awaits_vm_reify())
        }) || crate::value::gist::str_needs_dispatch(v)
    }) {
        return None;
    }
    Some(Value::str(
        items
            .iter()
            .map(Value::to_str_context)
            .collect::<Vec<_>>()
            .join(sep),
    ))
}
