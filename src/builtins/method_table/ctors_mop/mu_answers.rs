//! `Mu.so`, `Mu.not`, `Mu.is-lazy` (ADR-11276 slice 5, §9.81), `Mu.item` and
//! `Mu.sink` (§9.82): the answers every receiver reaches by name.
//!
//! Rakudo declares them on `Mu`, so a type object, a `Junction`, a code
//! object and a closed shape answer them as well as a value: `Int.so` is
//! `False`, `(sub {}).is-lazy` is `False`. A shape whose own type declares one
//! (`Match.not`, `List.is-lazy`, `Seq.is-lazy`, `Range.is-lazy`) answers its own
//! row first. A receiver with no shape reaches them through
//! [`crate::builtins::method_table::answer_shapeless`].
//!
//! These are the arity-0 rows a call dispatches to; the arity-1 `so`/`not`
//! rows of `mu_base` are the last candidate of a user override's `callsame`
//! chain and are reached only by their owner.

use crate::builtins::method_table::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:expr) => {
        MethodRow {
            owner: "Mu",
            name: $name,
            arity: 0,
            handler: $handler,
            flags: RowFlags::TYPE_OBJECT_OK,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("so", Handler::Pure(so)),
    row!("not", Handler::Pure(not)),
    row!("is-lazy", Handler::Narrow(is_lazy)),
    row!(
        "item",
        Handler::Narrow(crate::builtins::method_table::lazy::item)
    ),
    row!("sink", Handler::Narrow(sink)),
];

/// Asking a `Failure` for its truth handles it (Rakudo's `Failure.Bool` sets
/// `handled`), so it no longer throws when it is sunk or destroyed.
// Cost: O(1).
fn handle_failure(target: &Value) {
    if let ValueView::Instance { class_name, .. } = target.view()
        && class_name == "Failure"
    {
        target.mark_failure_handled();
    }
}

/// `Mu.so`: the receiver's truth, by the one truthiness rule
/// (`Value::truthy`). A user `Bool` override is called before the native
/// dispatch reaches the table.
// Cost: O(1).
pub(crate) fn so(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    handle_failure(target);
    Ok(Value::truth(target.truthy()))
}

/// `Mu.not`: the negation of [`so`].
// Cost: O(1).
pub(crate) fn not(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    handle_failure(target);
    Ok(Value::truth(!target.truthy()))
}

/// `Mu.is-lazy`: whether the receiver produces its values on demand without
/// a known end. An `Iterator` instance answers its stored flag; a consumed
/// `gather` list throws `X::Seq::Consumed`; any other `LazyList` asks
/// `is_genuinely_lazy`, the one authority for it (a plain `gather` or a finite
/// `.map` pipe is not lazy); everything else is lazy only when it is a lazy
/// array, an infinite range or a lazy `Seq`.
// Cost: O(1), the invocant's own lazy flag or range end.
pub(crate) fn is_lazy(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Iterator" => {
            let lazy = matches!(
                attributes.as_map().get("is_lazy").map(Value::view),
                Some(ValueView::Bool(true))
            );
            Some(Ok(Value::truth(lazy)))
        }
        ValueView::LazyList(list) => {
            let is_gather = list.env.get("__mutsu_lazylist_from_gather").is_some();
            if is_gather && crate::value::lazylist_is_consumed(&list) {
                return Some(Err(crate::value::seq_consumed_error()));
            }
            Some(Ok(Value::truth(list.is_genuinely_lazy())))
        }
        _ => Some(Ok(Value::truth(
            crate::builtins::methods_0arg::is_value_lazy(target),
        ))),
    }
}

/// `Mu.sink`: evaluate the receiver for its effects and answer `Nil`. A plain
/// array has nothing to run; an unhandled `Failure` throws its exception (a
/// handled one is a no-op); a `Seq` whose source is already settled marks itself
/// consumed. A `Seq` with a deferred source or a `LazyList` declines: the
/// interpreter pulls it, since its producer may run compiled code.
// Cost: O(1), except a reified `Seq`'s O(e) consumption mark, e = elements.
pub(crate) fn sink(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let Some(answer) = crate::builtins::method_table::list::sink(target, args) {
        return Some(answer);
    }
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Failure" => {
            if target.is_failure_handled() {
                return Some(Ok(Value::NIL));
            }
            match attributes.as_map().get("exception") {
                Some(ex) => {
                    let mut err = RuntimeError::new(ex.to_string_value());
                    err.exception = Some(Box::new(ex.clone()));
                    Some(Err(err))
                }
                None => Some(Ok(Value::NIL)),
            }
        }
        ValueView::Seq(body) => {
            if body.has_deferred_source() && !body.is_cached() {
                return None;
            }
            // No pull to run here (already reified, cache-requested or taken).
            let _ = body.sink_explicit(|_, _| {
                unreachable!("sink() only pulls a deferred, non-cached source, excluded above")
            });
            Some(Ok(Value::NIL))
        }
        ValueView::LazyList(_) => None,
        _ => Some(Ok(Value::NIL)),
    }
}
