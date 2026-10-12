//! `Any.join` (ADR-11276 slice 3C remainder, #12389 item 4).
//!
//! Rakudo declares `join` on `Any`, `List` and `Seq`; `List` and `Seq` have
//! their rows already. `Any.join` is `self.list.join`, so a hash reads as its
//! pairs, a `Pair` as a one-element list, a `Capture` or `Match` as its
//! positionals and a scalar as itself. [`join_core`] is the one body: the row
//! and the zero- and one-argument cascade arms all call it.

use super::{Handler, MethodRow, RowFlags, list};
use crate::value::{DispatchShape, RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[any_join_row(0), any_join_row(1)];

const fn any_join_row(arity: u8) -> MethodRow {
    MethodRow {
        owner: "Any",
        name: "join",
        arity,
        handler: Handler::Narrow(any_join),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    }
}

/// What [`join_core`] decided about a receiver.
enum Joined {
    /// The answer (or the error) of the join.
    Done(Result<Value, RuntimeError>),
    /// An element needs the interpreter to stringify it (or the receiver must
    /// be forced first).
    NeedsInterpreter,
    /// A `Thread`, whose `.join` is the thread-join primitive, not a list
    /// join.
    NotCovered,
}

/// The `Any.join` row.
// Cost: O(e + t), e = elements of the invocant, t = total chars of the result.
fn any_join(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let sep = args.first().map(Value::to_string_value).unwrap_or_default();
    match join_core(target, &sep) {
        Joined::Done(result) => Some(result),
        Joined::NeedsInterpreter | Joined::NotCovered => None,
    }
}

/// Join the elements `target` reads as with `sep`.
// Cost: O(e + t), e = elements of the invocant, t = total chars of the result
// (each element stringified once, one `join` into a single buffer).
fn join_core(target: &Value, sep: &str) -> Joined {
    if crate::builtins::is_join_lazy(target) {
        return Joined::Done(Ok(Value::str("...".to_string())));
    }
    match target.view() {
        ValueView::LazyList(_) => return Joined::NeedsInterpreter,
        ValueView::Seq(items) if items.is_consumed() && !items.is_cached() => {
            return Joined::Done(Err(crate::value::seq_consumed_error()));
        }
        _ => {}
    }
    // `.join` stringifies every element, so a zero-denominator Rational among
    // them dies like its own `.Str` (GH #9621).
    if let Err(err) = crate::runtime::utils::check_str_coercion_zero_denominator(target) {
        return Joined::Done(Err(err));
    }
    let from_items = |items: &[Value]| match list::join_items(items, sep) {
        Some(joined) => Joined::Done(Ok(joined)),
        None => Joined::NeedsInterpreter,
    };
    // A Uni/NFC/NFD/NFKC/NFKD value decomposes into its codepoints in their
    // original (unsorted) order: `'ba'.NFC.join(',')` is `"98,97"`.
    if let ValueView::Uni(u) = target.view() {
        let joined = u
            .codepoints()
            .iter()
            .map(|cp| cp.to_string())
            .collect::<Vec<_>>()
            .join(sep);
        return Joined::Done(Ok(Value::str(joined)));
    }
    if crate::runtime::utils::is_shaped_array(target) {
        return from_items(&crate::runtime::utils::shaped_array_leaves(target));
    }
    if let Some(items) = list::join_source_items(target) {
        return from_items(&items);
    }
    match target.view() {
        ValueView::Capture { positional, .. } => {
            let joined = positional
                .iter()
                .map(Value::to_string_value)
                .collect::<Vec<_>>()
                .join(sep);
            Joined::Done(Ok(Value::str(joined)))
        }
        // `Match.join` joins the POSITIONAL CAPTURES (`.list`), not the matched
        // string; a captureless match joins to "".
        ValueView::Instance { .. } if target.is_match_instance() => {
            let items: Vec<Value> = target
                .match_list()
                .and_then(|v| v.as_list_items().map(|i| i.to_vec()))
                .unwrap_or_default();
            let joined = items
                .iter()
                .map(Value::to_str_context)
                .collect::<Vec<_>>()
                .join(sep);
            Joined::Done(Ok(Value::str(joined)))
        }
        // A `Blob`/`Buf` lists as its elements: `Blob.new(1, 2).join(",")` is
        // "1,2" (its own `Str` would throw `X::Buf::AsStr`).
        ValueView::Instance { attributes, .. }
            if matches!(
                target.dispatch_shape(),
                Some(DispatchShape::Blob | DispatchShape::Buf)
            ) =>
        {
            from_items(&crate::value::value_buf::buf_elems_or_empty(&attributes))
        }
        // A Pair is a one-element list, so the separator never appears: the
        // result is the Pair's own `.Str`, `key\tvalue`.
        ValueView::Pair(..) | ValueView::ValuePair(..) => {
            Joined::Done(Ok(Value::str(target.to_string_value())))
        }
        ValueView::Hash(map) => {
            let joined = map
                .iter()
                .map(|(k, v)| format!("{}\t{}", k, v.to_string_value()))
                .collect::<Vec<_>>()
                .join(sep);
            Joined::Done(Ok(Value::str(joined)))
        }
        // A Set/Bag/Mix reads as its pairs (`self.list`): `key\tTrue`,
        // `key\tweight`.
        ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
            from_items(&crate::runtime::utils::value_to_list(target))
        }
        // A Range has no materialized backing slice: expand it, so
        // `(1..5).join` is "12345", not the space-separated gist.
        _ if target.is_range() => from_items(&crate::runtime::utils::value_to_list(target)),
        ValueView::Str(_)
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..)
        | ValueView::Bool(_) => Joined::Done(Ok(Value::str(target.to_string_value()))),
        // The built-in temporal classes (never a user subclass, whose `Str` may
        // be its own) are one-element lists of their `.Str`.
        _ if matches!(
            target.dispatch_shape(),
            Some(
                DispatchShape::Date
                    | DispatchShape::DateTime
                    | DispatchShape::Instant
                    | DispatchShape::Duration
            )
        ) =>
        {
            Joined::Done(Ok(Value::str(target.to_string_value())))
        }
        // Any other built-in class with a shape (never a user subclass, whose
        // `Str` may be its own: `IO::Path`, `IO::Handle`, `Version`, ...) is a
        // one-element list of its `.Str`.
        _ if target.dispatch_shape().is_some() => {
            Joined::Done(Ok(Value::str(target.to_string_value())))
        }
        // `.join` on a Thread is the thread-join primitive (block until the
        // thread finishes), which the runtime's `native_thread` routes.
        ValueView::Instance { class_name, .. } if class_name == "Thread" => Joined::NotCovered,
        ValueView::Package(name) if name == "Thread" => Joined::NotCovered,
        // Anything else is a one-element list (`self.list`): an instance, a
        // type object, `Nil`, a code object. Its element is stringified with
        // its own `Str`, which may be a user method or warn (`Int.join` is ""
        // with a warning), so most need the interpreter.
        _ => from_items(std::slice::from_ref(target)),
    }
}
