//! `Any.serial`, `Any.batch` and `List.chrs` (ADR-11276 slice 3C remainder,
//! #12389 item 4). Each handler is the one body the cascade's arm calls too.

use super::{Handler, MethodRow, RowFlags};
use crate::value::JunctionKind;

macro_rules! junction_row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Any",
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::TYPE_OBJECT_OK,
            named: &[],
        }
    };
}
use crate::builtins::method_table::Named;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    // `Any.any`/`all`/`one`/`none`: a junction of the receiver's list
    // (ADR-11276 §9.134); a type object is a one-item list.
    junction_row!("any", any),
    junction_row!("all", all),
    junction_row!("one", one),
    junction_row!("none", none),
    // `Any.tree`: an `Iterable`'s elements treed level by level, any other
    // value itself (ADR-11276 §9.112).
    MethodRow {
        owner: "Any",
        name: "tree",
        arity: 0,
        handler: Handler::Pure(tree),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
    // `Any.nl-out`: the default newline separator, which `IO::Handle`
    // overrides with its own.
    MethodRow {
        owner: "Any",
        name: "nl-out",
        arity: 0,
        handler: Handler::Pure(nl_out),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
    // `$!.pending` (S04): the unhandled `Failure`s of the program. Rakudo has
    // no such method; mutsu answers it on any receiver, as `$!` may hold an
    // exception or nothing (`roast/S04-exceptions/pending.t`).
    MethodRow {
        owner: "Any",
        name: "pending",
        arity: 0,
        handler: Handler::Pure(pending),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "serial",
        arity: 0,
        handler: Handler::Pure(serial),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "batch",
        arity: 1,
        handler: Handler::Narrow(batch),
        flags: RowFlags::LIVE_VIEW,
        named: &[],
    },
    // `batch(:$elems!)`, Rakudo's named candidate (ADR-11276 §9.100).
    MethodRow {
        owner: "Any",
        name: "batch",
        arity: 0,
        handler: Handler::Named(batch_elems),
        flags: RowFlags::LIVE_VIEW,
        named: &["elems"],
    },
    MethodRow {
        owner: "List",
        name: "Supply",
        arity: 0,
        handler: Handler::Narrow(supply),
        flags: RowFlags::RANDOM,
        named: &[],
    },
    // `Any.chrs` is `self.list.chrs`: a number, a numeric string, a `Range`
    // or a `Seq` reaches it (ADR-11276 §9.112).
    MethodRow {
        owner: "Any",
        name: "chrs",
        arity: 0,
        handler: Handler::Pure(chrs),
        flags: RowFlags::NONE,
        named: &[],
    },
    // `Cool` declares it too (`self.list.chrs`).
    MethodRow {
        owner: "Cool",
        name: "chrs",
        arity: 0,
        handler: Handler::Pure(chrs),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "List",
        name: "chrs",
        arity: 0,
        handler: Handler::Pure(chrs),
        flags: RowFlags::NONE,
        named: &[],
    },
];

/// `Any.serial`: an ordinary value is already its own serial (non-parallel)
/// form, so it answers the invocant's *value* (like `.self`, issue #8490).
/// Only a hyper/race pipeline has a distinct serial form, which the hyper
/// method dispatch handles before reaching here.
// Cost: O(1).
pub(crate) fn serial(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(target.clone().deitemize_element())
}

/// `List.chrs`: the string of the characters whose codepoints the elements
/// (or the range's integers, or the lone invocant) name.
// Cost: O(e), e = elements of the invocant list.
pub(crate) fn chrs(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let val_to_i64 = |v: &Value| -> i64 {
        match v.view() {
            ValueView::Int(i) => i,
            ValueView::Num(f) => f as i64,
            _ => v.to_string_value().parse::<i64>().unwrap_or(0),
        }
    };
    // An element that is neither an `Int` nor a `Str` (a `Match`, a `Rat`,
    // ...) cannot be converted: `chrs` answers a `Failure` (#13195).
    let refused = |elems: &[Value]| -> Option<Value> {
        elems.iter().enumerate().find_map(|(index, elem)| {
            (!matches!(elem.view(), ValueView::Int(_) | ValueView::Str(_))).then(|| {
                RuntimeError::type_check_operation_failure(
                    &format!("converting element #{index} to .chr"),
                    elem.clone(),
                    "Int",
                )
            })
        })
    };
    match target.view() {
        ValueView::Array(items, ..) => {
            if let Some(failure) = refused(&items) {
                return Ok(failure);
            }
        }
        ValueView::Seq(items) => {
            if let Some(failure) = refused(&items) {
                return Ok(failure);
            }
        }
        _ => {}
    }
    let items: Vec<i64> = match target.view() {
        ValueView::Array(items, ..) => items.iter().map(&val_to_i64).collect(),
        ValueView::Seq(items) => items.iter().map(&val_to_i64).collect(),
        ValueView::Range(a, b) => (a..=b).collect(),
        ValueView::RangeExcl(a, b) => (a..b).collect(),
        _ => vec![val_to_i64(target)],
    };
    let mut s = String::with_capacity(items.len());
    for &code in &items {
        s.push(crate::builtins::str_prim::codepoint::chr_char(code)?);
    }
    Ok(Value::str(s))
}

/// `Any.batch(N)` and the named `.batch(:elems(N))`: the invocant's elements in
/// sublists of at most `N`. An `Array` batches lazily through a live cursor
/// (`Rakudo::Iterator.Batch`); a `Blob`/`Buf` batches its byte values.
// Cost: O(1) per call on an Array (lazy, `ListGen::Batch`), O(n) per batch
// pulled; O(e) on any other invocant, e = elements (decomposed, then chunked
// eagerly; a lazy invocant throws X::Cannot::Lazy before reaching here).
pub(crate) fn batch(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let [arg] = args else { return None };
    let n = match arg.view() {
        ValueView::Int(i) => i,
        ValueView::Pair(key, val) if key == "elems" || key == "batch" => val.to_f64() as i64,
        _ => return None,
    };
    if n < 1 {
        let message =
            format!("Batching sublist length is out of range. Is: {n}, should be in 1..^Inf");
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "what".to_string(),
            Value::str_from("Batching sublist length"),
        );
        attrs.insert("got".to_string(), Value::int(n));
        attrs.insert("range".to_string(), Value::str_from("1..^Inf"));
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(ex));
        return Some(Err(err));
    }
    let n = n as usize;
    if let ValueView::Array(_, kind) = target.view()
        && kind != crate::value::ArrayKind::Shaped
    {
        return Some(Ok(Value::seq_list_gen(
            crate::value::ListGen::batch(target.clone(), n),
            false,
        )));
    }
    let items = match crate::builtins::methods_narg::buf::buf_get_bytes(target) {
        Some(bytes) => bytes.into_iter().map(|b| Value::int(b as i64)).collect(),
        None => crate::runtime::value_to_list_for_receiver(target),
    };
    let batches: Vec<Value> = items
        .chunks(n)
        .map(|chunk| Value::array(chunk.to_vec()))
        .collect();
    Some(Ok(Value::seq(batches)))
}

/// `Any.batch(:$elems!)`: as [`batch`] with the named size. Without `:elems`
/// no candidate binds (`.batch()` is `X::Multi::NoMatch` in Rakudo).
// Cost: as `batch`.
fn batch_elems(
    target: &Value,
    _args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let Some(elems) = named.get("elems") else {
        return Some(Err(
            crate::runtime::methods_signature_errors::make_multi_no_match_error("batch"),
        ));
    };
    batch(target, &[Value::pair("elems".to_string(), elems.clone())])
}

/// `List.Supply`: a (non-live) `Supply` that emits the invocant's elements.
// Cost: O(e), e = elements of the invocant (a range is expanded through
// `value_to_list`, which caps it).
fn supply(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    supply_of(target).map(Ok)
}

/// The `Supply` of `target`'s elements, or `None` for a lazy list or a
/// `Channel`, which the interpreter drains.
// Cost: O(e), e = elements of the invocant.
pub(crate) fn supply_of(target: &Value) -> Option<Value> {
    if matches!(
        target.view(),
        ValueView::LazyList(_) | ValueView::Channel(_)
    ) {
        return None;
    }
    let values = match target.view() {
        ValueView::Array(items, ..) => items.to_vec(),
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            items.to_vec()
        }
        // Route Range-family sources through `value_to_list`, which caps at
        // `MAX_RANGE_EXPAND` instead of expanding `(a..=i64::MAX)` directly
        // (`(1..Inf).Supply` once `capacity overflow`-panicked).
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => crate::runtime::utils::value_to_list(target),
        _ => vec![target.clone()],
    };
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("values".to_string(), Value::array(values));
    attrs.insert("taps".to_string(), Value::array(Vec::new()));
    attrs.insert("live".to_string(), Value::FALSE);
    Some(Value::make_instance(Symbol::intern("Supply"), attrs))
}

/// `Any.tree`: every level of the receiver's elements as a `Seq`, a
/// non-`Iterable` receiver itself.
// Cost: O(1) on an Array (a lazy `ListGen::Tree`); O(e) on a Seq, Slip, Hash
// or Range, e = elements of the first level.
fn tree(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::value::flat::tree_to_depth(target, usize::MAX))
}

/// A junction of `kind` over `target`'s list. A list held in a scalar
/// (`my $l = (1, 2); $l.any`) is the list, not one item of it.
// Cost: O(n), n = elements of the receiver's list.
fn junction_of(kind: JunctionKind, target: &Value) -> Result<Value, RuntimeError> {
    let target = target.clone().deitemize_for_sigil_bind();
    Ok(Value::junction(
        kind,
        crate::runtime::Interpreter::value_to_list(&target),
    ))
}

// Cost: O(n), n = elements of the receiver's list.
fn any(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    junction_of(JunctionKind::Any, target)
}

// Cost: O(n), n = elements of the receiver's list.
fn all(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    junction_of(JunctionKind::All, target)
}

// Cost: O(n), n = elements of the receiver's list.
fn one(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    junction_of(JunctionKind::One, target)
}

// Cost: O(n), n = elements of the receiver's list.
fn none(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    junction_of(JunctionKind::None, target)
}

/// `Any.nl-out`.
// Cost: O(1).
pub(super) fn nl_out(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::str_from("\n"))
}

/// `pending`: a list of the `Failure`s not yet handled.
// Cost: O(f), f = pending failures.
pub(super) fn pending(_target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::array_with_kind(
        crate::gc::Gc::new(crate::value::ArrayData::new(
            crate::value::get_pending_failures(),
        )),
        crate::value::ArrayKind::List,
    ))
}
