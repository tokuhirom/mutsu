//! `Any.serial`, `Any.batch` and `List.chrs` (ADR-11276 slice 3C remainder,
//! #12389 item 4). Each handler is the one body the cascade's arm calls too.

use super::{Handler, MethodRow, RowFlags};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Any",
        name: "serial",
        arity: 0,
        handler: Handler::Pure(serial),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "batch",
        arity: 1,
        handler: Handler::Narrow(batch),
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
    let items: Vec<i64> = match target.view() {
        ValueView::Array(items, ..) => items.iter().map(&val_to_i64).collect(),
        ValueView::Seq(items) => items.iter().map(&val_to_i64).collect(),
        ValueView::Range(a, b) => (a..=b).collect(),
        ValueView::RangeExcl(a, b) => (a..b).collect(),
        _ => vec![val_to_i64(target)],
    };
    let s: String = items
        .iter()
        .filter_map(|&code| char::from_u32(code as u32))
        .collect();
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
