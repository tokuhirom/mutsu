//! `Capture`'s rows (ADR-11276 §10, slice 3C), and the `Capture` coercion every
//! collection owner declares.
//!
//! A `Capture` keeps its positional part (`.list`) and its named part
//! (`.hash`); `keys`, `values`, `kv`, `pairs` and `antipairs` interleave them,
//! the positional part indexed from 0. `values` and `kv` flatten a value that
//! is a list or an array (not an itemized one), as Rakudo's slips do.
//!
//! A `Match` is a `Capture` whose parts are its captures (ADR-11276 §9.95):
//! the `Match` shape reaches these rows for the names in
//! `crate::value::CAPTURE_FOR_MATCH`, and [`parts`] reads its positional
//! captures (an unbound one as `Mu`) and its named ones in key order.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{ArrayKind, RuntimeError, Value, ValueMap, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

macro_rules! coercion_row {
    ($owner:literal) => {
        MethodRow {
            owner: $owner,
            name: "Capture",
            arity: 0,
            handler: Handler::Narrow(to_capture),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Capture", "keys", keys),
    row!("Capture", "values", values),
    row!("Capture", "kv", kv),
    row!("Capture", "pairs", pairs),
    row!("Capture", "antipairs", antipairs),
    row!("Capture", "hash", hash),
    row!("Capture", "list", list),
    row!("Capture", "elems", elems),
    row!("Capture", "Numeric", elems),
    MethodRow {
        owner: "Capture",
        name: "AT-POS",
        arity: 1,
        handler: Handler::Narrow(at_pos),
        // `Capture:D: Int(Any) $pos`: every index is coerced (`index_arg`).
        flags: RowFlags::INDEX_ARG.or(RowFlags::INDEX_COERCES),
        named: &[],
    },
    MethodRow {
        owner: "Capture",
        name: "EXISTS-POS",
        arity: 1,
        handler: Handler::Narrow(exists_pos),
        // `Capture:D: Int(Any) $pos`: every index is coerced (`index_arg`).
        flags: RowFlags::INDEX_ARG.or(RowFlags::INDEX_COERCES),
        named: &[],
    },
    // `.Capture`: every collection turns itself into a Capture.
    coercion_row!("Capture"),
    coercion_row!("List"),
    coercion_row!("Map"),
    coercion_row!("Range"),
    coercion_row!("Pair"),
    coercion_row!("Set"),
    coercion_row!("SetHash"),
    coercion_row!("Bag"),
    coercion_row!("BagHash"),
    coercion_row!("Mix"),
    coercion_row!("MixHash"),
];

/// `.Capture`: the Capture a value turns into (one implementation for every
/// owner). A list holding a `Pair` with a non-`Str` key is the interpreter's
/// (`Interpreter::try_interpreter_capture`): its key is named by its own
/// `.Str`, which is user code for a custom class.
// Cost: O(e), e = elements of the receiver.
fn to_capture(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    // A `Match` is a `Capture` already: `Capture.Capture` is the receiver.
    if target.is_match_instance() {
        return Some(Ok(target.clone()));
    }
    if crate::builtins::methods_0arg::coercion::capture_needs_str_key(target) {
        return None;
    }
    Some(crate::builtins::methods_0arg::coercion::value_to_capture(
        target,
    ))
}

/// A capture's positional values and its `(name, value)` pairs.
type Parts = (Vec<Value>, Vec<(String, Value)>);

/// The positional and named parts of a `Capture`, or of a `Match`: its
/// positional captures (an unbound one as `Mu`) and its named captures in key
/// order. A `Match` is asked first, so a lazy one (a grammar cursor) is read
/// from its capture node without materializing it.
// Cost: O(n), n = positional plus named parts (each cloned once); plus
// O(k log k) to order a `Match`'s k named captures.
fn parts(target: &Value) -> Option<Parts> {
    if target.is_match_instance() {
        let list = target.match_list();
        let positional = match list.as_ref().map(Value::view) {
            Some(ValueView::Array(items, _)) => items
                .iter()
                .map(|v| v.clone().unbound_capture_as_mu())
                .collect(),
            _ => Vec::new(),
        };
        let named_value = target.match_named();
        let mut named: Vec<(String, Value)> = match named_value.as_ref().map(Value::view) {
            Some(ValueView::Hash(map)) => map.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
            _ => Vec::new(),
        };
        named.sort_by(|a, b| a.0.cmp(&b.0));
        return Some((positional, named));
    }
    let ValueView::Capture { positional, named } = target.view() else {
        return None;
    };
    Some((
        positional.to_vec(),
        named.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
    ))
}

/// Push `value` onto `out`, flattening a list or an array that is not
/// itemized (a quantified capture's `Array` of matches, `\([1, 2]).values`).
// Cost: O(e), e = elements of a flattened value; O(1) otherwise.
fn push_flat(out: &mut Vec<Value>, value: Value) {
    if let ValueView::Array(items, ArrayKind::List | ArrayKind::Array) = value.view() {
        out.extend(items.iter().cloned());
    } else {
        out.push(value);
    }
}

/// `.keys`: the positional indices, then the names.
// Cost: O(n), n = positional plus named arguments.
pub(crate) fn keys(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (positional, named) = parts(target)?;
    let mut keys: Vec<Value> = (0..positional.len() as i64).map(Value::int).collect();
    keys.extend(named.into_iter().map(|(k, _)| Value::str(k)));
    Some(Ok(Value::seq(keys)))
}

/// `.values`: the positional values, then the named ones, each list or array
/// flattened.
// Cost: O(n), n = positional plus named arguments and their flattened elements.
pub(crate) fn values(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (positional, named) = parts(target)?;
    let mut vals = Vec::with_capacity(positional.len() + named.len());
    for v in positional {
        push_flat(&mut vals, v);
    }
    for (_, v) in named {
        push_flat(&mut vals, v);
    }
    Some(Ok(Value::seq(vals)))
}

/// `.kv`: each key followed by its value, a list or array value flattened.
// Cost: O(n), n = positional plus named arguments and their flattened elements.
pub(crate) fn kv(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (positional, named) = parts(target)?;
    let mut kv = Vec::with_capacity(positional.len() * 2 + named.len() * 2);
    for (idx, v) in positional.into_iter().enumerate() {
        kv.push(Value::int(idx as i64));
        push_flat(&mut kv, v);
    }
    for (k, v) in named {
        kv.push(Value::str(k));
        push_flat(&mut kv, v);
    }
    Some(Ok(Value::seq(kv)))
}

/// `.pairs`: `index => value` for the positional part, `name => value` for
/// the named one.
// Cost: O(n), n = positional plus named arguments.
pub(crate) fn pairs(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (positional, named) = parts(target)?;
    let mut pairs: Vec<Value> = positional
        .into_iter()
        .enumerate()
        .map(|(idx, v)| Value::value_pair(Value::int(idx as i64), v))
        .collect();
    // ADR-0021 I2/P3: `.pairs`' output is data, not a call site -- the
    // named lane's entries default positional like the positional lane
    // above, even though they came from a named argument.
    pairs.extend(
        named
            .into_iter()
            .map(|(k, v)| Value::value_pair(Value::str(k), v)),
    );
    Some(Ok(Value::seq(pairs)))
}

/// `.antipairs`: `value => index` and `value => name`.
// Cost: O(n), n = positional plus named arguments.
pub(crate) fn antipairs(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (positional, named) = parts(target)?;
    let mut pairs: Vec<Value> = positional
        .into_iter()
        .enumerate()
        .map(|(idx, v)| Value::value_pair(v, Value::int(idx as i64)))
        .collect();
    pairs.extend(
        named
            .into_iter()
            .map(|(k, v)| Value::value_pair(v, Value::str(k))),
    );
    Some(Ok(Value::seq(pairs)))
}

/// `.hash`: the named part, as an immutable `Map` whose values are not
/// element containers (`\(:x<1 2>).hash<x>.VAR.^name` is `List`).
// Cost: O(n), n = named arguments.
pub(crate) fn hash(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (_, named) = parts(target)?;
    let mut map = ValueMap::default();
    for (k, v) in named {
        map.insert(k, v);
    }
    let mut data = crate::value::HashData::new(map);
    data.declared_type = Some("Map".to_string());
    data.bare_values = true;
    Some(Ok(Value::hash_with_data(crate::gc::Gc::new(data))))
}

/// `.Hash`: the named part, as a mutable `Hash` (unlike `.hash`, which is a `Map`).
// Cost: O(n), n = named arguments.
pub(crate) fn hash_coerce(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Capture { named, .. } = target.view() else {
        return None;
    };
    let mut map = ValueMap::default();
    for (k, v) in named {
        map.insert(k.clone(), v.clone());
    }
    Some(Ok(Value::hash(map)))
}

/// `.list`: the positional part. A `Match`'s is a live view of its positional
/// captures, an unbound one a hole (`Value::match_list_view`).
// Cost: O(p), p = positional arguments; O(1) for a Match with no unbound
// capture (shared), O(p) otherwise.
pub(crate) fn list(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if target.is_match_instance() {
        return Some(Ok(target.match_list_view()));
    }
    let ValueView::Capture { positional, .. } = target.view() else {
        return None;
    };
    Some(Ok(Value::array(positional.to_vec())))
}

/// `.elems` and `.Numeric`: the number of positional arguments.
// Cost: O(1).
pub(crate) fn elems(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if target.is_match_instance() {
        let list = target.match_list();
        let count = match list.as_ref().map(Value::view) {
            Some(ValueView::Array(items, _)) => items.len(),
            _ => 0,
        };
        return Some(Ok(Value::int(count as i64)));
    }
    let ValueView::Capture { positional, .. } = target.view() else {
        return None;
    };
    Some(Ok(Value::int(positional.len() as i64)))
}

/// `.AT-POS($pos)`: the positional argument at `pos` (`Nil` past the end), a
/// failure for a negative index.
// Cost: O(1) for an Int index; O(d) to parse a Str one, d = chars.
pub(crate) fn at_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let positional = positionals(target)?;
    let idx = index_of(&args[0])?;
    if idx < 0 {
        return Some(Ok(RuntimeError::out_of_range_failure(
            "Index",
            Value::int(idx),
            "0..^Inf",
        )));
    }
    Some(Ok(positional
        .get(idx as usize)
        .cloned()
        .unwrap_or(Value::NIL)))
}

/// `.EXISTS-POS($pos)`: whether the positional part has an argument at
/// `pos`; a negative index is out of range.
// Cost: O(1) for an Int index; O(d) to parse a Str one, d = chars.
pub(crate) fn exists_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let positional = positionals(target)?;
    let idx = index_of(&args[0])?;
    if idx < 0 {
        return Some(Err(RuntimeError::out_of_range(
            "Index",
            Value::int(idx),
            "0..^Inf",
        )));
    }
    // A `Match`'s optional group that did not match is no capture (`Nil`).
    let is_match = target.is_match_instance();
    Some(Ok(Value::truth(
        positional
            .get(idx as usize)
            .is_some_and(|v| !(is_match && v.is_nil())),
    )))
}

/// The positional part of a `Capture`, or a `Match`'s positional captures
/// (one that did not match is `Nil`), which `AT-POS`/`EXISTS-POS` index.
// Cost: O(n), n = positional parts (copied).
fn positionals(target: &Value) -> Option<Vec<Value>> {
    if target.is_match_instance() {
        return Some(match target.match_list().as_ref().map(Value::view) {
            Some(ValueView::Array(items, _)) => items.to_vec(),
            _ => Vec::new(),
        });
    }
    match target.view() {
        ValueView::Capture { positional, .. } => Some(positional.to_vec()),
        _ => None,
    }
}

/// The integer a positional index argument stands for, or `None` for an
/// argument that is no number (the call then takes the cascades, which die on
/// it as Rakudo does).
// Cost: O(d), d = chars of a Str index; O(1) otherwise.
fn index_of(arg: &Value) -> Option<i64> {
    match arg.view() {
        ValueView::Int(i) => Some(i),
        ValueView::Num(f) if f.is_finite() => Some(f.floor() as i64),
        ValueView::Rat(n, d) if d > 0 => Some(n.div_euclid(d)),
        ValueView::Str(s) => s.trim().parse::<f64>().ok().map(|f| f.floor() as i64),
        _ => None,
    }
}
