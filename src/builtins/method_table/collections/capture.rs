//! `Capture`'s rows (ADR-11276 §10, slice 3C), and the `Capture` coercion every
//! collection owner declares.
//!
//! A `Capture` keeps its positional part (`.list`) and its named part
//! (`.hash`); `keys`, `values`, `kv`, `pairs` and `antipairs` interleave them,
//! the positional part indexed from 0.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueMap, ValueView};

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
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Capture",
        name: "EXISTS-POS",
        arity: 1,
        handler: Handler::Narrow(exists_pos),
        flags: RowFlags::NONE,
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
    if crate::builtins::methods_0arg::coercion::capture_needs_str_key(target) {
        return None;
    }
    Some(crate::builtins::methods_0arg::coercion::value_to_capture(
        target,
    ))
}

/// `.keys`: the positional indices, then the names.
// Cost: O(n), n = positional plus named arguments.
pub(crate) fn keys(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Capture { positional, named } = target.view() else {
        return None;
    };
    let mut keys: Vec<Value> = (0..positional.len() as i64).map(Value::int).collect();
    keys.extend(named.keys().map(|k| Value::str(k.clone())));
    Some(Ok(Value::seq(keys)))
}

/// `.values`: the positional values, then the named ones.
// Cost: O(n), n = positional plus named arguments.
pub(crate) fn values(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Capture { positional, named } = target.view() else {
        return None;
    };
    let mut vals = positional.to_vec();
    vals.extend(named.values().cloned());
    Some(Ok(Value::seq(vals)))
}

/// `.kv`: each key followed by its value.
// Cost: O(n), n = positional plus named arguments.
pub(crate) fn kv(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Capture { positional, named } = target.view() else {
        return None;
    };
    let mut kv = Vec::with_capacity(positional.len() * 2 + named.len() * 2);
    for (idx, v) in positional.iter().enumerate() {
        kv.push(Value::int(idx as i64));
        kv.push(v.clone());
    }
    for (k, v) in named.iter() {
        kv.push(Value::str(k.clone()));
        kv.push(v.clone());
    }
    Some(Ok(Value::seq(kv)))
}

/// `.pairs`: `index => value` for the positional part, `name => value` for
/// the named one.
// Cost: O(n), n = positional plus named arguments.
pub(crate) fn pairs(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Capture { positional, named } = target.view() else {
        return None;
    };
    let mut pairs: Vec<Value> = positional
        .iter()
        .enumerate()
        .map(|(idx, v)| Value::value_pair(Value::int(idx as i64), v.clone()))
        .collect();
    // ADR-0021 I2/P3: `.pairs`' output is data, not a call site -- the
    // named lane's entries default positional like the positional lane
    // above, even though they came from a named argument.
    pairs.extend(
        named
            .iter()
            .map(|(k, v)| Value::value_pair(Value::str(k.clone()), v.clone())),
    );
    Some(Ok(Value::seq(pairs)))
}

/// `.antipairs`: `value => index` and `value => name`.
// Cost: O(n), n = positional plus named arguments.
pub(crate) fn antipairs(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Capture { positional, named } = target.view() else {
        return None;
    };
    let mut pairs: Vec<Value> = positional
        .iter()
        .enumerate()
        .map(|(idx, v)| Value::value_pair(v.clone(), Value::int(idx as i64)))
        .collect();
    pairs.extend(
        named
            .iter()
            .map(|(k, v)| Value::value_pair(v.clone(), Value::str(k.clone()))),
    );
    Some(Ok(Value::seq(pairs)))
}

/// `.hash`: the named part, as an immutable `Map` whose values are not
/// element containers (`\(:x<1 2>).hash<x>.VAR.^name` is `List`).
// Cost: O(n), n = named arguments.
pub(crate) fn hash(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Capture { named, .. } = target.view() else {
        return None;
    };
    let mut map = ValueMap::default();
    for (k, v) in named {
        map.insert(k.clone(), v.clone());
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

/// `.list`: the positional part.
// Cost: O(p), p = positional arguments.
pub(crate) fn list(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Capture { positional, .. } = target.view() else {
        return None;
    };
    Some(Ok(Value::array(positional.to_vec())))
}

/// `.elems` and `.Numeric`: the number of positional arguments.
// Cost: O(1).
pub(crate) fn elems(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Capture { positional, .. } = target.view() else {
        return None;
    };
    Some(Ok(Value::int(positional.len() as i64)))
}

/// `.AT-POS($pos)`: the positional argument at `pos` (`Nil` past the end), a
/// failure for a negative index.
// Cost: O(1) for an Int index; O(d) to parse a Str one, d = chars.
pub(crate) fn at_pos(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Capture { positional, .. } = target.view() else {
        return None;
    };
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
    let ValueView::Capture { positional, .. } = target.view() else {
        return None;
    };
    let idx = index_of(&args[0])?;
    if idx < 0 {
        return Some(Err(RuntimeError::out_of_range(
            "Index",
            Value::int(idx),
            "0..^Inf",
        )));
    }
    Some(Ok(Value::truth((idx as usize) < positional.len())))
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
