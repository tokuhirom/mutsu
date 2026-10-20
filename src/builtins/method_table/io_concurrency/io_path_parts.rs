//! `IO::Path::Parts`'s rows (ADR-11276 §9.114): the three parts of a path
//! (`IO::Spec.split`, `IO::Path.parts`) as one `Positional`, `Associative`
//! and `Iterable` object.
//!
//! Rakudo declares `AT-POS`, `AT-KEY` and `flat` on the class: the parts by
//! position (a `volume`, `dirname`, `basename` pair) or by name, and the three
//! pairs in that order; `iterator` iterates those pairs. Every other list
//! method is `Any`'s: those Rakudo writes on `self.iterator` (`map`, `head`,
//! `sort`, ...) read the pairs ([`pairs`], through `dispatch::call`),
//! those on `self.list` read the object as one item (`$parts.elems` is `1`),
//! and `.hash`/`.Hash`/`.Map` read its parts by name
//! (`map_hash_coerce::hash_contents`).

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::utils::io_path_parts_keys;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "IO::Path::Parts",
        name: "flat",
        arity: 0,
        handler: Handler::Narrow(flat),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "IO::Path::Parts",
        name: "AT-POS",
        arity: 1,
        handler: Handler::Narrow(at_pos),
        // `Int:D $pos`; any other index is bound through its `.Int`
        // (`index_arg`).
        flags: RowFlags::INDEX_ARG,
        named: &[],
    },
    MethodRow {
        owner: "IO::Path::Parts",
        name: "AT-KEY",
        arity: 1,
        handler: Handler::Narrow(at_key),
        flags: RowFlags::ANY_ARGS,
        named: &[],
    },
    MethodRow {
        owner: "IO::Path::Parts",
        name: "iterator",
        arity: 0,
        handler: Handler::Interp(iterator),
        flags: RowFlags::NONE,
        named: &[],
    },
];

/// The three part pairs of an `IO::Path::Parts` instance, in order; `None` for
/// any other value. `Any`'s methods that read `self.iterator`, a `for` loop and
/// an `@`-assignment iterate these.
// Cost: O(1).
pub(crate) fn pairs(target: &Value) -> Option<Vec<Value>> {
    match target.view() {
        ValueView::Instance { class_name, .. } if class_name == "IO::Path::Parts" => {
            all_pairs(target)
        }
        _ => None,
    }
}

/// The three part pairs of the receiver, in order.
// Cost: O(1).
fn all_pairs(target: &Value) -> Option<Vec<Value>> {
    (0..io_path_parts_keys().len())
        .map(|idx| pair_at(target, idx))
        .collect()
}

/// The part named `key`, or `Nil`.
fn part(target: &Value, key: &str) -> Option<Value> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(attributes.as_map().get(key).cloned().unwrap_or(Value::NIL))
}

/// The `key => part` pair of the part at position `idx`, or `Nil` past the
/// third. ADR-0021 I2: a data-minted pair is positional, not the
/// named-argument marker flavour.
fn pair_at(target: &Value, idx: usize) -> Option<Value> {
    let Some(key) = io_path_parts_keys().get(idx) else {
        return Some(Value::NIL);
    };
    Some(Value::value_pair(
        Value::str((*key).to_string()),
        part(target, key)?,
    ))
}

/// `IO::Path::Parts.flat`: the three part pairs, in order.
// Cost: O(1).
fn flat(target: &Value, _args: &[Value]) -> Answer {
    let pairs = all_pairs(target)?;
    Some(Ok(Value::seq(pairs)))
}

/// `IO::Path::Parts.iterator`: an iterator over the three part pairs.
// Cost: O(1).
fn iterator(interp: &mut Interpreter, target: &Value, _args: &[Value], _: Named<'_>) -> Answer {
    let pairs = all_pairs(target)?;
    Some(interp.call_method_with_values(Value::array(pairs), "iterator", vec![]))
}

/// `IO::Path::Parts.AT-POS($pos)`: the part pair at that position.
// Cost: O(1).
fn at_pos(target: &Value, args: &[Value]) -> Answer {
    let ValueView::Int(idx) = args.first()?.view() else {
        return None;
    };
    let idx = usize::try_from(idx).ok()?;
    pair_at(target, idx).map(Ok)
}

/// `IO::Path::Parts.AT-KEY($key)`: the part of that name.
// Cost: O(1).
fn at_key(target: &Value, args: &[Value]) -> Answer {
    part(target, &args.first()?.to_string_value()).map(Ok)
}
