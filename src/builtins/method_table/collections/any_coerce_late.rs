//! `Any`'s coercions to the quantity, map and sequence types, `Any.slice`,
//! `Mu.take` and `Bag.new-from-pairs`, as rows the MRO walk finds
//! (ADR-11276 §9.134, §9.135).
//!
//! Each is `OWNER_ONLY`: the guard step's shape tables never list it, so a
//! plain receiver keeps the native answer it had (the pure cascades, or the
//! pure row a twin stands behind: `Any.List`, `Any.hash`), and only the
//! by-name cascade's MRO walk (`any_list_method_by_mro`) reaches it, for the
//! receivers and calls those decline. The handlers are the bodies of the arms
//! they replace.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueMap, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const LATE: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::TYPE_OBJECT_OK);

macro_rules! late {
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    late!("Any", "Seq", 0, LATE, seq),
    late!("Any", "List", 0, LATE, list),
    late!("Any", "Set", 0, LATE, set),
    late!("Any", "SetHash", 0, LATE, set_hash),
    late!("Any", "Bag", 0, LATE, bag),
    late!("Any", "BagHash", 0, LATE, bag_hash),
    late!("Any", "Mix", 0, LATE, mix),
    late!("Any", "MixHash", 0, LATE, mix_hash),
    late!("Any", "Map", 0, LATE, map),
    late!("Any", "Hash", 0, LATE, hash_coerce),
    late!("Any", "hash", 0, LATE, hash),
    late!(
        "Any",
        "slice",
        0,
        RowFlags::OWNER_ONLY
            .or(RowFlags::ANY_ARGS)
            .or(RowFlags::SLURPY),
        slice
    ),
    late!("Mu", "take", 0, LATE, take),
    late!(
        "Bag",
        "new-from-pairs",
        0,
        LATE.or(RowFlags::ANY_ARGS)
            .or(RowFlags::SLURPY)
            .or(RowFlags::ANY_NAMED),
        new_from_pairs
    ),
    late!(
        "BagHash",
        "new-from-pairs",
        0,
        LATE.or(RowFlags::ANY_ARGS)
            .or(RowFlags::SLURPY)
            .or(RowFlags::ANY_NAMED),
        new_from_pairs
    ),
];

// Cost: O(n), n = elements of the receiver.
fn seq(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.dispatch_seq_coercion(t.clone()))
}

// Cost: O(n), n = elements of the receiver.
fn list(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    // A `Channel` drains its items (blocking receives), not one-item-wraps:
    // its own coercion answers it.
    if matches!(t.view(), ValueView::Channel(_)) {
        return None;
    }
    Some(i.dispatch_list_coercion(t.clone()))
}

// Cost: O(n), n = elements of the receiver.
fn set(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.dispatch_to_set_with_what(t.clone(), "Set"))
}

// Cost: O(n), n = elements of the receiver.
fn set_hash(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(
        i.dispatch_to_set_with_what(t.clone(), "SetHash")
            .map(|set| set.quanthash_with_mutability(true)),
    )
}

// Cost: O(n), n = elements of the receiver.
fn bag(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.dispatch_to_bag_with_what(t.clone(), "Bag"))
}

// Cost: O(n), n = elements of the receiver.
fn bag_hash(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(
        i.dispatch_to_bag_with_what(t.clone(), "BagHash")
            .map(|bag| bag.quanthash_with_mutability(true)),
    )
}

// Cost: O(n), n = elements of the receiver.
fn mix(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.dispatch_to_mix_with_what(t.clone(), "Mix"))
}

/// `.MixHash` is `.Mix` plus the mutable flag and embedded `MixHash` type
/// metadata; the metadata lives in the `Mix` Arc, so the whole coercion is a
/// pure value op shared with the VM native path.
// Cost: O(n), n = elements of the receiver.
fn mix_hash(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(crate::builtins::quanthash_coerce::to_mixhash(t.clone()))
}

// Cost: O(n), n = elements of the receiver.
fn map(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    if matches!(t.view(), ValueView::Package(_)) {
        return Some(Ok(Value::package(Symbol::intern("Map"))));
    }
    Some(crate::builtins::map_hash_coerce::to_map(t.clone()))
}

// Cost: O(n), n = elements of the receiver.
fn hash_coerce(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    // `.Hash` on the `Any` type object (an uninitialized `my $d`) or on `Nil`
    // is the empty hash, like the `.hash` method.
    if t.is_any_type_object() || t.is_nil() {
        return Some(Ok(Value::hash(ValueMap::default())));
    }
    if matches!(t.view(), ValueView::Package(_)) {
        return Some(Ok(Value::package(Symbol::intern("Hash"))));
    }
    Some(crate::builtins::map_hash_coerce::to_hash(t.clone(), true))
}

/// An instance's `.hash` is the interpreter's (its own `hash`, a `Map` or
/// `Hash` subclass), so this row declines it.
// Cost: O(n), n = elements of the receiver.
fn hash(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    if matches!(t.view(), ValueView::Instance { .. }) {
        return None;
    }
    Some(i.dispatch_to_hash(t.clone()))
}

/// `Seq.slice(@indices)`: the elements at the given 0-based indices, a
/// `Range` contributing every index it spans; no index is the empty `Seq`.
// Cost: O(n + k), n = elements of the receiver, k = indices requested.
fn slice(_: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    if args.is_empty() {
        return Some(Ok(Value::seq(Vec::new())));
    }
    let items = if let ValueView::Seq(arc) = t.view() {
        arc.to_vec()
    } else {
        crate::runtime::utils::value_to_list(t)
    };
    let mut result = Vec::with_capacity(args.len());
    for arg in args {
        let indices: Vec<usize> = match arg.view() {
            ValueView::Int(i) if i >= 0 => vec![i as usize],
            ValueView::Num(n) if n >= 0.0 => vec![n as usize],
            _ if arg.is_range() => crate::runtime::utils::value_to_list(arg)
                .iter()
                .filter_map(|v| match v.view() {
                    ValueView::Int(i) if i >= 0 => Some(i as usize),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        };
        result.extend(indices.into_iter().filter_map(|i| items.get(i).cloned()));
    }
    Some(Ok(Value::seq(result)))
}

/// `Mu.take`: the receiver taken into the enclosing `gather`, and returned.
// Cost: O(1).
fn take(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.take_value(t.clone()).map(|()| t.clone()))
}

/// `Bag.new-from-pairs(...)` / `BagHash.new-from-pairs(...)`: the pairs'
/// values are the counts. An instance's call is not this class method's.
// Cost: O(n), n = elements of the arguments.
fn new_from_pairs(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    let ValueView::Package(name) = t.view() else {
        return None;
    };
    let cn = name.resolve();
    if !matches!(cn.as_str(), "Bag" | "BagHash") {
        return None;
    }
    // A string-keyed pair (`"a" => 3`) is a pair to count, not an adverb.
    let args: Vec<&Value> = args.iter().chain(named.pairs()).collect();
    if args.iter().any(|arg| Interpreter::is_lazy_for_coerce(arg)) {
        return Some(Err(RuntimeError::cannot_lazy_what(&cn)));
    }
    let mut all_items = Vec::new();
    for arg in args {
        match arg.view() {
            ValueView::Array(items, ..) => all_items.extend(items.iter().cloned()),
            _ => all_items.push(arg.clone()),
        }
    }
    let result = i.dispatch_to_bag_with_what(Value::array(all_items), &cn);
    Some(if cn == "BagHash" {
        result.map(|bag| bag.quanthash_with_mutability(true))
    } else {
        result
    })
}
