//! `STORE` on `Array`, `List`, `Hash` and `Map` (ADR-11276 §9.174): the
//! container protocol's re-initialisation, which `@a = ...` / `%h = ...` and a
//! user's `.STORE` reach.
//!
//! Rakudo declares them so:
//!
//! - `Array.STORE(Iterable:D)` / `STORE(Mu \item)` replaces the elements,
//!   checking each against the element type, and answers the array.
//! - `List.STORE` without `:INITIALIZE` is list assignment into the list's
//!   elements, so a list of plain values refuses with the first element's
//!   `X::Assignment::RO` (`(1, 2).STORE((3, 4))` names `Int (1)`).
//! - `Hash.STORE(\to_store)` and `STORE(\keys, \values)` replace the pairs.
//! - `Map.STORE` takes `:INITIALIZE`; a `Map` already built refuses with
//!   `X::Assignment::RO`.
//!
//! The store goes into the container's shared node, keeping its element type,
//! `is default` and other metadata (`Interpreter::store_into_array`,
//! `store_into_hash`). A user `is Hash`/`is Map` instance and a mixin reach the
//! same rows through `native_container_store`, which hands them their storage.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const INITIALIZE: &[&str] = &["INITIALIZE"];

macro_rules! row {
    ($owner:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: "STORE",
            arity: $arity,
            handler: Handler::Mut($handler),
            // Every `STORE` candidate wants a defined invocant (#13881).
            flags: RowFlags::DEFINITE_MULTI,
            named: INITIALIZE,
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Array", 1, positional_store),
    row!("List", 1, positional_store),
    row!("Hash", 1, associative_store),
    row!("Hash", 2, associative_store),
    row!("Map", 1, associative_store),
];

/// Whether the call passed a true `:INITIALIZE`.
// Cost: O(n), n = named arguments.
fn initializing(named: Named<'_>) -> bool {
    named.get("INITIALIZE").is_some_and(Value::truthy)
}

/// `Array.STORE` / `List.STORE`. Owners `Array` then `List` reach this one
/// handler, which tells them apart by the value's kind.
// Cost: O(n), n = elements stored (one type check each for a typed array).
fn positional_store(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    named: Named<'_>,
) -> Answer {
    let target = place.value().deref_container().descalarize().clone();
    let ValueView::Array(items, kind) = target.view() else {
        return None;
    };
    if kind.is_immutable_list() && !initializing(named) {
        // List assignment into the elements: a plain value is the first to
        // refuse. A list of containers (`($a, $b).STORE(...)`) assigns
        // through them, which the interpreter's list assignment does.
        let first = items.iter().next()?.clone();
        if first.is_container_ref() {
            return None;
        }
        return Some(Err(RuntimeError::assignment_ro_typename(
            &crate::value::types::what_type_name(&first),
            &crate::runtime::gist_value(&first),
        )));
    }
    let values = crate::runtime::utils::value_to_list(&args[0]);
    if let Err(err) = super::array::check_element_types(interp, place.name(), &target, &values) {
        return Some(Err(err));
    }
    Interpreter::store_into_array(&items, kind, &args[0]);
    place.reseat(interp);
    // The answer is the container, not the item a `$` binding holds it in.
    Some(Ok(target.deitemize_for_sigil_bind()))
}

/// `Hash.STORE(\to_store)`, `Hash.STORE(\keys, \values)` and `Map.STORE`.
// Cost: O(n), n = pairs stored.
fn associative_store(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    named: Named<'_>,
) -> Answer {
    let target = place.value().deref_container().descalarize().clone();
    let ValueView::Hash(gc) = target.view() else {
        return None;
    };
    if target.is_immutable_map() && !initializing(named) {
        return Some(Err(RuntimeError::assignment_ro_typename(
            "Map",
            &crate::runtime::gist_value(&target),
        )));
    }
    let to_store = match args {
        [keys, values] => {
            let keys = crate::runtime::utils::value_to_list(keys);
            let values = crate::runtime::utils::value_to_list(values);
            let pairs: Vec<Value> = keys
                .into_iter()
                .zip(values.into_iter().chain(std::iter::repeat(Value::NIL)))
                .flat_map(|(key, value)| [key, value])
                .collect();
            Value::array(pairs)
        }
        [to_store] => to_store.clone(),
        _ => return None,
    };
    // A typed hash (`Hash[Int]`, `my Int %h`) checks each stored value against
    // its element type before the node is replaced, so a refused store leaves
    // the hash as it was.
    let values = stored_hash_values(&to_store);
    if let Err(err) = interp.check_array_value_element_types(&target, &values) {
        return Some(Err(err));
    }
    if let Err(err) = interp.store_into_hash(&gc, &to_store) {
        return Some(Err(err));
    }
    place.reseat(interp);
    Some(Ok(target.deitemize_for_sigil_bind()))
}

/// The values `Hash.STORE` is about to file under keys. Like Rakudo's
/// `Hash.STORE`, each item is taken on its own: a `Pair` stores its value, a
/// `Hash` its values, and any other item is a key whose value is the item that
/// follows it.
// Cost: O(n), n = items in `to_store`.
fn stored_hash_values(to_store: &Value) -> Vec<Value> {
    let items = crate::runtime::utils::value_to_list(to_store);
    let mut values = Vec::with_capacity(items.len());
    let mut iter = items.iter();
    while let Some(item) = iter.next() {
        match item.view() {
            ValueView::Pair(_, value) | ValueView::ValuePair(_, value) => values.push(value.clone()),
            ValueView::Hash(map) => values.extend(map.values().cloned()),
            // A bare key: its value is the next item.
            _ => {
                if let Some(value) = iter.next() {
                    values.push(value.clone());
                }
            }
        }
    }
    values
}
