//! `ASSIGN-KEY` and `DELETE-KEY` on a `Hash` and the six quant hashes
//! (ADR-11276 slice 4, §9.37).
//!
//! The subscript protocol's two keyed mutators were four copies: the VM's
//! by-name `CallMethodMut` arms (a `Hash` rebuild plus an in-place shortcut, and a
//! rebuild per quant-hash kind), the `nextsame`/`callsame` bridges behind a user
//! `is Hash` / `is BagHash` subclass, the `Mixin` bridge and the by-value
//! interpreter entry. They are one row per owner now, reached through
//! [`invoke_mut`](crate::builtins::method_table::invoke_mut) for a named
//! binding, a detached container (an `is Hash` instance's storage, a `Hash`
//! inside a `Mixin`) and a by-value receiver alike.
//!
//! Every write goes **in place** through the container's shared `Gc` node
//! (container identity, ADR-0013 §3), so every alias of the hash or quant hash
//! sees it and the container's type metadata (keyed by the node) stays
//! attached; the place only re-seats the dual store afterwards. The immutable
//! `Set`, `Bag` and `Mix` refuse both methods with `X::Assignment::RO`, as the
//! VM arms did. `Map`'s `DELETE-KEY` refuses with Rakudo's wording
//! ([`refuse_map_removal`]); a `Pair` never reaches a row (it has no owner).

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::{Interpreter, refuse_map_removal};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueMap, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Mut($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Hash", "ASSIGN-KEY", 2, hash_assign_key),
    row!("Hash", "DELETE-KEY", 1, hash_delete_key),
    row!("SetHash", "ASSIGN-KEY", 2, sethash_assign_key),
    row!("SetHash", "DELETE-KEY", 1, sethash_delete_key),
    row!("BagHash", "ASSIGN-KEY", 2, baghash_assign_key),
    row!("BagHash", "DELETE-KEY", 1, baghash_delete_key),
    row!("MixHash", "ASSIGN-KEY", 2, mixhash_assign_key),
    row!("MixHash", "DELETE-KEY", 1, mixhash_delete_key),
    row!("Set", "ASSIGN-KEY", 2, immutable_set),
    row!("Set", "DELETE-KEY", 1, immutable_set),
    row!("Bag", "ASSIGN-KEY", 2, immutable_bag),
    row!("Bag", "DELETE-KEY", 1, immutable_bag),
    row!("Mix", "ASSIGN-KEY", 2, immutable_mix),
    row!("Mix", "DELETE-KEY", 1, immutable_mix),
];

/// The container the call acts on, as a handle on its shared node: a
/// `ContainerRef` cell a routine captured it in and a `Scalar` wrapper are seen
/// through.
// Cost: O(1).
fn container(place: &ReceiverPlace<'_>) -> Value {
    place.value().deref_container().descalarize().clone()
}

fn read_only(typename: &str, value: &Value) -> RuntimeError {
    RuntimeError::assignment_ro_typename(typename, &crate::runtime::gist_value(value))
}

/// `Hash.ASSIGN-KEY($key, $value)`: store `$value` under `$key`, an object
/// hash keying by `.WHICH` and remembering the key object. Answers `$value`.
// Cost: O(1) expected (one hash insert; an object hash also records its key
// object).
fn hash_assign_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut hash = container(place);
    let value = args[1].clone();
    let written = hash.with_hash_mut(|gc| -> Result<(), RuntimeError> {
        let data = crate::value::gc_data_mut(gc);
        let object_hash = data.key_type.is_some();
        let key = if object_hash {
            crate::runtime::utils::value_which_key(&args[0])
        } else {
            args[0].to_string_value()
        };
        // An entry `BIND-KEY`-bound to a bare value refuses assignment.
        Interpreter::check_assign_key_writable(data, &key)?;
        if object_hash {
            data.original_keys
                .get_or_insert_with(ValueMap::default)
                .insert(key.clone(), args[0].clone());
        }
        Value::hash_insert_through(&mut data.map, key, value.clone());
        Ok(())
    })?;
    Some(written.map(|()| {
        place.reseat(interp);
        value
    }))
}

/// `Hash.DELETE-KEY($key)`: remove the pair and answer what it held (the hash's
/// default or its value type object when the key was absent).
// Cost: O(1) expected (one hash probe and one removal).
fn hash_delete_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut hash = container(place);
    if let Err(error) = refuse_map_removal(&hash) {
        return Some(Err(error));
    }
    let ValueView::Hash(map) = hash.view() else {
        return None;
    };
    // An object hash stores `.WHICH` keys; a plain hash the stringified key.
    let key = if map.key_type.is_some() {
        crate::runtime::utils::value_which_key(&args[0])
    } else {
        args[0].to_string_value()
    };
    let old = if map.contains_key(&key) {
        interp.resolve_hash_entry(&map, &key)
    } else {
        map.default.as_deref().cloned().unwrap_or_else(|| {
            Value::package(Symbol::intern(map.value_type.as_deref().unwrap_or("Any")))
        })
    };
    hash.with_hash_mut(|gc| {
        let data = crate::value::gc_data_mut(gc);
        data.remove(&key);
        if let Some(original) = data.original_keys.as_mut() {
            original.remove(&key);
        }
    })?;
    place.reseat(interp);
    Some(Ok(old))
}

/// `SetHash.ASSIGN-KEY($key, $value)`: a true value adds the element, a false
/// one removes it. Answers `$value`.
// Cost: O(1) expected.
fn sethash_assign_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut set = container(place);
    let (key, elem) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
    let value = args[1].clone();
    set.with_set_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        if value.truthy() {
            crate::runtime::utils::record_quanthash_original(
                data.original_keys.get_or_insert_with(Default::default),
                &key,
                &elem,
            );
            data.elements.insert(key);
        } else {
            data.elements.remove(&key);
            if let Some(originals) = data.original_keys.as_mut() {
                originals.remove(&key);
            }
        }
    })?;
    place.reseat(interp);
    Some(Ok(value))
}

/// `SetHash.DELETE-KEY($key)`: remove the element and answer whether it was in.
// Cost: O(1) expected.
fn sethash_delete_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut set = container(place);
    let (key, _) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
    let existed = set.with_set_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        let existed = data.elements.remove(&key);
        if let Some(originals) = data.original_keys.as_mut() {
            originals.remove(&key);
        }
        existed
    })?;
    place.reseat(interp);
    Some(Ok(Value::truth(existed)))
}

/// `BagHash.ASSIGN-KEY($key, $count)`: a positive count stores, anything else
/// removes the key. Answers `$count`.
// Cost: O(1) expected.
fn baghash_assign_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut bag = container(place);
    let (key, elem) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
    let value = args[1].clone();
    let count = match Interpreter::bag_assignment_count(&value) {
        Ok(count) => count,
        Err(error) => return Some(Err(error)),
    };
    bag.with_bag_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        if num_traits::Signed::is_positive(&count) {
            crate::runtime::utils::record_quanthash_original(
                data.original_keys.get_or_insert_with(Default::default),
                &key,
                &elem,
            );
            data.counts.insert(key, count);
        } else {
            data.counts.remove(&key);
            if let Some(originals) = data.original_keys.as_mut() {
                originals.remove(&key);
            }
        }
    })?;
    place.reseat(interp);
    Some(Ok(value))
}

/// `BagHash.DELETE-KEY($key)`: remove the key and answer the count it had.
// Cost: O(1) expected.
fn baghash_delete_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut bag = container(place);
    let (key, _) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
    let old = bag.with_bag_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        let old = data.counts.remove(&key).unwrap_or_default();
        if let Some(originals) = data.original_keys.as_mut() {
            originals.remove(&key);
        }
        old
    })?;
    place.reseat(interp);
    Some(Ok(Value::from_bigint(old)))
}

/// `MixHash.ASSIGN-KEY($key, $weight)`: a non-zero weight stores, zero removes
/// the key. Answers `$weight`.
// Cost: O(1) expected.
fn mixhash_assign_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut mix = container(place);
    let (key, elem) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
    let value = args[1].clone();
    let weight = match Interpreter::mix_assignment_weight(&value) {
        Ok(weight) => weight,
        Err(error) => return Some(Err(error)),
    };
    mix.with_mix_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        if weight == 0.0 {
            data.weights.remove(&key);
            if let Some(originals) = data.original_keys.as_mut() {
                originals.remove(&key);
            }
        } else {
            crate::runtime::utils::record_quanthash_original(
                data.original_keys.get_or_insert_with(Default::default),
                &key,
                &elem,
            );
            data.weights.insert(key, weight);
        }
    })?;
    place.reseat(interp);
    Some(Ok(value))
}

/// `MixHash.DELETE-KEY($key)`: remove the key and answer the weight it had.
// Cost: O(1) expected.
fn mixhash_delete_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let mut mix = container(place);
    let (key, _) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
    let old = mix.with_mix_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        let old = data.weights.remove(&key).unwrap_or(0.0);
        if let Some(originals) = data.original_keys.as_mut() {
            originals.remove(&key);
        }
        old
    })?;
    place.reseat(interp);
    Some(Ok(crate::value::mix_weight_to_value(old)))
}

/// `ASSIGN-KEY` and `DELETE-KEY` on an immutable `Set`.
// Cost: O(n) to render the receiver for the message, n = elements.
fn immutable_set(
    _interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(Err(read_only("Set", &container(place))))
}

/// `ASSIGN-KEY` and `DELETE-KEY` on an immutable `Bag`.
// Cost: O(n) to render the receiver for the message, n = elements.
fn immutable_bag(
    _interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(Err(read_only("Bag", &container(place))))
}

/// `ASSIGN-KEY` and `DELETE-KEY` on an immutable `Mix`.
// Cost: O(n) to render the receiver for the message, n = elements.
fn immutable_mix(
    _interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(Err(read_only("Mix", &container(place))))
}
