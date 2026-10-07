//! `Hash.push` and `Hash.append` (ADR-11276 §9.23).
//!
//! Both merge the pairs of their arguments into the hash: a key that is already
//! there turns its value into an `Array` of the old and the new one (`append`
//! flattens an `Array` argument into that stack). The arguments are Rakudo's
//! `+new` capture: a `Pair`, a list, `Seq` or `Slip` of them, a `Hash`, or an
//! alternating `key, value` list.
//!
//! The write goes **in place** through the hash's shared `Gc` node (container
//! identity, ADR-0013 §3), so every holder sees it. What the place adds is the
//! name: a typed or object hash (`my Int %h{Rat}`) declares its key and value
//! types on the variable as well as on the container, a `%h` may hold a shared
//! `ContainerRef` cell (a `%r := %h` rebind, an `rw` argument, having been
//! passed to a Raku-level routine), and a name that does not resolve to a hash
//! at all gets a fresh one. A detached hash (`f().push(...)`) has the container's
//! own metadata and nothing else.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueMap, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Hash",
            name: $name,
            arity: 0,
            handler: Handler::Mut($handler),
            flags: RowFlags::SLURPY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[row!("push", push_row), row!("append", append_row)];

/// `Hash.push`: a repeated key stacks the new value after the old ones.
// Cost: O(p), p = pairs the arguments name (one hash insert each).
fn push_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(push_or_append(interp, place, true, args))
}

/// `Hash.append`: a repeated key stacks the new value after the old ones, and
/// an `Array` value is flattened into the stack.
// Cost: O(p), p = pairs the arguments name (one hash insert each).
fn append_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(push_or_append(interp, place, false, args))
}

/// Merge the pairs `args` name into the receiver, in place.
// Cost: O(p), p = pairs the arguments name (one hash insert each); typed and
// object hashes add one type check per pair.
fn push_or_append(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    is_push: bool,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    // What the type error names: the variable, or `%_` for a hash with none.
    let name = place.name().map(str::to_string);
    let shown = name.clone().unwrap_or_else(|| "%_".to_string());
    // The declared constraints: the hash's own metadata is authoritative (it
    // travels with the container), the variable's is the fallback.
    let var_key = name
        .as_deref()
        .and_then(|n| interp.var_hash_key_constraint(n));
    let var_value = name.as_deref().and_then(|n| interp.var_type_constraint(n));
    let stored = place.slot(interp).map(|v| v.clone());
    let (key_constraint, value_constraint, is_object_hash) = match stored.as_ref().map(Value::view)
    {
        Some(ValueView::Hash(h)) => (
            h.key_type.clone().or_else(|| var_key.clone()),
            h.value_type.clone().or_else(|| var_value.clone()),
            h.key_type.is_some() || var_key.is_some(),
        ),
        _ => (var_key.clone(), var_value, var_key.is_some()),
    };
    let needs_typed_push = is_object_hash
        || value_constraint
            .as_deref()
            .is_some_and(|c| !matches!(c, "" | "Any" | "Mu"));
    if needs_typed_push {
        return typed_push(
            interp,
            place,
            is_push,
            args,
            &shown,
            stored.as_ref(),
            key_constraint,
            value_constraint,
            is_object_hash,
        );
    }

    let hash_present = stored
        .as_ref()
        .is_some_and(|slot| matches!(slot.view(), ValueView::Hash(_)));
    if hash_present {
        let pairs = Interpreter::hash_push_collect_pairs(args.to_vec());
        // `slot` resolves to the same hash `stored` was cloned from.
        return Ok(place
            .slot(interp)
            .expect("the hash resolved above")
            .with_hash_mut(|arc_hash| {
                // Container identity (§3): push through a shared node.
                let hash = crate::value::gc_data_mut(arc_hash);
                for (k, v) in pairs {
                    Interpreter::hash_push_insert(hash, k, v, is_push);
                }
                Value::hash_with_data(arc_hash.clone())
            })
            .expect("the slot is a hash"));
    }

    // The name does not resolve to a hash: build one from the receiver as the
    // call read it, and bind it under the name.
    let mut hash: ValueMap = match place.value().view() {
        ValueView::Hash(h) => h.map.clone(),
        _ => ValueMap::default(),
    };
    for (k, v) in Interpreter::hash_push_collect_pairs(args.to_vec()) {
        Interpreter::hash_push_insert(&mut hash, k, v, is_push);
    }
    let result = Value::hash_with_data(Value::hash_arc(hash));
    place.assign(interp, result.clone());
    Ok(result)
}

/// The typed / object-hash arm: type-check each pushed key against the key
/// constraint and each value against the element type, and reject a push that
/// would turn a scalar-typed value into an array (a repeated key).
// Cost: O(p) type checks and inserts, p = pairs the arguments name.
#[allow(clippy::too_many_arguments)]
fn typed_push(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    is_push: bool,
    args: &[Value],
    shown: &str,
    stored: Option<&Value>,
    key_constraint: Option<String>,
    value_constraint: Option<String>,
    is_object_hash: bool,
) -> Result<Value, RuntimeError> {
    let kv_pairs = Interpreter::hash_push_collect_pairs_kv(args.to_vec());
    // Snapshot the existing stored value for each pushed key so the
    // duplicate-key array-conflict check can run before the mutable borrow
    // (`type_matches_value` needs `&mut Interpreter`).
    let existing: Vec<Option<Value>> = {
        let h = match stored.map(Value::view) {
            Some(ValueView::Hash(h)) => Some(h),
            _ => None,
        };
        kv_pairs
            .iter()
            .map(|(k, _)| {
                let wk = if is_object_hash {
                    crate::runtime::utils::value_which_key(k)
                } else {
                    k.to_string_value()
                };
                h.as_ref().and_then(|h| h.map.get(&wk).cloned())
            })
            .collect()
    };
    for (i, (k, v)) in kv_pairs.iter().enumerate() {
        if let Some(kc) = &key_constraint
            && !matches!(kc.as_str(), "" | "Any" | "Mu")
            && !interp.type_matches_value(kc, k)
        {
            return Err(interp.type_check_element_failure(shown, kc, k));
        }
        if let Some(vc) = &value_constraint
            && !matches!(vc.as_str(), "" | "Any" | "Mu")
        {
            if !matches!(v.view(), ValueView::Nil) && !interp.type_matches_value(vc, v) {
                return Err(interp.type_check_element_failure(shown, vc, v));
            }
            // A typed Hash-valued append merges two nested hashes, so it does
            // not turn the value into an Array. Other duplicate keys still need
            // the resulting Array checked against the element constraint.
            let merges_hashes = !is_push
                && matches!(vc.as_str(), "Hash" | "Hash()")
                && existing[i].as_ref().is_some_and(|ex| {
                    matches!(ex.view(), ValueView::Hash(_))
                        && matches!(v.view(), ValueView::Hash(_))
                });
            if !merges_hashes && let Some(ex) = &existing[i] {
                let resulting = match ex.view() {
                    ValueView::Array(arr, ..) => {
                        let mut items = arr.to_vec();
                        items.push(v.clone());
                        Value::real_array(items)
                    }
                    _ => Value::real_array(vec![ex.clone(), v.clone()]),
                };
                if !interp.type_matches_value(vc, &resulting) {
                    return Err(interp.type_check_element_failure(shown, vc, &resulting));
                }
            }
        }
    }
    let hash_present = place
        .slot(interp)
        .is_some_and(|slot| matches!(slot.view(), ValueView::Hash(_)));
    if hash_present {
        return Ok(place
            .slot(interp)
            .expect("the hash resolved above")
            .with_hash_mut(|arc_hash| {
                // Container identity (§3): push through a shared node.
                let hash = crate::value::gc_data_mut(arc_hash);
                for (k, v) in kv_pairs {
                    let wk = if is_object_hash {
                        crate::runtime::utils::value_which_key(&k)
                    } else {
                        k.to_string_value()
                    };
                    if is_object_hash {
                        hash.original_keys
                            .get_or_insert_with(ValueMap::default)
                            .insert(wk.clone(), k);
                    }
                    Interpreter::hash_push_insert_typed(
                        hash,
                        wk,
                        v,
                        is_push,
                        value_constraint.as_deref(),
                    );
                }
                Value::hash_with_data(arc_hash.clone())
            })
            .expect("the slot is a hash"));
    }
    // No existing hash under the name: build a fresh typed hash from the
    // pushed pairs (preserving the constraints).
    let mut map = ValueMap::default();
    let mut orig = ValueMap::default();
    for (k, v) in kv_pairs {
        let wk = if is_object_hash {
            crate::runtime::utils::value_which_key(&k)
        } else {
            k.to_string_value()
        };
        if is_object_hash {
            orig.insert(wk.clone(), k);
        }
        Interpreter::hash_push_insert_typed(&mut map, wk, v, is_push, value_constraint.as_deref());
    }
    let mut hd = crate::value::HashData::new(map);
    if is_object_hash {
        hd.original_keys = Some(orig);
        hd.key_type = key_constraint;
    }
    hd.value_type = value_constraint;
    let result = Value::hash_with_data(crate::gc::Gc::new(hd));
    place.assign(interp, result.clone());
    Ok(result)
}
