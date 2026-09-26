//! `SetHash.set` / `.unset` and `SetHash`/`BagHash`/`MixHash` `.grab` /
//! `.grabpairs` — the QuantHash mutators that remove or add whole keys.
//!
//! Like `BagHash.add`/`.remove` (`vm_baghash_mutators`), every mutation here
//! goes **in place** through the QuantHash's shared `Gc` node. A mutable
//! QuantHash is a reference type in Raku: `my $b = $a` aliases it, and a
//! `SetHash` held in an attribute, returned by an accessor or stored in an
//! element is the same object every holder sees. The previous implementation
//! rebuilt a fresh node and re-bound the invocant's *variable name*, which only
//! reached a plain lexical: for `$!q.grab` the rebind landed on an env key no
//! attribute read consults, and `$obj.q.grab` had no name at all (#9609).
//!
//! The `Set`/`Bag`/`Mix` coercions never let a mutable node be shared with an
//! immutable one (see [`Value::quanthash_with_mutability`]), so writing through
//! the node can never change a `Set` value behind its holder's back.

use crate::value::{RuntimeError, Value, ValueView};
use num_bigint::BigInt;
use num_traits::Signed;

/// The mutable QuantHash receiver behind `target`, seeing through a `Scalar`
/// container, or `None` when `method` is not one of these mutators for this
/// invocant (an immutable `Set` keeps falling through to its own error path).
pub(crate) fn quanthash_mutator_receiver<'a>(target: &'a Value, method: &str) -> Option<&'a Value> {
    if !matches!(method, "set" | "unset" | "grab" | "grabpairs") {
        return None;
    }
    let inner = match target.view() {
        ValueView::Scalar(inner) => inner,
        _ => target,
    };
    let applies = match inner.view() {
        ValueView::Set(_, true) => true,
        ValueView::Bag(_, true) | ValueView::Mix(_, true) => {
            matches!(method, "grab" | "grabpairs")
        }
        _ => false,
    };
    applies.then_some(inner)
}

/// The value a Callable count argument (`$s.grab(* div 2)`) is invoked with:
/// the number of distinct keys for `grabpairs` (and for any `SetHash` form),
/// the total weight for a `BagHash`/`MixHash` `grab`.
// Cost: O(k), k = distinct keys (the total is a sum over the weights).
fn callable_count_input(receiver: &Value, method: &str) -> Value {
    match receiver.view() {
        ValueView::Set(data, _) => Value::int(data.elements.len() as i64),
        ValueView::Bag(data, _) if method == "grab" => {
            Value::from_bigint(data.counts.values().sum::<BigInt>())
        }
        ValueView::Bag(data, _) => Value::int(data.counts.len() as i64),
        ValueView::Mix(data, _) if method == "grab" => {
            crate::value::mix_weight_to_value(data.weights.values().sum::<f64>())
        }
        ValueView::Mix(data, _) => Value::int(data.weights.len() as i64),
        _ => Value::int(0),
    }
}

/// Lower the result of a Callable count argument to the `Int` count.
fn callable_count_to_int(count: Value) -> Value {
    match count.view() {
        ValueView::Int(n) => Value::int(n),
        ValueView::Num(f) => Value::int(f as i64),
        ValueView::Rat(n, d) if d != 0 => Value::int(n / d),
        _ => count,
    }
}

/// Resolve a Callable count argument (`$s.grab(* div 2)`) to its `Int`
/// count by invoking it through `call` with [`callable_count_input`]; any
/// other argument list is returned unchanged.
pub(crate) fn resolve_callable_count(
    receiver: &Value,
    method: &str,
    args: Vec<Value>,
    call: impl FnOnce(Value, Vec<Value>) -> Result<Value, RuntimeError>,
) -> Result<Vec<Value>, RuntimeError> {
    if !matches!(method, "grab" | "grabpairs") || args.len() != 1 || args[0].as_sub().is_none() {
        return Ok(args);
    }
    let input = callable_count_input(receiver, method);
    let count = call(args.into_iter().next().expect("one argument"), vec![input])?;
    Ok(vec![callable_count_to_int(count)])
}

/// Apply `method` to `receiver` (which must have come from
/// [`quanthash_mutator_receiver`]), mutating its shared node in place. A
/// Callable count argument must already have been resolved to an `Int` by the
/// caller (it needs the interpreter; see [`callable_count_input`]).
// Cost: O(a) for set/unset, a = keys named by the arguments; O(k + g) for a
// SetHash/MixHash grab, k = distinct keys, g = keys grabbed; O(k·g) for a
// BagHash grab (a weighted pick rescans the counts per draw).
pub(crate) fn apply_quanthash_mutator(
    receiver: &Value,
    method: &str,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    // Own a handle on the shared node before mutating: with two holders the
    // write goes through `gc_data_mut`'s aliased branch, i.e. in place.
    let mut node = receiver.clone();
    match receiver.view() {
        ValueView::Set(..) if matches!(method, "set" | "unset") => {
            set_or_unset(&mut node, method == "set", args);
            Ok(Value::NIL)
        }
        ValueView::Set(..) => grab_set(&mut node, method, args),
        ValueView::Bag(..) => grab_bag(&mut node, method, args),
        ValueView::Mix(..) => grab_mix(&mut node, method, args),
        _ => unreachable!("quanthash_mutator_receiver admitted a non-QuantHash"),
    }
}

/// The requested count: 1 with no argument, every key for `*`.
fn grab_count(args: &[Value], whatever: usize) -> usize {
    match args.first().map(Value::view) {
        None => 1,
        Some(ValueView::Whatever) => whatever,
        Some(_) => args[0].to_f64().max(0.0) as usize,
    }
}

fn is_nan_count(args: &[Value]) -> bool {
    matches!(args.first().map(Value::view), Some(ValueView::Num(f)) if f.is_nan())
}

/// The shared "one grabbed item unless a count was given" return shape.
fn grab_result(mut grabbed: Vec<Value>, single: bool) -> Value {
    if single && grabbed.len() == 1 {
        grabbed.pop().expect("one grabbed item")
    } else {
        Value::seq(grabbed)
    }
}

fn random_index(len: usize) -> usize {
    (crate::builtins::rng::builtin_rand() * len as f64) as usize % len
}

/// `SetHash.set(*@keys)` / `.unset(*@keys)`: each argument is one key, or a
/// list whose elements are keys.
fn set_or_unset(node: &mut Value, setting: bool, args: &[Value]) {
    node.with_set_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        for arg in args {
            let keys: Vec<Value> = match arg.view() {
                ValueView::Array(items, _) => items.to_vec(),
                ValueView::Seq(items) => items.to_vec(),
                ValueView::Slip(items) => items.to_vec(),
                _ => vec![arg.clone()],
            };
            for key in keys {
                let (k, elem) = crate::runtime::utils::quanthash_elem_entry(&key);
                if setting {
                    crate::runtime::utils::record_quanthash_original(
                        data.original_keys.get_or_insert_with(Default::default),
                        &k,
                        &elem,
                    );
                    data.elements.insert(k);
                } else {
                    data.elements.remove(&k);
                    if let Some(originals) = data.original_keys.as_mut() {
                        originals.remove(&k);
                    }
                }
            }
        }
    });
}

fn grab_set(node: &mut Value, method: &str, args: &[Value]) -> Result<Value, RuntimeError> {
    if is_nan_count(args) {
        return Err(RuntimeError::new(
            "Cannot .grab from a SetHash with NaN elements",
        ));
    }
    let single = method == "grab" && args.is_empty();
    let grabbed = node.with_set_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        let count = grab_count(args, data.elements.len());
        let mut keys: Vec<String> = data.elements.iter().cloned().collect();
        let mut grabbed = Vec::new();
        for _ in 0..count.min(keys.len()) {
            let key = keys.swap_remove(random_index(keys.len()));
            let elem = data.typed_key(&key);
            data.elements.remove(&key);
            if let Some(originals) = data.original_keys.as_mut() {
                originals.remove(&key);
            }
            grabbed.push(if method == "grabpairs" {
                crate::runtime::utils::quanthash_typed_pair(elem, Value::TRUE)
            } else {
                elem
            });
        }
        grabbed
    });
    let grabbed = grabbed.unwrap_or_default();
    if grabbed.is_empty() && single {
        return Ok(Value::NIL);
    }
    Ok(grab_result(grabbed, single))
}

fn grab_bag(node: &mut Value, method: &str, args: &[Value]) -> Result<Value, RuntimeError> {
    if is_nan_count(args) {
        return Err(RuntimeError::new("Cannot convert NaN to Int"));
    }
    let single = args.is_empty();
    let grabbed = node.with_bag_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        let whatever = if method == "grabpairs" {
            data.counts.len()
        } else {
            crate::runtime::utils::bigint_to_i128_sat(&data.counts.values().sum::<BigInt>()).max(0)
                as usize
        };
        let count = grab_count(args, whatever);
        let mut grabbed = Vec::new();
        for _ in 0..count {
            if data.counts.is_empty() {
                break;
            }
            let key = if method == "grabpairs" {
                let keys: Vec<&String> = data.counts.keys().collect();
                keys[random_index(keys.len())].clone()
            } else {
                // A weighted pick: one unit of weight per draw.
                let total: i128 = data
                    .counts
                    .values()
                    .map(crate::runtime::utils::bigint_to_i128_sat)
                    .sum();
                if total <= 0 {
                    break;
                }
                let r = (crate::builtins::rng::builtin_rand() * total as f64) as i128;
                let mut cumulative = 0i128;
                let mut chosen = None;
                for (k, v) in &data.counts {
                    cumulative += crate::runtime::utils::bigint_to_i128_sat(v);
                    if r < cumulative {
                        chosen = Some(k.clone());
                        break;
                    }
                }
                match chosen {
                    Some(k) => k,
                    None => break,
                }
            };
            let elem = data.typed_key(&key);
            let remaining = if method == "grabpairs" {
                let weight = data.counts.remove(&key).unwrap_or_default();
                grabbed.push(crate::runtime::utils::quanthash_typed_pair(
                    elem,
                    Value::from_bigint(weight),
                ));
                false
            } else {
                let c = data.counts.get_mut(&key).expect("chosen key is present");
                *c -= BigInt::from(1);
                grabbed.push(elem);
                c.is_positive()
            };
            if !remaining {
                data.counts.remove(&key);
                if let Some(originals) = data.original_keys.as_mut() {
                    originals.remove(&key);
                }
            }
        }
        grabbed
    });
    let grabbed = grabbed.unwrap_or_default();
    if grabbed.is_empty() && method == "grab" && single {
        return Ok(Value::NIL);
    }
    Ok(grab_result(grabbed, single))
}

fn grab_mix(node: &mut Value, method: &str, args: &[Value]) -> Result<Value, RuntimeError> {
    let single = args.is_empty();
    let grabbed = node.with_mix_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        let count = grab_count(args, data.weights.len());
        let mut keys: Vec<String> = data.weights.keys().cloned().collect();
        let mut grabbed = Vec::new();
        for _ in 0..count.min(keys.len()) {
            let key = keys.swap_remove(random_index(keys.len()));
            let elem = data.typed_key(&key);
            let weight = data.weights.remove(&key).unwrap_or(0.0);
            if let Some(originals) = data.original_keys.as_mut() {
                originals.remove(&key);
            }
            grabbed.push(if method == "grabpairs" {
                crate::runtime::utils::quanthash_typed_pair(
                    elem,
                    crate::value::mix_weight_to_value(weight),
                )
            } else {
                elem
            });
        }
        grabbed
    });
    let grabbed = grabbed.unwrap_or_default();
    if grabbed.is_empty() {
        return Ok(Value::seq(Vec::new()));
    }
    Ok(grab_result(grabbed, single))
}
