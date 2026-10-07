//! `SetHash.set` / `.unset` and the QuantHash `.grab` / `.grabpairs` — the
//! mutators that remove or add whole keys (ADR-11276 §9.23).
//!
//! Like `BagHash.add`/`.remove` (`baghash`), every mutation here goes **in
//! place** through the QuantHash's shared `Gc` node. A mutable QuantHash is a
//! reference type in Raku: `my $b = $a` aliases it, and a `SetHash` held in an
//! attribute, returned by an accessor or stored in an element is the same object
//! every holder sees. The previous implementation rebuilt a fresh node and
//! re-bound the invocant's *variable name*, which only reached a plain lexical:
//! for `$!q.grab` the rebind landed on an env key no attribute read consults, and
//! `$obj.q.grab` had no name at all (#9609).
//!
//! The `Set`/`Bag`/`Mix` coercions never let a mutable node be shared with an
//! immutable one (see [`Value::quanthash_with_mutability`]), so writing through
//! the node can never change a `Set` value behind its holder's back.
//!
//! The immutable owners (`Set`, `Bag`, `Mix`) declare `grab` and `grabpairs`
//! too, and their rows throw `X::Immutable`, as Rakudo's do. `SetHash` has
//! `set`/`unset` and no other QuantHash does. A row is slurpy from zero
//! arguments: the count is optional (`$s.grab`, `$s.grab(2)`, `$s.grab(*)`,
//! `$s.grab(* div 2)`), and a call with another shape is answered as it was
//! before the rows (the extra argument is ignored). `set`/`unset` take exactly
//! one positional, as Rakudo's do, and raise its arity error for any other count.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};
use num_bigint::BigInt;
use num_traits::Signed;

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Mut($handler),
            flags: RowFlags::SLURPY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("SetHash", "set", set_row),
    row!("SetHash", "unset", unset_row),
    row!("SetHash", "grab", grab_row),
    row!("SetHash", "grabpairs", grabpairs_row),
    row!("BagHash", "grab", grab_row),
    row!("BagHash", "grabpairs", grabpairs_row),
    row!("MixHash", "grab", mixhash_grab_row),
    row!("MixHash", "grabpairs", grabpairs_row),
    row!("Set", "grab", immutable_grab_row),
    row!("Set", "grabpairs", immutable_grabpairs_row),
    row!("Bag", "grab", immutable_grab_row),
    row!("Bag", "grabpairs", immutable_grabpairs_row),
    row!("Mix", "grab", immutable_grab_row),
    row!("Mix", "grabpairs", immutable_grabpairs_row),
];

/// `SetHash.set`.
// Cost: O(k), k = keys named by the arguments (one user `WHICH` call each).
fn set_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(run(interp, place, "set", args))
}

/// `SetHash.unset`.
// Cost: O(k), k = keys named by the arguments (one user `WHICH` call each).
fn unset_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(run(interp, place, "unset", args))
}

/// `SetHash.grab`, `BagHash.grab`.
// Cost: see `apply`.
fn grab_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(run(interp, place, "grab", args))
}

/// `SetHash.grabpairs`, `BagHash.grabpairs`, `MixHash.grabpairs`.
// Cost: see `apply`.
fn grabpairs_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(run(interp, place, "grabpairs", args))
}

/// `MixHash.grab`: Rakudo declares it and refuses it (a grabbed element of a
/// weighted bag has no one-unit meaning), with an `X::AdHoc`.
// Cost: O(1).
fn mixhash_grab_row(
    _interp: &mut Interpreter,
    _place: &mut ReceiverPlace<'_>,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(Err(RuntimeError::new(
        ".grab is not supported on a MixHash",
    )))
}

/// `Set.grab`, `Bag.grab`, `Mix.grab`: immutable, so `X::Immutable`.
// Cost: O(1).
fn immutable_grab_row(
    _interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(Err(immutable(place.value(), "grab")))
}

/// `Set.grabpairs`, `Bag.grabpairs`, `Mix.grabpairs`: immutable, so `X::Immutable`.
// Cost: O(1).
fn immutable_grabpairs_row(
    _interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(Err(immutable(place.value(), "grabpairs")))
}

/// The `X::Immutable` of calling `method` on an immutable `Set`, `Bag` or `Mix`.
// Cost: O(1).
fn immutable(receiver: &Value, method: &str) -> RuntimeError {
    let typename = match receiver.descalarize().view() {
        ValueView::Set(..) => "Set",
        ValueView::Bag(..) => "Bag",
        _ => "Mix",
    };
    RuntimeError::immutable(typename, method)
}

/// Resolve a Callable count, user `WHICH` keys and the mutation itself, then
/// re-seat the receiver's dual store.
// Cost: O(k + m), k = keys passed (one user `WHICH` call each for `set` and
// `unset`), m = the mutator's own cost (see `apply`).
fn run(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    method: &str,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let receiver = place.value().descalarize().clone();
    let args = resolve_callable_count(&receiver, method, args.to_vec(), |f, a| {
        interp.call_sub_value(f, a, false)
    })?;
    if matches!(method, "set" | "unset") {
        // Key each object by its user `WHICH`, which the pure mutator cannot run.
        interp.warm_which_identity_all(&args);
    }
    let result = apply(&receiver, method, &args)?;
    place.reseat(interp);
    Ok(result)
}

/// The value a Callable count argument (`$s.grab(* div 2)`) is invoked with:
/// the number of distinct keys for `grabpairs` (and for any `SetHash` form),
/// the total weight for a `BagHash` `grab`.
// Cost: O(k), k = distinct keys (the total is a sum over the weights).
fn callable_count_input(receiver: &Value, method: &str) -> Value {
    match receiver.view() {
        ValueView::Set(data, _) => Value::int(data.elements.len() as i64),
        ValueView::Bag(data, _) if method == "grab" => {
            Value::from_bigint(data.counts.values().sum::<BigInt>())
        }
        ValueView::Bag(data, _) => Value::int(data.counts.len() as i64),
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
fn resolve_callable_count(
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

/// Apply `method` to `receiver` (which is a mutable QuantHash), mutating its shared node in place. A
/// Callable count argument must already have been resolved to an `Int` by the
/// caller (it needs the interpreter; see [`callable_count_input`]).
// Cost: O(a) for set/unset, a = keys named by the arguments; O(k + g) for a
// SetHash/MixHash grab, k = distinct keys, g = keys grabbed; O(k·g) for a
// BagHash grab (a weighted pick rescans the counts per draw).
fn apply(receiver: &Value, method: &str, args: &[Value]) -> Result<Value, RuntimeError> {
    // Own a handle on the shared node before mutating: with two holders the
    // write goes through `gc_data_mut`'s aliased branch, i.e. in place.
    let mut node = receiver.clone();
    match receiver.view() {
        ValueView::Set(..) if matches!(method, "set" | "unset") => {
            // Rakudo declares `method set(SetHash:D: \to-set, *%_)`: exactly one
            // positional; `invoke_mut` has already dropped the named arguments.
            if args.len() != 1 {
                let word = if args.is_empty() { "few" } else { "many" };
                return Err(RuntimeError::new(format!(
                    "Too {word} positionals passed; expected 2 arguments but got {}",
                    args.len() + 1
                )));
            }
            set_or_unset(&mut node, method == "set", args);
            Ok(Value::NIL)
        }
        ValueView::Set(..) => grab_set(&mut node, method, args),
        ValueView::Bag(..) => grab_bag(&mut node, method, args),
        ValueView::Mix(..) => grabpairs_mix(&mut node, args),
        _ => unreachable!("a QuantHash row was dispatched to a non-QuantHash"),
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

/// `SetHash.set(\to-set)` / `.unset(\to-set)`: the argument is one key, or a
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
                    // Setting an element that is already present stores the new
                    // element object, as rakudo's `SetHash.set` rebinds the key
                    // (an object with a custom `WHICH` may differ in every
                    // other attribute; CRDT's LWW-Element-Set relies on it).
                    if !matches!(elem.view(), ValueView::Str(_)) {
                        data.original_keys
                            .get_or_insert_with(Default::default)
                            .insert(k.clone(), elem.clone());
                    }
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

/// `MixHash.grabpairs`: `MixHash.grab` is refused (see `mixhash_grab_row`).
fn grabpairs_mix(node: &mut Value, args: &[Value]) -> Result<Value, RuntimeError> {
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
            grabbed.push(crate::runtime::utils::quanthash_typed_pair(
                elem,
                crate::value::mix_weight_to_value(weight),
            ));
        }
        grabbed
    });
    let grabbed = grabbed.unwrap_or_default();
    if grabbed.is_empty() {
        return Ok(Value::seq(Vec::new()));
    }
    Ok(grab_result(grabbed, single))
}
