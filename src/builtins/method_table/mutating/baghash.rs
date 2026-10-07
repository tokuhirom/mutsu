//! `BagHash.add` / `BagHash.remove` — the two named per-key count mutators
//! (ADR-11276 §9.23).
//!
//! Rakudo declares BOTH methods on `BagHash` itself, **not** on the `Baggy`
//! role (verified against the reference implementation:
//! `BagHash.^can('add')[0].package.^name` is `BagHash`, and
//! `MixHash.^can('add')` / `SetHash.^can('add')` / `Bag.^can('add')` are all
//! empty). So `MixHash`, `SetHash`, `Bag` and `Mix` correctly have no such
//! method and must keep falling through to "No such method".
//!
//! Semantics (all established against `raku` before implementing):
//!
//! * The single positional argument is iterated **one level**: a `Str`, `Int`
//!   or `Pair` is a single element (a `Pair` is not `Iterable`, so it becomes
//!   an element key in its own right), while a `List`/`Array`/`Seq`/`Range`
//!   yields its elements and a `Hash`/`Set`/`Bag`/`Mix` yields its
//!   `key => weight` pairs. There is no deep flattening — a nested list is one
//!   element. `runtime::utils::value_to_list` is exactly this iteration.
//! * Each yielded element moves its own count by exactly `+1` (`add`) or `-1`
//!   (`remove`); a duplicated element therefore moves twice.
//! * A count that lands at or below zero drops the key entirely — the same rule
//!   the subscript store enforces (`$b<k> = 0` removes `k`) — so `remove` of an
//!   absent key is a no-op and never stores a negative count.
//! * Both return `Nil`.
//!
//! The counts are adjusted **in place** through the bag's shared `Gc` node (the
//! mechanism `$b<k>++` already uses), so every alias of the BagHash observes the
//! mutation and an invocant with no variable name to write back through
//! (`$obj.bag.add(...)`, `@a[0].remove(...)`) works the same way. The place's
//! only job is to re-seat the same, mutated value in both halves of the dual
//! store, so a later locals-to-env sync cannot resurrect a stale snapshot of
//! the bag (`my %b is BagHash` reproduced exactly that).

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

/// `add` and `remove` take one positional argument, but the row is slurpy from
/// zero so that a call with another count reaches the handler, which raises
/// Rakudo's arity error (`method add(BagHash:D: \to-add, *%_)`) instead of the
/// call falling through to "No such method". The row declares no named
/// argument: the guard step drops them (`BagHash.new(1,2,2).add(1, :zzz)` is
/// `Nil` in Rakudo, where mutsu used to die with "Too many positionals").
pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "BagHash",
        name: "add",
        arity: 0,
        handler: Handler::Mut(add_row),
        flags: RowFlags::SLURPY,
        named: &[],
    },
    MethodRow {
        owner: "BagHash",
        name: "remove",
        arity: 0,
        handler: Handler::Mut(remove_row),
        flags: RowFlags::SLURPY,
        named: &[],
    },
];

/// `BagHash.add`.
// Cost: O(n), n = elements the argument yields.
fn add_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(adjust(interp, place, true, args))
}

/// `BagHash.remove`.
// Cost: O(n), n = elements the argument yields.
fn remove_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(adjust(interp, place, false, args))
}

/// Move each element of the one argument's count by one, in place through the
/// bag's shared node, and answer `Nil`.
// Cost: O(n) bag updates, n = elements the argument yields.
fn adjust(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    adding: bool,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        let word = if args.is_empty() { "few" } else { "many" };
        return Err(RuntimeError::new(format!(
            "Too {word} positionals passed; expected 2 arguments but got {}",
            args.len() + 1
        )));
    }
    let items = crate::runtime::utils::value_to_list(&args[0]);
    // Own a handle on the shared node before mutating: the write goes through
    // `gc_data_mut`, which for an aliased node writes the contents in place.
    let mut bag = place.value().descalarize().clone();
    debug_assert!(matches!(bag.view(), ValueView::Bag(_, true)));
    bag.with_bag_mut(|gc, _| {
        let data = crate::value::gc_data_mut(gc);
        for item in &items {
            let (key, elem) = crate::runtime::utils::quanthash_elem_entry(item);
            let current = data
                .counts
                .get(&key)
                .cloned()
                .unwrap_or_else(num_bigint::BigInt::default);
            let next = if adding { current + 1 } else { current - 1 };
            if num_traits::Signed::is_positive(&next) {
                crate::runtime::utils::record_quanthash_original(
                    data.original_keys.get_or_insert_with(Default::default),
                    &key,
                    &elem,
                );
                data.counts.insert(key, next);
            } else {
                data.counts.remove(&key);
                if let Some(originals) = data.original_keys.as_mut() {
                    originals.remove(&key);
                }
            }
        }
    });
    place.reseat(interp);
    Ok(Value::NIL)
}
