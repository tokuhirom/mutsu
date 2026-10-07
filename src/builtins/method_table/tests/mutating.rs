//! Tests of the mutating group (ADR-11276 §10, slice 3F).

use super::*;
use crate::symbol::Symbol;
use crate::value::DispatchShape;

/// A `Mut` row is registered by its owner only: no receiver shape finds it, so
/// no shape lookup, call-site lane or pure entry can run a handler that has
/// effects, and the debug cross-check cannot apply a mutation twice.
#[test]
fn a_mut_row_is_found_by_no_shape() {
    for row in rows().filter(|row| row.handler.is_mut()) {
        let name = Symbol::intern(row.name);
        for shape in DispatchShape::ALL {
            for arity in row.arities() {
                assert!(
                    lookup(shape, name, arity).is_none_or(|found| !std::ptr::eq(found, row)),
                    "{}.{} (a Mut row) is reachable by the {shape:?} shape",
                    row.owner,
                    row.name,
                );
            }
        }
    }
}

/// The pure entries decline a `Mut` row even when a shape's MRO lists its owner.
#[test]
fn a_pure_entry_declines_a_mut_row() {
    let list = sample(DispatchShape::Array);
    for name in [
        "push", "pop", "shift", "splice", "append", "unshift", "prepend",
    ] {
        let method = Symbol::intern(name);
        let args = [Value::int(1)];
        assert!(try_dispatch(&list, method, &args).is_none(), ".{name}");
        assert!(answer(&list, method, &args).is_none(), ".{name}");
    }
    let hash = sample(DispatchShape::Hash);
    assert!(try_dispatch(&hash, Symbol::intern("push"), &[Value::int(1)]).is_none());
    let bag = sample(DispatchShape::BagHash);
    assert!(try_dispatch(&bag, Symbol::intern("add"), &[Value::int(1)]).is_none());
}

/// Every `Mut` row is in the per-name arity mask `invoke_mut` tests first, at
/// every arity it takes; a slurpy row answers a call longer than the masks go.
#[test]
fn a_mut_row_is_in_the_mut_name_mask() {
    for row in rows().filter(|row| row.handler.is_mut()) {
        let name = Symbol::intern(row.name);
        for arity in row.arities() {
            assert!(
                names_a_mut_row(name, usize::from(arity)),
                "{}.{} is not in the mutating name mask at arity {arity}",
                row.owner,
                row.name,
            );
        }
        if row.flags.contains(RowFlags::SLURPY) {
            assert!(
                names_a_mut_row(name, 40),
                "{}.{} is slurpy but a 40-argument call misses the mask",
                row.owner,
                row.name,
            );
        }
        let found = owner_row(Symbol::intern(row.owner), name, usize::from(row.arity));
        assert!(
            found.is_some(),
            "{}.{} has no owner row",
            row.owner,
            row.name
        );
    }
}

/// The owner chain of a receiver's value kind is what the guard step looks the
/// row up along: an `Array` before its `List`, a mutable quant hash by its own
/// owner, an immutable one by the immutable owner.
#[test]
fn the_owner_chain_follows_the_value_kind() {
    use crate::builtins::method_table::mut_owners_of;
    assert_eq!(
        mut_owners_of(&sample(DispatchShape::Array), false),
        Some(&["Array", "List"][..])
    );
    assert_eq!(
        mut_owners_of(&sample(DispatchShape::List), false),
        Some(&["Array", "List"][..])
    );
    assert_eq!(
        mut_owners_of(&sample(DispatchShape::Hash), false),
        Some(&["Hash", "Map"][..])
    );
    assert_eq!(
        mut_owners_of(&sample(DispatchShape::BagHash), false),
        Some(&["BagHash"][..])
    );
    assert_eq!(
        mut_owners_of(&sample(DispatchShape::Bag), false),
        Some(&["Bag"][..])
    );
    assert_eq!(
        mut_owners_of(&sample(DispatchShape::SetHash), false),
        Some(&["SetHash"][..])
    );
    // A `Str` has a row only when the call names the variable it writes.
    assert_eq!(mut_owners_of(&sample(DispatchShape::Str), false), None);
    assert_eq!(
        mut_owners_of(&sample(DispatchShape::Str), true),
        Some(&["Str"][..])
    );
    assert_eq!(mut_owners_of(&sample(DispatchShape::Int), true), None);
}
