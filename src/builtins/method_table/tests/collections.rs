//! Tests of the collections group (ADR-11276 §10, slice 3C).

use super::*;

#[test]
fn counted_head_tail_rows_resolve_for_every_plain_shape() {
    for name in ["head", "tail"] {
        for shape in DispatchShape::ALL
            .into_iter()
            .filter(|shape| shape.inherits())
        {
            assert_eq!(
                owner_of(shape, Symbol::intern(name), 1),
                "Any",
                "Any.{name} should resolve for {shape:?}"
            );
        }
    }
}

/// `List`'s count rows reach `Array` through its MRO, and `Map`'s reach
/// `Hash`.
#[test]
fn count_rows_reach_their_subtypes() {
    for name in ["keys", "Numeric", "Int"] {
        let sym = Symbol::intern(name);
        assert_eq!(owner_of(DispatchShape::Array, sym, 0), "List");
        assert_eq!(owner_of(DispatchShape::List, sym, 0), "List");
        assert_eq!(owner_of(DispatchShape::Hash, sym, 0), "Map");
    }
}

#[test]
fn aggregate_rows_resolve_to_the_rakudo_owners() {
    for name in ["minmax", "sum"] {
        let sym = Symbol::intern(name);
        assert_eq!(
            owner_of(DispatchShape::List, sym, 0),
            "Any",
            "Any.{name} should resolve for List"
        );
        assert_eq!(
            owner_of(DispatchShape::Array, sym, 0),
            "Any",
            "Any.{name} should resolve for Array"
        );
        assert!(
            lookup(DispatchShape::Hash, sym, 0).is_some_and(|row| row.owner == "Any"),
            "Any.{name} should resolve for Hash"
        );
    }
    for name in ["permutations", "combinations"] {
        let sym = Symbol::intern(name);
        assert_eq!(owner_of(DispatchShape::List, sym, 0), "List");
        assert_eq!(owner_of(DispatchShape::Array, sym, 0), "List");
        assert!(lookup(DispatchShape::Hash, sym, 0).is_none());
    }
}

#[test]
fn hash_sum_rejects_its_pair_list() {
    let result = try_dispatch(&sample(DispatchShape::Hash), Symbol::intern("sum"), &[]);
    assert!(
        matches!(result, Some(Err(_))),
        "Hash.sum should reject Pairs"
    );
}
