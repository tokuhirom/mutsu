use super::*;

fn sample(shape: DispatchShape) -> Value {
    match shape {
        DispatchShape::List => Value::array(vec![Value::int(1), Value::int(2)]),
        DispatchShape::Array => Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                Value::int(1),
                Value::int(2),
            ])),
            crate::value::ArrayKind::Array,
        ),
        DispatchShape::Hash => Value::hash(crate::value::ValueMap::from_iter([(
            "a".to_string(),
            Value::int(1),
        )])),
        DispatchShape::Str => Value::str_from("abc"),
        DispatchShape::Int => Value::int(7),
        DispatchShape::Num => Value::num(1.5),
        DispatchShape::Rat => crate::value::make_rat(1, 3),
        DispatchShape::FatRat => Value::fat_rat_raw(1, 3),
        DispatchShape::Complex => Value::complex(1.0, 2.0),
    }
}

fn rows() -> impl Iterator<Item = &'static MethodRow> {
    FAMILIES.iter().flat_map(|rows| rows.iter())
}

#[test]
fn samples_have_their_shape() {
    for shape in SHAPES {
        assert_eq!(sample(shape).dispatch_shape(), Some(shape));
    }
}

#[test]
fn no_row_is_registered_twice() {
    let mut seen = std::collections::HashSet::new();
    for row in rows() {
        assert!(
            seen.insert((row.owner, row.name, row.arity)),
            "{}.{} has two rows",
            row.owner,
            row.name
        );
    }
}

/// Every row is reachable from at least one shape, and answers a plain
/// receiver of every shape it resolves for. An error is still an answer: the
/// receiver may fail the method's semantics (for example, summing Hash Pairs).
#[test]
fn every_row_is_reached_and_answers() {
    for row in rows() {
        let mut reached = false;
        for shape in SHAPES {
            let Some(found) = lookup(shape, Symbol::intern(row.name), row.arity) else {
                continue;
            };
            if !std::ptr::eq(found, row) {
                continue;
            }
            reached = true;
            let args = vec![Value::int(0); usize::from(row.arity)];
            let target = if row.name == "invert"
                && matches!(shape, DispatchShape::List | DispatchShape::Array)
            {
                Value::array(vec![Value::value_pair(Value::str_from("a"), Value::int(1))])
            } else {
                sample(shape)
            };
            let result = try_dispatch(&target, Symbol::intern(row.name), &args);
            assert!(
                result.is_some(),
                "{}.{} on a {shape:?} did not answer",
                row.owner,
                row.name
            );
        }
        assert!(reached, "{}.{} is reached by no shape", row.owner, row.name);
    }
}

/// A row is found one MRO level up: `Array` gets `List`'s `elems`, `Hash`
/// gets `Map`'s. A row is never found for a shape outside its owner's
/// subtypes.
#[test]
fn lookup_walks_the_mro() {
    let elems = Symbol::intern("elems");
    assert_eq!(
        lookup(DispatchShape::Array, elems, 0).unwrap().owner,
        "List"
    );
    assert_eq!(lookup(DispatchShape::List, elems, 0).unwrap().owner, "List");
    assert_eq!(lookup(DispatchShape::Hash, elems, 0).unwrap().owner, "Map");
    assert!(lookup(DispatchShape::Str, elems, 0).is_none());
    let numerator = Symbol::intern("numerator");
    assert!(lookup(DispatchShape::Rat, numerator, 0).is_some());
    assert!(lookup(DispatchShape::Num, numerator, 0).is_none());
    // Rakudo's `Int` does not do `Rational`: `5.numerator` is no method.
    assert!(lookup(DispatchShape::Int, numerator, 0).is_none());
    assert_eq!(
        lookup(DispatchShape::FatRat, numerator, 0).unwrap().owner,
        "FatRat"
    );
}

#[test]
fn counted_head_tail_rows_resolve_for_every_plain_shape() {
    for name in ["head", "tail"] {
        for shape in SHAPES {
            assert_eq!(
                lookup(shape, Symbol::intern(name), 1).unwrap().owner,
                "Any",
                "Any.{name} should resolve for {shape:?}"
            );
        }
    }
}

/// A big-component rational has the shape of the type its flag names.
#[test]
fn big_rationals_take_their_type_s_shape() {
    let big = num_bigint::BigInt::from(u64::MAX) * num_bigint::BigInt::from(3);
    let three = num_bigint::BigInt::from(3);
    assert_eq!(
        Value::bigrat(big.clone(), three.clone() + 1).dispatch_shape(),
        Some(DispatchShape::Rat)
    );
    assert_eq!(
        Value::bigfatrat(big.clone(), three + 1).dispatch_shape(),
        Some(DispatchShape::FatRat)
    );
    assert_eq!(
        Value::bigint(big).dispatch_shape(),
        Some(DispatchShape::Int)
    );
}

#[test]
fn a_call_with_another_arity_takes_the_cascades() {
    let target = sample(DispatchShape::List);
    assert!(try_dispatch(&target, Symbol::intern("elems"), &[Value::int(1)]).is_none());
}

/// The shape probe must refuse everything the receiver-state checks exist for.
#[test]
fn dispatch_shape_refuses_non_plain_receivers() {
    for (what, value) in [
        ("a type object", Value::package(Symbol::intern("Any"))),
        ("a Seq", Value::seq(vec![Value::int(1)])),
        ("a Bool", Value::TRUE),
        ("Nil", Value::NIL),
    ] {
        assert_eq!(
            value.dispatch_shape(),
            None,
            "{what} must not take the table"
        );
    }
}

/// A row's owner is the type Rakudo declares the method on, so `.^can`
/// (which reads the same owner from the recognition catalog) agrees with
/// where the table finds it. The catalog folds some owners (`FatRat` into
/// `Rat`, ADR-11276 §8), so a row is checked under its folded owner.
#[test]
fn rows_are_declared_by_rakudo() {
    for row in rows() {
        let owner = match crate::builtins::builtin_type_methods::canonical_builtin_owner(row.owner)
        {
            "" => row.owner,
            folded => folded,
        };
        assert!(
            crate::builtins::native_method_row::native_method_declared(owner, row.name),
            "{}.{} has a row, but the catalog does not record Rakudo declaring it there",
            row.owner,
            row.name
        );
    }
}

/// The name test carries the arity: a call with an argument count no row of
/// that name takes is refused before any lookup.
#[test]
fn the_name_test_knows_the_arity() {
    let index = Symbol::intern("index");
    assert!(names_a_row(index, 1));
    assert!(!names_a_row(index, 2));
    let substr = Symbol::intern("substr");
    assert!(names_a_row(substr, 1) && names_a_row(substr, 2));
    assert!(!names_a_row(Symbol::intern("elems"), 1));
    assert!(!names_a_row(substr, 200));
}

/// The shape test: a name with rows for other receivers is refused for this
/// one before any lookup.
#[test]
fn the_shape_test_knows_the_receiver() {
    assert!(
        SHAPES.len() <= 16,
        "a shape bit must fit Table::shapes' u16"
    );
    let int = Symbol::intern("Int");
    assert!(shape_has_row(DispatchShape::Int, int));
    assert!(shape_has_row(DispatchShape::Rat, int));
    assert!(!shape_has_row(DispatchShape::Str, int));
    // An inherited row sets the bit of the shape that reaches it.
    assert!(shape_has_row(DispatchShape::Array, Symbol::intern("elems")));
}

/// `List`'s count rows reach `Array` through its MRO, and `Map`'s reach
/// `Hash`.
#[test]
fn count_rows_reach_their_subtypes() {
    for name in ["keys", "Numeric", "Int"] {
        let sym = Symbol::intern(name);
        assert_eq!(lookup(DispatchShape::Array, sym, 0).unwrap().owner, "List");
        assert_eq!(lookup(DispatchShape::List, sym, 0).unwrap().owner, "List");
        assert_eq!(lookup(DispatchShape::Hash, sym, 0).unwrap().owner, "Map");
    }
}

#[test]
fn aggregate_rows_resolve_to_the_rakudo_owners() {
    for name in ["minmax", "sum"] {
        let sym = Symbol::intern(name);
        assert_eq!(
            lookup(DispatchShape::List, sym, 0).unwrap().owner,
            "Any",
            "Any.{name} should resolve for List"
        );
        assert_eq!(
            lookup(DispatchShape::Array, sym, 0).unwrap().owner,
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
        assert_eq!(lookup(DispatchShape::List, sym, 0).unwrap().owner, "List");
        assert_eq!(lookup(DispatchShape::Array, sym, 0).unwrap().owner, "List");
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
