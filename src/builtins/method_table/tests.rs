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
            seen.insert((row.owner, row.name)),
            "{}.{} has two rows",
            row.owner,
            row.name
        );
    }
}

/// Every row is reachable from at least one shape, and answers a plain
/// receiver of that shape with a value -- a row nothing can reach, or whose
/// handler refuses the receivers it is reached for, buys nothing.
#[test]
fn every_row_is_reached_and_answers() {
    for row in rows() {
        let mut reached = false;
        for shape in SHAPES {
            let Some(found) = lookup(shape, Symbol::intern(row.name)) else {
                continue;
            };
            if !std::ptr::eq(found, row) {
                continue;
            }
            reached = true;
            let args = vec![Value::int(0); usize::from(row.arity)];
            let result = try_dispatch(&sample(shape), Symbol::intern(row.name), &args);
            assert!(
                matches!(result, Some(Ok(_))),
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
    assert_eq!(lookup(DispatchShape::Array, elems).unwrap().owner, "List");
    assert_eq!(lookup(DispatchShape::List, elems).unwrap().owner, "List");
    assert_eq!(lookup(DispatchShape::Hash, elems).unwrap().owner, "Map");
    assert!(lookup(DispatchShape::Str, elems).is_none());
    let numerator = Symbol::intern("numerator");
    assert!(lookup(DispatchShape::Rat, numerator).is_some());
    assert!(lookup(DispatchShape::Num, numerator).is_none());
    // Rakudo's `Int` does not do `Rational`: `5.numerator` is no method.
    assert!(lookup(DispatchShape::Int, numerator).is_none());
    assert_eq!(lookup(DispatchShape::FatRat, numerator).unwrap().owner, "FatRat");
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
    assert_eq!(Value::bigint(big).dispatch_shape(), Some(DispatchShape::Int));
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
