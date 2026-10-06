use super::*;
use crate::value::{RuntimeError, Value};

mod collections;
mod ctors_mop;
mod instances;
mod io_concurrency;
mod mutating;
mod scalars;

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
    all_rows()
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
            if !row.handler.is_pure() {
                // An interpreter row is answered by the entries that have an
                // interpreter (`t/oo/method/method-table-guard-rows.t`); the
                // pure entry must decline it.
                assert!(
                    result.is_none(),
                    "{}.{} needs the interpreter but a pure entry answered it",
                    row.owner,
                    row.name
                );
                continue;
            }
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
    assert_eq!(lookup(DispatchShape::Str, elems, 0).unwrap().owner, "Any");
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

/// A named argument is split from the positionals and handed to the row that
/// declares it. A name no row binds takes the cascades, and so does a false
/// `:hammer`, which the row's signature does not bind.
#[test]
fn a_named_argument_reaches_only_the_row_that_declares_it() {
    let flat = Symbol::intern("flat");
    let nested = Value::array(vec![
        Value::int(1),
        Value::array(vec![Value::int(2), Value::array(vec![Value::int(3)])]),
    ]);
    let hammer = Value::pair("hammer".to_string(), Value::TRUE);
    let result = try_dispatch(&nested, flat, &[hammer]);
    assert!(
        matches!(result, Some(Ok(_))),
        "flat(:hammer) is a Named row"
    );
    for pair in [
        Value::pair("zzz".to_string(), Value::TRUE),
        Value::pair("hammer".to_string(), Value::FALSE),
    ] {
        assert!(
            try_dispatch(&nested, flat, &[pair]).is_none(),
            "an undeclared named argument must take the cascades"
        );
    }
    // A row that declares no named argument refuses every one: `elems` has
    // no `:hammer`.
    let hammer = Value::pair("hammer".to_string(), Value::TRUE);
    assert!(try_dispatch(&nested, Symbol::intern("elems"), &[hammer]).is_none());
}

/// A positional `Pair` (`ValuePair`) is not a named argument: ADR-0021.
#[test]
fn a_positional_pair_is_not_split_off() {
    let positional = Value::value_pair(Value::str_from("hammer"), Value::TRUE);
    let list = sample(DispatchShape::List);
    // `flat` takes no positional argument, so the call has no row.
    assert!(try_dispatch(&list, Symbol::intern("flat"), &[positional]).is_none());
}

/// By default a row is handed plain scalars only; `ANY_ARGS` opens it to any
/// plain argument, but never to what needs a probe the table skips.
#[test]
fn argument_admission_follows_the_row_flags() {
    use crate::value::JunctionKind;
    let list = sample(DispatchShape::List);
    let combinations = Symbol::intern("combinations");
    // `combinations($of)` is flagged ANY_ARGS: an Int and a Range both bind.
    for arg in [Value::int(2), Value::range(1, 2)] {
        assert!(
            matches!(try_dispatch(&list, combinations, &[arg]), Some(Ok(_))),
            "combinations binds an Int and a Range"
        );
    }
    // But a Junction must autothread, and a deferred Seq must be reified.
    let junction = Value::junction(JunctionKind::Any, vec![Value::int(1), Value::int(2)]);
    assert!(try_dispatch(&list, combinations, &[junction]).is_none());
    let seq = Value::seq(vec![Value::int(1)]);
    assert!(try_dispatch(&list, combinations, &[seq]).is_none());
    // `Str.index` has no ANY_ARGS: a Range is not a plain scalar.
    let text = sample(DispatchShape::Str);
    assert!(try_dispatch(&text, Symbol::intern("index"), &[Value::range(1, 2)]).is_none());
    assert!(matches!(
        try_dispatch(&text, Symbol::intern("index"), &[Value::str_from("b")]),
        Some(Ok(_))
    ));
}

/// An interpreter row is skipped by the pure entries: only a caller that has
/// the interpreter reaches it.
#[test]
fn an_interpreter_row_needs_an_interpreter() {
    let list = sample(DispatchShape::List);
    let collate = Symbol::intern("collate");
    assert!(names_a_row(collate, 0));
    assert!(try_dispatch(&list, collate, &[]).is_none());
    assert!(answer(&list, collate, &[]).is_none());
    let row = resolve(DispatchShape::List, collate, 0).expect("Any.collate resolves for a List");
    assert!(!super::row(row).handler.is_pure());
}
