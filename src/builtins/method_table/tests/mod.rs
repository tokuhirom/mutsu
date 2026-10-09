use super::*;
use crate::symbol::Symbol;
use crate::value::{DispatchShape, Value};

mod collections;
mod const_index;
mod ctors_mop;
mod instances;
mod io_concurrency;
mod mutating;
mod scalars;

/// The type object of a shape that has no instances.
fn type_sample(class: &str) -> Value {
    Value::package(Symbol::intern(class))
}

/// How a call on `sample(shape)` reaches the table: as an instance, or for a
/// shape with no instances, as its type object.
fn receiver_of_sample(shape: DispatchShape) -> Receiver {
    if shape.has_instances() {
        Receiver::instance(shape)
    } else {
        Receiver::type_object(shape)
    }
}

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
        DispatchShape::Bool => Value::TRUE,
        DispatchShape::Range => Value::range(1, 3),
        DispatchShape::Pair => Value::pair("a".to_string(), Value::int(1)),
        DispatchShape::Capture => {
            Value::capture(vec![Value::int(1)], crate::value::ValueMap::default())
        }
        DispatchShape::Version => Value::version_from_str("1.2.3"),
        DispatchShape::Uni => Value::uni("NFC".to_string(), "ab".to_string()),
        DispatchShape::Set => Value::set(["a".to_string()].into_iter().collect()),
        DispatchShape::SetHash => Value::set_hash(["a".to_string()].into_iter().collect()),
        DispatchShape::Bag => Value::bag([("a".to_string(), 2)].into_iter().collect()),
        DispatchShape::BagHash => Value::bag_hash([("a".to_string(), 2)].into_iter().collect()),
        DispatchShape::Mix => Value::mix([("a".to_string(), 1.5)].into_iter().collect()),
        DispatchShape::MixHash => Value::mix_hash([("a".to_string(), 1.5)].into_iter().collect()),
        DispatchShape::Date => crate::builtins::methods_0arg::temporal::make_date(2024, 3, 5),
        DispatchShape::DateTime => {
            crate::builtins::methods_0arg::temporal::make_datetime(2024, 3, 5, 7, 8, 9.0, 0)
        }
        DispatchShape::Instant => {
            crate::builtins::method_table::instances_sample("Instant", Value::int(1_000_000_010))
        }
        DispatchShape::Duration => crate::builtins::method_table::instances_sample(
            "Duration",
            crate::value::make_rat(15, 2),
        ),
        DispatchShape::Match => Value::make_match_object_full(
            3,
            5,
            &[],
            &Default::default(),
            crate::value::regex_caps::MatchTarget::new("xxxab"),
        ),
        DispatchShape::IoSpecUnix => type_sample("IO::Spec::Unix"),
        DispatchShape::IoSpecWin32 => type_sample("IO::Spec::Win32"),
        DispatchShape::IoSpecCygwin => type_sample("IO::Spec::Cygwin"),
        DispatchShape::IoSpecQnx => type_sample("IO::Spec::QNX"),
        DispatchShape::IoHandle => {
            let mut attributes = crate::value::AttrMap::new();
            attributes.insert("handle".to_string(), Value::int(-1));
            Value::make_instance_without_destroy(Symbol::intern("IO::Handle"), attributes)
        }
        DispatchShape::IoPath => {
            let mut attributes = crate::value::AttrMap::new();
            attributes.insert("path".to_string(), Value::str_from("foo/bar"));
            Value::make_instance(Symbol::intern("IO::Path"), attributes)
        }
        DispatchShape::Seq => Value::seq(vec![Value::int(1), Value::int(2)]),
        DispatchShape::Blob => crate::value::value_buf::make_buf(
            Symbol::intern("Blob"),
            vec![Value::int(1), Value::int(2)],
        ),
        DispatchShape::Buf => crate::value::value_buf::make_buf(
            Symbol::intern("Buf"),
            vec![Value::int(1), Value::int(2)],
        ),
        DispatchShape::BacktraceFrame => backtrace_frame_sample(),
        DispatchShape::Backtrace => {
            let mut attributes = crate::value::AttrMap::new();
            attributes.insert(
                "frames".to_string(),
                Value::array(vec![backtrace_frame_sample()]),
            );
            attributes.insert("text".to_string(), Value::str_from("  in block <unit>\n"));
            Value::make_instance(Symbol::intern("Backtrace"), attributes)
        }
    }
}

/// A `Backtrace::Frame` instance, as the backtrace builder makes one.
fn backtrace_frame_sample() -> Value {
    let mut attributes = crate::value::AttrMap::new();
    attributes.insert("subname".to_string(), Value::str_from("<unit>"));
    attributes.insert("file".to_string(), Value::str_from("x.raku"));
    attributes.insert("line".to_string(), Value::int(1));
    Value::make_instance(Symbol::intern("Backtrace::Frame"), attributes)
}

/// The owner of the row an instance of `shape` dispatches `method` to, or
/// `"-"` when there is none.
fn owner_of(shape: DispatchShape, method: Symbol, arity: u8) -> &'static str {
    lookup(shape, method, arity).map_or("-", |row| row.owner)
}

/// The row an instance of `shape` dispatches `method` to.
fn lookup(shape: DispatchShape, method: Symbol, arity: u8) -> Option<&'static MethodRow> {
    table::lookup(Receiver::instance(shape), method, arity)
}

fn rows() -> impl Iterator<Item = &'static MethodRow> {
    all_rows()
}

#[test]
fn samples_have_their_shape() {
    for shape in DispatchShape::ALL {
        if shape == DispatchShape::Seq {
            // Only the entries after the consumption step decode a `Seq`.
            assert_eq!(sample(shape).dispatch_shape(), None);
            assert_eq!(sample(shape).settled_seq_shape(), Some(shape));
        } else if shape.has_instances() {
            assert_eq!(sample(shape).dispatch_shape(), Some(shape));
        } else {
            assert_eq!(sample(shape).type_object_shape(), Some(shape));
        }
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
        // A row that is reached only through its owner (`OWNER_ONLY`, or a
        // `Mut` row, whose one entry is `invoke_mut`) has no shape to find it.
        if row.flags.contains(RowFlags::OWNER_ONLY) || row.handler.is_mut() {
            continue;
        }
        let mut reached = false;
        for shape in DispatchShape::ALL {
            let Some(found) = table::lookup(
                receiver_of_sample(shape),
                Symbol::intern(row.name),
                row.arity,
            ) else {
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
    assert_eq!(owner_of(DispatchShape::Array, elems, 0), "List");
    assert_eq!(owner_of(DispatchShape::List, elems, 0), "List");
    assert_eq!(owner_of(DispatchShape::Hash, elems, 0), "Map");
    assert_eq!(owner_of(DispatchShape::Str, elems, 0), "Any");
    let numerator = Symbol::intern("numerator");
    assert!(lookup(DispatchShape::Rat, numerator, 0).is_some());
    assert!(lookup(DispatchShape::Num, numerator, 0).is_none());
    // Rakudo's `Int` does not do `Rational`: `5.numerator` is no method.
    assert!(lookup(DispatchShape::Int, numerator, 0).is_none());
    assert_eq!(owner_of(DispatchShape::FatRat, numerator, 0), "FatRat");
}

#[test]
fn a_call_with_another_arity_takes_the_cascades() {
    let target = sample(DispatchShape::List);
    assert!(try_dispatch(&target, Symbol::intern("elems"), &[Value::int(1)]).is_none());
}

/// The shape probe must refuse everything the receiver-state checks exist for.
#[test]
fn dispatch_shape_refuses_non_plain_receivers() {
    let user_date = Value::make_instance(Symbol::intern("MyDate"), crate::value::AttrMap::new());
    for (what, value) in [
        ("a type object", Value::package(Symbol::intern("Any"))),
        ("a Seq", Value::seq(vec![Value::int(1)])),
        ("Nil", Value::NIL),
        ("a user subclass instance", user_date),
    ] {
        assert_eq!(
            value.dispatch_shape(),
            None,
            "{what} must not take the table"
        );
    }
}

/// A built-in class's own instance has a shape; a subclass of it, with
/// another class name, has none.
#[test]
fn an_instance_has_a_shape_only_for_the_built_in_class() {
    let date = Value::make_instance(Symbol::intern("Date"), crate::value::AttrMap::new());
    assert_eq!(date.dispatch_shape(), Some(DispatchShape::Date));
    let sub = Value::make_instance(Symbol::intern("Date::Sub"), crate::value::AttrMap::new());
    assert_eq!(sub.dispatch_shape(), None);
}

/// A shape added after the first nine is closed: only rows its own type owns
/// reach it, because an ancestor's row (`Any.elems`) was written for the
/// shapes that existed.
#[test]
fn a_new_shape_reaches_only_its_own_rows() {
    let elems = Symbol::intern("elems");
    assert!(lookup(DispatchShape::Str, elems, 0).is_some());
    for shape in DispatchShape::ALL {
        if shape.inherits() {
            continue;
        }
        // A closed shape may have an `elems` row of its own (`Set.elems`),
        // never `Any`'s.
        assert!(
            lookup(shape, elems, 0).is_none_or(|row| row.owner == shape.type_name()),
            "{shape:?} is closed but reached Any.elems"
        );
    }
    for row in rows() {
        for shape in DispatchShape::ALL {
            let Some(found) = lookup(shape, Symbol::intern(row.name), row.arity) else {
                continue;
            };
            assert!(
                shape.reaches(found.owner, row.name),
                "{shape:?} reached a row its type does not own"
            );
        }
    }
}

/// A type object is answered only by a row that says so, and only for a
/// built-in type.
#[test]
fn a_type_object_answers_only_flagged_rows() {
    let int = Value::package(Symbol::intern("Int"));
    assert_eq!(int.dispatch_shape(), None);
    assert_eq!(int.type_object_shape(), Some(DispatchShape::Int));
    // `Int.Bool` is `False`, answered by a TYPE_OBJECT_OK row.
    assert!(matches!(
        try_dispatch(&int, Symbol::intern("Bool"), &[]),
        Some(Ok(v)) if v == Value::FALSE
    ));
    // `Int.abs` has a row, but not for a type object.
    assert!(try_dispatch(&int, Symbol::intern("abs"), &[]).is_none());
    // A user class's type object has no shape.
    let user = Value::package(Symbol::intern("MyClass"));
    assert_eq!(user.type_object_shape(), None);
    assert!(try_dispatch(&user, Symbol::intern("Bool"), &[]).is_none());
    // The receiver of a type object and of an instance differ in the memo byte.
    let instance = Receiver::instance(DispatchShape::Int);
    let type_object = Receiver::type_object(DispatchShape::Int);
    assert_ne!(instance.to_bits(), type_object.to_bits());
}

/// Every receiver packs into its own byte, so the call-site memo can tell
/// them apart.
#[test]
fn receivers_pack_into_distinct_bytes() {
    let mut seen = std::collections::HashSet::new();
    for shape in DispatchShape::ALL {
        for receiver in [Receiver::instance(shape), Receiver::type_object(shape)] {
            assert!(seen.insert(receiver.to_bits()), "{receiver:?} collides");
        }
    }
}

/// A row's owner is the type Rakudo declares the method on, so `.^can`
/// (which reads the same owner from the recognition catalog) agrees with
/// where the table finds it. The catalog folds some owners (`FatRat` into
/// `Rat`, ADR-11276 §8), so a row is checked under its folded owner.
#[test]
fn rows_are_declared_by_rakudo() {
    let undeclared: Vec<String> = rows()
        // `DESTROY` is a submethod: Rakudo's `.^method_table` does not list it.
        .filter(|row| row.name != "DESTROY")
        .filter(|row| {
            let owner =
                match crate::builtins::builtin_type_methods::canonical_builtin_owner(row.owner) {
                    "" => row.owner,
                    folded => folded,
                };
            !crate::builtins::native_method_row::native_method_declared(owner, row.name)
        })
        .map(|row| format!("{}.{}", row.owner, row.name))
        .collect();
    assert!(
        undeclared.is_empty(),
        "rows the catalog does not record Rakudo declaring: {undeclared:?}"
    );
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
        DispatchShape::ALL.len() <= 64,
        "a shape bit must fit Table::shapes' u64"
    );
    let int = Symbol::intern("Int");
    assert!(shape_has_row(Receiver::instance(DispatchShape::Int), int));
    assert!(shape_has_row(Receiver::instance(DispatchShape::Rat), int));
    assert!(!shape_has_row(Receiver::instance(DispatchShape::Str), int));
    // An inherited row sets the bit of the shape that reaches it.
    assert!(shape_has_row(
        Receiver::instance(DispatchShape::Array),
        Symbol::intern("elems")
    ));
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
    let row = resolve(Receiver::instance(DispatchShape::List), collate, 0);
    assert!(
        row.is_some_and(|id| !super::row(id).handler.is_pure()),
        "Any.collate resolves for a List, to an interpreter row"
    );
}

/// Not a check: prints every registered row as a tab-separated line
/// (`ROW owner name arity handler flags named`) for
/// `scripts/method-rows-report.py`, which runs it with `--ignored --nocapture`.
#[test]
#[ignore = "prints the registered rows for scripts/method-rows-report.py"]
fn dump_rows() {
    for row in rows() {
        let kind = match row.handler {
            Handler::Pure(_) => "Pure",
            Handler::Narrow(_) => "Narrow",
            Handler::Named(_) => "Named",
            Handler::Interp(_) => "Interp",
            Handler::Mut(_) => "Mut",
        };
        let mut flags = Vec::new();
        if row.flags.contains(RowFlags::TYPE_OBJECT_OK) {
            flags.push("TYPE_OBJECT_OK");
        }
        if row.flags.contains(RowFlags::ANY_ARGS) {
            flags.push("ANY_ARGS");
        }
        println!(
            "ROW\t{}\t{}\t{}\t{}\t{}\t{}",
            row.owner,
            row.name,
            row.arity,
            kind,
            flags.join(","),
            row.named.join(",")
        );
    }
}
