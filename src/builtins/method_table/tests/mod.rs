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

/// Language revision 6.e for the test thread while it lives, so a
/// `REVISION_E` row is in force (ADR-11276 §9.146). The revision is
/// thread-local; the previous one comes back on drop.
pub(super) struct RevisionE(String);

impl RevisionE {
    pub(super) fn enter() -> RevisionE {
        let previous = crate::parser::current_language_version();
        crate::parser::set_current_language_version("6.e");
        RevisionE(previous)
    }
}

impl Drop for RevisionE {
    fn drop(&mut self) {
        crate::parser::set_current_language_version(&self.0);
    }
}

/// The type object of a shape that has no instances.
/// A bare instance of a built-in class: every attribute reads as unset.
fn instance_sample(class: &str) -> Value {
    Value::make_instance(Symbol::intern(class), crate::value::AttrMap::new())
}

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
        DispatchShape::Unicode => type_sample("Unicode"),
        DispatchShape::CompUnitDepSpec => {
            let mut attributes = crate::value::AttrMap::new();
            attributes.insert("short-name".to_string(), Value::str_from("Foo"));
            Value::make_instance(
                Symbol::intern("CompUnit::DependencySpecification"),
                attributes,
            )
        }
        DispatchShape::IoPathParts => {
            let mut attributes = crate::value::AttrMap::new();
            attributes.insert("volume".to_string(), Value::str_from(""));
            attributes.insert("dirname".to_string(), Value::str_from("/a"));
            attributes.insert("basename".to_string(), Value::str_from("b.txt"));
            Value::make_instance(Symbol::intern("IO::Path::Parts"), attributes)
        }
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
        DispatchShape::Format => crate::runtime::Interpreter::make_format("%d-%s".to_string()),
        DispatchShape::Formatter => type_sample("Formatter"),
        DispatchShape::RakudoInternals => type_sample("Rakudo::Internals"),
        DispatchShape::RakudoSorting => type_sample("Rakudo::Sorting"),
        DispatchShape::BacktraceFrame => backtrace_frame_sample(),
        DispatchShape::Attribute => {
            // `DEPRECATED` answers only an attribute the trait marked.
            let mut attributes = crate::value::AttrMap::new();
            attributes.insert("DEPRECATED".to_string(), Value::str_from("use y"));
            Value::make_instance(Symbol::intern("Attribute"), attributes)
        }
        DispatchShape::Variable => instance_sample("Variable"),
        DispatchShape::Promise => Value::promise(crate::value::SharedPromise::new()),
        DispatchShape::Channel => Value::channel(crate::value::SharedChannel::new()),
        DispatchShape::HyperSeq => Value::hyper_seq(vec![Value::int(1)]),
        DispatchShape::RaceSeq => Value::race_seq(vec![Value::int(1)]),
        DispatchShape::Collation => {
            crate::runtime::Interpreter::make_collation_instance(1, 1, 1, 1)
        }
        DispatchShape::Label => crate::value::label::make_label("FOO", "x.raku", 3, "", ""),
        DispatchShape::Backtrace => {
            let mut attributes = crate::value::AttrMap::new();
            attributes.insert(
                "frames".to_string(),
                Value::array(vec![backtrace_frame_sample()]),
            );
            attributes.insert("text".to_string(), Value::str_from("  in block <unit>\n"));
            Value::make_instance(Symbol::intern("Backtrace"), attributes)
        }
        // An opaque shape's handlers read the runtime state its attributes
        // name; a bare instance has none, and its rows decline or refuse it.
        shape => {
            assert!(shape.is_opaque(), "{shape:?} needs a sample");
            instance_sample(shape.type_name())
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
        } else if shape.is_opaque() {
            // Only the table's receiver decode sees an opaque shape.
            assert_eq!(sample(shape).dispatch_shape(), None);
            assert_eq!(sample(shape).receiver_shape(), Some(shape));
        } else if shape.has_instances() {
            assert_eq!(sample(shape).dispatch_shape(), Some(shape));
        } else {
            assert_eq!(sample(shape).type_object_shape(), Some(shape));
        }
    }
}

/// A `REVISION_E` row is found by its owner only: the shape tables are static,
/// so a row in them could not be skipped under an older language revision
/// (ADR-11276 §9.146).
#[test]
fn revision_e_rows_are_owner_only() {
    for row in rows() {
        assert!(
            !row.flags.contains(RowFlags::REVISION_E) || row.flags.contains(RowFlags::OWNER_ONLY),
            "{}.{} is REVISION_E but not OWNER_ONLY",
            row.owner,
            row.name
        );
    }
}

#[test]
fn no_row_is_registered_twice() {
    // The one exception is a row's late twin (ADR-11276 §9.135, §9.138): an
    // `OWNER_ONLY` `Handler::Interp` row registered after the first row of
    // the same owner, name and arity, which only the MRO walks reach. The
    // first row is not itself such a row.
    let mut seen = std::collections::HashSet::new();
    let mut twins = std::collections::HashSet::new();
    for row in rows() {
        let key = (row.owner, row.name, row.arity);
        if seen.insert(key) {
            continue;
        }
        let distinct_first = rows()
            .find(|first| (first.owner, first.name, first.arity) == key)
            .is_some_and(|first| {
                // A deferral base is reached by `invoke_base` only, never by a
                // walk, so a late row may stand behind it too.
                first.flags.contains(RowFlags::DEFERRAL_BASE)
                    || !(matches!(first.handler, Handler::Interp(_))
                        && first.flags.contains(RowFlags::OWNER_ONLY))
            });
        assert!(
            distinct_first
                && matches!(row.handler, Handler::Interp(_))
                && row.flags.contains(RowFlags::OWNER_ONLY)
                && twins.insert(key),
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
            // A receiver the receiver step redirects for this name (a Hash's
            // `self.list` methods read its pairs) is never asked of the row as
            // it stands: the table's entry declines it (`receiver_step::claims`).
            if super::receiver_step::claims(&target, row.name) {
                continue;
            }
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
            // An opaque shape's sample carries none of the runtime state its
            // rows read (a lock id, a configuration), so they may decline it.
            if shape.is_opaque() {
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
/// reach it, because an ancestor's row (`Any.nl-out`) was written for the
/// shapes that existed. (`Any.elems` reads any receiver as its list, so it is
/// audited for every shape, §9.163.)
#[test]
fn a_new_shape_reaches_only_its_own_rows() {
    let nl_out = Symbol::intern("nl-out");
    assert!(lookup(DispatchShape::Str, nl_out, 0).is_some());
    for shape in DispatchShape::ALL {
        if shape.inherits() {
            continue;
        }
        // A closed shape may have an `nl-out` row of its own
        // (`IO::Handle.nl-out`), never `Any`'s.
        assert!(
            lookup(shape, nl_out, 0).is_none_or(|row| row.owner != "Any"),
            "{shape:?} is closed but reached Any.nl-out"
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
    // `Int.abs` has a row, but not for a type object: its invocant is `:D`,
    // so the row refuses one (`RowFlags::DEFINITE`).
    assert!(matches!(
        try_dispatch(&int, Symbol::intern("abs"), &[]),
        Some(Err(e)) if e.message.contains("must be an object instance")
    ));
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
/// `Rat`, ADR-11276 §8), so a row is checked under its own owner or, failing
/// that, its folded one (`Routine` folds into `Code`, but its rows are
/// recorded under `Routine`, ADR-11276 §9.121).
#[test]
fn rows_are_declared_by_rakudo() {
    // A `REVISION_E` row is checked against what Rakudo declares under 6.e.
    let _revision = RevisionE::enter();
    let undeclared: Vec<String> = rows()
        // Rakudo declares no `raku` on `Label`; the rendering is mutsu's own.
        .filter(|row| !(row.owner == "Label" && row.name == "raku"))
        // `IS-MACOS` is the companion mutsu gives `IS-WIN`; Rakudo declares none.
        .filter(|row| !(row.owner == "Rakudo::Internals" && row.name == "IS-MACOS"))
        // `DESTROY` is a submethod: Rakudo's `.^method_table` does not list it.
        .filter(|row| row.name != "DESTROY")
        // Roast pins `NFC.chars`/`NFC.comb` (`S15-string-types/NF-types.t`),
        // which current Rakudo no longer declares (ADR-11276 §9.116).
        .filter(|row| !(row.owner == "Uni" && matches!(row.name, "chars" | "comb")))
        // Roast pins `VM.properties` (`S02-magicals/VM.t`), which Rakudo does
        // not declare (RT #124624, ADR-11276 §9.155).
        .filter(|row| !(row.owner == "VM" && row.name == "properties"))
        // Roast pins `IO::Socket::INET.getpeername` (`S16-unfiled/getpeername.t`),
        // which Rakudo does not declare (ADR-11276 §9.158).
        .filter(|row| !(row.owner == "IO::Socket::INET" && row.name == "getpeername"))
        // Rakudo renders an `IO::Handle` through `Mu.raku`'s attribute dump;
        // mutsu's handle keeps its state elsewhere, so the class renders itself
        // (ADR-11276 §9.162).
        .filter(|row| !(row.owner == "IO::Handle" && row.name == "raku"))
        // Roast pins `$!.pending` (`S04-exceptions/pending.t`), which Rakudo
        // does not declare (ADR-11276 §9.161).
        .filter(|row| !(row.owner == "Any" && row.name == "pending"))
        // Rakudo's `Failure` answers `rethrow`, `die` and `fail` through its
        // `FALLBACK`, which throws the exception it carries (ADR-11276 §9.163).
        .filter(|row| {
            !(row.owner == "Failure" && matches!(row.name, "rethrow" | "die" | "fail" | "throw"))
        })
        // Rakudo's quant hashes inherit `Any.BIND-KEY`, which refuses them with
        // `X::Bind`; mutsu's receiver-mutating rows are found by the receiver's
        // own kind, so the refusal is a row of each (ADR-11276 §9.41).
        .filter(|row| {
            !(row.name == "BIND-KEY"
                && matches!(
                    row.owner,
                    "Set" | "SetHash" | "Bag" | "BagHash" | "Mix" | "MixHash"
                ))
        })
        // `is DEPRECATED` mixes a role declaring `DEPRECATED` into the
        // attribute; mutsu answers it on `Attribute` (ADR-11276 §9.161).
        .filter(|row| !(row.owner == "Attribute" && row.name == "DEPRECATED"))
        // A method of language revision 6.e is checked against what 6.e adds
        // (`declared.e`), which the catalog need not record for a name 6.d
        // already recognizes on the owner (`Complex.sign`).
        .filter(|row| {
            !(row.flags.contains(RowFlags::REVISION_E)
                && crate::builtins::method_introspection::declared_in_revision_e(
                    row.owner, row.name,
                ))
        })
        .filter(|row| {
            let owner =
                match crate::builtins::builtin_type_methods::canonical_builtin_owner(row.owner) {
                    "" => row.owner,
                    folded => folded,
                };
            !crate::builtins::method_introspection::native_method_declared(row.owner, row.name)
                && !crate::builtins::method_introspection::native_method_declared(owner, row.name)
        })
        .map(|row| format!("{}.{}", row.owner, row.name))
        .collect();
    assert!(
        undeclared.is_empty(),
        "rows Rakudo's method-table snapshot does not declare: {undeclared:?}"
    );
}

/// The name test carries the arity: a call with an argument count no row of
/// that name takes is refused before any lookup.
#[test]
fn the_name_test_knows_the_arity() {
    let samecase = Symbol::intern("samecase");
    assert!(names_a_row(samecase, 1));
    assert!(!names_a_row(samecase, 2));
    let substr = Symbol::intern("substr");
    assert!(names_a_row(substr, 1) && names_a_row(substr, 2));
    // (`elems` takes an argument on a `Supply`, whose row is slurpy.)
    assert!(!names_a_row(Symbol::intern("chars"), 1));
    assert!(!names_a_row(substr, 200));
}

/// The shape test: a name with rows for other receivers is refused for this
/// one before any lookup.
#[test]
fn the_shape_test_knows_the_receiver() {
    assert!(
        DispatchShape::ALL.len() <= 127,
        "a shape bit must fit Table::shapes' u128 and the receiver byte"
    );
    let polymod = Symbol::intern("polymod");
    assert!(shape_has_row(
        Receiver::instance(DispatchShape::Int),
        polymod
    ));
    assert!(shape_has_row(
        Receiver::instance(DispatchShape::Rat),
        polymod
    ));
    assert!(!shape_has_row(
        Receiver::instance(DispatchShape::Str),
        polymod
    ));
    // An inherited row sets the bit of the shape that reaches it.
    assert!(shape_has_row(
        Receiver::instance(DispatchShape::Array),
        Symbol::intern("elems")
    ));
}

/// A named argument is split from the positionals and handed to the row that
/// declares it. A false `:hammer`, which the row's signature does not bind,
/// takes the cascades; a name no row of the method binds is swallowed.
#[test]
fn a_named_argument_reaches_only_a_row_that_declares_it() {
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
    // `:hammer(False)` is the row's, and its handler leaves it to the cascades.
    let unhammered = Value::pair("hammer".to_string(), Value::FALSE);
    assert!(try_dispatch(&nested, flat, &[unhammered]).is_none());
    // A name no row of the method binds is swallowed by the implicit `*%_`:
    // the row answers as if it were not passed (`flat(:zzz)`, `elems(:hammer)`).
    let render = |result: Option<Result<Value, crate::value::RuntimeError>>| match result {
        Some(Ok(value)) => value.to_string_value(),
        _ => panic!("the row must answer"),
    };
    let zzz = Value::pair("zzz".to_string(), Value::TRUE);
    assert_eq!(
        render(try_dispatch(&nested, flat, &[zzz])),
        render(try_dispatch(&nested, flat, &[]))
    );
    let elems = Symbol::intern("elems");
    let hammer = Value::pair("hammer".to_string(), Value::TRUE);
    assert_eq!(
        render(try_dispatch(&nested, elems, &[hammer])),
        render(try_dispatch(&nested, elems, &[]))
    );
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
    // `Str.samecase` has no ANY_ARGS: a Range is not a plain scalar.
    let text = sample(DispatchShape::Str);
    assert!(try_dispatch(&text, Symbol::intern("samecase"), &[Value::range(1, 2)]).is_none());
    assert!(matches!(
        try_dispatch(&text, Symbol::intern("samecase"), &[Value::str_from("b")]),
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
        if row.flags.contains(RowFlags::ANY_NAMED) {
            flags.push("ANY_NAMED");
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
