//! `Any`'s `push`, `append`, `unshift` and `prepend`, and `Range`'s six
//! mutators (ADR-11276 §9.164).
//!
//! Rakudo declares the four growing methods on `Any` for an undefined
//! invocant only (`multi method push(Any:U \SELF: |values)`): the container
//! autovivifies to an `Array`, which then grows as `Array`'s row does. A
//! defined invocant that declares no mutator of its own (`5`, `"s"`) matches
//! no candidate (`X::Multi::NoMatch`); `pop` and `shift` are not declared on
//! `Any` at all, so they are `X::Method::NotFound` without a row.
//!
//! `Range` declares the six mutators to refuse them (`X::Immutable`); it has no
//! `splice`, which `Any`'s proto answers with no candidate.
//!
//! A named binding is autovivified here (`Interpreter::vivify_array_binding`,
//! §9.171) and grows through `Array`'s row as the binding it now is. An
//! undefined receiver with no binding (the value of an expression, as an
//! out-of-range element read through the interpreter) answers the fresh
//! `Array`, which the caller stores where the value came from.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::methods_signature_errors::{make_multi_no_match_error, make_x_immutable_error};
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;
type Grow = fn(&mut Interpreter, &mut ReceiverPlace<'_>, &[Value], Named<'_>) -> Answer;

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
    row!("Any", "push", any_push),
    row!("Any", "append", any_append),
    row!("Any", "unshift", any_unshift),
    row!("Any", "prepend", any_prepend),
    MethodRow {
        owner: "Any",
        name: "ASSIGN-KEY",
        arity: 2,
        handler: Handler::Mut(any_assign_key),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "BIND-KEY",
        arity: 2,
        handler: Handler::Mut(any_bind_key),
        flags: RowFlags::NONE,
        named: &[],
    },
    MethodRow {
        owner: "Any",
        name: "DELETE-KEY",
        arity: 1,
        handler: Handler::Mut(any_delete_key),
        flags: RowFlags::NONE,
        named: &[],
    },
    row!("Range", "push", range_push),
    row!("Range", "append", range_append),
    row!("Range", "unshift", range_unshift),
    row!("Range", "prepend", range_prepend),
    row!("Range", "pop", range_pop),
    row!("Range", "shift", range_shift),
];

macro_rules! any_grow {
    ($fn:ident, $name:literal, $array_row:path) => {
        // Cost: as `Array`'s row, O(k) for k elements.
        fn $fn(
            interp: &mut Interpreter,
            place: &mut ReceiverPlace<'_>,
            args: &[Value],
            named: Named<'_>,
        ) -> Answer {
            grow(interp, place, $name, args, named, $array_row)
        }
    };
}

any_grow!(any_push, "push", super::array::push_row);
any_grow!(any_append, "append", super::array::append_row);
any_grow!(any_unshift, "unshift", super::array::unshift_row);
any_grow!(any_prepend, "prepend", super::array::prepend_row);

/// `Any.push` and its kin: autovivify an undefined receiver to an `Array` and
/// grow it through `Array`'s row; refuse a defined scalar.
// Cost: as `Array`'s row, O(k) for k elements, plus one env write to vivify a
// binding.
fn grow(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    method: &str,
    args: &[Value],
    named: Named<'_>,
    array_row: Grow,
) -> Answer {
    let value = place.value().descalarize();
    // An `Array` type object in a named binding (`my Array $a; $a.push(1)`)
    // vivifies as an undefined `Any` does.
    let array_type = matches!(value.view(), ValueView::Package(name) if name == "Array");
    // A role's type object is not an `Any` (`my Positional $p; $p.push(1)`
    // matches no `Any:U` candidate), so it is not vivified.
    let role_type = matches!(value.view(), ValueView::Package(name)
        if interp.is_role_type_name(&name.resolve()));
    match value.view() {
        ValueView::Nil => {}
        ValueView::Package(name) if name == "Any" || name == "Mu" => {}
        // Any other type object a named binding holds (`my Int $x`, `my $x =
        // Foo`) is an undefined invocant too: the binding vivifies through
        // an ordinary assignment, whose type check refuses an `Array` into
        // `my Int $x` (`X::TypeCheck::Assignment`), as Rakudo's does (#13823).
        ValueView::Package(_) if place.name().is_some() && !role_type => {}
        ValueView::Int(_)
        | ValueView::Num(_)
        | ValueView::Str(_)
        | ValueView::Bool(_)
        | ValueView::Rat(..)
        | ValueView::Complex(..) => return Some(Err(make_multi_no_match_error(method))),
        _ => return None,
    }
    if let Some(name) = place.name() {
        let name = name.to_string();
        if let Some(type_name) = containerless_type_object(interp, &name, value) {
            // A bare `Array` type object has no container to vivify: Rakudo's
            // vivification calls `STORE` on `Array:U`, which no candidate binds
            // (#13888). Any other type object is read-only.
            if array_type {
                return Some(Err(
                    crate::runtime::methods_signature_errors::make_multi_no_match_error_detailed(
                        "STORE",
                        "Array",
                        false,
                        "Array:D",
                        &[
                            "(Array:D $:: Iterable:D \\iterable, *%_ --> Array:D)".to_string(),
                            "(Array:D $:: QuantHash:D \\qh, *%_ --> Array:D)".to_string(),
                            "(Array:D $:: Mu \\item, *%_ --> Array:D)".to_string(),
                        ],
                    ),
                ));
            }
            return Some(Err(RuntimeError::assignment_ro_type_object(&type_name)));
        }
        // The vivification is an assignment to the binding, so it obeys the
        // binding's type: `my Int $x; $x.push(3)` refuses the `Array`.
        let fresh = Value::real_array(Vec::new());
        if !name.starts_with(['@', '%', '&'])
            && let Some(constraint) = interp.binding_store_constraint(place.code(), &name)
            && !interp.type_matches_value(&constraint, &fresh)
        {
            return Some(Err(interp.type_check_assignment_failure(
                &name,
                &constraint,
                &fresh,
            )));
        }
        let vivified = match interp.vivify_binding(&name, fresh) {
            Ok(vivified) => vivified,
            Err(err) => return Some(Err(err)),
        };
        let mut array = place.rebound(&vivified)?;
        return array_row(interp, &mut array, args, named);
    }
    let mut fresh = Value::real_array(Vec::new());
    let mut array = ReceiverPlace::detached(&mut fresh);
    match array_row(interp, &mut array, args, named)? {
        Ok(_) => Some(Ok(fresh)),
        Err(err) => Some(Err(err)),
    }
}

/// The type a name holds when it is bound straight to a type object, with no
/// container to vivify: a sigilless name (`my \s = Any`), the bare type word
/// of `Hash.ASSIGN-KEY(...)`, or a sigiled one bound to it (`my $x := Any`). A parameter or `for` alias is read-only for the
/// assignment's own reason, which `vivify_binding` raises.
// Cost: O(1), a tag probe, one env probe and one readonly probe.
fn containerless_type_object(interp: &Interpreter, name: &str, value: &Value) -> Option<String> {
    let ValueView::Package(type_name) = value.descalarize().view() else {
        return None;
    };
    let type_name = type_name.resolve();
    // A sigilless binding is stored under its term key; a bare type word
    // (`Hash.ASSIGN-KEY(...)`) is its own spelling and names no binding. A `$`
    // scalar arrives without its sigil, so neither test can mistake it.
    let sigilless = name.starts_with(crate::term_names::TERM_PREFIX);
    let bare_type_word = name == type_name && interp.env().get(name).is_none();
    (sigilless
        || bare_type_word
        || interp
            .readonly_kind(name)
            .is_some_and(|kind| kind != crate::ast::ReadonlyKind::Alias))
    .then_some(type_name)
}

/// Whether `value` is an undefined receiver `Any`'s keyed rows answer: `Nil`
/// or the `Any`, `Mu` or `Hash` type object a container holds before its
/// first write. Another type object (a typed scalar's, a user class's) is not
/// one (#13823).
// Cost: O(1), a tag probe and a short name compare.
fn undefined_hash_receiver(value: &Value) -> bool {
    match value.descalarize().view() {
        ValueView::Nil => true,
        ValueView::Package(name) => name == "Any" || name == "Mu" || name == "Hash",
        _ => false,
    }
}

/// `Any.ASSIGN-KEY` on an undefined binding: vivify it to a
/// `Hash` and store through `Hash`'s row (`multi method ASSIGN-KEY(Any:U \SELF:
/// \key, Mu \assignee)`). A receiver with no binding has nowhere to keep the
/// hash and takes the cascade.
// Cost: as `Hash`'s row, O(1) expected, plus one env write.
fn vivify_hash_then(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    named: Named<'_>,
    hash_row: Grow,
) -> Answer {
    if !undefined_hash_receiver(place.value()) {
        return None;
    }
    let name = place.name()?.to_string();
    if let Some(type_name) = containerless_type_object(interp, &name, place.value()) {
        return Some(Err(RuntimeError::assign_requires_concrete_object(
            &type_name,
        )));
    }
    let vivified =
        match interp.vivify_binding(&name, Value::hash(crate::value::ValueMap::default())) {
            Ok(vivified) => vivified,
            Err(err) => return Some(Err(err)),
        };
    let mut hash = place.rebound(&vivified)?;
    hash_row(interp, &mut hash, args, named)
}

// Cost: see `vivify_hash_then`.
fn any_assign_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    named: Named<'_>,
) -> Answer {
    vivify_hash_then(
        interp,
        place,
        args,
        named,
        super::subscript::hash_assign_key,
    )
}

/// `Any.BIND-KEY` on an undefined binding: vivify it to a `Hash` and bind the
/// argument's value under the key.
// Cost: as `Hash`'s row, O(1) expected, plus one env write.
fn any_bind_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    named: Named<'_>,
) -> Answer {
    if !undefined_hash_receiver(place.value()) {
        return None;
    }
    let name = place.name()?.to_string();
    // A type object with no container has no `BIND-KEY` candidate at all.
    if let Some(type_name) = containerless_type_object(interp, &name, place.value()) {
        let profile = interp.format_call_arg_profile(args);
        return Some(Err(
            crate::runtime::methods_signature_errors::make_multi_no_match_error_detailed(
                "BIND-KEY",
                &type_name,
                false,
                &profile,
                &[],
            ),
        ));
    }
    let vivified =
        match interp.vivify_binding(&name, Value::hash(crate::value::ValueMap::default())) {
            Ok(vivified) => vivified,
            Err(err) => return Some(Err(err)),
        };
    // `Any.BIND-KEY(Any:U \SELF: $key, $BIND)` takes the value, not the
    // caller's variable, so the hash binds what the argument holds now.
    let mut hash = place.rebound(&vivified)?.with_arg_sources(&[]);
    super::subscript_bind::hash_bind_key(interp, &mut hash, args, named)
}

/// `Any.DELETE-KEY` on an undefined receiver: there is nothing to delete, and
/// the receiver stays undefined (`multi method DELETE-KEY(Any:U: \key) { Nil }`).
// Cost: O(1).
fn any_delete_key(
    _interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    _args: &[Value],
    _named: Named<'_>,
) -> Answer {
    undefined_hash_receiver(place.value()).then_some(Ok(Value::NIL))
}

macro_rules! range_refusal {
    ($fn:ident, $name:literal) => {
        // Cost: O(1).
        fn $fn(
            _interp: &mut Interpreter,
            _place: &mut ReceiverPlace<'_>,
            _args: &[Value],
            _named: Named<'_>,
        ) -> Answer {
            Some(Err(make_x_immutable_error($name, "Range")))
        }
    };
}

range_refusal!(range_push, "push");
range_refusal!(range_append, "append");
range_refusal!(range_unshift, "unshift");
range_refusal!(range_prepend, "prepend");
range_refusal!(range_pop, "pop");
range_refusal!(range_shift, "shift");
