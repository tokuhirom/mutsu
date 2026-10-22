//! Late rows of `Mu`, `Any` and `Array` that the by-name cascade
//! `dispatch_method_by_name_1` answered with arms (ADR-11276 §9.136): the
//! zero-argument `say`/`print`/`put`/`note` that print the invocant,
//! `return-rw`, `can`, `does`, `are` and `Array.shape`; and the twins of
//! `Any.nl-out` and `pending` (ADR-11276 §9.161).
//!
//! Each is `OWNER_ONLY`: only the MRO walk (`any_list_method_by_mro`) reaches
//! it, after every receiver-specific answer (an `IO::Handle`'s or an
//! `IO::CatHandle`'s own `say` is its owner's row, ahead of `Mu` in its MRO).
//! The handlers are the routines the deleted arms called.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const LATE: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::TYPE_OBJECT_OK);

macro_rules! late {
    ($owner:literal, $name:literal, $arity:literal, $flags:expr, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    late!("Mu", "say", 0, LATE, say),
    late!("Mu", "print", 0, LATE, print),
    late!("Mu", "put", 0, LATE, put),
    late!("Mu", "note", 0, LATE, note),
    late!("Mu", "return-rw", 0, LATE, return_rw),
    late!("Mu", "can", 1, LATE, can),
    late!("Mu", "does", 1, LATE, does),
    late!(
        "Any",
        "are",
        0,
        LATE.or(RowFlags::SLURPY).or(RowFlags::ANY_NAMED),
        are
    ),
    late!("Array", "shape", 0, LATE, shape),
    // The twins of `Any`'s pure `nl-out` and `pending` rows (`any_misc.rs`)
    // for a receiver with no shape (a user class's instance).
    late!("Any", "nl-out", 0, LATE, nl_out),
    late!("Any", "pending", 0, LATE, pending),
];

// Cost: O(1).
fn nl_out(_: &mut Interpreter, t: &Value, a: &[Value], _: Named<'_>) -> Answer {
    Some(super::any_misc::nl_out(t, a))
}

// Cost: O(f), f = pending failures.
fn pending(_: &mut Interpreter, t: &Value, a: &[Value], _: Named<'_>) -> Answer {
    Some(super::any_misc::pending(t, a))
}

// Cost: O(n), n = characters of the invocant's gist.
fn say(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.dispatch_say(t))
}

// Cost: O(n), n = characters of the invocant's `Str`.
fn print(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.dispatch_print(t))
}

// Cost: O(n), n = characters of the invocant's `Str`.
fn put(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.dispatch_put(t))
}

// Cost: O(n), n = characters of the invocant's gist.
fn note(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.dispatch_note(t))
}

// Cost: O(1).
fn return_rw(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(t.clone()))
}

/// `Mu.can($name)`: the methods of that name the receiver's MRO provides.
// Cost: O(m), m = classes in the receiver's MRO.
fn can(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let name = args[0].to_string_value();
    Some(Ok(Value::array(i.collect_can_methods(t, &name))))
}

/// `Mu.does($type)`: whether the receiver does the type, named by a type
/// object, a string or an instance.
// Cost: O(m), m = classes and roles in the receiver's type.
fn does(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let type_name = match args[0].view() {
        ValueView::Package(name) => name.resolve(),
        ValueView::Str(name) => name.to_string(),
        ValueView::Instance { class_name, .. } => class_name.resolve(),
        // A plain value stands for its type: `2.does(2)` asks `Int` about
        // `Int` (Test's `does-ok($x, 2, ...)`).
        _ => crate::runtime::value_type_name(&args[0]).to_string(),
    };
    Some(Ok(Value::truth(i.type_matches_value(&type_name, t))))
}

/// `Any.are`: the narrowest type every element shares.
// Cost: O(n), n = elements of the receiver.
fn are(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    let mut all = args.to_vec();
    all.extend(named.pairs().iter().cloned());
    Some(i.dispatch_are(t.clone(), &all))
}

// Cost: O(d), d = dimensions of the array.
fn shape(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_shape(t)
}
