//! Late rows that `dispatch_method_by_name_3` answered with arms (ADR-11276
//! §9.139): `Seq.Numeric`, the temporal type objects' `Date`/`DateTime`,
//! `Thread.run`, the `Str`/`Stringy` of a list whose elements need the
//! interpreter, and the hash-likes' `keys`/`values`/`AT-KEY`/`EXISTS-KEY` (a
//! `Stash`, an object hash, a `Match`, a `Capture`, a `Pair`).
//!
//! Each is `OWNER_ONLY`, reached only by that cascade's walk of the
//! receiver's MRO for a late row, after every earlier answer declined. A
//! handler declines a receiver it does not serve, and the walk goes on.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

const LATE: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS);
const TYPE_LATE: RowFlags = LATE.or(RowFlags::TYPE_OBJECT_OK);

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
    late!("Seq", "Numeric", 0, LATE, seq_numeric),
    late!("Date", "Date", 0, TYPE_LATE, to_date),
    late!("Date", "DateTime", 0, TYPE_LATE, to_datetime),
    late!("DateTime", "Date", 0, TYPE_LATE, to_date),
    late!("DateTime", "DateTime", 0, TYPE_LATE, to_datetime),
    late!("Thread", "run", 0, LATE, thread_run),
    late!("List", "Str", 0, LATE, list_str),
    late!("Seq", "Str", 0, LATE, list_str),
    late!("Seq", "Stringy", 0, LATE, list_str),
    late!("Any", "keys", 0, LATE, keys),
    late!("Any", "values", 0, LATE, values),
    late!("Any", "AT-KEY", 1, LATE, at_key),
    late!("Any", "EXISTS-KEY", 1, LATE, exists_key),
];

// Cost: O(1), plus a `PredictiveIterator`'s `count-only`.
fn seq_numeric(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.seq_numeric_late(t)
}

// Cost: O(1).
fn to_date(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Interpreter::temporal_type_coercion_late(t, "Date").map(Ok)
}

// Cost: O(1).
fn to_datetime(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Interpreter::temporal_type_coercion_late(t, "DateTime").map(Ok)
}

// Cost: O(1), plus the thread's start.
fn thread_run(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.thread_run_late(t)
}

// Cost: O(e) element renders, e = elements of a list.
fn list_str(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_list_str_method(t.clone())
}

// Cost: O(k), k = keys of the receiver.
fn keys(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_keys_method(t.clone())
}

// Cost: O(e), e = values of the receiver.
fn values(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.dispatch_values_method(t.clone()))
}

// Cost: O(k * b), k = indexed keys, b = bytes per key.
fn at_key(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_at_key_method(t, args)
}

// Cost: O(b), b = bytes of the key.
fn exists_key(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_match_exists_key(t, args)
}
