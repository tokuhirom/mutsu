//! Late rows that `dispatch_method_by_name_1` answered with arms (ADR-11276
//! §9.142):
//!
//! - `Mu.say`/`put` with arguments, for a class that acts as `$*OUT` by
//!   declaring its own `print`;
//! - `Junction.THREAD`;
//! - `Cool.encode` for what the `Str` rows did not take (a type object, an
//!   argument list they do not bind).
//!
//! Each is `OWNER_ONLY`, reached only by that cascade's walk of the
//! receiver's MRO for a late row.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

const LATE: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_NAMED)
    .or(RowFlags::JUNCTION_ARGS);

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
    late!("Mu", "say", 1, LATE, say),
    late!("Mu", "put", 1, LATE, put),
    late!("Junction", "THREAD", 0, LATE, thread),
    late!(
        "Cool",
        "encode",
        0,
        LATE.or(RowFlags::TYPE_OBJECT_OK),
        encode
    ),
];

/// The call's arguments, named ones after the positional ones.
// Cost: O(a), a = arguments of the call.
fn joined(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = Vec::with_capacity(args.len() + named.pairs().len());
    all.extend(args.iter().cloned());
    all.extend(named.pairs().iter().cloned());
    all
}

// Cost: O(n), n = total chars of the rendered arguments.
fn say(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    i.mu_say_put_via_print(t, "say", &joined(args, named))
}

// Cost: O(n), n = total chars of the rendered arguments.
fn put(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    i.mu_say_put_via_print(t, "put", &joined(args, named))
}

// Cost: O(v) calls, v = the junction's values.
fn thread(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    i.junction_thread(t, args)
}

// Cost: O(n), n = chars of the receiver's string.
fn encode(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    Some(i.dispatch_encode(t, &joined(args, named)))
}
