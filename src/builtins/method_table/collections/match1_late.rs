//! Late rows that `dispatch_method_by_name_1` answered with arms (ADR-11276
//! §9.142):
//!
//! - `Mu.say`/`put` with arguments, for a class that acts as `$*OUT` by
//!   declaring its own `print`;
//! - `Junction.THREAD`;
//! - `Cool.encode` for what the `Str` rows did not take (a type object, an
//!   argument list they do not bind);
//! - `Blob.unpack` (experimental, as the `unpack` routine);
//! - `Enumeration.enums` of an enum's type object or value (§9.143);
//! - `Mu.set_why` (§9.145), which documents the invocant's type; a
//!   metaobject's own `set_why` is the meta-level route, so it declines one.
//!
//! Each is `OWNER_ONLY`, reached only by that cascade's walk of the
//! receiver's MRO and the roles its classes compose
//! (`Interpreter::late_walk_owners`).

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
    late!("Blob", "unpack", 0, LATE, unpack),
    late!(
        "Enumeration",
        "enums",
        0,
        LATE.or(RowFlags::TYPE_OBJECT_OK),
        enums
    ),
    late!(
        "Mu",
        "set_why",
        1,
        RowFlags::OWNER_ONLY.or(RowFlags::TYPE_OBJECT_OK),
        set_why
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

// Cost: O(b + t), b = bytes of the buffer, t = chars of the template.
fn unpack(_: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    if !matches!(t.view(), crate::value::ValueView::Instance { .. }) {
        return None;
    }
    let bytes = Interpreter::extract_buf_bytes(t);
    let template = args.first().map(Value::to_string_value).unwrap_or_default();
    Some(crate::builtins::pack::unpack(&bytes, &template))
}

// Cost: O(n), n = values of the enum.
fn enums(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    i.dispatch_enums(t)
}

// Cost: O(1) amortized (one metadata map insert).
fn set_why(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    if i.is_how_receiver(t) {
        return None;
    }
    Some(i.dispatch_mu_set_why(t, args.first()?.clone()))
}
