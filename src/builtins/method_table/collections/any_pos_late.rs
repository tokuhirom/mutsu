//! Late rows of `Any`'s positional protocol (ADR-11276 §9.138): `AT-POS`,
//! `EXISTS-POS`, `ASSIGN-POS`, `BIND-POS` and `DELETE-POS` with several
//! indices (Rakudo's `**@indices` candidates), a shaped array's bounds and a
//! native array's refusals.
//!
//! Each is `OWNER_ONLY`, reached only by `call_method_with_values`' walk of a
//! list's or array's MRO for a late row, after the subscript rows declined.
//! A named argument the method does not declare was stripped before; one that
//! is left counts as an index, as it always did. `EXISTS-POS` also answers a
//! `Match`'s positional capture, from `dispatch_method_by_name_3`'s walk
//! (§9.139).

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
    ($name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: "Any",
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: LATE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    late!("AT-POS", 1, at_pos),
    late!("EXISTS-POS", 1, exists_pos),
    late!("ASSIGN-POS", 2, assign_pos),
    late!("BIND-POS", 2, bind_pos),
    late!("DELETE-POS", 0, delete_pos),
];

/// The call's arguments, a named one left over counted as an index.
// Cost: O(a), a = arguments of the call.
fn indices(args: &[Value], named: Named<'_>) -> Vec<Value> {
    let mut all = Vec::with_capacity(args.len() + named.pairs().len());
    all.extend(args.iter().cloned());
    all.extend(named.pairs().iter().cloned());
    all
}

macro_rules! handler {
    ($fn:ident, $name:literal) => {
        // Cost: O(d), d = indices of the call or dimensions of a shaped invocant.
        fn $fn(_: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
            Interpreter::array_pos_late(t, $name, &indices(args, named))
        }
    };
}

handler!(at_pos, "AT-POS");
// Cost: O(d), d = indices of the call or dimensions of a shaped invocant.
fn exists_pos(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    let all = indices(args, named);
    Interpreter::array_pos_late(t, "EXISTS-POS", &all).or_else(|| match all.as_slice() {
        // A `Match`'s positional capture (`dispatch_method_by_name_3`, §9.139).
        [_] => i.dispatch_match_exists_pos(t, &all),
        _ => None,
    })
}
handler!(assign_pos, "ASSIGN-POS");
handler!(bind_pos, "BIND-POS");
handler!(delete_pos, "DELETE-POS");
