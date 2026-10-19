//! Late rows of `Mu` and `Cool` that were the last-resort arms of
//! `dispatch_instance_and_fallback` (ADR-11276 §9.138): `gist`, `raku`,
//! `perl`, `DUMP`, `isa`, `REPR`, `Str`, `Stringy`, `Int`, `Numeric`, `Real`
//! and `EVAL`.
//!
//! Each is `OWNER_ONLY`, registered behind any row of the same owner, name
//! and arity, and reached only by that cascade's own last step, which walks
//! the receiver's MRO for a late row (`invoke_mro_late`), after a user method,
//! a delegate, a `FALLBACK`, a class-level attribute and every other row
//! declined. `EVAL` is `Cool`'s and `RakuAST::Node`'s, so a receiver that is
//! neither (a `Blob`) has none, as in Rakudo.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

const LATE: RowFlags = RowFlags::OWNER_ONLY.or(RowFlags::TYPE_OBJECT_OK);

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
    late!("Mu", "DUMP", 0, LATE, dump),
    late!("Mu", "gist", 0, LATE, gist),
    late!("Mu", "raku", 0, LATE, raku),
    late!("Mu", "perl", 0, LATE, perl),
    late!(
        "Mu",
        "isa",
        1,
        LATE.or(RowFlags::ANY_ARGS).or(RowFlags::JUNCTION_ARGS),
        isa
    ),
    late!("Mu", "REPR", 0, LATE, repr),
    late!("Mu", "Str", 0, LATE, str_),
    late!("Mu", "Stringy", 0, LATE, stringy),
    late!("Mu", "Int", 0, LATE, int),
    late!("Mu", "Numeric", 0, LATE, numeric),
    late!("Mu", "Real", 0, LATE, real),
    // `EVAL(*%opts)`: every named argument is the method's.
    late!("Cool", "EVAL", 0, LATE.or(RowFlags::ANY_NAMED), eval),
    late!(
        "RakuAST::Node",
        "EVAL",
        0,
        LATE.or(RowFlags::ANY_NAMED),
        eval
    ),
];

// Cost: O(n), n = chars of the rendered string.
fn dump(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Interpreter::mu_dump_late(t)))
}

// Cost: O(n), n = chars of the rendered string.
fn gist(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Interpreter::mu_gist_late(t)))
}

// Cost: O(v) method calls for a junction, v = its values; O(n) otherwise.
fn raku(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(i.mu_raku_late(t)))
}

/// `Mu.perl` is `self.raku` (ADR-11276 §9.152): reached as the base candidate
/// of a user `perl`'s `callsame` and through `.^find_method('perl')`; every
/// other call to `perl` is resolved to `raku` at the call entries.
// Cost: one method call, `raku` on the receiver.
fn perl(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.call_method_with_values(t.clone(), "raku", Vec::new()))
}

// Cost: O(s + m), s = subset chain length, m = MRO length.
fn isa(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    i.mu_isa_late(t, args.first()?).map(Ok)
}

// Cost: O(n), n = chars of a type-object name.
fn repr(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(i.mu_repr_late(t)))
}

// Cost: O(n), n = chars of the rendered string, plus a user `Str` call.
fn str_(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.mu_str_late(t, "Str"))
}

// Cost: O(n), n = chars of the rendered string, plus a user `Str` call.
fn stringy(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.mu_str_late(t, "Stringy"))
}

// Cost: O(1), plus a user coercion call.
fn int(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.mu_numeric_coercion(t, "Int"))
}

// Cost: O(1), plus a user coercion call.
fn numeric(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.mu_numeric_coercion(t, "Numeric"))
}

// Cost: O(1), plus a user coercion call.
fn real(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.mu_numeric_coercion(t, "Real"))
}

// Cost: O(c), c = cost of compiling and running the receiver's text.
fn eval(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(i.call_function("EVAL", vec![t.clone()]))
}
