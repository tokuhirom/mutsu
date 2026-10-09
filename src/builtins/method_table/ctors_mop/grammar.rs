//! `Grammar.parse`, `.subparse` and `.parsefile` (ADR-11276 slice 4, §9.46).
//!
//! Rakudo declares the three on `Grammar` itself, so a user grammar's own
//! `method parse` that defers with `nextsame`/`callwith` reaches them as the
//! last candidate of its MRO. A grammar value has no row shape, so each row is
//! reached through its owner by [`crate::builtins::method_table::invoke_owner_raw`],
//! the receiver first among the arguments, and answers with the one
//! implementation the direct `G.parse(...)` call uses
//! ([`Interpreter::dispatch_instance_parse`]).

use crate::builtins::method_table::{Handler, MethodRow, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal) => {
        MethodRow {
            owner: "Grammar",
            name: $name,
            arity: 2,
            handler: Handler::Interp(|interp, target, args, _named| {
                grammar_parse(interp, target, $name, args)
            }),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::SLURPY),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[row!("parse"), row!("parsefile"), row!("subparse")];

/// Run `method` on the grammar `target` (a type object or an instance). `args`
/// is the receiver followed by the call's own arguments, named ones included
/// as pairs; a receiver that is not a grammar declines.
// Cost: O(n) in the parsed text, n = its length, plus the grammar's own rules.
fn grammar_parse(
    interp: &mut Interpreter,
    target: &Value,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let class_name = match target.view() {
        ValueView::Package(name) => name.resolve(),
        ValueView::Instance { class_name, .. } => class_name.resolve(),
        _ => return None,
    };
    if !interp.class_is_grammar(&class_name) {
        return None;
    }
    Some(interp.dispatch_instance_parse(target.clone(), &class_name, method, &args[1..]))
}
