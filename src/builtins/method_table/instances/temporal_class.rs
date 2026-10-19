//! `Date.today`, `Date.new-from-daycount` and `DateTime.now` (ADR-11276
//! §9.123): class methods of `Date` and `DateTime`, called on the type object
//! or a user subclass's.
//!
//! The rows are reached through their owner (`RowFlags::OWNER_ONLY`,
//! `invoke_owner`) from the by-name dispatch that answered them before
//! (`dispatch_method_by_name_3`); `Interpreter::temporal_class_owners` names
//! the owner of a subclass's type object from its MRO. `invoke_owner` answers
//! a type object only from a `TYPE_OBJECT_OK` row, so the instance rows of the
//! two owners never see one.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident) => {
        row!($owner, $name, $arity, $handler, &[])
    };
    ($owner:literal, $name:literal, $arity:literal, $handler:ident, $named:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::TYPE_OBJECT_OK),
            named: $named,
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Date", "today", 0, today, &["formatter"]),
    row!("Date", "new-from-daycount", 1, new_from_daycount),
    row!("DateTime", "now", 0, now, &["formatter", "timezone"]),
];

/// `Date.today`.
// Cost: O(1), plus a subclass's constructor.
fn today(i: &mut Interpreter, t: &Value, _args: &[Value], _named: Named<'_>) -> Answer {
    Some(i.date_class_today(t))
}

/// `Date.new-from-daycount($daycount)`.
// Cost: O(1), plus a subclass's constructor.
fn new_from_daycount(i: &mut Interpreter, t: &Value, args: &[Value], _named: Named<'_>) -> Answer {
    Some(i.date_class_from_daycount(t, &args[0]))
}

/// `DateTime.now(:$timezone, :&formatter)`.
// Cost: O(1), plus a subclass's constructor.
fn now(i: &mut Interpreter, t: &Value, _args: &[Value], named: Named<'_>) -> Answer {
    i.dispatch_datetime_now(t, named.pairs())
}
