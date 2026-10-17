//! `Metamodel::EnumHOW`'s own metamethods' rows (ADR-11276 slice 3G, §9.140):
//! `enum_values`, `elems`, `enum_from_value`, `enum_value_list`,
//! `add_enum_value`, `set_export_callback`, `compose_values`, `is_composed` and
//! `compose`.
//!
//! As with `class_how`'s rows, the type object is the first argument and the
//! rows are reached through their owner by
//! [`crate::builtins::method_table::invoke_owner_raw`], ahead of the shared
//! `Metamodel::*HOW` owners. mutsu's HOW is one dispatcher over every kind,
//! so these rows also see a type that is no enum. The query metamethods report
//! the EnumHOW-only error for one, `elems` answers `Any.elems`'s 1, and
//! `is_composed` and `compose` decline to the shared ClassHOW rows.
//! The routines are `Interpreter::enum_how_*` (`runtime/methods_enumhow.rs`).

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: "Metamodel::EnumHOW",
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::SLURPY),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("enum_values", 1, enum_values),
    row!("elems", 1, elems),
    row!("enum_from_value", 2, enum_from_value),
    row!("enum_value_list", 1, enum_value_list),
    row!("add_enum_value", 2, add_enum_value),
    row!("set_export_callback", 2, set_export_callback),
    row!("compose_values", 1, compose_values),
    row!("is_composed", 1, is_composed),
    row!("compose", 1, compose),
];

// Cost: O(n), n = number of enum values.
fn enum_values(i: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(i.enum_how_enum_values(args.first()?))
}

// Cost: O(n), n = number of enum values.
fn elems(i: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(i.enum_how_elems(args.first()?)))
}

// Cost: O(n), n = number of enum values.
fn enum_from_value(i: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(i.enum_how_enum_from_value(args.first()?, args.get(1)?))
}

// Cost: O(n), n = number of enum values.
fn enum_value_list(i: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(i.enum_how_enum_value_list(args.first()?)))
}

// Cost: O(1) amortized.
fn add_enum_value(i: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(i.enum_how_add_enum_value(args))
}

// Cost: O(1).
fn set_export_callback(i: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(i.enum_how_set_export_callback(args))
}

// Cost: O(1) plus the export callback.
fn compose_values(i: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    Some(i.enum_how_compose_values(args))
}

// Cost: O(1).
fn is_composed(i: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    i.enum_how_is_composed(args.first()?).map(Ok)
}

// Cost: O(1); the shared ClassHOW `compose` runs after it.
fn compose(i: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    i.enum_how_mark_composed(args.first()?);
    None
}
