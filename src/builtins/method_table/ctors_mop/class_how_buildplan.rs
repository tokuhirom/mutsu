//! `Metamodel::ClassHOW`'s BUILDPLAN metamethods (ADR-14162): `create_BUILDPLAN`,
//! which a HOW mixin overrides and `callsame`s into, and the `BUILDPLAN` /
//! `BUILDALLPLAN` readers. Handlers are in `runtime/buildplan.rs`.

use crate::builtins::method_table::{Handler, MethodRow, RowFlags};

macro_rules! row {
    ($name:literal, $handler:expr) => {
        MethodRow {
            owner: "Metamodel::ClassHOW",
            name: $name,
            arity: 1,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::SLURPY),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    // Cost: O(m * a), m = the type's MRO length, a = attributes per class.
    row!("create_BUILDPLAN", |interp, _target, args, _named| Some(
        interp.mop_create_buildplan(args.to_vec())
    )),
    // Cost: O(a), a = the type's attributes, unless stored (O(1)).
    row!("BUILDPLAN", |interp, _target, args, _named| Some(
        interp.mop_buildplan(args.to_vec(), false)
    )),
    // Cost: O(m * a), unless stored (O(1)).
    row!("BUILDALLPLAN", |interp, _target, args, _named| Some(
        interp.mop_buildplan(args.to_vec(), true)
    )),
];
