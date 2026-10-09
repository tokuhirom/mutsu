//! `Mu.BUILDALL` and `Mu.POPULATE` (ADR-11276 slice 4, §9.47).
//!
//! Rakudo declares both on `Mu`, so a user `BUILDALL`/`POPULATE` (typically
//! installed by a custom HOW's `add_method`, as OO::Monitors does) that defers
//! with `callsame`/`nextsame` ends at them. mutsu builds the instance natively
//! before the user hook runs (`run_user_buildall_hook`), so the base candidate
//! answers the already-built instance. The rows are reached through their owner
//! by [`crate::builtins::method_table::invoke_owner_raw`], the receiver first.

use crate::builtins::method_table::{Handler, MethodRow, RowFlags};

macro_rules! row {
    ($name:literal) => {
        MethodRow {
            owner: "Mu",
            name: $name,
            arity: 1,
            handler: Handler::Interp(|_interp, target, _args, _named| Some(Ok(target.clone()))),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::SLURPY),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[row!("BUILDALL"), row!("POPULATE")];
