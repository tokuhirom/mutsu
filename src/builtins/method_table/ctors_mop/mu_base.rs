//! `Mu.BUILDALL`, `Mu.POPULATE`, `Mu.clone` and `Mu.new` (ADR-11276 slice 4, §9.47, §9.48).
//!
//! Rakudo declares both on `Mu`, so a user `BUILDALL`/`POPULATE` (typically
//! installed by a custom HOW's `add_method`, as OO::Monitors does) that defers
//! with `callsame`/`nextsame` ends at them. mutsu builds the instance natively
//! before the user hook runs (`run_user_buildall_hook`), so the base candidate
//! answers the already-built instance. The rows are reached as the base of a
//! deferral by [`crate::builtins::method_table::invoke_base`], the receiver first.
//!
//! `clone` is the native attribute-copying clone of an instance with the call's
//! `:attr(v)` twiddles applied (`Interpreter::native_instance_clone_value`, the
//! implementation the direct `.clone` call shares); the receiver is the only
//! positional, and a receiver that is not an instance declines.
//!
//! `new` is `Mu.new(*%attrinit)`, the last candidate of a user `new` override
//! (`Interpreter::mu_new_base`): the nearest builtin ancestor's native
//! constructor, then the named-arguments-only check, then `bless`. It is the
//! base of a deferral only; a plain `Foo.new(...)` still reaches the per-type
//! constructors (`methods_object_dispatch_new.rs`), which are not rows yet.

use crate::builtins::method_table::{Handler, MethodRow, RowFlags};

macro_rules! row {
    ($name:literal) => {
        MethodRow {
            owner: "Mu",
            name: $name,
            arity: 1,
            handler: Handler::Interp(|_interp, target, _args, _named| Some(Ok(target.clone()))),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::DEFERRAL_BASE),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("BUILDALL"),
    row!("POPULATE"),
    MethodRow {
        owner: "Mu",
        name: "clone",
        arity: 1,
        handler: Handler::Interp(|interp, target, args, _named| {
            interp.native_instance_clone_value(target, &args[1..])
        }),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::DEFERRAL_BASE),
        named: &[],
    },
    MethodRow {
        owner: "Mu",
        name: "new",
        arity: 1,
        handler: Handler::Interp(|interp, target, args, _named| {
            interp.mu_new_base(target.clone(), args[1..].to_vec())
        }),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::DEFERRAL_BASE),
        named: &[],
    },
];
