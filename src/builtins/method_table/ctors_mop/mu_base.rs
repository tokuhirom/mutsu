//! `Mu.BUILDALL`, `Mu.POPULATE`, `Mu.clone` and `Mu.new` (ADR-11276 slice 4, §9.47, §9.48), and
//! the base answers `Mu` gives `defined`, `Bool`, `so`, `not`, `WHICH`, `WHERE`, `gist`, `Str` and
//! `raku` (§9.56), and the output methods `say`, `print`, `put` and `note` (§9.57).
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
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

macro_rules! base_row {
    ($name:literal, $handler:expr) => {
        MethodRow {
            owner: "Mu",
            name: $name,
            arity: 1,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY.or(RowFlags::DEFERRAL_BASE),
            named: &[],
        }
    };
}

/// Whether the receiver is a type object (`Mu:U`), which `Mu`'s definedness
/// methods answer `False` for.
fn is_type_object(target: &Value) -> bool {
    matches!(target.view(), ValueView::Package(_))
}

/// The name a type object renders as.
fn type_object_name(target: &Value) -> String {
    match target.view() {
        ValueView::Package(name) => name.resolve().to_string(),
        _ => String::new(),
    }
}

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
    // `Mu.defined` and `Mu.Bool`: a type object is neither, anything else is both.
    // Cost: O(1).
    base_row!("defined", |_interp, target, _args, _named| Some(Ok(
        Value::truth(!is_type_object(target))
    ))),
    base_row!("Bool", |_interp, target, _args, _named| Some(Ok(
        Value::truth(!is_type_object(target))
    ))),
    // `Mu.so` and `Mu.not` ask the receiver's own `Bool`, so a user override is seen.
    // Cost: O(1) plus the receiver's `Bool`.
    base_row!("so", |interp, target, _args, _named| Some(
        interp
            .call_method_with_values(target.clone(), "Bool", Vec::new())
            .map(|v| Value::truth(v.truthy()))
    )),
    base_row!("not", |interp, target, _args, _named| Some(
        interp
            .call_method_with_values(target.clone(), "Bool", Vec::new())
            .map(|v| Value::truth(!v.truthy()))
    )),
    // `Mu.WHICH` and `Mu.WHERE`: the identity the native cascade computes.
    // Cost: O(1).
    base_row!("WHICH", |_interp, target, _args, _named| {
        crate::builtins::native_method_0arg(target, Symbol::intern("WHICH"))
    }),
    base_row!("WHERE", |_interp, target, _args, _named| {
        crate::builtins::native_method_0arg(target, Symbol::intern("WHERE"))
    }),
    // `Mu.gist`: `(Name)` for a type object, else the receiver's own `raku`.
    // Cost: O(1) plus the receiver's `raku`.
    base_row!("gist", |interp, target, _args, _named| {
        if is_type_object(target) {
            return Some(Ok(Value::str(format!("({})", type_object_name(target)))));
        }
        Some(interp.call_method_with_values(target.clone(), "raku", Vec::new()))
    }),
    // `Mu.Str` and `Mu.raku` of a type object: no value, and the type's name.
    // Cost: O(1).
    base_row!("Str", |_interp, target, _args, _named| {
        if is_type_object(target) {
            return Some(Ok(Value::str(String::new())));
        }
        Some(Ok(Value::str(target.to_string_value())))
    }),
    base_row!("raku", |interp, target, args, _named| {
        if is_type_object(target) {
            return Some(Ok(Value::str(type_object_name(target))));
        }
        interp.default_instance_repr(target, "raku", &args[1..])
    }),
    // `Mu.say`, `Mu.print`, `Mu.put` and `Mu.note`: write the receiver's `gist` or
    // `Str` to `$*OUT` (`$*ERR` for `note`) -- what a user `method say { ...
    // callsame }` reaches. An `IO::CatHandle` has write methods of its own and
    // declines.
    // Cost: O(n), n = chars rendered and written.
    base_row!("say", |interp, target, _args, _named| {
        (!Interpreter::is_io_cathandle(target)).then(|| interp.dispatch_say(target))
    }),
    base_row!("print", |interp, target, _args, _named| {
        (!Interpreter::is_io_cathandle(target)).then(|| interp.dispatch_print(target))
    }),
    base_row!("put", |interp, target, _args, _named| {
        (!Interpreter::is_io_cathandle(target)).then(|| interp.dispatch_put(target))
    }),
    base_row!("note", |interp, target, _args, _named| {
        (!Interpreter::is_io_cathandle(target)).then(|| interp.dispatch_note(target))
    }),
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
