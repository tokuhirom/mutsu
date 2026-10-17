//! The rows of 6.e's `Format` and `Formatter` (ADR-11276 §9.153).
//!
//! A `Format` is a `Str` subclass that is also a `Callable`: Rakudo's
//! `class Format is Str does Callable`. Its string is the `Str` payload of a
//! native subclass (`__mutsu_str_value`, `builtins::native_base`), so every
//! method it inherits from `Str`, `Cool`, `Any` and `Mu` answers on the
//! string. These rows are the methods `Format` declares itself: `raku`,
//! `arity`, `count`, `directives`, `Callable`, `CALL-ME` and `AST`.
//!
//! `Formatter` is a type object only: `CODE` and `AST` compile a format
//! string. Both classes' `new` are constructors (`runtime::native_ctor`).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($owner:literal, $name:literal, $handler:expr) => {
        row!($owner, $name, 0, $handler, RowFlags::NONE)
    };
    ($owner:literal, $name:literal, $arity:literal, $handler:expr, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: $handler,
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static FORMAT_ROWS: &[MethodRow] = &[
    row!("Format", "raku", Handler::Narrow(raku)),
    row!("Format", "arity", Handler::Narrow(count)),
    row!("Format", "count", Handler::Narrow(count)),
    row!("Format", "directives", Handler::Narrow(directives)),
    row!("Format", "Callable", Handler::Interp(callable)),
    row!("Format", "AST", Handler::Narrow(format_ast)),
    row!(
        "Format",
        "CALL-ME",
        0,
        Handler::Interp(call_me),
        RowFlags::ANY_ARGS.or(RowFlags::SLURPY)
    ),
];

pub(super) static FORMATTER_ROWS: &[MethodRow] = &[
    row!(
        "Formatter",
        "CODE",
        1,
        Handler::Interp(code),
        RowFlags::TYPE_OBJECT_OK.or(RowFlags::ANY_ARGS)
    ),
    row!(
        "Formatter",
        "AST",
        1,
        Handler::Narrow(formatter_ast),
        RowFlags::TYPE_OBJECT_OK.or(RowFlags::ANY_ARGS)
    ),
];

/// The format string of a `Format` receiver; `None` for anything else (a user
/// class that shadows the name carries no `Str` payload).
fn format_of(target: &Value) -> Option<String> {
    Interpreter::format_string_of(target)
}

/// `Format.raku`: the constructor call that rebuilds it.
// Cost: O(n), n = chars of the format.
fn raku(target: &Value, _: &[Value]) -> Answer {
    let fmt = format_of(target)?;
    let quoted = crate::value::raku_repr::escape_raku_str(&fmt);
    Some(Ok(Value::str(format!("Format.new({quoted})"))))
}

/// `Format.arity` and `Format.count`: the arguments the format consumes, one
/// per directive (a `*` width or precision included).
// Cost: O(n), n = chars of the format scanned for directives.
fn count(target: &Value, _: &[Value]) -> Answer {
    let fmt = format_of(target)?;
    Some(Ok(Value::int(
        crate::runtime::sprintf_directive_count(&fmt) as i64,
    )))
}

/// `Format.directives`: the conversion of each directive, flags and widths
/// stripped (`'%05d%3x'` is `('d', 'x')`).
// Cost: O(n), n = chars of the format scanned for directives.
fn directives(target: &Value, _: &[Value]) -> Answer {
    let fmt = format_of(target)?;
    let directives = crate::runtime::sprintf::sprintf_arg_specs(&fmt)
        .into_iter()
        .map(|(_, spec)| Value::str(spec.to_string()))
        .collect();
    Some(Ok(Value::array(directives)))
}

/// `Format.Callable`: the block that renders the format.
// Cost: O(d), d = directives (one parameter each).
fn callable(interp: &mut Interpreter, target: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let fmt = format_of(target)?;
    let count = crate::runtime::sprintf_directive_count(&fmt);
    Some(Ok(interp.format_callable(&fmt, count)))
}

/// `Format.CALL-ME`: `sprintf` of the format with the call's arguments, which
/// must match its directives in number and type.
// Cost: O(n + a), n = chars of the format, a = arguments rendered.
fn call_me(_: &mut Interpreter, target: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let fmt = format_of(target)?;
    Some(Interpreter::format_render(&fmt, args))
}

/// `Format.AST`: the format compiled to a RakuAST block.
// Cost: O(n), n = chars of the format.
fn format_ast(target: &Value, _: &[Value]) -> Answer {
    let fmt = format_of(target)?;
    Some(Ok(crate::rakuast::formatter_ast(&fmt)))
}

/// `Formatter.CODE($format)`: the block that renders `$format`.
// Cost: O(d), d = directives (one parameter each).
fn code(interp: &mut Interpreter, _: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let fmt = args.first().map(Value::to_string_value).unwrap_or_default();
    let count = crate::runtime::sprintf_directive_count(&fmt);
    Some(Ok(interp.format_callable(&fmt, count)))
}

/// `Formatter.AST($format)`: `$format` compiled to a RakuAST block.
// Cost: O(n), n = chars of the format.
fn formatter_ast(_: &Value, args: &[Value]) -> Answer {
    let fmt = args.first().map(Value::to_string_value).unwrap_or_default();
    Some(Ok(crate::rakuast::formatter_ast(&fmt)))
}
