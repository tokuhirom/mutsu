//! `Mu.Str` and `Mu.Stringy` (ADR-11276 slice 5, §9.93): the string coercions
//! a receiver reaches when its own type declares none.
//!
//! Rakudo declares both on `Mu` (`Stringy` is `self.Str`), and `Str` again on
//! most built-in types. The closed shapes answer their own `Str` rows first;
//! `Mu.Str` answers what is left: a built-in type object (the "uninitialized
//! value" warning, resuming with `""`), an unhandled `Failure` (it throws), a
//! lazy array (`...`), a `Junction`, a `Promise`, a `Channel` and the other
//! receivers with no shape, by their string form. It declines what needs the
//! interpreter: an instance or a mixin (its class or role may declare `Str`
//! or `message`), a user type object, an enum, a lazy list that must be
//! forced, a list or quant hash holding an element whose class may declare
//! `Str`, a regex (it warns) and a code object (the `Code.Str` row).
//!
//! `Mu.Stringy` is an interpreter row: it calls the receiver's own `Str`, so a
//! user class that declares only `Str`, a `Hash` subclass and every closed
//! shape answer `Stringy` with their `Str`.

use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{ArrayKind, RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Mu",
        name: "Str",
        arity: 0,
        handler: Handler::Narrow(str_),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
    MethodRow {
        owner: "Mu",
        name: "Stringy",
        arity: 0,
        handler: Handler::Interp(stringy_in),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    },
];

/// `Map` declares `Str` itself: `Mu.Str`'s answer for a map value (the pairs,
/// one per line). Found by owner only.
pub(super) static MAP_ROWS: &[MethodRow] = &[MethodRow {
    owner: "Map",
    name: "Str",
    arity: 0,
    handler: Handler::Narrow(str_),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];

/// `Mu.Str`.
// Cost: O(1) for a type object or a `Failure`; O(n) otherwise, n = chars of
// the rendered string (plus O(e) to probe a list's elements, e = elements).
fn str_(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Failure" => {
            // Using a Failure in string context throws the wrapped exception.
            return Some(Err(match attributes.as_map().get("exception") {
                Some(ex) => RuntimeError::from_exception_value(ex.clone()),
                None => RuntimeError::new("Failed"),
            }));
        }
        // A built-in type object warns and is the empty string; a user type
        // object's name and its own `Str` are the interpreter's.
        ValueView::Package(name) => {
            let name = name.resolve();
            crate::builtin_types::catalog::builtin_type_mro_strs(&name)?;
            let msg = format!(
                "Use of uninitialized value of type {name} in string context.\nMethods .^name, .raku, .gist, or .say can be used to stringify it to something meaningful."
            );
            return Some(Err(RuntimeError::warn_signal_with_resume(
                msg,
                Value::str(String::new()),
            )));
        }
        ValueView::Instance { .. }
        | ValueView::Mixin(..)
        | ValueView::LazyList(_)
        | ValueView::Enum { .. } => {
            return None;
        }
        // A lazy (infinite-backed) array stringifies to a bounded `...`
        // placeholder rather than materializing its capped backing.
        ValueView::Array(_, ArrayKind::Lazy) => return Some(Ok(Value::str_from("..."))),
        // A zero-denominator Rational dies with `X::Numeric::DivideByZero`,
        // which the interpreter raises with its context.
        ValueView::Rat(_, 0) | ValueView::FatRat(_, 0) => return None,
        // A regex warns and is the empty string; every other code object
        // answers its name with a warning (the `Code.Str` row).
        ValueView::Regex(_)
        | ValueView::RegexWithAdverbs(..)
        | ValueView::Sub(_)
        | ValueView::WeakSub(_)
        | ValueView::Routine { .. } => return None,
        _ => {}
    }
    // A collection stringifies every element, so a zero-denominator Rational
    // inside it dies like its own `.Str` (GH #9608).
    if matches!(
        target.view(),
        ValueView::Array(..)
            | ValueView::Seq(..)
            | ValueView::Slip(..)
            | ValueView::Hash(..)
            | ValueView::Pair(..)
            | ValueView::ValuePair(..)
    ) && let Some(err) = crate::runtime::utils::zero_denominator_rational_error(target)
    {
        return Some(Err(err));
    }
    // A list or quant hash holding an object element needs the interpreter:
    // the element's class may define its own `Str`
    // (`dispatch_list_str_method`, `quanthash_element_stringify.rs`).
    if Interpreter::list_str_needs_interpreter(target)
        || Interpreter::quanthash_needs_element_dispatch(target)
    {
        return None;
    }
    Some(Ok(if target.is_str_value() {
        target.clone()
    } else {
        Value::str(target.to_string_value())
    }))
}

/// `Mu.Stringy`: `self.Str`, as Rakudo declares it.
// Cost: one method call of `Str` on the receiver.
fn stringy_in(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    // The compiled path finds a user `Str` ahead of a built-in ancestor's
    // native one (a `Date` subclass overriding `Str`).
    Some(interp.try_compiled_method_or_interpret(target.clone(), "Str", Vec::new()))
}
