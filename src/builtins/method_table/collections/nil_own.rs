//! `Nil`'s own methods (ADR-11276), as Rakudo declares them in
//! `src/core.c/Nil.rakumod`:
//!
//! - the `Cool` text methods "that should just warn": `Use of Nil.<name>
//!   coerced to empty string`, resumed with `""`. Each keeps its own signature:
//!   `chars`, `chomp`, `flip`, `lc`, `tc`, `tclc`, `uc` and `wordcase` take no
//!   positional argument (`Nil:`), the others any (`Nil: |`);
//! - the coercions `Int(|)`, `Numeric()` and `Str()`, which warn about the
//!   numeric or string context and answer `0` or `""`;
//! - the subscript protocol's writers: `STORE` throws `X::Assignment::RO`,
//!   `BIND-POS`/`BIND-KEY` answer an `X::Bind` `Failure`, and
//!   `ASSIGN-POS`/`ASSIGN-KEY` die with `Use of Nil.<name> not allowed`;
//! - `new(*@)`, which answers `Nil`;
//! - the six QuantHash coercions, `Set.new(Nil)` and its siblings.
//!
//! Each row is reached through its owner only (`Nil` has no shape): the VM's
//! `Nil` receiver entries ask `invoke_owner(["Nil"])` before `Nil`'s
//! `FALLBACK`, and `answer_shapeless` finds them for a `Nil` receiver.
//! A `Failure` is a `Nil` too, but Rakudo's `!warn`/`!die` throw it instead:
//! every handler declines a receiver that is not the `Nil` value itself.
//!
//! Not here: `push`/`append`/`unshift`/`prepend` (mutsu reads an undefined
//! element as `Nil` and autovivifies it through `Any`'s growing rows; a literal
//! `Nil.push` is refused at compile time), `iterator`, `gist`, `raku`, `sink`
//! and `FALLBACK` (answered by their own entries).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// A `Nil:` candidate: no positional argument, any named one (`*%_`).
const BARE: RowFlags = RowFlags::OWNER_ONLY
    .or(RowFlags::TYPE_OBJECT_OK)
    .or(RowFlags::ANY_NAMED);

/// A `Nil: |` candidate: any argument list.
const CAPTURE: RowFlags = BARE
    .or(RowFlags::SLURPY)
    .or(RowFlags::ANY_ARGS)
    .or(RowFlags::OBJECT_ARGS);

macro_rules! nil {
    ($name:literal, $flags:expr, $handler:ident) => {
        MethodRow {
            owner: "Nil",
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: &[],
        }
    };
}

/// A text method "that should just warn": the handler names the method in
/// the warning, as Rakudo's `self!warn: 'chars'` does.
macro_rules! warns {
    ($name:literal, $flags:expr) => {
        MethodRow {
            owner: "Nil",
            name: $name,
            arity: 0,
            handler: Handler::Interp(|interp, target, _, _| empty_string(interp, target, $name)),
            flags: $flags,
            named: &[],
        }
    };
}

/// A writer that dies: `self!die: 'ASSIGN-POS'`.
macro_rules! dies {
    ($name:literal) => {
        MethodRow {
            owner: "Nil",
            name: $name,
            arity: 0,
            handler: Handler::Interp(|_, target, args, _| not_allowed(target, args, $name)),
            flags: CAPTURE,
            named: &[],
        }
    };
}

/// A QuantHash coercion: `Set.new(Nil)`.
macro_rules! quanthash {
    ($name:literal) => {
        MethodRow {
            owner: "Nil",
            name: $name,
            arity: 0,
            handler: Handler::Interp(|interp, target, _, _| quanthash(interp, target, $name)),
            flags: BARE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    // `multi method chars(Nil: --> '') { self!warn: 'chars' }` and kin.
    warns!("chars", BARE),
    warns!("chomp", BARE),
    warns!("flip", BARE),
    warns!("lc", BARE),
    warns!("tc", BARE),
    warns!("tclc", BARE),
    warns!("uc", BARE),
    warns!("wordcase", BARE),
    // `multi method chop(Nil: | --> '') { self!warn: 'chop' }` and kin.
    warns!("chop", CAPTURE),
    warns!("codes", CAPTURE),
    warns!("comb", CAPTURE),
    warns!("contains", CAPTURE),
    warns!("ends-with", CAPTURE),
    warns!("indent", CAPTURE),
    warns!("index", CAPTURE),
    warns!("indices", CAPTURE),
    warns!("lines", CAPTURE),
    warns!("rindex", CAPTURE),
    warns!("starts-with", CAPTURE),
    warns!("trans", CAPTURE),
    warns!("substr", CAPTURE),
    warns!("subst", CAPTURE),
    warns!("substr-eq", CAPTURE),
    warns!("substr-rw", CAPTURE),
    warns!("words", CAPTURE),
    // The coercions.
    nil!("Int", CAPTURE, numeric_zero),
    nil!("Numeric", BARE, numeric_zero),
    nil!("Str", BARE, string_empty),
    // The subscript protocol's writers.
    nil!("STORE", CAPTURE, store),
    nil!("BIND-POS", CAPTURE, bind),
    nil!("BIND-KEY", CAPTURE, bind_key),
    dies!("ASSIGN-POS"),
    dies!("ASSIGN-KEY"),
    // `multi method new(Nil: *@ --> Nil) { }`.
    nil!("new", CAPTURE, new),
    // `multi method Set(Nil:) { Set.new(Nil) }` and kin.
    quanthash!("Set"),
    quanthash!("SetHash"),
    quanthash!("Bag"),
    quanthash!("BagHash"),
    quanthash!("Mix"),
    quanthash!("MixHash"),
];

// Cost: O(1).
fn is_nil(target: &Value) -> bool {
    matches!(target.view(), ValueView::Nil)
}

/// `self!warn: $name`, resumed with the empty string.
// Cost: O(1) plus the warning's handlers.
fn empty_string(interp: &mut Interpreter, target: &Value, name: &str) -> Answer {
    is_nil(target).then(|| {
        interp.raise_resumable_warning(
            &format!("Use of Nil.{name} coerced to empty string"),
            Value::str(String::new()),
        )
    })
}

/// `Int` and `Numeric`: `warn "Use of Nil in numeric context"`, then `0`.
// Cost: O(1) plus the warning's handlers.
fn numeric_zero(interp: &mut Interpreter, target: &Value, _: &[Value], _: Named<'_>) -> Answer {
    is_nil(target)
        .then(|| interp.raise_resumable_warning("Use of Nil in numeric context", Value::int(0)))
}

/// `Str`: `warn "Use of Nil in string context"`, then `""`.
// Cost: O(1) plus the warning's handlers.
fn string_empty(interp: &mut Interpreter, target: &Value, _: &[Value], _: Named<'_>) -> Answer {
    is_nil(target).then(|| {
        interp.raise_resumable_warning("Use of Nil in string context", Value::str(String::new()))
    })
}

/// `STORE(|)`: `X::Assignment::RO.new(:value(Nil)).throw`.
// Cost: O(1).
fn store(_: &mut Interpreter, target: &Value, _: &[Value], _: Named<'_>) -> Answer {
    is_nil(target).then(|| Err(RuntimeError::assignment_ro_value(Value::NIL)))
}

/// `BIND-POS`/`BIND-KEY`: `X::Bind.new(:target<Nil>).Failure`.
// Cost: O(1).
fn bind(_: &mut Interpreter, target: &Value, _: &[Value], _: Named<'_>) -> Answer {
    if !is_nil(target) {
        return None;
    }
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("target".to_string(), Value::str_from("Nil"));
    attrs.insert("message".to_string(), Value::str_from("Cannot bind to Nil"));
    let exception = Value::make_instance(Symbol::intern("X::Bind"), attrs);
    let mut failure = std::collections::HashMap::new();
    failure.insert("exception".to_string(), exception);
    Some(Ok(Value::make_instance(Symbol::intern("Failure"), failure)))
}

/// `BIND-KEY`: [`bind`], except that a key and a value bind `Any`'s
/// narrower `Any:U \SELF: \key, \bindee` candidate, whose `SELF` must be a
/// writable container: a `Nil` invocant is an `Any` value without one.
// Cost: O(1).
fn bind_key(interp: &mut Interpreter, target: &Value, args: &[Value], named: Named<'_>) -> Answer {
    if is_nil(target) && args.len() == 2 {
        return Some(Err(crate::runtime::utils::parameter_rw_not_container(
            "self",
            &Value::package(Symbol::intern("Any")),
        )));
    }
    bind(interp, target, args, named)
}

/// `self!die: $name`. A key and a value (`ASSIGN-POS(0, 1)`) bind `Any`'s
/// narrower `Any:U \SELF: $pos, \assignee` candidate instead, whose
/// autovivifying assignment to the immutable `Nil` throws `X::Assignment::RO`.
// Cost: O(1).
fn not_allowed(target: &Value, args: &[Value], name: &str) -> Answer {
    if !is_nil(target) {
        return None;
    }
    if args.len() == 2 {
        return Some(Err(RuntimeError::assignment_ro_value(Value::NIL)));
    }
    Some(Err(RuntimeError::new(format!("Use of Nil.{name} not allowed"))))
}

/// `new(*@)`: `Nil`, whatever the arguments.
// Cost: O(1).
fn new(_: &mut Interpreter, target: &Value, _: &[Value], _: Named<'_>) -> Answer {
    is_nil(target).then_some(Ok(Value::NIL))
}

/// `$name.new(Nil)`, for `$name` one of the six QuantHash types.
// Cost: O(1), one element.
fn quanthash(interp: &mut Interpreter, target: &Value, name: &str) -> Answer {
    if !is_nil(target) {
        return None;
    }
    interp.try_native_quanthash_construct_for_package(Symbol::intern(name), &[Value::NIL])
}
