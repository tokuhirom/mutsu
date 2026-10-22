//! `ACCEPTS` of the code objects, of `Signature` and of a type object
//! (ADR-11276 §9.164).
//!
//! Rakudo declares a candidate on each of them, and none is the identity test
//! of `Any.ACCEPTS`:
//!
//! - `Regex.ACCEPTS($topic)` matches the topic and answers the `Match` (or
//!   `Nil`), as `$topic ~~ $regex` does;
//! - `Code.ACCEPTS($topic)` is `self.count ?? self($topic) !! self()`, the
//!   call's own result;
//! - `WhateverCode.ACCEPTS($topic)` is the predicate's truth;
//! - `Signature.ACCEPTS($capture)` is the bind test of `$capture ~~ $sig`;
//! - `Mu.ACCEPTS($topic)` on a type object (`Mu:U:`) is the type check,
//!   smiley included (`(Int:D).ACCEPTS(5)`).
//!
//! Every candidate binds `Mu $topic`, so the rows are [`Handler::Interp`] with
//! [`RowFlags::OBJECT_ARGS`]. None of these receivers has a shape: the code
//! rows are reached through their owner ([`super::code::answer`]), the
//! `Signature` row as a built-in class's own row and `Mu`'s as the row a type
//! object with no closer candidate answers (`method_table::answer_shapeless`).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const ACCEPTS: RowFlags = RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS);

macro_rules! row {
    ($owner:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: "ACCEPTS",
            arity: 1,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Regex", regex, ACCEPTS.or(RowFlags::OWNER_ONLY)),
    row!(
        "WhateverCode",
        whatever_code,
        ACCEPTS.or(RowFlags::OWNER_ONLY)
    ),
    row!("Code", code, ACCEPTS.or(RowFlags::OWNER_ONLY)),
    row!("Signature", signature, ACCEPTS.or(RowFlags::OWNER_ONLY)),
    row!(
        "Mu",
        type_object,
        ACCEPTS
            .or(RowFlags::OWNER_ONLY)
            .or(RowFlags::TYPE_OBJECT_OK)
    ),
];

/// The owners whose `ACCEPTS` row answers the code object `target`, closest
/// first, or `None` when it is not one.
// Cost: O(1).
pub(crate) fn code_accepts_owners(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Regex(_) | ValueView::RegexWithAdverbs(_) => Some(&["Regex", "Code"]),
        ValueView::Sub(data) if is_whatever_code(&data) => Some(&["WhateverCode", "Code"]),
        ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => Some(&["Code"]),
        _ => None,
    }
}

/// Whether the closure is a `WhateverCode` (`* > 3`): the compiler records the
/// callable's type in its environment.
// Cost: O(1).
fn is_whatever_code(data: &crate::value::SubData) -> bool {
    matches!(
        data.env
            .get_sym(crate::symbol::well_known::callable_type())
            .map(Value::view),
        Some(ValueView::Str(kind)) if kind.as_str() == "WhateverCode"
    )
}

/// `Regex.ACCEPTS`: `$topic.match(self)`, the `Match` or `Nil`.
// Cost: the regex engine's on the topic's string.
fn regex(interp: &mut Interpreter, target: &Value, args: &[Value], _named: Named<'_>) -> Answer {
    Some(interp.dispatch_match_method(args[0].clone(), std::slice::from_ref(target)))
}

/// `WhateverCode.ACCEPTS`: the predicate called with the topic, as a `Bool`
/// (paths' `:file(* eq $name)` matcher reads it so).
// Cost: O(1) plus the call.
fn whatever_code(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Answer {
    Some(
        interp
            .call_sub_value(target.clone(), vec![args[0].clone()])
            .map(|value| Value::truth(value.truthy())),
    )
}

/// `Code.ACCEPTS`: `self.count ?? self($topic) !! self()`, answering the call's
/// own result (`(-> $x { $x }).ACCEPTS(0)` is `0`, not `False`, #9429). A
/// `&name` handle or a weak closure is called with the topic.
// Cost: O(1) plus the call.
fn code(interp: &mut Interpreter, target: &Value, args: &[Value], _named: Named<'_>) -> Answer {
    let call_args = match target.view() {
        ValueView::Sub(data) if data.empty_sig => Vec::new(),
        _ => vec![args[0].clone()],
    };
    Some(interp.call_sub_value(target.clone(), call_args))
}

/// `Signature.ACCEPTS($capture)`: whether the capture binds (Cro's route
/// matcher calls `$handler.signature.ACCEPTS($cap)`).
// Cost: the bind's, O(p), p = parameters.
fn signature(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Answer {
    Some(Ok(Value::truth(interp.smart_match(&args[0], target))))
}

/// `Mu.ACCEPTS` of a type object: the type check `$topic ~~ Type`. A defined
/// receiver is not this candidate's (`Mu:U:`), and a class that declares its
/// own `ACCEPTS` answers through it. An `enum E does Weird` composes the
/// role's `ACCEPTS` onto the enum's type object, which has no class to find it
/// in: its candidates are tried first, and the type check answers when none
/// binds the topic.
// Cost: the type check's, O(m), m = classes in the topic's MRO; plus a user
// candidate's call.
fn type_object(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Answer {
    let ValueView::Package(name) = target.view() else {
        return None;
    };
    let resolved = name.resolve();
    let base = crate::runtime::types::strip_type_smiley(&resolved).0;
    if interp.has_user_method_including_role(base, "ACCEPTS") {
        return None;
    }
    if let Some(result) = interp.dispatch_enum_role_method(target, "ACCEPTS", args) {
        return Some(result);
    }
    Some(Ok(Value::truth(interp.smart_match(&args[0], target))))
}
