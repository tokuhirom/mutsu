//! `ACCEPTS` of the open shapes, `Uni`, `Blob` and `Any` (ADR-11276 §9.108).
//!
//! Rakudo declares `ACCEPTS` on `Mu` (the type check, which a type object
//! answers through the interpreter), on `Any` (`self === topic`), and again on
//! the types whose smartmatch is not identity: `Numeric` (and `Int`, which
//! agrees with it), `Str`, `Bool`, `List`, `Map`, `Date`, `DateTime`, `Seq`,
//! `IO::Path`, `Match`, `Uni` and `Blob`. Every open shape reaches the row of the
//! first of those in its MRO, so `Any.ACCEPTS` answers only the receivers
//! that declare nothing closer: a `Capture` (through
//! `ANY_LIST_FOR_CLOSED_SHAPES`) and an instance of a user class (through
//! `answer_shapeless`).
//!
//! The rows of the open shapes are the smartmatch operator's answers
//! (`$topic ~~ self`), which is the one implementation of their candidates;
//! `Capture` and `Uni` are not, because the operator does not agree with
//! Rakudo there (`1 ~~ \(1)` and `"abc" ~~ "abc".NFC` are `False`).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $handler:expr) => {
        MethodRow {
            owner: $owner,
            name: "ACCEPTS",
            arity: 1,
            handler: $handler,
            flags: RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Numeric", Handler::Interp(smartmatch)),
    row!("Str", Handler::Interp(smartmatch)),
    row!("List", Handler::Interp(smartmatch)),
    row!("Map", Handler::Interp(smartmatch)),
    row!("Date", Handler::Interp(smartmatch)),
    row!("DateTime", Handler::Interp(smartmatch)),
    row!("Seq", Handler::Interp(smartmatch)),
    row!("IO::Path", Handler::Interp(smartmatch)),
    row!("Bool", Handler::Narrow(bool_accepts)),
    row!("Uni", Handler::Narrow(uni_accepts)),
    row!("Blob", Handler::Interp(blob_accepts)),
    row!("Buf", Handler::Interp(blob_accepts)),
    row!("Match", Handler::Narrow(match_accepts)),
    row!("Any", Handler::Narrow(any_accepts)),
];

/// `Numeric.ACCEPTS`, `Str.ACCEPTS`, `List.ACCEPTS`, `Map.ACCEPTS`,
/// `Date.ACCEPTS`, `DateTime.ACCEPTS`, `Seq.ACCEPTS` and `IO::Path.ACCEPTS`:
/// the smartmatch of the topic against the receiver.
// Cost: the smartmatch's: O(1) for a scalar, O(n) for a string or a list,
// n = its length.
fn smartmatch(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    // An exception raised while matching (a Blob topic against a Str
    // receiver dies `X::Buf::AsStr`) propagates.
    // A user class composing `Real` compares through the operator, which only
    // the interpreter's smartmatch does (#13894).
    if !crate::builtins::method_table::is_user_real_instance(interp, target)
        && let Some(pure) = crate::vm::vm_smart_match::pure_smart_match(&args[0], target)
    {
        return Some(Ok(Value::truth(pure)));
    }
    Some(interp.try_smart_match(&args[0], target).map(Value::truth))
}

/// `$topic ~~ $matcher` as the operator decides it for a matcher with no
/// user `ACCEPTS`: the pure comparison first (which reads an enum value as
/// its underlying value, as `Enumeration.ACCEPTS` does), then the
/// interpreter's smartmatch.
// Cost: the smartmatch's.
pub(crate) fn accepts_by_smartmatch(
    interp: &mut Interpreter,
    topic: &Value,
    matcher: &Value,
) -> bool {
    crate::vm::vm_smart_match::pure_smart_match(topic, matcher)
        .unwrap_or_else(|| interp.smart_match(topic, matcher))
}

/// `Bool.ACCEPTS(Mu \topic)`: the receiver, whatever the topic
/// (`True ~~ False` is `False`, anything `~~ True` is `True`).
// Cost: O(1).
fn bool_accepts(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.descalarize().view() {
        ValueView::Bool(b) => Some(Ok(Value::truth(b))),
        _ => None,
    }
}

/// `Uni.ACCEPTS(Uni:D \topic)`: the same codepoints, whatever the
/// normalization form of either (`"ab".NFC.ACCEPTS("ab".NFKC)` is `True`,
/// `"é".NFD.ACCEPTS("é".NFC)` is `False`). Any other topic is `Any`'s
/// identity test, which a `Uni` receiver never passes.
// Cost: O(c), c = codepoints of the shorter `Uni`.
fn uni_accepts(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Uni(mine) = target.descalarize().view() else {
        return None;
    };
    let same = match args[0].descalarize().view() {
        ValueView::Uni(theirs) => mine.codepoints() == theirs.codepoints(),
        _ => false,
    };
    Some(Ok(Value::truth(same)))
}

/// `Blob.ACCEPTS(Blob:D \topic)`: the same elements, as the smartmatch
/// compares two buffers. Any other topic is `Any`'s identity test, which a
/// buffer receiver never passes.
// Cost: O(e), e = elements of the shorter buffer.
fn blob_accepts(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let topic = args[0].descalarize();
    let is_buffer = matches!(topic.view(), ValueView::Instance { attributes, .. }
        if crate::value::value_buf::has_buf_elems(&attributes));
    Some(Ok(Value::truth(
        is_buffer && interp.smart_match(topic, target),
    )))
}

/// `Match.ACCEPTS(Mu $)`: the receiver itself, whatever the topic (a `Match`
/// smartmatched against is the match already made, as `when` reads it).
// Cost: O(1).
fn match_accepts(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    target
        .descalarize()
        .is_match_instance()
        .then(|| Ok(target.descalarize().clone()))
}

/// `Any.ACCEPTS(Mu \topic)`: `self === topic` (`\(1).ACCEPTS(\(1))` is
/// `True`, `\(1).ACCEPTS(1)` is `False`).
// Cost: as `===`: O(1) for a scalar or an instance, O(k) for a `Capture`,
// k = its elements.
fn any_accepts(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::truth(crate::value::identity::values_identical(
        target, &args[0],
    ))))
}
