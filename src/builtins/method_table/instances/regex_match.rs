//! `Match`'s rows (ADR-11276 §9.18).
//!
//! A regex match is a lazy value (`ValueRepr::Match`: a capture node and the
//! shared subject) until something asks for its structure. The scalar
//! accessors (`from`, `to`, `pos`, `Str`, `Bool`, `orig`, `made`, ...) answer
//! straight from the node through the seam accessors of `value::match_view`,
//! so a call to one of them does not materialize the match; the structural
//! ones (`gist`, `raku`, `caps`, `chunks`, `actions`) read the attribute map
//! and so do (that is the same cost the cascade paid). A handler therefore
//! never reads its receiver through `view()` unless it needs the map.
//!
//! Only a plain `Match` has the shape: a grammar cursor reports the grammar's
//! own class and may override any of these, and an instance of a `Match`
//! subclass has another class name. The cascade's `Match` blocks keep
//! answering those receivers, calling these handlers.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::methods_0arg::match_helpers;
use crate::value::{AttrMap, RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Match",
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("from", from_row),
    row!("to", to_row),
    row!("pos", pos_row),
    row!("Str", str_row),
    row!("Bool", bool_row),
    row!("orig", orig_row),
    row!("target", orig_row),
    row!("made", made_row),
    row!("ast", made_row),
    row!("clone", clone_row),
    row!("prematch", prematch_row),
    row!("postmatch", postmatch_row),
    row!("actions", actions_row),
    row!("caps", caps_row),
    row!("chunks", chunks_row),
    row!("gist", gist_row),
    row!("raku", raku_row),
    // The numeric and text views of the matched text: what `Cool` gives any
    // value, applied to `Match.Str`, so the answers are the string's own
    // (`"42".Int`, a `Failure` for `"b".Int`).
    row!("Int", int_row),
    row!("Num", num_row),
    row!("Numeric", numeric_row),
    row!("chars", chars_row),
    row!("not", not_row),
    row!("WHICH", which_row),
    MethodRow {
        owner: "Match",
        name: "replace-with",
        arity: 1,
        handler: Handler::Narrow(replace_with_row),
        flags: RowFlags::ANY_ARGS,
        named: &[],
    },
];

/// `Match.from`: the offset the match starts at, in graphemes.
// Cost: O(1) for an ASCII subject; O(p) otherwise, p = chars before the match.
pub(crate) fn from(target: &Value) -> Value {
    Value::int(match_helpers::match_value_from(target))
}

/// `Match.to`: the offset the match ends at, in graphemes.
// Cost: O(1) for an ASCII subject; O(p) otherwise, p = chars before the end.
pub(crate) fn to(target: &Value) -> Value {
    Value::int(match_helpers::match_value_to(target))
}

/// `Match.pos`: where the match left the cursor, in graphemes.
// Cost: O(1) for an ASCII subject; O(p) otherwise, p = chars before it.
pub(crate) fn pos(target: &Value) -> Value {
    Value::int(match_helpers::match_visible_pos(
        target,
        target.match_pos().unwrap_or(0),
    ))
}

/// `Match.Str`: the matched text.
// Cost: O(k), k = chars matched (copied out of the shared subject).
pub(crate) fn str_value(target: &Value) -> Value {
    target
        .match_str_value()
        .unwrap_or_else(|| Value::str(String::new()))
}

/// `Match.Bool`: a failed `.subparse` match is false; every other match is
/// true, an empty one included.
// Cost: O(1).
pub(crate) fn truthiness(target: &Value) -> Value {
    Value::truth(!target.match_is_failed())
}

/// `Match.orig` (and `target`): the string the match ran against.
// Cost: O(1) (the shared subject).
pub(crate) fn orig(target: &Value) -> Value {
    target
        .match_orig()
        .unwrap_or_else(|| Value::str(String::new()))
}

/// `Match.made` (and `ast`): the made value, `Nil` when there is none.
// Cost: O(1).
pub(crate) fn made(target: &Value) -> Value {
    target.match_ast().unwrap_or(Value::NIL)
}

/// `Match.prematch`: the text before the match.
// Cost: O(p), p = chars of the prefix, for a lazy match (sliced from its shared
// subject); O(n), n = chars of `.orig`, for a rebuilt eager one.
pub(crate) fn prematch(target: &Value) -> Value {
    if let Some(pre) = target.match_side_text(true) {
        return Value::str(pre);
    }
    if let Some(orig) = target.match_orig() {
        let orig = orig.to_string_value();
        let from = target.match_from().unwrap_or(0).max(0) as usize;
        let chars: Vec<char> = orig.chars().collect();
        return Value::str(chars[..from.min(chars.len())].iter().collect::<String>());
    }
    Value::str(String::new())
}

/// `Match.postmatch`: the text after the match.
// Cost: O(s), s = chars of the suffix, for a lazy match (sliced from its shared
// subject); O(n), n = chars of `.orig`, for a rebuilt eager one.
pub(crate) fn postmatch(target: &Value) -> Value {
    if let Some(post) = target.match_side_text(false) {
        return Value::str(post);
    }
    if let Some(orig) = target.match_orig() {
        let orig = orig.to_string_value();
        let to = target.match_to().unwrap_or(0).max(0) as usize;
        let chars: Vec<char> = orig.chars().collect();
        return Value::str(chars[to.min(chars.len())..].iter().collect::<String>());
    }
    Value::str(String::new())
}

/// `Match.actions`: the actions object a grammar parse ran with, `Nil` when
/// there is none.
// Cost: O(a), a = attributes of the match.
pub(crate) fn actions(attributes: &AttrMap) -> Value {
    attributes.get("actions").cloned().unwrap_or(Value::NIL)
}

/// `Match.caps`: the captures in order, as `Pair`s.
// Cost: O(c log c), c = captures (sorted by position).
pub(crate) fn caps(attributes: &AttrMap) -> Value {
    match_helpers::match_caps(attributes)
}

/// `Match.chunks`: the matched and unmatched stretches in order.
// Cost: O(c log c + n), c = captures, n = chars of the match.
pub(crate) fn chunks(attributes: &AttrMap) -> Value {
    match_helpers::match_chunks(attributes)
}

/// `Match.gist`: the matched text in corner brackets, then the positional and
/// named captures nested.
// Cost: O(c + n), c = captures (recursively), n = chars of the match.
pub(crate) fn gist(attributes: &AttrMap) -> Value {
    Value::str(crate::runtime::utils::match_gist(attributes, 0))
}

/// `Match.raku`: the `Match.new(...)` constructor call that rebuilds it.
// Cost: O(c + n), c = captures (recursively), n = chars of the match.
pub(crate) fn raku(attributes: &AttrMap) -> Value {
    Value::str(match_helpers::match_raku_repr(attributes))
}

fn from_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(from(target))
}

fn to_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(to(target))
}

fn pos_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(pos(target))
}

fn str_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(str_value(target))
}

fn bool_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(truthiness(target))
}

fn orig_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(orig(target))
}

fn made_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(made(target))
}

/// `Match.clone`: a match is immutable, so a value clone (sharing the
/// attribute storage) is a correct clone that keeps every capture.
// Cost: O(1).
fn clone_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(target.clone())
}

fn prematch_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(prematch(target))
}

fn postmatch_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(postmatch(target))
}

/// A row whose handler reads the attribute map, which materializes a lazy
/// match.
macro_rules! attr_row_fns {
    ($($row:ident => $handler:ident),* $(,)?) => {
        $(
            fn $row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
                match target.view() {
                    ValueView::Instance { attributes, .. } => Ok($handler(&attributes.as_map())),
                    _ => Err(RuntimeError::new(concat!(
                        "Match.",
                        stringify!($handler),
                        ": receiver is not a Match"
                    ))),
                }
            }
        )*
    };
}

attr_row_fns! {
    actions_row => actions,
    caps_row => caps,
    chunks_row => chunks,
    gist_row => gist,
    raku_row => raku,
}

/// A zero-argument method of the matched text, answered by the string's own.
// Cost: O(k) plus the string's method, k = chars matched.
fn of_matched_text(target: &Value, method: &str) -> Result<Value, RuntimeError> {
    let text = str_value(target);
    crate::builtins::native_method_0arg(&text, crate::symbol::Symbol::intern(method))
        .unwrap_or_else(|| {
            Err(RuntimeError::new(format!(
                "No such method '{method}' for invocant of type 'Match'"
            )))
        })
}

/// `Match.Int`: the matched text as an `Int`.
// Cost: O(k), k = chars matched.
fn int_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    of_matched_text(target, "Int")
}

/// `Match.Num`: the matched text as a `Num`.
// Cost: O(k), k = chars matched.
fn num_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    of_matched_text(target, "Num")
}

/// `Match.Numeric`: the matched text as a number.
// Cost: O(k), k = chars matched.
fn numeric_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    of_matched_text(target, "Numeric")
}

/// `Match.chars`: the length of the matched text, in graphemes.
// Cost: O(k), k = chars matched.
fn chars_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    of_matched_text(target, "chars")
}

/// `Match.not`: whether the match failed.
// Cost: O(1).
fn not_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(Value::truth(!truthiness(target).truthy()))
}

/// `Match.WHICH`: the identity of the match object.
// Cost: O(1).
fn which_row(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(crate::builtins::methods_0arg::which::which_of(target))
}

/// `Match.replace-with($replacement)`: the subject with the matched part
/// replaced, `Nil` for a failed match.
// Cost: O(n + r), n = subject chars copied and r = replacement chars.
pub(crate) fn replace_with(target: &Value, replacement: &Value) -> Option<Value> {
    if target.match_is_failed() {
        return Some(Value::NIL);
    }
    let before = target.match_side_text(true).or_else(|| {
        let orig = target.match_orig()?.to_string_value();
        let from = target.match_from()?.max(0) as usize;
        Some(orig.chars().take(from).collect())
    })?;
    let after = target.match_side_text(false).or_else(|| {
        let orig = target.match_orig()?.to_string_value();
        let to = target.match_to()?.max(0) as usize;
        Some(orig.chars().skip(to).collect())
    })?;
    Some(Value::str(format!(
        "{}{}{}",
        before,
        replacement.to_string_value(),
        after
    )))
}

fn replace_with_row(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let [replacement] = args else {
        return None;
    };
    replace_with(target, replacement).map(Ok)
}
