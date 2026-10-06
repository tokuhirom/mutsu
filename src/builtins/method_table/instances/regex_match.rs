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
