//! `Backtrace`'s and `Backtrace::Frame`'s rows (ADR-11276 §9.35).
//!
//! Both are built-in classes whose instances carry their state as attributes:
//! a `Backtrace` has a `frames` list of `Backtrace::Frame` instances and its
//! rendered `text`, a frame has `subname`, `file`, `line` and the
//! `is-hidden`/`is-setting` markers the backtrace builder stamps
//! (`vm::vm_helpers::build_backtrace_value`). Every handler reads those
//! attributes, so they are pure rows; the rendering itself is
//! [`crate::builtins::backtrace_methods`], which the rows share with the
//! callers that build a backtrace's text.
//!
//! Both shapes are closed: a user subclass has another class name and no
//! shape, and `elems`, `List`, `Seq`, `Stringy` (the ancestor rows of `Any`
//! and `Mu`, answered from the `frames` list) stay with the cascade until a
//! slice audits them for the shapes.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::backtrace_methods::{
    dispatch, frame_code_value, frame_is_routine, frame_str, frames_of,
};
use crate::builtins::method_table::Named;
use crate::gc::Gc;
use crate::value::{InstanceAttrs, RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

/// The answer of the row for `method` called with `args` on a `Backtrace` or
/// `Backtrace::Frame` instance, for the cascades that a call reaches when the
/// table's own entry did not take it (the debug cross-check runs them on every
/// row's call; a generic instance fallback would answer such a call wrongly).
/// The arguments are split as the guard step splits them: a string-keyed pair
/// is a named argument.
// Cost: O(r + a), r = rows of this group, a = arguments.
pub(crate) fn answer(target: &Value, method: &str, args: &[Value]) -> Answer {
    let ValueView::Instance { class_name, .. } = target.view() else {
        return None;
    };
    let owner = match class_name.resolve().as_str() {
        "Backtrace" => "Backtrace",
        "Backtrace::Frame" => "Backtrace::Frame",
        _ => return None,
    };
    let (named, positional): (Vec<Value>, Vec<Value>) = args
        .iter()
        .cloned()
        .partition(|arg| arg.is_string_pair_value());
    let row = ROWS.iter().find(|row| {
        row.owner == owner && row.name == method && usize::from(row.arity) == positional.len()
    })?;
    if named.iter().any(|pair| match pair.view() {
        ValueView::Pair(key, _) => !row.named.contains(&key.as_str()),
        _ => true,
    }) {
        return None;
    }
    match row.handler {
        Handler::Narrow(f) if named.is_empty() => f(target, &positional),
        Handler::Named(f) => f(target, &positional, Named::new(&named)),
        _ => None,
    }
}

const NICE_NAMED: &[&str] = &["oneline"];
const NEXT_NAMED: &[&str] = &["named", "noproto", "setting"];

pub(super) static ROWS: &[MethodRow] = &[
    super::narrow_row!("Backtrace", "Str", 0, str_row),
    super::narrow_row!("Backtrace", "gist", 0, gist_row),
    super::narrow_row!("Backtrace", "list", 0, list_row),
    super::narrow_row!("Backtrace", "flat", 0, list_row),
    super::narrow_row!("Backtrace", "full", 0, full_row),
    super::narrow_row!("Backtrace", "concise", 0, concise_row),
    super::narrow_row!("Backtrace", "summary", 0, summary_row),
    super::narrow_row!("Backtrace", "is-runtime", 0, is_runtime_row),
    super::narrow_row!("Backtrace", "AT-POS", 1, at_pos_row),
    super::narrow_row!("Backtrace", "outer-caller-idx", 1, outer_caller_idx_row),
    named_row("nice", 0, nice_row, NICE_NAMED),
    // The one-argument cascade took any single argument (`:oneline` included)
    // and the recognition table claims the arity.
    named_row("nice", 1, nice_row, NICE_NAMED),
    named_row("next-interesting-index", 0, next_row, NEXT_NAMED),
    named_row("next-interesting-index", 1, next_row, NEXT_NAMED),
    named_row("next-interesting-index", 2, next_row, NEXT_NAMED),
    super::narrow_row!("Backtrace::Frame", "subname", 0, subname_row),
    super::narrow_row!("Backtrace::Frame", "file", 0, file_row),
    super::narrow_row!("Backtrace::Frame", "line", 0, line_row),
    super::narrow_row!("Backtrace::Frame", "Str", 0, frame_str_row),
    super::narrow_row!("Backtrace::Frame", "code", 0, code_row),
    super::narrow_row!("Backtrace::Frame", "is-routine", 0, is_routine_row),
    super::narrow_row!("Backtrace::Frame", "is-hidden", 0, is_hidden_row),
    super::narrow_row!("Backtrace::Frame", "is-setting", 0, is_setting_row),
];

const fn named_row(
    name: &'static str,
    arity: u8,
    handler: fn(&Value, &[Value], Named<'_>) -> Answer,
    named: &'static [&'static str],
) -> MethodRow {
    MethodRow {
        owner: "Backtrace",
        name,
        arity,
        handler: Handler::Named(handler),
        flags: RowFlags::NONE,
        named,
    }
}

fn attrs_of(target: &Value) -> Option<Gc<InstanceAttrs>> {
    match target.view() {
        ValueView::Instance { attributes, .. } => Some(attributes.clone()),
        _ => None,
    }
}

/// An attribute of the instance, or `default` when it has none.
fn attr_or(target: &Value, key: &str, default: Value) -> Answer {
    let attributes = attrs_of(target)?;
    Some(Ok(attributes.as_map().get(key).cloned().unwrap_or(default)))
}

/// `Backtrace.Str`: the text the builder rendered.
// Cost: O(a), a = attributes of the instance (one copy of the text).
fn str_row(target: &Value, _args: &[Value]) -> Answer {
    let attributes = attrs_of(target)?;
    let text = attributes
        .as_map()
        .get("text")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    Some(Ok(Value::str(text)))
}

/// `Backtrace.gist`: `Backtrace(3 frames)`.
// Cost: O(f), f = frames.
fn gist_row(target: &Value, _args: &[Value]) -> Answer {
    let attributes = attrs_of(target)?;
    let count = attributes
        .as_map()
        .get("frames")
        .map(|v| crate::runtime::utils::value_to_list(v).len())
        .unwrap_or(0);
    let noun = if count == 1 { "frame" } else { "frames" };
    Some(Ok(Value::str(format!("Backtrace({count} {noun})"))))
}

/// `Backtrace.list` (and `flat`): the frames.
// Cost: O(a), a = attributes of the instance.
fn list_row(target: &Value, _args: &[Value]) -> Answer {
    attr_or(target, "frames", Value::array(vec![]))
}

/// `Backtrace.full`: every frame, one per line (each frame's `.Str` is
/// newline-terminated, as in Rakudo). mutsu tracks no hidden or setting frames
/// in this list, so it is the frame list verbatim.
// Cost: O(f + t), f = frames, t = characters rendered.
fn full_row(target: &Value, _args: &[Value]) -> Answer {
    let attributes = attrs_of(target)?;
    Some(Ok(Value::str(render(&attributes, |_| true))))
}

/// `Backtrace.concise`: the routine frames that are not setting frames.
// Cost: O(f + t), f = frames, t = characters rendered.
fn concise_row(target: &Value, _args: &[Value]) -> Answer {
    let attributes = attrs_of(target)?;
    Some(Ok(Value::str(render(&attributes, |fa| {
        frame_is_routine(fa) && !is_setting(fa)
    }))))
}

/// `Backtrace.summary`: the frames that are routines or not setting frames.
// Cost: O(f + t), f = frames, t = characters rendered.
fn summary_row(target: &Value, _args: &[Value]) -> Answer {
    let attributes = attrs_of(target)?;
    Some(Ok(Value::str(render(&attributes, |fa| {
        frame_is_routine(fa) || !is_setting(fa)
    }))))
}

fn is_setting(frame: &Gc<InstanceAttrs>) -> bool {
    frame.as_map().get("is-setting").is_some_and(Value::truthy)
}

/// The text of the frames `keep` accepts. `frames_of` dereferences each
/// element: a `.grep` over a backtrace promotes its frames to shared cells.
// Cost: O(f + t), f = frames, t = characters rendered.
fn render(attributes: &Gc<InstanceAttrs>, keep: impl Fn(&Gc<InstanceAttrs>) -> bool) -> String {
    let mut out = String::new();
    for frame in frames_of(attributes) {
        if let ValueView::Instance { attributes: fa, .. } = frame.view()
            && keep(&fa)
        {
            out.push_str(&frame_str(&fa));
        }
    }
    out
}

/// `Backtrace.is-runtime`: whether the backtrace was captured while running
/// (the builder stamps the flag; a compile-time backtrace answers `False`).
// Cost: O(a), a = attributes of the instance.
fn is_runtime_row(target: &Value, _args: &[Value]) -> Answer {
    let attributes = attrs_of(target)?;
    let is_runtime = attributes
        .as_map()
        .get("is-runtime")
        .is_some_and(|v| v.truthy());
    Some(Ok(Value::truth(is_runtime)))
}

/// `Backtrace.AT-POS($i)`: the frame at that position, `Nil` out of range.
// Cost: O(f), f = frames.
fn at_pos_row(target: &Value, args: &[Value]) -> Answer {
    let idx = match crate::builtins::method_table::positional::at_pos_index(&args[0]) {
        Ok(idx) => idx,
        Err(answer) => return answer,
    };
    let attributes = attrs_of(target)?;
    let frames = attributes
        .as_map()
        .get("frames")
        .cloned()
        .unwrap_or(Value::NIL);
    Some(Ok(crate::runtime::utils::value_to_list(&frames)
        .get(idx)
        .cloned()
        .unwrap_or(Value::NIL)))
}

/// `Backtrace.outer-caller-idx($startidx)`. The `Int` is mandatory, so the
/// zero-argument call has no row.
// Cost: O(f), f = frames.
fn outer_caller_idx_row(target: &Value, args: &[Value]) -> Answer {
    dispatch(&attrs_of(target)?, "outer-caller-idx", args)
}

/// `nice(:oneline)` and `next-interesting-index($i, :named, :setting)`: the
/// introspection helpers read their flags as the call's pairs.
fn introspect(target: &Value, method: &str, args: &[Value], named: Named<'_>) -> Answer {
    let all: Vec<Value> = args.iter().chain(named.pairs()).cloned().collect();
    dispatch(&attrs_of(target)?, method, &all)
}

// Cost: O(f + t), f = frames, t = characters rendered.
fn nice_row(target: &Value, args: &[Value], named: Named<'_>) -> Answer {
    introspect(target, "nice", args, named)
}

// Cost: O(f), f = frames.
fn next_row(target: &Value, args: &[Value], named: Named<'_>) -> Answer {
    introspect(target, "next-interesting-index", args, named)
}

// Cost: O(a), a = attributes of the instance.
fn subname_row(target: &Value, _args: &[Value]) -> Answer {
    attr_or(target, "subname", Value::str(String::new()))
}

// Cost: O(a), a = attributes of the instance.
fn file_row(target: &Value, _args: &[Value]) -> Answer {
    attr_or(target, "file", Value::str(String::new()))
}

// Cost: O(a), a = attributes of the instance.
fn line_row(target: &Value, _args: &[Value]) -> Answer {
    attr_or(target, "line", Value::int(0))
}

// Cost: O(a), a = attributes of the instance.
fn is_hidden_row(target: &Value, _args: &[Value]) -> Answer {
    attr_or(target, "is-hidden", Value::FALSE)
}

// Cost: O(a), a = attributes of the instance.
fn is_setting_row(target: &Value, _args: &[Value]) -> Answer {
    attr_or(target, "is-setting", Value::FALSE)
}

// Cost: O(a), a = attributes of the instance.
fn frame_str_row(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::str(frame_str(&attrs_of(target)?))))
}

// Cost: O(a), a = attributes of the instance.
fn code_row(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(frame_code_value(&attrs_of(target)?)))
}

// Cost: O(a), a = attributes of the instance.
fn is_routine_row(target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::truth(frame_is_routine(&attrs_of(target)?))))
}
