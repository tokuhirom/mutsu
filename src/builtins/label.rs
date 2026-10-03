//! `Label`: the first-class value a loop label term evaluates to.
//!
//! `FOO: for ... { }` declares a label; the bareword `FOO` in term position
//! inside its scope is a `Label` object (`raku-doc/doc/Type/Label.rakudoc`), not
//! a string. It carries `name`, `file` and `line`, and the 20 characters of
//! source on either side of the label's name that its `.gist` quotes. The value
//! is built once, when the parser sees the declaration, so every reference to
//! one label is the same object.
//!
//! A `Label` drives loop control dynamically: `FOO.next`, `next(FOO)` and
//! `next |c` (a capture holding `FOO`) raise the same labelled control signal
//! as the static `next FOO`, so it also works from a routine called inside the
//! labelled loop. The signal matches the loop by label name, exactly as the
//! static form does.

use crate::value::label::{LABEL_CLASS, attr_str, label_gist, label_name, label_str};
use crate::value::{AttrMap, Control, RuntimeError, Value, ValueView};

/// `Label.raku`: `Label.new(name => "FOO", file => "FILE", line => LINE)`.
// Cost: O(n), n = length of the rendered string.
pub(crate) fn label_raku(attributes: &AttrMap) -> String {
    let quote = |s: String| crate::builtins::methods_0arg::raku_repr::escape_raku_str(&s);
    format!(
        "Label.new(name => {}, file => {}, line => {})",
        quote(attr_str(attributes, "name")),
        quote(attr_str(attributes, "file")),
        attr_str(attributes, "line")
    )
}

/// The zero-argument methods of a `Label` instance; `None` for any other
/// receiver or method. `.next` / `.last` / `.redo` raise the labelled loop
/// control signal (or `X::ControlFlow` with no loop to act on).
// Cost: O(n) for the rendering methods, n = rendered length; O(1) otherwise.
pub(crate) fn label_method_0arg(
    target: &Value,
    method: &str,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    if class_name != LABEL_CLASS {
        return None;
    }
    let attributes = attributes.as_map();
    let control = |c: Control| {
        Some(Err(
            crate::runtime::loop_handler_depth::loop_control_signal(
                c,
                Some(attr_str(&attributes, "name")),
            ),
        ))
    };
    match method {
        "name" | "file" => Some(Ok(Value::str(attr_str(&attributes, method)))),
        "line" => Some(Ok(attributes.get("line").cloned().unwrap_or(Value::int(0)))),
        "Str" => Some(Ok(Value::str(label_str(&attributes)))),
        "gist" => Some(Ok(Value::str(label_gist(&attributes)))),
        "raku" | "perl" => Some(Ok(Value::str(label_raku(&attributes)))),
        "next" => control(Control::Next),
        "last" => control(Control::Last),
        "redo" => control(Control::Redo),
        _ => None,
    }
}

/// v6.e's `last VALUE` / `next VALUE` (`OpCode::LastValue` / `NextValue`): the
/// loop-control signal carrying `value` as the iteration's contribution to the
/// loop's result. A `Label` value is the labelled form spelled as an argument
/// (`last(FOO)`), which carries no value.
// Cost: O(1).
pub(crate) fn loop_control_value_signal(control: Control, value: Value) -> RuntimeError {
    if let Some(name) = label_name(&value) {
        return crate::runtime::loop_handler_depth::loop_control_signal(control, Some(name));
    }
    let mut sig = crate::runtime::loop_handler_depth::loop_control_signal(control, None);
    if crate::runtime::loop_handler_depth::loop_handler_in_scope() {
        sig.return_value = Some(value);
    }
    sig
}

/// The routine forms `next(LABEL)` / `last(LABEL)` / `redo(LABEL)` (also what
/// `next |c` slips into): zero arguments is the plain loop control, one `Label`
/// the labelled one. Anything else has no candidate, as in Rakudo.
// Cost: O(1).
pub(crate) fn loop_control_call(control: Control, args: &[Value]) -> RuntimeError {
    let word = match control {
        Control::Last => "last",
        Control::Next => "next",
        _ => "redo",
    };
    match args {
        [] => crate::runtime::loop_handler_depth::loop_control_signal(control, None),
        [arg] if let Some(name) = label_name(arg) => {
            crate::runtime::loop_handler_depth::loop_control_signal(control, Some(name))
        }
        _ => {
            let types: Vec<String> = args
                .iter()
                .map(|a| match a.view() {
                    ValueView::Package(name) => format!("{name}:U"),
                    _ => format!("{}:D", crate::runtime::utils::value_type_name(a)),
                })
                .collect();
            crate::runtime::Interpreter::multi_no_match_exception(
                word,
                format!(
                    "Cannot resolve caller {word}({}); none of these signatures matches:\n    ( --> Nil)\n    (Label:D $x --> Nil)",
                    types.join(", ")
                ),
            )
        }
    }
}
