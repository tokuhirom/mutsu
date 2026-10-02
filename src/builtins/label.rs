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

use crate::symbol::Symbol;
use crate::value::{AttrMap, Control, RuntimeError, Value, ValueView};

/// The class name a label value is an instance of.
pub(crate) const LABEL_CLASS: &str = "Label";

/// Build the `Label` value for a declaration.
// Cost: O(1) (five attribute inserts).
pub(crate) fn make_label(
    name: &str,
    file: &str,
    line: i64,
    prematch: &str,
    postmatch: &str,
) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("name".to_string(), Value::str(name.to_string()));
    attrs.insert("file".to_string(), Value::str(file.to_string()));
    attrs.insert("line".to_string(), Value::int(line));
    attrs.insert("prematch".to_string(), Value::str(prematch.to_string()));
    attrs.insert("postmatch".to_string(), Value::str(postmatch.to_string()));
    Value::make_instance(Symbol::intern(LABEL_CLASS), attrs)
}

fn attr_str(attributes: &AttrMap, key: &str) -> String {
    attributes
        .get(key)
        .map(Value::to_string_value)
        .unwrap_or_default()
}

/// The name a `Label` value carries, if `value` is one.
// Cost: O(1).
pub(crate) fn label_name(value: &Value) -> Option<String> {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == LABEL_CLASS => Some(attr_str(&attributes.as_map(), "name")),
        _ => None,
    }
}

/// `Label.Str`: `NAME FILE:LINE`.
// Cost: O(n), n = length of the rendered string.
pub(crate) fn label_str(attributes: &AttrMap) -> String {
    format!(
        "{} {}:{}",
        attr_str(attributes, "name"),
        attr_str(attributes, "file"),
        attr_str(attributes, "line")
    )
}

/// `Label.gist`: the label with the source around its declaration, as
/// Rakudo renders it (uncolored): `Label<FOO>(at FILE:LINE, 'PRE<HERE>FOOPOST')`.
// Cost: O(n), n = length of the rendered string.
pub(crate) fn label_gist(attributes: &AttrMap) -> String {
    let name = attr_str(attributes, "name");
    format!(
        "Label<{name}>(at {}:{}, '{}<HERE>{name}{}')",
        attr_str(attributes, "file"),
        attr_str(attributes, "line"),
        attr_str(attributes, "prematch"),
        attr_str(attributes, "postmatch"),
    )
}

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
            RuntimeError::new(format!(
                "Cannot resolve caller {word}({}); none of these signatures matches:\n    ( --> Nil)\n    (Label:D $x --> Nil)",
                types.join(", ")
            ))
        }
    }
}
