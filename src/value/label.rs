//! The `Label` value: what a loop label term evaluates to (see
//! `builtins/label.rs` for its methods and the loop-control routine forms).
//! Construction and the `Str`/`gist` renderings live here, in the value layer,
//! because the parser builds the value and `Value`'s own display renders it.

use crate::symbol::Symbol;
use crate::value::{AttrMap, Value, ValueView};

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

pub(crate) fn attr_str(attributes: &AttrMap, key: &str) -> String {
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
