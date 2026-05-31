use crate::value::Value;
use std::collections::HashMap;

/// Construct the `.message` string for a structured exception from its class
/// name and attribute map.  Returns `None` when the class name is not
/// recognised (caller should fall back to the generic behaviour).
pub fn format_exception_message(
    class_name: &str,
    attrs: &HashMap<String, Value>,
) -> Option<String> {
    match class_name {
        "X::Str::Numeric" => {
            let reason = attr_str(attrs, "reason");
            let source = attr_str(attrs, "source");
            let pos = attrs.get("pos").map(|v| v.to_f64() as usize).unwrap_or(0);
            // Insert the ⏏ marker at position `pos` inside the source string.
            let marked = if pos <= source.len() {
                let mut s = String::with_capacity(source.len() + 3);
                s.push_str(&source[..pos]);
                s.push('\u{23CF}'); // ⏏
                s.push_str(&source[pos..]);
                s
            } else {
                format!("{}\u{23CF}", source)
            };
            Some(format!(
                "Cannot convert string to number: {} in '{}' (indicated by \u{23CF})",
                reason, marked
            ))
        }
        "X::Method::NotFound" => {
            let method = attr_str(attrs, "method");
            let typename = attr_str(attrs, "typename");
            Some(format!(
                "No such method '{}' for invocant of type '{}'",
                method, typename
            ))
        }
        "X::Undeclared" => {
            let what = attr_str_or(attrs, "what", "Variable");
            let symbol = attr_str(attrs, "symbol");
            let mut msg = format!("{} '{}' is not declared", what, symbol);
            // Append suggestions if present
            if let Some(sugg) = attrs.get("suggestions") {
                let items = suggestion_list(sugg);
                if !items.is_empty() {
                    if items.len() == 1 {
                        msg.push_str(&format!(". Did you mean '{}'?", items[0]));
                    } else {
                        let quoted: Vec<String> =
                            items.iter().map(|s| format!("'{}'", s)).collect();
                        msg.push_str(&format!(
                            ". Did you mean any of these: {}?",
                            quoted.join(", ")
                        ));
                    }
                }
            }
            Some(msg)
        }
        "X::Cannot::Lazy" => {
            let action = attr_str(attrs, "action");
            Some(format!("Cannot {} a lazy list", action))
        }
        "X::ControlFlow::Return" => Some("Attempt to return outside of any Routine".to_string()),
        "X::OutOfRange" => {
            let what = attr_str_or(attrs, "what", "Argument");
            let got = attr_str(attrs, "got");
            let range = attr_str(attrs, "range");
            let mut msg = format!("{} out of range. Is: {}, should be in {}", what, got, range);
            if let Some(comment) = attrs.get("comment") {
                let c = comment.to_string_value();
                if !c.is_empty() {
                    msg.push_str(&format!("; {}", c));
                }
            }
            Some(msg)
        }
        "X::Immutable" => {
            let typename = attr_str(attrs, "typename");
            let method = attr_str(attrs, "method");
            if method.is_empty() {
                Some(format!("Cannot modify an immutable {}", typename))
            } else {
                Some(format!(
                    "Cannot call '{}' on an immutable '{}'",
                    method, typename
                ))
            }
        }
        "X::Multi::NoMatch" => {
            let name = attr_str(attrs, "name");
            Some(format!(
                "Cannot resolve caller {}; none of these signatures match",
                name
            ))
        }
        "X::Multi::Ambiguous" => {
            let name = attr_str(attrs, "name");
            Some(format!(
                "Ambiguous call to '{}'; these signatures all match",
                name
            ))
        }
        "X::Redeclaration" => {
            let symbol = attr_str(attrs, "symbol");
            let what = attr_str_or(attrs, "what", "symbol");
            if what == "routine" {
                Some(format!(
                    "Redeclaration of {} '{}'. Did you mean to declare a multi-sub?",
                    what, symbol
                ))
            } else {
                Some(format!("Redeclaration of {} '{}'.", what, symbol))
            }
        }
        "X::StubCode" => Some("Stub code executed".to_string()),
        "X::Bind" => {
            let target = attr_str(attrs, "target");
            if target.is_empty() {
                Some("Cannot use bind operator with this left-hand side".to_string())
            } else {
                Some(format!("Cannot bind to {}", target))
            }
        }
        "X::Match::Bool" => {
            let type_name = attr_str_or(attrs, "type", "");
            Some(format!(
                "Cannot use Bool as Matcher with '{}'. Did you mean to use $_ inside a block?",
                type_name
            ))
        }
        "X::Assignment::RO" => {
            if let Some(val) = attrs.get("value") {
                let type_name = value_type_label(val);
                let val_str = val.to_string_value();
                Some(format!(
                    "Cannot modify an immutable {} ({})",
                    type_name, val_str
                ))
            } else {
                Some("Cannot modify an immutable value".to_string())
            }
        }
        "X::NYI" => {
            let feature = attr_str(attrs, "feature");
            Some(format!("{} not yet implemented. Sorry.", feature))
        }
        "X::Signature::Placeholder" => {
            Some("Placeholder variable cannot override an existing signature".to_string())
        }
        "X::IO::Closed" => {
            let op = attr_str(attrs, "operation");
            Some(format!("Cannot do '{}' on a closed handle", op))
        }
        _ => None,
    }
}

/// Extract a string attribute, defaulting to "" if absent.
fn attr_str(attrs: &HashMap<String, Value>, key: &str) -> String {
    attrs
        .get(key)
        .map(|v| v.to_string_value())
        .unwrap_or_default()
}

/// Extract a string attribute with a custom default.
fn attr_str_or(attrs: &HashMap<String, Value>, key: &str, default: &str) -> String {
    attrs
        .get(key)
        .map(|v| v.to_string_value())
        .unwrap_or_else(|| default.to_string())
}

/// Extract a list of suggestion strings from a Value.
fn suggestion_list(v: &Value) -> Vec<String> {
    match v {
        Value::Array(items, _) => items.iter().map(|item| item.to_string_value()).collect(),
        Value::Nil => Vec::new(),
        other => {
            let s = other.to_string_value();
            if s.is_empty() { Vec::new() } else { vec![s] }
        }
    }
}

/// Get a human-readable type label for a value (used by X::Assignment::RO).
fn value_type_label(v: &Value) -> String {
    match v {
        Value::Int(_) => "Int".to_string(),
        Value::Num(_) => "Num".to_string(),
        Value::Str(_) => "Str".to_string(),
        Value::Bool(_) => "Bool".to_string(),
        Value::Rat(_, _) => "Rat".to_string(),
        Value::Complex(_, _) => "Complex".to_string(),
        Value::Instance { class_name, .. } => class_name.resolve(),
        _ => "Any".to_string(),
    }
}
