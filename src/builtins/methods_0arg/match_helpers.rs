use crate::value::Value;
use std::collections::HashMap;

use super::raku_repr::raku_value;

/// Produce Raku-compatible `.raku` representation for a Match object.
pub(super) fn match_raku_repr(attributes: &HashMap<String, Value>) -> String {
    let orig = attributes
        .get("orig")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let from = match attributes.get("from") {
        Some(Value::Int(n)) => *n,
        _ => 0,
    };
    let to = match attributes.get("to") {
        Some(Value::Int(n)) => *n,
        _ => 0,
    };

    let escaped_orig = orig
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\t', "\\t")
        .replace('\r', "\\r")
        .replace('\0', "\\0");

    let mut parts = vec![
        format!(":orig(\"{}\")", escaped_orig),
        format!(":from({})", from),
        format!(":pos({})", to),
    ];

    // Positional captures (:list)
    if let Some(Value::Array(items, ..)) = attributes.get("list")
        && !items.is_empty()
    {
        let items_raku: Vec<String> = items.iter().map(value_raku_repr).collect();
        let trailing = if items.len() == 1 { "," } else { "" };
        parts.push(format!(":list(({}{})", items_raku.join(", "), trailing));
    }

    // Named captures (:hash)
    if let Some(Value::Hash(map, ..)) = attributes.get("named")
        && !map.is_empty()
    {
        let mut pairs: Vec<String> = map
            .iter()
            .map(|(k, v)| format!("(:{}({}))", k, value_raku_repr(v)))
            .collect();
        pairs.sort(); // Deterministic output
        parts.push(format!(":hash(Map.new({}))", pairs.join(", ")));
    }

    format!("Match.new({})", parts.join(", "))
}

/// Produce `.raku` representation for a value, recursing into Match objects.
fn value_raku_repr(val: &Value) -> String {
    match val {
        Value::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Match" => match_raku_repr(attributes),
        Value::Array(items, ..) => {
            let items_raku: Vec<String> = items.iter().map(value_raku_repr).collect();
            format!("[{}]", items_raku.join(", "))
        }
        other => {
            // Use the existing raku_value helper for non-Match values
            raku_value(other)
        }
    }
}

/// Extract the `from` position from a Match object value.
pub(super) fn match_value_from(val: &Value) -> i64 {
    if let Value::Instance { attributes, .. } = val
        && let Some(Value::Int(n)) = attributes.get("from")
    {
        return *n;
    }
    0
}

/// Extract the `to` position from a Match object value.
pub(super) fn match_value_to(val: &Value) -> i64 {
    if let Value::Instance { attributes, .. } = val
        && let Some(Value::Int(n)) = attributes.get("to")
    {
        return *n;
    }
    0
}

/// Collect all captures from a Match object sorted by position.
/// Positional captures use `ValuePair(Int => Match)` and named captures use
/// `Pair(Str => Match)` to preserve the Raku-visible key types.
pub(super) fn match_caps(attributes: &HashMap<String, Value>) -> Value {
    let mut pairs: Vec<(i64, Value)> = Vec::new();

    // Collect named capture positions to filter out shadowed positional captures
    let mut named_positions: Vec<(i64, i64)> = Vec::new();
    if let Some(Value::Hash(named, ..)) = attributes.get("named") {
        for (_key, val) in named.iter() {
            for item in expand_capture_items(val) {
                named_positions.push((match_value_from(item), match_value_to(item)));
            }
        }
    }

    // Collect positional captures, expanding quantified arrays
    if let Some(Value::Array(items, ..)) = attributes.get("list") {
        for (i, val) in items.iter().enumerate() {
            for item in expand_capture_items(val) {
                let from = match_value_from(item);
                let to = match_value_to(item);
                let shadowed = named_positions
                    .iter()
                    .any(|(nf, nt)| *nf == from && *nt == to);
                if !shadowed {
                    pairs.push((
                        from,
                        Value::ValuePair(Box::new(Value::Int(i as i64)), Box::new(item.clone())),
                    ));
                }
            }
        }
    }

    // Collect named captures, expanding quantified arrays
    if let Some(Value::Hash(named, ..)) = attributes.get("named") {
        for (key, val) in named.iter() {
            for item in expand_capture_items(val) {
                let from = match_value_from(item);
                pairs.push((from, Value::Pair(key.clone(), Box::new(item.clone()))));
            }
        }
    }

    // Sort by position
    pairs.sort_by_key(|(from, _)| *from);

    Value::array(pairs.into_iter().map(|(_, pair)| pair).collect())
}

/// Expand a capture value: if it's an Array of Matches (from quantified captures),
/// return each element; otherwise return the single value.
fn expand_capture_items(val: &Value) -> Vec<&Value> {
    match val {
        Value::Array(items, _) if items.iter().all(|v| matches!(v, Value::Instance { .. })) => {
            items.iter().collect()
        }
        _ => vec![val],
    }
}

/// Like `.caps` but also includes non-captured text between captures as `~ => text` pairs.
pub(super) fn match_chunks(attributes: &HashMap<String, Value>) -> Value {
    // Collect named capture positions to filter out shadowed positional captures
    let mut named_positions: Vec<(i64, i64)> = Vec::new();
    if let Some(Value::Hash(named, ..)) = attributes.get("named") {
        for (_key, val) in named.iter() {
            for item in expand_capture_items(val) {
                named_positions.push((match_value_from(item), match_value_to(item)));
            }
        }
    }

    // Collect all captures with their from/to positions
    let mut captures: Vec<(i64, i64, Value)> = Vec::new();

    if let Some(Value::Array(items, ..)) = attributes.get("list") {
        for (i, val) in items.iter().enumerate() {
            for item in expand_capture_items(val) {
                let from = match_value_from(item);
                let to = match_value_to(item);
                let shadowed = named_positions
                    .iter()
                    .any(|(nf, nt)| *nf == from && *nt == to);
                if !shadowed {
                    captures.push((
                        from,
                        to,
                        Value::ValuePair(Box::new(Value::Int(i as i64)), Box::new(item.clone())),
                    ));
                }
            }
        }
    }

    if let Some(Value::Hash(named, ..)) = attributes.get("named") {
        for (key, val) in named.iter() {
            for item in expand_capture_items(val) {
                let from = match_value_from(item);
                let to = match_value_to(item);
                captures.push((from, to, Value::Pair(key.clone(), Box::new(item.clone()))));
            }
        }
    }

    // Sort by position
    captures.sort_by_key(|(from, _, _)| *from);

    let match_from = match attributes.get("from") {
        Some(Value::Int(n)) => *n,
        _ => 0,
    };
    let match_to = match attributes.get("to") {
        Some(Value::Int(n)) => *n,
        _ => 0,
    };
    let orig = attributes
        .get("orig")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let orig_chars: Vec<char> = orig.chars().collect();

    let mut result: Vec<Value> = Vec::new();
    let mut pos = match_from;

    for (from, to, pair) in captures {
        if from > pos {
            // Insert non-captured text
            let start = pos as usize;
            let end = from as usize;
            let text: String = orig_chars
                .get(start..end)
                .map(|s| s.iter().collect())
                .unwrap_or_default();
            result.push(Value::Pair("~".to_string(), Box::new(Value::str(text))));
        }
        result.push(pair);
        pos = to;
    }

    // Trailing non-captured text
    if pos < match_to {
        let start = pos as usize;
        let end = match_to as usize;
        let text: String = orig_chars
            .get(start..end)
            .map(|s| s.iter().collect())
            .unwrap_or_default();
        result.push(Value::Pair("~".to_string(), Box::new(Value::str(text))));
    }

    Value::array(result)
}
