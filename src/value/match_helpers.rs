use crate::value::{Value, ValueView};

use crate::value::AttrMap;
use crate::value::raku_repr::raku_value;

/// Produce Raku-compatible `.raku` representation for a Match object.
// Cost: O(s), s = the length of the rendered text (every node carries the whole
// `orig`), written once into one buffer rather than re-copied at each nesting
// level.
pub(crate) fn match_raku_repr(attributes: &AttrMap) -> String {
    let mut out = String::new();
    write_match_raku(attributes, &mut out, &mut EscapedOrig::default());
    out
}

/// The escaped form of the last `orig` rendered: every node of one match shares
/// the same subject, so it is escaped once and copied after that.
#[derive(Default)]
struct EscapedOrig {
    raw: String,
    escaped: String,
}

impl EscapedOrig {
    // Cost: O(n) to compare, plus O(n) to escape when the subject changed.
    fn push(&mut self, orig: &str, out: &mut String) {
        if self.raw != orig {
            self.raw.clear();
            self.raw.push_str(orig);
            self.escaped.clear();
            push_escaped_orig(orig, &mut self.escaped);
        }
        out.push_str(&self.escaped);
    }
}

/// Append `orig` to `out` escaped as a double-quoted Raku string body.
fn push_escaped_orig(orig: &str, out: &mut String) {
    for c in orig.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\0' => out.push_str("\\0"),
            c => out.push(c),
        }
    }
}

fn write_match_raku(attributes: &AttrMap, out: &mut String, esc: &mut EscapedOrig) {
    use std::fmt::Write;
    let orig = attributes
        .get("orig")
        .map(|v| v.to_string_value())
        .unwrap_or_default();
    let from = match attributes.get("from").map(Value::view) {
        Some(ValueView::Int(n)) => n,
        _ => 0,
    };
    // `:pos` is where the cursor ended: `to` unless a `)>` narrowed it.
    let pos = match attributes
        .get("pos")
        .or_else(|| attributes.get("to"))
        .map(Value::view)
    {
        Some(ValueView::Int(n)) => n,
        _ => 0,
    };

    out.push_str("Match.new(:orig(\"");
    esc.push(&orig, out);
    let _ = write!(out, "\"), :from({from}), :pos({pos})");

    // Positional captures (:list)
    if let Some(ValueView::Array(items, ..)) = attributes.get("list").map(Value::view)
        && !items.is_empty()
    {
        // `:list(( ... ))`: an outer paren for the `:list(...)` adverb plus an
        // inner paren for the captured List literal — both must be closed.
        out.push_str(", :list((");
        for (i, v) in items.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            // An unbound interior slot iterates as `Mu` (`match_list_view`).
            write_value_raku(&v.clone().unbound_capture_as_mu(), out, esc);
        }
        if items.len() == 1 {
            out.push(',');
        }
        out.push_str("))");
    }

    // Named captures (:hash)
    if let Some(ValueView::Hash(map)) = attributes.get("named").map(Value::view)
        && !map.is_empty()
    {
        // Deterministic output: the rendered pairs sorted as text. The text of a
        // pair starts `(:key(`, so the order is the keys' unless a key holds a
        // `(`; only then is the text itself compared.
        let mut entries: Vec<_> = map.iter().collect();
        if entries.iter().any(|(k, _)| k.contains('(')) {
            let mut pairs: Vec<String> = entries
                .iter()
                .map(|(k, v)| {
                    let mut pair = format!("(:{k}(");
                    write_value_raku(v, &mut pair, esc);
                    pair.push_str("))");
                    pair
                })
                .collect();
            pairs.sort();
            let _ = write!(out, ", :hash(Map.new({}))", pairs.join(", "));
        } else {
            entries.sort_by(|a, b| {
                a.0.bytes()
                    .chain(Some(b'('))
                    .cmp(b.0.bytes().chain(Some(b'(')))
            });
            out.push_str(", :hash(Map.new(");
            for (i, (k, v)) in entries.into_iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                let _ = write!(out, "(:{k}(");
                write_value_raku(v, out, esc);
                out.push_str("))");
            }
            out.push_str("))");
        }
    }

    out.push(')');
}

/// Append the `.raku` representation of a value to `out`, recursing into Match
/// objects.
fn write_value_raku(val: &Value, out: &mut String, esc: &mut EscapedOrig) {
    match val.view() {
        ValueView::Instance { attributes, .. } if val.is_match_instance() => {
            write_match_raku(&(attributes).as_map(), out, esc);
        }
        ValueView::Array(items, ..) => {
            out.push('[');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                write_value_raku(item, out, esc);
            }
            out.push(']');
        }
        // Use the existing raku_value helper for non-Match values
        _ => out.push_str(&raku_value(val)),
    }
}

/// Extract the `from` position from a Match object value.
pub(crate) fn match_value_from(val: &Value) -> i64 {
    match_visible_pos(val, val.match_from().unwrap_or(0))
}

/// Extract the `to` position from a Match object value.
pub(crate) fn match_value_to(val: &Value) -> i64 {
    match_visible_pos(val, val.match_to().unwrap_or(0))
}

/// Convert a Match's internal codepoint offset into the grapheme index that
/// `.from` / `.to` / `.pos` report (Raku counts characters, so `\r\n` is one).
// Cost: O(n), n = chars of `.orig` for the ASCII/CRLF scan; a non-trivial
// subject also pays an O(p) grapheme count, p = chars of the prefix.
pub(crate) fn match_visible_pos(val: &Value, cp: i64) -> i64 {
    use crate::grapheme_break::UnicodeSegmentation;
    let Some(orig) = val.match_orig() else {
        return cp;
    };
    let text = orig.to_string_value();
    if cp <= 0 || text.is_ascii() && !text.contains("\r\n") {
        return cp;
    }
    let prefix: String = text.chars().take(cp as usize).collect();
    prefix.graphemes(true).count() as i64
}

/// The code-point offset of grapheme index `g` in `text` (the inverse of
/// [`match_visible_pos`]): `:p(N)` / `:c(N)` count graphemes, the engine counts
/// code points. An index past the end maps past the end.
// Cost: O(1) for ASCII without `\r\n`; otherwise O(p), p = chars before the
// `g`-th grapheme.
pub(crate) fn grapheme_to_cp_index(text: &str, g: usize) -> usize {
    use crate::grapheme_break::UnicodeSegmentation;
    if text.is_ascii() && !text.contains("\r\n") {
        return g;
    }
    let mut cp = 0usize;
    let mut graphemes = text.graphemes(true);
    for i in 0..g {
        match graphemes.next() {
            Some(gr) => cp += gr.chars().count(),
            None => return cp + (g - i),
        }
    }
    cp
}

/// Collect all captures from a Match object sorted by position.
/// Positional captures use `ValuePair(Int => Match)` and named captures use
/// `Pair(Str => Match)` to preserve the Raku-visible key types.
pub(crate) fn match_caps(attributes: &AttrMap) -> Value {
    let mut pairs: Vec<(i64, i64, Value)> = Vec::new();

    // Collect named capture positions to filter out shadowed positional captures
    let mut named_positions: Vec<(i64, i64)> = Vec::new();
    if let Some(ValueView::Hash(named)) = attributes.get("named").map(Value::view) {
        for val in named.values() {
            for item in &expand_capture_items(val) {
                named_positions.push((match_value_from(item), match_value_to(item)));
            }
        }
    }

    // Collect positional captures, expanding quantified arrays
    if let Some(ValueView::Array(items, ..)) = attributes.get("list").map(Value::view) {
        for (i, val) in items.iter().enumerate() {
            for item in &expand_capture_items(val) {
                let from = match_value_from(item);
                let to = match_value_to(item);
                let shadowed = named_positions
                    .iter()
                    .any(|(nf, nt)| *nf == from && *nt == to);
                if !shadowed {
                    pairs.push((
                        from,
                        to,
                        Value::value_pair(Value::int(i as i64), item.clone()),
                    ));
                }
            }
        }
    }

    // Collect named captures, expanding quantified arrays
    if let Some(ValueView::Hash(named)) = attributes.get("named").map(Value::view) {
        for (key, val) in named.iter() {
            for item in &expand_capture_items(val) {
                let from = match_value_from(item);
                let to = match_value_to(item);
                pairs.push((from, to, Value::pair(key.clone(), item.clone())));
            }
        }
    }

    // Sort by position. Tie-break on the end offset so a zero-width capture
    // (e.g. an empty `%%` separator) sorts before a capture that starts at the
    // same offset but spans further, matching Raku's match order.
    pairs.sort_by_key(|(from, to, _)| (*from, *to));

    Value::array(pairs.into_iter().map(|(_, _, pair)| pair).collect())
}

/// Expand a capture value: if it's an Array of Matches (from quantified captures),
/// return each element; otherwise return the single value. An unbound slot
/// (`Nil`, an unmatched `(x)?` before a later capture) captured nothing, so it
/// yields no entry: raku's `.caps` / `.chunks` hold only what was bound.
// Cost: O(e), e = the entries of a quantified capture (1 otherwise).
fn expand_capture_items(val: &Value) -> Vec<Value> {
    if val.is_nil() {
        return Vec::new();
    }
    match val.view() {
        ValueView::Array(items, _)
            if items
                .iter()
                .all(|v| matches!(v.view(), ValueView::Instance { .. })) =>
        {
            items.iter().cloned().collect()
        }
        _ => vec![val.clone()],
    }
}

/// Like `.caps` but also includes non-captured text between captures as `~ => text` pairs.
pub(crate) fn match_chunks(attributes: &AttrMap) -> Value {
    // Collect named capture positions to filter out shadowed positional captures
    let mut named_positions: Vec<(i64, i64)> = Vec::new();
    if let Some(ValueView::Hash(named)) = attributes.get("named").map(Value::view) {
        for val in named.values() {
            for item in &expand_capture_items(val) {
                named_positions.push((match_value_from(item), match_value_to(item)));
            }
        }
    }

    // Collect all captures with their from/to positions
    let mut captures: Vec<(i64, i64, Value)> = Vec::new();

    if let Some(ValueView::Array(items, ..)) = attributes.get("list").map(Value::view) {
        for (i, val) in items.iter().enumerate() {
            for item in &expand_capture_items(val) {
                let from = match_value_from(item);
                let to = match_value_to(item);
                let shadowed = named_positions
                    .iter()
                    .any(|(nf, nt)| *nf == from && *nt == to);
                if !shadowed {
                    captures.push((
                        from,
                        to,
                        Value::value_pair(Value::int(i as i64), item.clone()),
                    ));
                }
            }
        }
    }

    if let Some(ValueView::Hash(named)) = attributes.get("named").map(Value::view) {
        for (key, val) in named.iter() {
            for item in &expand_capture_items(val) {
                let from = match_value_from(item);
                let to = match_value_to(item);
                captures.push((from, to, Value::pair(key.clone(), item.clone())));
            }
        }
    }

    // Sort by position; tie-break on end offset so a zero-width capture sorts
    // before a longer capture starting at the same offset (Raku match order).
    captures.sort_by_key(|(from, to, _)| (*from, *to));

    let match_from = match attributes.get("from").map(Value::view) {
        Some(ValueView::Int(n)) => n,
        _ => 0,
    };
    let match_to = match attributes.get("to").map(Value::view) {
        Some(ValueView::Int(n)) => n,
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
            result.push(Value::pair("~".to_string(), Value::str(text)));
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
        result.push(Value::pair("~".to_string(), Value::str(text)));
    }

    // raku's `Match.chunks` is a `Seq` (its `.caps` stays a `List`).
    Value::seq(result)
}
