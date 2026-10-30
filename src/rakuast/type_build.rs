//! Bounded composition of the parser's type spellings into source nodes.

use super::bareword::simple_type_node;
use super::convert::{is_pseudo_type, node_field, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, name_parts};
use crate::ast::{Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) const MAX_TYPE_DEPTH: usize = 128;

/// A simple type uses exactly the parser's identifier grammar, including
/// internal hyphens, apostrophes, Unicode and qualified segments.
pub(super) fn is_simple_type(t: &str) -> bool {
    is_pseudo_type(t) || {
        let t = crate::qualified::type_capture_name(t).unwrap_or(t);
        !t.is_empty() && name_parts::identifier_segments(t).all(crate::parser::is_raku_identifier)
    }
}

pub(super) fn build_type_node(t: &str) -> Result<RakuAstNode, RuntimeError> {
    build_type_node_at(t, 0)
}

/// A declaration's capture type is a `Type::Capture`; other type positions
/// retain their existing interpretation of a leading `::`.
pub(super) fn declaration_type_node(t: &str) -> Result<RakuAstNode, RuntimeError> {
    if let Some(name) = crate::qualified::type_capture_name(t)
        && !name.is_empty()
        && name
            .chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '-')
    {
        return super::convert::type_capture_node(name);
    }
    build_type_node(t)
}

/// A return constraint may pin a numeric value instead of naming a type.
// Cost: O(n), n = constraint spelling length.
pub(super) fn return_constraint_node(t: &str) -> Result<RakuAstNode, RuntimeError> {
    if let Some(value) = crate::parser::numeric_literal_value(t) {
        return super::convert::convert_literal(&value);
    }
    if let Some(text) = string_literal_value(t) {
        return Ok(super::convert::quoted_string(Value::str(text)));
    }
    build_type_node(t)
}

/// The text of a `'…'` / `"…"` return-value literal that needs no
/// interpolation or escape processing, or `None`.
// Cost: O(n), n = constraint spelling length.
pub(super) fn plain_string_literal(t: &str) -> Option<&str> {
    let quote = t.chars().next().filter(|c| matches!(c, '\'' | '"'))?;
    let inner = t.strip_prefix(quote)?.strip_suffix(quote)?;
    if inner.contains(['\'', '"', '\\', '$', '@', '%', '&', '{']) {
        return None;
    }
    Some(inner)
}

/// The value of a return-value string literal: a plain one
/// ([`plain_string_literal`]), or any other `'…'` / `"…"` the parser reads as
/// one constant string (escapes such as `'[\\'` or `"\{\n"`, no interpolation).
// Cost: O(n), n = constraint spelling length.
pub(super) fn string_literal_value(t: &str) -> Option<String> {
    if let Some(text) = plain_string_literal(t) {
        return Some(text.to_string());
    }
    if !t.starts_with(['\'', '"']) {
        return None;
    }
    let (stmts, _) = crate::parser::parse_fragment(t).ok()?;
    let stmts: Vec<&Stmt> = stmts
        .iter()
        .filter(|stmt| !matches!(stmt, Stmt::SetLine(..)))
        .collect();
    match stmts.as_slice() {
        [Stmt::Expr(Expr::Literal(value))] if matches!(value.view(), ValueView::Str(_)) => {
            Some(value.to_string_value())
        }
        _ => None,
    }
}

/// `t` without a trailing smiley (`:D`, `:U`, `:_`). A name that ends in the
/// letter after its own `::` (`Example2::D`, a role called `D`) has no smiley.
// Cost: O(1).
pub(super) fn strip_smiley<'a>(t: &'a str, smiley: &str) -> Option<&'a str> {
    t.strip_suffix(smiley).filter(|base| !base.ends_with(':'))
}

pub(super) fn build_type_node_at(t: &str, depth: usize) -> Result<RakuAstNode, RuntimeError> {
    if depth >= MAX_TYPE_DEPTH {
        return Err(unsupported("type composition exceeds 128 levels"));
    }
    if let Some(base) = strip_smiley(t, ":_") {
        return Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TypeAnyDefinedness,
            fields: vec![node_field(
                Some("base-type"),
                build_type_node_at(base, depth + 1)?,
            )],
        });
    }
    if let Some(base) = strip_smiley(t, ":D").or_else(|| strip_smiley(t, ":U")) {
        return Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TypeDefinedness,
            fields: vec![
                node_field(Some("base-type"), build_type_node_at(base, depth + 1)?),
                RakuAstField {
                    name: Some("definite"),
                    value: RakuAstFieldValue::Node(Value::truth(t.ends_with(":D"))),
                },
            ],
        });
    }
    if let Some((open, delimiter)) = application_open(t)? {
        if delimiter == '[' {
            return super::type_args::parameterized_type_node_at(t, None, depth);
        }
        let inner = &t[open + 1..t.len() - 1];
        let mut fields = vec![node_field(
            Some("base-type"),
            build_type_node_at(&t[..open], depth + 1)?,
        )];
        if !inner.is_empty() {
            fields.push(node_field(
                Some("constraint"),
                build_type_node_at(inner, depth + 1)?,
            ));
        }
        return Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TypeCoercion,
            fields,
        });
    }
    if is_simple_type(t) {
        return Ok(simple_type_node(t));
    }
    Err(unsupported(&format!("type `{t}`")))
}

/// Locate the last outer application, skipping nested arguments and quotes.
/// Apostrophes inside an identifier remain name characters.
// Cost: O(n), n = spelling length; nesting is bounded at MAX_TYPE_DEPTH.
pub(super) fn application_open(t: &str) -> Result<Option<(usize, char)>, RuntimeError> {
    let mut stack = Vec::new();
    let mut quote = None;
    let mut escaped = false;
    let mut previous = None;
    let mut outer = None;
    let mut tail = None;
    for (index, ch) in t.char_indices() {
        if let Some(delimiter) = quote {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == delimiter {
                quote = None;
            }
        } else if opens_quote(previous, ch) {
            quote = Some(ch);
        } else if matches!(ch, '[' | '(' | '{') {
            if stack.len() >= MAX_TYPE_DEPTH {
                return Err(unsupported("type composition exceeds 128 levels"));
            }
            if stack.is_empty() {
                outer = Some((index, ch));
            }
            stack.push(ch);
        } else if matches!(ch, ']' | ')' | '}') {
            let expected = match ch {
                ']' => '[',
                ')' => '(',
                _ => '{',
            };
            if stack.pop() != Some(expected) {
                return Err(unsupported("malformed type application"));
            }
            if stack.is_empty() && index + ch.len_utf8() == t.len() {
                tail = outer.filter(|(_, delimiter)| matches!(delimiter, '[' | '('));
            }
        }
        previous = Some(ch);
    }
    if !stack.is_empty() || quote.is_some() {
        return Err(unsupported("malformed type application"));
    }
    Ok(tail)
}

pub(super) fn opens_quote(previous: Option<char>, ch: char) -> bool {
    ch == '"' || (ch == '\'' && !previous.is_some_and(|c| c.is_alphanumeric() || c == '_'))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_composition_is_bounded_across_argument_edges() {
        let nested = format!(
            "{}Int{}",
            "Array[".repeat(MAX_TYPE_DEPTH + 1),
            "]".repeat(MAX_TYPE_DEPTH + 1)
        );
        assert!(build_type_node(&nested).is_err());
        assert!(build_type_node(&format!("Int{}", ":D".repeat(MAX_TYPE_DEPTH + 1))).is_err());
        assert!(build_type_node("Int:D(Str)").is_ok());
        assert!(build_type_node("Array[Int](Cool)").is_ok());
    }

    #[test]
    fn type_identifiers_share_the_parser_grammar() {
        for name in ["Type-Name", "Doesn't", "Types::Version-info", "Élément"] {
            assert!(is_simple_type(name), "{name}");
        }
        for name in ["3", "Type-3", "Trailing'", "Type::", "Type::::Name"] {
            assert!(!is_simple_type(name), "{name}");
        }
    }
}
