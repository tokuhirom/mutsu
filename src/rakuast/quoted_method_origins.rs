//! Origins of a quoted method call (ADR-10723 Stage 2; #15780):
//! `$x."uc"()`, `$x.'lc'`, `$x."$name"(1)`, `self!"name"()`.
//!
//! Rakudo starts the `Call::QuotedMethod` at the opening quote of its name and
//! ends it with the call; the name's `QuotedString` is the quoted text, and
//! the `ArgList` the inside of the parentheses (empty, at the `)`, when there
//! are no arguments). The segments of an interpolating name are placed by
//! `string_origins`, once the `QuotedString` has its range.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;
use crate::value::{Value, ValueView};

/// Place `call`, a `Call::QuotedMethod` whose text runs from after `from` to
/// the end of `span`: the call, its name and its arguments. `false` when the
/// text is not a quoted method call this reads.
// Cost: O(n), n = length of the call's source.
pub(super) fn place(call: &mut RakuAstNode, from: usize, span: SrcSpan) -> bool {
    if call.class != RakuAstClass::CallQuotedMethod || call.origin.0.is_some() {
        return false;
    }
    let to = span.to as usize;
    let Some((quote_at, quote_end, args)) =
        super::unit_source::with_text(SrcSpan::new(from, to), |text| {
            let after_ws = text.trim_start();
            let after_op = after_ws
                .strip_prefix('.')
                .map(|r| r.strip_prefix(['?', '+', '*']).unwrap_or(r))
                .or_else(|| after_ws.strip_prefix('!'))?;
            let quote_at = from + text.len() - after_op.len();
            let len = quoted_len(after_op)?;
            let rest = &after_op[len..];
            let args = if rest.is_empty() {
                None
            } else if rest.starts_with('(') && rest.ends_with(')') {
                Some(SrcSpan::new(quote_at + len + 1, to - 1))
            } else {
                return None;
            };
            Some((quote_at, quote_at + len, args))
        })
    else {
        return false;
    };
    super::origin::set_span(call, SrcSpan::new(quote_at, to));
    let args = args.and_then(|inside| {
        super::call_origins::parenthesized(super::infix_origins::child(call, "args"), inside)
    });
    if let Some((from, to)) = args {
        super::empty_fields::set_span(call, "args", SrcSpan::new(from, to));
    }
    for field in call.fields.iter_mut() {
        let RakuAstFieldValue::Node(held) = &mut field.value else {
            continue;
        };
        let ValueView::RakuAst(part) = held.view() else {
            continue;
        };
        let mut part = RakuAstNode::clone(part);
        match (field.name, args) {
            (Some("name"), _) => {
                let at = SrcSpan::new(quote_at, quote_end);
                super::origin::set_span(&mut part, at);
                super::origin::quoted_text(&mut part, at);
            }
            (Some("args"), Some((from, to))) => {
                super::origin::set_span(&mut part, SrcSpan::new(from, to))
            }
            _ => continue,
        }
        *held = Value::rakuast(Box::new(part));
    }
    true
}

/// Place the segments of the interpolating name of the quoted method call
/// that `node`, an `ApplyPostfix`, applies (`$x."$name"()`), once the name
/// has its range.
// Cost: O(n + s), n = length of the name's source, s = its segments.
pub(super) fn segments(names: &NameScope, node: &mut RakuAstNode) {
    super::infix_origins::with_child(node, "postfix", |call| {
        if call.class == RakuAstClass::CallQuotedMethod {
            super::infix_origins::with_child(call, "name", |name| {
                super::string_origins::interpolated(names, name)
            });
        }
    });
}

/// The length of the quoted string `text` starts with, through its closing
/// quote: `'...'`, or `"..."` whose `{ ... }` interpolations may hold quotes
/// of their own. A backslash escapes the character after it.
// Cost: O(n), n = length of the string.
fn quoted_len(text: &str) -> Option<usize> {
    let quote = text.chars().next().filter(|c| matches!(c, '"' | '\''))?;
    let mut depth = 0usize;
    let mut chars = text.char_indices().skip(1);
    while let Some((at, c)) = chars.next() {
        match c {
            '\\' => {
                chars.next()?;
            }
            '{' if quote == '"' => depth += 1,
            '}' if quote == '"' => depth = depth.checked_sub(1)?,
            // A quote inside an interpolated block is that block's own.
            '"' | '\'' if depth > 0 => {}
            c if c == quote => return Some(at + 1),
            _ => {}
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::quoted_len;

    #[test]
    fn quoted_names() {
        assert_eq!(quoted_len(r#""uc"()"#), Some(4));
        assert_eq!(quoted_len("'lc'"), Some(4));
        assert_eq!(quoted_len(r#""{'u' ~ "c"}"()"#), Some(13));
        assert_eq!(quoted_len(r#""a\"b""#), Some(6));
        assert_eq!(quoted_len(r#""open"#), None);
        assert_eq!(quoted_len("uc"), None);
    }
}
