//! Origins of a substitution's parts (ADR-10723 Stage 2, #15780):
//! `s:g/a/b/`, `S/a/$0/`, `ss/a b/c d/`, `s[\d] = 5`, `s{a} += 1`.
//!
//! The parser keeps the pattern and replacement as text, so they are read
//! back from the substitution's own range: its `:name` adverbs, the pattern
//! between the delimiters (laid out by the regex walk), and either the
//! replacement between the second and third delimiter (a string, whose
//! segments are placed as an interpolating string's are) or, after a
//! bracketed pattern, the assignment operator and the expression after it.
//! Each part is placed only where the text spells it: exact or absent.

use super::infix_origins::{byte_range, child, leaf_text, with_child};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;

/// Place the adverbs, pattern, operator and replacement of `node`, a
/// `Substitution` with a range.
// Cost: O(n), n = length of the substitution's source.
pub(super) fn derive(names: &NameScope, node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(text) = super::unit_source::with_text(SrcSpan::new(from, to), |t| Some(t.to_string()))
    else {
        return;
    };
    let Some(layout) = layout(&text) else {
        return;
    };
    let span = |(a, b): (usize, usize)| SrcSpan::new(from + a, from + b);
    super::regex_origins::place_adverbs(node, &layout.adverbs);
    with_child(node, "pattern", |pattern| {
        if pattern.origin.0.is_none() {
            super::regex_origins::walk_body(names, pattern, span(layout.pattern));
        }
    });
    match layout.rest {
        Rest::Quoted(body) => with_child(node, "replacement", |replacement| {
            if replacement.class != RakuAstClass::QuotedString
                || replacement.origin.0.is_some()
                || body.0 == body.1
            {
                return;
            }
            super::origin::set_span(replacement, span(body));
            super::string_origins::segments(names, replacement, from + body.0, from + body.1);
        }),
        Rest::Assigned { infix, value } => {
            if !infix_spelled(node, &text[infix.0..infix.1]) {
                return;
            }
            with_child(node, "infix", |op| {
                super::origin::set_span(op, span(infix));
                if op.class == RakuAstClass::MetaInfixAssign {
                    super::infix_origins::with_child_at(op, 0, |inner| {
                        super::origin::set_span(inner, span((infix.0, infix.1 - 1)))
                    });
                }
            });
            with_child(node, "replacement", |replacement| {
                if replacement.origin.0.is_none() && value.0 < value.1 {
                    super::origin::set_span(replacement, span(value));
                    super::origin::named_term(replacement, span(value));
                    super::origin::quoted_text(replacement, span(value));
                    super::infix_origins::derive(names, replacement);
                }
            });
        }
    }
}

/// Where a substitution's parts are written, as offsets into its text.
struct Layout {
    /// The `:name` adverbs, each from its colon.
    adverbs: Vec<(usize, usize)>,
    /// The pattern between its delimiters.
    pattern: (usize, usize),
    rest: Rest,
}

/// What follows the pattern.
enum Rest {
    /// `s/a/b/`: the replacement between the second and third delimiter.
    Quoted((usize, usize)),
    /// `s[a] OP= b`: the operator and the expression after it.
    Assigned {
        infix: (usize, usize),
        value: (usize, usize),
    },
}

/// Read a substitution's `text`: `s` / `S` / `ss` / `Ss`, its adverbs, its
/// delimited pattern and what follows it. `None` when the text is not such a
/// substitution through to its end.
// Cost: O(n), n = length of `text`.
fn layout(text: &str) -> Option<Layout> {
    let mut at = ["ss", "Ss", "s", "S"]
        .into_iter()
        .find(|p| text.starts_with(p))?
        .len();
    let mut adverbs = Vec::new();
    while text[at..].starts_with(':') {
        let len = text[at + 1..]
            .find(|c: char| !(c.is_alphanumeric() || c == '_' || c == '-'))
            .unwrap_or(text.len() - at - 1);
        if len == 0 {
            return None;
        }
        adverbs.push((at, at + 1 + len));
        at += 1 + len;
    }
    at += text[at..].len() - text[at..].trim_start().len();
    let open = text[at..].chars().next()?;
    let body = at + open.len_utf8();
    let close = crate::source_comments::matching_bracket(open);
    let Some(close) = close else {
        // `s/a/b/`: one delimiter, three times.
        if open.is_alphanumeric() || open.is_whitespace() || matches!(open, '(' | ':') {
            return None;
        }
        let pattern_end = body + unescaped(&text[body..], open)?;
        let replacement = pattern_end + open.len_utf8();
        let replacement_end = replacement + unescaped(&text[replacement..], open)?;
        return (replacement_end + open.len_utf8() == text.len()).then_some(Layout {
            adverbs,
            pattern: (body, pattern_end),
            rest: Rest::Quoted((replacement, replacement_end)),
        });
    };
    let pattern_end = body + closing(&text[body..], open, close)?;
    let after = pattern_end + close.len_utf8();
    let infix = after + (text[after..].len() - text[after..].trim_start().len());
    let eq = infix + text[infix..].find('=')?;
    let value = eq + 1 + (text[eq + 1..].len() - text[eq + 1..].trim_start().len());
    Some(Layout {
        adverbs,
        pattern: (body, pattern_end),
        rest: Rest::Assigned {
            infix: (infix, eq + 1),
            value: (value, text.len()),
        },
    })
}

/// Whether the operator written as `written` (`=`, `+=`, `~=`) is the
/// substitution's `infix`: an `Assignment` for `=`, else a
/// `MetaInfix::Assign` over the operator before the `=`.
// Cost: O(1).
fn infix_spelled(node: &RakuAstNode, written: &str) -> bool {
    let Some(infix) = child(node, "infix") else {
        return false;
    };
    match infix.class {
        RakuAstClass::Assignment => written == "=",
        RakuAstClass::MetaInfixAssign => {
            super::infix_origins::child_at(infix, 0)
                .and_then(leaf_text)
                .is_some_and(|op| written.strip_suffix('=') == Some(op.as_str()))
                && written.len() > 1
        }
        _ => false,
    }
}

/// The offset in `text` of the first `delimiter` not escaped by a backslash.
// Cost: O(n), n = bytes scanned.
fn unescaped(text: &str, delimiter: char) -> Option<usize> {
    let mut chars = text.char_indices();
    while let Some((at, c)) = chars.next() {
        if c == '\\' {
            chars.next();
        } else if c == delimiter {
            return Some(at);
        }
    }
    None
}

/// The offset in `text` (which starts after `open`) of the `close` that
/// matches it, counting nested pairs and skipping escaped characters and
/// quoted strings.
// Cost: O(n), n = bytes scanned.
fn closing(text: &str, open: char, close: char) -> Option<usize> {
    let mut depth = 1usize;
    let mut quote: Option<char> = None;
    let mut chars = text.char_indices();
    while let Some((at, c)) = chars.next() {
        if c == '\\' {
            chars.next();
            continue;
        }
        if let Some(q) = quote {
            if c == q {
                quote = None;
            }
            continue;
        }
        if matches!(c, '\'' | '"') {
            quote = Some(c);
        } else if c == open {
            depth += 1;
        } else if c == close {
            depth -= 1;
            if depth == 0 {
                return Some(at);
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::{Rest, closing, layout};

    #[test]
    fn delimited_and_bracketed() {
        let l = layout("s:g/a/b/").unwrap();
        assert_eq!(l.adverbs, vec![(1, 3)]);
        assert_eq!(l.pattern, (4, 5));
        assert!(matches!(l.rest, Rest::Quoted((6, 7))));
        let l = layout("s[\\d] += 5").unwrap();
        assert_eq!(l.pattern, (2, 4));
        assert!(matches!(
            l.rest,
            Rest::Assigned {
                infix: (6, 8),
                value: (9, 10)
            }
        ));
        assert!(layout("s/a/b/ x").is_none());
        assert_eq!(closing("a ']' [b]] c", '[', ']'), Some(9));
    }
}
