//! The adverbs of a quoted regex (`m:i:g/.../`, `m:p($pos)/.../`) and where
//! its body starts, read off the regex's text for its origins
//! (`regex_origins`; ADR-10723 Stage 2, #15780).

use super::infix_origins::{byte_range, leaf_text};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;
use crate::value::{Value, ValueView};

/// What a quoted regex's text says before its body.
pub(super) struct Opener {
    /// The `:name` adverbs, as offsets into the text.
    pub(super) adverbs: Vec<(usize, usize)>,
    /// Where the body starts.
    pub(super) body_at: usize,
    /// The delimiter that closes the body.
    pub(super) close: char,
}

/// A quoted regex's adverbs (`:i`) and where its body starts and what closes
/// it, read from its `text`.
// Cost: O(n), n = length of the prefix.
pub(super) fn opener(text: &str) -> Option<Opener> {
    let mut at = ["rx", "ms", "m"]
        .into_iter()
        .find(|p| text.starts_with(p) && !text[p.len()..].starts_with(char::is_alphanumeric))
        .map_or(0, str::len);
    let mut adverbs = Vec::new();
    while text[at..].starts_with(':') {
        let len = text[at + 1..]
            .find(|c: char| !(c.is_alphanumeric() || c == '_' || c == '-'))
            .unwrap_or(text.len() - at - 1);
        if len == 0 {
            return None;
        }
        // `:p($pos)`: a value in parentheses after the name.
        let value = match text[at + 1 + len..].strip_prefix('(') {
            Some(inner) => 2 + super::subst_origins::closing(inner, '(', ')')?,
            None => 0,
        };
        adverbs.push((at, at + 1 + len + value));
        at += 1 + len + value;
    }
    at += text[at..].len() - text[at..].trim_start().len();
    let open = text[at..].chars().next()?;
    let close = match open {
        '/' => '/',
        '{' => '}',
        '[' => ']',
        '(' => ')',
        '<' => '>',
        '!' | '|' => open,
        _ => return None,
    };
    Some(Opener {
        adverbs,
        body_at: at + open.len_utf8(),
        close,
    })
}

/// Give a quoted regex's `:name` adverbs the ranges read off its prefix.
// Cost: O(a), a = adverbs.
pub(super) fn place_adverbs(node: &mut RakuAstNode, written: &[(usize, usize)]) {
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("adverbs")) else {
        return;
    };
    let RakuAstFieldValue::List(items) = &mut field.value else {
        return;
    };
    if items.len() != written.len() {
        return;
    }
    for (item, &(a, b)) in items.iter_mut().zip(written) {
        let ValueView::RakuAst(pair) = item.view() else {
            return;
        };
        let spelled = matches!(
            pair.class,
            RakuAstClass::ColonPairTrue | RakuAstClass::ColonPairValue
        ) && leaf_text(pair).is_some_and(|name| {
            super::unit_source::with_text(SrcSpan::new(from + a + 1, from + b), |t| {
                let rest = t.strip_prefix(name.as_str())?;
                (rest.is_empty() == (pair.class == RakuAstClass::ColonPairTrue)).then_some(())
            })
            .is_some()
        });
        // `:2nd`: a number, then the key.
        let numbered = pair.class == RakuAstClass::ColonPairNumber
            && leaf_text(pair).is_some_and(|name| {
                super::unit_source::with_text(SrcSpan::new(from + a + 1, from + b), |t| {
                    let digits = t.strip_suffix(name.as_str())?;
                    (!digits.is_empty() && digits.bytes().all(|c| c.is_ascii_digit())).then_some(())
                })
                .is_some()
            });
        if !spelled && !numbered {
            return;
        }
        let mut pair = RakuAstNode::clone(pair);
        if numbered {
            super::origin::set_span(&mut pair, SrcSpan::new(from + a, from + b));
            *item = Value::rakuast(Box::new(pair));
            continue;
        }
        super::origin::set_span(&mut pair, SrcSpan::new(from + a, from + b));
        // `:p($pos)` / `:c(0)`: its parentheses and what they hold; a value
        // the parser rebuilt is placed from a fragment parse of its text.
        let name_len = leaf_text(&pair).map_or(0, |n| n.len());
        let open = from + a + 1 + name_len;
        if open < from + b
            && let Some(text) =
                super::unit_source::with_text(SrcSpan::new(open, from + b), |t| Some(t.to_string()))
        {
            super::infix_origins::with_child(&mut pair, "value", |value| {
                if value.class == RakuAstClass::CircumfixParentheses && value.origin.0.is_none() {
                    super::fragment_origins::expression(&NameScope::default(), value, &text, open);
                }
                super::list_origins::circumfix(value);
            });
        }
        super::operator_origins::derive(&mut pair);
        *item = Value::rakuast(Box::new(pair));
    }
}
