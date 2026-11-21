//! Origins of the colonpairs in a hash composer (`{:2b, :!c, :d<x y>, :e(5),
//! :z}`; ADR-10723 Stage 2, #15780). The parser keeps a composer's entries as
//! key / value pairs and the colonpair forms only as a spelling, so the pairs
//! are read back from the composer's text: from its `{`, each entry is either
//! a node already placed (`"a" => 1`), skipped to its end, or a colonpair
//! written exactly as its node says, placed there. A comma separates them.
//! Anything else stops the walk: exact or absent.

use super::infix_origins::{byte_range, leaf_text};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;
use crate::value::{Value, ValueView};

/// Place the colonpairs of `composer`, a `Circumfix::HashComposer` with a
/// range, then its comma list from them.
// Cost: O(n + p), n = length of the composer's text, p = its entries.
pub(super) fn derive(names: &NameScope, composer: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(composer) else {
        return;
    };
    let Some(text) = super::unit_source::with_text(SrcSpan::new(from, to), |t| {
        (t.starts_with('{') && t.ends_with('}')).then(|| t.to_string())
    }) else {
        return;
    };
    let Some(field) = composer.fields.iter_mut().find(|f| f.name.is_none()) else {
        return;
    };
    let RakuAstFieldValue::Node(held) = &mut field.value else {
        return;
    };
    let ValueView::RakuAst(contents) = held.view() else {
        return;
    };
    let mut contents = RakuAstNode::clone(contents);
    let mut at = 1;
    if contents.class == RakuAstClass::ApplyListInfix {
        let Some(operands) = contents.fields.iter_mut().find_map(|f| match &mut f.value {
            RakuAstFieldValue::List(items) if f.name == Some("operands") => Some(items),
            _ => None,
        }) else {
            return;
        };
        for operand in operands.iter_mut() {
            let ValueView::RakuAst(entry) = operand.view() else {
                return;
            };
            let mut entry = RakuAstNode::clone(entry);
            let Some(end) = place_entry(names, &mut entry, &text, from, at) else {
                return;
            };
            *operand = Value::rakuast(Box::new(entry));
            let rest = &text[end..];
            let Some(after) = rest.trim_start().strip_prefix(',') else {
                break;
            };
            at = text.len() - after.len();
        }
        super::infix_origins::derive(names, &mut contents);
    } else {
        place_entry(names, &mut contents, &text, from, at);
    }
    *held = Value::rakuast(Box::new(contents));
}

/// Place `entry`, written after whitespace at byte `at` of `text` (the
/// composer's text, starting at byte `base` of the unit), and answer where
/// in `text` it ends.
// Cost: O(n), n = length of the entry's text.
fn place_entry(
    names: &NameScope,
    entry: &mut RakuAstNode,
    text: &str,
    base: usize,
    at: usize,
) -> Option<usize> {
    let start = at + (text[at..].len() - text[at..].trim_start().len());
    if let Some((from, to)) = byte_range(entry) {
        return (from == base + start).then_some(to - base);
    }
    let len = colonpair_len(entry, &text[start..])?;
    super::origin::set_span(entry, SrcSpan::new(base + start, base + start + len));
    super::infix_origins::derive(names, entry);
    Some(start + len)
}

/// The length of the colonpair `node` written at the start of `text`: `:z`,
/// `:!c`, `:2b`, or `:d` followed by its bracketed value. `None` when `text`
/// does not spell it.
// Cost: O(n), n = length of the colonpair's text.
pub(super) fn colonpair_len(node: &RakuAstNode, text: &str) -> Option<usize> {
    let key = leaf_text(node)?;
    let rest = text.strip_prefix(':')?;
    let rest = match node.class {
        RakuAstClass::ColonPairTrue | RakuAstClass::ColonPairValue => rest,
        RakuAstClass::ColonPairFalse => rest.strip_prefix('!')?,
        RakuAstClass::ColonPairNumber => {
            let digits = rest.find(|c: char| !c.is_ascii_digit())?;
            (digits > 0).then(|| &rest[digits..])?
        }
        _ => return None,
    };
    let after = rest.strip_prefix(key.as_str())?;
    let name_end = text.len() - after.len();
    if after.starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-') {
        return None;
    }
    if node.class != RakuAstClass::ColonPairValue {
        return Some(name_end);
    }
    Some(name_end + bracketed_len(after)?)
}

/// The length of the bracketed value `text` starts with (`(5)`, `<x y>`,
/// `[1, 2]`, `{...}`, `«a»`): to the bracket closing the first, counting
/// nested brackets of the same kind and skipping quoted strings in code
/// brackets.
// Cost: O(n), n = length of the value's text.
fn bracketed_len(text: &str) -> Option<usize> {
    let open = text.chars().next()?;
    let close = match open {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        '\u{ab}' => '\u{bb}',
        _ => return None,
    };
    let code = matches!(open, '(' | '[' | '{');
    let mut depth = 0usize;
    let mut quote: Option<char> = None;
    let mut chars = text.char_indices();
    while let Some((at, c)) = chars.next() {
        if let Some(q) = quote {
            if c == '\\' {
                chars.next();
            } else if c == q {
                quote = None;
            }
            continue;
        }
        if code && matches!(c, '"' | '\'') {
            quote = Some(c);
        } else if c == open {
            depth += 1;
        } else if c == close {
            depth -= 1;
            if depth == 0 {
                return Some(at + c.len_utf8());
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::bracketed_len;

    #[test]
    fn bracketed_values() {
        assert_eq!(bracketed_len("(5), :z"), Some(3));
        assert_eq!(bracketed_len("<x y>}"), Some(5));
        assert_eq!(bracketed_len("((1), \")\")x"), Some(10));
        assert_eq!(bracketed_len("\u{ab}a\u{bb}"), Some(5));
        assert_eq!(bracketed_len("(1"), None);
        assert_eq!(bracketed_len("x"), None);
    }
}
