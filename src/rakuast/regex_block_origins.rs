//! Origins of a code block inside a regex body (ADR-10723 Stage 2; #15780):
//! the block of a predicate assertion (`<?{ so $<word> }>`, `<!{ ... }>`)
//! and of a block quantifier (`\d ** {$n}`).
//!
//! The regex parser keeps the block's code without positions, so the code
//! between the braces is parsed again as a fragment (`fragment_origins`) and
//! its ranges are copied onto the block's statements; the `Block`, its
//! `Blockoid` and its `StatementList` are placed from the braces. Exact or
//! absent: a block whose braces cannot be matched in the text stays without
//! ranges.

use super::infix_origins::with_child_at;
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;
use crate::value::{Value, ValueView};

/// Place the `Block` held by the `index`-th field of `node`, written at the
/// start of `text` (byte `at` of the unit); the length of its text.
// Cost: O(n), n = length of the block's text, plus the parse of its code.
pub(super) fn held_code_block(
    names: &NameScope,
    node: &mut RakuAstNode,
    index: usize,
    text: &str,
    at: usize,
) -> Option<usize> {
    let field = node.fields.get_mut(index)?;
    let RakuAstFieldValue::Node(held) = &mut field.value else {
        return None;
    };
    let ValueView::RakuAst(block) = held.view() else {
        return None;
    };
    let mut block = RakuAstNode::clone(block);
    let len = code_block(names, &mut block, text, at);
    *held = Value::rakuast(Box::new(block));
    len
}

/// Place `block`, a `Block` written at the start of `text`, which sits at
/// byte `at` of the unit. The length of its text (through its `}`), or
/// `None` when `text` does not start with a block whose end can be found.
// Cost: O(n), n = length of the block's text, plus the parse of its code.
fn code_block(
    names: &NameScope,
    block: &mut RakuAstNode,
    text: &str,
    at: usize,
) -> Option<usize> {
    if block.class != RakuAstClass::Block || !text.starts_with('{') {
        return None;
    }
    let close = closing_brace(text)?;
    let whole = SrcSpan::new(at, at + close + 1);
    super::origin::set_span(block, whole);
    with_child_at(block, 0, |blockoid| {
        if blockoid.class != RakuAstClass::Blockoid {
            return;
        }
        super::origin::set_span(blockoid, whole);
        with_child_at(blockoid, 0, |list| {
            super::fragment_origins::statements(names, list, &text[1..close], at + 1);
            super::list_origins::statement_list(list, SrcSpan::new(at + 1, at + close));
        });
    });
    Some(close + 1)
}

/// The offset of the `}` that closes the `{` `text` starts with. A quoted
/// string (`"a"`, `'b'`) is skipped whole, so a brace inside it is not
/// counted; `None` when a comment or a backslash comes first, or a quote does
/// not end.
// Cost: O(n), n = length of the block's text.
fn closing_brace(text: &str) -> Option<usize> {
    let mut depth = 0usize;
    let mut chars = text.char_indices().peekable();
    let mut prev = None;
    while let Some((at, c)) = chars.next() {
        match c {
            '{' => depth += 1,
            '}' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(at);
                }
            }
            // `don't-x`: an apostrophe between letters is part of a name.
            '\'' if prev.is_some_and(char::is_alphanumeric)
                && chars.peek().is_some_and(|&(_, n)| n.is_alphabetic()) => {}
            '"' | '\'' => {
                // Through the closing quote; an escape skips the next char.
                loop {
                    match chars.next()?.1 {
                        '\\' => {
                            chars.next()?;
                        }
                        q if q == c => break,
                        _ => {}
                    }
                }
            }
            '#' | '\\' => return None,
            _ => {}
        }
        prev = Some(c);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::closing_brace;

    #[test]
    fn skips_quoted_strings() {
        assert_eq!(closing_brace(r#"{ "a}" }"#), Some(7));
        assert_eq!(closing_brace("{ 'b\\'}' }"), Some(10));
        assert_eq!(closing_brace("{ f(don't-x) }"), Some(13));
        assert_eq!(closing_brace(r#"{ "open }"#), None);
        assert_eq!(closing_brace("{ # } \n }"), None);
    }
}
