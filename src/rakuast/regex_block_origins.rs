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
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;

/// Place `block`, a `Block` written at the start of `text`, which sits at
/// byte `at` of the unit. The length of its text (through its `}`), or
/// `None` when `text` does not start with a block whose end can be found.
// Cost: O(n), n = length of the block's text, plus the parse of its code.
pub(super) fn code_block(
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

/// The offset of the `}` that closes the `{` `text` starts with; `None` when
/// a quote or comment comes first (a brace inside it would be miscounted).
// Cost: O(n), n = length of the block's text.
fn closing_brace(text: &str) -> Option<usize> {
    let mut depth = 0usize;
    for (at, c) in text.char_indices() {
        match c {
            '{' => depth += 1,
            '}' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    return Some(at);
                }
            }
            '"' | '\'' | '#' | '\\' => return None,
            _ => {}
        }
    }
    None
}
