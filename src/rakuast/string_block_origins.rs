//! Origins of a `{ ... }` block interpolated in a string (`"a{1 + 2}b"`;
//! ADR-10723 Stage 2, #15780).

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// The range of a `{ ... }` block written at `cursor`, its `Blockoid`'s and
/// `StatementList`'s, and its statements' (from their expressions). Quoted
/// strings inside the block are skipped whole (`regex_block_origins`); a
/// block whose text holds a comment, a backslash or a `「」` quote, which
/// could hide a brace, is left alone.
// Cost: O(n + s), n = length of the block, s = its statements.
pub(super) fn block_at(
    block: &mut RakuAstNode,
    cursor: usize,
    end: usize,
) -> Option<(usize, usize)> {
    let close = super::unit_source::with_text(SrcSpan::new(cursor, end), |text| {
        if !text.starts_with('{') {
            return None;
        }
        let close = super::regex_block_origins::closing_brace(text)?;
        (!text[..close].contains(['「', '」'])).then_some(cursor + close)
    })?;
    let inside = super::list_origins::bracketed(cursor, close + 1, ('{', '}'))?;
    let whole = SrcSpan::new(cursor, close + 1);
    super::routine_origins::block_origins(
        block,
        crate::ast::parsed::BlockSpans {
            span: whole,
            blockoid: whole,
            list: SrcSpan::NONE,
        },
    );
    super::infix_origins::with_child(block, "body", |blockoid| {
        super::infix_origins::with_child_at(blockoid, 0, |list| {
            // `"{:a(7)}"`: statements the parser left unplaced are read
            // again from the text between the braces.
            super::lone_term_origins::loop_statement(list, inside);
            super::list_origins::statement_list(list, inside);
            mark_statement_keys(list);
        })
    });
    Some((cursor, close + 1))
}

/// Rakudo's grammar attaches the statements in an interpolated block with
/// key origins, including statements whose range comes from their expression.
// Cost: O(n + s log l), n = nodes copied, s = statements, l = lines in the unit.
fn mark_statement_keys(list: &mut RakuAstNode) {
    for field in &mut list.fields {
        let RakuAstFieldValue::Node(held) = &mut field.value else {
            continue;
        };
        let ValueView::RakuAst(statement) = held.view() else {
            continue;
        };
        if statement.class != RakuAstClass::StatementExpression {
            continue;
        }
        let Some((from, to)) = super::infix_origins::byte_range(statement) else {
            continue;
        };
        let mut statement = RakuAstNode::clone(statement);
        statement.origin.0 = super::unit_source::span_origin(SrcSpan::new(from, to), true);
        *held = Value::rakuast(Box::new(statement));
    }
}
