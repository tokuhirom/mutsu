//! Origins of a `{ ... }` block interpolated in a string (`"a{1 + 2}b"`;
//! ADR-10723 Stage 2, #15780).

use super::RakuAstNode;
use crate::ast::SrcSpan;

/// The range of a `{ ... }` block written at `cursor`, its `Blockoid`'s and
/// `StatementList`'s, and its statements' (from their expressions). Quoted
/// strings inside the block are skipped whole (`regex_block_origins`); a
/// block whose text holds a comment, a backslash or a `「」` quote, which
/// could hide a brace, is left alone.
// Cost: O(n + s), n = length of the block, s = its statements.
pub(super) fn block_at(block: &mut RakuAstNode, cursor: usize, end: usize) -> Option<(usize, usize)> {
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
            super::list_origins::statement_list(list, inside)
        })
    });
    Some((cursor, close + 1))
}
