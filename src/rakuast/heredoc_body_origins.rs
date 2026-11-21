//! Dropping the origins a heredoc's body took from another term of its
//! shape (ADR-10723 Stage 2, #15780): `is qq:to/E/, "@l[]\n"` writes a
//! string of the body's shape before the body, and the ranges noted for the
//! one went to the other.

use super::RakuAstNode;

/// `qq:to/E/, "@l[]\n"`: when `node`, a heredoc's body written at
/// `written`, has a range outside it, a string of the same shape took the
/// body's ranges; they are all dropped, to be placed again from `written`.
// Cost: O(n), n = nodes under `node`.
pub(super) fn drop_borrowed(node: &mut RakuAstNode, written: crate::ast::SrcSpan) {
    if super::infix_origins::byte_range(node)
        .is_some_and(|(from, to)| from < written.from as usize || to > written.to as usize)
    {
        super::origin::clear_tree(node);
    }
}
