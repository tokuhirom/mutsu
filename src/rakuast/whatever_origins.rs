//! Origins of an infix application with a priming `*` operand (`* + 1`,
//! `1 + *`). Rakudo gives the `WhateverCode::Argument` no origin of its own,
//! yet the application spans it; the `*` is found in the source next to the
//! other operand and the operator, and is trusted only when written there.

use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// A byte range of the source, when known.
type Range = Option<(usize, usize)>;

/// Whether `node`'s operand `name` is a priming `*`.
// Cost: O(f), f = fields of `node`.
fn is_priming(node: &RakuAstNode, name: &str) -> bool {
    super::infix_origins::child(node, name)
        .is_some_and(|c| c.class == RakuAstClass::WhateverCodeArgument)
}

/// The byte ranges of the left and right operands of `node`, where an
/// origin-less priming `*` is located from the range of the other operand
/// and the operator spelling `op`.
// Cost: O(n), n = length of the source before / after the operand.
pub(super) fn operand_ranges(
    node: &RakuAstNode,
    op: &str,
    left: Range,
    right: Range,
) -> (Range, Range) {
    let mut left = left;
    let mut right = right;
    if left.is_none()
        && is_priming(node, "left")
        && let Some((right_start, _)) = right
    {
        left = super::unit_source::with_text(SrcSpan::new(0, right_start), |before| {
            let head = before
                .trim_end()
                .strip_suffix(op)?
                .trim_end()
                .strip_suffix('*')?;
            Some((head.len(), head.len() + 1))
        });
    } else if right.is_none()
        && is_priming(node, "right")
        && let Some((_, left_end)) = left
    {
        right = super::unit_source::with_text_from(left_end, |after| {
            let rest = after.trim_start().strip_prefix(op)?;
            let star = rest.trim_start();
            star.starts_with('*').then(|| {
                let at = left_end + (after.len() - star.len());
                (at, at + 1)
            })
        });
    }
    (left, right)
}
