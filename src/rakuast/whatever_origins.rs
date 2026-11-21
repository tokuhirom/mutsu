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
    // `* + *`: both operands primed, read off the application's own text.
    if left.is_none()
        && right.is_none()
        && is_priming(node, "left")
        && is_priming(node, "right")
        && let Some((from, to)) = super::infix_origins::byte_range(node)
    {
        let both = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
            let middle = text.strip_prefix('*')?.strip_suffix('*')?.trim();
            (middle == op).then_some(())
        });
        if both.is_some() {
            return (Some((from, from + 1)), Some((to - 1, to)));
        }
    }
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

/// Locate an origin-less priming `*` that starts or ends `node`, an
/// `ApplyListInfix` with a range (`* max *`, `1 min *`), in `ranges`, the
/// byte ranges of its `operands`: the first at the application's start, the
/// last at its end, when the text there is a `*`.
// Cost: O(k), k = operands.
pub(super) fn list_operand_ranges(
    node: &RakuAstNode,
    operands: &[crate::value::Value],
    ranges: &mut [Range],
) {
    let Some((from, to)) = super::infix_origins::byte_range(node) else {
        return;
    };
    let priming = |i: usize| {
        operands.get(i).is_some_and(|v| match v.view() {
            crate::value::ValueView::RakuAst(o) => o.class == RakuAstClass::WhateverCodeArgument,
            _ => false,
        })
    };
    let last = ranges.len().saturating_sub(1);
    let written = |at: usize| {
        super::unit_source::with_text(SrcSpan::new(at, at + 1), |t| (t == "*").then_some(()))
            .is_some()
    };
    if ranges.len() > 1 && ranges[0].is_none() && priming(0) && written(from) {
        ranges[0] = Some((from, from + 1));
    }
    if ranges.len() > 1 && ranges[last].is_none() && priming(last) && to > from && written(to - 1) {
        ranges[last] = Some((to - 1, to));
    }
}
