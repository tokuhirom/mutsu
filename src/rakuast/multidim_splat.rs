//! `@a[|| @i]`: a list spread over the subscript's dimensions. Rakudo 2026.09
//! models the `||` as two `|` prefixes, `SemiList(ApplyPrefix(|,
//! ApplyPrefix(|, @i)))`; the parser keeps it as an `Expr::MultiDimIndex`
//! whose one dimension is `@i` (`convert::splat_dimensions` is the read side).

use super::lower::{named_child, positional_leaf};
use super::{RakuAstClass, RakuAstNode};
use crate::value::ValueView;

/// The operand `X` when `statement` is the one statement `| | X` of a
/// subscript's `SemiList`.
// Cost: O(1).
pub(super) fn spread_operand(statement: &RakuAstNode) -> Option<&RakuAstNode> {
    if statement.class != RakuAstClass::StatementExpression {
        return None;
    }
    let outer = slip_operand(named_child(statement, "expression").ok()?)?;
    slip_operand(outer)
}

/// The operand of an `ApplyPrefix(Prefix("|"), operand)`.
fn slip_operand(node: &RakuAstNode) -> Option<&RakuAstNode> {
    if node.class != RakuAstClass::ApplyPrefix {
        return None;
    }
    let prefix = named_child(node, "prefix").ok()?;
    let is_slip = prefix.class == RakuAstClass::Prefix
        && matches!(positional_leaf(prefix).ok()?.view(), ValueView::Str(s) if s.as_str() == "|");
    if !is_slip {
        return None;
    }
    named_child(node, "operand").ok()
}
