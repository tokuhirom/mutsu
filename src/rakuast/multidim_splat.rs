//! `@a[|| @i]` (6.e): a list spread over the subscript's dimensions. Rakudo
//! 2026.09 spells it `SemiList(ApplyPrefix(Prefix::Multislice, @i))`; the
//! parser keeps it as an `Expr::MultiDimIndex` whose one dimension is `@i`
//! (`convert::splat_dimensions` is the read side).

use super::lower::named_child;
use super::{RakuAstClass, RakuAstNode};

/// The operand `X` when `statement` is the one statement `|| X` of a
/// subscript's `SemiList`.
// Cost: O(1).
pub(super) fn spread_operand(statement: &RakuAstNode) -> Option<&RakuAstNode> {
    if statement.class != RakuAstClass::StatementExpression {
        return None;
    }
    let apply = named_child(statement, "expression").ok()?;
    if apply.class != RakuAstClass::ApplyPrefix
        || named_child(apply, "prefix").ok()?.class != RakuAstClass::PrefixMultislice
    {
        return None;
    }
    named_child(apply, "operand").ok()
}
