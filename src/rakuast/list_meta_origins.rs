//! The operator of a list-associative metaop application (`1 X~ 2 X~ 3`,
//! `@a Z+ @b`; #15780): a `MetaInfix::Cross` / `MetaInfix::Zip` over an
//! `Infix`, placed on the last operator as a plain list infix is.

use super::infix_origins::{child, child_at, leaf_text, with_child, with_child_at};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// The text an `ApplyListInfix`'s metaop is written as (`X~`, `Z+`), or `None`
/// when its operator is not a `Cross` / `Zip` over a plain `Infix`.
// Cost: O(f + l), f = fields of the metaop, l = length of the operator.
pub(super) fn written(node: &RakuAstNode) -> Option<String> {
    let meta = child(node, "infix")?;
    let prefix = match meta.class {
        RakuAstClass::MetaInfixCross => 'X',
        RakuAstClass::MetaInfixZip => 'Z',
        _ => return None,
    };
    let inner = child_at(meta, 0).filter(|inner| inner.class == RakuAstClass::Infix)?;
    Some(format!("{prefix}{}", leaf_text(inner)?))
}

/// Places the metaop over `at..at + op.len()` and the `Infix` it wraps after
/// its one-character prefix.
// Cost: O(f), f = fields of the metaop.
pub(super) fn place(node: &mut RakuAstNode, at: usize, op: &str) {
    with_child(node, "infix", |meta| {
        super::origin::set_span(meta, SrcSpan::new(at, at + op.len()));
        with_child_at(meta, 0, |inner| {
            super::origin::set_span(inner, SrcSpan::new(at + 1, at + op.len()))
        });
    });
}
