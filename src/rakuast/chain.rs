//! Chained comparisons (`0 <= $i < 3`) read back from RakuAST.
//!
//! Rakudo has no chain node: `a < b < c` is the left-nested
//! `ApplyInfix(ApplyInfix(a < b) < c)`, and the chain is the *associativity of
//! the infix*, applied when the tree is compiled. mutsu's parser builds a
//! [`Expr::ChainedCompare`] for such a run, so the lowering has to recognise
//! the nesting and rebuild the node: a plain `Binary` of a `Binary` compares
//! the first comparison's boolean with the next operand (`0 <= 9 < 3` is then
//! `True < 3`).
//!
//! Only the nesting the converter itself renders is a chain: the left operand
//! is an unparenthesized comparison, which for a negated link
//! (`a !before b before c`) is `ApplyPrefix(!, ApplyInfix)`. A parenthesized
//! left operand is a `Circumfix::Parentheses` node and stays a plain
//! comparison, exactly as in the parser's own tree.

use super::lower::{infix_token, lower_expr, named_child, prefix_token};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::value::RuntimeError;

/// One comparison of a chain: the node holding its `left` / `right` operands,
/// its operator, and whether it is the negated form (`!before`).
struct Link<'a> {
    operands: &'a RakuAstNode,
    op: TokenKind,
    negated: bool,
}

/// The comparison `node` is, or `None` when it is not an unparenthesized one.
// Cost: O(1).
fn link(node: &RakuAstNode) -> Option<Link<'_>> {
    let (operands, negated) = match node.class {
        RakuAstClass::ApplyInfix => (node, false),
        RakuAstClass::ApplyPrefix => {
            if prefix_token(named_child(node, "prefix").ok()?).ok()? != TokenKind::Bang {
                return None;
            }
            let operand = named_child(node, "operand").ok()?;
            if operand.class != RakuAstClass::ApplyInfix {
                return None;
            }
            (operand, true)
        }
        _ => return None,
    };
    let infix = named_child(operands, "infix").ok()?;
    if infix.class != RakuAstClass::Infix {
        return None;
    }
    let op = infix_token(infix).ok()?;
    crate::chain_compare::is_chain_op(&op).then_some(Link {
        operands,
        op,
        negated,
    })
}

/// `a < b < c` -> `Expr::ChainedCompare`, or `None` when `node` is not a chain
/// of at least two comparisons.
// Cost: O(n), n = size of the chain's operands.
pub(super) fn lower_chain(node: &RakuAstNode) -> Result<Option<Expr>, RuntimeError> {
    let mut links = Vec::new();
    let mut cursor = node;
    while let Some(l) = link(cursor) {
        let left = named_child(l.operands, "left")?;
        links.push(l);
        cursor = left;
    }
    if links.len() < 2 {
        return Ok(None);
    }
    // `links` runs from the last comparison back to the first; `cursor` is the
    // chain's first operand.
    let mut operands = vec![lower_expr(cursor)?];
    let mut ops = Vec::with_capacity(links.len());
    for l in links.iter().rev() {
        operands.push(lower_expr(named_child(l.operands, "right")?)?);
        ops.push((l.op.clone(), l.negated));
    }
    Ok(Some(Expr::ChainedCompare { operands, ops }))
}
