//! Operand lists of same-operator chains.
//!
//! The parser nests a chain of one infix left-associatively: `a op b op c`
//! is `(a op b) op c`, and `a X b X c` is `MetaOp(X, MetaOp(X, a, b), c)`.
//! Every consumer that treats the chain as one list-associative application
//! (the `^^` compiler and sink warning, RakuAST's `ApplyListInfix`, the
//! multi-way `X`/`Z` metaops) walks that left spine. Parentheses end a chain:
//! `a op (b op c)` keeps its `Grouped` right operand whole.

use super::Expr;
use crate::token_kind::TokenKind;

impl Expr {
    /// The operands of the left-nested `op` chain rooted at `self`, in source
    /// order: `[a, b, c]` for `(a op b) op c`; `[self]` when `self` is not an
    /// `op` application.
    // Cost: O(n), n = number of operands in the chain.
    pub(crate) fn flatten_binary_chain(&self, op: &TokenKind) -> Vec<&Expr> {
        let mut rights = Vec::new();
        let mut cur = self;
        while let Expr::Binary {
            left,
            op: cur_op,
            right,
        } = cur
            && cur_op == op
        {
            rights.push(&**right);
            cur = left;
        }
        let mut out = Vec::with_capacity(rights.len() + 1);
        out.push(cur);
        out.extend(rights.into_iter().rev());
        out
    }

    /// The operands of the left-nested `meta`/`op` metaoperator chain rooted at
    /// `self`, in source order: `[a, b, c]` for `a X+ b X+ c`; `[self]` when
    /// `self` is not that metaop.
    // Cost: O(n), n = number of operands in the chain.
    pub(crate) fn flatten_meta_chain(&self, meta: &str, op: &str) -> Vec<&Expr> {
        let mut rights = Vec::new();
        let mut cur = self;
        while let Expr::MetaOp {
            meta: cur_meta,
            op: cur_op,
            left,
            right,
        } = cur
            && cur_meta == meta
            && cur_op == op
        {
            rights.push(&**right);
            cur = left;
        }
        let mut out = Vec::with_capacity(rights.len() + 1);
        out.push(cur);
        out.extend(rights.into_iter().rev());
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    fn lit(n: i64) -> Expr {
        Expr::Literal(Value::int(n))
    }

    fn bin(left: Expr, op: TokenKind, right: Expr) -> Expr {
        Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right),
        }
    }

    fn meta(left: Expr, m: &str, op: &str, right: Expr) -> Expr {
        Expr::MetaOp {
            meta: m.to_string(),
            op: op.to_string(),
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    /// The integer literal each operand is, or `None` for a non-literal.
    fn ints(operands: Vec<&Expr>) -> Vec<Option<i64>> {
        operands
            .into_iter()
            .map(|e| match e {
                Expr::Literal(v) => v.as_int(),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn binary_chain_flattens_the_left_spine_only() {
        // (1 ^^ 2) ^^ 3
        let chain = bin(
            bin(lit(1), TokenKind::XorXor, lit(2)),
            TokenKind::XorXor,
            lit(3),
        );
        assert_eq!(
            ints(chain.flatten_binary_chain(&TokenKind::XorXor)),
            vec![Some(1), Some(2), Some(3)]
        );
        // 1 ^^ (2 ^^ 3): a parenthesised right operand stays whole.
        let grouped = bin(
            lit(1),
            TokenKind::XorXor,
            Expr::Grouped(Box::new(bin(lit(2), TokenKind::XorXor, lit(3)))),
        );
        assert_eq!(
            ints(grouped.flatten_binary_chain(&TokenKind::XorXor)),
            vec![Some(1), None]
        );
        // A different operator is a leaf.
        let other = bin(lit(1), TokenKind::OrOr, lit(2));
        let operands = other.flatten_binary_chain(&TokenKind::XorXor);
        assert_eq!(operands.len(), 1);
        assert!(std::ptr::eq(operands[0], &other));
    }

    #[test]
    fn meta_chain_requires_the_same_meta_and_op() {
        let chain = meta(meta(lit(1), "X", "+", lit(2)), "X", "+", lit(3));
        assert_eq!(
            ints(chain.flatten_meta_chain("X", "+")),
            vec![Some(1), Some(2), Some(3)]
        );
        let mixed = meta(meta(lit(1), "Z", "+", lit(2)), "X", "+", lit(3));
        assert_eq!(
            ints(mixed.flatten_meta_chain("X", "+")),
            vec![None, Some(3)]
        );
        let mixed_op = meta(meta(lit(1), "X", "*", lit(2)), "X", "+", lit(3));
        assert_eq!(
            ints(mixed_op.flatten_meta_chain("X", "+")),
            vec![None, Some(3)]
        );
    }
}
