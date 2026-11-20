//! A chain of three or more `Z` operands across the RakuAST boundary.
//!
//! The parser runs `A Z B Z C` (and `A Z+ B Z+ C`, and the comma-column form
//! `1, 2 Z(&) 3, 4 Z(&) 5`) as one `zip(A, B, C, :with(&infix:<+>))` call,
//! marked `Spelling::ZipChain`. Rakudo keeps the list infix over every operand
//! (`ApplyListInfix(MetaInfix::Zip(Infix("+")), operands)`; measured on
//! 2026.11). The view rebuilds the left-nested meta-op chain the source
//! spelled and renders it as the ordinary `Z` chain does; lowering takes the
//! chain back to the same `zip` call.

use super::RakuAstNode;
use super::view::{unsupported, view_expr};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::token_kind::TokenKind;
use crate::value::{RuntimeError, ValueView};

/// The `ApplyListInfix` of a marked `zip(...)` call.
// Cost: O(n), n = size of the operands.
pub(super) fn view_node(names: &NameScope, expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Call { args, .. } = expr else {
        return view_expr(names, expr);
    };
    let (operands, op) = match args.split_last() {
        Some((
            Expr::Binary {
                left,
                op: TokenKind::FatArrow,
                right,
                ..
            },
            operands,
        )) if is_with_key(left) => (operands, combining_op(right)?),
        _ => (args.as_slice(), String::new()),
    };
    let mut operands = operands.iter().cloned();
    let Some(mut chain) = operands.next() else {
        return Err(unsupported("empty zip chain"));
    };
    for operand in operands {
        chain = Expr::MetaOp {
            meta: "Z".to_string(),
            op: op.clone(),
            left: Box::new(chain),
            right: Box::new(operand),
        };
    }
    view_expr(names, &chain)
}

/// Whether `key` is the `with` key of the combining-operator adverb.
fn is_with_key(key: &Expr) -> bool {
    matches!(key, Expr::Literal(v) if matches!(v.view(), ValueView::Str(s) if s.as_str() == "with"))
}

/// The operator of a `&infix:<OP>` adverb value.
fn combining_op(value: &Expr) -> Result<String, RuntimeError> {
    match value {
        Expr::CodeVar(name) => name
            .strip_prefix("infix:<")
            .and_then(|rest| rest.strip_suffix('>'))
            .map(str::to_string)
            .ok_or_else(|| unsupported("zip chain operator")),
        _ => Err(unsupported("zip chain operator")),
    }
}
