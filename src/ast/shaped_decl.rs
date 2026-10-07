//! A shaped array declaration, `my @a[3]` / `my @a[2;3] = ...`.
//!
//! The parser has no node for the shape: the declaration's initializer is
//! `Array.new(shape => DIMS)`, with `data => VALUE` when it is initialized
//! (and then the `__shaped_decl` trait, so the value-copy of a plain `my @u =
//! @shaped` does not strip the shape). Rakudo has a `shape` on the
//! `VarDeclaration::Simple`. [`new_expr`] / [`new_with_data_expr`] are the
//! parser's and the RakuAST lowering's builders, [`split`] the converter's
//! recognizer.

use super::Expr;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

/// The trait marking an initialized shaped declaration.
pub(crate) const SHAPED_DECL: &str = "__shaped_decl";

fn pair(key: &str, value: Expr) -> Expr {
    Expr::Binary {
        left: Box::new(Expr::Literal(Value::str_from(key))),
        op: TokenKind::FatArrow,
        right: Box::new(value),
        form: Default::default(),
    }
}

fn shape_value(dims: Vec<Expr>) -> Expr {
    if dims.len() == 1 {
        dims.into_iter()
            .next()
            .unwrap_or(Expr::Literal(Value::int(0)))
    } else {
        Expr::ArrayLiteral(dims)
    }
}

fn array_new(args: Vec<Expr>) -> Expr {
    Expr::MethodCall {
        target: Box::new(Expr::BareWord("Array".to_string())),
        name: Symbol::intern("new"),
        args,
        modifier: None,
        quoted: false,
        sugar: false,
    }
}

/// The initializer of an uninitialized shaped array: `Array.new(shape => ...)`.
// Cost: O(d), d = dimensions.
pub(crate) fn new_expr(dims: Vec<Expr>) -> Expr {
    array_new(vec![pair("shape", shape_value(dims))])
}

/// The initializer of an initialized shaped array.
// Cost: O(d), d = dimensions.
pub(crate) fn new_with_data_expr(dims: Vec<Expr>, data: Expr) -> Expr {
    array_new(vec![pair("shape", shape_value(dims)), pair("data", data)])
}

/// The dimensions (and the data, when there is one) of a shaped array's
/// initializer, or `None` for any other expression.
// Cost: O(d), d = dimensions (they are cloned).
pub(crate) fn split(expr: &Expr) -> Option<(Vec<Expr>, Option<&Expr>)> {
    let Expr::MethodCall {
        target,
        name,
        args,
        modifier: None,
        quoted: false,
        ..
    } = expr
    else {
        return None;
    };
    if !matches!(target.as_ref(), Expr::BareWord(t) if t == "Array") || name.as_str() != "new" {
        return None;
    }
    let value_of = |at: usize, key: &str| match args.get(at) {
        Some(Expr::Binary {
            left,
            op: TokenKind::FatArrow,
            right,
            ..
        }) if matches!(left.as_ref(), Expr::Literal(v) if v.as_str() == Some(key)) => {
            Some(right.as_ref())
        }
        _ => None,
    };
    let shape = value_of(0, "shape")?;
    let data = match args.len() {
        1 => None,
        2 => Some(value_of(1, "data")?),
        _ => return None,
    };
    let dims = match shape {
        Expr::ArrayLiteral(dims) => dims.clone(),
        single => vec![single.clone()],
    };
    Some((dims, data))
}
