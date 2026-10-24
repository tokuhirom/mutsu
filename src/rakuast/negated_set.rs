//! Lowering of the infixes whose RakuAST form the parser desugars (#14912): the
//! precomposed negated set glyphs (`∉`) and the left-exclusive sequences
//! (`^...`). The read direction is in `convert.rs`.

use super::RakuAstNode;
use crate::ast::Expr;
use crate::token_kind::{NOT_THREADED, TokenKind};
use crate::value::ValueView;

/// The positive relation of an `Infix` written as a precomposed negated set
/// glyph (`∉` -> `(elem)`), or `None` for any other infix.
// Cost: O(1).
pub(super) fn glyph_relation(infix: &RakuAstNode) -> Option<TokenKind> {
    let name = super::lower::positional_leaf(infix).ok()?;
    let ValueView::Str(text) = name.view() else {
        return None;
    };
    let mut chars = text.chars();
    let glyph = chars.next()?;
    if chars.next().is_some() {
        return None;
    }
    crate::parser::negated_glyph_relation(glyph)
}

/// `left GLYPH right` as the parser builds it: the junction-threading negation
/// of the positive relation.
// Cost: O(1).
pub(super) fn threaded_negation(left: Expr, relation: TokenKind, right: Expr) -> Expr {
    Expr::Unary {
        op: TokenKind::Ident(NOT_THREADED.to_string()),
        expr: Box::new(Expr::Binary {
            left: Box::new(left),
            op: relation,
            right: Box::new(right),
            form: Default::default(),
        }),
        word: false,
    }
}

/// The plain sequence operator a left-exclusive one (`^...`, `^…^`) builds on.
// Cost: O(1).
pub(super) fn left_exclusive_base(op: &str) -> Option<&'static str> {
    match op {
        "^..." | "^\u{2026}" => Some("..."),
        "^...^" | "^\u{2026}^" => Some("...^"),
        _ => None,
    }
}

/// `.skip(1)` over a sequence: what the leading `^` of `^...` drops.
// Cost: O(1).
pub(super) fn skip_first(sequence: Expr) -> Expr {
    Expr::MethodCall {
        target: Box::new(sequence),
        name: crate::symbol::Symbol::intern("skip"),
        args: vec![Expr::Literal(crate::value::Value::int(1))],
        modifier: None,
        quoted: false,
        sugar: false,
    }
}
