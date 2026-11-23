//! The names `sink_warn` gives sunk literals and type objects in rakudo's
//! "Useless use of X in sink context" worry, and the operators it treats as
//! pure.

use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::value::Value;
use crate::value::ValueView;

pub(super) fn describe_literal(v: &Value) -> Option<String> {
    match v.view() {
        ValueView::Int(n) => Some(format!("constant integer {}", n)),
        ValueView::BigInt(n) => Some(format!("constant integer {}", *n)),
        ValueView::Num(_) => Some(format!(
            "constant floating-point number {}",
            v.to_string_value()
        )),
        ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => {
            Some(format!("constant rational {}", v.to_string_value()))
        }
        ValueView::Str(s) => Some(format!("constant string \"{}\"", *s)),
        ValueView::Complex(..) => Some(format!("constant value {}", v.to_string_value())),
        ValueView::Package(_) => Some(format!("constant value {}", v.to_string_value())),
        ValueView::Mixin(inner, _) => {
            // `<123.456>` etc. parse to a Mixin carrying a Str representation;
            // describe by the inner value's category but the stringified whole.
            describe_literal(inner).map(|_| format!("constant value {}", v.to_string_value()))
        }
        _ => None,
    }
}

/// Like [`describe_literal`], but substitutes the literal's original source text
/// for the canonical number rendering, keeping the user's radix / scientific /
/// Unicode format in the warning.
pub(super) fn describe_literal_with_src(v: &Value, src: &str) -> Option<String> {
    match v.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => Some(format!("constant integer {}", src)),
        ValueView::Num(_) => Some(format!("constant floating-point number {}", src)),
        // A source-preserving colonpair (`:foo(42)`): echo the original syntax
        // quoted, matching Raku's `Useless use of ":foo(42)" in sink context`.
        ValueView::Pair(..) | ValueView::ValuePair(..) => Some(format!("\"{}\"", src)),
        _ => describe_literal(v),
    }
}

/// Pure infix operators that produce a value with no side effect. Operators not
/// listed here are treated as potentially effectful and never flagged.
pub(super) fn pure_op_symbol(op: &TokenKind) -> Option<&'static str> {
    Some(match op {
        TokenKind::Plus => "+",
        TokenKind::Minus => "-",
        TokenKind::Star => "*",
        TokenKind::StarStar => "**",
        TokenKind::Slash => "/",
        TokenKind::Percent => "%",
        TokenKind::Tilde => "~",
        TokenKind::EqEq => "==",
        TokenKind::BangEq => "!=",
        TokenKind::Lt => "<",
        TokenKind::Lte => "<=",
        TokenKind::Gt => ">",
        TokenKind::Gte => ">=",
        TokenKind::LtEqGt => "<=>",
        TokenKind::FatArrow => "=>",
        _ => return None,
    })
}

/// Pure prefix operators that produce a value with no side effect. Operators
/// not listed here are treated as potentially effectful and never flagged.
pub(super) fn pure_prefix_symbol(op: &TokenKind) -> Option<&'static str> {
    Some(match op {
        TokenKind::Plus => "+",
        TokenKind::Minus => "-",
        TokenKind::Tilde => "~",
        TokenKind::Bang => "!",
        TokenKind::Question => "?",
        _ => return None,
    })
}

/// Core type-object names that appear as bare words and are useless in sink
/// context (e.g. `Mu`, `Any`, `Cool`).
pub(super) fn is_type_name(s: &str) -> bool {
    matches!(
        s,
        "Mu" | "Any"
            | "Cool"
            | "Junction"
            | "Int"
            | "Num"
            | "Rat"
            | "Complex"
            | "Str"
            | "Bool"
            | "Nil"
            | "Real"
            | "Numeric"
    )
}

/// Whether `expr` hands its sink on rather than being one useless value: a
/// list, a parenthesized expression, a spelled term or a `do` block.
// Cost: O(1).
pub(super) fn distributes_sink(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::ArrayLiteral(_) | Expr::Grouped(_) | Expr::Spelled(_) | Expr::DoBlock { .. }
    )
}

/// Whether `expr` is an operator application, which a worry names by its text.
// Cost: O(1).
pub(super) fn is_operator_expr(expr: &Expr) -> bool {
    matches!(expr, Expr::Binary { .. } | Expr::Unary { .. })
}
