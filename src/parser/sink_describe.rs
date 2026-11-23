//! What a sunk expression is called in rakudo's "Useless use of X in sink
//! context" worry (`sink_warn`), and whether it is useless at all.

use crate::ast::Expr;
use crate::token_kind::TokenKind;
use crate::value::Value;
use crate::value::ValueView;

/// Returns the description (the "X" in "Useless use of X in sink context") if
/// `expr` is a pure, side-effect-free value/expression, otherwise `None`.
pub(super) fn describe_useless(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Grouped(inner) => describe_useless(inner),
        Expr::Spelled(spelled) => describe_useless(&spelled.expr),
        Expr::PositionalPair(inner) => describe_useless(inner),
        Expr::Literal(v) => describe_literal(v),
        // A source-preserving numeric literal: describe by the inner value's
        // category but render the number using the original source text so the
        // warning keeps the radix / scientific / Unicode form the user wrote.
        Expr::LiteralSrc(v, src) => describe_literal_with_src(v, src),
        // A `Var` node whose name already carries a sigil (`@a`, `%h`) is a
        // synthetic artifact of `:=` bind desugaring — the trailing result
        // expression a container bind leaves in its SyntheticBlock — not a
        // user-written bare scalar. A real bare `@a` parses to `ArrayVar("a")`,
        // so this never suppresses a genuine sink warning; it only stops the
        // spurious "Useless use of $@a in sink context" on `my @a := @b`.
        // A bare anonymous `$` (the unnamed state scalar) parses to a synthetic
        // `__ANON_STATE#<id>__` name. Raku reports it as "unnamed $ variable",
        // not by its internal name.
        Expr::Var(n) if n.starts_with("__ANON_STATE#") => Some("unnamed $ variable".to_string()),
        // The reserved `$self` lexical key already carries its sigil (ADR-0061),
        // so it must be reported before the generic sigiled-name skip below.
        Expr::Var(n) if n == crate::env::LEX_SELF => Some(crate::env::LEX_SELF.to_string()),
        Expr::Var(n) if n.starts_with(['$', '@', '%', '&']) => None,
        // `$.name` / `@.name` / `%.name` is a method call on `self`
        // (`gather { $.take-subs }` in Test::Describe), never a useless value.
        Expr::Var(n) | Expr::ArrayVar(n) | Expr::HashVar(n) if n.starts_with('.') => None,
        Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) => expr.sigiled_var_name(),
        Expr::ArrayLiteral(elems) if elems.is_empty() => Some("()".to_string()),
        Expr::BareWord(s) if is_type_name(s) => Some(s.clone()),
        Expr::Binary {
            left, op, right, ..
        } => {
            pure_op_symbol(op)?;
            // Only flag when both operands are themselves useless (no side
            // effects), e.g. `1 + 2` but not `$x + foo()`. Rakudo names the
            // expression by its spelling, which `render_source` approximates
            // (the whole-statement case reads the source itself).
            pure_operand(left)?;
            pure_operand(right)?;
            render_source(expr)
        }
        Expr::Unary {
            op, expr: inner, ..
        } => {
            pure_prefix_symbol(op)?;
            // Only flag when the operand itself is useless (no side effects),
            // e.g. `-5` / `?5` / `-$x` but not `-foo()`.
            describe_useless(inner)?;
            render_source(expr)
        }
        _ => None,
    }
}

/// Whether an operand of a pure operator has no side effect: a useless value,
/// or an array constructor of such values (`[] + 1` is useless, though a bare
/// `[]` statement is not reported).
// Cost: O(n), n = the size of the operand.
fn pure_operand(expr: &Expr) -> Option<()> {
    if let Expr::BracketArray(elems, _) = expr {
        return elems.iter().try_for_each(pure_operand);
    }
    describe_useless(expr).map(|_| ())
}

fn describe_literal(v: &Value) -> Option<String> {
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
fn describe_literal_with_src(v: &Value, src: &str) -> Option<String> {
    match v.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => Some(format!("constant integer {}", src)),
        ValueView::Num(_) => Some(format!("constant floating-point number {}", src)),
        // A source-preserving colonpair (`:foo(42)`): echo the original syntax
        // quoted, matching Raku's `Useless use of ":foo(42)" in sink context`.
        ValueView::Pair(..) | ValueView::ValuePair(..) => Some(format!("\"{}\"", src)),
        _ => describe_literal(v),
    }
}

/// Approximate the source text of a useless expression, for an operator
/// expression whose own text the warning cannot read back.
fn render_source(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Grouped(inner) | Expr::PositionalPair(inner) => render_source(inner),
        Expr::Literal(lit) => match lit.view() {
            ValueView::Str(s) => Some(format!("\"{}\"", *s)),
            _ => Some(lit.to_string_value()),
        },
        Expr::LiteralSrc(_, src) => Some(src.to_string()),
        Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) => expr.sigiled_var_name(),
        Expr::BareWord(s) => Some(s.clone()),
        Expr::BracketArray(elems, _) => Some(format!(
            "[{}]",
            elems
                .iter()
                .map(render_source)
                .collect::<Option<Vec<_>>>()?
                .join(", ")
        )),
        Expr::Unary {
            op, expr: inner, ..
        } => {
            let sym = pure_prefix_symbol(op)?;
            Some(format!("{}{}", sym, render_source(inner)?))
        }
        Expr::Binary {
            left, op, right, ..
        } => {
            let sym = pure_op_symbol(op)?;
            let l = render_source(left)?;
            let r = render_source(right)?;
            Some(format!("{} {} {}", l, sym, r))
        }
        _ => None,
    }
}

/// Pure infix operators that produce a value with no side effect. Operators not
/// listed here are treated as potentially effectful and never flagged.
fn pure_op_symbol(op: &TokenKind) -> Option<&'static str> {
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
fn pure_prefix_symbol(op: &TokenKind) -> Option<&'static str> {
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
fn is_type_name(s: &str) -> bool {
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
