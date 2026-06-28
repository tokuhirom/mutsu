use crate::ast::Expr;
use crate::value::Value;

pub(super) fn starts_with_term_token(input: &str) -> bool {
    let Some(ch) = input.chars().next() else {
        return false;
    };
    ch.is_ascii_digit()
        || ch.is_alphabetic()
        || matches!(
            ch,
            '$' | '@'
                | '%'
                | '&'
                | '\''
                | '"'
                | '\u{2018}'
                | '\u{2019}'
                | '\u{201A}'
                | '\u{201C}'
                | '\u{201D}'
                | '\u{201E}'
                | '('
                | '['
                | '{'
                | ':'
        )
}

/// Returns true if the input starts with a token that is unambiguously a new
/// term after a postfix `++`/`--`: a sigilled variable (`$`, `@`, `%`), a digit,
/// or a string literal. These can never begin an infix operator, so flagging
/// them avoids false positives with word infixes (`and`, `or`, `xx`, ...).
pub(super) fn starts_with_postfix_ambiguous_term(input: &str) -> bool {
    let Some(ch) = input.chars().next() else {
        return false;
    };
    ch.is_ascii_digit()
        || matches!(
            ch,
            '$' | '@'
                | '%'
                | '\''
                | '"'
                | '\u{2018}'
                | '\u{2019}'
                | '\u{201A}'
                | '\u{201C}'
                | '\u{201D}'
                | '\u{201E}'
        )
}

/// Returns true if the input starts with a token that is unambiguously a new
/// term (not an infix operator or statement modifier).  More conservative than
/// `starts_with_term_token`: only digits and quote characters, which can never
/// be the start of an operator.
pub(super) fn starts_with_unambiguous_term(input: &str) -> bool {
    let Some(ch) = input.chars().next() else {
        return false;
    };
    ch.is_ascii_digit()
        || matches!(
            ch,
            '\'' | '"'
                | '\u{2018}'
                | '\u{2019}'
                | '\u{201A}'
                | '\u{201C}'
                | '\u{201D}'
                | '\u{201E}'
                // U+221E INFINITY: the `Inf` literal. A value directly followed
                // by `∞` with no infix operator (`1∞`) is a bogus postfix in
                // Raku -> X::Syntax::Confused.
                | '\u{221E}'
        )
}

/// Returns true if the expression is a pure value that cannot take arguments
/// (i.e., a literal or variable, not a function call or bareword that might
/// be a function name).  Used to detect "two terms in a row" parse errors.
/// Returns `true` if the expression is a literal value (number, string, etc.)
/// that cannot appear as the left-hand side of a bind operator (`:=`).
pub(super) fn is_literal_expr(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(_))
}

/// True when the LHS of an indexed `:=` bind targets an immutable container:
/// a literal scalar (`10[0] := 1`, `"Hi"[0] := 1`) or an all-literal list
/// (`(1,2)[0] := 3`). Binding into such a target is illegal → X::Bind.
pub(super) fn index_bind_target_is_immutable(target: &Expr) -> bool {
    match target {
        Expr::Grouped(inner) => index_bind_target_is_immutable(inner),
        Expr::Literal(v) => matches!(
            v,
            Value::Int(_) | Value::Str(_) | Value::Num(_) | Value::Rat(..) | Value::Bool(_)
        ),
        Expr::BareWord(name) => crate::runtime::Interpreter::is_builtin_type(name),
        Expr::ArrayLiteral(elems) => {
            !elems.is_empty() && elems.iter().all(|e| matches!(e, Expr::Literal(_)))
        }
        _ => false,
    }
}

/// True for the Raku pseudo-package names (lexical/dynamic scope pseudo-stashes).
/// Binding to one of these (`OUTER := 5`) is illegal → X::Bind.
pub(super) fn is_pseudo_package(name: &str) -> bool {
    matches!(
        name,
        "MY" | "OUR"
            | "CORE"
            | "GLOBAL"
            | "PROCESS"
            | "UNIT"
            | "SETTING"
            | "OUTER"
            | "CALLER"
            | "CALLERS"
            | "DYNAMIC"
            | "COMPILING"
            | "CLIENT"
            | "LEXICAL"
    )
}

pub(super) fn is_pure_value_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Literal(_)
            | Expr::Var(_)
            | Expr::ArrayVar(_)
            | Expr::HashVar(_)
            | Expr::StringInterpolation(_)
            | Expr::ArrayLiteral(_)
    )
}
