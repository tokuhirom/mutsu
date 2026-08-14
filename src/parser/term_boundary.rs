//! Shared "two terms in a row" term-boundary detection.
//!
//! Several parse sites reach a fully-parsed term and must decide whether the
//! text that follows on the same line is a second, unparseable term (`1 1`,
//! `"a" "b"`, an initializer's `my $x = 1 1`, a listop's `say 1 1`) rather
//! than a legitimate continuation. What they share is the notion of what CAN
//! unambiguously start a *new* term; each site still owns its own list of
//! legitimate continuations (a bare statement's `;`/`}`/...,  a `my`
//! initializer's trailing comma-list, a listop's comma/adverb argument
//! continuations, ...) because those differ per site and getting that list
//! wrong rejects valid programs instead of merely missing a diagnosis. See
//! `todo/tickets/two-terms-in-a-row-is-not-a-parse-error.md`.

use crate::ast::Expr;

/// True if the input starts with a token that unambiguously begins a NEW
/// term rather than continuing the current one via an infix operator, a
/// postfix, or a statement modifier keyword. Deliberately conservative: only
/// digits and quote characters (plus the `Inf` literal's `\u{221E}`) can
/// never be the start of an operator or keyword, so only these are safe to
/// flag without risking a false positive against some infix/word-operator
/// this module doesn't enumerate.
pub(crate) fn starts_with_unambiguous_term(input: &str) -> bool {
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

/// True if the expression is a "pure value" (a literal, variable, or array
/// literal) that cannot itself take listop-style arguments. A following
/// unambiguous term can only be a syntax error for these — unlike, say, a
/// bareword `Call`, which may already have absorbed a following term as its
/// own argument, so the boundary check must not fire for it.
pub(crate) fn is_pure_value_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Literal(_)
            | Expr::LiteralSrc(..)
            | Expr::Var(_)
            | Expr::ArrayVar(_)
            | Expr::HashVar(_)
            | Expr::StringInterpolation(_)
            | Expr::ArrayLiteral(_)
    )
}
