use crate::ast::Expr;
use crate::parser::expr::operators::enrich_expected_error;
use crate::parser::helpers::{is_ident_char, ws};
use crate::parser::parse_result::PResult;
use crate::token_kind::TokenKind;

use super::hyper_concat::concat_expr;
use super::meta_bracket::block_newline_terminates;

fn parse_set_op(input: &str) -> Option<(TokenKind, usize)> {
    if let Some((canonical, len)) = crate::parser::stmt::simple::l10n_match_infix(input) {
        match canonical.as_str() {
            "(elem)" => return Some((TokenKind::SetElem, len)),
            "(cont)" => return Some((TokenKind::SetCont, len)),
            _ => {}
        }
    }
    if input.starts_with("(==)") {
        Some((TokenKind::Ident("(==)".to_string()), 4))
    } else if input.starts_with('≡') {
        Some((TokenKind::Ident("≡".to_string()), '≡'.len_utf8()))
    } else if input.starts_with('≢') {
        Some((TokenKind::Ident("≢".to_string()), '≢'.len_utf8()))
    } else if input.starts_with("(|)") {
        Some((TokenKind::SetUnion, 3))
    } else if input.starts_with('∪') {
        Some((TokenKind::SetUnion, '∪'.len_utf8()))
    } else if input.starts_with("(&)") {
        Some((TokenKind::SetIntersect, 3))
    } else if input.starts_with('∩') {
        Some((TokenKind::SetIntersect, '∩'.len_utf8()))
    } else if input.starts_with("(.)") {
        Some((TokenKind::SetMultiply, 3))
    } else if input.starts_with('⊍') {
        Some((TokenKind::SetMultiply, '⊍'.len_utf8()))
    } else if input.starts_with("(+)") {
        Some((TokenKind::SetAddition, 3))
    } else if input.starts_with('⊎') {
        Some((TokenKind::SetAddition, '⊎'.len_utf8()))
    } else if input.starts_with("(-)") {
        Some((TokenKind::SetDiff, 3))
    } else if input.starts_with('∖') {
        Some((TokenKind::SetDiff, '∖'.len_utf8()))
    } else if input.starts_with("(^)") {
        Some((TokenKind::SetSymDiff, 3))
    } else if input.starts_with('⊖') {
        Some((TokenKind::SetSymDiff, '⊖'.len_utf8()))
    } else if input.starts_with("(<=)") {
        Some((TokenKind::SetSubset, 4))
    } else if input.starts_with('⊆') {
        Some((TokenKind::SetSubset, '⊆'.len_utf8()))
    // 6.c "precedes" (baggy subset) operators `(<+)` / `≼`: deprecated aliases
    // of `(<=)` / `⊆`, removed in 6.d. mutsu's `SetSubset` already compares
    // Bag/Mix multiplicities, so they map to the same TokenKind.
    } else if input.starts_with("(<+)") {
        Some((TokenKind::SetSubset, 4))
    } else if input.starts_with('≼') {
        Some((TokenKind::SetSubset, '≼'.len_utf8()))
    } else if input.starts_with("(>=)") {
        Some((TokenKind::SetSuperset, 4))
    } else if input.starts_with('⊇') {
        Some((TokenKind::SetSuperset, '⊇'.len_utf8()))
    // 6.c "succeeds" (baggy superset) operators `(>+)` / `≽`.
    } else if input.starts_with("(>+)") {
        Some((TokenKind::SetSuperset, 4))
    } else if input.starts_with('≽') {
        Some((TokenKind::SetSuperset, '≽'.len_utf8()))
    } else if input.starts_with("!(<)") {
        Some((TokenKind::Ident("⊄".to_string()), 4))
    } else if input.starts_with("(<)") {
        Some((TokenKind::SetStrictSubset, 3))
    } else if input.starts_with('⊂') {
        Some((TokenKind::SetStrictSubset, '⊂'.len_utf8()))
    } else if input.starts_with("(>)") {
        Some((TokenKind::SetStrictSuperset, 3))
    } else if input.starts_with('⊃') {
        Some((TokenKind::SetStrictSuperset, '⊃'.len_utf8()))
    } else if input.starts_with('⊄') {
        Some((TokenKind::Ident("⊄".to_string()), '⊄'.len_utf8()))
    } else if input.starts_with('⊅') {
        Some((TokenKind::Ident("⊅".to_string()), '⊅'.len_utf8()))
    } else if input.starts_with("(elem)") {
        Some((TokenKind::SetElem, 6))
    } else if input.starts_with('∈') {
        Some((TokenKind::SetElem, '∈'.len_utf8()))
    } else if input.starts_with("(cont)") {
        Some((TokenKind::SetCont, 6))
    } else if input.starts_with('∋') {
        Some((TokenKind::SetCont, '∋'.len_utf8()))
    } else {
        None
    }
}

/// Parse a negated set-membership / set-relational operator that returns a Bool
/// and can therefore be negated with the `!` meta-prefix or written with a
/// precomposed Unicode "negated" glyph. Returns the *positive* `TokenKind`,
/// which the caller wraps in a `Bang` unary.
///
/// Handles the ASCII forms `!(elem)`, `!(cont)`, `!(<=)`, `!(>=)`, `!(<)`,
/// `!(>)`, `!(==)`, the same relations spelled with their Unicode glyph
/// (`!\u{2208}`, `!\u{2286}`, ...), and the precomposed negated glyphs `\u{2209}`,
/// `\u{220C}`, `\u{2288}`, `\u{2289}`.
///
/// The `!` meta-prefix applies to any Bool-returning infix, so rather than
/// listing the negatable spellings a second time this defers to `parse_set_op`
/// and keeps whatever it returns if the relation is iffy. That is what admits
/// `@vars .= grep: * !\u{2208} @$positional` (Math::Symbolic), which the ASCII-only
/// list used to reject. The non-Bool set operators (`\u{222a}`, `\u{2229}`, `\u{2216}`, ...) stay
/// out, so `!\u{222a}` is still not an operator.
fn parse_negated_set_op(input: &str) -> Option<(TokenKind, usize)> {
    if let Some(rest) = input.strip_prefix('!') {
        let (tok, len) = parse_set_op(rest)?;
        if !is_iffy_set_op(&tok) {
            return None;
        }
        return Some((tok, 1 + len));
    }
    // Precomposed Unicode negated glyphs missing from `parse_set_op`.
    if input.starts_with('\u{2209}') {
        Some((TokenKind::SetElem, '\u{2209}'.len_utf8()))
    } else if input.starts_with('\u{220C}') {
        Some((TokenKind::SetCont, '\u{220C}'.len_utf8()))
    } else if input.starts_with('\u{2288}') {
        Some((TokenKind::SetSubset, '\u{2288}'.len_utf8()))
    } else if input.starts_with('\u{2289}') {
        Some((TokenKind::SetSuperset, '\u{2289}'.len_utf8()))
    } else {
        None
    }
}

/// True for the set operators that return a Bool and can therefore carry the
/// `!` meta-negation. `(==)`/`\u{2261}` are set equality, also Bool-valued.
fn is_iffy_set_op(tok: &TokenKind) -> bool {
    match tok {
        TokenKind::SetElem
        | TokenKind::SetCont
        | TokenKind::SetSubset
        | TokenKind::SetSuperset
        | TokenKind::SetStrictSubset
        | TokenKind::SetStrictSuperset => true,
        TokenKind::Ident(name) => matches!(
            name.as_str(),
            "(==)" | "\u{2261}" | "\u{2262}" | "\u{2284}" | "\u{2285}"
        ),
        _ => false,
    }
}

/// Structural infix: but, does, set operators
pub(crate) fn structural_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = concat_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if block_newline_terminates(input, rest, r) {
            break;
        }
        let localized = crate::parser::stmt::simple::l10n_match_infix(r);
        let (but_len, does_len) = match localized.as_ref().map(|(name, len)| (name.as_str(), *len))
        {
            Some(("but", len)) => (len, 0),
            Some(("does", len)) => (0, len),
            _ => (0, 0),
        };
        if (but_len > 0)
            || (but_len == 0
                && r.starts_with("but")
                && !is_ident_char(r.as_bytes().get(3).copied()))
        {
            let len = if but_len > 0 { but_len } else { 3 };
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after 'but'", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("but".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        if (does_len > 0)
            || (does_len == 0
                && r.starts_with("does")
                && !is_ident_char(r.as_bytes().get(4).copied()))
        {
            let len = if does_len > 0 { does_len } else { 4 };
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after 'does'", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("does".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // S-metaop variants used as infix operators (e.g. S&)
        if r.starts_with("S&") && !is_ident_char(r.as_bytes().get(2).copied()) {
            let r = &r[2..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after 'S&'", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::Ident("S&".to_string()),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Negated set membership / relational operators: !(elem), !(cont),
        // !(<=), !(>=), !(>), !(==), and Unicode glyphs (in/cont/subset/superset).
        // Lower to `!(left <positive set op> right)` — a negated Bool.
        if let Some((tok, len)) = parse_negated_set_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after negated set operator",
                    r.len(),
                )
            })?;
            left = Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Expr::Binary {
                    left: Box::new(left),
                    op: tok,
                    right: Box::new(right),
                }),
            };
            rest = r;
            continue;
        }
        // Set operators: (|), (&), (-), (^), (<=), (>=), (<), (>), (elem), (cont)
        if let Some((tok, len)) = parse_set_op(r) {
            // `∪=`, `(&)=`, `(-)=`, … are set-operator *compound assignments*
            // (`lhs OP= rhs`), not an infix set op followed by a stray `=`.
            // Stop here so the assignment handler parses the whole statement;
            // otherwise the infix branch consumes `∪` and chokes on `=`.
            let after_op = &r[len..];
            if matches!(
                tok,
                TokenKind::SetUnion
                    | TokenKind::SetIntersect
                    | TokenKind::SetMultiply
                    | TokenKind::SetDiff
                    | TokenKind::SetSymDiff
                    | TokenKind::SetAddition
            ) && after_op.starts_with('=')
                && !after_op.starts_with("==")
                && !after_op.starts_with("=>")
            {
                break;
            }
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = concat_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after set operator", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: tok,
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}
