use crate::ast::Expr;
use crate::parser::expr::operators::enrich_expected_error;
use crate::parser::helpers::{is_ident_char, ws};
use crate::parser::parse_result::PResult;
use crate::token_kind::TokenKind;

use super::hyper_concat::concat_expr;
use super::meta_bracket::block_newline_terminates;

fn parse_set_op(input: &str) -> Option<(TokenKind, usize)> {
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
    } else if input.starts_with("(>=)") {
        Some((TokenKind::SetSuperset, 4))
    } else if input.starts_with('⊇') {
        Some((TokenKind::SetSuperset, '⊇'.len_utf8()))
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
/// Handles the ASCII forms `!(elem)`, `!(cont)`, `!(<=)`, `!(>=)`, `!(>)`,
/// `!(==)` and the Unicode glyphs `\u{2209}` (\u{2209}), `\u{220C}` (\u{220C}),
/// `\u{2288}` (\u{2288}), `\u{2289}` (\u{2289}). `!(<)` and `\u{2284}`/`\u{2285}`/`\u{2262}`
/// already have dedicated handling in `parse_set_op`, so they are intentionally
/// left out here.
fn parse_negated_set_op(input: &str) -> Option<(TokenKind, usize)> {
    if let Some(rest) = input.strip_prefix('!') {
        // Check the 4-char forms before the 3-char ones so that e.g. `(<=)`
        // is not mis-parsed as `(<)` followed by stray `=`.
        let positive = if rest.starts_with("(elem)") {
            Some((TokenKind::SetElem, 6))
        } else if rest.starts_with("(cont)") {
            Some((TokenKind::SetCont, 6))
        } else if rest.starts_with("(<=)") {
            Some((TokenKind::SetSubset, 4))
        } else if rest.starts_with("(>=)") {
            Some((TokenKind::SetSuperset, 4))
        } else if rest.starts_with("(>)") {
            Some((TokenKind::SetStrictSuperset, 3))
        } else if rest.starts_with("(==)") {
            Some((TokenKind::Ident("(==)".to_string()), 4))
        } else {
            None
        };
        return positive.map(|(tok, len)| (tok, 1 + len));
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

/// Structural infix: but, does, set operators
pub(crate) fn structural_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = concat_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if block_newline_terminates(input, rest, r) {
            break;
        }
        if r.starts_with("but") && !is_ident_char(r.as_bytes().get(3).copied()) {
            let r = &r[3..];
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
        if r.starts_with("does") && !is_ident_char(r.as_bytes().get(4).copied()) {
            let r = &r[4..];
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
