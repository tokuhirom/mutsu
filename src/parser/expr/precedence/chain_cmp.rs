use super::*;

pub(crate) fn make_chain_cmp(left: Expr, op: TokenKind, right: Expr, negated: bool) -> Expr {
    let cmp = Expr::Binary {
        left: Box::new(left),
        op,
        right: Box::new(right),
    };
    if negated {
        Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cmp),
        }
    } else {
        cmp
    }
}

pub(crate) fn wrap_smartmatch_rhs(right: Expr) -> Expr {
    match right {
        // Keep Pair shape for `%hash ~~ key => !*.foo` by wrapping only value side.
        Expr::Binary {
            left,
            op: TokenKind::FatArrow,
            right,
        } => {
            let value = if contains_whatever(&right)
                && !matches!(&*right, Expr::Whatever | Expr::HyperWhatever)
            {
                Expr::WhateverCurry(right)
            } else {
                *right
            };
            Expr::Binary {
                left,
                op: TokenKind::FatArrow,
                right: Box::new(value),
            }
        }
        other => {
            if contains_whatever(&other) && !matches!(&other, Expr::Whatever | Expr::HyperWhatever)
            {
                Expr::WhateverCurry(Box::new(other))
            } else {
                other
            }
        }
    }
}

/// Extract a comparison operator from the start of the input, returning the op and its length.
pub(in crate::parser::expr) fn parse_comparison_op(r: &str) -> Option<(ComparisonOp, usize)> {
    // A block's `}` at end of the previous line terminated the statement, so
    // whatever sits here begins a new one -- `before`/`after`/`eq`/... at this
    // position is a term, not an infix (see `parser::stmt_ending_brace`).
    if crate::parser::stmt_ending_brace::infix_barred_by_stmt_ending_brace(r) {
        return None;
    }
    // Unicode comparison operators
    if r.starts_with('\u{2A75}') {
        // ⩵ (U+2A75) — numeric equality (alias for ==)
        return Some((ComparisonOp::NumEq, '\u{2A75}'.len_utf8()));
    } else if r.starts_with('\u{2A76}') {
        // ⩶ (U+2A76) — value identity (alias for ===)
        return Some((ComparisonOp::StrictEq, '\u{2A76}'.len_utf8()));
    } else if r.starts_with('\u{2260}') {
        // ≠ (U+2260) — numeric inequality (alias for !=)
        return Some((ComparisonOp::NumNe, '\u{2260}'.len_utf8()));
    } else if r.starts_with('\u{2264}') {
        // ≤ (U+2264) — numeric less-than-or-equal (alias for <=)
        return Some((ComparisonOp::NumLe, '\u{2264}'.len_utf8()));
    } else if r.starts_with('\u{2265}') {
        // ≥ (U+2265) — numeric greater-than-or-equal (alias for >=)
        return Some((ComparisonOp::NumGe, '\u{2265}'.len_utf8()));
    }
    // ≅ (U+2245) — approximately equal
    if r.starts_with('\u{2245}') {
        return Some((ComparisonOp::ApproxEq, '\u{2245}'.len_utf8()));
    }
    if r.starts_with("=~=") {
        return Some((ComparisonOp::ApproxEq, 3));
    }
    if r.starts_with("!=:=") {
        return Some((ComparisonOp::ContainerNe, 4));
    }
    if r.starts_with("=:=") {
        return Some((ComparisonOp::ContainerEq, 3));
    }
    if r.starts_with("===") {
        Some((ComparisonOp::StrictEq, 3))
    } else if r.starts_with("==") && !r.starts_with("===") && !r.starts_with("==>") {
        Some((ComparisonOp::NumEq, 2))
    } else if r.starts_with("!%%") {
        Some((ComparisonOp::NotDivisibleBy, 3))
    } else if r.starts_with("!===") {
        Some((ComparisonOp::StrictNe, 4))
    } else if r.starts_with("!=") {
        Some((ComparisonOp::NumNe, 2))
    } else if r.starts_with("!~~") {
        Some((ComparisonOp::SmartNotMatch, 3))
    } else if r.starts_with("~~") {
        Some((ComparisonOp::SmartMatch, 2))
    } else if r.starts_with("<=>") {
        Some((ComparisonOp::Spaceship, 3))
    } else if r.starts_with("<=") && !r.starts_with("<=>") && !r.starts_with("<==") {
        Some((ComparisonOp::NumLe, 2))
    } else if r.starts_with(">=") {
        Some((ComparisonOp::NumGe, 2))
    } else if r.starts_with('<') && !r.starts_with("<<") && !r.starts_with("<=") {
        Some((ComparisonOp::NumLt, 1))
    } else if r.starts_with('>') && !r.starts_with(">>") && !r.starts_with(">=") {
        Some((ComparisonOp::NumGt, 1))
    } else if r.starts_with("eq") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrEq, 2))
    } else if r.starts_with("ne") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrNe, 2))
    } else if r.starts_with("lt") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrLt, 2))
    } else if r.starts_with("gt") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrGt, 2))
    } else if r.starts_with("le") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrLe, 2))
    } else if r.starts_with("ge") && !is_ident_char(r.as_bytes().get(2).copied()) {
        Some((ComparisonOp::StrGe, 2))
    } else if r.starts_with("leg") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Leg, 3))
    } else if r.starts_with("cmp") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Cmp, 3))
    } else if r.starts_with("coll") && !is_ident_char(r.as_bytes().get(4).copied()) {
        Some((ComparisonOp::Coll, 4))
    } else if r.starts_with("unicmp") && !is_ident_char(r.as_bytes().get(6).copied()) {
        Some((ComparisonOp::Unicmp, 6))
    } else if r.starts_with("eqv") && !is_ident_char(r.as_bytes().get(3).copied()) {
        Some((ComparisonOp::Eqv, 3))
    } else if r.starts_with("before") && !is_ident_char(r.as_bytes().get(6).copied()) {
        Some((ComparisonOp::Before, 6))
    } else if r.starts_with("after") && !is_ident_char(r.as_bytes().get(5).copied()) {
        Some((ComparisonOp::After, 5))
    } else {
        None
    }
}

pub(in crate::parser::expr) fn parse_negated_meta_comparison_op(
    r: &str,
) -> Option<(ComparisonOp, usize)> {
    let inner = r.strip_prefix('!')?;
    let (op, len) = parse_comparison_op(inner)?;
    // Operators that already have their own !-prefixed spelling are not meta-negated forms.
    if matches!(
        op,
        ComparisonOp::NumNe
            | ComparisonOp::NotDivisibleBy
            | ComparisonOp::SmartMatch
            | ComparisonOp::SmartNotMatch
    ) {
        return None;
    }
    Some((op, len + 1))
}

/// The range operator spelling at the start of `r`, if any. Longest match
/// first, and `...` (the sequence operator) is deliberately not one of these.
fn peek_range_op(r: &str) -> Option<&'static str> {
    for op in ["^..^", "^..", "..^"] {
        if r.starts_with(op) && !(op == "^.." && r.starts_with("^...")) {
            return Some(op);
        }
    }
    if r.starts_with("..") && !r.starts_with("...") {
        return Some("..");
    }
    None
}

/// The range operators are non-associative, so `1..2..3` is a compile error in
/// rakudo (`X::Syntax::NonAssociative`, carrying both spellings) rather than
/// something to parse. `range_expr` builds one range and returns, so the
/// trailing `..3` used to be left unconsumed and the statement failed with the
/// parser's generic "Confused" — a diagnosis that named neither operator.
fn reject_chained_range(after_rhs: &str, left_op: &'static str) -> Result<(), PError> {
    let (r, _) = ws(after_rhs)?;
    match peek_range_op(r) {
        Some(right_op) => Err(non_associative_pair_error(left_op, right_op)),
        None => Ok(()),
    }
}

/// Range operators are structural (non-associative) per rakudo's own
/// precedence table, so `OP=` over one is the same rejected assignment
/// metaop as `cmp=`/`<=>=`: `Cannot make assignment out of .. because
/// structural infix operators are too diffy` (verified against
/// `raku -e '6 ..= 2'` and the `^..`/`..^`/`^..^` siblings). Unlike the
/// comparison operators in `comparison.rs`, ranges are matched by literal
/// spelling here rather than through `ComparisonOp`, so the check is a
/// direct string peek instead of going through `reject_diffy_assign_meta`.
fn reject_range_diffy_assign(after_op: &str, op_str: &'static str) -> Result<(), PError> {
    if after_op.starts_with('=') && !after_op.starts_with("==") && !after_op.starts_with("=>") {
        return Err(cannot_meta_assign_diffy_error(
            op_str,
            "structural infix",
            after_op.len(),
        ));
    }
    Ok(())
}

/// Range: ..  ..^  ^..  ^..^
pub(crate) fn range_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, left) = structural_expr(input)?;
    let (r, _) = ws(rest)?;

    if let Some(stripped) = r.strip_prefix("^..^") {
        check_range_precedence_worry(input)?;
        reject_range_diffy_assign(stripped, "^..^")?;
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r).map_err(|err| {
            enrich_expected_error(err, "expected range RHS after '^..^'", r.len())
        })?;
        reject_chained_range(r, "^..^")?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDotCaret,
                right: Box::new(right),
            },
        ));
    }
    if r.starts_with("^..") && !r.starts_with("^...") {
        check_range_precedence_worry(input)?;
        let stripped = &r[3..];
        reject_range_diffy_assign(stripped, "^..")?;
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '^..'", r.len()))?;
        reject_chained_range(r, "^..")?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDot,
                right: Box::new(right),
            },
        ));
    }
    if let Some(stripped) = r.strip_prefix("..^") {
        check_range_precedence_worry(input)?;
        reject_range_diffy_assign(stripped, "..^")?;
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '..^'", r.len()))?;
        reject_chained_range(r, "..^")?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDotCaret,
                right: Box::new(right),
            },
        ));
    }
    if r.starts_with("..") && !r.starts_with("...") {
        check_range_precedence_worry(input)?;
        let r = &r[2..];
        reject_range_diffy_assign(r, "..")?;
        let (r, _) = ws(r)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '..'", r.len()))?;
        reject_chained_range(r, "..")?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::DotDot,
                right: Box::new(right),
            },
        ));
    }
    Ok((rest, left))
}
