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

pub(crate) fn build_chain_cmp_expr(
    operands: &[Expr],
    ops: &[(TokenKind, bool)],
    index: usize,
    left: Expr,
) -> Expr {
    let (op, negated) = ops[index].clone();
    if index == ops.len() - 1 {
        return make_chain_cmp(left, op, operands[index + 1].clone(), negated);
    }

    let tmp_idx = CHAIN_CMP_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let tmp_name = format!("__mutsu_chain_cmp_{tmp_idx}");
    let tmp_var = Expr::Var(tmp_name.clone());
    let cmp = make_chain_cmp(left, op, tmp_var.clone(), negated);
    let rest = build_chain_cmp_expr(operands, ops, index + 1, tmp_var.clone());
    Expr::DoBlock {
        body: vec![
            Stmt::VarDecl {
                name: tmp_name,
                expr: operands[index + 1].clone(),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            },
            Stmt::Expr(Expr::Binary {
                left: Box::new(cmp),
                op: TokenKind::AndAnd,
                right: Box::new(rest),
            }),
        ],
        label: None,
    }
}

pub(crate) fn build_chain_cmp_expr_with_repeated_middle(
    operands: &[Expr],
    ops: &[(TokenKind, bool)],
) -> Expr {
    let mut result = make_chain_cmp(
        operands[0].clone(),
        ops[0].0.clone(),
        operands[1].clone(),
        ops[0].1,
    );
    let mut prev_right = operands[1].clone();
    for (idx, (op, negated)) in ops.iter().enumerate().skip(1) {
        let next_right = operands[idx + 1].clone();
        let next_cmp = make_chain_cmp(prev_right, op.clone(), next_right.clone(), *negated);
        result = Expr::Binary {
            left: Box::new(result),
            op: TokenKind::AndAnd,
            right: Box::new(next_cmp),
        };
        prev_right = next_right;
    }
    result
}

pub(crate) fn wrap_smartmatch_rhs(right: Expr) -> Expr {
    match right {
        // Keep Pair shape for `%hash ~~ key => !*.foo` by wrapping only value side.
        Expr::Binary {
            left,
            op: TokenKind::FatArrow,
            right,
        } => {
            let value = if contains_whatever(&right) && !matches!(&*right, Expr::Whatever) {
                wrap_whatevercode(&right)
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
            if contains_whatever(&other) && !matches!(&other, Expr::Whatever) {
                wrap_whatevercode(&other)
            } else {
                other
            }
        }
    }
}

/// Extract a comparison operator from the start of the input, returning the op and its length.
pub(in crate::parser::expr) fn parse_comparison_op(r: &str) -> Option<(ComparisonOp, usize)> {
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

/// Range: ..  ..^  ^..  ^..^
pub(crate) fn range_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, left) = structural_expr(input)?;
    let (r, _) = ws(rest)?;

    if let Some(stripped) = r.strip_prefix("^..^") {
        check_range_precedence_worry(input)?;
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r).map_err(|err| {
            enrich_expected_error(err, "expected range RHS after '^..^'", r.len())
        })?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::CaretDotDotCaret,
                right: Box::new(right),
            },
        ));
    }
    if let Some(stripped) = r.strip_prefix("^..") {
        check_range_precedence_worry(input)?;
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '^..'", r.len()))?;
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
        let (r, _) = ws(stripped)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '..^'", r.len()))?;
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
        let (r, _) = ws(r)?;
        let (r, right) = structural_expr(r)
            .map_err(|err| enrich_expected_error(err, "expected range RHS after '..'", r.len()))?;
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
