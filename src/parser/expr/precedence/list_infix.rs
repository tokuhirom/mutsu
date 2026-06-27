use super::*;

/// List-infix operators only: Z, X, meta-ops, infix funcs.
/// Used in NoSequence mode (e.g. inside parenthesized expressions).
pub(crate) fn list_infix_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = range_expr(input)?;
    let mut current_assoc_key = None;
    rest = parse_list_infix_loop(rest, input, &mut left, &mut current_assoc_key)?;
    Ok((rest, left))
}

/// Sequence (..., ...^) and list-infix (Z, X, meta-ops, infix funcs).
/// All have Raku precedence level f= (list associative).
pub(crate) fn sequence_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = range_expr(input)?;
    let mut current_assoc_key: Option<String> = None;

    // Helper: wrap LHS WhateverCode before building the sequence node.
    fn maybe_wrap_lhs(left: &mut Expr) {
        if contains_whatever(left) && !matches!(left, Expr::Whatever) {
            *left = wrap_whatevercode(left);
        }
    }

    loop {
        let (r, _) = ws(rest)?;

        // ...^ / …^ (exclusive-end sequence), ... / … (sequence)
        if let Some((r2, op, op_str)) = strip_sequence_op(r) {
            if let Some(prev) = current_assoc_key.as_deref()
                && prev != op_str
            {
                return Err(non_list_associative_error(prev, op_str));
            }
            let (r2, _) = ws(r2)?;
            let (r2, mut right) =
                comparison_expr_mode(r2, ExprMode::NoSequence).map_err(|err| {
                    enrich_expected_error(
                        err,
                        format!("expected expression after '{op_str}'").as_str(),
                        r2.len(),
                    )
                })?;
            if contains_whatever(&right) && !matches!(right, Expr::Whatever) {
                right = wrap_whatevercode(&right);
            }
            maybe_wrap_lhs(&mut left);
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            current_assoc_key = Some(op_str.to_string());
            rest = r2;
            continue;
        }
        // Try Z/X/meta/infix-func operators
        let new_rest = parse_list_infix_loop(rest, input, &mut left, &mut current_assoc_key)?;
        if new_rest.len() < rest.len() {
            rest = new_rest;
            continue;
        }
        break;
    }
    Ok((rest, left))
}
