use super::list_infix::{attach_trailing_adverbs, wrap_left_exclusive_sequence};
use super::ternary::is_assignment_expr;
use super::*;

/// The "item" level: Z/X's operand precedence.
///
/// operators.rakudoc places the *List infix* level (`Z`, `X`, their meta-ops,
/// the sequence operator `...`, `minmax`) LOOSER than the comma operator, and
/// hence looser than every tighter operator (comparison, `&&`, `||`, `min`,
/// item assignment `=`, the loose unary `so`/`not`, and the junctions). So both
/// operands of a list-infix operator absorb the whole tighter expression:
/// `1 == 1 Z 2 == 2` is `(1 == 1) Z (2 == 2)`.
///
/// `item_expr` parses exactly that operand: everything tighter than the comma
/// operator, but NOT the loose word-logicals (`and`/`or`/`xor`) and NOT the
/// list-infix operators themselves. It is `assign_not_expr_mode` (which owns the
/// item-assignment `=`/`:=`, the loose unary `not`/`so`, and the whole
/// comparison/junction/range descent) plus a trailing conditional (`?? !!`),
/// which is TIGHTER than item assignment and therefore appears here rather than
/// at the top-level word-logical layer.
pub(crate) fn item_expr(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, left) = assign_not_expr_mode(input, mode)?;
    // If the item layer already consumed an assignment (`$x = ...`), the ternary
    // condition would be that assignment, which is looser than `?? !!` — so a
    // following `??` binds to the assignment's RHS, which was already parsed
    // recursively through `item_expr`. Nothing more to do here.
    if is_assignment_expr(&left) {
        return Ok((rest, left));
    }
    let (rest_ws, _) = ws(rest)?;
    let Ok((after_q, _)) = parse_tag(rest_ws, "??") else {
        return Ok((rest, left));
    };
    let cond = left;
    let (after_q, _) = ws(after_q)?;
    let (after_then, then_expr) = item_expr(after_q, mode).map_err(|err| {
        enrich_expected_error(err, "expected then-expression after '??'", after_q.len())
    })?;
    // A bare identifier in then-position is usually the head of a listop call
    // whose comma args (and possibly the `!!`) were not consumed yet — mirror the
    // top-level `ternary_mode` guard so the caller can retry/report.
    if mode == ExprMode::Full
        && let Expr::BareWord(name) = &then_expr
        && name != "self"
        && !name.contains("::")
        && !crate::runtime::utils::is_known_type_constraint(name)
        && !crate::runtime::utils::is_builtin_enum_value(name)
        && !crate::runtime::utils::is_builtin_constant_term(name)
        && !crate::parser::stmt::simple::is_user_declared_type(name)
        && !crate::parser::stmt::simple::is_user_declared_value_term(name)
    {
        return Err(PError::expected("expected '!!' in ternary expression"));
    }
    let (after_then, _) = ws(after_then)?;
    let (after_bang, _) = parse_tag(after_then, "!!").map_err(|err| {
        enrich_expected_error(err, "expected '!!' in ternary expression", after_then.len())
    })?;
    let (after_bang, _) = ws(after_bang)?;
    let (rest, else_expr) = item_expr(after_bang, mode).map_err(|err| {
        enrich_expected_error(err, "expected else-expression after '!!'", after_bang.len())
    })?;
    if is_assignment_expr(&then_expr) || is_assignment_expr(&else_expr) {
        return Err(conditional_precedence_too_loose_error());
    }
    Ok((
        rest,
        Expr::Ternary {
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        },
    ))
}

/// The top-level list-infix layer: `Z`, `X`, their meta-ops, infixed functions,
/// the sequence operator (`...`/`...^`), and the feed operators. Sits just below
/// the loose word-logicals (`and`/`or`) and just above the item level
/// ([`item_expr`]). Each operand is parsed at the item level, so list-infix is
/// correctly looser than comparison / `&&` / `||` / `min` / item assignment.
///
/// In the call-argument modes (`ListopArg`, `NoSequenceNoFeed`) the list-infix
/// operators are handled by the dedicated call-argument machinery plus the
/// post-parse comma lift, so this layer is a straight pass-through to the item
/// level there (preserving the previous behaviour).
pub(crate) fn list_infix_top(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    if matches!(mode, ExprMode::ListopArg | ExprMode::NoSequenceNoFeed) {
        return assign_not_expr_mode(input, mode);
    }

    let (mut rest, mut left) = item_expr(input, mode)?;
    let operand = ListInfixOperand::Item(mode);
    let mut current_assoc_key: Option<String> = None;

    fn maybe_wrap_lhs(left: &mut Expr) {
        if contains_whatever(left) && !matches!(left, Expr::Whatever) {
            *left = wrap_whatevercode(left);
        }
    }

    loop {
        let (r, _) = ws(rest)?;

        // Sequence operators (`...`, `...^`, `…`, `…^`) share the list-infix
        // precedence level. Only handled in `Full` mode (NoSequence excludes it).
        if mode == ExprMode::Full
            && let Some((r2, op, op_str)) = strip_sequence_op(r)
        {
            if let Some(prev) = current_assoc_key.as_deref()
                && prev != op_str
            {
                return Err(non_list_associative_error(prev, op_str));
            }
            let (r2, _) = ws(r2)?;
            let (r2, mut right) = item_expr(r2, mode).map_err(|err| {
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
            left = wrap_left_exclusive_sequence(
                op_str,
                Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
            );
            current_assoc_key = Some(op_str.to_string());
            rest = r2;
            continue;
        }

        // Z/X/meta/infix-func operators (and feed operators) via the shared loop.
        let new_rest = parse_list_infix_loop_with_operand(
            rest,
            input,
            &mut left,
            &mut current_assoc_key,
            true,
            operand,
        )?;
        if new_rest.len() < rest.len() {
            rest = new_rest;
            continue;
        }
        break;
    }
    rest = attach_trailing_adverbs(rest, &mut left)?;
    Ok((rest, left))
}
