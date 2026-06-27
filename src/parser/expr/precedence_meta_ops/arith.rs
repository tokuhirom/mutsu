use crate::ast::Expr;
use crate::parser::expr::operators::{
    enrich_expected_error, parse_additive_op, parse_multiplicative_op,
};
use crate::parser::expr::postfix::{postfix_expr_continue, postfix_expr_tight_pub, prefix_expr};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult};
use crate::parser::stmt::assign::parse_meta_compound_assign_op;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::meta_bracket::{
    BracketInfix, block_newline_terminates, op_str_to_token_kind, parse_bracket_infix_op,
    parse_meta_op,
};

/// Try to parse a custom infix word operator at a given precedence range.
/// Returns Some(remaining_input) if a custom infix was parsed, None otherwise.
/// `min_level` is exclusive, `max_level` is inclusive.
pub(crate) fn try_custom_infix_at_level<'a>(
    r: &'a str,
    left: &mut Expr,
    min_level: i32,
    max_level: i32,
    operand_parser: fn(&str) -> PResult<'_, Expr>,
) -> Result<Option<&'a str>, PError> {
    if let Some((name, len)) = crate::parser::expr::precedence::parse_custom_infix_word(r)
        && let Some(level) = crate::parser::stmt::simple::lookup_custom_infix_precedence(&name)
        && level > min_level
        && level <= max_level
    {
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = operand_parser(r).map_err(|err| {
            enrich_expected_error(err, "expected expression after infix operator", r.len())
        })?;
        *left = Expr::InfixFunc {
            name: name.clone(),
            left: Box::new(left.clone()),
            right: vec![right],
            modifier: None,
        };
        return Ok(Some(r));
    }
    Ok(None)
}

/// Classify an operator string by its precedence level.
pub(crate) enum OpPrecedence {
    Multiplicative,
    Additive,
    Concatenation,
    Comparison,
    Other,
}

pub(crate) fn classify_base_op(op: &str) -> OpPrecedence {
    // Strip all meta prefixes to find the base operator
    let mut s = op;
    while let Some(rest) = s
        .strip_prefix('R')
        .or_else(|| s.strip_prefix('Z'))
        .or_else(|| s.strip_prefix('X'))
    {
        s = rest;
    }
    match s {
        "*" | "/" | "%" | "gcd" | "lcm" | "~&" => OpPrecedence::Multiplicative,
        "+" | "-" | "~|" | "~^" => OpPrecedence::Additive,
        "~" => OpPrecedence::Concatenation,
        "==" | "!=" | "=:=" | "!=:=" | "<" | ">" | "<=" | ">=" | "<=>" | "===" | "eq" | "ne"
        | "lt" | "gt" | "le" | "ge" | "leg" | "cmp" | "coll" | "unicmp" | "~~" | "%%" => {
            OpPrecedence::Comparison
        }
        _ => OpPrecedence::Other,
    }
}

/// Try to parse a bracket meta-op or bracket infix at a specific precedence level.
/// Returns Some((meta, op, total_len)) for meta-ops,
/// or Some(("", op, total_len)) for plain bracket infix.
fn try_bracket_op_at_level(input: &str, level: &OpPrecedence) -> Option<(String, String, usize)> {
    // Try R[op], Z[op], X[op]
    // Skip Z/X meta-ops — they need list-infix comma handling
    if let Some((meta, op, len)) = parse_meta_op(input)
        && meta != "Z"
        && meta != "X"
    {
        let full_op = format!("{}{}", meta, op);
        if std::mem::discriminant(&classify_base_op(&full_op)) == std::mem::discriminant(level) {
            return Some((meta, op, len));
        }
    }
    // Try [op] bracket infix
    if let Some(bracket_infix) = parse_bracket_infix_op(input) {
        match &bracket_infix {
            BracketInfix::PlainOp(op, len) => {
                if std::mem::discriminant(&classify_base_op(op)) == std::mem::discriminant(level) {
                    return Some((String::new(), op.clone(), *len));
                }
            }
            BracketInfix::MetaOp(meta, op, len) if meta != "Z" && meta != "X" => {
                let full_op = format!("{}{}", meta, op);
                if std::mem::discriminant(&classify_base_op(&full_op))
                    == std::mem::discriminant(level)
                {
                    return Some((meta.clone(), op.clone(), *len));
                }
            }
            _ => {}
        }
    }
    None
}

/// Addition/subtraction: + -
pub(crate) fn additive_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = multiplicative_expr(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if block_newline_terminates(input, rest, r) {
            break;
        }
        if let Some((op, len)) = parse_additive_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = multiplicative_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected expression after additive operator", r.len())
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Custom infix ops at additive level exactly (is equiv<+>)
        {
            use crate::parser::stmt::simple::PREC_ADDITIVE;
            if let Some(new_rest) = try_custom_infix_at_level(
                r,
                &mut left,
                PREC_ADDITIVE - 1,
                PREC_ADDITIVE,
                multiplicative_expr,
            )? {
                rest = new_rest;
                continue;
            }
        }
        if let Some((_, meta, op_name)) = parse_meta_compound_assign_op(r)
            && meta == "R"
            && matches!(classify_base_op(&op_name), OpPrecedence::Additive)
        {
            break;
        }
        // Try bracket meta-op at additive level: R[+], R[-], [R+], [R-], etc.
        if let Some((meta, op, len)) = try_bracket_op_at_level(r, &OpPrecedence::Additive) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = multiplicative_expr(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after bracket additive operator",
                    r.len(),
                )
            })?;
            if meta.is_empty() {
                // Plain bracket infix: [+] or [-]
                if let Some(tk) = op_str_to_token_kind(&op) {
                    left = Expr::Binary {
                        left: Box::new(left),
                        op: tk,
                        right: Box::new(right),
                    };
                }
            } else {
                left = Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            }
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Call `prefix_expr` and then apply whitespace-separated dotty method calls.
/// This places ws-dot at a precedence level between prefix/power and multiplicative,
/// so that `-2**2 . abs` parses as `(-(2**2)).abs` (ws-dot looser than prefix and **)
/// while `-1 * -1 . abs` parses as `-1 * ((-1).abs)` (ws-dot tighter than *).
fn prefix_expr_with_ws_dot(input: &str) -> PResult<'_, Expr> {
    let (rest, expr) = prefix_expr(input)?;
    postfix_expr_continue(rest, expr)
}

/// Multiplication/division: * / % div mod gcd lcm
pub(crate) fn multiplicative_expr(input: &str) -> PResult<'_, Expr> {
    let (mut rest, mut left) = prefix_expr_with_ws_dot(input)?;
    loop {
        let (r, _) = ws(rest)?;
        if block_newline_terminates(input, rest, r) {
            break;
        }
        if let Some((op, len)) = parse_multiplicative_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = prefix_expr_with_ws_dot(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after multiplicative operator",
                    r.len(),
                )
            })?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Custom infix ops at multiplicative level (between additive and multiplicative inclusive)
        // (covers is equiv<*>, is tighter<+>, is looser<*>)
        {
            use crate::parser::stmt::simple::{PREC_ADDITIVE, PREC_MULTIPLICATIVE};
            if let Some(new_rest) = try_custom_infix_at_level(
                r,
                &mut left,
                PREC_ADDITIVE,
                PREC_MULTIPLICATIVE,
                prefix_expr_with_ws_dot,
            )? {
                rest = new_rest;
                continue;
            }
        }
        if let Some((_, meta, op_name)) = parse_meta_compound_assign_op(r)
            && meta == "R"
            && matches!(classify_base_op(&op_name), OpPrecedence::Multiplicative)
        {
            break;
        }
        // Try bracket meta-op at multiplicative level: R[*], R[/], [R*], [R/], etc.
        if let Some((meta, op, len)) = try_bracket_op_at_level(r, &OpPrecedence::Multiplicative) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = prefix_expr_with_ws_dot(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after bracket multiplicative operator",
                    r.len(),
                )
            })?;
            if meta.is_empty() {
                if let Some(tk) = op_str_to_token_kind(&op) {
                    left = Expr::Binary {
                        left: Box::new(left),
                        op: tk,
                        right: Box::new(right),
                    };
                }
            } else {
                left = Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                };
            }
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Autoincrement prefix: ++expr, --expr
/// Binds tighter than ** (exponentiation) but looser than postfix.
/// e.g. `++$i ** 2` parses as `(++$i) ** 2`.
fn autoincrement_expr(
    input: &str,
    base_parser: fn(&str) -> PResult<'_, Expr>,
) -> PResult<'_, Expr> {
    use crate::parser::expr::operators::PrefixUnaryOp;
    use crate::parser::expr::operators::parse_prefix_unary_op;
    if let Some((op @ (PrefixUnaryOp::PreInc | PrefixUnaryOp::PreDec), len)) =
        parse_prefix_unary_op(input)
    {
        let rest = &input[len..];
        // Check for hyper prefix: ++<< / --<< / ++« / --«
        {
            let hm_len = if rest.starts_with('\u{00AB}') {
                Some('\u{00AB}'.len_utf8())
            } else if rest.starts_with("<<") {
                Some(2)
            } else {
                None
            };
            if let Some(marker_len) = hm_len {
                let symbol = match op {
                    PrefixUnaryOp::PreInc => "++",
                    PrefixUnaryOp::PreDec => "--",
                    _ => unreachable!(),
                };
                let r = &rest[marker_len..];
                let (r, _) = ws(r)?;
                let (r, arg) = prefix_expr(r)?;
                return Ok((
                    r,
                    Expr::Call {
                        name: Symbol::intern("__mutsu_hyper_prefix"),
                        args: vec![Expr::Literal(Value::str(symbol.to_string())), arg],
                    },
                ));
            }
        }
        // Recurse to allow nested ++/-- (e.g. ++++$x)
        let (rest, expr) = autoincrement_expr(rest, base_parser)?;
        if matches!(
            expr,
            Expr::PostfixOp {
                op: TokenKind::PlusPlus | TokenKind::MinusMinus,
                ..
            }
        ) {
            let message = match op {
                PrefixUnaryOp::PreInc => "prefix:<++>",
                PrefixUnaryOp::PreDec => "prefix:<-->",
                _ => unreachable!(),
            };
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Non-associative operator '{}' cannot be chained",
                    message
                )),
            );
            let exception =
                Value::make_instance(Symbol::intern("X::Syntax::NonAssociative"), attrs);
            return Err(PError::fatal_with_exception(
                format!("Non-associative operator '{}' cannot be chained", message),
                Box::new(exception),
            ));
        }
        return Ok((
            rest,
            Expr::Unary {
                op: op.token_kind(),
                expr: Box::new(expr),
            },
        ));
    }
    base_parser(input)
}

/// Exponentiation: **
/// Uses tight postfix (no whitespace-separated dotty) so that ws-dot
/// method calls bind looser than `**` and prefix operators.
/// e.g. `-2**2 . abs` parses as `(-(2**2)).abs` = `4`.
pub(crate) fn power_expr(input: &str) -> PResult<'_, Expr> {
    power_expr_inner(input, postfix_expr_tight_pub)
}

/// Like `power_expr` but uses tight postfix parsing (no whitespace-separated
/// dotty method calls).  Used as the operand parser for prefix `^` so that
/// `^2**64` correctly parses as `^(2**64)` while `^10 .batch(3)` still parses
/// as `(^10).batch(3)`.
pub(crate) fn power_expr_tight(input: &str) -> PResult<'_, Expr> {
    power_expr_inner(input, postfix_expr_tight_pub)
}

fn power_expr_inner(input: &str, base_parser: fn(&str) -> PResult<'_, Expr>) -> PResult<'_, Expr> {
    let (mut rest, mut base) = autoincrement_expr(input, base_parser)?;
    // Check for custom infixes at power level (tighter than multiplicative)
    loop {
        let (r, _) = ws(rest)?;
        if block_newline_terminates(input, rest, r) {
            break;
        }
        // Custom infix ops at power level (between multiplicative and prefix exclusive)
        // (covers is equiv<**>, is tighter<*>, is tighter<**>)
        {
            use crate::parser::stmt::simple::{PREC_MULTIPLICATIVE, PREC_PREFIX};
            if let Some(new_rest) = try_custom_infix_at_level(
                r,
                &mut base,
                PREC_MULTIPLICATIVE,
                PREC_PREFIX - 1,
                postfix_expr_tight_pub,
            )? {
                rest = new_rest;
                continue;
            }
        }
        if let Some(stripped) = r.strip_prefix("**") {
            let (r, _) = ws(stripped)?;
            let (r, exp) = prefix_expr(r).map_err(|err| {
                enrich_expected_error(err, "expected exponent expression after '**'", r.len())
            })?; // right-associative, allow prefix on RHS
            base = match base {
                Expr::Binary {
                    left,
                    op: TokenKind::StarStar,
                    right,
                } => Expr::Binary {
                    left,
                    op: TokenKind::StarStar,
                    right: Box::new(Expr::Binary {
                        left: right,
                        op: TokenKind::StarStar,
                        right: Box::new(exp),
                    }),
                },
                other => Expr::Binary {
                    left: Box::new(other),
                    op: TokenKind::StarStar,
                    right: Box::new(exp),
                },
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, base))
}
