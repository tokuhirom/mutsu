use super::super::parse_result::{PError, PResult, parse_char, take_while_opt, take_while1};

use crate::ast::{Expr, Stmt};
use crate::value::Value;

use super::super::expr::{expression, expression_no_sequence};
use super::super::helpers::ws;
use super::super::stmt::keyword;

/// Parse a parenthesized expression or list.
pub(super) fn paren_expr(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '(')?;
    let (input, _) = ws(input)?;
    if let Ok((input, _)) = parse_char(input, ')') {
        // Empty parens = empty list
        return Ok((input, Expr::ArrayLiteral(Vec::new())));
    }
    // Try class declaration in parens: (class A { })
    if (input.starts_with("class ") || input.starts_with("class\t") || input.starts_with("class\n"))
        && let Ok((r, class_stmt)) = super::super::stmt::class::class_decl(input)
    {
        let (r, _) = ws(r)?;
        if let Ok((r, _)) = parse_char(r, ')') {
            return Ok((r, Expr::DoStmt(Box::new(class_stmt))));
        }
    }
    // Try assignment expression: ($var = expr) or ($var op= expr)
    let (input, first) = if let Ok((r, var_expr)) = expression_no_sequence(input) {
        let (r2, _) = ws(r)?;
        if matches!(&var_expr, Expr::Var(_)) {
            if r2.starts_with('=') && !r2.starts_with("==") && !r2.starts_with("=>") {
                // Simple assignment: ($var = expr)
                let r2 = &r2[1..];
                let (r2, _) = ws(r2)?;
                let (r2, rhs) = expression(r2)?;
                if let Expr::Var(name) = &var_expr {
                    (
                        r2,
                        Expr::AssignExpr {
                            name: name.clone(),
                            expr: Box::new(rhs),
                        },
                    )
                } else {
                    unreachable!()
                }
            } else if let Some((stripped, op)) =
                super::super::stmt::assign::parse_compound_assign_op(r2)
            {
                // Compound assignment: ($var += expr)
                let (r2, _) = ws(stripped)?;
                let (r2, rhs) = expression(r2)?;
                if let Expr::Var(name) = &var_expr {
                    (
                        r2,
                        Expr::AssignExpr {
                            name: name.clone(),
                            expr: Box::new(Expr::Binary {
                                left: Box::new(Expr::Var(name.clone())),
                                op: op.token_kind(),
                                right: Box::new(rhs),
                            }),
                        },
                    )
                } else {
                    unreachable!()
                }
            } else {
                (r, var_expr)
            }
        } else {
            (r, var_expr)
        }
    } else {
        expression_no_sequence(input)?
    };
    let (input, _) = ws(input)?;
    // Check for inline statement modifier: ($_ with data), (expr if cond), etc.
    if let Some(result) = try_inline_modifier(input, first.clone()) {
        let (rest, modified_expr) = result?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, modified_expr));
    }
    // Check for sequence operator after single item: (1 ... 5)
    if let Some(seq) = try_parse_sequence_in_paren(input, std::slice::from_ref(&first)) {
        return seq;
    }
    if let Ok((input, _)) = parse_char(input, ')') {
        return Ok((input, first));
    }
    // Comma-separated list with sequence operator detection
    // Use expression_no_sequence so that `...` is not consumed as part of an item
    let (input, _) = parse_char(input, ',')?;
    let (input, _) = ws(input)?;
    let mut items = vec![first];
    // Handle trailing comma before close paren
    if let Ok((input, _)) = parse_char(input, ')') {
        return Ok((input, Expr::ArrayLiteral(items)));
    }
    // Check for sequence operator right after first comma
    if let Some(seq) = try_parse_sequence_in_paren(input, &items) {
        return seq;
    }
    let (mut input_rest, second) = expression_no_sequence(input)?;
    items.push(second);
    loop {
        let (input, _) = ws(input_rest)?;
        if let Ok((input, _)) = parse_char(input, ')') {
            return Ok((input, Expr::ArrayLiteral(items)));
        }
        // Check for sequence operator before comma
        if let Some(seq) = try_parse_sequence_in_paren(input, &items) {
            return seq;
        }
        let (input, _) = parse_char(input, ',')?;
        let (input, _) = ws(input)?;
        if let Ok((input, _)) = parse_char(input, ')') {
            return Ok((input, Expr::ArrayLiteral(items)));
        }
        // Check for sequence operator after comma
        if let Some(seq) = try_parse_sequence_in_paren(input, &items) {
            return seq;
        }
        let (input, next) = expression_no_sequence(input)?;
        items.push(next);
        input_rest = input;
    }
}

/// Try to parse a sequence operator (...) inside a paren expression.
/// If the input starts with ... or ...^, treat all collected items as seeds.
fn try_parse_sequence_in_paren<'a>(input: &'a str, seeds: &[Expr]) -> Option<PResult<'a, Expr>> {
    let (is_excl, rest) = if let Some(stripped) = input.strip_prefix("...^") {
        (true, stripped)
    } else if input.starts_with("...") && !input.starts_with("....") {
        (false, &input[3..])
    } else {
        return None;
    };
    // Parse the endpoint expression
    let result = (|| {
        let (rest, _) = ws(rest)?;
        // Special case: bare * means infinite sequence (Whatever/Inf)
        let (rest, endpoint) = if rest.starts_with('*')
            && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
        {
            (&rest[1..], Expr::Whatever)
        } else {
            super::super::expr::expression_no_sequence(rest)?
        };
        // There may be more comma items after the endpoint
        let (rest, _) = ws(rest)?;
        let mut extra_items = Vec::new();
        let mut r = rest;
        while r.starts_with(',') {
            let (r2, _) = parse_char(r, ',')?;
            let (r2, _) = ws(r2)?;
            if r2.starts_with(')') {
                r = r2;
                break;
            }
            let (r2, item) = super::super::expr::expression(r2)?;
            extra_items.push(item);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let (r, _) = parse_char(r, ')')?;

        let op = if is_excl {
            crate::token_kind::TokenKind::DotDotDotCaret
        } else {
            crate::token_kind::TokenKind::DotDotDot
        };
        let left = if seeds.len() == 1 {
            seeds[0].clone()
        } else {
            Expr::ArrayLiteral(seeds.to_vec())
        };
        // If there are extra items after the endpoint, pack endpoint + extras as Array right
        let right_expr = if extra_items.is_empty() {
            endpoint
        } else {
            let mut items = vec![endpoint];
            items.extend(extra_items);
            Expr::ArrayLiteral(items)
        };
        let seq = Expr::Binary {
            left: Box::new(left),
            op,
            right: Box::new(right_expr),
        };
        Ok((r, seq))
    })();
    Some(result)
}

/// Try to parse an inline statement modifier inside parenthesized expression.
/// Handles: ($_ with data), (expr if cond), (expr for list), etc.
fn try_inline_modifier<'a>(input: &'a str, expr: Expr) -> Option<PResult<'a, Expr>> {
    use super::super::stmt::modifier::parse_statement_modifier;
    // Check if the input starts with a modifier keyword
    let modifier_keywords = [
        "if", "unless", "with", "without", "for", "while", "until", "given",
    ];
    let is_modifier = modifier_keywords
        .iter()
        .any(|kw| keyword(kw, input).is_some());
    if !is_modifier {
        return None;
    }
    // Wrap expr as Stmt::Expr, apply one or more chained modifiers,
    // then wrap result as Expr::DoStmt.
    let stmt = Stmt::Expr(expr);
    let result = (|| {
        let (mut rest, mut modified_stmt) = parse_statement_modifier(input, stmt)?;
        loop {
            let (r, _) = ws(rest)?;
            if !modifier_keywords.iter().any(|kw| keyword(kw, r).is_some()) {
                rest = r;
                break;
            }
            let (r2, next_stmt) = parse_statement_modifier(r, modified_stmt)?;
            rest = r2;
            modified_stmt = next_stmt;
        }
        Ok((rest, Expr::DoStmt(Box::new(modified_stmt))))
    })();
    Some(result)
}

/// Parse an array literal [...].
pub(super) fn array_literal(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '[')?;
    let (input, _) = ws(input)?;
    let mut items = Vec::new();
    if let Ok((input, _)) = parse_char(input, ']') {
        return Ok((input, Expr::ArrayLiteral(items)));
    }
    let (mut rest, first) = expression(input)?;
    items.push(first);
    loop {
        let (r, _) = ws(rest)?;
        if let Ok((r, _)) = parse_char(r, ',') {
            let (r, _) = ws(r)?;
            if let Ok((r, _)) = parse_char(r, ']') {
                return Ok((r, Expr::ArrayLiteral(items)));
            }
            let (r, next) = expression(r)?;
            items.push(next);
            rest = r;
        } else {
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ']')?;
            return Ok((r, Expr::ArrayLiteral(items)));
        }
    }
}

/// Parse a < > quote-word list.
pub(super) fn angle_list(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '<')?;
    // Make sure it's not <= or <=> etc.
    if input.starts_with('=') || input.starts_with('-') {
        return Err(PError::expected("angle list"));
    }
    let mut words = Vec::new();
    let mut rest = input;
    loop {
        // Skip breaking whitespace (all Unicode whitespace except non-breaking spaces)
        let (r, _) = take_while_opt(rest, |c: char| {
            c.is_whitespace() && !is_non_breaking_space(c)
        });
        rest = r;
        if rest.starts_with('>') {
            rest = &rest[1..];
            break;
        }
        if rest.is_empty() {
            return Err(PError::expected("closing >"));
        }
        let (r, word) = take_while1(rest, |c: char| {
            c != '>' && (!c.is_whitespace() || is_non_breaking_space(c))
        })?;
        words.push(word.to_string());
        rest = r;
    }
    if words.len() == 1 {
        Ok((
            rest,
            Expr::Literal(Value::Str(words.into_iter().next().unwrap())),
        ))
    } else {
        let exprs: Vec<Expr> = words
            .into_iter()
            .map(|w| Expr::Literal(Value::Str(w)))
            .collect();
        Ok((rest, Expr::ArrayLiteral(exprs)))
    }
}

/// Returns true for non-breaking space characters that should not split words in `<...>`.
fn is_non_breaking_space(c: char) -> bool {
    matches!(c, '\u{00A0}' | '\u{2007}' | '\u{202F}' | '\u{FEFF}')
}
