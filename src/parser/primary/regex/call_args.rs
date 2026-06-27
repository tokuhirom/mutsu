//! Call argument list parsing and related helpers.

use crate::ast::Expr;
use crate::parser::expr::expression;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PResult, parse_char};

/// Parse a single argument in colon method-call syntax (.method: arg1, arg2).
/// Tries colonpair first (:name, :$var, :!flag, :0port), then expression.
pub(super) fn parse_colon_method_arg(input: &str) -> PResult<'_, Expr> {
    if input.starts_with(':')
        && !input.starts_with("::")
        && let Ok(result) = crate::parser::primary::misc::colonpair_expr(input)
    {
        return Ok(result);
    }
    expression(input)
}

pub(super) fn has_unescaped_statement_boundary(input: &str) -> bool {
    let mut escaped = false;
    for ch in input.chars() {
        if escaped {
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            continue;
        }
        if ch == ';' || ch == '\n' {
            return true;
        }
    }
    false
}

/// Convert semicolon-separated groups into Array args.
fn semicolon_groups_to_args(groups: Vec<Vec<Expr>>, _empty: Vec<Expr>) -> Vec<Expr> {
    groups
        .into_iter()
        .map(|g| {
            if g.len() == 1 {
                // Single-element group: wrap in array for consistency
                Expr::ArrayLiteral(g)
            } else {
                Expr::ArrayLiteral(g)
            }
        })
        .collect()
}

/// Parse comma-separated call arguments inside parens.
/// Semicolons act as list-associative separators: each `;`-delimited group
/// is collected into an `Array` node, producing one arg per group.
pub(in crate::parser) fn parse_call_arg_list(input: &str) -> PResult<'_, Vec<Expr>> {
    fn parse_call_arg_expr(input: &str) -> PResult<'_, Expr> {
        let (rest, expr) =
            if let Ok(result) = crate::parser::primary::misc::reduction_call_style_expr(input) {
                result
            } else if let Ok((rest, assign_expr)) =
                crate::parser::stmt::assign::try_parse_assign_expr(input)
            {
                // Only take the assignment fast path when it reaches an argument
                // boundary. Otherwise a parenthesized assignment like `($x = 10)`
                // can be consumed too early inside a larger expression.
                let trimmed = rest.trim_start();
                if trimmed.is_empty()
                    || trimmed.starts_with(',')
                    || trimmed.starts_with(')')
                    || trimmed.starts_with(';')
                {
                    (rest, assign_expr)
                } else {
                    expression(input)?
                }
            } else {
                expression(input)?
            };
        // Handle compound assignment on non-variable expressions in argument position
        // (e.g., `* *= 2` creates WhateverCode that mutates via compound assign).
        let (rest_ws, _) = crate::parser::helpers::ws(rest)?;
        if let Some((stripped, op)) = crate::parser::stmt::assign::parse_compound_assign_op(rest_ws)
        {
            let (r, _) = crate::parser::helpers::ws(stripped)?;
            let (r, rhs) = expression(r)?;
            let mut compound_expr = Expr::Binary {
                left: Box::new(expr),
                op: op.token_kind(),
                right: Box::new(rhs),
            };
            // Apply WhateverCode wrapping (e.g., `* *= 2` -> WhateverCode lambda)
            if crate::parser::expr::should_wrap_whatevercode(&compound_expr) {
                compound_expr = crate::parser::expr::wrap_whatevercode(&compound_expr);
            }
            return Ok((r, compound_expr));
        }
        Ok((rest, expr))
    }

    if input.starts_with(')') {
        return Ok((input, Vec::new()));
    }
    let (input, first) = parse_call_arg_expr(input)?;
    let mut current_group = vec![first];
    let mut groups: Vec<Vec<Expr>> = Vec::new();
    let mut has_semicolon = false;
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        if r.starts_with(';') && !r.starts_with(";;") {
            // Semicolon separator: finish current group, start new one
            has_semicolon = true;
            groups.push(std::mem::take(&mut current_group));
            let r = &r[1..];
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                // Trailing semicolon before close paren
                return Ok((r, semicolon_groups_to_args(groups, current_group)));
            }
            let (r, arg) = parse_call_arg_expr(r)?;
            current_group.push(arg);
            rest = r;
            continue;
        }
        // Adjacent colonpairs without commas: foo(:a :b :c) or foo(:a:b:c)
        if r.starts_with(':')
            && !r.starts_with("::")
            && let Ok((r2, arg)) = crate::parser::primary::misc::colonpair_expr(r)
        {
            current_group.push(arg);
            rest = r2;
            continue;
        }
        if !r.starts_with(',') {
            if has_semicolon {
                groups.push(std::mem::take(&mut current_group));
                return Ok((r, semicolon_groups_to_args(groups, current_group)));
            }
            return Ok((r, current_group));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            if has_semicolon {
                groups.push(std::mem::take(&mut current_group));
                return Ok((r, semicolon_groups_to_args(groups, current_group)));
            }
            return Ok((r, current_group));
        }
        let (r, arg) = parse_call_arg_expr(r)?;
        current_group.push(arg);
        rest = r;
    }
}
