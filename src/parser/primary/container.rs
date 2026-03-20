use super::super::parse_result::{PError, PResult, parse_char};

use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::super::expr::{expression, expression_no_sequence};
use super::super::helpers::{is_non_breaking_space, split_angle_words, ws};
use super::super::stmt::keyword;
use super::quote_adverbs::QuoteFlags;
use super::string::{parse_quotewords_quoted_atom, quotewords_atom_expr};

/// Parse a parenthesized expression or list.
pub(super) fn paren_expr(input: &str) -> PResult<'_, Expr> {
    // Try the comprehensive parenthesized assignment parser first.
    // This handles complex LHS forms like %hash{...}, @arr[...], method calls, etc.
    if let Ok((rest, assign_expr)) = super::super::stmt::assign::try_parse_assign_expr(input) {
        return Ok((rest, assign_expr));
    }
    let (input, _) = parse_char(input, '(')?;
    let (input, _) = ws(input)?;
    let content_start = input;
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
    // Try assignment expression: ($var = expr), (@arr = expr), (%hash = expr), or compound forms.
    let (input, first) = if let Ok((r, var_expr)) = expression_no_sequence(input) {
        let (r2, _) = ws(r)?;
        let assign_target = match &var_expr {
            Expr::Var(name) => Some((name.clone(), Expr::Var(name.clone()))),
            Expr::ArrayVar(name) => Some((format!("@{}", name), Expr::ArrayVar(name.clone()))),
            Expr::HashVar(name) => Some((format!("%{}", name), Expr::HashVar(name.clone()))),
            Expr::BareWord(name) => Some((name.clone(), Expr::BareWord(name.clone()))),
            _ => None,
        };
        if let Some((assign_name, lhs_expr)) = assign_target {
            if r2.starts_with('=') && !r2.starts_with("==") && !r2.starts_with("=>") {
                // Simple assignment: ($var = expr)
                let r2 = &r2[1..];
                let (r2, _) = ws(r2)?;
                let (r2, rhs) = expression(r2)?;
                (
                    r2,
                    Expr::AssignExpr {
                        name: assign_name,
                        expr: Box::new(rhs),
                    },
                )
            } else if let Some(stripped) = r2.strip_prefix("::=").or_else(|| r2.strip_prefix(":="))
            {
                // Binding expression in parens: ($var := expr)
                let (r2, _) = ws(stripped)?;
                let (r2, rhs) = expression(r2)?;
                (
                    r2,
                    Expr::AssignExpr {
                        name: assign_name,
                        expr: Box::new(rhs),
                    },
                )
            } else if let Some((stripped, op)) =
                super::super::stmt::assign::parse_compound_assign_op(r2)
            {
                // Compound assignment: ($var += expr)
                let (r2, _) = ws(stripped)?;
                let (r2, rhs) = expression(r2)?;
                (
                    r2,
                    Expr::AssignExpr {
                        name: assign_name,
                        expr: Box::new(Expr::Binary {
                            left: Box::new(lhs_expr),
                            op: op.token_kind(),
                            right: Box::new(rhs),
                        }),
                    },
                )
            } else {
                (r, var_expr)
            }
        } else {
            // Non-variable LHS compound assignment (e.g. `(* *= 2)` for WhateverCode)
            if let Some((stripped, op)) = super::super::stmt::assign::parse_compound_assign_op(r2) {
                let (r2, _) = ws(stripped)?;
                let (r2, rhs) = expression(r2)?;
                (
                    r2,
                    Expr::Binary {
                        left: Box::new(var_expr),
                        op: op.token_kind(),
                        right: Box::new(rhs),
                    },
                )
            } else {
                (r, var_expr)
            }
        }
    } else {
        expression_no_sequence(input)?
    };
    let (input, _) = ws(input)?;
    // If sequence syntax appears, try full expression parsing first.
    // This avoids mis-parsing cases like ("a"...* ~~ / z /) where
    // sequence is followed by another infix operator.
    if starts_with_sequence_op(input)
        && let Ok((r_full, full_expr)) = expression(content_start)
    {
        let (r_full_ws, _) = ws(r_full)?;
        if let Ok((r_after, _)) = parse_char(r_full_ws, ')') {
            return Ok((
                r_after,
                normalize_chained_zip_meta(normalize_sequence_waypoints(full_expr)),
            ));
        }
        // When content starts with nested parens, the full parse can already
        // consume the closing ')' of this paren expression (e.g. `(() ... *)`).
        if content_start.starts_with('(') && r_full_ws.is_empty() {
            return Ok((
                r_full_ws,
                normalize_chained_zip_meta(normalize_sequence_waypoints(full_expr)),
            ));
        }
    }
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
    // Chained colonpairs in parens: (:a(2) :b(3) :c(4)) → list of pairs.
    // In Raku, space-separated colonpairs inside parentheses form a list without commas.
    if is_colonpair_expr(&first) && looks_like_colonpair_start(input) {
        let mut items = vec![first];
        let mut rest = input;
        while looks_like_colonpair_start(rest) {
            let (r, pair) = super::misc::colonpair_expr(rest)?;
            items.push(pair);
            let (r, _) = ws(r)?;
            rest = r;
        }
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, finalize_paren_list(items)));
    }
    if let Ok((input, _)) = parse_char(input, ')') {
        // Parenthesized pair: (:a(3)) — mark as positional so it's not treated
        // as a named argument in function calls.
        let first = if matches!(
            &first,
            Expr::Binary {
                op: TokenKind::FatArrow,
                ..
            }
        ) {
            Expr::PositionalPair(Box::new(first))
        } else {
            first
        };
        return Ok((input, normalize_chained_zip_meta(first)));
    }
    // Comma-separated list with sequence operator detection
    // Use expression_no_sequence so that `...` is not consumed as part of an item
    let sep = if input.starts_with(',') {
        ','
    } else if input.starts_with(';') && !input.starts_with(";;") {
        ';'
    } else {
        return Err(PError::expected("',' or ';' in parenthesized list"));
    };
    let (input, _) = parse_char(input, sep)?;
    let (input, _) = ws(input)?;
    let mut items = vec![first];
    if let Some(result) = try_inline_modifier(input, finalize_paren_list(items.clone())) {
        let (rest, modified_expr) = result?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, modified_expr));
    }
    // Handle trailing comma before close paren
    if let Ok((input, _)) = parse_char(input, ')') {
        return Ok((input, finalize_paren_list(items)));
    }
    let (mut input_rest, second) = expression_no_sequence(input)?;
    items.push(second);
    loop {
        let (input, _) = ws(input_rest)?;
        if let Ok((input, _)) = parse_char(input, ')') {
            return Ok((input, finalize_paren_list(items)));
        }
        // Check for sequence operator before comma
        if let Some(seq) = try_parse_sequence_in_paren(input, &items) {
            return seq;
        }
        let sep = if input.starts_with(',') {
            ','
        } else if input.starts_with(';') && !input.starts_with(";;") {
            ';'
        } else {
            return Err(PError::expected("',' or ';' in parenthesized list"));
        };
        let (input, _) = parse_char(input, sep)?;
        let (input, _) = ws(input)?;
        if let Some(result) = try_inline_modifier(input, finalize_paren_list(items.clone())) {
            let (rest, modified_expr) = result?;
            let (rest, _) = ws(rest)?;
            let (rest, _) = parse_char(rest, ')')?;
            return Ok((rest, modified_expr));
        }
        if let Ok((input, _)) = parse_char(input, ')') {
            return Ok((input, finalize_paren_list(items)));
        }
        let (input, next) = expression_no_sequence(input)?;
        items.push(next);
        input_rest = input;
    }
}

/// Check if an expression is a colonpair (represented as a FatArrow binary expression).
fn is_colonpair_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Binary {
            op: TokenKind::FatArrow,
            ..
        }
    )
}

/// Check if the input starts with a colonpair pattern (`:name`, `:!name`, `:name(...)`, etc.)
/// but not `::` (namespace separator) or `:=` (binding) or `:N<radix>` (radix literal).
fn looks_like_colonpair_start(input: &str) -> bool {
    let Some(r) = input.strip_prefix(':') else {
        return false;
    };
    if r.starts_with(':') || r.starts_with('=') {
        return false;
    }
    // :36<...> is a radix literal, not a colonpair
    let digit_end = r
        .char_indices()
        .take_while(|(_, c)| crate::builtins::unicode::unicode_decimal_digit_value(*c).is_some())
        .last()
        .map(|(idx, c)| idx + c.len_utf8())
        .unwrap_or(0);
    if digit_end > 0 && r[digit_end..].starts_with('<') {
        return false;
    }
    // Must start with identifier char or `!` (negated colonpair)
    r.starts_with(|c: char| c.is_alphabetic() || c == '_' || c == '!')
}

fn finalize_paren_list(items: Vec<Expr>) -> Expr {
    let lifted = lift_meta_ops_in_paren_list(items);
    if let Some(expr) = lift_minmax_in_paren_list(&lifted) {
        return expr;
    }
    // If lifting produced a single MetaOp, return it unwrapped
    // (the Z/X meta-op already produces a list result)
    if lifted.len() == 1
        && (matches!(&lifted[0], Expr::MetaOp { .. })
            || matches!(&lifted[0], Expr::Call { name, .. } if *name == Symbol::intern("zip")))
    {
        return lifted.into_iter().next().unwrap();
    }
    Expr::ArrayLiteral(lifted)
}

fn lift_minmax_in_paren_list(items: &[Expr]) -> Option<Expr> {
    if items.len() < 3 {
        return None;
    }
    let idx = items.iter().position(|expr| {
        matches!(
            expr,
            Expr::InfixFunc {
                name,
                modifier: None,
                right,
                ..
            } if name == "minmax" && right.len() == 1
        )
    })?;
    let Expr::InfixFunc {
        name,
        left,
        right,
        modifier,
    } = &items[idx]
    else {
        return None;
    };

    let mut lhs_items: Vec<Expr> = items[..idx].to_vec();
    lhs_items.push((**left).clone());

    let mut rhs_items: Vec<Expr> = vec![right[0].clone()];
    rhs_items.extend_from_slice(&items[idx + 1..]);

    let lhs = if lhs_items.len() == 1 {
        lhs_items.remove(0)
    } else {
        Expr::ArrayLiteral(lhs_items)
    };
    let rhs = if rhs_items.len() == 1 {
        rhs_items.remove(0)
    } else {
        Expr::ArrayLiteral(rhs_items)
    };

    Some(Expr::InfixFunc {
        name: name.clone(),
        left: Box::new(lhs),
        right: vec![rhs],
        modifier: modifier.clone(),
    })
}

fn normalize_sequence_waypoints(expr: Expr) -> Expr {
    match expr {
        Expr::Binary { left, op, right }
            if matches!(
                op,
                crate::token_kind::TokenKind::DotDotDot
                    | crate::token_kind::TokenKind::DotDotDotCaret
            ) =>
        {
            let right = match *right {
                Expr::ArrayLiteral(items) => Expr::ArrayLiteral(
                    items
                        .into_iter()
                        .map(|item| match item {
                            Expr::Binary {
                                left,
                                op:
                                    crate::token_kind::TokenKind::DotDotDot
                                    | crate::token_kind::TokenKind::DotDotDotCaret,
                                right,
                            } => Expr::ArrayLiteral(vec![*left, *right]),
                            other => other,
                        })
                        .collect(),
                ),
                other => other,
            };
            Expr::Binary {
                left,
                op,
                right: Box::new(right),
            }
        }
        other => other,
    }
}

fn normalize_chained_zip_meta(expr: Expr) -> Expr {
    match expr {
        Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } if meta == "Z" => {
            let left = normalize_chained_zip_meta(*left);
            let right = normalize_chained_zip_meta(*right);

            // Case 1: left is an ArrayLiteral whose last element is a nested Z MetaOp.
            // e.g. (a, b Z+ c, d) Z+ (e, f)
            if let Expr::ArrayLiteral(mut left_items) = left.clone()
                && let Expr::ArrayLiteral(right_items) = right.clone()
                && let Some(Expr::MetaOp {
                    meta: inner_meta,
                    op: inner_op,
                    left: inner_left,
                    right: inner_right,
                }) = left_items.pop()
                && inner_meta == "Z"
                && inner_op == op
                && let Expr::ArrayLiteral(inner_right_items) = *inner_right
            {
                let mut first_col = left_items;
                first_col.push(*inner_left);
                let to_expr = |col: Vec<Expr>| {
                    if col.len() == 1 {
                        col.into_iter().next().unwrap()
                    } else {
                        Expr::ArrayLiteral(col)
                    }
                };
                let mut args = vec![
                    to_expr(first_col),
                    to_expr(inner_right_items),
                    to_expr(right_items),
                ];
                args.push(Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str_from("with"))),
                    op: TokenKind::FatArrow,
                    right: Box::new(Expr::CodeVar(format!("infix:<{}>", op))),
                });
                return Expr::Call {
                    name: Symbol::intern("zip"),
                    args,
                };
            }

            // Case 2: left is itself a Z MetaOp with the same op.
            // e.g. (A Z B Z C) where left = Z(A, B), right = C
            // Collect all operands into a multi-way zip call.
            if let Expr::MetaOp {
                meta: ref inner_meta,
                op: ref inner_op,
                ..
            } = left
                && inner_meta == "Z"
                && *inner_op == op
            {
                let mut args = collect_zip_operands(&left, &op);
                args.push(right);
                if !op.is_empty() {
                    args.push(Expr::Binary {
                        left: Box::new(Expr::Literal(Value::str_from("with"))),
                        op: TokenKind::FatArrow,
                        right: Box::new(Expr::CodeVar(format!("infix:<{}>", op))),
                    });
                }
                return Expr::Call {
                    name: Symbol::intern("zip"),
                    args,
                };
            }

            Expr::MetaOp {
                meta,
                op,
                left: Box::new(left),
                right: Box::new(right),
            }
        }
        other => other,
    }
}

/// Collect all operands from a left-nested chain of Z MetaOps with the same op.
fn collect_zip_operands(expr: &Expr, expected_op: &str) -> Vec<Expr> {
    if let Expr::MetaOp {
        meta,
        op,
        left,
        right,
    } = expr
        && meta == "Z"
        && op == expected_op
    {
        let mut operands = collect_zip_operands(left, expected_op);
        operands.push(*right.clone());
        return operands;
    }
    vec![expr.clone()]
}

fn lift_meta_ops_in_paren_list(items: Vec<Expr>) -> Vec<Expr> {
    let meta_idx = items.iter().position(|e| matches!(e, Expr::MetaOp { .. }));
    if let Some(idx) = meta_idx
        && idx > 0
        && let Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } = &items[idx]
    {
        // Flatten left-nested same-op chains into column lists.
        // Example source shape:
        //   (a, (b Zop c,d) Zop e,f)
        // becomes columns:
        //   (a,b), (c,d), (e,f)
        let as_column = |expr: &Expr| match expr {
            Expr::ArrayLiteral(items) => items.clone(),
            other => vec![other.clone()],
        };
        let mut columns_rev: Vec<Vec<Expr>> = vec![as_column(right)];
        let mut chain_left = left.as_ref().clone();
        while let Expr::MetaOp {
            meta: inner_meta,
            op: inner_op,
            left: inner_left,
            right: inner_right,
        } = &chain_left
        {
            if *inner_meta != *meta || *inner_op != *op {
                break;
            }
            columns_rev.push(as_column(inner_right));
            chain_left = *inner_left.clone();
        }
        let mut first_col: Vec<Expr> = items[..idx].to_vec();
        first_col.push(chain_left);
        columns_rev.push(first_col);
        columns_rev.reverse();

        let lifted_expr = if *meta == "Z" && columns_rev.len() > 2 {
            let mut args: Vec<Expr> = columns_rev
                .into_iter()
                .map(|col| {
                    if col.len() == 1 {
                        col.into_iter().next().unwrap()
                    } else {
                        Expr::ArrayLiteral(col)
                    }
                })
                .collect();
            args.push(Expr::Binary {
                left: Box::new(Expr::Literal(Value::str_from("with"))),
                op: TokenKind::FatArrow,
                right: Box::new(Expr::CodeVar(format!("infix:<{}>", op))),
            });
            Expr::Call {
                name: Symbol::intern("zip"),
                args,
            }
        } else {
            let mut iter = columns_rev.into_iter();
            let lhs = iter.next().unwrap_or_default();
            let rhs = iter.next().unwrap_or_default();
            let lhs = if lhs.len() == 1 {
                lhs.into_iter().next().unwrap()
            } else {
                Expr::ArrayLiteral(lhs)
            };
            let rhs = if rhs.len() == 1 {
                rhs.into_iter().next().unwrap()
            } else {
                Expr::ArrayLiteral(rhs)
            };
            Expr::MetaOp {
                meta: meta.clone(),
                op: op.clone(),
                left: Box::new(lhs),
                right: Box::new(rhs),
            }
        };

        let mut result = vec![lifted_expr];
        result.extend(items[idx + 1..].to_vec());
        return result;
    }
    items
}

/// Parse itemized parenthesized expression: `$(...)`.
///
/// In Raku, `$(expr)` creates an item container — the value is evaluated and
/// wrapped in a scalar so that operations like `.flat` treat it as a single
/// opaque element.  We lower this to a method call `.item` on the inner
/// expression, which mirrors what Rakudo does internally.
pub(super) fn itemized_paren_expr(input: &str) -> PResult<'_, Expr> {
    let Some(rest) = input.strip_prefix('$') else {
        return Err(PError::expected("itemized parenthesized expression"));
    };
    if !rest.starts_with('(') {
        return Err(PError::expected("itemized parenthesized expression"));
    }
    let (rest, inner) = paren_expr(rest)?;
    // Lower $(expr) to expr.item — wraps the value in a Scalar container
    Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(inner),
            name: Symbol::intern("item"),
            args: vec![],
            modifier: None,
            quoted: false,
        },
    ))
}

/// Parse itemized brace expression: `${ }`.
///
/// In Raku, `${ a => 1, b => 2 }` creates an itemized hash — it wraps the
/// hash in a Scalar container so it's treated as a single element.
pub(super) fn itemized_brace_expr(input: &str) -> PResult<'_, Expr> {
    let Some(rest) = input.strip_prefix('$') else {
        return Err(PError::expected("itemized brace expression"));
    };
    if !rest.starts_with('{') {
        return Err(PError::expected("itemized brace expression"));
    }
    let (rest, inner) = super::misc::block_or_hash_expr(rest)?;
    // When the inner expression is a Hash literal, ${ } creates an itemized hash
    // (wrapped in a Scalar container), not a Capture.
    if matches!(inner, Expr::Hash(_)) {
        Ok((
            rest,
            Expr::MethodCall {
                target: Box::new(inner),
                name: Symbol::intern("item"),
                args: vec![],
                modifier: None,
                quoted: false,
            },
        ))
    } else {
        Ok((rest, Expr::CaptureLiteral(vec![inner])))
    }
}

/// Parse itemized bracket expression: `$[...]`.
///
/// Rakudo lowers this as a normal bracket constructor followed by `.item`.
pub(super) fn itemized_bracket_expr(input: &str) -> PResult<'_, Expr> {
    let Some(rest) = input.strip_prefix('$') else {
        return Err(PError::expected("itemized bracket expression"));
    };
    if !rest.starts_with('[') {
        return Err(PError::expected("itemized bracket expression"));
    }
    let (rest, inner) = array_literal(rest)?;
    Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(inner),
            name: Symbol::intern("item"),
            args: vec![],
            modifier: None,
            quoted: false,
        },
    ))
}

/// Try to parse a sequence operator (...) inside a paren expression.
/// If the input starts with ... or ...^, treat all collected items as seeds.
fn try_parse_sequence_in_paren<'a>(input: &'a str, seeds: &[Expr]) -> Option<PResult<'a, Expr>> {
    let (is_excl, rest) = if let Some(stripped) = input.strip_prefix("...^") {
        (true, stripped)
    } else if let Some(stripped) = input.strip_prefix("…^") {
        (true, stripped)
    } else if input.starts_with("...") && !input.starts_with("....") {
        (false, &input[3..])
    } else if let Some(stripped) = input.strip_prefix("…") {
        (false, stripped)
    } else {
        return None;
    };
    // Parse the endpoint expression
    let result = (|| {
        let (rest, _) = ws(rest)?;
        // Special case: bare * means infinite sequence (Whatever/Inf)
        // Only treat as bare Whatever if followed by `)` or `,` (after whitespace),
        // not when followed by an operator like `> 64` (which is WhateverCode).
        let (rest, endpoint) = if rest.starts_with('*')
            && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
        {
            let after_star = rest[1..].trim_start();
            if after_star.starts_with(')') || after_star.starts_with(',') || after_star.is_empty() {
                (&rest[1..], Expr::Whatever)
            } else {
                super::super::expr::expression_no_sequence(rest)?
            }
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
            let item = match item {
                Expr::Binary {
                    left,
                    op:
                        crate::token_kind::TokenKind::DotDotDot
                        | crate::token_kind::TokenKind::DotDotDotCaret,
                    right,
                } => Expr::ArrayLiteral(vec![*left, *right]),
                other => other,
            };
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

fn starts_with_sequence_op(input: &str) -> bool {
    input.starts_with("...") || input.starts_with("…")
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
        return Ok((input, Expr::BracketArray(items, false)));
    }
    let (mut rest, first) = expression(input)?;
    items.push(first);
    loop {
        let (r, _) = ws(rest)?;
        if let Ok((r, _)) = parse_char(r, ',') {
            let (r, _) = ws(r)?;
            if let Ok((r, _)) = parse_char(r, ']') {
                return Ok((r, Expr::BracketArray(merge_sequence_seeds(items), true)));
            }
            let (r, next) = expression(r)?;
            items.push(next);
            rest = r;
        } else {
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ']')?;
            return Ok((r, Expr::BracketArray(merge_sequence_seeds(items), false)));
        }
    }
}

fn merge_sequence_seeds(items: Vec<Expr>) -> Vec<Expr> {
    if items.len() < 2 {
        return items;
    }
    let last = items.last().unwrap();
    if let Expr::Binary { left, op, right } = last
        && matches!(
            op,
            crate::token_kind::TokenKind::DotDotDot | crate::token_kind::TokenKind::DotDotDotCaret
        )
    {
        let mut seeds: Vec<Expr> = items[..items.len() - 1].to_vec();
        seeds.push(*left.clone());
        let merged = Expr::Binary {
            left: Box::new(Expr::ArrayLiteral(seeds)),
            op: op.clone(),
            right: right.clone(),
        };
        vec![merged]
    } else {
        items
    }
}

/// Parse a hash constructor literal: %(key => value, :name, ...)
pub(super) fn percent_hash_literal(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '%')?;
    let (input, _) = parse_char(input, '(')?;
    let (mut rest, _) = ws(input)?;
    let mut pairs = Vec::new();

    if let Ok((rest_after, _)) = parse_char(rest, ')') {
        return Ok((rest_after, Expr::Hash(pairs)));
    }

    loop {
        let (r, item) = expression(rest)?;
        let (key, value) = match item {
            Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::FatArrow,
                right,
            } => {
                let key = match *left {
                    Expr::Literal(Value::Str(s)) => s.to_string(),
                    Expr::Literal(Value::Int(i)) => i.to_string(),
                    _ => return Err(PError::expected("hash pair key")),
                };
                (key, Some(*right))
            }
            _ => return Err(PError::expected("hash pair")),
        };
        pairs.push((key, value));

        let (r, _) = ws(r)?;
        if let Ok((r, _)) = parse_char(r, ',') {
            let (r, _) = ws(r)?;
            if let Ok((r_after, _)) = parse_char(r, ')') {
                return Ok((r_after, Expr::Hash(pairs)));
            }
            rest = r;
            continue;
        }
        // Newline-separated colonpairs without commas: treat as separate
        // statements (like Raku), keeping only the last entry.
        if r.starts_with(':') {
            pairs.clear();
            rest = r;
            continue;
        }
        let (r, _) = parse_char(r, ')')?;
        return Ok((r, Expr::Hash(pairs)));
    }
}

/// Parse a < > quote-word list.
pub(super) fn angle_list(input: &str) -> PResult<'_, Expr> {
    parse_quote_word_list(input, "<", ">", true, false)
}

/// Parse a « » quote-word list.
pub(super) fn french_quote_list(input: &str) -> PResult<'_, Expr> {
    parse_quote_word_list(input, "«", "»", false, true)
}

/// Parse a << >> quote-word list.
pub(super) fn double_angle_list(input: &str) -> PResult<'_, Expr> {
    parse_quote_word_list(input, "<<", ">>", false, true)
}

fn parse_quote_word_list<'a>(
    input: &'a str,
    open: &str,
    close: &str,
    reject_lt_operators: bool,
    quoted_words: bool,
) -> PResult<'a, Expr> {
    let Some(input) = input.strip_prefix(open) else {
        return Err(PError::expected("quote-word list"));
    };
    // For `<...>`, reject leading operator forms like <= and <=>.
    // Allow negative words/numerics such as <-1/0> and word lists starting
    // with a bare hyphen like <- a - b ->.
    if reject_lt_operators
        && (input.starts_with('=')
            || (input.starts_with('-')
                && !input.as_bytes().get(1).copied().is_some_and(|b| {
                    b.is_ascii_alphanumeric()
                        || b == b' '
                        || matches!(b, b'_' | b'/' | b'.' | b'+' | b'-')
                })))
    {
        return Err(PError::expected("angle list"));
    }
    let end = if quoted_words {
        find_quote_word_close(input, close)
    } else if close == ">" {
        find_nested_angle_close(input)
    } else {
        input.find(close)
    };
    let Some(end) = end else {
        return Err(PError::expected("closing quote-word delimiter"));
    };
    let content = &input[..end];
    let rest = &input[end + close.len()..];
    if quoted_words {
        let exprs = split_quotish_words(content)?;
        return Ok((rest, super::string::make_word_result_expr(exprs)));
    }
    let words = split_angle_words(content);
    if words.len() == 1 {
        let expr = angle_word_expr(words[0]);
        // Single-word angle brackets: <7+8i> produces plain Complex, not ComplexStr
        // and <2/3> produces plain Rat, not RatStr.
        let expr = match expr {
            Expr::Literal(Value::Mixin(inner, _))
                if matches!(inner.as_ref(), Value::Complex(..)) =>
            {
                Expr::Literal(inner.as_ref().clone())
            }
            other => other,
        };
        Ok((rest, expr))
    } else {
        // Multi-element lists: fractions also become allomorphic (RatStr)
        let exprs = words
            .iter()
            .map(|w| Expr::Literal(angle_word_value_full_allomorphic(w)))
            .collect();
        Ok((rest, Expr::ArrayLiteral(exprs)))
    }
}

fn find_quote_word_close(input: &str, close: &str) -> Option<usize> {
    let mut i = 0usize;
    let mut quoted_by: Option<char> = None;
    let mut escaped = false;
    let mut angle_depth = 0usize;
    while i < input.len() {
        let rest = &input[i..];
        if quoted_by.is_none()
            && angle_depth == 0
            && rest.starts_with(close)
            && !rest
                .strip_prefix(close)
                .is_some_and(|after| close == ">>" && after.starts_with('>'))
        {
            return Some(i);
        }
        if quoted_by.is_none() && rest.starts_with("#`") {
            let after = skip_quotish_embedded_comment(&rest[2..])?;
            i = input.len() - after.len();
            continue;
        }
        if quoted_by.is_none() && rest.starts_with('#') {
            let end = rest.find('\n').unwrap_or(rest.len());
            i += end;
            continue;
        }
        let mut chars = rest.chars();
        let ch = chars.next()?;
        let ch_len = ch.len_utf8();
        if let Some(quote) = quoted_by {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == quote {
                quoted_by = None;
            }
        } else if ch == '"'
            || ch == '\''
            || ch == '“'
            || ch == '”'
            || ch == '„'
            || ch == '‘'
            || ch == '’'
            || ch == '‚'
        {
            quoted_by = Some(match ch {
                '"' => '"',
                '\'' => '\'',
                '“' => '”',
                '”' => '”',
                '„' => '”',
                '‘' => '’',
                '’' => '’',
                '‚' => '’',
                _ => unreachable!(),
            });
        } else if close == ">>" {
            if ch == '<' {
                angle_depth += 1;
            } else if ch == '>' && angle_depth > 0 {
                angle_depth -= 1;
            }
        }
        i += ch_len;
    }
    None
}

/// Find the closing `>` for `<...>`, handling nested `<>` pairs
/// (e.g. `<:13<01>/:13<07>>`).
fn find_nested_angle_close(input: &str) -> Option<usize> {
    let mut depth: usize = 0;
    let mut i = 0usize;
    while i < input.len() {
        let b = input.as_bytes()[i];
        if b == b'<' {
            depth += 1;
        } else if b == b'>' {
            if depth == 0 {
                return Some(i);
            }
            depth -= 1;
        }
        i += 1;
    }
    None
}

fn split_quotish_words(content: &str) -> Result<Vec<Expr>, PError> {
    let mut words = Vec::new();
    let mut rest = content;
    let flags = QuoteFlags::qq_double();
    loop {
        rest = trim_quotish_ws_and_comments(rest)?;
        if rest.is_empty() {
            break;
        }
        if let Some((r, quoted)) = parse_quoted_word(rest)? {
            words.push(quoted);
            rest = r;
            continue;
        }
        let word_len = find_quotish_word_end(rest);
        let (word, r) = rest.split_at(word_len);
        if word.starts_with(':')
            && let Ok((remaining, expr)) = expression(word)
            && remaining.is_empty()
        {
            words.push(Expr::MethodCall {
                target: Box::new(expr),
                name: Symbol::intern("item"),
                args: vec![],
                modifier: None,
                quoted: false,
            });
        } else if word.contains('$')
            || word.contains('@')
            || word.contains('%')
            || word.contains('&')
            || word.contains('{')
            || word.contains('\\')
        {
            let expr = super::quote_adverbs::process_content_with_flags(word, &flags);
            words.push(quotewords_atom_expr(expr));
        } else {
            // «...» and <<...>> are equivalent to qqww:v, so fractions
            // also produce allomorphic types (RatStr).
            words.push(Expr::Literal(angle_word_value_full_allomorphic(word)));
        }
        rest = r;
    }
    Ok(words)
}

fn trim_quotish_ws_and_comments(mut input: &str) -> Result<&str, PError> {
    loop {
        let trimmed = trim_breaking_ws(input);
        if trimmed.len() != input.len() {
            input = trimmed;
            continue;
        }
        if let Some(after) = input.strip_prefix("#`") {
            input = skip_quotish_embedded_comment(after)
                .ok_or_else(|| PError::expected("Opening bracket required for #` comment"))?;
            continue;
        }
        if let Some(after) = input.strip_prefix('#') {
            let end = after.find('\n').unwrap_or(after.len());
            input = &after[end..];
            continue;
        }
        return Ok(input);
    }
}

fn skip_quotish_embedded_comment(input: &str) -> Option<&str> {
    let mut chars = input.chars();
    let open = chars.next()?;
    let close = match open {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        '«' => '»',
        '“' => '”',
        '”' => '”',
        '„' => '”',
        '‘' => '’',
        '’' => '’',
        '‚' => '’',
        _ => return None,
    };
    let mut count = 1usize;
    let mut rest = chars.as_str();
    while rest.starts_with(open) {
        count += 1;
        rest = &rest[open.len_utf8()..];
    }
    let close_seq: String = std::iter::repeat_n(close, count).collect();
    let open_seq: String = std::iter::repeat_n(open, count).collect();
    if count == 1 {
        let mut depth = 1i32;
        let mut scan = rest;
        while !scan.is_empty() {
            let ch = scan.chars().next().unwrap();
            if ch == open {
                depth += 1;
            } else if ch == close {
                depth -= 1;
                if depth == 0 {
                    return Some(&scan[ch.len_utf8()..]);
                }
            }
            scan = &scan[ch.len_utf8()..];
        }
        None
    } else {
        let mut depth = 1i32;
        let mut scan = rest;
        while !scan.is_empty() {
            if scan.starts_with(&close_seq) {
                depth -= 1;
                if depth == 0 {
                    return Some(&scan[close_seq.len()..]);
                }
                scan = &scan[close_seq.len()..];
            } else if scan.starts_with(&open_seq) {
                depth += 1;
                scan = &scan[open_seq.len()..];
            } else {
                let ch = scan.chars().next().unwrap();
                scan = &scan[ch.len_utf8()..];
            }
        }
        None
    }
}

fn find_quotish_word_end(input: &str) -> usize {
    for (idx, c) in input.char_indices() {
        if (c.is_whitespace() && !is_non_breaking_space(c))
            || c == '#'
            || c == '"'
            || c == '\''
            || c == '“'
            || c == '‘'
            || c == '”'
            || c == '’'
            || c == '„'
            || c == '‚'
            || c == '｢'
        {
            return idx;
        }
    }
    input.len()
}

fn trim_breaking_ws(input: &str) -> &str {
    let mut idx = 0usize;
    for (i, c) in input.char_indices() {
        if !c.is_whitespace() || is_non_breaking_space(c) {
            idx = i;
            break;
        }
        idx = i + c.len_utf8();
    }
    &input[idx..]
}

fn parse_quoted_word(input: &str) -> Result<Option<(&str, Expr)>, PError> {
    parse_quotewords_quoted_atom(input)
}
pub(crate) fn angle_word_expr(word: &str) -> Expr {
    Expr::Literal(angle_word_value(word))
}

pub(crate) fn angle_word_value(word: &str) -> Value {
    angle_word_value_impl(word, false)
}

/// Like `angle_word_value` but always produces allomorphic types for fractions.
/// Used for multi-element word lists where `<2/3>` becomes RatStr, not plain Rat.
pub(crate) fn angle_word_value_full_allomorphic(word: &str) -> Value {
    angle_word_value_impl(word, true)
}

fn angle_word_value_impl(word: &str, fraction_allomorphic: bool) -> Value {
    // Raku `<...>` words produce allomorphic types: numeric-looking words
    // become IntStr, RatStr, NumStr, or ComplexStr — values that smartmatch
    // against both their numeric type and Str.
    // We represent allomorphs as Mixin(numeric_value, {"Str": Str(word)}).
    // For single-element <2/3>, fraction notation produces a plain Rat, not RatStr.
    // For multi-element lists, fractions produce RatStr.

    // Normalize U+2212 MINUS SIGN to ASCII minus for numeric parsing.
    // The allomorphic Str part retains the original word spelling.
    let normalized;
    let parse_word = if word.contains('\u{2212}') {
        normalized = word.replace('\u{2212}', "-");
        normalized.as_str()
    } else {
        word
    };
    if let Some(rat) = parse_angle_rat_word(parse_word) {
        if fraction_allomorphic {
            return make_allomorphic_value(rat, word);
        }
        return rat;
    }
    if let Some(complex) = parse_angle_complex(parse_word) {
        return make_allomorphic_value(complex, word);
    }
    if let Ok((rest, expr)) = super::number::integer(parse_word)
        && rest.is_empty()
        && let Expr::Literal(val) = expr
    {
        return make_allomorphic_value(val, word);
    }
    if let Ok((rest, expr)) = super::number::decimal(parse_word)
        && rest.is_empty()
        && let Expr::Literal(val) = expr
    {
        return make_allomorphic_value(val, word);
    }
    if let Ok((rest, expr)) = super::number::dot_decimal(parse_word)
        && rest.is_empty()
        && let Expr::Literal(val) = expr
    {
        return make_allomorphic_value(val, word);
    }
    if let Some(val) = parse_angle_num(parse_word) {
        return make_allomorphic_value(val, word);
    }
    Value::str(word.to_string())
}

fn make_allomorphic_value(val: Value, word: &str) -> Value {
    let mut mixins = std::collections::HashMap::new();
    mixins.insert("Str".to_string(), Value::str(word.to_string()));
    Value::mixin(val, mixins)
}

fn parse_angle_rat_word(word: &str) -> Option<Value> {
    let (lhs, rhs) = word.split_once('/')?;
    if lhs.is_empty() || rhs.is_empty() {
        return None;
    }
    // Don't parse negative denominators as Rat (Raku spec: <1/-3> is Str)
    if rhs.starts_with('-') {
        return None;
    }
    // Try i64 first, fall back to BigInt for large numbers
    if let (Some(n), Some(d)) = (parse_angle_int(lhs), parse_angle_int(rhs)) {
        return Some(crate::value::make_rat(n, d));
    }
    // BigInt fallback
    let numer = parse_angle_bigint(lhs)?;
    let denom = parse_angle_bigint(rhs)?;
    Some(crate::value::make_big_rat(numer, denom))
}

fn parse_angle_bigint(s: &str) -> Option<num_bigint::BigInt> {
    let (sign_neg, rest) = if let Some(rest) = s.strip_prefix('+') {
        (false, rest)
    } else if let Some(rest) = s.strip_prefix('-') {
        (true, rest)
    } else {
        (false, s)
    };
    if rest.is_empty() {
        return None;
    }
    let clean: String = rest.chars().filter(|c| *c != '_').collect();
    if clean.is_empty() {
        return None;
    }
    // Support 0x, 0b, 0o prefixes
    let val = if let Some(hex) = clean
        .strip_prefix("0x")
        .or_else(|| clean.strip_prefix("0X"))
    {
        num_bigint::BigInt::parse_bytes(hex.as_bytes(), 16)?
    } else if let Some(bin) = clean
        .strip_prefix("0b")
        .or_else(|| clean.strip_prefix("0B"))
    {
        num_bigint::BigInt::parse_bytes(bin.as_bytes(), 2)?
    } else if let Some(oct) = clean
        .strip_prefix("0o")
        .or_else(|| clean.strip_prefix("0O"))
    {
        num_bigint::BigInt::parse_bytes(oct.as_bytes(), 8)?
    } else {
        clean.parse::<num_bigint::BigInt>().ok()?
    };
    if sign_neg { Some(-val) } else { Some(val) }
}

/// Parse an integer that may have a 0x/0b/0o prefix, sign, or underscores.
fn parse_angle_int(s: &str) -> Option<i64> {
    let (sign, rest) = if let Some(rest) = s.strip_prefix('+') {
        (1i64, rest)
    } else if let Some(rest) = s.strip_prefix('-') {
        (-1i64, rest)
    } else {
        (1i64, s)
    };
    if rest.is_empty() {
        return None;
    }
    let clean: String = rest.chars().filter(|c| *c != '_').collect();
    if clean.is_empty() {
        return None;
    }
    if let Some(hex) = clean
        .strip_prefix("0x")
        .or_else(|| clean.strip_prefix("0X"))
    {
        return i64::from_str_radix(hex, 16).ok().map(|n| sign * n);
    }
    if let Some(bin) = clean
        .strip_prefix("0b")
        .or_else(|| clean.strip_prefix("0B"))
    {
        return i64::from_str_radix(bin, 2).ok().map(|n| sign * n);
    }
    if let Some(oct) = clean
        .strip_prefix("0o")
        .or_else(|| clean.strip_prefix("0O"))
    {
        return i64::from_str_radix(oct, 8).ok().map(|n| sign * n);
    }
    clean.parse::<i64>().ok().map(|n| sign * n)
}

/// Parse a complex number literal from an angle bracket word.
/// Handles forms like: 3+0i, -2+5i, 0+31337i, 3-3i, 5i, -3i, 3.5+2.1i, 2e0+0i
fn parse_angle_complex(word: &str) -> Option<Value> {
    let word = word.trim();
    // Must end with 'i'
    if !word.ends_with('i') {
        return None;
    }
    let without_i = &word[..word.len() - 1];

    // Pure imaginary: just "Ni" (e.g. "5i", "-3i")
    if let Ok(imag) = without_i.parse::<f64>() {
        return Some(Value::Complex(0.0, imag));
    }

    // Find the last '+' or '-' that splits real from imaginary.
    // Skip the first character to allow a leading sign on the real part.
    // Also skip 'e'/'E' followed by sign (scientific notation like 2e-3).
    let bytes = without_i.as_bytes();
    let mut split_pos = None;
    let mut i = 1;
    while i < bytes.len() {
        if (bytes[i] == b'+' || bytes[i] == b'-')
            && i > 0
            && bytes[i - 1] != b'e'
            && bytes[i - 1] != b'E'
        {
            split_pos = Some(i);
        }
        i += 1;
    }

    let split_pos = split_pos?;
    let real_str = &without_i[..split_pos];
    let imag_str = &without_i[split_pos..];

    let real: f64 = real_str.parse().ok()?;
    let imag: f64 = imag_str.parse().ok()?;
    Some(Value::Complex(real, imag))
}

/// Parse a Num (floating-point with exponent) from an angle bracket word.
/// Handles forms like: 2e0, 5e0, -8e0, 3.5e2
fn parse_angle_num(word: &str) -> Option<Value> {
    let word = word.trim();
    // Must contain 'e' or 'E' to be a Num (otherwise it would have been caught by decimal)
    if !word.contains('e') && !word.contains('E') {
        return None;
    }
    let val: f64 = word.parse().ok()?;
    Some(Value::Num(val))
}
