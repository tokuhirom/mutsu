use super::super::parse_result::{PError, PResult, parse_char};

use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::super::expr::{expression, expression_no_sequence};
use super::super::helpers::{is_non_breaking_space, split_angle_words, ws};
use super::super::stmt::keyword;
use super::string::{
    double_quoted_string, single_quoted_string, smart_double_quoted_string,
    smart_single_quoted_string,
};

/// Parse a parenthesized expression or list.
pub(super) fn paren_expr(input: &str) -> PResult<'_, Expr> {
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
            (r, var_expr)
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
            return Ok((r_after, normalize_sequence_waypoints(full_expr)));
        }
        // When content starts with nested parens, the full parse can already
        // consume the closing ')' of this paren expression (e.g. `(() ... *)`).
        if content_start.starts_with('(') && r_full_ws.is_empty() {
            return Ok((r_full_ws, normalize_sequence_waypoints(full_expr)));
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
        return Ok((input, first));
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

fn finalize_paren_list(items: Vec<Expr>) -> Expr {
    let lifted = lift_meta_ops_in_paren_list(items);
    if let Some(expr) = lift_minmax_in_paren_list(&lifted) {
        return expr;
    }
    // If lifting produced a single MetaOp, return it unwrapped
    // (the Z/X meta-op already produces a list result)
    if lifted.len() == 1 && matches!(&lifted[0], Expr::MetaOp { .. }) {
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
        let mut seeds: Vec<Expr> = items[..idx].to_vec();
        seeds.push(*left.clone());
        let new_meta = Expr::MetaOp {
            meta: meta.clone(),
            op: op.clone(),
            left: Box::new(Expr::ArrayLiteral(seeds)),
            right: right.clone(),
        };
        let mut result = vec![new_meta];
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
            name: "item".to_string(),
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
    Ok((rest, Expr::CaptureLiteral(vec![inner])))
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
            name: "item".to_string(),
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
        return Ok((input, Expr::BracketArray(items)));
    }
    let (mut rest, first) = expression(input)?;
    items.push(first);
    loop {
        let (r, _) = ws(rest)?;
        if let Ok((r, _)) = parse_char(r, ',') {
            let (r, _) = ws(r)?;
            if let Ok((r, _)) = parse_char(r, ']') {
                return Ok((r, Expr::BracketArray(merge_sequence_seeds(items))));
            }
            let (r, next) = expression(r)?;
            items.push(next);
            rest = r;
        } else {
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ']')?;
            return Ok((r, Expr::BracketArray(merge_sequence_seeds(items))));
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
                    Expr::Literal(Value::Str(s)) => s,
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
    // Allow negative words/numerics such as <-1/0>.
    if reject_lt_operators
        && (input.starts_with('=')
            || (input.starts_with('-')
                && !input
                    .as_bytes()
                    .get(1)
                    .copied()
                    .is_some_and(|b| b.is_ascii_alphanumeric() || matches!(b, b'_' | b'/' | b'.'))))
    {
        return Err(PError::expected("angle list"));
    }
    let end = if quoted_words {
        find_quote_word_close(input, close)
    } else {
        input.find(close)
    };
    let Some(end) = end else {
        return Err(PError::expected("closing quote-word delimiter"));
    };
    let content = &input[..end];
    let rest = &input[end + close.len()..];
    let exprs = if quoted_words {
        split_quotish_words(content)?
    } else {
        split_angle_words(content)
            .into_iter()
            .map(angle_word_expr)
            .collect()
    };
    if exprs.len() == 1 {
        Ok((rest, exprs.into_iter().next().unwrap()))
    } else {
        Ok((rest, Expr::ArrayLiteral(exprs)))
    }
}

fn find_quote_word_close(input: &str, close: &str) -> Option<usize> {
    let mut i = 0usize;
    let mut quoted_by: Option<char> = None;
    let mut escaped = false;
    while i < input.len() {
        let rest = &input[i..];
        if quoted_by.is_none() && rest.starts_with(close) {
            return Some(i);
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
        } else if ch == '"' || ch == '\'' || ch == '“' || ch == '‘' {
            quoted_by = Some(match ch {
                '"' => '"',
                '\'' => '\'',
                '“' => '”',
                '‘' => '’',
                _ => unreachable!(),
            });
        }
        i += ch_len;
    }
    None
}

fn split_quotish_words(content: &str) -> Result<Vec<Expr>, PError> {
    let mut words = Vec::new();
    let mut rest = content;
    loop {
        rest = trim_breaking_ws(rest);
        if rest.is_empty() {
            break;
        }
        if let Some((r, quoted)) = parse_quoted_word(rest)? {
            words.push(Expr::Literal(Value::Str(quoted)));
            rest = r;
            continue;
        }
        let word_len = rest
            .char_indices()
            .find_map(|(idx, c)| (c.is_whitespace() && !is_non_breaking_space(c)).then_some(idx))
            .unwrap_or(rest.len());
        let (word, r) = rest.split_at(word_len);
        words.push(angle_word_expr(word));
        rest = r;
    }
    Ok(words)
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

fn parse_quoted_word(input: &str) -> Result<Option<(&str, String)>, PError> {
    if let Ok((rest, expr)) = single_quoted_string(input) {
        return quoted_word_literal(rest, expr);
    }
    if let Ok((rest, expr)) = smart_single_quoted_string(input) {
        return quoted_word_literal(rest, expr);
    }
    if let Ok((rest, expr)) = double_quoted_string(input) {
        return quoted_word_literal(rest, expr);
    }
    if let Ok((rest, expr)) = smart_double_quoted_string(input) {
        return quoted_word_literal(rest, expr);
    }
    Ok(None)
}

fn quoted_word_literal(rest: &str, expr: Expr) -> Result<Option<(&str, String)>, PError> {
    if let Expr::Literal(Value::Str(s)) = expr {
        Ok(Some((rest, s)))
    } else {
        Err(PError::expected("string literal word"))
    }
}
fn angle_word_expr(word: &str) -> Expr {
    // Raku `<...>` words produce allomorphic types: numeric-looking words
    // become IntStr, RatStr, NumStr, or ComplexStr — values that smartmatch
    // against both their numeric type and Str.
    // We represent allomorphs as Mixin(numeric_value, {"Str": Str(word)}).
    // Fraction notation (e.g. <3/2>) produces a plain Rat, not RatStr.
    if let Some((n, d)) = parse_angle_rat_word(word) {
        return Expr::Literal(crate::value::make_rat(n, d));
    }
    if let Some(complex) = parse_angle_complex(word) {
        return make_allomorphic_expr(complex, word);
    }
    if let Ok((rest, expr)) = super::number::integer(word)
        && rest.is_empty()
    {
        if let Expr::Literal(val) = expr {
            return make_allomorphic_expr(val, word);
        }
        return expr;
    }
    if let Ok((rest, expr)) = super::number::decimal(word)
        && rest.is_empty()
    {
        if let Expr::Literal(val) = expr {
            return make_allomorphic_expr(val, word);
        }
        return expr;
    }
    if let Ok((rest, expr)) = super::number::dot_decimal(word)
        && rest.is_empty()
    {
        if let Expr::Literal(val) = expr {
            return make_allomorphic_expr(val, word);
        }
        return expr;
    }
    if let Some(val) = parse_angle_num(word) {
        return make_allomorphic_expr(val, word);
    }
    Expr::Literal(Value::Str(word.to_string()))
}

/// Create an allomorphic expression: a numeric value that also matches Str.
fn make_allomorphic_expr(val: Value, word: &str) -> Expr {
    let mut mixins = std::collections::HashMap::new();
    mixins.insert("Str".to_string(), Value::Str(word.to_string()));
    Expr::Literal(Value::Mixin(Box::new(val), mixins))
}

fn parse_angle_rat_word(word: &str) -> Option<(i64, i64)> {
    let (lhs, rhs) = word.split_once('/')?;
    if lhs.is_empty() || rhs.is_empty() {
        return None;
    }
    let numer = parse_signed_i64_with_underscores(lhs)?;
    let denom = parse_signed_i64_with_underscores(rhs)?;
    Some((numer, denom))
}

fn parse_signed_i64_with_underscores(s: &str) -> Option<i64> {
    let (sign, rest) = if let Some(rest) = s.strip_prefix('+') {
        (1i64, rest)
    } else if let Some(rest) = s.strip_prefix('-') {
        (-1i64, rest)
    } else {
        (1i64, s)
    };
    if rest.is_empty() || !rest.chars().all(|c| c.is_ascii_digit() || c == '_') {
        return None;
    }
    let clean: String = rest.chars().filter(|c| *c != '_').collect();
    if clean.is_empty() {
        return None;
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
