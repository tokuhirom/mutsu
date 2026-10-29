use crate::ast::{Expr, Stmt};
use crate::parser::expr::{contains_whatever, expression, expression_no_sequence};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::parser::stmt::keyword;
use crate::token_kind::TokenKind;

/// Try to parse a sequence operator (...) inside a paren expression.
/// If the input starts with ... or ...^, treat all collected items as seeds.
pub(crate) fn try_parse_sequence_in_paren<'a>(
    input: &'a str,
    seeds: &[Expr],
) -> Option<PResult<'a, Expr>> {
    // `is_left_excl` marks the `^...` / `^...^` left-exclusive forms, which
    // generate the same series and then drop the first element (`.skip(1)`).
    let (is_excl, is_left_excl, rest) = sequence_prefix(input)?;
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
                expression_no_sequence(rest)?
            }
        } else {
            expression_no_sequence(rest)?
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
            let (r2, item) = expression(r2)?;
            let item = match item {
                Expr::Binary {
                    left,
                    op:
                        crate::token_kind::TokenKind::DotDotDot
                        | crate::token_kind::TokenKind::DotDotDotCaret,
                    right,
                    ..
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
            form: Default::default(),
        };
        // Left-exclusive sequence: drop the first generated element (stays lazy).
        let seq = if is_left_excl {
            Expr::MethodCall {
                target: Box::new(seq),
                name: crate::symbol::Symbol::intern("skip"),
                args: vec![Expr::Literal(crate::value::Value::int(1))],
                modifier: None,
                quoted: false,
                sugar: false,
            }
        } else {
            seq
        };
        Ok((r, seq))
    })();
    Some(result)
}

pub(crate) fn starts_with_sequence_op(input: &str) -> bool {
    sequence_prefix(input).is_some()
}

/// The sequence operator at the start of `input`: whether it excludes the end
/// (`...^`), whether it excludes the start (`^...`), and the text after it.
/// `^...` / `^…` generate the same series and then drop the first element.
fn sequence_prefix(input: &str) -> Option<(bool, bool, &str)> {
    let (rest, kind, written) = crate::parser::expr::precedence_meta_ops::strip_sequence_op(input)?;
    Some((
        kind == TokenKind::DotDotDotCaret,
        written.starts_with('^'),
        rest,
    ))
}

fn maybe_wrap_whatever(expr: Expr) -> Expr {
    if contains_whatever(&expr) && !matches!(expr, Expr::Whatever) {
        Expr::WhateverCurry(Box::new(expr))
    } else {
        expr
    }
}

/// Raku's list-infix precedence makes the sequence operators (`...`/`…`/`...^`/`…^`)
/// LOOSER than comma, so in an argument list `a, b ... limit` the whole comma list
/// `a, b` is the sequence's seed and the entire list collapses into ONE sequence
/// argument (exactly like `(a, b ... limit)` in parentheses — see
/// [`try_parse_sequence_in_paren`]). Call/listop sites parse each comma-separated
/// argument individually, which would otherwise split the seed and drop the
/// endpoint. This helper detects a sequence operator at the top comma level and,
/// when present, returns the single sequence `Expr` spanning the whole list.
///
/// Returns `None` for an ordinary comma list with no top-level sequence operator,
/// so the caller keeps its normal per-argument handling. Terminator-agnostic (it
/// stops at the natural argument boundary via `expression_no_sequence`), so it
/// serves parenthesized calls, unparenthesized listops, and the `meth: args`
/// colon form alike.
pub(crate) fn try_parse_sequence_arg_list(input: &str) -> Option<PResult<'_, Expr>> {
    // A leading sequence operator with no seed is the `{ ... }`-style yada stub
    // used as an argument (`f(...)`), not a sequence — leave it to the normal path.
    if starts_with_sequence_op(input) {
        return None;
    }
    let (mut rest, first) = expression_no_sequence(input).ok()?;
    let mut seeds = vec![first];
    let op_input = loop {
        let (rws, _) = ws(rest).ok()?;
        if starts_with_sequence_op(rws) {
            break rws;
        }
        if !rws.starts_with(',') || rws.starts_with(",,") {
            return None; // ordinary list — no top-level sequence operator
        }
        let after = &rws[1..];
        let (after, _) = ws(after).ok()?;
        // End of the argument list (trailing comma) before any sequence operator,
        // or a colonpair / named argument — not a positional seed list.
        if after.is_empty()
            || after.starts_with(')')
            || after.starts_with(';')
            || after.starts_with('}')
            || (after.starts_with(':') && !after.starts_with("::"))
        {
            return None;
        }
        let (r2, item) = expression_no_sequence(after).ok()?;
        seeds.push(item);
        rest = r2;
    };
    Some(build_sequence_from_seeds(op_input, seeds))
}

/// Build a sequence `Expr` from already-parsed `seeds`, with `input` positioned at
/// the sequence operator. Parses the endpoint and any extra endpoint items
/// (`a ... limit, extra`), stopping at the natural argument boundary. Unlike
/// [`try_parse_sequence_in_paren`] it does NOT require a closing paren.
fn build_sequence_from_seeds(input: &str, seeds: Vec<Expr>) -> PResult<'_, Expr> {
    // `is_left_excl` marks the `^...` / `^...^` forms, which produce the same
    // series as `...` / `...^` but drop the first element (wrapped in `.skip(1)`
    // below). Check the longer `^...^` prefix before `^...`, and `...^` before `...`.
    let Some((is_excl, is_left_excl, rest)) = sequence_prefix(input) else {
        return Err(PError::expected("expected sequence operator"));
    };
    let (rest, _) = ws(rest)?;
    // Bare `*` endpoint = infinite sequence.
    let (mut rest, endpoint) = if rest.starts_with('*')
        && !rest[1..].starts_with(|c: char| c.is_alphanumeric() || c == '_')
    {
        let after_star = rest[1..].trim_start();
        if after_star.is_empty()
            || after_star.starts_with(')')
            || after_star.starts_with(',')
            || after_star.starts_with(';')
            || after_star.starts_with('}')
        {
            (&rest[1..], Expr::Whatever)
        } else {
            expression_no_sequence(rest)?
        }
    } else {
        expression_no_sequence(rest)?
    };
    // Extra endpoint items after the first (`a ... limit, extra1, extra2`) are
    // absorbed into the sequence, since the operator owns the whole comma level.
    // Parse each with the FULL expression parser (like `try_parse_sequence_in_paren`)
    // so a chained/waypoint sequence in an extra (`1 ... 3, 5 ... 7`) is consumed;
    // a nested `a ... b` waypoint becomes an `[a, b]` pair, matching the paren form.
    let mut extra_items = Vec::new();
    loop {
        let (rws, _) = ws(rest)?;
        if !rws.starts_with(',') || rws.starts_with(",,") {
            rest = rws;
            break;
        }
        let after = &rws[1..];
        let (after_ws, _) = ws(after)?;
        if after_ws.is_empty()
            || after_ws.starts_with(')')
            || after_ws.starts_with(';')
            || after_ws.starts_with('}')
            || (after_ws.starts_with(':') && !after_ws.starts_with("::"))
        {
            rest = rws; // leave the trailing comma for the caller
            break;
        }
        let (r2, item) = expression(after_ws)?;
        let item = match item {
            Expr::Binary {
                left,
                op: TokenKind::DotDotDot | TokenKind::DotDotDotCaret,
                right,
                ..
            } => Expr::ArrayLiteral(vec![*left, *right]),
            other => other,
        };
        extra_items.push(item);
        rest = r2;
    }
    let op = if is_excl {
        TokenKind::DotDotDotCaret
    } else {
        TokenKind::DotDotDot
    };
    let left = if seeds.len() == 1 {
        maybe_wrap_whatever(seeds.into_iter().next().unwrap())
    } else {
        Expr::ArrayLiteral(seeds.into_iter().map(maybe_wrap_whatever).collect())
    };
    let right = if extra_items.is_empty() {
        maybe_wrap_whatever(endpoint)
    } else {
        let mut v = vec![maybe_wrap_whatever(endpoint)];
        v.extend(extra_items.into_iter().map(maybe_wrap_whatever));
        Expr::ArrayLiteral(v)
    };
    let seq = Expr::Binary {
        left: Box::new(left),
        op,
        right: Box::new(right),
        form: Default::default(),
    };
    // `^...` / `^...^` drop the seed element from the generated series.
    let seq = if is_left_excl {
        Expr::MethodCall {
            target: Box::new(seq),
            name: crate::symbol::Symbol::intern("skip"),
            args: vec![Expr::Literal(crate::value::Value::int(1))],
            modifier: None,
            quoted: false,
            sugar: false,
        }
    } else {
        seq
    };
    Ok((rest, seq))
}

/// The body of a bare `{ ... }` block literal, or `None` for anything else
/// (including a pointy block `-> $x { ... }`, a `sub { ... }`, and the implicit
/// `@_` form `{ @_ }`, none of which are bare blocks).
///
/// A bare block with placeholder parameters (`{ $^a + $^b }`) is built as an
/// `AnonSubParams` by `make_anon_sub`, so it is told apart from a pointy block
/// by its parameter list being exactly the placeholders its body declares.
fn bare_block_body(expr: &Expr) -> Option<Vec<Stmt>> {
    match expr {
        Expr::AnonSub {
            body,
            is_block: true,
            ..
        } => Some(body.clone()),
        Expr::AnonSubParams {
            params,
            body,
            is_whatever_code: false,
            ..
        } if !params.is_empty() && *params == crate::ast::collect_placeholders_shallow(body) => {
            Some(body.clone())
        }
        _ => None,
    }
}

/// Try to parse an inline statement modifier inside parenthesized expression.
/// Handles: ($_ with data), (expr if cond), (expr for list), etc.
pub(crate) fn try_inline_modifier<'a>(input: &'a str, expr: Expr) -> Option<PResult<'a, Expr>> {
    use crate::parser::stmt::modifier::{
        is_stmt_modifier_after_trailing_comma, parse_statement_modifier,
    };
    // A keyword followed by => is a pair key, including after a list's comma.
    if !is_stmt_modifier_after_trailing_comma(input) {
        return None;
    }
    // A bare `{ ... }` block under `for` / `while` / `until` is the statement
    // the modifier repeats: `({ $_ + 1 } for 1,2,3)` is `(2 3 4)`, and the
    // block runs on each pass of a `while`. Parenthesised content is parsed as
    // an expression, so the block arrives as a closure term; hand it to the
    // loop as the `Stmt::Block` statement position would have produced.
    //
    // Under `if` / `unless` / `given` / `with` / `without` / `when` rakudo
    // leaves a block in expression position uncalled -- `({ 42 } if 1)` is the
    // Block itself -- so it stays a term there.
    let runs_block = keyword("for", input).is_some()
        || keyword("while", input).is_some()
        || keyword("until", input).is_some();
    let stmt = match bare_block_body(&expr).filter(|_| runs_block) {
        Some(body) => Stmt::Block(body),
        None => Stmt::Expr(expr),
    };
    let result = (|| {
        let (mut rest, mut modified_stmt) = parse_statement_modifier(input, stmt)?;
        loop {
            let (r, _) = ws(rest)?;
            if !is_stmt_modifier_after_trailing_comma(r) {
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
