use crate::ast::{Expr, Stmt};
use crate::parser::expr::{
    contains_whatever, expression, expression_no_sequence, wrap_whatevercode,
};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::parser::stmt::keyword;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

/// Finalize a flat (comma-separated) parenthesized list.
/// Lifts embedded meta-ops (X/Z), minmax, and handles Whatever-curry.
pub(crate) fn finalize_paren_list(items: Vec<Expr>) -> Expr {
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
        return maybe_curry_xz_metaop(lifted.into_iter().next().unwrap());
    }
    // A standalone X/Z meta-op element with Whatever operands curries into a
    // WhateverCode (e.g. `(* X+ *, 5)`); list-operand metaops were already folded
    // by the lift above and left as plain (extending) MetaOps.
    let lifted = lifted.into_iter().map(maybe_curry_xz_metaop).collect();
    Expr::ArrayLiteral(lifted)
}

/// Lift a top-level list-infix meta-op (`Z`/`X`) or `minmax` across an
/// unparenthesized listop argument list. Raku's list-infix operators are LOOSER
/// than the comma separating listop arguments, so `say 100, 200 Z+ 42, 23` is
/// `say((100, 200) Z+ (42, 23))` — the whole comma level is one operand — not
/// `say(100, (200 Z+ (42, 23)))`. The per-argument parse leaves the meta-op with
/// only its immediately-preceding element as its left operand (and the comma
/// tail absorbed on the right); this reuses the same columnar lift the
/// parenthesized-list finalizer applies (`finalize_paren_list`) to pull the
/// leading arguments into the left operand.
///
/// Analogous to [`try_parse_sequence_arg_list`], which does the same absorption
/// for the sequence operator (`...`). Returns an ordinary comma list untouched
/// (no top-level meta-op/minmax), so the caller keeps its per-argument handling.
pub(crate) fn lift_list_infix_in_arg_list(items: Vec<Expr>) -> Vec<Expr> {
    let has_metaop = items.iter().any(|e| matches!(e, Expr::MetaOp { .. }));
    if !has_metaop && lift_minmax_in_paren_list(&items).is_none() {
        return items;
    }
    let lifted = lift_meta_ops_in_paren_list(items);
    if let Some(expr) = lift_minmax_in_paren_list(&lifted) {
        return vec![expr];
    }
    // NOTE: unlike `finalize_paren_list`, this does NOT Whatever-curry the lifted
    // meta-op. The argument-list callers run each element through the full
    // `expression` parser, whose `should_wrap_whatevercode` already curries a
    // genuine standalone `*` (`say * X+ *`) while correctly leaving a `*` that is
    // a list *extender* inside an operand (`<a b c> Z (1 xx *)`) untouched.
    // Currying here would wrap the latter into a WhateverCode by mistake.
    lifted
}

/// Whatever-curry an X/Z meta-op when (and only when) a *standalone* `*` operand
/// makes it a WhateverCode. A `*` that is merely the trailing element of a
/// comma-list operand is a list *extender* (zip/cross repeats the last element),
/// not a curry placeholder, so an `ArrayLiteral` operand never triggers currying.
pub(crate) fn maybe_curry_xz_metaop(expr: Expr) -> Expr {
    if matches!(&expr, Expr::MetaOp { meta, .. } if meta == "X" || meta == "Z")
        && xz_metaop_curries(&expr)
    {
        crate::parser::expr::wrap_whatevercode(&expr)
    } else {
        expr
    }
}

/// True when an X/Z meta-op (possibly a left-nested chain of them) has a
/// standalone Whatever operand that should curry.
fn xz_metaop_curries(expr: &Expr) -> bool {
    if let Expr::MetaOp {
        meta, left, right, ..
    } = expr
        && (meta == "X" || meta == "Z")
    {
        operand_curries(left) || operand_curries(right)
    } else {
        false
    }
}

/// An operand curries when it carries a standalone Whatever placeholder. A
/// comma-list operand (`ArrayLiteral`) is an extender, never a curry. Nested
/// X/Z meta-ops recurse (so `1 X+ * X+ 3` curries through the inner op even
/// though the global `contains_whatever` deliberately ignores X/Z).
fn operand_curries(e: &Expr) -> bool {
    match e {
        Expr::ArrayLiteral(_) => false,
        Expr::MetaOp { meta, .. } if meta == "X" || meta == "Z" => xz_metaop_curries(e),
        _ => {
            contains_whatever(e)
                || matches!(
                    e,
                    Expr::Lambda {
                        is_whatever_code: true,
                        ..
                    } | Expr::AnonSubParams {
                        is_whatever_code: true,
                        ..
                    }
                )
        }
    }
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

pub(crate) fn normalize_sequence_waypoints(expr: Expr) -> Expr {
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

pub(crate) fn normalize_chained_zip_meta(expr: Expr) -> Expr {
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

        let col_to_expr = |col: Vec<Expr>| {
            if col.len() == 1 {
                col.into_iter().next().unwrap()
            } else {
                Expr::ArrayLiteral(col)
            }
        };
        // A standalone `*` column (`(1,2 Z~ * Z~ 3,4)`) makes the whole chain a
        // WhateverCode. We must keep it as a nested meta-op so the curry logic in
        // `maybe_curry_xz_metaop` can wrap it; the lazy multi-arg `zip` call form
        // (used for plain `Z` chains to preserve laziness) is not curryable.
        let has_standalone_whatever = columns_rev
            .iter()
            .any(|col| col.len() == 1 && matches!(col[0], Expr::Whatever));
        let lifted_expr = if *meta == "Z" && columns_rev.len() > 2 && !has_standalone_whatever {
            let mut args: Vec<Expr> = columns_rev.into_iter().map(col_to_expr).collect();
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
            // Left-fold the columns into a nested meta-op chain. This handles X
            // chains of any arity (`a X~ b X~ c` => `(a X~ b) X~ c`, previously
            // the trailing columns were dropped) and curryable Z chains alike.
            let mut iter = columns_rev.into_iter().map(col_to_expr);
            let mut acc = iter.next().unwrap_or(Expr::ArrayLiteral(Vec::new()));
            for col in iter {
                acc = Expr::MetaOp {
                    meta: meta.clone(),
                    op: op.clone(),
                    left: Box::new(acc),
                    right: Box::new(col),
                };
            }
            acc
        };

        let mut result = vec![lifted_expr];
        result.extend(items[idx + 1..].to_vec());
        return result;
    }
    items
}

/// Try to parse a sequence operator (...) inside a paren expression.
/// If the input starts with ... or ...^, treat all collected items as seeds.
pub(crate) fn try_parse_sequence_in_paren<'a>(
    input: &'a str,
    seeds: &[Expr],
) -> Option<PResult<'a, Expr>> {
    // `is_left_excl` marks the `^...` / `^...^` left-exclusive forms, which
    // generate the same series and then drop the first element (`.skip(1)`).
    let (is_excl, is_left_excl, rest) = if let Some(stripped) = input.strip_prefix("^...^") {
        (true, true, stripped)
    } else if input.starts_with("^...") && !input.starts_with("^....") {
        (false, true, &input[4..])
    } else if let Some(stripped) = input.strip_prefix("...^") {
        (true, false, stripped)
    } else if let Some(stripped) = input.strip_prefix("\u{2026}^") {
        (true, false, stripped)
    } else if input.starts_with("...") && !input.starts_with("....") {
        (false, false, &input[3..])
    } else if let Some(stripped) = input.strip_prefix('\u{2026}') {
        (false, false, stripped)
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
        // Left-exclusive sequence: drop the first generated element (stays lazy).
        let seq = if is_left_excl {
            Expr::MethodCall {
                target: Box::new(seq),
                name: crate::symbol::Symbol::intern("skip"),
                args: vec![Expr::Literal(crate::value::Value::int(1))],
                modifier: None,
                quoted: false,
            }
        } else {
            seq
        };
        Ok((r, seq))
    })();
    Some(result)
}

pub(crate) fn starts_with_sequence_op(input: &str) -> bool {
    input.starts_with("...")
        || input.starts_with('\u{2026}')
        // Left-exclusive sequence (`^...` / `^...^`); the `^...^` case also starts
        // with `^...`, so this single prefix covers both.
        || input.starts_with("^...")
}

fn maybe_wrap_whatever(expr: Expr) -> Expr {
    if contains_whatever(&expr) && !matches!(expr, Expr::Whatever) {
        wrap_whatevercode(&expr)
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
    let (is_excl, is_left_excl, rest) = if let Some(s) = input.strip_prefix("^...^") {
        (true, true, s)
    } else if input.starts_with("^...") && !input.starts_with("^....") {
        (false, true, &input[4..])
    } else if let Some(s) = input.strip_prefix("...^") {
        (true, false, s)
    } else if let Some(s) = input.strip_prefix("\u{2026}^") {
        (true, false, s)
    } else if input.starts_with("...") && !input.starts_with("....") {
        (false, false, &input[3..])
    } else if let Some(s) = input.strip_prefix('\u{2026}') {
        (false, false, s)
    } else {
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
    };
    // `^...` / `^...^` drop the seed element from the generated series.
    let seq = if is_left_excl {
        Expr::MethodCall {
            target: Box::new(seq),
            name: crate::symbol::Symbol::intern("skip"),
            args: vec![Expr::Literal(crate::value::Value::int(1))],
            modifier: None,
            quoted: false,
        }
    } else {
        seq
    };
    Ok((rest, seq))
}

/// Try to parse an inline statement modifier inside parenthesized expression.
/// Handles: ($_ with data), (expr if cond), (expr for list), etc.
pub(crate) fn try_inline_modifier<'a>(input: &'a str, expr: Expr) -> Option<PResult<'a, Expr>> {
    use crate::parser::stmt::modifier::parse_statement_modifier;
    // Check if the input starts with a modifier keyword
    let modifier_keywords = [
        "if", "unless", "with", "without", "for", "while", "until", "given", "when",
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
