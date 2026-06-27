mod operators;
mod postfix;
pub(crate) mod precedence;
mod precedence_meta_ops;
mod whatever;
mod whatever_replace;
mod whatever_wrap;

#[cfg(test)]
mod tests;
#[cfg(test)]
mod tests_errors;
#[cfg(test)]
mod tests_meta;
#[cfg(test)]
mod tests_postfix;

use super::memo::{MemoEntry, MemoStats, ParseMemo};
use super::parse_result::PResult;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::helpers::ws;
pub(in crate::parser) use postfix::postfix_expr_continue;
pub(in crate::parser) use postfix::{QuotedMethodName, parse_quoted_method_name};
use precedence::ternary;

// Re-exports for WhateverCode detection (`whatever.rs`).
pub(in crate::parser) use whatever::should_wrap_whatevercode;
pub(super) use whatever::{contains_whatever, is_whatever};
pub(crate) use whatever::{count_whatever, expr_contains_topic, exprs_structurally_eq};

// Re-exports for WhateverCode wrapping (`whatever_wrap.rs`).
pub(in crate::parser) use whatever_wrap::wrap_whatevercode;
use whatever_wrap::{try_wrap_whatevercode_call_chain, wrap_composition_operands};

// Re-exports for WhateverCode body construction (`whatever_replace.rs`).
pub(crate) use whatever_replace::{replace_whatever_numbered, replace_whatever_single};

thread_local! {
    static EXPR_MEMO_TLS: RefCell<HashMap<(usize, usize), MemoEntry<Expr>>> = RefCell::new(HashMap::new());
    static EXPR_MEMO_STATS_TLS: RefCell<MemoStats> = RefCell::new(MemoStats::default());
}

static EXPR_MEMO: ParseMemo<Expr> = ParseMemo::new(&EXPR_MEMO_TLS, &EXPR_MEMO_STATS_TLS);

pub(super) fn reset_expression_memo() {
    EXPR_MEMO.reset();
}

pub(super) fn expression_memo_stats() -> (usize, usize, usize) {
    EXPR_MEMO.stats()
}

pub(super) fn expression(input: &str) -> PResult<'_, Expr> {
    if let Some(cached) = EXPR_MEMO.get(input) {
        return cached;
    }
    let result = (|| {
        if (input.starts_with("qx") || input.starts_with("qqx"))
            && let Ok((rest, expr)) = super::primary::string::qx_string(input)
            && rest.trim_start().is_empty()
        {
            return Ok((rest, expr));
        }
        let (rest, mut expr) = ternary(input)?;
        // Handle => (fat arrow / pair constructor) - lower precedence than ternary
        let (r, _) = ws(rest)?;
        if r.starts_with("=>") && !r.starts_with("==>") {
            let r = &r[2..];
            let (r, _) = ws(r)?;
            let (r, value) = parse_fat_arrow_value(r)?;
            // Auto-quote bareword on LHS of => only for plain barewords.
            // Parenthesized forms like `(Mu) => 4` must preserve the original value key.
            let consumed = &input[..input.len() - rest.len()];
            let is_bareword = matches!(&expr, Expr::BareWord(_));
            let left = match expr {
                Expr::BareWord(ref name) if !consumed.trim_start().starts_with('(') => {
                    Expr::Literal(Value::str(name.clone()))
                }
                _ => expr,
            };
            let pair = Expr::Binary {
                left: Box::new(left),
                op: TokenKind::FatArrow,
                right: Box::new(value),
            };
            // Non-bareword keys (quoted strings, expressions) produce positional pairs,
            // not named arguments. Bareword keys (a => 3) are named arguments.
            let result = if is_bareword {
                pair
            } else {
                Expr::PositionalPair(Box::new(pair))
            };
            return Ok((r, result));
        }
        expr = wrap_composition_operands(expr);
        // Wrap WhateverCode expressions in a lambda, but not bare * (Whatever).
        // Smartmatch handles RHS WhateverCode in precedence parsing, and LHS should
        // remain a normal expression (e.g. `(*) ~~ HyperWhatever:D`).
        if should_wrap_whatevercode(&expr) {
            expr = match expr {
                Expr::CallOn { target, args }
                    if should_wrap_whatevercode(&target) && !args.iter().any(contains_whatever) =>
                {
                    Expr::CallOn {
                        target: Box::new(wrap_whatevercode(&target)),
                        args,
                    }
                }
                ref e if try_wrap_whatevercode_call_chain(e).is_some() => {
                    try_wrap_whatevercode_call_chain(&expr).unwrap()
                }
                other => wrap_whatevercode(&other),
            };
        }
        Ok((rest, expr))
    })();
    EXPR_MEMO.store(input, &result);
    result
}

/// Like [`expression`] but does NOT consume a trailing simple/compound
/// assignment. Used for signature `where` constraints so that a following `=`
/// introduces the parameter default instead of being parsed as an assignment to
/// the constraint expression. Applies the same fat-arrow and WhateverCode
/// wrapping as [`expression`].
pub(in crate::parser) fn expression_no_assign(input: &str) -> PResult<'_, Expr> {
    let (rest, mut expr) = precedence::ternary_no_assign(input)?;
    let (r, _) = ws(rest)?;
    if r.starts_with("=>") && !r.starts_with("==>") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, value) = parse_fat_arrow_value(r)?;
        let consumed = &input[..input.len() - rest.len()];
        let is_bareword = matches!(&expr, Expr::BareWord(_));
        let left = match expr {
            Expr::BareWord(ref name) if !consumed.trim_start().starts_with('(') => {
                Expr::Literal(Value::str(name.clone()))
            }
            _ => expr,
        };
        let pair = Expr::Binary {
            left: Box::new(left),
            op: TokenKind::FatArrow,
            right: Box::new(value),
        };
        let result = if is_bareword {
            pair
        } else {
            Expr::PositionalPair(Box::new(pair))
        };
        return Ok((r, result));
    }
    expr = wrap_composition_operands(expr);
    if should_wrap_whatevercode(&expr) {
        expr = match expr {
            Expr::CallOn { target, args }
                if should_wrap_whatevercode(&target) && !args.iter().any(contains_whatever) =>
            {
                Expr::CallOn {
                    target: Box::new(wrap_whatevercode(&target)),
                    args,
                }
            }
            ref e if try_wrap_whatevercode_call_chain(e).is_some() => {
                try_wrap_whatevercode_call_chain(&expr).unwrap()
            }
            other => wrap_whatevercode(&other),
        };
    }
    Ok((rest, expr))
}

pub(in crate::parser) fn expression_no_sequence(input: &str) -> PResult<'_, Expr> {
    // Same as expression but skip memo and don't include sequence
    let (rest, mut expr) = precedence::ternary_mode(input, operators::ExprMode::NoSequence)?;
    let (r, _) = ws(rest)?;
    if r.starts_with("=>") && !r.starts_with("==>") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, value) = parse_fat_arrow_value(r)?;
        let consumed = &input[..input.len() - rest.len()];
        let is_bareword = matches!(&expr, Expr::BareWord(_));
        let left = match expr {
            Expr::BareWord(ref name) if !consumed.trim_start().starts_with('(') => {
                Expr::Literal(Value::str(name.clone()))
            }
            _ => expr,
        };
        let pair = Expr::Binary {
            left: Box::new(left),
            op: TokenKind::FatArrow,
            right: Box::new(value),
        };
        let result = if is_bareword {
            pair
        } else {
            Expr::PositionalPair(Box::new(pair))
        };
        return Ok((r, result));
    }
    expr = wrap_composition_operands(expr);
    // Keep bare `*` as Whatever (Inf). Only wrap true WhateverCode expressions.
    if should_wrap_whatevercode(&expr) {
        expr = match expr {
            // Preserve call-on semantics for forms like *²(3):
            // wrap the callable target as WhateverCode, then invoke it.
            Expr::CallOn { target, args }
                if should_wrap_whatevercode(&target) && !args.iter().any(contains_whatever) =>
            {
                Expr::CallOn {
                    target: Box::new(wrap_whatevercode(&target)),
                    args,
                }
            }
            other => wrap_whatevercode(&other),
        };
    }
    Ok((rest, expr))
}

/// Parse a listop argument expression: full expression excluding sequence (...),
/// feed (==>/<==), and comma operators. This matches Raku's "list prefix" precedence
/// where `grep $_ == 1, 1, 2, 3` parses as `grep(($_ == 1), 1, 2, 3)` and
/// `@a ==> grep {...} ==> @b` keeps the feed operators outside of grep's arguments.
#[allow(dead_code)]
pub(in crate::parser) fn listop_arg_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, mut expr) = precedence::ternary_mode(input, operators::ExprMode::NoSequenceNoFeed)?;
    let (r, _) = ws(rest)?;
    if r.starts_with("=>") && !r.starts_with("==>") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, value) = parse_fat_arrow_value(r)?;
        let consumed = &input[..input.len() - rest.len()];
        let is_bareword = matches!(&expr, Expr::BareWord(_));
        let left = match expr {
            Expr::BareWord(ref name) if !consumed.trim_start().starts_with('(') => {
                Expr::Literal(Value::str(name.clone()))
            }
            _ => expr,
        };
        let pair = Expr::Binary {
            left: Box::new(left),
            op: TokenKind::FatArrow,
            right: Box::new(value),
        };
        let result = if is_bareword {
            pair
        } else {
            Expr::PositionalPair(Box::new(pair))
        };
        return Ok((r, result));
    }
    expr = wrap_composition_operands(expr);
    if should_wrap_whatevercode(&expr) {
        expr = match expr {
            Expr::CallOn { target, args }
                if should_wrap_whatevercode(&target) && !args.iter().any(contains_whatever) =>
            {
                Expr::CallOn {
                    target: Box::new(wrap_whatevercode(&target)),
                    args,
                }
            }
            other => wrap_whatevercode(&other),
        };
    }
    Ok((rest, expr))
}

pub(in crate::parser) fn call_arg_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, mut expr) = precedence::call_arg_expr(input)?;
    let (r, _) = ws(rest)?;
    if r.starts_with("=>") && !r.starts_with("==>") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, value) = parse_fat_arrow_value(r)?;
        let consumed = &input[..input.len() - rest.len()];
        let is_bareword = matches!(&expr, Expr::BareWord(_));
        let left = match expr {
            Expr::BareWord(ref name) if !consumed.trim_start().starts_with('(') => {
                Expr::Literal(Value::str(name.clone()))
            }
            _ => expr,
        };
        let pair = Expr::Binary {
            left: Box::new(left),
            op: TokenKind::FatArrow,
            right: Box::new(value),
        };
        let result = if is_bareword {
            pair
        } else {
            Expr::PositionalPair(Box::new(pair))
        };
        return Ok((r, result));
    }
    expr = wrap_composition_operands(expr);
    if should_wrap_whatevercode(&expr) {
        expr = match expr {
            Expr::CallOn { target, args }
                if should_wrap_whatevercode(&target) && !args.iter().any(contains_whatever) =>
            {
                Expr::CallOn {
                    target: Box::new(wrap_whatevercode(&target)),
                    args,
                }
            }
            other => wrap_whatevercode(&other),
        };
    }
    Ok((rest, expr))
}

pub(in crate::parser) fn parse_fat_arrow_value(input: &str) -> PResult<'_, Expr> {
    // Fat arrow (=>) has lower precedence than ternary (??!!),
    // so the RHS should be parsed at ternary level.
    let (rest, value) = ternary(input)?;
    let (r, _) = ws(rest)?;
    if r.starts_with("=>") && !r.starts_with("==>") {
        let r = &r[2..];
        let (r, _) = ws(r)?;
        let (r, right) = parse_fat_arrow_value(r)?;
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(value),
                op: TokenKind::FatArrow,
                right: Box::new(right),
            },
        ));
    }
    Ok((rest, value))
}

pub(in crate::parser) fn term_expr(input: &str) -> PResult<'_, Expr> {
    postfix::prefix_expr(input)
}
