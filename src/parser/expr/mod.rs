mod operators;
mod postfix;
mod precedence;

use super::memo::{MemoEntry, MemoStats, ParseMemo};
use super::parse_result::PResult;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::helpers::ws;
pub(in crate::parser) use postfix::postfix_expr_continue;
use precedence::{or_expr, ternary};

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
                    Expr::Literal(Value::Str(name.clone()))
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
                other => wrap_whatevercode(&other),
            };
        }
        Ok((rest, expr))
    })();
    EXPR_MEMO.store(input, &result);
    result
}

pub(super) fn expression_no_sequence(input: &str) -> PResult<'_, Expr> {
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
                Expr::Literal(Value::Str(name.clone()))
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

fn parse_fat_arrow_value(input: &str) -> PResult<'_, Expr> {
    let (rest, value) = or_expr(input)?;
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

fn should_wrap_whatevercode(expr: &Expr) -> bool {
    if !contains_whatever(expr) || is_whatever(expr) {
        return false;
    }
    match expr {
        Expr::Binary {
            op: TokenKind::SmartMatch | TokenKind::BangTilde,
            ..
        } => false,
        Expr::Binary {
            op: TokenKind::Ident(name),
            ..
        } if name == "o" => false,
        _ => true,
    }
}

fn wrap_composition_operands(expr: Expr) -> Expr {
    match expr {
        Expr::Binary { left, op, right } => {
            let left = wrap_composition_operands(*left);
            let right = wrap_composition_operands(*right);
            if matches!(&op, TokenKind::Ident(name) if name == "o") {
                let mut bare_count = 0usize;
                if is_whatever(&left) {
                    bare_count += 1;
                }
                if is_whatever(&right) {
                    bare_count += 1;
                }
                if bare_count > 0 {
                    let mut params = Vec::new();
                    let mut param_defs = Vec::new();
                    let left_expr = if is_whatever(&left) {
                        let name = format!("__wc_{}", params.len());
                        params.push(name.clone());
                        param_defs.push(make_wc_param(name.clone()));
                        Expr::Var(name)
                    } else if should_wrap_whatevercode(&left) {
                        wrap_whatevercode(&left)
                    } else {
                        left
                    };
                    let right_expr = if is_whatever(&right) {
                        let name = format!("__wc_{}", params.len());
                        params.push(name.clone());
                        param_defs.push(make_wc_param(name.clone()));
                        Expr::Var(name)
                    } else if should_wrap_whatevercode(&right) {
                        wrap_whatevercode(&right)
                    } else {
                        right
                    };
                    let body_expr = Expr::Binary {
                        left: Box::new(left_expr),
                        op,
                        right: Box::new(right_expr),
                    };
                    if params.len() == 1 {
                        return Expr::Lambda {
                            param: params[0].clone(),
                            body: vec![Stmt::Expr(body_expr)],
                        };
                    }
                    return Expr::AnonSubParams {
                        params,
                        param_defs,
                        return_type: None,
                        body: vec![Stmt::Expr(body_expr)],
                        is_rw: false,
                    };
                }
                let left_wrapped = if should_wrap_whatevercode(&left) {
                    wrap_whatevercode(&left)
                } else {
                    left
                };
                let right_wrapped = if should_wrap_whatevercode(&right) {
                    wrap_whatevercode(&right)
                } else {
                    right
                };
                Expr::Binary {
                    left: Box::new(left_wrapped),
                    op,
                    right: Box::new(right_wrapped),
                }
            } else {
                Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                }
            }
        }
        Expr::Unary { op, expr } => Expr::Unary {
            op,
            expr: Box::new(wrap_composition_operands(*expr)),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::MethodCall {
            target: Box::new(wrap_composition_operands(*target)),
            name,
            args: args.into_iter().map(wrap_composition_operands).collect(),
            modifier,
            quoted,
        },
        Expr::CallOn { target, args } => Expr::CallOn {
            target: Box::new(wrap_composition_operands(*target)),
            args: args.into_iter().map(wrap_composition_operands).collect(),
        },
        Expr::Index { target, index } => Expr::Index {
            target: Box::new(wrap_composition_operands(*target)),
            index: Box::new(wrap_composition_operands(*index)),
        },
        other => other,
    }
}

fn make_wc_param(name: String) -> crate::ast::ParamDef {
    crate::ast::ParamDef {
        name,
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        slurpy: false,
        double_slurpy: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
    }
}

pub(super) fn or_expr_pub(input: &str) -> PResult<'_, Expr> {
    or_expr(input)
}

fn is_whatever(expr: &Expr) -> bool {
    matches!(expr, Expr::Whatever)
}

fn contains_whatever(expr: &Expr) -> bool {
    match expr {
        e if is_whatever(e) => true,
        // Don't treat * inside range/sequence operators as WhateverCode
        Expr::Binary {
            op:
                TokenKind::DotDot
                | TokenKind::DotDotCaret
                | TokenKind::CaretDotDot
                | TokenKind::CaretDotDotCaret
                | TokenKind::DotDotDot
                | TokenKind::DotDotDotCaret,
            ..
        } => false,
        Expr::Binary { left, right, .. } => contains_whatever(left) || contains_whatever(right),
        Expr::Unary { expr, .. } => contains_whatever(expr),
        // Pseudo-methods (.WHAT, .WHO, .HOW, etc.) are always evaluated immediately
        // on Whatever, they don't create WhateverCode. e.g. *.WHAT returns (Whatever).
        Expr::MethodCall { target, name, .. }
            if matches!(
                name.as_str(),
                "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "DEFINITE" | "VAR"
            ) && is_whatever(target) =>
        {
            false
        }
        Expr::MethodCall { target, .. } => contains_whatever(target),
        Expr::CallOn { target, args } => {
            contains_whatever(target) || args.iter().any(contains_whatever)
        }
        // Only check target, not index: @a[*-1] should NOT make the whole expr a WhateverCode.
        // The [*-1] subscript handles its own WhateverCode wrapping.
        Expr::Index { target, .. } => contains_whatever(target),
        _ => false,
    }
}

/// Count the number of distinct Whatever (`*`) placeholders in an expression.
fn count_whatever(expr: &Expr) -> usize {
    match expr {
        e if is_whatever(e) => 1,
        Expr::Binary {
            left,
            op: TokenKind::AndAnd,
            right,
        } => {
            if let (
                Expr::Binary {
                    left: ll,
                    right: lr,
                    ..
                },
                Expr::Binary {
                    left: rl,
                    right: rr,
                    ..
                },
            ) = (left.as_ref(), right.as_ref())
                && is_whatever(lr)
                && is_whatever(rl)
            {
                return count_whatever(ll) + 1 + count_whatever(rr);
            }
            count_whatever(left) + count_whatever(right)
        }
        Expr::Binary {
            op:
                TokenKind::DotDot
                | TokenKind::DotDotCaret
                | TokenKind::CaretDotDot
                | TokenKind::CaretDotDotCaret
                | TokenKind::DotDotDot
                | TokenKind::DotDotDotCaret,
            ..
        } => 0,
        Expr::Binary { left, right, .. } => count_whatever(left) + count_whatever(right),
        Expr::Unary { expr, .. } => count_whatever(expr),
        Expr::MethodCall { target, .. } => count_whatever(target),
        Expr::CallOn { target, args } => {
            count_whatever(target) + args.iter().map(count_whatever).sum::<usize>()
        }
        // Only check target, not index (subscript handles its own WhateverCode)
        Expr::Index { target, .. } => count_whatever(target),
        _ => 0,
    }
}

/// Replace Whatever expressions with numbered parameter variables.
/// `counter` tracks the next parameter index to assign.
fn replace_whatever_numbered(expr: &Expr, counter: &mut usize) -> Expr {
    match expr {
        e if is_whatever(e) => {
            let var_name = format!("__wc_{}", counter);
            *counter += 1;
            Expr::Var(var_name)
        }
        Expr::Binary {
            left,
            op: TokenKind::AndAnd,
            right,
        } => {
            if let (
                Expr::Binary {
                    left: ll,
                    op: lop,
                    right: lr,
                },
                Expr::Binary {
                    left: rl,
                    op: rop,
                    right: rr,
                },
            ) = (left.as_ref(), right.as_ref())
                && is_whatever(lr)
                && is_whatever(rl)
            {
                let shared = format!("__wc_{}", counter);
                *counter += 1;
                return Expr::Binary {
                    left: Box::new(Expr::Binary {
                        left: Box::new(replace_whatever_numbered(ll, counter)),
                        op: lop.clone(),
                        right: Box::new(Expr::Var(shared.clone())),
                    }),
                    op: TokenKind::AndAnd,
                    right: Box::new(Expr::Binary {
                        left: Box::new(Expr::Var(shared)),
                        op: rop.clone(),
                        right: Box::new(replace_whatever_numbered(rr, counter)),
                    }),
                };
            }
            Expr::Binary {
                left: Box::new(replace_whatever_numbered(left, counter)),
                op: TokenKind::AndAnd,
                right: Box::new(replace_whatever_numbered(right, counter)),
            }
        }
        // Unwrap a nested WhateverCode lambda: reuse its body with renumbered params
        Expr::Lambda { param, body } if param == "_" => {
            // Single-param WhateverCode: rename $_ to the next numbered param
            let var_name = format!("__wc_{}", counter);
            *counter += 1;
            if let Some(Stmt::Expr(e)) = body.first() {
                rename_var(e, "_", &var_name)
            } else {
                Expr::Var(var_name)
            }
        }
        Expr::AnonSubParams { params, body, .. }
            if params.iter().all(|p| p.starts_with("__wc_")) =>
        {
            // Multi-param WhateverCode: renumber all params
            let mut renames = Vec::new();
            for old_name in params {
                let new_name = format!("__wc_{}", counter);
                *counter += 1;
                renames.push((old_name.clone(), new_name));
            }
            let mut body_expr = if let Some(Stmt::Expr(e)) = body.first() {
                e.clone()
            } else {
                return expr.clone();
            };
            for (old, new) in &renames {
                body_expr = rename_var(&body_expr, old, new);
            }
            body_expr
        }
        Expr::Binary { left, op, right } => Expr::Binary {
            left: Box::new(replace_whatever_numbered(left, counter)),
            op: op.clone(),
            right: Box::new(replace_whatever_numbered(right, counter)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: op.clone(),
            expr: Box::new(replace_whatever_numbered(expr, counter)),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::MethodCall {
            target: Box::new(replace_whatever_numbered(target, counter)),
            name: name.clone(),
            args: args.clone(),
            modifier: *modifier,
            quoted: *quoted,
        },
        Expr::CallOn { target, args } => Expr::CallOn {
            target: Box::new(replace_whatever_numbered(target, counter)),
            args: args
                .iter()
                .map(|a| replace_whatever_numbered(a, counter))
                .collect(),
        },
        Expr::Index { target, index } => Expr::Index {
            target: Box::new(replace_whatever_numbered(target, counter)),
            index: index.clone(),
        },
        _ => expr.clone(),
    }
}

/// Rename a variable in an expression tree.
fn rename_var(expr: &Expr, old_name: &str, new_name: &str) -> Expr {
    match expr {
        Expr::Var(name) if name == old_name => Expr::Var(new_name.to_string()),
        Expr::Binary { left, op, right } => Expr::Binary {
            left: Box::new(rename_var(left, old_name, new_name)),
            op: op.clone(),
            right: Box::new(rename_var(right, old_name, new_name)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: op.clone(),
            expr: Box::new(rename_var(expr, old_name, new_name)),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::MethodCall {
            target: Box::new(rename_var(target, old_name, new_name)),
            name: name.clone(),
            args: args
                .iter()
                .map(|a| rename_var(a, old_name, new_name))
                .collect(),
            modifier: *modifier,
            quoted: *quoted,
        },
        Expr::CallOn { target, args } => Expr::CallOn {
            target: Box::new(rename_var(target, old_name, new_name)),
            args: args
                .iter()
                .map(|a| rename_var(a, old_name, new_name))
                .collect(),
        },
        Expr::Index { target, index } => Expr::Index {
            target: Box::new(rename_var(target, old_name, new_name)),
            index: Box::new(rename_var(index, old_name, new_name)),
        },
        _ => expr.clone(),
    }
}

/// Build a WhateverCode lambda from an expression containing Whatever placeholders.
fn wrap_whatevercode(expr: &Expr) -> Expr {
    if let Expr::CallOn { target, args } = expr
        && should_wrap_whatevercode(target)
        && !args.iter().any(contains_whatever)
    {
        return Expr::CallOn {
            target: Box::new(wrap_whatevercode(target)),
            args: args.clone(),
        };
    }

    let wc_count = count_whatever(expr);

    if wc_count <= 1 {
        // Single-arg: use Lambda with param "_" for backward compat
        // Use a special single-arg replacer that maps * and nested single-arg WC to $_
        let body_expr = replace_whatever_single(expr);
        Expr::Lambda {
            param: "_".to_string(),
            body: vec![Stmt::Expr(body_expr)],
        }
    } else {
        // Multi-arg: use AnonSubParams with numbered params
        let mut counter = 0;
        let body_expr = replace_whatever_numbered(expr, &mut counter);
        let params: Vec<String> = (0..counter).map(|i| format!("__wc_{}", i)).collect();
        Expr::AnonSubParams {
            params: params.clone(),
            param_defs: params
                .iter()
                .map(|name| crate::ast::ParamDef {
                    name: name.clone(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: false,
                    sigilless: false,
                    type_constraint: None,
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    double_slurpy: false,
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                    shape_constraints: None,
                })
                .collect(),
            return_type: None,
            body: vec![Stmt::Expr(body_expr)],
            is_rw: false,
        }
    }
}

/// Replace Whatever and nested single-arg WhateverCode with $_ (for single-arg wrapping).
fn replace_whatever_single(expr: &Expr) -> Expr {
    match expr {
        e if is_whatever(e) => Expr::Var("_".to_string()),
        // A nested single-arg WhateverCode: unwrap and reuse body (already uses $_)
        Expr::Lambda { param, body } if param == "_" => {
            if let Some(Stmt::Expr(e)) = body.first() {
                e.clone()
            } else {
                Expr::Var("_".to_string())
            }
        }
        Expr::Binary { left, op, right } => Expr::Binary {
            left: Box::new(replace_whatever_single(left)),
            op: op.clone(),
            right: Box::new(replace_whatever_single(right)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: op.clone(),
            expr: Box::new(replace_whatever_single(expr)),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::MethodCall {
            target: Box::new(replace_whatever_single(target)),
            name: name.clone(),
            args: args.clone(),
            modifier: *modifier,
            quoted: *quoted,
        },
        Expr::CallOn { target, args } => Expr::CallOn {
            target: Box::new(replace_whatever_single(target)),
            args: args.iter().map(replace_whatever_single).collect(),
        },
        Expr::Index { target, index } => Expr::Index {
            target: Box::new(replace_whatever_single(target)),
            index: index.clone(),
        },
        _ => expr.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::operators::*;
    use super::*;

    #[test]
    fn parse_binary_add() {
        let (rest, expr) = expression("1 + 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::Plus,
                ..
            }
        ));
    }

    #[test]
    fn parse_comparison() {
        let (rest, expr) = expression("$x == $y").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::EqEq,
                ..
            }
        ));
    }

    #[test]
    fn parse_not_smartmatch() {
        let (rest, expr) = expression("\"abc\" !~~ \"xyz\"").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::BangTilde,
                ..
            }
        ));
    }

    #[test]
    fn parse_method_call() {
        let (rest, expr) = expression("$x.defined").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::MethodCall { .. }));
    }

    #[test]
    fn parse_dynamic_quoted_method_call() {
        let (rest, expr) = expression("$_.\"$k\"()").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::DynamicMethodCall { .. }));
    }

    #[test]
    fn parse_dynamic_sigiled_method_call() {
        let (rest, expr) = expression("$o.$meth").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::DynamicMethodCall { .. }));
    }

    #[test]
    fn parse_dynamic_sigiled_method_call_with_args() {
        let (rest, expr) = expression("$o.$meth(1, 2)").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::DynamicMethodCall { args, .. } => assert_eq!(args.len(), 2),
            _ => panic!("expected dynamic method call"),
        }
    }

    #[test]
    fn parse_ternary() {
        let (rest, expr) = expression("$x ?? 1 !! 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Ternary { .. }));
    }

    #[test]
    fn parse_junctive_op_all() {
        assert_eq!(parse_junctive_op("?|"), Some((JunctiveOp::Or, 2)));
        assert_eq!(parse_junctive_op("?&"), Some((JunctiveOp::And, 2)));
        assert_eq!(parse_junctive_op("?^"), Some((JunctiveOp::Xor, 2)));
        assert_eq!(parse_junctive_op("?"), None);
        assert_eq!(parse_junctive_op("??"), None);
    }

    #[test]
    fn parse_or_or_op_all() {
        assert_eq!(parse_or_or_op("||"), Some((LogicalOp::OrOr, 2)));
        assert_eq!(parse_or_or_op("//"), Some((LogicalOp::DefinedOr, 2)));
        assert_eq!(parse_or_or_op("///"), None);
        assert_eq!(parse_or_or_op("|"), None);
    }

    #[test]
    fn parse_and_and_op_all() {
        assert_eq!(parse_and_and_op("&&"), Some((LogicalOp::AndAnd, 2)));
        assert_eq!(parse_and_and_op("&"), None);
    }

    #[test]
    fn parse_word_logical_op_all() {
        assert_eq!(parse_word_logical_op("or "), Some((LogicalOp::Or, 2)));
        assert_eq!(parse_word_logical_op("and "), Some((LogicalOp::And, 3)));
        assert_eq!(parse_word_logical_op("or_foo"), None);
        assert_eq!(parse_word_logical_op("and_bar"), None);
        assert_eq!(parse_word_logical_op("oracle"), None);
    }

    #[test]
    fn parse_logical_operators_in_expr() {
        // Test word forms
        let (rest, expr) = expression("1 or 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::OrWord,
                ..
            }
        ));

        let (rest, expr) = expression("1 and 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::AndAnd,
                ..
            }
        ));

        // Test symbolic forms
        let (rest, expr) = expression("1 || 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::OrOr,
                ..
            }
        ));

        let (rest, expr) = expression("1 && 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::AndAnd,
                ..
            }
        ));

        let (rest, expr) = expression("1 // 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Binary {
                op: TokenKind::SlashSlash,
                ..
            }
        ));
    }

    #[test]
    fn parse_junctive_operators_in_expr() {
        let inputs = [("1 ?| 2", "?|"), ("1 ?& 2", "?&"), ("1 ?^ 2", "?^")];

        for (input, expected_op) in inputs {
            let (rest, expr) = expression(input).unwrap();
            assert_eq!(rest, "");
            if let Expr::Binary { op, .. } = expr {
                match op {
                    TokenKind::Ident(s) => assert_eq!(s, expected_op),
                    _ => panic!("Expected Ident token for {}", expected_op),
                }
            } else {
                panic!("Expected Binary expression");
            }
        }
    }

    #[test]
    fn parse_fat_arrow_chains_right_associatively() {
        let (rest, expr) = expression("1 => 2 => 3 => 4").unwrap();
        assert_eq!(rest, "");
        // Non-bareword key => outermost wrapped in PositionalPair
        // Inner chain pairs are raw Binary (not wrapped)
        match expr {
            Expr::PositionalPair(inner) => match *inner {
                Expr::Binary {
                    op: TokenKind::FatArrow,
                    left,
                    right,
                } => {
                    assert!(matches!(*left, Expr::Literal(Value::Int(1))));
                    match *right {
                        Expr::Binary {
                            op: TokenKind::FatArrow,
                            left: right_left,
                            right: right_right,
                        } => {
                            assert!(matches!(*right_left, Expr::Literal(Value::Int(2))));
                            match *right_right {
                                Expr::Binary {
                                    op: TokenKind::FatArrow,
                                    left: tail_left,
                                    right: tail_right,
                                } => {
                                    assert!(matches!(*tail_left, Expr::Literal(Value::Int(3))));
                                    assert!(matches!(*tail_right, Expr::Literal(Value::Int(4))));
                                }
                                _ => panic!("expected final fat-arrow pair"),
                            }
                        }
                        _ => panic!("expected nested fat-arrow pair"),
                    }
                }
                _ => panic!("expected fat-arrow pair inside PositionalPair"),
            },
            _ => panic!("expected PositionalPair for non-bareword key"),
        }
    }

    #[test]
    fn parse_fat_arrow_chain_in_call_arguments() {
        let (rest, expr) = expression("is($list, 1 => 2 => 3 => 4, \"x\")").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args } => {
                assert_eq!(name, "is");
                assert!(args.len() >= 3);
                // Non-bareword key => outermost PositionalPair wrapping
                // Inner chain pairs are raw Binary
                match &args[1] {
                    Expr::PositionalPair(inner) => match inner.as_ref() {
                        Expr::Binary {
                            op: TokenKind::FatArrow,
                            right,
                            ..
                        } => {
                            assert!(matches!(
                                right.as_ref(),
                                Expr::Binary {
                                    op: TokenKind::FatArrow,
                                    ..
                                }
                            ));
                        }
                        _ => panic!("expected chained fat-arrow in second argument"),
                    },
                    _ => panic!("expected PositionalPair for non-bareword key"),
                }
            }
            _ => panic!("expected call expression"),
        }
    }

    #[test]
    fn expression_memo_reuses_result() {
        reset_expression_memo();
        let (rest, expr) = expression("1 + 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Binary { .. }));
        let (rest2, expr2) = expression("1 + 2").unwrap();
        assert_eq!(rest2, "");
        assert!(matches!(expr2, Expr::Binary { .. }));
    }

    #[test]
    fn comparison_requires_rhs_expression() {
        let err = expression("1 <").unwrap_err();
        assert!(
            err.message()
                .contains("expression after comparison operator")
        );
    }

    #[test]
    fn parse_strict_not_equal_operator() {
        let (rest, expr) = expression("1 !=== 2").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Unary {
                op: TokenKind::Bang,
                expr,
            } => match *expr {
                Expr::Binary {
                    op: TokenKind::EqEqEq,
                    ..
                } => {}
                _ => panic!("Expected !=== to lower to !(===)"),
            },
            _ => panic!("Expected Binary expression"),
        }
    }

    #[test]
    fn parse_negated_comparison_meta_operators() {
        let (rest, expr) = expression("2 !== 3").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Unary {
                op: TokenKind::Bang,
                expr,
            } => match *expr {
                Expr::Binary {
                    op: TokenKind::EqEq,
                    ..
                } => {}
                _ => panic!("Expected !== to lower to !(==)"),
            },
            _ => panic!("Expected Unary expression"),
        }

        let (rest, expr) = expression("$a !eq $b").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Unary {
                op: TokenKind::Bang,
                expr,
            } => match *expr {
                Expr::Binary {
                    op: TokenKind::Ident(op),
                    ..
                } => assert_eq!(op, "eq"),
                _ => panic!("Expected !eq to lower to !(eq)"),
            },
            _ => panic!("Expected Unary expression"),
        }
    }

    #[test]
    fn chained_comparison_requires_rhs_expression() {
        let err = expression("1 < 2 <").unwrap_err();
        assert!(err.message().contains("chained comparison operator"));
    }

    #[test]
    fn range_requires_rhs_expression() {
        let err = expression("1 ..").unwrap_err();
        assert!(err.message().contains("range RHS"));
    }

    #[test]
    fn ternary_requires_then_and_else_expression() {
        let err_then = expression("$x ??").unwrap_err();
        assert!(err_then.message().contains("then-expression"));

        let err_else = expression("$x ?? 1 !!").unwrap_err();
        assert!(err_else.message().contains("else-expression"));
    }

    #[test]
    fn additive_requires_rhs_expression() {
        let err = expression("1 +").unwrap_err();
        assert!(err.message().contains("additive operator"));
    }

    #[test]
    fn parse_postfix_method_requires_name() {
        let err = expression("$x.").unwrap_err();
        assert!(err.message().contains("method name"));
    }

    #[test]
    fn parse_postfix_colon_args_require_expression() {
        let err = expression("$x.foo:").unwrap_err();
        assert!(err.message().contains("expected"));
    }

    #[test]
    fn parse_postfix_angle_index_requires_closing() {
        let err = expression("$x<foo").unwrap_err();
        assert!(err.message().contains("closing '>'"));
    }

    #[test]
    fn parse_postfix_angle_index_zen() {
        let (rest, expr) = expression("$x<>").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Var(ref n) if n == "x"));
    }

    #[test]
    fn parse_postfix_angle_index_zen_values_adverb() {
        let (rest, expr) = expression("%h<>:v").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::MethodCall {
                target, name, args, ..
            } => {
                assert_eq!(name, "values");
                assert!(args.is_empty());
                assert!(matches!(*target, Expr::HashVar(ref n) if n == "h"));
            }
            _ => panic!("expected method call expression"),
        }
    }

    #[test]
    fn parse_postfix_angle_index_multiple_keys() {
        let (rest, expr) = expression("%h<a b c>").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Index { index, .. } => {
                assert!(matches!(*index, Expr::ArrayLiteral(ref items) if items.len() == 3));
            }
            _ => panic!("expected index expression"),
        }
    }

    #[test]
    fn parse_postfix_call_adverb_block_as_arg() {
        let (rest, expr) = expression("foo():{ 42 }").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args } => {
                assert_eq!(name, "foo");
                assert_eq!(args.len(), 2);
                assert!(args.iter().any(|arg| matches!(arg, Expr::AnonSub { .. })));
                assert!(args.iter().any(|arg| matches!(
                    arg,
                    Expr::Binary { left, op: crate::token_kind::TokenKind::FatArrow, .. }
                    if matches!(left.as_ref(), Expr::Literal(crate::value::Value::Str(s)) if s == "__mutsu_test_callsite_line")
                )));
            }
            _ => panic!("expected call expression"),
        }
    }

    #[test]
    fn parse_postfix_call_adverb_colonpairs_as_args() {
        let (rest, expr) = expression("fiddle():x(\"a\"):y").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args } => {
                assert_eq!(name, "fiddle");
                assert_eq!(args.len(), 3);
                assert!(
                    args.iter()
                        .filter(|arg| matches!(arg, Expr::Binary { .. }))
                        .count()
                        >= 3
                );
                assert!(args.iter().any(|arg| matches!(
                    arg,
                    Expr::Binary { left, op: crate::token_kind::TokenKind::FatArrow, .. }
                    if matches!(left.as_ref(), Expr::Literal(crate::value::Value::Str(s)) if s == "__mutsu_test_callsite_line")
                )));
            }
            _ => panic!("expected call expression"),
        }
    }

    #[test]
    fn parse_listop_block_then_colon_args() {
        let (rest, expr) = expression("map { $_ * 2 }: @list").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args } => {
                assert_eq!(name, "map");
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], Expr::AnonSub { .. }));
                assert!(matches!(args[1], Expr::ArrayVar(ref n) if n == "list"));
            }
            _ => panic!("expected call expression"),
        }
    }

    #[test]
    fn parse_method_colon_arg_pointy_with_return_type() {
        let (rest, expr) = expression("@a.map: -> \\x --> Int { x }").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::MethodCall { name, args, .. } => {
                assert_eq!(name, "map");
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Expr::Lambda { ref param, .. } if param == "x"));
            }
            _ => panic!("expected method call expression"),
        }
    }

    #[test]
    fn parse_paren_expr_with_space_dot_method_chain() {
        let (rest, expr) = expression("(^3 .map: -> \\x --> Int { $i++ }).sink").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::MethodCall { name, .. } => assert_eq!(name, "sink"),
            _ => panic!("expected outer method call"),
        }
    }

    #[test]
    fn parse_slip_prefix_with_parenthesized_expr() {
        let (rest, expr) = expression("|(f)").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Unary {
                op: TokenKind::Pipe,
                ..
            }
        ));
    }

    #[test]
    fn parse_slip_prefix_with_reduction_expr() {
        let (rest, expr) = expression("|[\\+] 1..*").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Unary {
                op: TokenKind::Pipe,
                ..
            }
        ));
    }

    #[test]
    fn parse_slip_prefix_with_topic_method_call() {
        let (rest, expr) = expression("|.value").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Unary {
                op: TokenKind::Pipe,
                expr
            } if matches!(*expr, Expr::MethodCall { .. })
        ));
    }

    #[test]
    fn parse_slip_prefix_with_quote_word_list() {
        let (rest, expr) = expression("|<a b>").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Unary {
                op: TokenKind::Pipe,
                ..
            }
        ));
    }

    #[test]
    fn parse_hyper_prefix_slip_left_on_angle_list() {
        let (rest, expr) = expression("|<< <a x y z>").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Unary {
                op: TokenKind::Pipe,
                ..
            }
        ));
    }

    #[test]
    fn parse_hyper_prefix_slip_right_on_angle_list() {
        let (rest, expr) = expression("|>> <a x y z>").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Unary {
                op: TokenKind::Pipe,
                ..
            }
        ));
    }

    #[test]
    fn parse_prefix_boolify_codevar_method_call() {
        let (rest, expr) = expression("?&foo.cando($c)").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Unary {
                op: TokenKind::Question,
                expr,
            } => match *expr {
                Expr::MethodCall { target, .. } => {
                    assert!(matches!(*target, Expr::CodeVar(ref name) if name == "foo"));
                }
                _ => panic!("expected method call operand"),
            },
            _ => panic!("expected prefix boolify expression"),
        }
    }

    #[test]
    fn parse_hyper_prefix_metaop_negate() {
        let (rest, expr) = expression("-« ([1, 2], 3)").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args } => {
                assert_eq!(name, "__mutsu_hyper_prefix");
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], Expr::Literal(Value::Str(ref s)) if s == "-"));
            }
            _ => panic!("expected hyper prefix metaop call"),
        }
    }

    #[test]
    fn parse_parenthesized_sequence_with_following_smartmatch() {
        let (rest, expr) = expression("(\"a\"...* ~~ / z /)").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Binary { .. }));
    }

    #[test]
    fn parse_parenthesized_sequence_with_empty_paren_seed_and_postfix_index() {
        let (rest, expr) = expression("(() ... *)[0]").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Index { .. }));
    }

    #[test]
    fn parse_s_metaop_infix_and() {
        let (rest, expr) = expression("1 S& 2").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Binary { .. }));
    }

    #[test]
    fn parse_pair_lvalue_colonparen_form() {
        let (rest, expr) = expression(":(:$a is raw)").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::Literal(Value::Instance { ref class_name, .. }) if class_name == "Signature"
        ));
    }

    #[test]
    fn parse_custom_postfix_operator_call() {
        let (rest, expr) = expression("$base!").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args } => {
                assert_eq!(name, "postfix:<!>");
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Expr::Var(ref n) if n == "base"));
            }
            _ => panic!("expected postfix operator call"),
        }
    }

    #[test]
    fn parse_unicode_custom_postfix_operator_call() {
        let (rest, expr) = expression("3§").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args } => {
                assert_eq!(name, "postfix:<§>");
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Expr::Literal(Value::Int(3))));
            }
            _ => panic!("expected unicode postfix operator call"),
        }
    }

    #[test]
    fn parse_dot_custom_postfix_operator_call() {
        let (rest, expr) = expression("5.!").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::Call { name, args } => {
                assert_eq!(name, "postfix:<!>");
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Expr::Literal(Value::Int(5))));
            }
            _ => panic!("expected dot-postfix operator call"),
        }
    }

    #[test]
    fn parse_dot_postfix_increment() {
        let (rest, expr) = expression("$x.++").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::PostfixOp {
                op: TokenKind::PlusPlus,
                expr,
            } => {
                assert!(matches!(*expr, Expr::Var(ref n) if n == "x"));
            }
            _ => panic!("expected dot-postfix increment"),
        }
    }

    #[test]
    fn parse_dot_hyper_postfix_increment() {
        let (rest, expr) = expression("$x.>>++").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::HyperMethodCall {
                target, name, args, ..
            } => {
                assert!(matches!(*target, Expr::Var(ref n) if n == "x"));
                assert_eq!(name, "postfix:<++>");
                assert!(args.is_empty());
            }
            _ => panic!("expected hyper postfix increment"),
        }
    }

    #[test]
    fn parse_dot_postfix_decrement() {
        let (rest, expr) = expression("$x.--").unwrap();
        assert_eq!(rest, "");
        match expr {
            Expr::PostfixOp {
                op: TokenKind::MinusMinus,
                expr,
            } => {
                assert!(matches!(*expr, Expr::Var(ref n) if n == "x"));
            }
            _ => panic!("expected dot-postfix decrement"),
        }
    }

    #[test]
    fn parse_ampersand_sigiled_callable_invocation() {
        let (rest, expr) = expression("&$x()").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(
            expr,
            Expr::CallOn { target, args } if args.is_empty() && matches!(*target, Expr::Var(ref n) if n == "x")
        ));
    }
}
