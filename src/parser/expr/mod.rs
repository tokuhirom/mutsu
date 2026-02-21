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
            let (r, value) = or_expr(r)?;
            // Auto-quote bareword on LHS of => to a string literal
            let left = match expr {
                Expr::BareWord(ref name) => Expr::Literal(Value::Str(name.clone())),
                _ => expr,
            };
            return Ok((
                r,
                Expr::Binary {
                    left: Box::new(left),
                    op: TokenKind::FatArrow,
                    right: Box::new(value),
                },
            ));
        }
        // Wrap WhateverCode expressions in a lambda, but not bare * (Whatever)
        if contains_whatever(&expr) && !is_whatever(&expr) {
            let body_expr = replace_whatever(&expr);
            expr = Expr::Lambda {
                param: "_".to_string(),
                body: vec![Stmt::Expr(body_expr)],
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
        let (r, value) = or_expr(r)?;
        let left = match expr {
            Expr::BareWord(ref name) => Expr::Literal(Value::Str(name.clone())),
            _ => expr,
        };
        return Ok((
            r,
            Expr::Binary {
                left: Box::new(left),
                op: TokenKind::FatArrow,
                right: Box::new(value),
            },
        ));
    }
    if contains_whatever(&expr) {
        let body_expr = replace_whatever(&expr);
        expr = Expr::Lambda {
            param: "_".to_string(),
            body: vec![Stmt::Expr(body_expr)],
        };
    }
    Ok((rest, expr))
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
        Expr::MethodCall { target, .. } => contains_whatever(target),
        Expr::Index { target, index } => contains_whatever(target) || contains_whatever(index),
        _ => false,
    }
}

fn replace_whatever(expr: &Expr) -> Expr {
    match expr {
        e if is_whatever(e) => Expr::Var("_".to_string()),
        Expr::Binary { left, op, right } => Expr::Binary {
            left: Box::new(replace_whatever(left)),
            op: op.clone(),
            right: Box::new(replace_whatever(right)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: op.clone(),
            expr: Box::new(replace_whatever(expr)),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
        } => Expr::MethodCall {
            target: Box::new(replace_whatever(target)),
            name: name.clone(),
            args: args.clone(),
            modifier: *modifier,
        },
        Expr::Index { target, index } => Expr::Index {
            target: Box::new(replace_whatever(target)),
            index: Box::new(replace_whatever(index)),
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
        assert!(matches!(expr, Expr::MethodCall { .. }));
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
    fn parse_postfix_angle_index_requires_key() {
        let err = expression("$x<>").unwrap_err();
        assert!(err.message().contains("angle index key"));
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
                assert_eq!(args.len(), 1);
                assert!(matches!(args[0], Expr::AnonSub(_)));
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
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], Expr::Binary { .. }));
                assert!(matches!(args[1], Expr::Binary { .. }));
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
                assert!(matches!(args[0], Expr::AnonSub(_)));
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
    fn parse_parenthesized_sequence_with_following_smartmatch() {
        let (rest, expr) = expression("(\"a\"...* ~~ / z /)").unwrap();
        assert_eq!(rest, "");
        assert!(matches!(expr, Expr::Binary { .. }));
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
        assert!(matches!(expr, Expr::Binary { .. }));
    }
}
