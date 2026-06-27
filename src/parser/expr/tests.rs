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
fn parse_smartmatch_with_p5_subst_rhs() {
    let (rest, expr) = expression("$path ~~ s:Perl5:g{/+} = '/'").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Binary {
            op: TokenKind::SmartMatch,
            right,
            ..
        } if matches!(
            &*right,
            Expr::Subst {
                pattern,
                replacement,
                global: true,
                perl5: true,
                ..
            } if pattern == "/+" && replacement == "/"
        )
    ));
}

#[test]
fn parse_smartmatch_with_p5_subst_rhs_and_qq_replacement() {
    let (rest, expr) = expression("$bar ~~ s:P5:g{$rule3}=qq{$subst}").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Binary {
            op: TokenKind::SmartMatch,
            right,
            ..
        } if matches!(
            &*right,
            Expr::AssignExpr { name, .. } if name == "_"
        )
    ));
}

#[test]
fn parse_smartmatch_with_regex_junction_rhs() {
    let (rest, expr) = expression("'testing' ~~ /t/ & /s/ & /g/").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Binary {
            op: TokenKind::SmartMatch,
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
fn parse_ternary_allows_parenthesized_assignment_branches() {
    let (rest, expr) = expression("$x ?? ($y = 1) !! ($y = 2)").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Ternary {
            then_expr,
            else_expr,
            ..
        } => {
            assert!(matches!(*then_expr, Expr::Grouped(_)));
            assert!(matches!(*else_expr, Expr::Grouped(_)));
        }
        _ => panic!("expected ternary expression"),
    }
}

#[test]
fn parse_parenthesized_compound_assign_as_lvalue_expression() {
    let (rest, expr) = expression("($x += 2) *= 3").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::AssignExpr { name, expr, .. } => {
            assert_eq!(name, "x");
            assert!(matches!(*expr, Expr::Binary { .. }));
        }
        other => panic!("expected nested AssignExpr, got {other:?}"),
    }
}

#[test]
fn parse_parenthesized_metaassign_chain_as_lvalue_expression() {
    let (rest, expr) = expression("(@a ||= 42) += 10").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::AssignExpr { name, expr, .. } => {
            assert_eq!(name, "@a");
            assert!(matches!(*expr, Expr::Binary { .. }));
        }
        other => panic!("expected nested AssignExpr, got {other:?}"),
    }
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
            op: TokenKind::AndWord,
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
fn parse_one_junction_operator_without_spaces() {
    let (rest, expr) = expression("3^2").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Binary {
            op: TokenKind::Caret,
            ..
        }
    ));
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
fn parse_upto_with_infinity_literal() {
    let (rest, expr) = expression("^∞").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Binary { left, op, right } => {
            assert!(matches!(*left, Expr::Literal(Value::Int(0))));
            assert!(matches!(op, TokenKind::DotDotCaret));
            assert!(matches!(
                *right,
                Expr::Literal(Value::Int(_))
                    | Expr::Literal(Value::BigInt(_))
                    | Expr::Literal(Value::Num(_))
            ));
        }
        _ => panic!("expected upto range expression"),
    }
}

#[test]
fn parse_upto_with_negative_literal() {
    let (rest, expr) = expression("^-1").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Binary { left, op, right } => {
            assert!(matches!(*left, Expr::Literal(Value::Int(0))));
            assert!(matches!(op, TokenKind::DotDotCaret));
            assert!(matches!(
                *right,
                Expr::Unary {
                    op: TokenKind::Minus,
                    ..
                }
            ));
        }
        _ => panic!("expected upto range expression"),
    }
}

#[test]
fn parse_topical_dot_angle_expression() {
    let (rest, expr) = expression(".<a>").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(expr, Expr::Index { .. }));
}

#[test]
fn parse_topical_dot_brace_expression() {
    let (rest, expr) = expression(".{'a'}").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(expr, Expr::Index { .. }));
}

#[test]
fn parse_dynamic_pseudostash_dot_angle_with_dynamic_var() {
    let (rest, expr) = expression("DYNAMIC::.<$*x80>").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Index { target, index, .. }
            if matches!(target.as_ref(), Expr::PseudoStash(s) if s.as_str() == "DYNAMIC::")
            && matches!(index.as_ref(), Expr::Literal(Value::Str(s)) if s.as_str() == "$*x80")
    ));
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
