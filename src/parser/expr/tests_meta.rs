use super::*;

#[test]
fn parse_hash_literal_with_hash_spread_expr() {
    let (rest, expr) = expression("{year => 1984, %args}").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Call { name, args } => {
            assert_eq!(name, "hash");
            assert_eq!(args.len(), 2);
        }
        _ => panic!("expected lowered hash() call"),
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
fn parse_slip_prefix_with_upto_operator() {
    let (rest, expr) = expression("|^2").unwrap();
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
fn parse_parenthesized_slip_prefix_with_method_chain() {
    let (rest, expr) = expression("(|^2).Seq").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::MethodCall { name, .. } if name == "Seq"
    ));
}

#[test]
fn parse_take_listop_with_xx_expression_argument() {
    let (rest, expr) = expression("take (@b.shift xx 2) xx 2").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(expr, Expr::Call { name, .. } if name == "take"));
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
fn parse_slip_prefix_with_space_before_french_quote_list() {
    let (rest, expr) = expression("| «echo test»").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Unary {
            op: TokenKind::Pipe,
            expr
        } if matches!(
            *expr,
            Expr::ArrayLiteral(ref items) if items.len() == 2
        ) || matches!(
            *expr,
            Expr::Call { ref name, ref args }
                if name.resolve() == "list" && args.len() == 2
        )
    ));
}

#[test]
fn parse_slip_prefix_with_space_before_var() {
    let (rest, expr) = expression("|   @cmd").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Unary {
            op: TokenKind::Pipe,
            expr
        } if matches!(*expr, Expr::ArrayVar(ref name) if name == "cmd")
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
            assert!(matches!(args[0], Expr::Literal(Value::Str(ref s)) if s.as_str() == "-"));
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
fn parse_zip_meta_with_parenthesized_set_intersection() {
    let (rest, expr) = expression("1..3 Z(&) 2..4").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::MetaOp { meta, op, .. } => {
            assert_eq!(meta, "Z");
            assert_eq!(op, "(&)");
        }
        _ => panic!("expected zip meta op"),
    }
}

#[test]
fn parse_zip_meta_with_unicode_set_intersection() {
    let (rest, expr) = expression("1..3 Z∩ 2..4").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::MetaOp { meta, op, .. } => {
            assert_eq!(meta, "Z");
            assert_eq!(op, "∩");
        }
        _ => panic!("expected zip meta op"),
    }
}

#[test]
fn parse_unicode_set_union_infix() {
    let (rest, expr) = expression("1 ∪ 2").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Binary {
            op: TokenKind::SetUnion,
            ..
        }
    ));
}

#[test]
fn parse_whatever_with_unicode_set_union_infix() {
    let (rest, expr) = expression("* ∪ *").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::AnonSubParams { body, .. }
            if matches!(
                body.as_slice(),
                [Stmt::Expr(Expr::Binary { op: TokenKind::SetUnion, .. })]
            )
    ));
}

#[test]
fn parse_zip_metaop_with_set_union_ascii() {
    let (rest, expr) = expression("1..3 Z(|) 2..4").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::MetaOp { ref meta, ref op, .. } if meta == "Z" && op == "(|)"
    ));
}

#[test]
fn parse_zip_metaop_with_set_union_unicode() {
    let (rest, expr) = expression("1..3 Z∪ 2..4").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::MetaOp { ref meta, ref op, .. } if meta == "Z" && op == "∪"
    ));
}

#[test]
fn parse_cross_metaop_with_bitshift_right() {
    let (rest, expr) = expression("1 X+> 2").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::MetaOp { ref meta, ref op, .. } if meta == "X" && op == "+>"
    ));
}

#[test]
fn parse_grouped_sequences_as_metaop_operands() {
    let (rest, expr) = expression("(1 ... *) Z~ ('a' ... 'z')").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::MetaOp { ref meta, ref op, .. } if meta == "Z" && op == "~"
    ));
}

#[test]
fn parse_bracket_infix_func_chain_with_same_operator() {
    let (rest, expr) = expression("2 [&foo] 3 [&foo] 4").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::InfixFunc { ref name, modifier: None, .. } if name == "foo"
    ));
}

#[test]
fn parse_reverse_metaop_on_comma_list_operator() {
    let (rest, expr) = expression("1 R, 2 R, 3 R, 4").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(expr, Expr::MetaOp { .. }));
}

#[test]
fn parse_reverse_metaop_andand() {
    let (rest, expr) = expression("$x R&& 0").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(expr, Expr::MetaOp { .. }));
}

#[test]
fn parse_reverse_metaop_oror() {
    let (rest, expr) = expression("$x R|| 1").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(expr, Expr::MetaOp { .. }));
}

#[test]
fn parse_reverse_meta_compound_assign_rhs_target() {
    let (rest, expr) = expression("10 R+= ($a ||= 42)").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::DoBlock { .. } | Expr::AssignExpr { .. }
    ));
}

#[test]
fn parse_reverse_meta_compound_assign_non_lvalue_target() {
    let (rest, expr) = expression("($a ||= 42) R+= 10").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(expr, Expr::DoBlock { .. }));
}

#[test]
fn parse_reverse_dot_concat_reports_obsolete() {
    let err = expression("3 R. \"foo\"").unwrap_err();
    assert!(err.message().contains("X::Obsolete"));
}
