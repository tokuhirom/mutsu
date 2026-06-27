use super::*;

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
            assert!(matches!(args[0], Expr::Var(ref n) if n.as_str() == "base"));
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
            assert!(matches!(*expr, Expr::Var(ref n) if n.as_str() == "x"));
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
            assert!(matches!(*target, Expr::Var(ref n) if n.as_str() == "x"));
            assert_eq!(name, "postfix:<++>");
            assert!(args.is_empty());
        }
        _ => panic!("expected hyper postfix increment"),
    }
}

#[test]
fn parse_hyper_index_without_dot() {
    let (rest, expr) = expression("%h{*}»[1]").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::HyperMethodCall {
            target, name, args, ..
        } => {
            assert_eq!(name, "AT-POS");
            assert_eq!(args.len(), 1);
            assert!(matches!(*target, Expr::Index { .. }));
        }
        _ => panic!("expected hyper index call"),
    }
}

#[test]
fn parse_hyper_index_with_dot() {
    let (rest, expr) = expression("%h{*}».[1]").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::HyperMethodCall {
            target, name, args, ..
        } => {
            assert_eq!(name, "AT-POS");
            assert_eq!(args.len(), 1);
            assert!(matches!(*target, Expr::Index { .. }));
        }
        _ => panic!("expected hyper dot-index call"),
    }
}

#[test]
fn parse_dot_ampersand_block_call() {
    let (rest, expr) = expression("$m.&{ 3 }").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            ..
        } => {
            assert!(matches!(*target, Expr::Var(ref n) if n.as_str() == "m"));
            assert!(matches!(*name_expr, Expr::AnonSub { .. }));
            assert!(args.is_empty());
        }
        _ => panic!("expected dynamic method call"),
    }
}

#[test]
fn parse_hyper_dot_ampersand_block_call() {
    let (rest, expr) = expression("$m>>.&{ 3 }").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            assert!(matches!(*target, Expr::Var(ref n) if n.as_str() == "m"));
            assert!(matches!(*name_expr, Expr::AnonSub { .. }));
            assert!(args.is_empty());
        }
        _ => panic!("expected hyper dynamic method call"),
    }
}

#[test]
fn parse_hyper_method_with_unspace_after_operator() {
    let (rest, expr) = expression("$m»\\\n.foo").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::HyperMethodCall { target, name, .. } => {
            assert!(matches!(*target, Expr::Var(ref n) if n.as_str() == "m"));
            assert_eq!(name, "foo");
        }
        _ => panic!("expected hyper method call"),
    }
}

#[test]
fn parse_array_slice_assignment_with_comma_rhs() {
    let (rest, expr) = expression("@a[0,1] = 10,20").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::IndexAssign { value, .. } => match *value {
            Expr::ArrayLiteral(items) => {
                assert_eq!(items.len(), 2);
                assert!(matches!(items[0], Expr::Literal(Value::Int(10))));
                assert!(matches!(items[1], Expr::Literal(Value::Int(20))));
            }
            other => panic!("expected ArrayLiteral RHS, got {other:?}"),
        },
        _ => panic!("expected IndexAssign expression"),
    }
}

#[test]
fn parse_hash_slice_assignment_with_comma_rhs() {
    let (rest, expr) = expression("%h{1,2} = \"one\", \"two\"").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::IndexAssign { value, .. } => match *value {
            Expr::ArrayLiteral(items) => {
                assert_eq!(items.len(), 2);
                assert!(
                    matches!(items[0], Expr::Literal(Value::Str(ref s)) if s.as_str() == "one")
                );
                assert!(
                    matches!(items[1], Expr::Literal(Value::Str(ref s)) if s.as_str() == "two")
                );
            }
            other => panic!("expected ArrayLiteral RHS, got {other:?}"),
        },
        _ => panic!("expected IndexAssign expression"),
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
            assert!(matches!(*expr, Expr::Var(ref n) if n.as_str() == "x"));
        }
        _ => panic!("expected dot-postfix decrement"),
    }
}

#[test]
fn parse_dot_method_name_then_postfix_decrement() {
    let (rest, expr) = expression("$obj.b--").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::PostfixOp {
            op: TokenKind::MinusMinus,
            expr,
        } => match *expr {
            Expr::MethodCall { name, .. } => assert_eq!(name.resolve(), "b"),
            _ => panic!("expected MethodCall inside postfix decrement"),
        },
        _ => panic!("expected postfix decrement on method call"),
    }
}

#[test]
fn parse_ascii_minus_on_angle_complex_literal() {
    let (rest, expr) = expression("-<42+2i>").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Unary {
            op: TokenKind::Minus,
            expr,
        } => {
            // Single-element <42+2i> produces plain Complex (not ComplexStr allomorph)
            assert!(matches!(*expr, Expr::Literal(Value::Complex(42.0, 2.0))));
        }
        _ => panic!("expected unary minus expression"),
    }
}

#[test]
fn parse_is_deeply_with_unicode_and_ascii_minus_complex_literals() {
    let (rest, expr) = expression("is-deeply −<42+2i>, -<42+2i>").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Call { name, args } => {
            assert_eq!(name, "is-deeply");
            assert!(args.len() >= 2);
            assert!(matches!(
                args[0],
                Expr::Unary {
                    op: TokenKind::Minus,
                    ..
                }
            ));
            assert!(matches!(
                args[1],
                Expr::Unary {
                    op: TokenKind::Minus,
                    ..
                }
            ));
        }
        _ => panic!("expected is-deeply call"),
    }
}

#[test]
fn parse_expr_listop_with_topic_method_first_arg() {
    let (rest, expr) = expression("is-deeply .bool-only, True, 'ok'").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Call { name, args } => {
            assert_eq!(name, "is-deeply");
            assert!(args.len() >= 3);
            assert!(matches!(args[0], Expr::MethodCall { .. }));
        }
        _ => panic!("expected is-deeply call"),
    }
}

#[test]
fn parse_unary_plus_on_topic_method_call() {
    let (rest, expr) = expression("+.lines").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Unary {
            op: TokenKind::Plus,
            ..
        }
    ));
}

#[test]
fn parse_x_with_bare_whatever_rhs_without_whatevercode_wrap() {
    let (rest, expr) = expression("'a' x *").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Binary {
            op: TokenKind::Ident(ref name),
            ..
        } if name == "x"
    ));
}

#[test]
fn parse_xx_with_bare_whatever_rhs_without_whatevercode_wrap() {
    let (rest, expr) = expression("42 xx *").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::Binary {
            op: TokenKind::Ident(ref name),
            ..
        } if name == "xx"
    ));
}

#[test]
fn parse_xx_with_bare_whatever_and_postfix_index_without_wrap() {
    let (rest, expr) = expression("((2,4,6) xx *)[^2]").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(expr, Expr::Index { .. }));
}

#[test]
fn parse_ternary_with_expr_listop_branches() {
    let (rest, expr) =
        expression(".can('bool-only') ?? is-deeply .bool-only, True, 'ok' !! skip 'x'").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Ternary {
            then_expr,
            else_expr,
            ..
        } => {
            assert!(matches!(*then_expr, Expr::Call { .. }));
            assert!(matches!(*else_expr, Expr::Call { .. }));
        }
        _ => panic!("expected ternary expression"),
    }
}

#[test]
fn parse_ampersand_sigiled_callable_invocation() {
    let (rest, expr) = expression("&$x()").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        expr,
        Expr::CallOn { target, args } if args.is_empty() && matches!(*target, Expr::Var(ref n) if n.as_str() == "x")
    ));
}

#[test]
fn parse_ampersand_infix_operator_reference_double_angles() {
    let (rest, expr) = expression("&infix:<<(<=)>>").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(expr, Expr::CodeVar(ref name) if name == "infix:<(<=)>"));
}

#[test]
fn parse_ampersand_infix_operator_reference_unicode_symbol() {
    let (rest, expr) = expression("&infix:<⊆>").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(expr, Expr::CodeVar(ref name) if name == "infix:<⊆>"));
}
