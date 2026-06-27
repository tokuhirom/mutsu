use super::*;

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
fn parse_container_not_equal_operator() {
    let (rest, expr) = expression("$a !=:= $b").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Unary {
            op: TokenKind::Bang,
            expr,
        } => match *expr {
            Expr::Binary {
                op: TokenKind::Ident(op),
                ..
            } => assert_eq!(op, "=:="),
            _ => panic!("Expected !=:= to lower to !(=:=)"),
        },
        _ => panic!("Expected unary ! expression"),
    }
}

#[test]
fn parse_cross_with_container_not_equal_operator() {
    let (rest, expr) = expression("$a X!=:= $b").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::MetaOp { meta, op, .. } => {
            assert_eq!(meta, "X");
            assert_eq!(op, "!=:=");
        }
        _ => panic!("Expected cross meta operator expression"),
    }
}

#[test]
fn parse_cross_dot_string_reports_obsolete_error() {
    let err = expression("3 X. \"foo\"").unwrap_err();
    assert!(err.message().contains("X::Obsolete"));
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
fn parse_private_postfix_colon_args() {
    let (rest, expr) = expression("self.bless!SET-SELF: pulled, i").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            ..
        } => {
            assert_eq!(name, "SET-SELF");
            assert_eq!(modifier, Some('!'));
            assert_eq!(args.len(), 2);
            assert!(matches!(args[0], Expr::BareWord(ref n) if n.as_str() == "pulled"));
            assert!(matches!(args[1], Expr::BareWord(ref n) if n.as_str() == "i"));
            assert!(matches!(
                *target,
                Expr::MethodCall {
                    name,
                    modifier: None,
                    ..
                } if name == "bless"
            ));
        }
        _ => panic!("expected private method call expression"),
    }
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
    assert!(matches!(
        expr,
        Expr::MethodCall {
            target,
            name,
            args,
            ..
        } if name == "__mutsu_zen_angle"
            && args.is_empty()
            && matches!(*target, Expr::Var(ref n) if n.as_str() == "x")
    ));
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
            assert!(matches!(*target, Expr::HashVar(ref n) if n.as_str() == "h"));
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
                if matches!(left.as_ref(), Expr::Literal(crate::value::Value::Str(s)) if s.as_str() == "__mutsu_test_callsite_line")
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
                if matches!(left.as_ref(), Expr::Literal(crate::value::Value::Str(s)) if s.as_str() == "__mutsu_test_callsite_line")
            )));
        }
        _ => panic!("expected call expression"),
    }
}

#[test]
fn parse_subscript_adverb_does_not_become_call_adverb() {
    let (rest, expr) = expression("say @a[0]:p").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Call { name, args } => {
            assert_eq!(name, "say");
            assert_eq!(args.len(), 1);
            assert!(
                matches!(args[0], Expr::Call { ref name, .. } if name == "__mutsu_subscript_adverb")
            );
        }
        _ => panic!("expected call expression"),
    }
}

#[test]
fn parse_hash_subscript_adverb_does_not_become_call_adverb() {
    let (rest, expr) = expression("say %h<a>:kv").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Call { name, args } => {
            assert_eq!(name, "say");
            assert_eq!(args.len(), 1);
            assert!(
                matches!(args[0], Expr::Call { ref name, .. } if name == "__mutsu_subscript_adverb")
            );
        }
        _ => panic!("expected call expression"),
    }
}

#[test]
fn parse_listop_block_then_colon_args() {
    // In Raku, `map { block }: @list` is invocant-colon syntax:
    // it means `{ block }.map(@list)`, i.e., a method call on the block.
    let (rest, expr) = expression("map { $_ * 2 }: @list").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::MethodCall { name, args, .. } => {
            assert_eq!(name, "map");
            assert_eq!(args.len(), 1);
            assert!(matches!(args[0], Expr::ArrayVar(ref n) if n.as_str() == "list"));
        }
        _ => panic!("expected method call expression"),
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
            assert!(matches!(args[0], Expr::Lambda { ref param, .. } if param.as_str() == "x"));
        }
        _ => panic!("expected method call expression"),
    }
}

#[test]
fn parse_method_colon_args_adjacent_colonpairs() {
    let src = "IO::Path.new: :volume<foo> :dirname<bar> :basename<ber> :SPEC(IO::Spec::Win32)";
    let (rest, expr) = expression(src).unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::MethodCall { name, args, .. } => {
            assert_eq!(name, "new");
            assert_eq!(args.len(), 4);
        }
        _ => panic!("expected method call expression"),
    }
}

#[test]
fn parse_indir_with_hyphenated_call_arg_and_block() {
    let (rest, expr) = expression("indir make-temp-dir, { 42 }").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::Call { name, args } => {
            assert_eq!(name, "indir");
            assert_eq!(args.len(), 2);
        }
        _ => panic!("expected call expression"),
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
