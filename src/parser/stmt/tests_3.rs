//! Unit tests for `stmt` parsing (split out of mod.rs, part 3).

use super::*;
use crate::ast::Expr;
use crate::value::Value;

#[test]
fn parse_class_decl_with_also_is_rw_trait() {
    let (rest, stmts) = program("class Foo { has $.x; also is rw; has $.y }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    if let Stmt::ClassDecl {
        name,
        class_is_rw,
        body,
        ..
    } = &stmts[0]
    {
        assert_eq!(name.resolve(), "Foo");
        assert!(*class_is_rw);
        assert_eq!(body.len(), 2);
    } else {
        panic!("expected ClassDecl");
    }
}

#[test]
fn parse_class_has_decl_is_readonly_trait() {
    let (rest, stmts) = program("class Foo { has $.x is readonly }").unwrap();
    assert_eq!(rest, "");
    if let Stmt::ClassDecl { body, .. } = &stmts[0] {
        assert!(matches!(
            &body[0],
            Stmt::HasDecl {
                is_readonly: true,
                ..
            }
        ));
    } else {
        panic!("expected ClassDecl");
    }
}

#[test]
fn parse_class_has_decl_dot_equals_initializer() {
    let src = r#"class MyHandle {
        has Buf[uint8] $.data .= new: "x".encode;
    }"#;
    let (rest, stmts) = program(src).unwrap();
    assert_eq!(rest, "");
    if let Stmt::ClassDecl { body, .. } = &stmts[0] {
        assert!(matches!(
            &body[0],
            Stmt::HasDecl {
                default: Some(_),
                ..
            }
        ));
    } else {
        panic!("expected ClassDecl");
    }
}

#[test]
fn parse_class_attribute_postfix_of_type() {
    let (rest, stmts) = program("class Foo { has @.a of Int }").unwrap();
    assert_eq!(rest, "");
    if let Stmt::ClassDecl { body, .. } = &stmts[0] {
        if let Stmt::HasDecl {
            sigil,
            type_constraint,
            ..
        } = &body[0]
        {
            assert_eq!(*sigil, '@');
            assert_eq!(type_constraint.as_deref(), Some("Int"));
        } else {
            panic!("expected HasDecl");
        }
    } else {
        panic!("expected ClassDecl");
    }
}

#[test]
fn parse_class_attribute_where_constraint() {
    let (rest, stmts) = program("class Foo { has $.x where * > 0 }").unwrap();
    assert_eq!(rest, "");
    if let Stmt::ClassDecl { body, .. } = &stmts[0] {
        if let Stmt::HasDecl {
            where_constraint, ..
        } = &body[0]
        {
            assert!(where_constraint.is_some());
        } else {
            panic!("expected HasDecl");
        }
    } else {
        panic!("expected ClassDecl");
    }
}

#[test]
fn parse_method_decl_with_match_var_param() {
    let (rest, stmts) = program("class Foo { method TOP($/) { 1 } }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    if let Stmt::ClassDecl { body, .. } = &stmts[0] {
        assert!(matches!(&body[0], Stmt::MethodDecl { name, .. } if name == "TOP"));
    } else {
        panic!("expected ClassDecl");
    }
}

#[test]
fn parse_method_decl_with_typed_invocant_marker() {
    let (rest, stmts) = program("class A { method AT-KEY(A:D: $key) { 1 } }").unwrap();
    assert_eq!(rest, "");
    if let Stmt::ClassDecl { body, .. } = &stmts[0] {
        if let Stmt::MethodDecl { param_defs, .. } = &body[0] {
            assert_eq!(param_defs.len(), 1);
            assert_eq!(param_defs[0].name, "key");
        } else {
            panic!("expected MethodDecl");
        }
    } else {
        panic!("expected ClassDecl");
    }
}

#[test]
fn parse_method_decl_with_operator_name() {
    let (rest, stmts) =
        program("class A { method postcircumfix:<{ }>($key) { %!attrs{$key} } }").unwrap();
    assert_eq!(rest, "");
    if let Stmt::ClassDecl { body, .. } = &stmts[0] {
        assert!(matches!(&body[0], Stmt::MethodDecl { name, .. } if name == "postcircumfix:<{ }>"));
    } else {
        panic!("expected ClassDecl");
    }
}

#[test]
fn parse_sub_decl_with_typed_invocant_marker() {
    let (rest, stmts) = program("sub f(A:D: $key) { $key }").unwrap();
    assert_eq!(rest, "");
    if let Stmt::SubDecl { param_defs, .. } = &stmts[0] {
        assert_eq!(param_defs.len(), 1);
        assert_eq!(param_defs[0].name, "key");
    } else {
        panic!("expected SubDecl");
    }
}

#[test]
fn parse_method_decl_with_explicit_invocant_param() {
    let (rest, stmts) = program("class Foo { method bar ($self: $num) { $num } }").unwrap();
    assert_eq!(rest, "");
    if let Stmt::ClassDecl { body, .. } = &stmts[0] {
        if let Stmt::MethodDecl { param_defs, .. } = &body[0] {
            assert_eq!(param_defs.len(), 2);
            assert_eq!(param_defs[0].name, "self");
            assert!(param_defs[0].traits.iter().any(|t| t == "invocant"));
            assert_eq!(param_defs[1].name, "num");
            assert!(!param_defs[1].traits.iter().any(|t| t == "invocant"));
        } else {
            panic!("expected MethodDecl");
        }
    } else {
        panic!("expected ClassDecl");
    }
}

#[test]
fn parse_method_decl_with_explicit_invocant_and_return_type() {
    let (rest, stmts) =
        program("class Foo { method bar ($self: $num --> Foo) returns Foo { $self } }").unwrap();
    assert_eq!(rest, "");
    if let Stmt::ClassDecl { body, .. } = &stmts[0] {
        if let Stmt::MethodDecl { param_defs, .. } = &body[0] {
            assert_eq!(param_defs.len(), 2);
            assert!(param_defs[0].traits.iter().any(|t| t == "invocant"));
            assert_eq!(param_defs[1].name, "num");
        } else {
            panic!("expected MethodDecl");
        }
    } else {
        panic!("expected ClassDecl");
    }
}

#[test]
fn parse_role_decl_with_generics_and_does_clause() {
    let (rest, stmts) = program("role R2[Cool ::T] does R1[T] is ok { }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::RoleDecl { name, .. } if name == "R2"));
}

#[test]
fn parse_grammar_decl_with_does_clause() {
    let (rest, stmts) = program("grammar G does R { rule TOP { ^ <x> } }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::ClassDecl { name, .. } if name == "G"));
}

#[test]
fn parse_token_decl_with_regex_like_body() {
    let (rest, stmts) = program("token TOP { <fred>+ }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::TokenDecl { name, .. } if name == "TOP"));
}

#[test]
fn parse_token_decl_with_sym_variant_name() {
    let (rest, stmts) = program("token fred:sym<foo> { <sym> \\d+ }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::TokenDecl { name, .. } if name == "fred:sym<foo>"));
}

#[test]
fn parse_make_with_token_term_in_array() {
    let (rest, stmts) = program("make [token { \"bar\" }]").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
}

#[test]
fn parse_chained_constant_declarator_rhs() {
    let (rest, stmts) = program("my $a = constant $b = 42;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
}

#[test]
fn parse_my_no_space() {
    let (rest, stmts) = program("my $a=0; say $a").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2, "stmts: {:?}", stmts);
    if let Stmt::VarDecl { name, expr, .. } = &stmts[0] {
        assert_eq!(name, "a");
        assert!(
            matches!(expr, Expr::Literal(Value::Int(0))),
            "Expected Int(0), got {:?}",
            expr
        );
    } else {
        panic!("Expected VarDecl, got {:?}", stmts[0]);
    }
}

#[test]
fn statement_memo_hits_on_reparse() {
    reset_statement_memo();
    let input = "say 42";
    let _ = statement_pub(input).unwrap();
    let (hits1, misses1, stores1) = statement_memo_stats();
    assert_eq!(hits1, 0);
    assert!(misses1 >= 1);
    assert!(stores1 >= 1);

    let _ = statement_pub(input).unwrap();
    let (hits2, _misses2, stores2) = statement_memo_stats();
    assert!(hits2 >= 1);
    assert!(stores2 >= stores1);
}

#[test]
fn merge_expected_messages_deduplicates() {
    let merged = super::super::parse_result::merge_expected_messages(
        "expected foo",
        &["foo".to_string(), "bar".to_string()],
    );
    assert_eq!(merged, vec!["foo", "bar"]);
}

#[test]
fn merge_expected_messages_strips_prefix_consistently() {
    let merged = super::super::parse_result::merge_expected_messages(
        "expected alpha",
        &["beta".to_string(), "gamma".to_string()],
    );
    assert_eq!(merged, vec!["alpha", "beta", "gamma"]);
}

#[test]
fn statement_modifier_reports_missing_condition() {
    let base = Stmt::Expr(Expr::Literal(Value::Int(1)));
    let err = parse_statement_modifier(" if ", base).unwrap_err();
    assert!(err.message().contains("after 'if'"));
}

#[test]
fn assign_stmt_reports_missing_rhs_for_compound_assign() {
    let err = assign::assign_stmt("$x +=").unwrap_err();
    assert!(err.message().contains("compound assignment"));
}

#[test]
fn assign_stmt_reports_missing_method_name_for_mutating_call() {
    let err = assign::assign_stmt("$x .=").unwrap_err();
    assert!(err.message().contains("method name after '.='"));
}

#[test]
fn assign_stmt_parses_zip_compound_assign() {
    let (rest, stmt) = assign::assign_stmt("$a Z+= 2;").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(stmt, Stmt::Assign { .. }));
}

#[test]
fn assign_stmt_parses_comma_compound_assign() {
    let (rest, stmt) = assign::assign_stmt("%h ,= 1;").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(stmt, Stmt::Assign { .. }));
}

#[test]
fn assign_stmt_scalar_item_assignment_stops_at_comma() {
    // `$x = 1, 2` is `($x = 1), 2`: only the first element is assigned, and
    // the rest become a sink-context comma list.
    let (rest, stmt) = assign::assign_stmt("$x = 1, 2;").unwrap();
    assert_eq!(rest, "");
    match stmt {
        Stmt::Expr(Expr::ArrayLiteral(items)) => {
            assert_eq!(items.len(), 2);
            assert!(matches!(
                &items[0],
                Expr::AssignExpr { name, expr, .. }
                    if name == "x" && matches!(expr.as_ref(), Expr::Literal(Value::Int(1)))
            ));
            assert!(matches!(&items[1], Expr::Literal(Value::Int(2))));
        }
        other => panic!("expected comma-list expr statement, got {:?}", other),
    }
}

#[test]
fn assign_stmt_scalar_plain_assignment_has_no_comma() {
    // Without a trailing comma the assignment is a plain `Stmt::Assign`.
    let (rest, stmt) = assign::assign_stmt("$x = 1;").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        stmt,
        Stmt::Assign { name, expr, .. }
            if name == "x" && matches!(expr, Expr::Literal(Value::Int(1)))
    ));
}

#[test]
fn assign_stmt_array_list_assignment_absorbs_comma() {
    // `@a = 1, 2` keeps the comma-absorbing RHS (list assignment).
    let (rest, stmt) = assign::assign_stmt("@a = 1, 2;").unwrap();
    assert_eq!(rest, "");
    assert!(matches!(
        stmt,
        Stmt::Assign { name, expr, .. }
            if name == "@a" && matches!(&expr, Expr::ArrayLiteral(items) if items.len() == 2)
    ));
}
