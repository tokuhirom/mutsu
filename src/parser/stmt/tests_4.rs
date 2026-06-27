//! Unit tests for `stmt` parsing (split out of mod.rs, part 4).

use super::*;
use crate::ast::{CallArg, Expr};
use crate::value::Value;

#[test]
fn assign_expr_parses_reverse_bracket_metaop_assign() {
    let (rest, expr) = assign::try_parse_assign_expr("$y [R/]= 1").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::AssignExpr { name, expr, .. } => {
            assert_eq!(name, "y");
            match *expr {
                Expr::MetaOp {
                    meta,
                    op,
                    left,
                    right,
                } => {
                    assert_eq!(meta, "R");
                    assert_eq!(op, "/");
                    assert!(matches!(*left, Expr::Var(ref n) if n == "y"));
                    assert!(matches!(*right, Expr::Literal(Value::Int(1))));
                }
                other => panic!("expected meta-op assignment expr, got {other:?}"),
            }
        }
        other => panic!("expected AssignExpr, got {other:?}"),
    }
}

#[test]
fn assign_expr_bracket_metaop_leaves_following_call_args() {
    let (rest, _) = assign::try_parse_assign_expr("$y [R/]= 1, 1/5, 'desc'").unwrap();
    assert_eq!(rest, ", 1/5, 'desc'");
}

#[test]
fn assign_stmt_parses_nested_bracket_metaop_assign() {
    let (rest, stmt) = assign::assign_stmt("$x [R[+]]= 1;").unwrap();
    assert_eq!(rest, "");
    match stmt {
        Stmt::Assign { expr, .. } => match expr {
            Expr::MetaOp { meta, op, .. } => {
                assert_eq!(meta, "R");
                assert_eq!(op, "+");
            }
            other => panic!("expected meta-op assignment stmt, got {other:?}"),
        },
        other => panic!("expected Assign stmt, got {other:?}"),
    }
}

#[test]
fn assign_expr_parses_unbracketed_cross_metaop_assign() {
    let (rest, expr) = assign::try_parse_assign_expr("@a X*= 10").unwrap();
    assert_eq!(rest, "");
    match expr {
        Expr::AssignExpr { name, expr, .. } => {
            assert_eq!(name, "@a");
            match *expr {
                Expr::MetaOp {
                    meta,
                    op,
                    left,
                    right,
                } => {
                    assert_eq!(meta, "X");
                    assert_eq!(op, "*");
                    assert!(matches!(*left, Expr::ArrayVar(ref n) if n == "a"));
                    assert!(matches!(*right, Expr::Literal(Value::Int(10))));
                }
                other => panic!("expected meta-op assignment expr, got {other:?}"),
            }
        }
        other => panic!("expected AssignExpr, got {other:?}"),
    }
}

#[test]
fn assign_stmt_parses_unbracketed_cross_metaop_assign() {
    let (rest, stmt) = assign::assign_stmt("@a X*= 10;").unwrap();
    assert_eq!(rest, "");
    match stmt {
        Stmt::Assign { name, expr, .. } => {
            assert_eq!(name, "@a");
            match expr {
                Expr::MetaOp { meta, op, .. } => {
                    assert_eq!(meta, "X");
                    assert_eq!(op, "*");
                }
                other => panic!("expected meta-op assignment stmt, got {other:?}"),
            }
        }
        other => panic!("expected Assign stmt, got {other:?}"),
    }
}

#[test]
fn assign_stmt_parses_unbracketed_cross_plain_assign() {
    let (rest, stmt) = assign::assign_stmt("@a X= 10;").unwrap();
    assert_eq!(rest, "");
    match stmt {
        Stmt::Assign { name, expr, .. } => {
            assert_eq!(name, "@a");
            match expr {
                Expr::MetaOp { meta, op, .. } => {
                    assert_eq!(meta, "X");
                    assert_eq!(op, "=");
                }
                other => panic!("expected meta-op assignment stmt, got {other:?}"),
            }
        }
        other => panic!("expected Assign stmt, got {other:?}"),
    }
}

#[test]
fn parse_meta_compound_assign_op_recognizes_reverse_add_assign() {
    let parsed = assign::parse_meta_compound_assign_op("R+= 10");
    assert!(parsed.is_some());
    let (rest, meta, op) = parsed.unwrap();
    assert_eq!(rest, " 10");
    assert_eq!(meta, "R");
    assert_eq!(op, "+");
}

#[test]
fn known_call_stmt_accepts_bracket_metaop_assign_argument() {
    simple::register_module_exports("Test");
    let parsed = simple::known_call_stmt("is $y [R/]= 1, 1/5, \"[R/]= works correctly (1)\";");
    assert!(parsed.is_ok());
}

#[test]
fn statement_accepts_known_call_with_bracket_metaop_assign_argument() {
    simple::register_module_exports("Test");
    let parsed = statement("is $y [R/]= 1, 1/5, \"[R/]= works correctly (1)\";");
    assert!(parsed.is_ok());
}

#[test]
fn expr_stmt_parses_reverse_assignment_into_var_decl() {
    let (rest, stmt) = simple::expr_stmt("1 R= my $x").unwrap();
    assert_eq!(rest, "");
    match stmt {
        Stmt::VarDecl { name, expr, .. } => {
            assert_eq!(name, "x");
            assert!(matches!(expr, Expr::Literal(Value::Int(1))));
        }
        other => panic!("expected VarDecl, got {other:?}"),
    }
}

#[test]
fn expr_stmt_rejects_reverse_bind_assignment() {
    let err = simple::expr_stmt("5 R:= $x").unwrap_err();
    assert!(err.message().contains("Cannot reverse the args of :="));
}

#[test]
fn expr_stmt_non_lvalue_assignment_is_not_silently_ignored() {
    let (rest, stmt) = simple::expr_stmt("(\"a\" R~ \"b\") = 1").unwrap();
    assert_eq!(rest, "");
    match stmt {
        Stmt::Expr(Expr::Call { name, .. }) => {
            assert_eq!(name, "__mutsu_assign_callable_lvalue");
        }
        other => panic!("expected assignment call expression, got {other:?}"),
    }
}

#[test]
fn known_call_stmt_reports_argument_parse_context() {
    simple::register_module_exports("Test");
    let err = simple::known_call_stmt("ok ,").unwrap_err();
    assert!(err.message().contains("known call arguments"));
}

#[test]
fn known_call_stmt_reports_missing_comma_argument() {
    simple::register_module_exports("Test");
    let err = simple::known_call_stmt("ok(,)").unwrap_err();
    assert!(err.message().contains("known call arguments"));
}

#[test]
fn known_call_stmt_reports_missing_named_argument_value() {
    simple::register_module_exports("Test");
    let err = simple::known_call_stmt("ok :foo()").unwrap_err();
    assert!(err.message().contains("named argument value"));
}

#[test]
fn known_call_stmt_parses_unicode_and_ascii_minus_angle_complex_args() {
    simple::reset_user_subs();
    simple::register_module_exports("Test");
    let (rest, stmt) =
        simple::known_call_stmt("is-deeply −<42+2i>, -<42+2i>, 'prefix, Complex'").unwrap();
    assert_eq!(rest, "");
    match stmt {
        Stmt::Call { name, args } => {
            assert_eq!(name, "is-deeply");
            assert!(args.len() >= 2);
            assert!(matches!(
                args[0],
                CallArg::Positional(Expr::Unary {
                    op: crate::token_kind::TokenKind::Minus,
                    ..
                })
            ));
            assert!(matches!(
                args[1],
                CallArg::Positional(Expr::Unary {
                    op: crate::token_kind::TokenKind::Minus,
                    ..
                })
            ));
        }
        other => panic!("Expected Call, got {other:?}"),
    }
}

#[test]
fn known_call_stmt_keeps_chained_assignment_as_single_argument() {
    simple::register_module_exports("Test");
    let (rest, stmt) = simple::known_call_stmt("is $x = $y = $z, 18;").unwrap();
    assert_eq!(rest, "");
    match stmt {
        Stmt::Call { name, args } => {
            assert_eq!(name, "is");
            assert!(matches!(
                args.first(),
                Some(CallArg::Positional(Expr::AssignExpr { .. }))
            ));
            assert!(matches!(
                args.get(1),
                Some(CallArg::Positional(Expr::Literal(Value::Int(18))))
            ));
        }
        other => panic!("expected known call statement, got {other:?}"),
    }
}

#[test]
fn known_call_stmt_splits_assignment_from_following_test_args() {
    simple::register_module_exports("Test");
    let (rest, stmt) = simple::known_call_stmt(
        "is $ = bar(), \"NoModule::bar\", \"can import symbol not inside module\";",
    )
    .unwrap();
    assert_eq!(rest, "");
    match stmt {
        Stmt::Call { name, args } => {
            assert_eq!(name, "is");
            assert!(matches!(
                args.first(),
                Some(CallArg::Positional(Expr::AssignExpr { .. }))
            ));
            assert!(matches!(
                args.get(1),
                Some(CallArg::Positional(Expr::Literal(Value::Str(expected))))
                    if **expected == "NoModule::bar"
            ));
            assert!(matches!(
                args.get(2),
                Some(CallArg::Positional(Expr::Literal(Value::Str(expected))))
                    if **expected == "can import symbol not inside module"
            ));
        }
        other => panic!("expected known call statement, got {other:?}"),
    }
}

#[test]
fn qualified_ident_requires_segment_after_double_colon() {
    let err = qualified_ident("Foo::").unwrap_err();
    assert!(err.message().contains("identifier after '::'"));
}

#[test]
fn import_scope_is_lexical() {
    simple::reset_user_subs();
    // At top scope, "ok" is not imported
    assert!(!simple::is_imported_function("ok"));
    // Push a child scope and import Test
    simple::push_scope();
    simple::register_module_exports("Test");
    assert!(simple::is_imported_function("ok"));
    // Pop the child scope — "ok" should no longer be visible
    simple::pop_scope();
    assert!(
        !simple::is_imported_function("ok"),
        "imported function should not be visible after scope pop"
    );
}

#[test]
fn import_scope_inherits_from_parent() {
    simple::reset_user_subs();
    // Import at top scope
    simple::register_module_exports("Test");
    assert!(simple::is_imported_function("ok"));
    // Push a child scope — parent imports should still be visible
    simple::push_scope();
    assert!(
        simple::is_imported_function("ok"),
        "parent scope imports should be visible in child"
    );
    simple::pop_scope();
    // Still visible in parent
    assert!(simple::is_imported_function("ok"));
    simple::reset_user_subs();
}

#[test]
fn user_sub_scope_is_lexical() {
    simple::reset_user_subs();
    assert!(!simple::is_user_declared_sub("my-func"));
    // Declare a sub in a child scope
    simple::push_scope();
    simple::register_user_sub("my-func");
    assert!(simple::is_user_declared_sub("my-func"));
    // Pop — sub should no longer be visible
    simple::pop_scope();
    assert!(
        !simple::is_user_declared_sub("my-func"),
        "user sub should not be visible after scope pop"
    );
}

#[test]
fn user_sub_scope_inherits_from_parent() {
    simple::reset_user_subs();
    simple::register_user_sub("outer-func");
    assert!(simple::is_user_declared_sub("outer-func"));
    // Child scope should see parent's subs
    simple::push_scope();
    assert!(
        simple::is_user_declared_sub("outer-func"),
        "parent sub should be visible in child scope"
    );
    simple::pop_scope();
    assert!(simple::is_user_declared_sub("outer-func"));
    simple::reset_user_subs();
}

#[test]
fn done_parses_as_react_done_stmt() {
    let (rest, stmt) = simple::known_call_stmt("done").unwrap();
    assert!(rest.is_empty(), "rest should be empty, got: {rest:?}");
    assert!(
        matches!(stmt, Stmt::ReactDone),
        "expected ReactDone, got: {stmt:?}"
    );
}

#[test]
fn done_with_semicolon_parses_as_react_done_stmt() {
    let (rest, stmt) = simple::known_call_stmt("done;").unwrap();
    assert!(rest.is_empty(), "rest should be empty, got: {rest:?}");
    assert!(
        matches!(stmt, Stmt::ReactDone),
        "expected ReactDone, got: {stmt:?}"
    );
}

#[test]
fn proceed_parses_as_proceed_stmt() {
    let (rest, stmt) = simple::known_call_stmt("proceed").unwrap();
    assert!(rest.is_empty());
    assert!(matches!(stmt, Stmt::Proceed));
}

#[test]
fn succeed_parses_as_succeed_stmt() {
    let (rest, stmt) = simple::known_call_stmt("succeed").unwrap();
    assert!(rest.is_empty());
    assert!(matches!(stmt, Stmt::Succeed));
}

#[test]
fn var_name_accepts_root_stash_symbol_declarator() {
    let (rest, name) = var_name("$::<!@#$>").unwrap();
    assert_eq!(rest, "");
    assert_eq!(name, "::<!@#$>");
}
