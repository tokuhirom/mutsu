//! Unit tests for `stmt` parsing (split out of mod.rs, part 1).

use super::*;
use crate::ast::{CallArg, Expr};
use crate::token_kind::TokenKind;
use crate::value::Value;

#[test]
fn parse_use_test() {
    let (rest, stmts) = program("use Test;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::Use { module, .. } if module == "Test"));
}

#[test]
fn parse_block_semicolon_expr() {
    // `{ 1 }; 2` should parse: block followed by semicolon then expression
    let result = program("{ 1 }; 2");
    assert!(
        result.is_ok(),
        "Failed to parse '{{ 1 }}; 2': {:?}",
        result.err()
    );
    let (rest, stmts) = result.unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
}

#[test]
fn parse_block_same_line_no_separator_errors() {
    // `sub f { 3 } sub g { 3 }` should fail
    let result = program("sub f { 3 } sub g { 3 }");
    assert!(result.is_err());
}

#[test]
fn parse_no_strict() {
    let (rest, stmts) = program("no strict;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::No { module, .. } if module == "strict"));
}

#[test]
fn parse_plan_call() {
    let (rest, stmts) = program("use Test;\nplan 1;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
    assert!(matches!(&stmts[1], Stmt::Call { name, .. } if name == "plan"));
}

#[test]
fn parse_ok_call_with_named() {
    let (rest, stmts) = program("use Test;\nok 0, :todo(1);").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
    if let Stmt::Call { name, args } = &stmts[1] {
        assert_eq!(name, "ok");
        assert!(args.len() >= 2);
        assert!(
            args.iter()
                .any(|arg| matches!(arg, CallArg::Named { name, .. } if name == "todo"))
        );
    } else {
        panic!("Expected Call");
    }
}

#[test]
fn parse_is_deeply_unicode_ascii_minus_complex_args() {
    let src = "use Test;\nis-deeply −<42+2i>, -<42+2i>, 'prefix, Complex';";
    let (rest, stmts) = program(src).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
    if let Stmt::Call { name, args } = &stmts[1] {
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
    } else {
        panic!("Expected Call");
    }
}

#[test]
fn parse_is_deeply_unicode_ascii_minus_complex_args_in_block() {
    let src = "use Test;\n{ is-deeply −<42+2i>, -<42+2i>, 'prefix, Complex'; }";
    let (rest, stmts) = program(src).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
    assert!(matches!(stmts[1], Stmt::Block(_)));
}

#[test]
fn parse_my_var_decl() {
    let (rest, stmts) = program("my $x = '0';").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::VarDecl { name, .. } if name == "x"));
}

#[test]
fn parse_my_var_decl_with_of_type_constraint() {
    let (rest, stmts) = program("my R1 of Int $x = 1;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(
        &stmts[0],
        Stmt::VarDecl {
            name,
            type_constraint: Some(tc),
            ..
        } if name == "x" && tc == "R1[Int]"
    ));
}

#[test]
fn parse_my_grouped_decl_with_type_smiley() {
    let (rest, stmts) = program("my Int:D ($x = 5);").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::SyntheticBlock(_)));
}

#[test]
fn parse_my_invalid_type_smiley_reports_specific_exception() {
    let err = program("my Int:foo $a;").unwrap_err();
    assert!(err.message().contains("Invalid type smiley"));
}

#[test]
fn parse_my_subset_decl_as_single_statement() {
    let (rest, stmts) = program("my subset S-Int of Int;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(
        matches!(&stmts[0], Stmt::SubsetDecl { name, base, .. } if name.resolve() == "S-Int" && base == "Int")
    );
}

#[test]
fn parse_method_arg_hyphenated_subset_name() {
    let src = "use Test; { my subset S-Int of Int; my subset S-Str of Str; nok S-Int.isa(S-Str), 'isa subset'; }";
    let (rest, stmts) = program(src).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
}

#[test]
fn parse_pointy_param_where_constraint() {
    let (rest, stmts) = program("-> $ where Int|Bool { }(one True);").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
}

#[test]
fn parse_plan_skip_all() {
    let (rest, stmts) = program("use Test;\nplan skip-all => \"msg\";").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
    if let Stmt::Call { name, args } = &stmts[1] {
        assert_eq!(name, "plan");
        assert!(matches!(&args[0], CallArg::Named { name, .. } if name == "skip-all"));
    } else {
        panic!("Expected Call");
    }
}

#[test]
fn parse_basic_test_program() {
    let input = "use Test;\nplan 1;\nok 1, 'test';";
    let (rest, stmts) = program(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 3);
}

#[test]
fn parse_if_else() {
    let (rest, stmts) = program("if $x { say 1; } else { say 2; }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::If { .. }));
}

#[test]
fn parse_if_elsif_with_binding_var() {
    let (rest, stmts) = program("if 0 -> $a { } elsif 1 -> $b { } else { }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    match &stmts[0] {
        Stmt::If {
            binding_var,
            else_branch,
            ..
        } => {
            assert_eq!(binding_var.as_deref(), Some("a"));
            assert!(matches!(
                else_branch.first(),
                Some(Stmt::If {
                    binding_var: Some(next),
                    ..
                }) if next == "b"
            ));
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn parse_if_else_with_binding_var_desugars_to_var_decl() {
    let (rest, stmts) = program("if 0 { } else -> $c { say $c; }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    match &stmts[0] {
        Stmt::If {
            binding_var,
            else_branch,
            ..
        } => {
            let source = binding_var.as_ref().expect("generated if binding var");
            assert!(source.starts_with("$__mutsu_if_bind_"));
            assert!(matches!(
                else_branch.first(),
                Some(Stmt::VarDecl { name, expr, .. })
                    if name == "c"
                        && matches!(
                            expr,
                            Expr::Var(var) if var == source.trim_start_matches('$')
                        )
            ));
        }
        _ => panic!("Expected If statement"),
    }
}

#[test]
fn parse_for_loop() {
    let (rest, stmts) = program("for 1..10 -> $i { say $i; }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::For { .. }));
}

#[test]
fn parse_for_loop_with_rw_param_trait() {
    let (rest, stmts) = program("for @a -> $x is rw { $x++ }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::For { .. }));
}

#[test]
fn parse_for_loop_with_parenthesized_pointy_params() {
    let (rest, stmts) = program("for @a, @b -> ($x, $y) { say $x; say $y; }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(
        &stmts[0],
        Stmt::For {
            param: Some(name),
            param_def,
            ..
        } if name == "__for_unpack"
            && matches!(
                param_def.as_ref(),
                Some(def) if def.sub_signature.as_ref().is_some_and(|sig| sig.len() == 2)
            )
    ));
}

#[test]
fn parse_compound_assign_on_method_lhs() {
    let (rest, stmts) = program("for @a -> { .key //= ++$i }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
}

#[test]
fn parse_topic_method_assign_with_statement_modifier() {
    let (rest, stmts) = program(".key = 1 for @a;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
}

#[test]
fn parse_statement_modifier_for_with_trailing_comma() {
    let (rest, stmts) = program("my @x = gather { take $_ for 1, 2, }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
}

#[test]
fn parse_user_prefix_sub_with_looser_trait_as_full_rhs_expression() {
    let src = "sub prefix:<pIO> ($p) is looser(&[~]) { $p.IO }\nmy $pre = 'foo';\nsay pIO  $pre ~ '_BAR';";
    let (rest, stmts) = program(src).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 3);
    match &stmts[2] {
        Stmt::Say(args) => {
            assert_eq!(args.len(), 1);
            assert!(matches!(
                &args[0],
                Expr::Call { name, args }
                    if name == "prefix:<pIO>"
                        && args.len() == 1
                        && matches!(
                            &args[0],
                            Expr::Binary { op, .. } if matches!(op, TokenKind::Tilde)
                        )
            ));
        }
        _ => panic!("expected say statement"),
    }
}

#[test]
fn parse_block_with_unless_statement_modifier() {
    let (rest, stmts) = program("{ my $a = 1; } unless False;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(
        &stmts[0],
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } if then_branch.len() == 1
            && matches!(&then_branch[0], Stmt::Block(_))
            && else_branch.is_empty()
    ));
}

#[test]
fn parse_parenthesized_assign_lvalue_stmt() {
    let (rest, stmts) = program("($x = $y) = 5;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    match &stmts[0] {
        Stmt::Block(stmts) => {
            assert_eq!(stmts.len(), 2);
            assert!(matches!(
                &stmts[0],
                Stmt::Expr(Expr::AssignExpr { name, expr, .. })
                    if name == "x" && matches!(expr.as_ref(), Expr::Var(n) if n == "y")
            ));
            assert!(matches!(
                &stmts[1],
                Stmt::Assign { name, expr, .. }
                    if name == "x" && matches!(expr, Expr::Literal(Value::Int(5)))
            ));
        }
        other => panic!("expected block stmt, got {:?}", other),
    }
}

#[test]
fn parse_parenthesized_bind_lvalue_stmt() {
    let (rest, stmts) = program("($rv1, $rv2) := |(t2);").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::Block(_)));
}
