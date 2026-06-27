//! Unit tests for `stmt` parsing (split out of mod.rs, part 2).

use super::*;
use crate::ast::Expr;
use crate::value::Value;

#[test]
fn parse_for_loop_with_optional_pointy_param() {
    let (rest, stmts) = program("for 1..5 -> $x, $y? { say $x }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::For { .. }));
}

#[test]
fn parse_for_loop_with_default_pointy_param() {
    let (rest, stmts) = program("for 1..5 -> $x, $y = 7 { say $x }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::For { .. }));
}

#[test]
fn parse_for_loop_with_pointy_return_type() {
    let (rest, stmts) = program("for ^10 -> $_ --> List { take $_ }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::For { .. }));
}

#[test]
fn parse_for_with_typed_unpacking_param_signature() {
    let (rest, stmts) = program("for a => 1 -> Pair $p (:$key, :$value) { $p }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::For { .. }));
}

#[test]
fn parse_for_with_anonymous_unpacking_param_signature() {
    let (rest, stmts) = program("for A.new -> $ (:$x) { $x }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::For { .. }));
}

#[test]
fn parse_for_with_parenthesized_positional_destructuring_param() {
    let (rest, stmts) = program("for @pairs -> ($a, $b) { $a }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    match &stmts[0] {
        Stmt::For {
            param: Some(param),
            param_def,
            ..
        } => {
            assert_eq!(param, "__for_unpack");
            let def = param_def.as_ref().as_ref().expect("for param_def");
            let sub = def.sub_signature.as_ref().expect("for sub_signature");
            assert_eq!(sub.len(), 2);
        }
        _ => panic!("expected for statement with destructuring param"),
    }
}

#[test]
fn parse_for_with_parenthesized_iterable_expression() {
    let (rest, stmts) = program("for (@a) { .say }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    match &stmts[0] {
        Stmt::For { iterable, .. } => {
            assert!(matches!(iterable, Expr::ArrayVar(name) if name == "a"));
        }
        _ => panic!("expected for statement"),
    }
}

#[test]
fn parse_for_with_operator_code_ref_in_iterable_list() {
    let src = "for &infix:<<(==)>>, \"(==)\", &infix:<≡>, \"≡\" -> &op, $name { }";
    let (rest, stmts) = program(src).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::For { .. }));
}

#[test]
fn parse_chained_inline_modifiers_in_paren_expr() {
    let (rest, stmts) = program("my @odd = ($_ * $_ if $_ % 2 for 0..10);").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
}

#[test]
fn parse_for_expression_in_parens() {
    let (rest, stmts) = program("my $x = (for ^2 { 41; 42 });").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
}

#[test]
fn parse_topic_mutating_method_stmt() {
    let (rest, stmts) = program(".=fmt('%03b');").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    // .=method on topic assigns back to $_
    assert!(matches!(&stmts[0], Stmt::Assign { name, .. } if name == "_"));
}

#[test]
fn parse_paren_expr_mutating_method_stmt() {
    let (rest, stmts) = program("(class { method foo() { self } }.new).=foo;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    // .= on non-lvalue expression is parsed as a method call in expression context
    assert!(matches!(&stmts[0], Stmt::Expr(Expr::MethodCall { .. })));
}

#[test]
fn parse_my_class_expr_in_parens() {
    let (rest, stmts) =
        program("(my class :: does Real { method Num { 42e0 } }.new.Bridge);").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::Expr(_)));
}

#[test]
fn parse_gather_for_expression() {
    let (rest, stmts) = program("my $x = (gather for ^5 { take 1 });").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
}

#[test]
fn parse_labeled_for_expression() {
    let (rest, stmts) = program("my @x = (MEOW: for ^2 { 1 });").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
}

#[test]
fn parse_sub_param_dollar_question() {
    let (rest, stmts) = program("sub foo($?) { 1 }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::SubDecl { .. }));
}

#[test]
fn parse_topic_amp_call_inside_for_block() {
    let (rest, stmts) = program("sub foo($?) { 1 }; for 1 { .&foo() };").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
    assert!(matches!(&stmts[0], Stmt::SubDecl { .. }));
    assert!(matches!(&stmts[1], Stmt::For { .. }));
}

#[test]
fn parse_block_valued_colonpair() {
    let (rest, stmts) = program("my $x = :out{ .contains('ok') };").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
}

#[test]
fn parse_block_valued_colonpair_with_topic_amp_call_colon_args() {
    let (rest, stmts) = program("my $x = :out{ .&output-has: :0noks, :0fails, :0todos };").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
}

#[test]
fn parse_my_regex_decl() {
    let (rest, stmts) = program("my regex rx { abc };").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(
        &stmts[0],
        Stmt::TokenDecl { name, body, .. }
            if name == "rx"
                && matches!(
                    body.as_slice(),
                    [Stmt::Expr(Expr::Literal(Value::Regex(pattern)))] if pattern.as_str() == "abc"
                )
    ));
}

#[test]
fn parse_sub_decl() {
    let (rest, stmts) = program("sub foo($x) { return $x; }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::SubDecl { name, .. } if name == "foo"));
}

#[test]
fn parse_sub_decl_with_compact_anonymous_sigil_params() {
    let (rest, stmts) = program("sub foo($$$$) { 1 }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    if let Stmt::SubDecl { param_defs, .. } = &stmts[0] {
        assert_eq!(param_defs.len(), 4);
        assert!(
            param_defs
                .iter()
                .all(|p| p.name == "__ANON_STATE__" && !p.named && !p.slurpy)
        );
    } else {
        panic!("expected SubDecl");
    }
}

#[test]
fn parse_forward_sub_declaration_with_semicolon() {
    let (rest, stmts) = program("sub foo($$$$); sub foo($$$$) { 1 }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
    if let Stmt::SubDecl {
        body, param_defs, ..
    } = &stmts[0]
    {
        assert!(body.is_empty());
        assert_eq!(param_defs.len(), 4);
    } else {
        panic!("expected SubDecl");
    }
}

#[test]
fn parse_declared_postfix_operator_call_in_following_statement() {
    let (rest, stmts) = program("sub postfix:<!!>($x) { $x }; (4!!, 5!!).join(' ');").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
    assert!(matches!(&stmts[0], Stmt::SubDecl { name, .. } if name == "postfix:<!!>"));
    assert!(matches!(&stmts[1], Stmt::Expr(Expr::MethodCall { .. })));
}

#[test]
fn parse_forward_sub_without_signature_is_invalid() {
    let err = program("sub foo;").expect_err("expected parse error");
    assert!(err.to_string().contains("X::UnitScope::Invalid"));
}

#[test]
fn parse_ternary_statement() {
    let (rest, stmts) = program("1 ?? 2 !! 3;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::Expr(Expr::Ternary { .. })));
}

#[test]
fn parse_main_semicolon_captures_following_mainline() {
    let (rest, stmts) = program("my @*ARGS = <x>; sub MAIN($a); say $a;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 2);
    assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
    if let Stmt::SubDecl { name, body, .. } = &stmts[1] {
        assert_eq!(name, "MAIN");
        assert_eq!(body.len(), 1);
        assert!(matches!(&body[0], Stmt::Say(_)));
    } else {
        panic!("expected MAIN SubDecl");
    }
}

#[test]
fn parse_unit_main_semicolon_captures_following_mainline() {
    let (rest, stmts) = program("unit sub MAIN($a); say $a;").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    if let Stmt::SubDecl { name, body, .. } = &stmts[0] {
        assert_eq!(name, "MAIN");
        assert_eq!(body.len(), 1);
        assert!(matches!(&body[0], Stmt::Say(_)));
    } else {
        panic!("expected MAIN SubDecl");
    }
}

#[test]
fn parse_main_semicolon_after_module_is_too_late() {
    let err = program("module AtBeginning {}; sub MAIN;").expect_err("expected parse error");
    assert!(err.to_string().contains("X::UnitScope::TooLate"));
}

#[test]
fn parse_sub_decl_with_typed_slurpy_param() {
    let (rest, stmts) = program("sub foo (Code *$block) { return $block.(); }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    if let Stmt::SubDecl { param_defs, .. } = &stmts[0] {
        assert_eq!(param_defs.len(), 1);
        assert!(param_defs[0].slurpy);
        assert_eq!(param_defs[0].type_constraint.as_deref(), Some("Code"));
    } else {
        panic!("expected SubDecl");
    }
}

#[test]
fn parse_sub_decl_with_slurpy_code_param() {
    let (rest, stmts) = program("sub bar (*&block) { return &block.(); }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    if let Stmt::SubDecl { param_defs, .. } = &stmts[0] {
        assert_eq!(param_defs.len(), 1);
        assert!(param_defs[0].slurpy);
    } else {
        panic!("expected SubDecl");
    }
}

#[test]
fn parse_multi_sub_with_alternate_signature_chain() {
    let src = "multi sub infix:<+> (Base $b, Exponent $e) | (Exponent $e, Base $b) { 1 }";
    let (rest, stmts) = program(src).unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    if let Stmt::SubDecl {
        multi,
        signature_alternates,
        ..
    } = &stmts[0]
    {
        assert!(*multi);
        assert_eq!(signature_alternates.len(), 1);
    } else {
        panic!("expected SubDecl");
    }
}

#[test]
fn parse_class_decl() {
    let (rest, stmts) = program("class Foo { has $.x; method bar { 42 } }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(&stmts[0], Stmt::ClassDecl { name, .. } if name == "Foo"));
}

#[test]
fn parse_class_decl_with_is_rw_trait() {
    let (rest, stmts) = program("class Foo is rw { has $.x }").unwrap();
    assert_eq!(rest, "");
    assert_eq!(stmts.len(), 1);
    assert!(matches!(
        &stmts[0],
        Stmt::ClassDecl {
            name,
            class_is_rw: true,
            ..
        } if name == "Foo"
    ));
}
