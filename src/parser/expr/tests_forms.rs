//! The source form the parser records for constructs the compiler treats
//! alike (`Binary.form`, `MethodCall.on_topic`, `Index.spelling`): the RakuAST
//! boundary renders the node raku has for each, so the form must survive the
//! parse -- both the bare expression parser and the whole-program one, which
//! runs the post-parse passes over the tree.

use super::*;
use crate::ast::{BinaryForm, IndexSpelling};

/// The expression of the last statement of `source`, parsed as a program.
fn program_expr(source: &str) -> Expr {
    let (stmts, _) = crate::parser::parse_program(source).unwrap();
    match stmts.into_iter().rev().find_map(|stmt| match stmt {
        Stmt::Expr(expr) => Some(expr),
        _ => None,
    }) {
        Some(expr) => expr,
        None => panic!("no expression statement in {source:?}"),
    }
}

#[test]
fn prefix_caret_keeps_its_form() {
    let (_, caret) = expression("^5").unwrap();
    assert!(matches!(
        caret,
        Expr::Binary {
            form: BinaryForm::CaretPrefix,
            ..
        }
    ));
    let (_, written) = expression("0 ..^ 5").unwrap();
    assert!(matches!(
        written,
        Expr::Binary {
            form: BinaryForm::Infix,
            ..
        }
    ));
    assert!(matches!(
        program_expr("^5"),
        Expr::Binary {
            form: BinaryForm::CaretPrefix,
            ..
        }
    ));
}

#[test]
fn colonpairs_keep_their_form() {
    for (source, want) in [
        ("foo(:a(1))", BinaryForm::ColonPairValue),
        ("foo(:a)", BinaryForm::ColonPairTrue),
        ("foo(:!a)", BinaryForm::ColonPairFalse),
        ("foo(a => 1)", BinaryForm::Infix),
    ] {
        let Expr::Call { args, .. } = program_expr(source) else {
            panic!("{source}: not a call");
        };
        let [Expr::Binary { form, .. }] = args.as_slice() else {
            panic!("{source}: not one pair argument");
        };
        assert_eq!(*form, want, "{source}");
    }
}

#[test]
fn topic_calls_keep_their_form() {
    for (source, want) in [(".say", true), ("$_.say", false)] {
        let Expr::MethodCall { on_topic, .. } = program_expr(source) else {
            panic!("{source}: not a method call");
        };
        assert_eq!(on_topic, want, "{source}");
    }
}

#[test]
fn angle_subscripts_keep_their_form() {
    for (source, want) in [
        ("my %h; %h<a>", IndexSpelling::Angle),
        ("my %h; %h<a b>", IndexSpelling::Angle),
        ("my %h; %h{'a'}", IndexSpelling::Subscript),
    ] {
        let Expr::Index { spelling, .. } = program_expr(source) else {
            panic!("{source}: not an index");
        };
        assert_eq!(spelling, want, "{source}");
    }
}
