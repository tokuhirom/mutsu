use super::RakuAstClass;
use super::convert::convert_expr;
use crate::ast::Expr;
use crate::symbol::Symbol;

fn call(name: &str, args: Vec<Expr>) -> Expr {
    Expr::Call {
        name: Symbol::intern(name),
        args,
        listop: false,
    }
}

#[test]
fn unmarked_generated_identifiers_still_refuse() {
    let names = &crate::scope::NameScope::default();
    for expr in [
        Expr::Var("__generated_test".into()),
        Expr::ArrayVar("__generated_test".into()),
        Expr::HashVar("__generated_test".into()),
        call("__generated_test", Vec::new()),
    ] {
        assert!(convert_expr(names, &expr).is_err());
    }
}

#[test]
fn source_call_provenance_does_not_escape_into_arguments() {
    let names = &crate::scope::NameScope::default();
    let expr = Expr::source_identifier(
        call("__written_test", vec![call("__generated_test", Vec::new())]),
        "__written_test(...)",
    );
    let error = convert_expr(names, &expr).expect_err("the generated argument must still refuse");
    assert!(error.message.contains("__generated_test"));
}

#[test]
fn source_names_do_not_activate_generated_helper_conversion() {
    let names = &crate::scope::NameScope::default();
    let expr = Expr::source_identifier(call("__object_hash", Vec::new()), "__object_hash()");
    assert_eq!(
        convert_expr(names, &expr).unwrap().class,
        RakuAstClass::CallName
    );
    // The reserved name is lowered output now; unmarked, it is refused like any other.
    assert!(convert_expr(names, &call("__object_hash", Vec::new())).is_err());
}
