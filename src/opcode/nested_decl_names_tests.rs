//! `class_nested_decl_names` is stored in a compiled declaration plan, which
//! `MUTSU_PRECOMP_VERIFY` compares byte-for-byte between a cached and a fresh
//! compile: its order must not follow a hash set's per-process order.
use super::*;

#[test]
fn nested_decl_names_are_sorted() {
    let parsed = crate::parser::parse_program(
        "class A { for 1..2 { my $zeta; my $alpha; my $mid; my $beta; my $omega } }",
    )
    .ok();
    assert!(parsed.is_some(), "the class parses");
    let stmts = parsed.map(|(stmts, _)| stmts).unwrap_or_default();
    let body = stmts.iter().find_map(|s| match s {
        Stmt::ClassDecl { body, .. } => Some(body),
        _ => None,
    });
    assert!(body.is_some(), "a class declaration");
    let body = body.map(Vec::as_slice).unwrap_or_default();
    let names: Vec<String> = class_nested_decl_names(body)
        .iter()
        .map(|sym| sym.resolve().to_string())
        .collect();
    assert!(names.len() >= 5, "all five loop `my`s are found: {names:?}");
    let mut sorted = names.clone();
    sorted.sort();
    assert_eq!(names, sorted, "names are in a deterministic order");
}
