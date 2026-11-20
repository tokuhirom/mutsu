//! The finalization of a unit is a pure function of the statements and the
//! `NameScope` (ADR-15760 §7.5, condition 1): finalizing the same statements
//! again gives the same tree, and a finalization that runs while another unit's
//! state is installed leaves that state as it found it.

use crate::ast::Stmt;
use crate::scope::NameScope;

/// The statements without their line markers.
fn shape(stmts: Vec<Stmt>) -> String {
    let stmts: Vec<Stmt> = stmts
        .into_iter()
        .filter(|s| !matches!(s, Stmt::SetLine(..)))
        .collect();
    format!("{stmts:?}")
}

fn finalized(names: &NameScope, source: &str) -> String {
    let (parsed, _) = crate::parse_dispatch::parse_source_unfinalized(source).expect("it parses");
    shape(crate::rakuast::frontend::finish_unit_in(names, parsed).expect("it finalizes"))
}

/// Units that exercise each piece of unit state: an operator routine (the
/// per-list operator scopes), a redeclared `True` (the keyword scopes), a
/// shadowed setting constant, a sigilless term and a routine word (the unit
/// facts).
const SOURCES: &[&str] = &[
    "sub infix:<plus>($a, $b) { $a + $b }; say 1 plus 2; { say 3 plus 4 }",
    "{ constant True = 42; say True }; say True",
    "constant e = 3; say e + 1; my \\x = 5; say x;",
    "sub hello { 'hi' }; say hello; sub f(\\pi) { pi }; say f(2); say pi > 3;",
];

#[test]
fn finalizing_again_gives_the_same_tree() {
    for source in SOURCES {
        let names = NameScope::default();
        let first = finalized(&names, source);
        // The same scope value, reused: nothing of the first unit is left in it.
        assert_eq!(finalized(&names, source), first, "{source}");
        assert_eq!(finalized(&NameScope::default(), source), first, "{source}");
    }
}

#[test]
fn a_finalization_inside_another_unit_restores_its_state() {
    let names = NameScope::default();
    let (outer, _) = crate::parse_dispatch::parse_source_unfinalized(
        "constant e = 3; sub hello { 1 }; my \\x = 2;",
    )
    .expect("it parses");
    let _unit = super::super::shadowed_terms::scan_stmts(&names, &outer);
    let _operators = super::super::infix_func::enter_scope(&names);
    assert!(super::super::shadowed_terms::is_shadowed(&names, "e"));
    assert!(super::super::declared_routines::is_declared(&names, "hello"));
    for source in SOURCES {
        let _ = finalized(&names, source);
    }
    assert!(super::super::shadowed_terms::is_shadowed(&names, "e"));
    assert!(super::super::shadowed_terms::is_declared_term(&names, "x"));
    assert!(super::super::declared_routines::is_declared(&names, "hello"));
    assert!(!super::super::declared_routines::is_declared(&names, "f"));
    assert!(!super::super::shadowed_terms::is_shadowed(&names, "True"));
}
