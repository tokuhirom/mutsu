use super::pure::analyse_stmt;
use crate::ast::Stmt;

/// The statements without their line markers, whose positions the round
/// trip does not keep.
fn shape(stmts: Vec<Stmt>) -> String {
    let stmts: Vec<Stmt> = stmts
        .into_iter()
        .filter(|s| !matches!(s, Stmt::SetLine(..)))
        .collect();
    format!("{stmts:?}")
}

fn round_tripped(source: &str) -> String {
    let (spelled, _) = crate::parse_dispatch::parse_source_spelled(source).unwrap();
    shape(crate::rakuast::frontend::round_trip(&spelled).unwrap())
}

fn ordinary(source: &str) -> String {
    shape(crate::parse_dispatch::parse_source(source).unwrap().0)
}

/// A pure family crosses the round trip as the tree an ordinary parse
/// builds: the leaf is that tree, not a rebuilt one.
#[test]
fn pure_families_cross_as_the_ordinary_tree() {
    for source in [
        "say 1 + 2;",
        "my $x = 5; say $x * 2;",
        "my @a = 1, 2, 3; say @a.map({ $_ + 1 });",
        "for 1..3 -> $i { say $i }\nsay 4;",
        "if 1 { say 'a' } else { say 'b' }\nsay 2;",
        "say 'x' if 1;",
        "my %h = a => 1; say %h<a>;",
        "my $s = 'abc'; say \"v: $s\";",
    ] {
        assert_eq!(round_tripped(source), ordinary(source), "{source}");
    }
}

/// A statement holding a construct the conversion still renders is not
/// carried as a leaf.
#[test]
fn a_regex_with_code_is_converted() {
    let (spelled, _) =
        crate::parse_dispatch::parse_source_spelled("say 'a' ~~ /a <?{ 1 }>/;").unwrap();
    let _scope = super::Scope::enter();
    assert!(analyse_stmt(&spelled[spelled.len() - 1]).is_none());
}

/// Outside a round trip nothing is carried: `.AST` renders every node.
#[test]
fn no_leaf_outside_a_round_trip() {
    let (spelled, _) = crate::parse_dispatch::parse_source_spelled("say 1;").unwrap();
    assert!(super::stmt_leaf(&spelled[spelled.len() - 1], None).is_none());
}
