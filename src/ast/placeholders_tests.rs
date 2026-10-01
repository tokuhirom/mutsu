use super::*;

fn parse(src: &str) -> Vec<Stmt> {
    crate::parser::parse_program(src).expect("parse").0
}

const NESTED: &str = "$^a + 1; if True { $^b }; $^c if True; my &f = sub { $^d };";

#[test]
fn own_scope_stops_at_signature_taking_blocks() {
    assert_eq!(collect_placeholders_shallow(&parse(NESTED)), ["^a", "^c"]);
}

#[test]
fn deep_scope_enters_nested_blocks_and_closures() {
    assert_eq!(
        collect_placeholders(&parse(NESTED)),
        ["^a", "^b", "^c", "^d"]
    );
}

#[test]
fn deep_scope_stops_at_routine_declarations() {
    assert!(collect_placeholders(&parse("sub f { $^a }")).is_empty());
}

#[test]
fn unattached_stops_at_every_block_but_a_whatevercode() {
    let stmts = parse(r#"say $^a; say "{$^b}"; do { say $^c }; say * + $^d; say @_; say %_<k>"#);
    assert_eq!(
        collect_unattached_placeholders(&stmts),
        ["$^a", "$^d", "@_", "%_"]
    );
}

#[test]
fn where_assign_collects_targets_in_nested_blocks() {
    let stmts = parse("$^x = 1; if True { $^y = 2 }; $^z + 1");
    assert_eq!(collect_where_assign_placeholders(&stmts), ["^x", "^y"]);
}

#[test]
fn source_text_parsed_later_is_scanned() {
    assert_eq!(
        collect_placeholders_shallow(&parse("my $s = 'x'; $s ~~ s/x/$^r/;")),
        ["^r"]
    );
}
