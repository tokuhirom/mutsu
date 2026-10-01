use super::*;

fn parse(src: &str) -> Vec<Stmt> {
    crate::parser::parse_program(src).expect("parse").0
}

#[derive(Default)]
struct Names(Vec<(String, NameKind)>);

impl Visit for Names {
    fn visit_name(&mut self, name: &str, kind: NameKind) {
        self.0.push((name.to_string(), kind));
    }
}

fn names_of(src: &str) -> Vec<(String, NameKind)> {
    let mut n = Names::default();
    walk_stmts(&mut n, &parse(src));
    n.0
}

fn has(names: &[(String, NameKind)], name: &str, kind: NameKind) -> bool {
    names.iter().any(|(n, k)| n == name && *k == kind)
}

#[test]
fn string_literals_are_not_names() {
    let names = names_of(r#"say "return"; my %h = EVAL => 1; f(:samewith)"#);
    assert!(!names.iter().any(|(n, _)| n == "return"));
    assert!(!names.iter().any(|(n, _)| n == "EVAL"));
    assert!(!names.iter().any(|(n, _)| n == "samewith"));
}

#[test]
fn identifier_positions_are_typed() {
    let names = names_of("sub f(Int $x) { g($x); $x.m; &h }");
    assert!(has(&names, "f", NameKind::SubDecl));
    assert!(has(&names, "Int", NameKind::Type));
    assert!(has(&names, "g", NameKind::Call));
    assert!(has(&names, "m", NameKind::Method));
    assert!(has(&names, "h", NameKind::CodeVar));
}

#[test]
fn walk_descends_into_closures_and_parameters() {
    let names = names_of("my $c = -> $y = k() { { inner() } }");
    assert!(has(&names, "k", NameKind::Call));
    assert!(has(&names, "inner", NameKind::Call));
}

#[test]
fn contains_word_matches_whole_words_only() {
    assert!(contains_word("T", "T"));
    assert!(contains_word("Array[T]", "T"));
    assert!(contains_word("T:D", "T"));
    assert!(contains_word("Key-T", "T"));
    assert!(!contains_word("Test", "T"));
    assert!(!contains_word("KT", "T"));
    assert!(!contains_word("T_x", "T"));
}
