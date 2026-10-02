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

/// Renames every `$x` to `$renamed`.
struct RenameVar;

impl VisitMut for RenameVar {
    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        if let Expr::Var(name) = expr
            && name == "x"
        {
            *name = "renamed".to_string();
        }
        walk_expr_mut(self, expr);
    }
}

fn var_count(stmts: &[Stmt], name: &str) -> usize {
    let mut n = Names::default();
    walk_stmts(&mut n, stmts);
    n.0.iter()
        .filter(|(n, k)| n == name && *k == NameKind::Var)
        .count()
}

#[test]
fn visit_mut_reaches_nested_positions() {
    // A pointy default, a nested block, a `where` clause, a regex code
    // block and an attribute default.
    let mut stmts = parse(
        "my $c = -> $y = $x { { f($x) } }; sub g(:$z where $x) { $x ~~ / <?{ $x }> / }; \
         class C { has $.a = $x }",
    );
    let before = var_count(&stmts, "x");
    assert!(before >= 6, "{before}");
    walk_stmts_mut(&mut RenameVar, &mut stmts);
    assert_eq!(var_count(&stmts, "x"), 0);
    assert_eq!(var_count(&stmts, "renamed"), before);
}

#[test]
fn walk_param_mut_resets_param_code() {
    let mut stmts = parse("sub f($a = 1 + 2) { }");
    let first_param = |stmts: &[Stmt]| {
        stmts
            .iter()
            .find_map(|s| match s {
                Stmt::SubDecl { param_defs, .. } => Some(param_defs[0].code.clone()),
                _ => None,
            })
            .expect("a sub")
    };
    let old = first_param(&stmts);
    old.fill(|| crate::ast::ParamChunks {
        where_chunk: None,
        where_inline_predicate: false,
        default_chunk: None,
        shape_chunks: Vec::new(),
    });
    assert!(first_param(&stmts).is_filled());
    walk_stmts_mut(&mut RenameVar, &mut stmts);
    assert!(!first_param(&stmts).is_filled());
    // Only the rewritten node gets a fresh slot; other holders keep theirs.
    assert!(old.is_filled());
}
