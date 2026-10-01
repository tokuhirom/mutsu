//! Tests for [`super`]: the scanners against the `regex` patterns they
//! replaced, and the Pod/heredoc blanking.

use super::*;

/// Every Raku module source under the repository's module trees.
fn module_sources() -> Vec<(std::path::PathBuf, String)> {
    fn walk(dir: &std::path::Path, out: &mut Vec<(std::path::PathBuf, String)>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(&path, out);
            } else if path
                .extension()
                .is_some_and(|e| e == "rakumod" || e == "pm6" || e == "pm")
                && let Ok(text) = std::fs::read_to_string(&path)
            {
                out.push((path, text));
            }
        }
    }
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut out = Vec::new();
    for dir in ["modules", "vendor", "roast/packages", "t"] {
        walk(&root.join(dir), &mut out);
    }
    assert!(out.len() > 50, "found only {} module sources", out.len());
    out
}

/// The `regex` patterns these scanners replaced (#10439).
fn old_exported_names(source: &str) -> Vec<(String, bool)> {
    let sub_re = regex::Regex::new(
            r"\b(?:our\s+)?(?:proto\s+|multi\s+)?sub\s+([A-Za-z_][A-Za-z0-9_'\-]*)\b([^;{]*)\bis\s+export\b(\s*\([^)]*\))?",
        )
        .unwrap();
    let proto_re = regex::Regex::new(
        r"\bproto\s+([A-Za-z_][A-Za-z0-9_'\-]*)\b([^;{]*)\bis\s+export\b(\s*\([^)]*\))?",
    )
    .unwrap();
    let ta = regex::Regex::new(r"\bis\s+test-assertion\b").unwrap();
    let mut names: std::collections::BTreeMap<String, bool> = Default::default();
    for re in [&sub_re, &proto_re] {
        for caps in re.captures_iter(source) {
            let is_ta = ta.is_match(caps.get(2).map_or("", |m| m.as_str()));
            *names.entry(caps[1].to_string()).or_insert(false) |= is_ta;
        }
    }
    names.into_iter().collect()
}

fn old_unit_scope_routine_names(source: &str) -> Vec<String> {
    let re = regex::Regex::new(
            r"(?m)^(?:my\s+|our\s+)?(?:proto\s+|multi\s+|only\s+)?sub\s+([A-Za-z_][A-Za-z0-9_'\-]*(?:::[A-Za-z_][A-Za-z0-9_'\-]*)*)",
        )
        .unwrap();
    let mut names: Vec<String> = re
        .captures_iter(source)
        .map(|c| c[1].to_string())
        .filter(|n| n != "EXPORT" && !n.contains("::"))
        .collect();
    names.sort();
    names.dedup();
    names
}

fn old_declares_export_sub(source: &str) -> bool {
    regex::Regex::new(r"(?m)^(?:my\s+|our\s+)?sub\s+EXPORT\b")
        .unwrap()
        .is_match(source)
}

#[test]
fn scanners_agree_with_the_regexes_they_replaced_on_every_module() {
    for (path, source) in module_sources() {
        let code = code_text(&source);
        let p = path.display();
        assert_eq!(exported_names(&code), old_exported_names(&code), "{p}");
        assert_eq!(
            unit_scope_routine_names(&code),
            old_unit_scope_routine_names(&code),
            "{p}"
        );
        assert_eq!(
            declares_export_sub(&code),
            old_declares_export_sub(&code),
            "{p}"
        );
    }
}

#[test]
fn scanners_agree_with_the_regexes_on_edge_spellings() {
    for code in [
        "sub foo-(Int) is export {}",
        "sub foo- is export;",
        "proto sub bar(|) is export {*}",
        "multi sub baz($) is test-assertion is export(:DEFAULT) {}",
        "sub a is export sub b is export;",
        "sub\nfoo() is export\n(:x)\n{}",
        "our sub q'x is export",
        "sub is export",
        "mysub foo is export;",
        "sub foo is exported;",
        "sub foo isexport;",
        "my sub EXPORT(*@a) { }",
        "our   sub  EXPORT { }",
        "sub EXPORTS { }",
        "  sub EXPORT { }",
        "my proto sub x(|) {*}\nmulti sub y() {}\nsub Foo::z() {}\nsub\n  w {}",
        "sub é is export;",
        "sub x\u{E9} is export;",
    ] {
        assert_eq!(exported_names(code), old_exported_names(code), "{code:?}");
        assert_eq!(
            unit_scope_routine_names(code),
            old_unit_scope_routine_names(code),
            "{code:?}"
        );
        assert_eq!(
            declares_export_sub(code),
            old_declares_export_sub(code),
            "{code:?}"
        );
    }
}

#[test]
fn pod_and_heredoc_bodies_are_not_code() {
    let source = "\
my $doc = q:to/END/;
sub EXPORT { }
sub from-heredoc() is export { }
END
=begin pod
sub EXPORT { }
sub from-pod() is export { }
=end pod
=for comment
sub from-para() is export { }

=head1 sub from-heading() is export
sub still-pod() is export { }

sub real() is export { }
=finish
sub EXPORT { }
";
    let code = code_text(source);
    assert_eq!(code.lines().count(), source.lines().count());
    assert!(!declares_export_sub(&code));
    assert_eq!(exported_names(&code), vec![("real".to_string(), false)]);
    assert_eq!(unit_scope_routine_names(&code), vec!["real".to_string()]);
}

#[test]
fn heredoc_terminators_cover_the_quoting_forms() {
    assert_eq!(heredoc_terminators("my $x = q:to/END/;"), vec!["END"]);
    assert_eq!(
        heredoc_terminators("f(qq:to<A>, Q:heredoc[B]);"),
        vec!["A", "B"]
    );
    assert!(heredoc_terminators("my %h = :todo<x>;").is_empty());
}
