//! The declarations a document contains, at line granularity (ADR-0065 S4).
//!
//! This is the answer to "where is `Foo` defined" that an agent would otherwise
//! get by grepping — and grep's false positives (a mention in a comment, a
//! string, an unrelated same-named method) are exactly what makes it a poor
//! substitute.
//!
//! **Line granularity, on purpose.** mutsu's AST carries no positions at all;
//! the only positional information is `Stmt::SetLine`, a marker statement the
//! parser interleaves into every statement list, including the body of a class
//! or a routine. Walking the statements while tracking the most recent marker
//! therefore yields a declaration's line for free, with no span retrofit
//! (ADR-0065 D6). A declaration's *end* is approximated by the deepest marker
//! seen inside its body, so it stops at the last statement rather than at the
//! closing brace.
//!
//! **Works on a broken document.** Collection runs over a recovering parse, so
//! a file with a syntax error still yields the symbols around it — which is the
//! point, since a document under edit is broken most of the time (S3).

use crate::ast::{PackageKind, ParamDef, Stmt};

/// What a declaration declares. Deliberately mutsu's own vocabulary rather than
/// LSP's `SymbolKind`, which has no spelling for a role, a grammar token or a
/// subset; the server maps this to the nearest LSP kind at its boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Module,
    Package,
    Class,
    Grammar,
    Role,
    Subset,
    Enum,
    EnumMember,
    Sub,
    Method,
    /// `method !name` — visible in an outline, but not callable from outside.
    PrivateMethod,
    Token,
    Rule,
    /// `has $.x`
    Attribute,
    Variable,
}

impl SymbolKind {
    /// Whether a declaration of this kind holds other declarations worth
    /// listing — used to decide where a bare `my $x` is an outline entry (a
    /// class body, a module, the mainline) and where it is a local (a routine).
    fn is_package_like(self) -> bool {
        matches!(
            self,
            SymbolKind::Module
                | SymbolKind::Package
                | SymbolKind::Class
                | SymbolKind::Grammar
                | SymbolKind::Role
        )
    }
}

/// One declaration, and the ones nested inside it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    /// 1-based line the declaration is on.
    pub line: u32,
    /// 1-based last line the declaration covers, best effort: the deepest
    /// `Stmt::SetLine` seen inside its body. Never less than `line`.
    pub end_line: u32,
    /// The rendered signature, for a routine that has one: `(Int $n, :$verbose
    /// --> Str)`. Reconstructed from the parsed parameters rather than from the
    /// source text, so it reflects what mutsu actually understood the signature
    /// to be — which is the useful thing to show someone writing for mutsu.
    pub signature: Option<String>,
    pub children: Vec<Symbol>,
}

/// Every declaration in `source`, in source order, nested.
///
/// Never fails: a document that does not parse yields the declarations that
/// survived recovery, and one that parses to nothing yields an empty list.
pub fn symbols(source: &str) -> Vec<Symbol> {
    let collected = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let (stmts, _finish, _errors) = crate::parser::parse_program_recovering(source);
        let mut line = 1;
        collect(&stmts, &mut line, false)
    }));
    collected.unwrap_or_default()
}

/// Walk `stmts`, tracking the running line, and return the declarations found.
///
/// `in_routine` suppresses plain variable declarations: `my $x` at the top of a
/// class or a file is an outline entry, the same line inside a `sub` body is a
/// local and would bury the outline in noise.
fn collect(stmts: &[Stmt], line: &mut u32, in_routine: bool) -> Vec<Symbol> {
    let mut out = Vec::new();
    for stmt in stmts {
        match stmt {
            Stmt::SetLine(n) => {
                if *n > 0 {
                    *line = *n as u32;
                }
            }
            Stmt::ClassDecl {
                name,
                parents,
                body,
                ..
            } => {
                // A `grammar` parses to a `ClassDecl` whose implicit parent is
                // `Grammar`; there is no separate variant to match on.
                let kind = if parents.iter().any(|p| p == "Grammar") {
                    SymbolKind::Grammar
                } else {
                    SymbolKind::Class
                };
                out.push(declaration(name.resolve(), kind, line, body, false));
            }
            Stmt::RoleDecl { name, body, .. } => {
                out.push(declaration(
                    name.resolve(),
                    SymbolKind::Role,
                    line,
                    body,
                    false,
                ));
            }
            Stmt::Package {
                name, kind, body, ..
            } => {
                let kind = match kind {
                    PackageKind::Module => SymbolKind::Module,
                    PackageKind::Package => SymbolKind::Package,
                    PackageKind::Grammar => SymbolKind::Grammar,
                };
                out.push(declaration(name.resolve(), kind, line, body, false));
            }
            Stmt::SubDecl {
                name,
                body,
                param_defs,
                return_type,
                ..
            } => {
                out.push(declaration_with_signature(
                    name.resolve(),
                    SymbolKind::Sub,
                    line,
                    body,
                    true,
                    render_signature(param_defs, return_type.as_deref()),
                ));
            }
            Stmt::ProtoDecl {
                name,
                body,
                is_method,
                ..
            } => {
                let kind = if *is_method {
                    SymbolKind::Method
                } else {
                    SymbolKind::Sub
                };
                out.push(declaration(name.resolve(), kind, line, body, true));
            }
            Stmt::MethodDecl {
                name,
                body,
                is_private,
                param_defs,
                return_type,
                ..
            } => {
                let kind = if *is_private {
                    SymbolKind::PrivateMethod
                } else {
                    SymbolKind::Method
                };
                out.push(declaration_with_signature(
                    name.resolve(),
                    kind,
                    line,
                    body,
                    true,
                    render_signature(param_defs, return_type.as_deref()),
                ));
            }
            Stmt::TokenDecl { name, body, .. } => {
                out.push(declaration(
                    name.resolve(),
                    SymbolKind::Token,
                    line,
                    body,
                    true,
                ));
            }
            Stmt::RuleDecl { name, body, .. } => {
                out.push(declaration(
                    name.resolve(),
                    SymbolKind::Rule,
                    line,
                    body,
                    true,
                ));
            }
            Stmt::ProtoToken { name } => out.push(leaf(name.resolve(), SymbolKind::Token, *line)),
            Stmt::SubsetDecl { name, .. } => {
                out.push(leaf(name.resolve(), SymbolKind::Subset, *line))
            }
            Stmt::EnumDecl { name, variants, .. } => {
                let mut symbol = leaf(name.resolve(), SymbolKind::Enum, *line);
                symbol.children = variants
                    .iter()
                    .map(|(variant, _)| leaf(variant.clone(), SymbolKind::EnumMember, *line))
                    .collect();
                out.push(symbol);
            }
            Stmt::HasDecl { name, .. } => {
                out.push(leaf(name.resolve(), SymbolKind::Attribute, *line))
            }
            Stmt::VarDecl { name, .. } if !in_routine && !name.is_empty() => {
                out.push(leaf(name.clone(), SymbolKind::Variable, *line))
            }
            _ => {}
        }
    }
    out
}

fn leaf(name: String, kind: SymbolKind, line: u32) -> Symbol {
    Symbol {
        name,
        kind,
        line,
        end_line: line,
        signature: None,
        children: Vec::new(),
    }
}

/// Render a parsed signature back to Raku source form.
///
/// An approximation on purpose: `where` clauses, sub-signatures and default
/// *expressions* are dropped, because rendering an expression back to source
/// needs a printer mutsu does not have, and a half-rendered default would be a
/// diagnostic that lies. What is kept — type, sigil, name, optionality,
/// namedness, slurpiness, return type — is what a caller needs.
pub(crate) fn render_signature(params: &[ParamDef], return_type: Option<&str>) -> Option<String> {
    let rendered: Vec<String> = params
        .iter()
        .filter(|p| !p.is_invocant)
        .map(render_param)
        .collect();
    if rendered.is_empty() && return_type.is_none() {
        return Some("()".to_string());
    }
    let mut inner = rendered.join(", ");
    if let Some(ret) = return_type {
        if !inner.is_empty() {
            inner.push(' ');
        }
        inner.push_str(&format!("--> {ret}"));
    }
    Some(format!("({inner})"))
}

fn render_param(param: &ParamDef) -> String {
    let mut out = String::new();
    if let Some(constraint) = &param.type_constraint {
        out.push_str(constraint);
        out.push(' ');
    }
    if param.named {
        out.push(':');
    }
    let prefix = if param.double_slurpy {
        "**"
    } else if param.onearg {
        "+"
    } else if param.slurpy {
        "*"
    } else {
        ""
    };
    out.push_str(prefix);
    out.push_str(&sigil_name(param));
    // Raku spells the two optionality cases differently: a *named* parameter is
    // optional unless marked `!`, a positional is required unless marked `?`.
    // `required` in the AST only ever means "a named parameter written with
    // `!`" — a mandatory positional carries no flag at all, which is why the
    // positional case reads `optional_marker` instead.
    if param.named && param.required {
        out.push('!');
    } else if !param.named && param.optional_marker {
        out.push('?');
    }
    if param.default.is_some() {
        out.push_str(" = ...");
    }
    out
}

/// A parameter's name with its sigil.
///
/// `ParamDef::name` holds `@rest` for an array but a bare `a` for `$a`: the
/// scalar sigil is stripped at parse time. Putting it back is what makes a
/// rendered signature read like the source it came from.
fn sigil_name(param: &ParamDef) -> String {
    if param.sigilless || param.name.starts_with(['$', '@', '%', '&']) {
        param.name.clone()
    } else {
        format!("${}", param.name)
    }
}

/// Build a declaration and walk its body, using the running line cursor to
/// approximate where the declaration ends.
fn declaration(
    name: String,
    kind: SymbolKind,
    line: &mut u32,
    body: &[Stmt],
    body_is_routine: bool,
) -> Symbol {
    declaration_with_signature(name, kind, line, body, body_is_routine, None)
}

fn declaration_with_signature(
    name: String,
    kind: SymbolKind,
    line: &mut u32,
    body: &[Stmt],
    body_is_routine: bool,
    signature: Option<String>,
) -> Symbol {
    let start = *line;
    // The body's own `SetLine` markers advance the shared cursor; wherever it
    // ends up is the last line the declaration demonstrably covers. The closing
    // brace is not counted, because nothing marks it.
    let children = collect(body, line, body_is_routine || !kind.is_package_like());
    let end_line = (*line).max(start);
    Symbol {
        name,
        kind,
        line: start,
        end_line,
        signature,
        children,
    }
}

/// Find `name` and its enclosing chain, depth-first, in source order.
///
/// Returns the *first* declaration of that name. mutsu's own `multi` dispatch
/// makes several declarations of one name normal, and picking the first is what
/// a reader following a reference wants: the rest are found from there.
pub fn find(symbols: &[Symbol], name: &str) -> Option<Symbol> {
    for symbol in symbols {
        if symbol.name == name {
            return Some(symbol.clone());
        }
        if let Some(found) = find(&symbol.children, name) {
            return Some(found);
        }
    }
    None
}

/// Flatten the tree, pairing each symbol with the name of its container.
pub fn flatten(symbols: &[Symbol]) -> Vec<(Symbol, Option<String>)> {
    let mut out = Vec::new();
    push_flat(symbols, None, &mut out);
    out
}

fn push_flat(symbols: &[Symbol], container: Option<&str>, out: &mut Vec<(Symbol, Option<String>)>) {
    for symbol in symbols {
        push_flat(&symbol.children, Some(&symbol.name), out);
        out.push((
            Symbol {
                children: Vec::new(),
                ..symbol.clone()
            },
            container.map(str::to_string),
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(symbols: &[Symbol]) -> Vec<(String, SymbolKind, u32)> {
        symbols
            .iter()
            .map(|s| (s.name.clone(), s.kind, s.line))
            .collect()
    }

    #[test]
    fn a_document_yields_its_declarations_in_source_order() {
        let source = "\
my $top = 1;
sub baz() { 1 }
class Foo {
    has $.x;
    method bar($y) { $y }
}
";
        let found = symbols(source);
        assert_eq!(
            kinds(&found),
            vec![
                ("top".to_string(), SymbolKind::Variable, 1),
                ("baz".to_string(), SymbolKind::Sub, 2),
                ("Foo".to_string(), SymbolKind::Class, 3),
            ],
            "{found:#?}"
        );
        let foo = &found[2];
        assert_eq!(
            kinds(&foo.children),
            vec![
                ("x".to_string(), SymbolKind::Attribute, 4),
                ("bar".to_string(), SymbolKind::Method, 5),
            ]
        );
    }

    #[test]
    fn a_grammar_is_not_reported_as_a_plain_class() {
        let found = symbols("grammar G {\n    token TOP { <x> }\n    rule x { 'a' }\n}\n");
        assert_eq!(found.len(), 1);
        assert_eq!(found[0].kind, SymbolKind::Grammar);
        assert_eq!(
            kinds(&found[0].children),
            vec![
                ("TOP".to_string(), SymbolKind::Token, 2),
                ("x".to_string(), SymbolKind::Rule, 3),
            ]
        );
    }

    #[test]
    fn roles_modules_enums_and_subsets_all_appear() {
        let found = symbols(
            "role R { method m() {} }\nmodule M { sub s() {} }\nenum E <a b>;\nsubset S of Int;\n",
        );
        assert_eq!(
            kinds(&found),
            vec![
                ("R".to_string(), SymbolKind::Role, 1),
                ("M".to_string(), SymbolKind::Module, 2),
                ("E".to_string(), SymbolKind::Enum, 3),
                ("S".to_string(), SymbolKind::Subset, 4),
            ],
            "{found:#?}"
        );
        assert_eq!(
            kinds(&found[2].children),
            vec![
                ("a".to_string(), SymbolKind::EnumMember, 3),
                ("b".to_string(), SymbolKind::EnumMember, 3),
            ]
        );
    }

    /// A local inside a routine is not an outline entry; the same declaration
    /// at the top of a class or a file is.
    #[test]
    fn locals_inside_a_routine_are_not_outline_entries() {
        let found = symbols("sub f() {\n    my $local = 1;\n    $local\n}\nmy $outer = 2;\n");
        assert_eq!(
            kinds(&found),
            vec![
                ("f".to_string(), SymbolKind::Sub, 1),
                ("outer".to_string(), SymbolKind::Variable, 5),
            ],
            "{found:#?}"
        );
        assert!(found[0].children.is_empty(), "{:#?}", found[0]);
    }

    #[test]
    fn a_declaration_covers_the_lines_of_its_body() {
        let found = symbols("class C {\n    method a() { 1 }\n    method b() { 2 }\n}\nsay 1;\n");
        assert_eq!(found[0].line, 1);
        assert_eq!(
            found[0].end_line, 3,
            "the last statement inside the class is on line 3"
        );
    }

    /// ADR-0065 S3's payoff: a document under edit is broken most of the time,
    /// and its outline must survive that.
    #[test]
    fn a_document_that_does_not_parse_still_yields_its_other_declarations() {
        let found = symbols("sub good() { 1 }\nsay $c.f (1, 2);\nclass Later { }\n");
        let names: Vec<&str> = found.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"good"), "{found:#?}");
        assert!(names.contains(&"Later"), "{found:#?}");
    }

    #[test]
    fn find_returns_a_nested_declaration() {
        let found = symbols("class Foo {\n    method bar() { 1 }\n}\n");
        let bar = find(&found, "bar").expect("nested method");
        assert_eq!(bar.kind, SymbolKind::Method);
        assert_eq!(bar.line, 2);
        assert!(find(&found, "nope").is_none());
    }

    #[test]
    fn flatten_pairs_each_symbol_with_its_container() {
        let found = symbols("class Foo {\n    method bar() { 1 }\n}\n");
        let flat = flatten(&found);
        let bar = flat
            .iter()
            .find(|(s, _)| s.name == "bar")
            .expect("bar is present");
        assert_eq!(bar.1.as_deref(), Some("Foo"));
        let foo = flat
            .iter()
            .find(|(s, _)| s.name == "Foo")
            .expect("Foo is present");
        assert_eq!(foo.1, None);
    }

    #[test]
    fn a_routine_carries_its_rendered_signature() {
        let found = symbols("sub add(Int $a, Int $b --> Int) { $a + $b }\n");
        assert_eq!(
            found[0].signature.as_deref(),
            Some("(Int $a, Int $b --> Int)")
        );
    }

    #[test]
    fn optional_named_and_slurpy_parameters_keep_their_spelling() {
        let found = symbols("sub f($a, $b?, :$verbose, :$name!, *@rest) { 1 }\n");
        assert_eq!(
            found[0].signature.as_deref(),
            Some("($a, $b?, :$verbose, :$name!, *@rest)")
        );
    }

    #[test]
    fn a_routine_with_no_parameters_renders_empty_parentheses() {
        let found = symbols("sub nothing() { 1 }\n");
        assert_eq!(found[0].signature.as_deref(), Some("()"));
    }

    #[test]
    fn a_default_is_shown_as_present_without_rendering_the_expression() {
        // Rendering an expression back to source needs a printer mutsu does not
        // have, and a half-rendered default would be worse than an honest
        // ellipsis.
        let found = symbols("sub f($a = 42) { $a }\n");
        let signature = found[0].signature.as_deref().unwrap_or_default();
        assert!(signature.contains("= ..."), "{signature:?}");
    }

    #[test]
    fn a_class_has_no_signature_but_its_methods_do() {
        let found = symbols("class C {\n    method m(Str $s) { $s }\n}\n");
        assert_eq!(found[0].signature, None);
        assert_eq!(found[0].children[0].signature.as_deref(), Some("(Str $s)"));
    }

    #[test]
    fn an_empty_document_yields_nothing() {
        assert_eq!(symbols(""), Vec::new());
    }
}
