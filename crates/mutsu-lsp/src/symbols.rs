//! Turning mutsu's declarations into LSP symbols (ADR-0065 S4).

use lsp_types::{DocumentSymbol, Location, Position, Range, SymbolKind, Uri};
use mutsu::analysis::{self, Symbol};

use crate::positions::{name_range, span};

/// The nearest LSP kind for one of mutsu's.
///
/// LSP's vocabulary predates Raku and has no spelling for a role, a grammar
/// token or a subset, so several of these are approximations. They are chosen
/// for what a client *does* with them — pick an icon, group an outline — rather
/// than for taxonomy: a role behaves like an interface, a grammar token like a
/// function, a subset like a type alias.
pub fn lsp_kind(kind: analysis::SymbolKind) -> SymbolKind {
    use analysis::SymbolKind as K;
    match kind {
        K::Module => SymbolKind::MODULE,
        K::Package => SymbolKind::NAMESPACE,
        K::Class => SymbolKind::CLASS,
        K::Grammar => SymbolKind::CLASS,
        K::Role => SymbolKind::INTERFACE,
        K::Subset => SymbolKind::TYPE_PARAMETER,
        K::Enum => SymbolKind::ENUM,
        K::EnumMember => SymbolKind::ENUM_MEMBER,
        K::Sub => SymbolKind::FUNCTION,
        K::Method | K::PrivateMethod => SymbolKind::METHOD,
        K::Token | K::Rule => SymbolKind::FUNCTION,
        K::Attribute => SymbolKind::FIELD,
        K::Variable => SymbolKind::VARIABLE,
    }
}

/// The Raku declarator, shown as the symbol's `detail`.
///
/// This is where the information LSP's kind cannot carry goes: an outline that
/// says `INTERFACE` for a role and `CLASS` for a grammar has lost exactly the
/// distinction a Raku reader wants, and `detail` puts it back.
fn declarator(kind: analysis::SymbolKind) -> &'static str {
    use analysis::SymbolKind as K;
    match kind {
        K::Module => "module",
        K::Package => "package",
        K::Class => "class",
        K::Grammar => "grammar",
        K::Role => "role",
        K::Subset => "subset",
        K::Enum => "enum",
        K::EnumMember => "enum value",
        K::Sub => "sub",
        K::Method => "method",
        K::PrivateMethod => "private method",
        K::Token => "token",
        K::Rule => "rule",
        K::Attribute => "has",
        K::Variable => "variable",
    }
}

/// The outline of one document.
pub fn document_symbols(text: &str) -> Vec<DocumentSymbol> {
    analysis::symbols(text)
        .iter()
        .map(|s| to_document_symbol(text, s))
        .collect()
}

fn to_document_symbol(text: &str, symbol: &Symbol) -> DocumentSymbol {
    let full = span(text, symbol.line, symbol.end_line);
    let selection = name_range(text, symbol.line, &symbol.name);
    #[allow(deprecated)] // `DocumentSymbol::deprecated` is deprecated but not optional
    DocumentSymbol {
        name: symbol.name.clone(),
        detail: Some(declarator(symbol.kind).to_string()),
        kind: lsp_kind(symbol.kind),
        tags: None,
        deprecated: None,
        // The selection range must be inside the full range, and a declaration
        // whose body is empty has both on one line, so widen rather than risk a
        // client rejecting the pair.
        range: contain(full, selection),
        selection_range: selection,
        children: Some(
            symbol
                .children
                .iter()
                .map(|c| to_document_symbol(text, c))
                .collect(),
        ),
    }
}

/// The smallest range containing both.
fn contain(a: Range, b: Range) -> Range {
    Range {
        start: min_position(a.start, b.start),
        end: max_position(a.end, b.end),
    }
}

fn min_position(a: Position, b: Position) -> Position {
    if (a.line, a.character) <= (b.line, b.character) {
        a
    } else {
        b
    }
}

fn max_position(a: Position, b: Position) -> Position {
    if (a.line, a.character) >= (b.line, b.character) {
        a
    } else {
        b
    }
}

/// Where `name` is declared in `text`, if it is.
pub fn definition_in(uri: &Uri, text: &str, name: &str) -> Option<Location> {
    let symbol = analysis::symbols::find(&analysis::symbols(text), name)?;
    Some(Location {
        uri: uri.clone(),
        range: name_range(text, symbol.line, &symbol.name),
    })
}

/// Every declaration in `text`, flat, with its container — the shape
/// `workspaceSymbol` wants.
pub fn flat_symbols(text: &str) -> Vec<(Symbol, Option<String>, Range)> {
    analysis::symbols::flatten(&analysis::symbols(text))
        .into_iter()
        .map(|(symbol, container)| {
            let range = name_range(text, symbol.line, &symbol.name);
            (symbol, container, range)
        })
        .collect()
}

/// Whether `name` matches `query` the way a workspace-symbol picker does:
/// case-insensitive substring. LSP leaves the matching rule to the server, and
/// an agent typing an exact name gets an exact hit either way.
pub fn matches_query(name: &str, query: &str) -> bool {
    query.is_empty() || name.to_lowercase().contains(&query.to_lowercase())
}

#[cfg(test)]
mod tests {
    use super::*;

    const SOURCE: &str = "\
class Foo {
    has $.x;
    method bar($y) { $y }
}
sub baz() { 1 }
";

    #[test]
    fn the_outline_nests_a_class_body_under_the_class() {
        let outline = document_symbols(SOURCE);
        assert_eq!(outline.len(), 2, "{outline:#?}");
        assert_eq!(outline[0].name, "Foo");
        assert_eq!(outline[0].kind, SymbolKind::CLASS);
        assert_eq!(outline[0].detail.as_deref(), Some("class"));
        let children = outline[0].children.as_ref().expect("children");
        assert_eq!(children.len(), 2);
        assert_eq!(children[0].name, "x");
        assert_eq!(children[0].kind, SymbolKind::FIELD);
        assert_eq!(children[1].name, "bar");
        assert_eq!(children[1].kind, SymbolKind::METHOD);
        assert_eq!(outline[1].name, "baz");
        assert_eq!(outline[1].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn the_selection_range_covers_the_name_and_sits_inside_the_full_range() {
        let outline = document_symbols(SOURCE);
        let bar = &outline[0].children.as_ref().unwrap()[1];
        assert_eq!(
            bar.selection_range.start,
            Position {
                line: 2,
                character: 11
            }
        );
        assert_eq!(
            bar.selection_range.end,
            Position {
                line: 2,
                character: 14
            }
        );
        assert!(bar.range.start <= bar.selection_range.start);
        assert!(bar.range.end >= bar.selection_range.end);
    }

    #[test]
    fn a_class_range_spans_its_body() {
        let outline = document_symbols(SOURCE);
        assert_eq!(outline[0].range.start.line, 0);
        assert_eq!(
            outline[0].range.end.line, 2,
            "the last statement inside Foo is on 0-based line 2"
        );
    }

    #[test]
    fn a_grammar_keeps_its_declarator_where_the_lsp_kind_cannot() {
        let outline = document_symbols("grammar G {\n    token TOP { <x> }\n}\n");
        assert_eq!(
            outline[0].kind,
            SymbolKind::CLASS,
            "LSP has no grammar kind"
        );
        assert_eq!(outline[0].detail.as_deref(), Some("grammar"));
    }

    #[test]
    fn query_matching_is_case_insensitive_substring() {
        assert!(matches_query("frobnicate", "frob"));
        assert!(matches_query("Frobnicate", "frob"));
        assert!(matches_query("anything", ""));
        assert!(!matches_query("frobnicate", "zzz"));
    }

    #[test]
    fn flat_symbols_carry_their_container() {
        let flat = flat_symbols(SOURCE);
        let bar = flat.iter().find(|(s, _, _)| s.name == "bar").unwrap();
        assert_eq!(bar.1.as_deref(), Some("Foo"));
    }
}
