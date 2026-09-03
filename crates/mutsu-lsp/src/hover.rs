//! What mutsu can say about the name under the caret (ADR-0065 D3's `hover`:
//! "type/signature, and mutsu coverage status").
//!
//! The coverage half is the part that only a server built on the target runtime
//! can offer. Hovering a routine mutsu does not have says so, in the place a
//! reader is already looking, before the code is ever run.

use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Range};
use mutsu::analysis::{Symbol, SymbolKind};

/// What was found out about a name.
pub enum Known {
    /// Declared somewhere the server can see. `origin` says where, in words —
    /// "in this document", or a path.
    Declared { symbol: Symbol, origin: String },
    /// Not declared anywhere, but mutsu provides it.
    Builtin,
    /// mutsu has no routine by this name. The suggestions are mutsu's own.
    Unknown { suggestions: Vec<String> },
}

/// The Raku declarator for a kind, for the code block's opening word.
fn declarator(kind: SymbolKind) -> &'static str {
    match kind {
        SymbolKind::Module => "module",
        SymbolKind::Package => "package",
        SymbolKind::Class => "class",
        SymbolKind::Grammar => "grammar",
        SymbolKind::Role => "role",
        SymbolKind::Subset => "subset",
        SymbolKind::Enum => "enum",
        SymbolKind::EnumMember => "enum value",
        SymbolKind::Sub => "sub",
        SymbolKind::Method => "method",
        SymbolKind::PrivateMethod => "method !",
        SymbolKind::Token => "token",
        SymbolKind::Rule => "rule",
        SymbolKind::Attribute => "has",
        SymbolKind::Variable => "my",
    }
}

/// Render what is known into the hover a client shows.
pub fn render(name: &str, known: Known, range: Range) -> Hover {
    let markdown = match known {
        Known::Declared { symbol, origin } => {
            let signature = symbol.signature.clone().unwrap_or_default();
            format!(
                "```raku\n{} {}{}\n```\n\nDeclared {} on line {}.",
                declarator(symbol.kind),
                symbol.name,
                signature,
                origin,
                symbol.line,
            )
        }
        // The affirmative half of the coverage answer. Worth saying rather than
        // staying silent: "mutsu has this" is exactly what a writer targeting
        // mutsu wants confirmed, and silence is indistinguishable from "the
        // server did not understand the question".
        Known::Builtin => {
            format!("```raku\n{name}\n```\n\nA built-in routine. **mutsu implements this.**")
        }
        Known::Unknown { suggestions } => {
            let mut text = format!(
                "**mutsu has no routine named `{name}`.**\n\n\
                 It is declared nowhere in this document or workspace, and mutsu \
                 provides no built-in by that name."
            );
            if !suggestions.is_empty() {
                text.push_str(&format!(
                    "\n\nDid you mean {}?",
                    suggestions
                        .iter()
                        .map(|s| format!("`{s}`"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ));
            }
            text
        }
    };
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: markdown,
        }),
        range: Some(range),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::Position;

    fn any_range() -> Range {
        Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 1,
            },
        }
    }

    fn text_of(hover: &Hover) -> &str {
        match &hover.contents {
            HoverContents::Markup(m) => &m.value,
            _ => panic!("expected markdown"),
        }
    }

    fn symbol_of(source: &str, name: &str) -> Symbol {
        mutsu::analysis::symbols::find(&mutsu::analysis::symbols(source), name).expect("declared")
    }

    #[test]
    fn a_declared_routine_shows_its_signature_and_where_it_lives() {
        let symbol = symbol_of("sub add(Int $a, Int $b --> Int) { $a + $b }\n", "add");
        let hover = render(
            "add",
            Known::Declared {
                symbol,
                origin: "in this document".to_string(),
            },
            any_range(),
        );
        let text = text_of(&hover);
        assert!(text.contains("sub add(Int $a, Int $b --> Int)"), "{text}");
        assert!(
            text.contains("Declared in this document on line 1."),
            "{text}"
        );
    }

    #[test]
    fn a_declared_class_reads_as_a_class() {
        let symbol = symbol_of("class Widget { }\n", "Widget");
        let text = text_of(&render(
            "Widget",
            Known::Declared {
                symbol,
                origin: "in lib/Widget.rakumod".to_string(),
            },
            any_range(),
        ))
        .to_string();
        assert!(text.contains("class Widget"), "{text}");
        assert!(text.contains("in lib/Widget.rakumod"), "{text}");
    }

    /// ADR-0065 D4, in the place a reader is already looking.
    #[test]
    fn a_builtin_is_confirmed_rather_than_left_silent() {
        let text = text_of(&render("uc", Known::Builtin, any_range())).to_string();
        assert!(text.contains("mutsu implements this"), "{text}");
    }

    #[test]
    fn an_unknown_routine_says_so_and_offers_what_mutsu_does_have() {
        let hover = render(
            "elem",
            Known::Unknown {
                suggestions: vec!["elems".to_string()],
            },
            any_range(),
        );
        let text = text_of(&hover);
        assert!(text.contains("mutsu has no routine named `elem`"), "{text}");
        assert!(text.contains("Did you mean `elems`?"), "{text}");
    }

    #[test]
    fn an_unknown_routine_with_no_near_miss_offers_nothing() {
        let hover = render(
            "zzzzzz",
            Known::Unknown {
                suggestions: Vec::new(),
            },
            any_range(),
        );
        assert!(!text_of(&hover).contains("Did you mean"));
    }
}
