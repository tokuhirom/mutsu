//! Turning `mutsu::analysis::Diagnostic` into `lsp_types::Diagnostic`.

use lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString};
use mutsu::analysis::{self, Severity};

use crate::positions::diagnostic_range;

/// The `source` field every diagnostic carries, so a client showing several
/// language servers can tell whose opinion this is — and, more to the point for
/// an agent, that the verdict is *mutsu's* rather than rakudo's (ADR-0065 D4).
pub const SOURCE: &str = "mutsu";

/// Analyse `text` and return what LSP should be told about it.
pub fn diagnostics_for(text: &str) -> Vec<Diagnostic> {
    analysis::check(text)
        .into_iter()
        .map(|d| to_lsp(text, d))
        .collect()
}

fn to_lsp(text: &str, d: analysis::Diagnostic) -> Diagnostic {
    Diagnostic {
        range: diagnostic_range(text, d.line, d.column),
        severity: Some(match d.severity {
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
        }),
        code: d.code.map(|c| NumberOrString::String(c.to_string())),
        source: Some(SOURCE.to_string()),
        message: d.message,
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::Position;

    #[test]
    fn a_clean_document_produces_no_diagnostics() {
        assert!(diagnostics_for("my $x = 1;\nsay $x;\n").is_empty());
    }

    #[test]
    fn a_parse_error_lands_on_the_offending_line() {
        let text = "my $x = 1;\n}\nsay $x;\n";
        let diagnostics = diagnostics_for(text);
        assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
        let d = &diagnostics[0];
        assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(d.source.as_deref(), Some("mutsu"));
        assert_eq!(
            d.range.start,
            Position {
                line: 1,
                character: 0
            },
            "the stray brace is the first character of the second line"
        );
        assert_eq!(
            d.code,
            Some(NumberOrString::String("ParseUnparsed".to_string()))
        );
    }

    #[test]
    fn a_sink_context_warning_is_a_warning_not_an_error() {
        let text = "my $x = 1;\n$x == 1;\nsay $x;\n";
        let diagnostics = diagnostics_for(text);
        assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
        assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::WARNING));
        assert_eq!(diagnostics[0].range.start.line, 1);
    }

    #[test]
    fn a_document_that_crashes_the_parser_is_reported_not_propagated() {
        // Whatever input does this, if any does, must come back as a
        // diagnostic. The assertion is that `diagnostics_for` returns at all.
        let _ = diagnostics_for("}}}}}}}}\n");
        let _ = diagnostics_for("sub \0 { }\n");
        let _ = diagnostics_for("");
    }
}
