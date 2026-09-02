//! A non-executing analysis frontend over mutsu's parser (ADR-0065).
//!
//! Everything else that parses Raku in this crate does so on the way to running
//! it. This module is the one entry point that parses a document and throws the
//! AST away, reporting only what it learned — which is what a language server
//! needs, and what neither `Interpreter` (it runs the code) nor `dump_ast` (it
//! returns a formatted AST or one error) provides.
//!
//! Three properties are load-bearing, in the order ADR-0065 ranks them:
//!
//! - **Nothing executes.** `check` parses; it never compiles or runs. A
//!   document with `unlink "/etc/passwd"` in it is safe to open in an editor.
//!   (Parsing a `use` *does* read the imported module's source off disk, to
//!   harvest its exported names — Raku's grammar is not context-free with
//!   respect to the imported symbol table — but it does not run it.)
//! - **The message matters more than the range** (D5). An agent tolerates a
//!   range that is off by a few characters; it does not tolerate a wrong
//!   message, because it believes it. So the text comes through verbatim from
//!   the parser's own diagnosis, hint included.
//! - **It cannot take the process down.** A resident server outlives any one
//!   document, so a parser panic on malformed input has to become a diagnostic
//!   rather than an abort. mutsu is under active development and its parser is
//!   not panic-free; `check` catches it.

use crate::value::{RuntimeError, RuntimeErrorCode};

/// How much a [`Diagnostic`] should be believed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// The document does not parse, or mutsu cannot handle it.
    Error,
    /// The document parses, and mutsu has something to say about it.
    Warning,
}

/// One thing mutsu has to report about a document.
///
/// Positions are **1-based line and column**, matching the parser's own
/// numbering and the way errors are rendered on the CLI. A consumer that needs
/// LSP's 0-based `Position` converts at the boundary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: Severity,
    /// The parser's own message, with its hint appended when it has one.
    pub message: String,
    /// 1-based line. Falls back to 1 when the parser could not place the
    /// problem — never omitted, because a diagnostic with no anchor is one a
    /// consumer cannot act on.
    pub line: u32,
    /// 1-based column, or `None` when only the line is known. mutsu's AST has
    /// no positions at all (ADR-0065 D6); columns exist only on parse failures,
    /// which carry their own offset.
    pub column: Option<u32>,
    /// The parser's classification, where it has one: `ParseUnparsed`,
    /// `ParseExpected`, `ParseGeneric`. Stable enough to key a client-side
    /// rule on, unlike the message text.
    pub code: Option<&'static str>,
    /// Set when the problem is really in a *different* file than the one being
    /// checked — a parse failure inside a `use`d module. The position then
    /// refers to that file, not to this document, and the diagnostic is
    /// anchored at line 1 of this one.
    pub in_other_file: Option<String>,
}

impl Diagnostic {
    fn error(message: String) -> Self {
        Diagnostic {
            severity: Severity::Error,
            message,
            line: 1,
            column: None,
            code: None,
            in_other_file: None,
        }
    }
}

/// Deliberately exhaustive: a new `RuntimeErrorCode` variant should fail to
/// compile here rather than silently reach a consumer as `None`.
fn code_name(code: RuntimeErrorCode) -> &'static str {
    match code {
        RuntimeErrorCode::ParseUnparsed => "ParseUnparsed",
        RuntimeErrorCode::ParseExpected => "ParseExpected",
        RuntimeErrorCode::ParseGeneric => "ParseGeneric",
    }
}

/// Turn a parse failure into the single diagnostic it is.
fn diagnostic_from_parse_error(err: &RuntimeError) -> Diagnostic {
    let mut message = err.message.clone();
    if let Some(hint) = err.hint() {
        message.push_str("\nhint: ");
        message.push_str(hint);
    }

    // A failure raised while parsing a `use`d module carries that module's
    // file, and its line/column are relative to *that* file. Reporting those
    // coordinates against the open document would point at an unrelated line,
    // which under D5 is worse than pointing at nothing: an agent would edit the
    // wrong place. Anchor at line 1 and say which file it actually is.
    if let Some(other) = err.source_file() {
        let where_ = match (err.line(), err.column()) {
            (Some(l), Some(c)) => format!("{other}:{l}:{c}"),
            (Some(l), None) => format!("{other}:{l}"),
            _ => other.to_string(),
        };
        return Diagnostic {
            severity: Severity::Error,
            message: format!("in an imported file ({where_}): {message}"),
            line: 1,
            column: None,
            code: err.code().map(code_name),
            in_other_file: Some(other.to_string()),
        };
    }

    Diagnostic {
        severity: Severity::Error,
        message,
        line: err.line().unwrap_or(1) as u32,
        column: err.column().map(|c| c as u32),
        code: err.code().map(code_name),
        in_other_file: None,
    }
}

/// Split the `"\n    at FILE:LINE"` suffix `add_parse_warning` bakes into every
/// parse warning back into (message, line).
///
/// The suffix is a rendering detail of the CLI's warning output that happens to
/// be the only positional information a warning carries — it is not kept as a
/// separate field, because it has to survive the precompilation cache, which
/// persists message text only. Parsing it back is therefore the supported way
/// to recover the line, not a hack around a missing accessor.
fn split_warning_location(warning: &str) -> (String, Option<u32>, Option<String>) {
    let Some((message, location)) = warning.rsplit_once("\n    at ") else {
        return (warning.to_string(), None, None);
    };
    let Some((file, line)) = location.rsplit_once(':') else {
        return (warning.to_string(), None, None);
    };
    let Ok(line) = line.trim().parse::<u32>() else {
        return (warning.to_string(), None, None);
    };
    let file = (file != "-e").then(|| file.to_string());
    (message.to_string(), Some(line), file)
}

/// Parse `source` and report everything mutsu has to say about it, without
/// running any of it.
///
/// Returns an empty vector for a document that parses cleanly and raises no
/// warnings. A document that fails to parse yields exactly one error: mutsu's
/// parser stops at the first failure and discards the partial result, so
/// multiple diagnostics per document need `parse_program_partial` to grow
/// positions and errors first (ADR-0065 S3).
pub fn check(source: &str) -> Vec<Diagnostic> {
    // The parser is not panic-free, and a language server must outlive a
    // document that trips it. `AssertUnwindSafe` is the honest annotation here:
    // the parser's state is thread-local and every entry point resets what it
    // owns (`parse_program` clears the warning buffers, resets the memo
    // generation and the user-operator table, and re-points `ORIGINAL_SOURCE`),
    // so a caught panic leaves the *next* parse well-defined even though it may
    // leave this one's intermediate state torn.
    //
    // `parse_source` rather than `parse_compilation_unit`: the document is
    // parsed under whatever `use vX` it declares, but that version is restored
    // afterwards instead of being left behind. In a one-shot process the
    // difference is invisible; in a resident one it is the difference between
    // documents being analysed independently and a `use v6.e.PREVIEW` in the
    // first file silently changing how every later file is read.
    let parsed = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        crate::parse_dispatch::parse_source(source)
    }));

    let mut diagnostics = Vec::new();
    match parsed {
        Ok(Ok(_stmts)) => {}
        Ok(Err(err)) => diagnostics.push(diagnostic_from_parse_error(&err)),
        Err(payload) => {
            // "mutsu cannot handle this" is the single most valuable thing this
            // server reports (D4), and a panic is its bluntest form. Say so
            // plainly rather than dressing it up as a syntax error, so a
            // consumer does not go looking for a mistake in its own code.
            let detail = payload
                .downcast_ref::<&str>()
                .map(|s| (*s).to_string())
                .or_else(|| payload.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "unknown panic".to_string());
            diagnostics.push(Diagnostic::error(format!(
                "mutsu's parser crashed on this document: {detail}. This is a bug in mutsu, \
                 not necessarily in your code."
            )));
            return diagnostics;
        }
    }

    // Warnings are collected during the parse and drained here. Draining is
    // mandatory even when the parse failed: the buffer is thread-local and
    // process-lifetime, so anything left behind would resurface against an
    // unrelated document later in the same session.
    for (_file, warning) in crate::parser::take_parse_warnings() {
        let (message, line, other_file) = split_warning_location(&warning);
        diagnostics.push(Diagnostic {
            severity: Severity::Warning,
            message,
            line: line.unwrap_or(1),
            column: None,
            code: None,
            in_other_file: other_file,
        });
    }

    diagnostics
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_clean_document_reports_nothing() {
        assert_eq!(check("my $x = 1;\nsay $x;\n"), Vec::new());
    }

    #[test]
    fn a_parse_failure_reports_one_error_with_a_position() {
        let diagnostics = check("my $x = 1;\n}\n");
        assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
        let d = &diagnostics[0];
        assert_eq!(d.severity, Severity::Error);
        assert_eq!(d.line, 2, "the stray brace is on line 2");
        assert_eq!(d.column, Some(1));
        assert_eq!(d.code, Some("ParseUnparsed"));
    }

    #[test]
    fn nothing_executes() {
        // If `check` ran the document, this would print. It parses fine, so a
        // clean report is also the proof that parsing is all that happened.
        assert_eq!(check("say 'THIS MUST NOT BE PRINTED';\n"), Vec::new());
    }

    #[test]
    fn a_warning_keeps_its_line_and_loses_its_location_suffix() {
        // A statement in mainline sink context whose value is discarded.
        let diagnostics = check("my $x = 1;\n$x == 1;\nsay $x;\n");
        let warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();
        assert_eq!(warnings.len(), 1, "{diagnostics:#?}");
        assert_eq!(warnings[0].line, 2);
        assert!(
            !warnings[0].message.contains("\n    at "),
            "the location suffix must be split out, not left in the message: {:?}",
            warnings[0].message
        );
    }

    #[test]
    fn warnings_do_not_leak_into_the_next_document() {
        check("my $x = 1;\n$x == 1;\n");
        assert_eq!(check("my $y = 2;\nsay $y;\n"), Vec::new());
    }

    #[test]
    fn split_warning_location_handles_a_message_without_a_suffix() {
        let (message, line, file) = split_warning_location("plain warning");
        assert_eq!(message, "plain warning");
        assert_eq!(line, None);
        assert_eq!(file, None);
    }
}
