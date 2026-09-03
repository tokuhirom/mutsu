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

pub mod symbols;

pub use symbols::{Symbol, SymbolKind, symbols};

use crate::ast::Stmt;
use crate::interpreter::Interpreter;
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

/// Whether mutsu implements a built-in routine by this name.
///
/// The other half of D4's coverage question, in the form `hover` needs: a name
/// that is declared nowhere in the workspace is either a routine mutsu provides
/// or one it does not have at all, and those are very different answers to
/// someone writing Raku for mutsu.
pub fn is_builtin_routine(name: &str) -> bool {
    Interpreter::is_builtin_function(name) || Interpreter::is_test_function_name(name)
}

/// Names close to `name` that mutsu does have — the same "Did you mean"
/// candidates its own error carries, exposed for a consumer that wants to offer
/// them before the code is run.
pub fn suggest_routines(name: &str) -> Vec<String> {
    Interpreter::static_routine_suggestions(name, &std::collections::HashSet::new())
}

/// mutsu's CHECK-time undeclared-routine analysis, run without executing
/// anything (ADR-0065 D4).
///
/// This is the first diagnostic that answers "does mutsu support this?" rather
/// than "does this parse?". A core routine rakudo has and mutsu does not shows
/// up here exactly as a typo does — which is the point: an agent writing Raku
/// for mutsu has no other way to learn the difference short of running the
/// code.
///
/// It shares the runtime's own walker and static name tables rather than
/// reimplementing the rule, so the server and the interpreter cannot disagree
/// about what counts as declared. That walker's contract is exactly the one a
/// diagnostic needs: declarations are collected scope-blind across the whole
/// unit and the check abandons a unit that imports names it cannot see through,
/// so a missed construct yields a false *negative*, never a false positive.
///
/// No `Interpreter` is constructed. Everything the runtime entry point
/// additionally consults is per-interpreter registry state that a fresh one has
/// none of, so the verdict is identical — and constructing one would cost about
/// 9 ms and retain roughly 7 KiB, on every keystroke.
fn undeclared_routine_diagnostic(stmts: &[Stmt]) -> Option<Diagnostic> {
    let err =
        crate::runtime::undeclared_routines::check_undeclared_routines_without_interpreter(stmts)
            .err()?;
    let line = err.line().unwrap_or(1) as u32;
    Some(Diagnostic {
        severity: Severity::Error,
        message: err.message,
        line,
        // The walker records the statement line, not an offset; `Stmt::SetLine`
        // is the only positional information mutsu has (D6).
        column: None,
        // Its own code, not the `ParseGeneric` the error carries for the CLI's
        // "===SORRY!===" rendering: a consumer keying on the code must be able
        // to tell an unknown name from a syntax error.
        code: Some("UndeclaredRoutine"),
        in_other_file: None,
    })
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

/// The failures a *recovering* parse finds beyond the ones already reported.
///
/// mutsu's strict parser stops at the first failure, which for an editor is the
/// wrong shape: a document under edit is broken most of the time, and a report
/// that goes quiet after line 3 hides everything below it.
/// `parse_program_recovering` skips each unparseable statement and keeps going,
/// rendering every skipped one through the same path the strict error takes —
/// so the extra diagnostics are the same quality as the first, not a lower tier.
///
/// **Deduplicated by line against what is already reported.** The recovery pass
/// re-parses from scratch, so its first failure is almost always the strict
/// parse's failure seen again; and a statement skipped by recovery can leave the
/// parser mid-construct, so a second failure on a line already accounted for is
/// far more likely to be a cascade than a second real defect. Under D5 a
/// plausible-looking wrong diagnostic is the expensive kind of mistake, so the
/// tie is broken toward saying less.
fn recovered_parse_errors(source: &str, already: &[Diagnostic]) -> Vec<Diagnostic> {
    let recovered = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        crate::parser::parse_program_recovering(source)
    }));
    let Ok((_stmts, _finish, errors)) = recovered else {
        return Vec::new();
    };
    let mut reported: Vec<u32> = already.iter().map(|d| d.line).collect();
    let mut out = Vec::new();
    for err in &errors {
        let diagnostic = diagnostic_from_parse_error(err);
        if reported.contains(&diagnostic.line) {
            continue;
        }
        reported.push(diagnostic.line);
        out.push(diagnostic);
    }
    out
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
        match crate::parse_dispatch::parse_source(source) {
            Ok((stmts, _finish)) => Ok(undeclared_routine_diagnostic(&stmts)),
            Err(err) => Err(err),
        }
    }));

    let mut diagnostics = Vec::new();
    match parsed {
        Ok(Ok(undeclared)) => diagnostics.extend(undeclared),
        Ok(Err(err)) => {
            // The strict parse's diagnosis of the *first* failure is the best
            // one available: it carries the typed `X::` message, the
            // surrounding source context and the hint. Report it, then recover
            // past it to find what else is wrong.
            diagnostics.push(diagnostic_from_parse_error(&err));
            diagnostics.extend(recovered_parse_errors(source, &diagnostics));
        }
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

    fn errors(source: &str) -> Vec<Diagnostic> {
        check(source)
            .into_iter()
            .filter(|d| d.severity == Severity::Error)
            .collect()
    }

    /// ADR-0065 S3: mutsu's strict parser stops at the first failure, which for
    /// a document under edit hides everything below it.
    #[test]
    fn a_second_failure_further_down_the_document_is_reported_too() {
        let text = "say 1;\nsay $c.f (1, 2);\nsay 2;\nsay $d.g (3, 4);\nsay 3;\n";
        let errors = errors(text);
        assert_eq!(errors.len(), 2, "{errors:#?}");
        assert_eq!(errors[0].line, 2);
        assert_eq!(errors[1].line, 4);
    }

    /// The recovering pass re-parses from scratch, so its first failure is the
    /// strict parse's failure seen again. Reporting it twice would be noise a
    /// consumer has to learn to ignore.
    #[test]
    fn the_first_failure_is_not_reported_twice() {
        let errors = errors("say 1;\nsay $c.f (1, 2);\nsay 2;\n");
        assert_eq!(errors.len(), 1, "{errors:#?}");
        assert_eq!(errors[0].line, 2);
    }

    /// The first diagnostic keeps the strict parser's own diagnosis, which is
    /// the richest one available (typed `X::` message, source context, hint).
    #[test]
    fn the_first_diagnostic_is_still_the_strict_parsers_own() {
        let errors = errors("my $x = 1;\n}\nsay $x;\n");
        assert_eq!(errors[0].code, Some("ParseUnparsed"));
        assert_eq!(
            errors[0].column,
            Some(1),
            "the strict path knows the column"
        );
    }

    /// ADR-0065 D4: "mutsu does not have this" is the diagnostic the server
    /// exists to deliver, and a name nobody declared is its first form.
    #[test]
    fn a_call_to_a_routine_nobody_declared_is_reported() {
        let diagnostics = check("say 1;\nnosuchsub();\n");
        assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
        let d = &diagnostics[0];
        assert_eq!(d.severity, Severity::Error);
        assert_eq!(d.code, Some("UndeclaredRoutine"));
        assert_eq!(d.line, 2);
        assert!(
            d.message.contains("Undeclared routine") && d.message.contains("nosuchsub"),
            "{:?}",
            d.message
        );
    }

    #[test]
    fn a_declared_routine_is_not_reported() {
        assert_eq!(check("sub greet() { 1 }\ngreet();\n"), Vec::new());
    }

    #[test]
    fn a_builtin_routine_is_not_reported() {
        assert_eq!(check("say uc('x');\nsay elems([1, 2]);\n"), Vec::new());
    }

    /// D4 asks for the replacement to travel with the diagnostic. mutsu
    /// computes one for its own CLI error, so it comes through here for free —
    /// once the candidates include the unit's own subs, which they did not
    /// until this slice (pinned against real raku by
    /// `t/undeclared-routine-suggests-unit-own-subs.t`).
    #[test]
    fn a_near_miss_carries_a_suggestion() {
        let diagnostics = check("sub greeting() { 1 }\ngreetng();\n");
        assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
        assert!(
            diagnostics[0].message.contains("greeting"),
            "expected a 'Did you mean' pointing at the real name: {:?}",
            diagnostics[0].message
        );
    }

    /// The conservativeness contract, pinned. A unit that imports names the
    /// walker cannot see through is abandoned rather than second-guessed: a
    /// false positive here would be read by an agent as fact and acted on.
    #[test]
    fn a_unit_that_imports_unseen_names_is_not_second_guessed() {
        let diagnostics = check("use Test;\nnosuchsub();\n");
        assert!(
            diagnostics
                .iter()
                .all(|d| d.code != Some("UndeclaredRoutine")),
            "{diagnostics:#?}"
        );
    }

    /// Still true with the analysis running: `check` reads the interpreter's
    /// tables, it does not run the document.
    #[test]
    fn the_undeclared_check_does_not_run_the_document() {
        assert_eq!(check("say 'THIS MUST NOT BE PRINTED';\n"), Vec::new());
    }

    #[test]
    fn split_warning_location_handles_a_message_without_a_suffix() {
        let (message, line, file) = split_warning_location("plain warning");
        assert_eq!(message, "plain warning");
        assert_eq!(line, None);
        assert_eq!(file, None);
    }
}
