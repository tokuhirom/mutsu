//! The `use trace` pragma: every statement parsed while it is in effect gets a
//! [`Stmt::Trace`] hook ahead of it, which writes the statement's header and
//! source text to stderr when it is about to run.
//!
//! ```text
//! $ raku -e 'use trace; say 1; say 2'
//! 2 (-e line 1)
//! say 1
//! 1
//! 3 (-e line 1)
//! say 2
//! 2
//! ```
//!
//! # What is traced
//!
//! Rakudo's trace is applied when a statement is built, so the rules below are
//! read off its output (checked against `raku`, 2026.09):
//!
//! - `use trace` / `no trace` themselves are never printed; every other
//!   statement is -- declarations, `use` of any other module, `BEGIN`/`END`
//!   phasers and loops included. A statement inside a nested block is printed
//!   too, when that block runs, after the statement that holds the block.
//! - The pragma is lexical: it ends with the enclosing block, and `no trace`
//!   ends it earlier.
//! - The text is the statement's own source, exactly as written (line breaks
//!   included) without its `;` terminator; the header's line is the line the
//!   statement starts on.
//! - A block-form `if`/`unless` whose condition is a compile-time constant is
//!   replaced by the branch it picks before the trace is attached, so it is not
//!   printed (its branch's own statements are): [`folds_away`].
//!
//! # The statement number
//!
//! The leading number is rakudo's `$*STATEMENT_ID`, bumped on *every attempt*
//! to parse a `statement` -- including the failed attempt that ends each
//! non-empty statement list at its closing `}` (or at the end of the file),
//! the second attempt that follows a statement label, and the statements of
//! the `semilist`s inside `( )`, `[ ]` and subscripts ([`semilist_open`]). It is
//! numbered in source order, outer statements before the ones nested in them,
//! from the start of the compilation unit, so statements before the
//! `use trace` count (`sub g { 1 }; use trace; say 3` prints `5`).
//!
//! mutsu's parser backtracks, and parses some statements speculatively before
//! the statement around them, so it cannot keep a counter. Each attempt is
//! instead recorded by source position
//! ([`crate::parser::primary::record_statement_attempt`]), a hook carries its
//! statement's position until the unit is fully parsed, and
//! [`number_statements`] then replaces the position with its rank among all
//! the attempts.

mod semilist;

pub(in crate::parser) use semilist::semilist_open;
use semilist::text_before;

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{VisitMut, walk_stmt_mut};
use crate::parser::primary::record_statement_attempt;
use crate::token_kind::TokenKind;
use crate::value::ValueView;

thread_local! {
    /// How many [`unnumbered`] regions enclose the parse in progress.
    static UNNUMBERED: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
}

fn is_unnumbered() -> bool {
    UNNUMBERED.with(|depth| depth.get() > 0)
}

/// Run `parse` with statement numbering and tracing off: rakudo does not read
/// a proto's `{*}` body as a statement list, so it takes no statement number
/// and is never traced.
pub(in crate::parser) fn unnumbered<T>(parse: impl FnOnce() -> T) -> T {
    UNNUMBERED.with(|depth| depth.set(depth.get() + 1));
    let result = parse();
    UNNUMBERED.with(|depth| depth.set(depth.get() - 1));
    result
}

/// Whether `input` starts with a proto's dispatcher body, `{*}`.
pub(in crate::parser) fn is_dispatcher_body(input: &str) -> bool {
    input
        .strip_prefix('{')
        .map(str::trim_start)
        .and_then(|rest| rest.strip_prefix('*'))
        .is_some_and(|rest| rest.trim_start().starts_with('}'))
}

/// One attempt to parse a statement: what [`begin`] learned before the parse,
/// to be turned into a hook by [`Attempt::hook`] once the statement is known.
pub(super) struct Attempt {
    /// Where rakudo's statement attempt for it is, as a source offset -- the
    /// statement's number is the rank of this among all attempts
    /// ([`number_statements`]). `None` when the unit never mentions `trace`.
    position: Option<usize>,
    /// Whether `use trace` is in effect where the statement starts. A
    /// `use trace` / `no trace` statement itself is never traced, so the state
    /// it establishes is applied only after the statement ([`apply_pragma`]).
    tracing: bool,
    line: i64,
}

/// Record the statement attempt at `input` (the position of the statement's
/// first token) and capture the pragma state it starts under. `line` is the
/// statement's start line.
pub(super) fn begin(input: &str, line: i64, first_in_unit: bool) -> Attempt {
    // A language-version pragma that opens the compilation unit is read by the
    // grammar's prologue, ahead of the statement list: no attempt, no number.
    if is_unnumbered() || (first_in_unit && is_version_pragma(input)) {
        return Attempt {
            position: None,
            tracing: false,
            line,
        };
    }
    let mut position = record_statement_attempt(input).map(|(offset, _)| offset);
    // A labelled statement is two attempts of rakudo's rule: the outer one,
    // then the statement after the label. The header carries the second.
    if position.is_some()
        && let Some(label_len) = label_len(input)
    {
        position = record_statement_attempt(&input[label_len..]).map(|(offset, _)| offset);
    }
    Attempt {
        position,
        tracing: super::simple::trace_pragma_active(),
        line,
    }
}

/// Whether `input` starts with `use v6`, `use v6.d`, ... -- a language version.
fn is_version_pragma(input: &str) -> bool {
    input
        .strip_prefix("use")
        .map(str::trim_start)
        .and_then(|rest| rest.strip_prefix('v'))
        .is_some_and(|rest| rest.starts_with(|c: char| c.is_ascii_digit()))
}

/// A statement list ended at `input` after at least one statement: rakudo's
/// statement loop tries once more there and fails, which still takes a number.
/// (An *empty* list is skipped by an earlier alternative and takes none.)
pub(super) fn end_of_list(input: &str) {
    if !is_unnumbered() {
        let _ = record_statement_attempt(input);
    }
}

/// The run of `;` separators `run` (the text from `run_start` up to where the
/// statement parser resumes) holds attempts of rakudo's rule too: after a
/// statement's own terminator, every further `;` is an empty statement that
/// takes a number (`say 1;; say 2` numbers its second statement 4). When the
/// previous statement's parse did not consume its terminator
/// (`open_terminator`), the first `;` is that terminator, not an empty
/// statement.
pub(super) fn empty_statements(run_start: &str, resume: &str, open_terminator: bool) {
    let Some(run) = text_before(run_start, resume) else {
        return;
    };
    if is_unnumbered() || !run.contains(';') {
        return;
    }
    let skip = usize::from(open_terminator);
    for (at, _) in run.match_indices(';').skip(skip) {
        let _ = record_statement_attempt(&run_start[at..]);
    }
}

/// Apply a `use trace` / `no trace` statement to the lexical scope. Done by
/// the statement-list loop after the statement parsed, not inside the `use`
/// parser: the statement parser is memoized, and a replayed `use trace` would
/// skip a side effect made there.
pub(super) fn apply_pragma(stmt: &Stmt) {
    match stmt {
        Stmt::Use { module, .. } if module == "trace" => super::simple::set_trace_pragma(true),
        Stmt::No { module, .. } if module == "trace" => super::simple::set_trace_pragma(false),
        _ => {}
    }
}

impl Attempt {
    /// The hook to put ahead of `stmt`, which was parsed from `start` up to
    /// `rest`; `None` when the statement is not traced. Until the unit is
    /// finished the hook's `id` is the statement's source offset
    /// ([`number_statements`] turns it into the number).
    pub(super) fn hook(&self, start: &str, rest: &str, stmt: &Stmt) -> Option<Stmt> {
        if !self.tracing {
            return None;
        }
        let offset = u32::try_from(self.position?).ok()?;
        if is_trace_pragma(stmt) || folds_away(stmt) {
            return None;
        }
        crate::parser::primary::note_statement_hook();
        Some(Stmt::Trace {
            id: offset,
            line: self.line,
            source: statement_text(start, rest).to_string(),
        })
    }
}

/// Number the statements of a finished compilation unit: every `use trace`
/// hook still holds its statement's source offset in `id`, which becomes the
/// offset's rank among all the unit's statement attempts. Must run once, on the
/// whole tree, after the unit is fully parsed and before its source state is
/// replaced.
pub(in crate::parser) fn number_statements(stmts: &mut Vec<Stmt>) {
    struct Numberer(Vec<usize>);
    impl VisitMut for Numberer {
        fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
            if let Stmt::Trace { id, .. } = stmt {
                *id = u32::try_from(self.0.partition_point(|&at| at < *id as usize) + 1)
                    .unwrap_or(u32::MAX);
            }
            walk_stmt_mut(self, stmt);
        }
    }
    if let Some(positions) = crate::parser::primary::take_statement_numbering() {
        Numberer(positions).visit_stmts_mut(stmts);
    }
}

fn is_trace_pragma(stmt: &Stmt) -> bool {
    matches!(stmt, Stmt::Use { module, .. } | Stmt::No { module, .. } if module == "trace")
}

/// The length of a statement label (`FOO:`) at the start of `input`, if there
/// is one: an identifier glued to a single `:` and followed by whitespace.
fn label_len(input: &str) -> Option<usize> {
    let mut chars = input.char_indices();
    let (_, first) = chars.next()?;
    if !(first.is_alphabetic() || first == '_') {
        return None;
    }
    let colon = chars
        .find(|&(_, c)| !(c.is_alphanumeric() || c == '_' || c == '-'))
        .filter(|&(_, c)| c == ':')?
        .0;
    let after = &input[colon + 1..];
    (!after.starts_with(':') && after.starts_with(char::is_whitespace)).then_some(colon + 1)
}

/// The source text of a statement parsed from `start` up to `rest`: what the
/// parser consumed, less the `;` terminator, trailing comments and the
/// whitespace around them, which the statement parser may have taken along with
/// the statement.
fn statement_text<'a>(start: &'a str, rest: &str) -> &'a str {
    // A heredoc's remainder can be a splice that is not the tail of `start`;
    // the statement's own line is the best that can be said then.
    let consumed =
        text_before(start, rest).unwrap_or_else(|| start.lines().next().unwrap_or(start));
    let mut text = consumed;
    loop {
        let trimmed = text.trim_end();
        let trimmed = trimmed.strip_suffix(';').unwrap_or(trimmed);
        let trimmed = strip_trailing_comment(trimmed);
        if trimmed.len() == text.len() {
            return text;
        }
        text = trimmed;
    }
}

/// `text` without a comment that runs to its end: a `#` on the last line that
/// follows whitespace and is outside a quoted string. A heuristic -- a `#` in
/// a regex or a quote-like construct on the last line is not told apart -- but
/// the statement itself rarely ends in one.
fn strip_trailing_comment(text: &str) -> &str {
    let line_start = text.rfind('\n').map_or(0, |at| at + 1);
    let mut quote: Option<char> = None;
    let mut previous = ' ';
    let mut chars = text[line_start..].char_indices().peekable();
    while let Some((at, c)) = chars.next() {
        match (quote, c) {
            (_, '\\') => {
                chars.next();
            }
            (Some(q), _) if c == q => quote = None,
            (Some(_), _) => {}
            // An apostrophe between word characters (`don't`) is not a quote.
            (None, '\'')
                if previous.is_alphanumeric()
                    && chars
                        .peek()
                        .is_some_and(|&(_, next)| next.is_alphanumeric()) => {}
            (None, '\'' | '"') => quote = Some(c),
            (None, '#') if previous.is_whitespace() => return &text[..line_start + at],
            _ => {}
        }
        previous = c;
    }
    text
}

/// Whether rakudo resolves this statement at compile time, taking its trace
/// with it: a block-form `if`/`unless` (not a `with`, not a postfix modifier)
/// whose condition is a compile-time constant.
///
/// A constant-true condition is replaced by its block. A constant-false one is
/// replaced by the `else` block (or by nothing), *unless* an `elsif` chain
/// follows, which rakudo leaves to run time. The chain is lowered to an `else`
/// holding one nested `if`, so that shape stands for it here; an `else { if
/// ... }` written out by hand is indistinguishable and treated the same.
///
/// Known gap: rakudo also folds a condition made of constants (`if 1 + 1`,
/// `if ?1`, `if 1 && 2`); only literals, `True`/`False`, parentheses and `!`
/// are recognised here.
fn folds_away(stmt: &Stmt) -> bool {
    let Stmt::If {
        cond,
        else_branch,
        is_statement_modifier: false,
        with_kind: None,
        ..
    } = stmt
    else {
        return false;
    };
    match constant_truth(cond) {
        Some(true) => true,
        Some(false) => !has_elsif_chain(else_branch),
        None => false,
    }
}

fn has_elsif_chain(else_branch: &[Stmt]) -> bool {
    matches!(
        else_branch,
        [Stmt::If {
            is_statement_modifier: false,
            ..
        }]
    )
}

/// The truth value of a condition made only of constants rakudo folds, `None`
/// for anything else (`Nil`, a type object, a variable, a call). Parentheses
/// and `!` are peeled in a loop: this follows one path to a leaf, it is not a
/// walk of the tree.
fn constant_truth(mut expr: &Expr) -> Option<bool> {
    let mut negated = false;
    loop {
        match expr {
            Expr::Grouped(inner) => expr = inner,
            Expr::Unary {
                op: TokenKind::Bang,
                expr: inner,
            } => {
                negated = !negated;
                expr = inner;
            }
            Expr::Literal(value) | Expr::LiteralSrc(value, _) => {
                return matches!(
                    value.view(),
                    ValueView::Int(_)
                        | ValueView::BigInt(_)
                        | ValueView::Num(_)
                        | ValueView::Rat(..)
                        | ValueView::Str(_)
                        | ValueView::Bool(_)
                )
                .then(|| value.truthy() != negated);
            }
            _ => return None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn label_needs_an_identifier_a_single_colon_and_a_space() {
        assert_eq!(label_len("FOO: for 1 { }"), Some(4));
        assert_eq!(label_len("my-label: while 1 { }"), Some(9));
        assert_eq!(label_len("Foo::bar()"), None);
        assert_eq!(label_len("foo:bar"), None);
        assert_eq!(label_len("say 1"), None);
        assert_eq!(label_len("1: x"), None);
    }

    #[test]
    fn trailing_comment_is_not_part_of_the_statement() {
        assert_eq!(strip_trailing_comment("say 3 # last"), "say 3 ");
        assert_eq!(strip_trailing_comment("say \"a # b\""), "say \"a # b\"");
        assert_eq!(strip_trailing_comment("say 'a # b'"), "say 'a # b'");
        assert_eq!(strip_trailing_comment("say $#a"), "say $#a");
        assert_eq!(strip_trailing_comment("say 1\n# only a comment"), "say 1\n");
        assert_eq!(strip_trailing_comment("# don't"), "");
        assert_eq!(
            strip_trailing_comment("say 1 # c\nsay 2"),
            "say 1 # c\nsay 2"
        );
    }

    #[test]
    fn version_pragma_is_use_v_followed_by_a_digit() {
        assert!(is_version_pragma("use v6.d; say 1"));
        assert!(is_version_pragma("use  v6;"));
        assert!(!is_version_pragma("use variables :D;"));
        assert!(!is_version_pragma("use Test;"));
        assert!(!is_version_pragma("say 1"));
    }

    #[test]
    fn dispatcher_body_is_exactly_a_star_in_braces() {
        assert!(is_dispatcher_body("{*}"));
        assert!(is_dispatcher_body("{ * } # x"));
        assert!(!is_dispatcher_body("{ *.say }"));
        assert!(!is_dispatcher_body("{ 1 }"));
        assert!(!is_dispatcher_body("*"));
    }

    #[test]
    fn statement_text_drops_the_terminator_and_surrounding_space() {
        // `rest` must be a tail of `start`, as it is for a real parse.
        fn text(src: &str, rest_len: usize) -> &str {
            statement_text(src, &src[src.len() - rest_len..])
        }
        assert_eq!(text("say 1 ;  \nsay 2", "say 2".len()), "say 1");
        assert_eq!(
            text("for 1..2 { say $_ }\nsay 2", "\nsay 2".len()),
            "for 1..2 { say $_ }"
        );
        assert_eq!(text("say\n  3 +\n  4;", 0), "say\n  3 +\n  4");
    }
}
