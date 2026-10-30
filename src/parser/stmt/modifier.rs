#[path = "modifier_for.rs"]
mod for_loop;
#[path = "modifier_single.rs"]
mod single;
use for_loop::parse_for_modifier;
use single::parse_single_modifier;

use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{
    PError, PResult, TWO_TERMS_ACROSS_LINES, merge_expected_messages,
};

use crate::ast::parsed::{Modified, ParsedStmt, StatementModifier};
use crate::ast::{CallArg, Expr, Stmt};

use super::super::helpers::is_raku_identifier_start;
use super::{keyword, parse_comma_or_expr};

thread_local! {
    /// The "Missing semicolon" error for a modifier chain this thread stopped
    /// short of, keyed by the address of the offending keyword.
    ///
    /// A statement takes at most one conditional modifier and then at most one
    /// loop modifier; a further one is not this statement's business, it is the
    /// *enclosing* statement's. `do STMT` is the construct that has one:
    /// `do return False unless %h<auth> ~~ $!auth if $!auth;` (Pakku::Spec) is a
    /// `do`-wrapped statement carrying `unless …`, and the `if …` modifies the
    /// `do` statement itself. Raising the error where the chain is detected
    /// therefore rejects legal code, so the chain merely *ends* there and the
    /// error is handed to whoever discovers the keyword is unconsumable — the
    /// statement list, see `pending_extra_modifier_error`.
    static PENDING_EXTRA_MODIFIER: std::cell::RefCell<Option<(crate::parser::memo::MemoKey, PError)>> =
        const { std::cell::RefCell::new(None) };
}

/// The deferred "Missing semicolon" error iff it belongs to `at`, i.e. the
/// statement really did end leaving a modifier keyword nobody could consume.
///
/// Reading it does NOT consume the record: a block body is parsed more than once
/// (speculatively as a hash composer first, then as a block, the second time as a
/// pure memo hit that re-runs no modifier loop), so a record taken by the
/// discarded attempt would be missing from the one that survives. Only an
/// enclosing statement actually consuming the keyword clears it, in
/// `clear_pending_extra_modifier`.
pub(crate) fn pending_extra_modifier_error(at: &str) -> Option<PError> {
    let key = crate::parser::memo::record_key(at);
    PENDING_EXTRA_MODIFIER.with(|p| match p.borrow().as_ref() {
        Some((k, e)) if *k == key => Some(e.clone()),
        _ => None,
    })
}

/// Drop the deferred error once an enclosing statement has consumed the keyword
/// it was recorded for.
fn clear_pending_extra_modifier(at: &str) {
    let key = crate::parser::memo::record_key(at);
    PENDING_EXTRA_MODIFIER.with(|p| {
        let mut slot = p.borrow_mut();
        if slot.as_ref().is_some_and(|(k, _)| *k == key) {
            *slot = None;
        }
    });
}

/// After parsing a postfix modifier condition, check if the remaining input
/// starts on a new line with a bare word that is not a statement modifier.
/// This detects "two terms in a row across lines" errors like:
///   42 if 23
///   is 50; 1
/// where `is` on the next line is confused with a continuation.
fn check_two_terms_across_lines(cond_input: &str, r: &str) -> Result<(), PError> {
    // Only check if there's content after the condition
    if r.is_empty() || r.starts_with(';') || r.starts_with('}') {
        return Ok(());
    }
    // A condition that ends in a `}` block is self-terminating at end of line,
    // exactly like any block statement: `say 1 if @a.grep: { ... }` followed by a
    // statement on the next line is legitimate, not "two terms in a row". Detect
    // it from the consumed condition text (ends in `}`).
    if cond_input[..cond_input.len() - r.len()]
        .trim_end()
        .ends_with('}')
    {
        return Ok(());
    }
    // Check if whitespace before remaining contains a newline
    let trimmed = r.trim_start();
    let gap = &r[..r.len() - trimmed.len()];
    if !gap.contains('\n') {
        return Ok(());
    }
    // If the next token after the newline is a bare word that's not a
    // statement modifier keyword, it's "two terms in a row across lines"
    if trimmed.is_empty() || trimmed.starts_with(';') || trimmed.starts_with('}') {
        return Ok(());
    }
    if is_stmt_modifier_keyword(trimmed) {
        return Ok(());
    }
    let first_ch = trimmed.chars().next().unwrap_or('\0');
    if is_raku_identifier_start(first_ch) {
        // `trimmed` is the unconsumed rest at the offending second term, so the
        // reported position lands on that term rather than on the whole file.
        // `render_parse_error` separately re-derives the `------>` echo's own
        // (different) position from this exact message (#8329).
        return Err(PError::fatal_at(
            TWO_TERMS_ACROSS_LINES.to_string(),
            trimmed,
        ));
    }
    Ok(())
}

pub(super) fn is_stmt_modifier_keyword(input: &str) -> bool {
    leading_modifier_keyword(input).is_some()
}

/// A statement-modifier keyword sitting right after a *trailing comma*
/// (`die "x", if @c;` / `return 1, if 0;` — legal Raku, the trailing comma is an
/// empty list slot). Rejects a same-named **pair key** (`1, with => 2`) so a
/// legal comma list is never truncated at its last element.
pub(in crate::parser) fn is_stmt_modifier_after_trailing_comma(input: &str) -> bool {
    let Some(kw) = leading_modifier_keyword(input) else {
        return false;
    };
    !input[kw.len()..].trim_start().starts_with("=>")
}

/// The statement-modifier keyword at the start of `input`, if any.
pub(crate) fn leading_modifier_keyword(input: &str) -> Option<&'static str> {
    [
        "if", "unless", "for", "while", "until", "given", "when", "with", "without",
    ]
    .into_iter()
    .find(|kw| keyword(kw, input).is_some())
}

/// Whether a second statement modifier `next` is legal after a first `first`.
/// Raku only allows a *conditional* modifier (`if`/`unless`/`with`/`without`/`when`)
/// followed by a *loop* modifier (`for`/`while`/`until`/`given`), e.g.
/// `EXPR if COND for LIST` or `EXPR when COND given TOPIC`. Everything else (two
/// conditionals, two loops, a loop then a conditional) is X::Syntax::Confused.
fn second_modifier_allowed(first: &str, next: &str) -> bool {
    let first_is_cond = matches!(first, "if" | "unless" | "with" | "without" | "when");
    let next_is_loop = matches!(next, "for" | "while" | "until" | "given");
    first_is_cond && next_is_loop
}

/// Parse the statement modifiers (postfix if/unless/for/while/until/given/when)
/// after `stmt`, which ended at `input`. A condition modifier may be followed
/// by a loop modifier: `expr if cond for list` loops over `expr if cond`. The
/// statement and its modifiers are one [`Modified`] node (ADR-10723 Stage 2),
/// lowered by `rakuast::modifier_lower`.
pub(crate) fn parse_statement_modifier(input: &str, stmt: Stmt) -> PResult<'_, Stmt> {
    let (rest, stmt) = parse_statement_modifier_inner(input, stmt)?;
    // A signature declaration no modifier took stays as written until here.
    let stmt = super::decl::destructure::settle(stmt).map_err(PError::from_typed)?;
    Ok((rest, stmt))
}

fn parse_statement_modifier_inner(input: &str, stmt: Stmt) -> PResult<'_, Stmt> {
    let body = crate::parser::primary::trimmed_end(input);
    let (rest, _) = ws(input)?;
    // A trailing comma in the statement's argument list is an empty list slot, not
    // a syntax error: `die "x", if @c;` is legal Raku. Statements that parse a
    // a statement modifier follows. A comma before anything else is left alone so
    // real errors still surface. Comma-list statements parse their arguments
    // before reaching this point; the comma here is only the empty trailing slot
    // accepted by Raku (`die "x", if @c`).
    let rest = if let Some(after_comma) = rest.strip_prefix(',') {
        let (after_ws, _) = ws(after_comma)?;
        if is_stmt_modifier_after_trailing_comma(after_ws) {
            after_ws
        } else {
            rest
        }
    } else {
        rest
    };
    // A block's `}` at end of line ends the statement (rakudo's `$*ENDSTMT`):
    // `my @a = gather { ... }` / `try { ... }` / `my $h = {a => 1}` / a bare
    // block, followed on the next line by `if COND { ... }`, is two statements,
    // not a modifier on the first. The brace parsers record that position
    // (`parser::stmt_ending_brace`); `@a = @a.grep({ ... })` ends in `)`, so
    // its next-line `if` still IS a modifier.
    if crate::parser::stmt_ending_brace::at_stmt_ending_brace(rest) {
        return Ok((input, stmt));
    }
    let mut written = stmt;
    let mut condition: Option<StatementModifier> = None;
    let mut loop_: Option<StatementModifier> = None;
    let mut rest = rest;
    // The keywords of the modifiers parsed so far, in order.
    let mut parsed_kinds: Vec<&str> = Vec::new();
    // Whether the previous modifier's *condition* ended with a `{ ... }` block
    // (`return if @a.first: { ... }`) followed by a newline. A block that ends a
    // statement is itself a statement terminator in Raku, so the next line's
    // `if`/`for`/etc. begins a NEW statement rather than a second (illegal)
    // modifier — do not raise "Missing semicolon" for it.
    let mut prev_cond_block_terminated = false;

    loop {
        // A block-terminated condition followed by a newline ends the statement;
        // whatever comes next (including another modifier keyword) is a new one.
        if prev_cond_block_terminated {
            break;
        }

        // If there's a semicolon, the statement is terminated — no more modifiers
        // The enclosing statement or semilist owns this separator. In a
        // statement-prefix term such as `try`, consuming it here would make
        // the next conditional look like another modifier on the prefix.
        if rest.starts_with(';') {
            break;
        }

        // If at end of input or block, return as-is
        if rest.is_empty() || rest.starts_with('}') {
            break;
        }

        // An enclosing statement that can take this keyword has consumed it, so
        // the deferred error recorded for it is moot.
        clear_pending_extra_modifier(rest);

        // A second modifier is only legal as `conditional THEN loop`
        // (`EXPR if COND for LIST`). Any other chain — two conditionals, two
        // loops, a loop then a conditional, or a third modifier — ends this
        // statement, and needs a `;` unless an enclosing statement takes it.
        if let Some(next_kw) = leading_modifier_keyword(rest)
            && let Some(&first) = parsed_kinds.first()
            && (parsed_kinds.len() >= 2 || !second_modifier_allowed(first, next_kw))
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                crate::value::Value::str("Missing semicolon".to_string()),
            );
            attrs.insert(
                "reason".to_string(),
                crate::value::Value::str("Missing semicolon".to_string()),
            );
            // Reconstruct the source spans around the eject point (`rest` sits at
            // the offending second modifier) so the error carries `pre`/`post`.
            if let Some((pre, post)) = crate::parser::primary::source_span_at(rest) {
                attrs.insert("pre".to_string(), crate::value::Value::str(pre));
                attrs.insert("post".to_string(), crate::value::Value::str(post));
            }
            let ex = crate::value::Value::make_instance(
                crate::symbol::Symbol::intern("X::Syntax::Confused"),
                attrs,
            );
            let mut err =
                PError::fatal_with_exception("Missing semicolon".to_string(), Box::new(ex));
            err.remaining_len = Some(rest.len());
            let key = crate::parser::memo::record_key(rest);
            PENDING_EXTRA_MODIFIER.with(|p| {
                *p.borrow_mut() = Some((key, err));
            });
            break;
        }

        let kw = leading_modifier_keyword(rest);
        let Some((r, modifier)) = parse_single_modifier(rest, &mut written)? else {
            break;
        };
        // Did this modifier's condition end with a `{ ... }` block?
        let cond_consumed =
            crate::parser::primary::source_tail(rest, r).map_or("", |tail| tail.before());
        let cond_ends_block = cond_consumed.trim_end().ends_with('}')
            && !super::modifier_tail::operand_ends_with_subscript(&modifier.expr);
        if let Some(k) = kw {
            parsed_kinds.push(k);
        }
        if modifier.kind.is_loop() {
            loop_ = Some(modifier);
        } else {
            condition = Some(modifier);
        }
        let (r_ws, _) = ws(r)?;
        let gap_has_newline = r[..r.len() - r_ws.len()].contains('\n');
        prev_cond_block_terminated = cond_ends_block && gap_has_newline;
        rest = r_ws;
    }
    if condition.is_none() && loop_.is_none() {
        return Ok((rest, written));
    }
    let node = ParsedStmt::Modified(Modified {
        stmt: Box::new(written),
        condition,
        loop_,
        body,
    });
    let stmt = super::control::finish(node).map_err(PError::from_typed)?;
    Ok((rest, stmt))
}

/// Check if the input starts with a character that unambiguously begins a term
/// (string literal, number, etc.). Used to detect "Two terms in a row" errors.
fn starts_with_term_char(input: &str) -> bool {
    let Some(ch) = input.chars().next() else {
        return false;
    };
    ch.is_ascii_digit()
        || matches!(
            ch,
            '\'' | '"'
                | '\u{2018}'
                | '\u{2019}'
                | '\u{201A}'
                | '\u{201C}'
                | '\u{201D}'
                | '\u{201E}'
        )
}
