//! Raku's "a block's closing brace at end of line terminates the statement" rule.
//!
//! `}` that closes a *block* and is followed by a newline is a statement
//! separator, so whatever starts the next line begins a new statement — even
//! when it is spelled like an infix operator or a statement modifier:
//!
//! ```raku
//! g { 1 }
//! before { 2 }     # two calls, NOT `g({ 1 } before { 2 })`
//! my $h = {a => 1}
//! if $c { ... }    # an `if` statement, NOT a modifier on the declaration
//! ```
//!
//! Rakudo implements this with a `$*ENDSTMT` dynamic variable that its `ws`
//! rule consults, set by every blockoid (block, pointy block, hash composer,
//! routine body, regex declaration body). mutsu's parser has no single
//! whitespace chokepoint the infix layers share, so instead the brace parsers
//! record **where the next token after such a brace starts**, and each infix
//! layer and the statement-modifier parser ask [`at_stmt_ending_brace`] before
//! consuming an operator or a modifier keyword there.
//!
//! The block-*term* parsers (bare block, pointy block, hash composer) also
//! record where the most recent such term ended, so the `for` parser can tell
//! that its loop block was gobbled by the iterable expression
//! ([`block_term_within`]) — rakudo's `$*BORG<block>`.
//!
//! Positions are `(pointer, length)` pairs of input slices. Comparing both
//! makes them an exact position identity: a stale mark left over from an
//! earlier parse cannot alias a position in a different buffer. Recording a
//! position, not re-deriving "does this expression end in a block" from the
//! AST, keeps every brace-closed construct covered: an AST shape test has to
//! enumerate the variants, and the ones it had missed hash composers, pointy
//! blocks in a list, `do` under an infix, ...
//!
//! The parse memo (`parser::memo`) replays these marks on a hit
//! ([`snapshot_within`] / [`replay`]): a memoized expression that ended in a
//! block must leave the same marks as parsing it afresh.

use std::cell::Cell;

/// `(ptr, len)` of an input slice.
type Pos = (usize, usize);

fn pos(s: &str) -> Pos {
    (s.as_ptr() as usize, s.len())
}

/// A statement-ending brace: where its `}` ended, and the first token after
/// the newline that follows it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct EndingBrace {
    brace_end: Pos,
    next_token: Pos,
}

thread_local! {
    /// The most recent brace followed by a newline, or `None` when none is
    /// pending.
    static MARK: Cell<Option<EndingBrace>> = const { Cell::new(None) };
    /// Where the most recent block term (bare block, pointy block, hash
    /// composer) ended.
    static LAST_BLOCK_TERM: Cell<Option<Pos>> = const { Cell::new(None) };
}

/// Record that a block's `}` just ended, with `after_brace` being the input
/// immediately following it. Sets the statement-ending mark only when a
/// newline separates the brace from the next token — a `}` in the middle of a
/// line does not end the statement (`say ({ 1 } before { 2 })` really is an
/// infix `before`).
// Cost: O(w), w = length of the whitespace/comments after the brace.
pub(crate) fn mark_stmt_ending_brace(after_brace: &str) {
    let mut rest = after_brace;
    let mut saw_newline = false;
    loop {
        let trimmed = rest.trim_start_matches([' ', '\t', '\r']);
        if let Some(nl) = trimmed.strip_prefix('\n') {
            saw_newline = true;
            rest = nl;
            continue;
        }
        // A trailing `# comment` still leaves the brace at end of line.
        if trimmed.starts_with('#') {
            match trimmed.find('\n') {
                Some(idx) => {
                    saw_newline = true;
                    rest = &trimmed[idx + 1..];
                    continue;
                }
                // A comment running to end of input: nothing follows, so there
                // is no infix to bar.
                None => return,
            }
        }
        rest = trimmed;
        break;
    }
    if saw_newline && !rest.is_empty() {
        MARK.with(|m| {
            m.set(Some(EndingBrace {
                brace_end: pos(after_brace),
                next_token: pos(rest),
            }))
        });
    }
}

/// True when `r` (an input already positioned past the whitespace following an
/// expression) sits exactly at a token that a statement-ending `}` separated
/// from that expression: the statement ended at that brace, so neither an
/// infix operator nor a statement modifier spelled at `r` continues it.
// Cost: O(1).
pub(crate) fn at_stmt_ending_brace(r: &str) -> bool {
    MARK.with(|m| m.get().is_some_and(|mark| mark.next_token == pos(r)))
}

/// An infix operator spelled at `r` is not an infix when a statement-ending
/// `}` precedes it (see [`at_stmt_ending_brace`]).
// Cost: O(1).
pub(crate) fn infix_barred_by_stmt_ending_brace(r: &str) -> bool {
    at_stmt_ending_brace(r)
}

/// Whether `p` lies in `input[..=rest]`: a suffix of the same buffer that
/// starts at or after `input` and at or before `rest`.
fn within(p: Pos, input: &str, rest: &str) -> bool {
    let (start, end) = (
        input.as_ptr() as usize,
        input.as_ptr() as usize + input.len(),
    );
    let rest_start = rest.as_ptr() as usize;
    p.0 >= start && p.0 <= rest_start && p.0 + p.1 == end && rest_start + rest.len() == end
}

/// Record that a block *term* — a bare block, a pointy block or a hash
/// composer — just ended at `after`. Routine bodies and statement-prefix
/// blocks (`sub { }`, `do { }`) are not block terms.
// Cost: O(1).
pub(crate) fn mark_block_term(after: &str) {
    LAST_BLOCK_TERM.with(|m| m.set(Some(pos(after))));
}

/// True when a block term was parsed inside the expression spanning `input`
/// up to `rest`. The `for` parser uses it to tell `for 1, 2, { ... }` (the
/// loop block was gobbled by the list) from a merely missing block, matching
/// rakudo, which reports the gobble for a block term anywhere in the
/// iterable (`for (1, {2})`, `for 1, {2}, 3`).
// Cost: O(1).
pub(crate) fn block_term_within(input: &str, rest: &str) -> bool {
    LAST_BLOCK_TERM
        .with(Cell::get)
        .is_some_and(|p| within(p, input, rest))
}

/// The marks a parse left behind: those whose brace closed inside the
/// consumed span. Stored with a memo entry so a hit can [`replay`] them.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct Snapshot {
    mark: Option<EndingBrace>,
    last_block_term: Option<Pos>,
}

/// Capture the marks the parse of `input` up to `rest` left behind.
// Cost: O(1).
pub(crate) fn snapshot_within(input: &str, rest: &str) -> Snapshot {
    Snapshot {
        mark: MARK
            .with(Cell::get)
            .filter(|m| within(m.brace_end, input, rest)),
        last_block_term: LAST_BLOCK_TERM
            .with(Cell::get)
            .filter(|p| within(*p, input, rest)),
    }
}

/// Re-establish the marks a memoized parse left behind (see [`snapshot_within`]).
// Cost: O(1).
pub(crate) fn replay(snapshot: &Snapshot) {
    if let Some(mark) = snapshot.mark {
        MARK.with(|m| m.set(Some(mark)));
    }
    if let Some(p) = snapshot.last_block_term {
        LAST_BLOCK_TERM.with(|m| m.set(Some(p)));
    }
}
