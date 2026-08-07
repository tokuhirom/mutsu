//! Raku's "a block's closing brace at end of line terminates the statement" rule.
//!
//! `}` that closes a *block* and is followed by a newline is a statement
//! separator, so whatever starts the next line begins a new statement — even
//! when it is spelled like an infix operator:
//!
//! ```raku
//! g { 1 }
//! before { 2 }     # two calls, NOT `g({ 1 } before { 2 })`
//! ```
//!
//! Rakudo implements this with a `$*ENDSTMT` dynamic variable that its `ws`
//! rule consults. mutsu's parser has no single whitespace chokepoint the infix
//! layers share, so instead the block-term parser records **where the next
//! token after such a brace starts**, and each infix layer asks
//! [`infix_barred_by_stmt_ending_brace`] before consuming an operator there.
//!
//! The mark is a `(pointer, length)` pair of the next token's input slice.
//! Comparing both makes it an exact position identity: a stale mark left over
//! from an earlier parse cannot alias a position in a different buffer.

use std::cell::Cell;

thread_local! {
    /// `(ptr, len)` of the first token after a statement-ending `}`, or
    /// `(null, 0)` when there is none pending.
    static MARK: Cell<(*const u8, usize)> = const { Cell::new((std::ptr::null(), 0)) };
}

/// Record that a block's `}` just ended, with `after_brace` being the input
/// immediately following it. Sets the mark only when a newline separates the
/// brace from the next token — a `}` in the middle of a line does not end the
/// statement (`say ({ 1 } before { 2 })` really is an infix `before`).
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
        MARK.with(|m| m.set((rest.as_ptr(), rest.len())));
    }
}

/// True when `r` (an input already positioned past the whitespace following an
/// expression) sits exactly at a token that a statement-ending `}` separated
/// from that expression. An infix operator spelled there is not an infix.
pub(crate) fn infix_barred_by_stmt_ending_brace(r: &str) -> bool {
    MARK.with(|m| m.get() == (r.as_ptr(), r.len()))
}
