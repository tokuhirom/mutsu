//! rakudo's `infixstopper` for `$*IN_REGEX_ASSERTION` (#16888): while a slang
//! asks the parser for a core production with that dynamic set (EEL 0.3's
//! `<name: args>` calls, whose `arglist` override sets it), a `>` ends the
//! expression instead of starting an infix, so `<choose: 4>` reads `4`.
//!
//! The stop applies at the production's own bracket level only: EEL's overrides
//! of `circumfix:sym<( )>`, `<[ ]>`, `<{ }>` and `block` reset the dynamic, so
//! `<.bla: :value(* > 40)>` keeps its comparison.
// TODO: run the slang's own circumfix overrides when the parser enters a
// bracket (ADR-16888's wrapper hook) instead of assuming they reset the
// dynamic: core rakudo keeps it set inside brackets, where `<f: (1 > 2)>` is a
// parse error that this accepts.

use std::cell::RefCell;
use std::rc::Rc;

thread_local! {
    /// The text of the production being parsed with the stopper on.
    static STOPPER: RefCell<Option<Rc<str>>> = const { RefCell::new(None) };
}

/// Restores the stopper in force before a back-call, however it ends.
pub(crate) struct StopperGuard(Option<Rc<str>>);

impl Drop for StopperGuard {
    fn drop(&mut self) {
        let previous = self.0.take();
        STOPPER.with(|s| *s.borrow_mut() = previous);
    }
}

/// A core production at the start of `text` is parsed with
/// `$*IN_REGEX_ASSERTION` `on`; hold the guard while it is.
// Cost: O(n) when `on`, n = length of `text`; O(1) otherwise.
pub(crate) fn enter(text: &str, on: bool) -> StopperGuard {
    let new = on.then(|| Rc::from(text));
    StopperGuard(STOPPER.with(|s| std::mem::replace(&mut *s.borrow_mut(), new)))
}

/// Whether the infix that would start at `rest` is stopped: the stopper is on,
/// `rest` begins with `>` and stands outside every bracket the production
/// opened.
// Cost: O(1) unless `rest` starts with `>` while the stopper is on; then O(n),
// n = length of the production read so far.
pub(in crate::parser) fn stops_at(rest: &str) -> bool {
    if !rest.starts_with('>') {
        return false;
    }
    STOPPER.with(|s| {
        let s = s.borrow();
        let Some(text) = s.as_deref() else {
            return false;
        };
        // `rest` is a suffix of the production's text when it is read from it.
        let Some(read) = text.len().checked_sub(rest.len()) else {
            return false;
        };
        if !text.is_char_boundary(read) || &text[read..] != rest {
            return false;
        }
        at_top_level(&text[..read])
    })
}

/// Whether every bracket `read` opened is closed again, skipping quoted text.
// Cost: O(n), n = length of `read`.
fn at_top_level(read: &str) -> bool {
    let mut depth = 0usize;
    let mut chars = read.chars();
    while let Some(c) = chars.next() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth = depth.saturating_sub(1),
            '\'' | '"' => {
                while let Some(q) = chars.next() {
                    if q == '\\' {
                        chars.next();
                    } else if q == c {
                        break;
                    }
                }
            }
            _ => {}
        }
    }
    depth == 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stops_only_at_the_productions_own_level() {
        let text = "4, :v(* > 40), :t('(')> rest";
        let _guard = enter(text, true);
        let inner = text.find("> 40").unwrap();
        assert!(!stops_at(&text[inner..]));
        let outer = text.find("> rest").unwrap();
        assert!(stops_at(&text[outer..]));
    }

    #[test]
    fn off_without_the_dynamic() {
        let text = "4> rest";
        let _guard = enter(text, false);
        assert!(!stops_at(&text[1..]));
    }
}
