//! A single-slot memo for `Value::Str` -> `Vec<char>` conversion, shared by
//! the codepoint-indexed `nqp::` string ops (`nqp_ops_str.rs`,
//! `nqp_ops_text.rs`).
//!
//! A hand-rolled NQP scanner calls these ops once per character (or per
//! token) over the SAME string argument -- JSON::Fast's own JSON parser is
//! the case that surfaced this, walking a document with `nqp::iscclass`,
//! `nqp::findnotcclass` and `nqp::index($text, '"', $pos)` alike. Each op
//! collecting its string argument into a fresh `Vec<char>` on every call
//! turned a 332KB JSON document (a `License::SPDX` resource file, pulled in
//! by App::ShowPath's dependency chain) into an effective hang: O(n) work
//! repeated O(n) times.

use crate::value::{Value, ValueView};
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

/// A cached string's own `Arc` (kept alive to rule out ABA on its pointer
/// identity, see `CHAR_CACHE`) paired with its already-collected chars.
type CachedEntry = (Arc<String>, Rc<Vec<char>>);

std::thread_local! {
    /// Keyed by the `Arc<String>` payload's own identity. The cache holds
    /// its own `Arc` clone (a cheap refcount bump) so a dropped-and-
    /// reallocated `Arc<String>` can never alias a stale entry by pointer
    /// identity (ABA).
    static CHAR_CACHE: RefCell<Option<CachedEntry>> = const { RefCell::new(None) };
}

/// `args[i]`'s characters, memoized across consecutive calls against the
/// same underlying string value. Falls back to a fresh, uncached
/// `Vec<char>` for a non-`Str` value (numbers etc. coerced via `Display`),
/// which no caller of this helper hits on the hot scanning path.
pub(crate) fn cached_chars(args: &[Value], i: usize) -> Rc<Vec<char>> {
    if let Some(ValueView::Str(s)) = args.get(i).map(Value::view) {
        let arc = Arc::clone(&s);
        let hit = CHAR_CACHE.with(|cache| {
            cache
                .borrow()
                .as_ref()
                .filter(|(cached_arc, _)| Arc::ptr_eq(cached_arc, &arc))
                .map(|(_, v)| v.clone())
        });
        if let Some(v) = hit {
            return v;
        }
        let fresh = Rc::new(arc.chars().collect::<Vec<char>>());
        CHAR_CACHE.with(|cache| *cache.borrow_mut() = Some((arc, fresh.clone())));
        return fresh;
    }
    Rc::new(
        args.get(i)
            .map(|v| v.to_string_value())
            .unwrap_or_default()
            .chars()
            .collect(),
    )
}
