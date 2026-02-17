//! Generic memoization for parser combinators.
//!
//! Each memo table stores parse results keyed by `(ptr, len)` of the input `&str`.
//! Provides `get()`, `store()`, `reset()`, and `stats()` via thread-local storage.

use super::parse_result::{PError, PResult};
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(super) enum MemoEntry<T: Clone> {
    Ok { consumed: usize, value: Box<T> },
    Err(PError),
}

#[derive(Debug, Default, Clone, Copy)]
pub(super) struct MemoStats {
    pub hits: usize,
    pub misses: usize,
    pub stores: usize,
}

/// A thread-local memoization table for parser results.
///
/// Create a static instance via `ParseMemo::new()` referencing thread-local storage,
/// then call `get()`, `store()`, `reset()`, and `stats()`.
type MemoMap<T> = RefCell<HashMap<(usize, usize), MemoEntry<T>>>;

pub(super) struct ParseMemo<T: Clone + 'static> {
    memo: &'static std::thread::LocalKey<MemoMap<T>>,
    stats: &'static std::thread::LocalKey<RefCell<MemoStats>>,
}

impl<T: Clone + 'static> ParseMemo<T> {
    pub const fn new(
        memo: &'static std::thread::LocalKey<MemoMap<T>>,
        stats: &'static std::thread::LocalKey<RefCell<MemoStats>>,
    ) -> Self {
        ParseMemo { memo, stats }
    }

    fn key(input: &str) -> (usize, usize) {
        (input.as_ptr() as usize, input.len())
    }

    /// Look up a cached parse result. Returns `None` on cache miss.
    pub fn get<'a>(&self, input: &'a str) -> Option<PResult<'a, T>> {
        if !super::parse_memo_enabled() {
            return None;
        }
        let key = Self::key(input);
        let hit = self.memo.with(|m| m.borrow().get(&key).cloned());
        if let Some(entry) = hit {
            self.stats.with(|s| s.borrow_mut().hits += 1);
            return Some(match entry {
                MemoEntry::Ok { consumed, value } => Ok((&input[consumed..], *value)),
                MemoEntry::Err(err) => Err(err),
            });
        }
        self.stats.with(|s| s.borrow_mut().misses += 1);
        None
    }

    /// Store a parse result in the cache.
    pub fn store(&self, input: &str, result: &PResult<'_, T>) {
        if !super::parse_memo_enabled() {
            return;
        }
        let key = Self::key(input);
        let entry = match result {
            Ok((rest, value)) => MemoEntry::Ok {
                consumed: input.len().saturating_sub(rest.len()),
                value: Box::new(value.clone()),
            },
            Err(err) => MemoEntry::Err(err.clone()),
        };
        self.memo.with(|m| {
            m.borrow_mut().insert(key, entry);
        });
        self.stats.with(|s| s.borrow_mut().stores += 1);
    }

    /// Clear all cached entries and reset statistics.
    pub fn reset(&self) {
        if !super::parse_memo_enabled() {
            return;
        }
        self.memo.with(|m| m.borrow_mut().clear());
        self.stats.with(|s| *s.borrow_mut() = MemoStats::default());
    }

    /// Return `(hits, misses, stores)` statistics.
    pub fn stats(&self) -> (usize, usize, usize) {
        self.stats.with(|s| {
            let s = *s.borrow();
            (s.hits, s.misses, s.stores)
        })
    }
}
