//! Generic memoization for parser combinators.
//!
//! Each memo table stores parse results keyed by `(generation, ptr, len)` of the
//! input `&str`. Provides `get()`, `store()`, `reset()`, and `stats()` via
//! thread-local storage.

use super::parse_result::{PError, PResult};
use std::cell::{Cell, RefCell};
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

/// Memo keys are `(generation, ptr, len)`. The raw `(ptr, len)` of a `&str`
/// only identifies a slice while its owning buffer is alive: nested parses
/// (module export scans, EVAL) parse short-lived `String` buffers that are
/// dropped mid-way through the enclosing parse, and the allocator readily
/// hands the freed address to an unrelated later allocation — so a bare
/// pointer key can return a stale entry from a dead buffer for different
/// input, silently corrupting the parse. Mixing in a per-parse generation
/// makes that impossible: within one generation every live buffer's
/// `(ptr, len)` is unique, and entries from other generations never match.
pub(super) type MemoKey = (u64, usize, usize);

thread_local! {
    static CURRENT_GENERATION: Cell<u64> = const { Cell::new(0) };
    static NEXT_GENERATION: Cell<u64> = const { Cell::new(1) };
}

/// RAII guard returned by `begin_parse_generation()`; restores the enclosing
/// parse's generation on drop (the outer buffer is still alive, so its keys
/// are valid again).
pub(super) struct ParseGenerationGuard {
    prev: u64,
}

impl Drop for ParseGenerationGuard {
    fn drop(&mut self) {
        CURRENT_GENERATION.with(|g| g.set(self.prev));
    }
}

/// Enter a fresh parse generation for the duration of one `parse_program` /
/// `parse_program_partial` call. Never reuses a generation number: a nested
/// parse must not share the generation of any parse whose buffer has been
/// freed.
pub(super) fn begin_parse_generation() -> ParseGenerationGuard {
    let prev = CURRENT_GENERATION.with(|g| g.get());
    let fresh = NEXT_GENERATION.with(|n| {
        let v = n.get();
        n.set(v + 1);
        v
    });
    CURRENT_GENERATION.with(|g| g.set(fresh));
    ParseGenerationGuard { prev }
}

/// Build the memo key for `input` under the current parse generation. Shared
/// with sibling pointer-keyed tables (`STMT_ANON_STATES_TLS`) so they stay
/// sound the same way the memo tables do.
pub(in crate::parser) fn memo_key(input: &str) -> MemoKey {
    (
        CURRENT_GENERATION.with(|g| g.get()),
        input.as_ptr() as usize,
        input.len(),
    )
}

/// A thread-local memoization table for parser results.
///
/// Create a static instance via `ParseMemo::new()` referencing thread-local storage,
/// then call `get()`, `store()`, `reset()`, and `stats()`.
type MemoMap<T> = RefCell<HashMap<MemoKey, MemoEntry<T>>>;

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

    fn key(input: &str) -> MemoKey {
        memo_key(input)
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
        // Memoization assumes `rest` is a subslice of `input` so we can
        // recover it later as `&input[consumed..]`. Some parsers (notably
        // heredoc forms) may synthesize a combined remainder string that is
        // not a subslice. Skip caching those entries to avoid corrupt results.
        if let Ok((rest, _)) = result {
            let input_start = input.as_ptr() as usize;
            let input_end = input_start.saturating_add(input.len());
            let rest_start = rest.as_ptr() as usize;
            let rest_end = rest_start.saturating_add(rest.len());
            let rest_is_subslice =
                rest_start >= input_start && rest_end <= input_end && rest.len() <= input.len();
            if !rest_is_subslice {
                return;
            }
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

#[cfg(test)]
mod tests {
    use super::*;

    thread_local! {
        static TEST_MEMO_TLS: MemoMap<i32> = RefCell::new(HashMap::new());
        static TEST_MEMO_STATS_TLS: RefCell<MemoStats> = RefCell::new(MemoStats::default());
    }
    static TEST_MEMO: ParseMemo<i32> = ParseMemo::new(&TEST_MEMO_TLS, &TEST_MEMO_STATS_TLS);

    #[test]
    fn generation_isolates_entries_at_the_same_address() {
        if !crate::parser::parse_memo_enabled() {
            return;
        }
        TEST_MEMO.reset();
        let buffer = String::from("abcdef");
        let input: &str = &buffer;
        let outer = begin_parse_generation();

        let result: PResult<'_, i32> = Ok((&input[3..], 1));
        TEST_MEMO.store(input, &result);
        assert!(matches!(TEST_MEMO.get(input), Some(Ok((_, 1)))));

        {
            // A nested parse of a buffer that happens to sit at the same
            // (ptr, len) — modeled with the very same slice — must neither see
            // the outer entry nor leak its own entry back out.
            let _nested = begin_parse_generation();
            assert!(TEST_MEMO.get(input).is_none());
            let nested_result: PResult<'_, i32> = Ok((&input[1..], 2));
            TEST_MEMO.store(input, &nested_result);
            assert!(matches!(TEST_MEMO.get(input), Some(Ok((_, 2)))));
        }

        assert!(matches!(TEST_MEMO.get(input), Some(Ok((_, 1)))));
        drop(outer);
        TEST_MEMO.reset();
    }
}
