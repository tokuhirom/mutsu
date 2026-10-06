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
    Ok {
        consumed: usize,
        value: Box<T>,
        /// The statement-ending-brace marks the parse left behind, replayed
        /// on a hit (`parser::stmt_ending_brace`).
        braces: super::stmt_ending_brace::Snapshot,
    },
    /// Like `Ok`, but `rest` was not a subslice of the memoized `input` — it
    /// pointed into a permanently leaked buffer instead (see
    /// `primary::is_within_leaked_region`), so it is recovered by raw
    /// pointer/length rather than by an offset into `input`.
    OkLeaked {
        rest_ptr: usize,
        rest_len: usize,
        value: Box<T>,
    },
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
/// input, silently corrupting the parse. Two things make that impossible:
///
/// * A per-parse generation. Within one generation every live buffer's
///   `(ptr, len)` is unique, and entries from other generations never match.
/// * A key is made only for text inside the buffer the current generation was
///   begun for (or inside a permanently leaked region). A temporary `String`
///   built and parsed *inside* a generation — `format!("({args})")` for a trait
///   argument list is one — has no key at all, so it is parsed fresh and can
///   never be answered by the entry of an earlier temporary that sat at the
///   same address with the same length (#12065: `native('c', v6)` read back for
///   `native('m', v6)`). The soundness no longer depends on every call site
///   remembering to open a generation around its own scratch buffer.
pub(super) type MemoKey = (u64, usize, usize);

/// The parse generation in force and the live buffer it covers, as a half-open
/// address range. Outside any parse the range is empty, so nothing is keyed.
#[derive(Clone, Copy)]
struct Scope {
    generation: u64,
    start: usize,
    end: usize,
}

const NO_SCOPE: Scope = Scope {
    generation: 0,
    start: 0,
    end: 0,
};

thread_local! {
    static CURRENT_SCOPE: Cell<Scope> = const { Cell::new(NO_SCOPE) };
    static NEXT_GENERATION: Cell<u64> = const { Cell::new(1) };
}

/// RAII guard returned by `begin_parse_generation()` /
/// `begin_buffer_generation()`; restores the enclosing parse's scope on drop
/// (the outer buffer is still alive, so its keys are valid again).
pub(super) struct ParseGenerationGuard {
    prev: Scope,
}

impl Drop for ParseGenerationGuard {
    fn drop(&mut self) {
        CURRENT_SCOPE.with(|c| c.set(self.prev));
    }
}

/// A generation number that no parse has used. Never reused: a nested parse
/// must not share the generation of any parse whose buffer has been freed.
fn fresh_generation() -> u64 {
    NEXT_GENERATION.with(|n| {
        let v = n.get();
        n.set(v + 1);
        v
    })
}

/// Enter a fresh parse generation over the *same* live buffer as the enclosing
/// one, for a lexical scope whose declarations change what the same text means
/// (`block_with_pointy_params`, a routine body with parameters): entries made
/// under the enclosing scope must not be replayed inside it.
// Cost: O(1).
pub(super) fn begin_parse_generation() -> ParseGenerationGuard {
    let prev = CURRENT_SCOPE.with(|c| c.get());
    CURRENT_SCOPE.with(|c| {
        c.set(Scope {
            generation: fresh_generation(),
            ..prev
        })
    });
    ParseGenerationGuard { prev }
}

/// Enter a fresh parse generation for one `parse_program` /
/// `parse_program_recovering` call over `buffer`, the text every memo key of
/// this parse points into. The caller keeps `buffer` alive until the guard
/// drops.
// Cost: O(1).
pub(super) fn begin_buffer_generation(buffer: &str) -> ParseGenerationGuard {
    let prev = CURRENT_SCOPE.with(|c| c.get());
    let start = buffer.as_ptr() as usize;
    CURRENT_SCOPE.with(|c| {
        c.set(Scope {
            generation: fresh_generation(),
            start,
            end: start.saturating_add(buffer.len()),
        })
    });
    ParseGenerationGuard { prev }
}

/// Build the memo key for `input` under the current parse generation, or
/// `None` when `input` is not inside the buffer that generation covers (a
/// temporary buffer: it has no identity a later buffer cannot share). Shared
/// with sibling pointer-keyed tables (`STMT_ANON_STATES_TLS`) so they stay
/// sound the same way the memo tables do.
// Cost: O(1) for text inside the parse's buffer; O(r) otherwise, r = leaked
// heredoc regions (usually none).
pub(in crate::parser) fn memo_key(input: &str) -> Option<MemoKey> {
    let scope = CURRENT_SCOPE.with(|c| c.get());
    let start = input.as_ptr() as usize;
    let end = start.saturating_add(input.len());
    // A leaked region is never freed, so its addresses are unique forever.
    if (start >= scope.start && end <= scope.end) || super::primary::is_within_leaked_region(input)
    {
        Some((scope.generation, start, input.len()))
    } else {
        None
    }
}

/// [`memo_key`] for a table that only *compares* keys of a short-lived record
/// (`PENDING_EXTRA_MODIFIER`): text outside the parse's buffer gets a key in a
/// namespace of its own instead of none, so the record is still matched for
/// the same `(ptr, len)`.
// Cost: as `memo_key`.
pub(in crate::parser) fn record_key(input: &str) -> MemoKey {
    memo_key(input).unwrap_or((u64::MAX, input.as_ptr() as usize, input.len()))
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

    /// Look up a cached parse result. Returns `None` on cache miss.
    pub fn get<'a>(&self, input: &'a str) -> Option<PResult<'a, T>> {
        if !super::parse_memo_enabled() {
            return None;
        }
        let key = memo_key(input)?;
        let hit = self.memo.with(|m| m.borrow().get(&key).cloned());
        if let Some(entry) = hit {
            self.stats.with(|s| s.borrow_mut().hits += 1);
            return Some(match entry {
                MemoEntry::Ok {
                    consumed,
                    value,
                    braces,
                } => {
                    super::stmt_ending_brace::replay(&braces);
                    Ok((&input[consumed..], *value))
                }
                MemoEntry::OkLeaked {
                    rest_ptr,
                    rest_len,
                    value,
                } => {
                    // SAFETY: `store` only creates this variant when the rest
                    // pointer/length were taken from a live `&str` inside a
                    // region that `is_within_leaked_region` confirmed is
                    // `Box::leak`ed and therefore never freed — reconstructing
                    // it here is exactly as valid as the borrow it came from.
                    let rest = unsafe {
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                            rest_ptr as *const u8,
                            rest_len,
                        ))
                    };
                    Ok((rest, *value))
                }
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
        let Some(key) = memo_key(input) else {
            return;
        };
        // Memoization assumes `rest` is a subslice of `input` so we can
        // recover it later as `&input[consumed..]`. Some parsers (notably
        // heredoc forms whose marker line carries trailing code) instead
        // synthesize a combined remainder that lives in a permanently
        // leaked buffer. That is just as safe to record — by raw
        // pointer/length instead of an offset into `input` — as long as the
        // buffer never gets freed, which `is_within_leaked_region` confirms.
        // Refusing those entries outright (as opposed to recording them by
        // pointer) meant every backtracking attempt over such a heredoc
        // re-parsed and re-leaked it from scratch, multiplying cost at every
        // level of block nesting (#9674). Anything else that is neither a
        // subslice nor a registered leak is not safe to recover later, so it
        // is still left uncached.
        let entry = match result {
            Ok((rest, value)) => {
                let rest: &str = rest;
                let input_start = input.as_ptr() as usize;
                let input_end = input_start.saturating_add(input.len());
                let rest_start = rest.as_ptr() as usize;
                let rest_end = rest_start.saturating_add(rest.len());
                let rest_is_subslice =
                    rest_start >= input_start && rest_end <= input_end && rest.len() <= input.len();
                if rest_is_subslice {
                    MemoEntry::Ok {
                        consumed: input.len().saturating_sub(rest.len()),
                        value: Box::new(value.clone()),
                        braces: super::stmt_ending_brace::snapshot_within(input, rest),
                    }
                } else if super::primary::is_within_leaked_region(rest) {
                    MemoEntry::OkLeaked {
                        rest_ptr: rest_start,
                        rest_len: rest.len(),
                        value: Box::new(value.clone()),
                    }
                } else {
                    return;
                }
            }
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
        let outer = begin_buffer_generation(input);

        let result: PResult<'_, i32> = Ok((&input[3..], 1));
        TEST_MEMO.store(input, &result);
        assert!(matches!(TEST_MEMO.get(input), Some(Ok((_, 1)))));

        {
            // A nested scope over the same buffer must neither see the outer
            // entry nor leak its own entry back out.
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

    /// #12065: a scratch `String` built and parsed inside a parse (a trait's
    /// argument list wrapped in parentheses) is dropped, and the next scratch
    /// `String` lands at the same address with the same length. It is not part
    /// of the parse's buffer, so it has no key: nothing is stored for it and
    /// nothing is served to its successor.
    #[test]
    fn a_scratch_buffer_inside_a_parse_is_not_memoized() {
        if !crate::parser::parse_memo_enabled() {
            return;
        }
        TEST_MEMO.reset();
        let program = String::from("sub f() is native('c', v6) { * }");
        let _parse = begin_buffer_generation(&program);

        // Text inside the program's buffer is keyed and memoized.
        let inside: &str = &program[4..];
        let kept: PResult<'_, i32> = Ok((&inside[2..], 7));
        TEST_MEMO.store(inside, &kept);
        assert!(matches!(TEST_MEMO.get(inside), Some(Ok((_, 7)))));
        assert!(memo_key(inside).is_some());

        // The same scratch buffer, rewritten in place: same address, same length.
        let mut scratch = String::from("('c', v6)");
        let address = scratch.as_ptr();
        assert!(memo_key(&scratch).is_none(), "a scratch buffer has no key");
        let first: PResult<'_, i32> = Ok((&scratch[9..], 1));
        TEST_MEMO.store(&scratch, &first);
        scratch.clear();
        scratch.push_str("('m', v6)");
        assert_eq!(scratch.as_ptr(), address, "the buffer must not move");
        assert!(
            TEST_MEMO.get(&scratch).is_none(),
            "the successor must not be answered with the predecessor's entry"
        );
        TEST_MEMO.reset();
    }

    /// The same shape through the real expression memo: `parse_sub_traits`
    /// re-parses `format!("({args})")` of `is native('c', v6)` and then of
    /// `is native('m', v6)`. Rewriting one scratch `String` in place gives both
    /// the same address and length deterministically.
    #[test]
    fn the_expression_memo_does_not_answer_a_rewritten_scratch_buffer() {
        crate::parser::expr::reset_expression_memo();
        let program = String::from("sub a() is native('c', v6) { * }");
        let _parse = begin_buffer_generation(&program);
        let render = |text: &str| match crate::parser::expr::expression(text) {
            Ok((_, expr)) => format!("{expr:?}"),
            Err(_) => String::from("parse error"),
        };
        let mut scratch = String::from("('c', v6)");
        let address = scratch.as_ptr();
        let first = render(&scratch);
        assert!(first.contains("\"c\""), "{first}");
        scratch.clear();
        scratch.push_str("('m', v6)");
        assert_eq!(scratch.as_ptr(), address, "the buffer must not move");
        let second = render(&scratch);
        assert!(second.contains("\"m\""), "{second}");
        assert!(!second.contains("\"c\""), "{second}");
        crate::parser::expr::reset_expression_memo();
    }

    #[test]
    fn nothing_is_keyed_outside_a_parse() {
        let loose = String::from("abcdef");
        assert!(memo_key(&loose).is_none());
        // A record key (compared, never replayed) still distinguishes it.
        assert_eq!(record_key(&loose), record_key(&loose));
    }
}
