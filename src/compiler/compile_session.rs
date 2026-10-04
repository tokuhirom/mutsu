//! Compile sessions: where the values the compiler mints come from
//! (ADR-11756 §2.3).
//!
//! The compiler mints values that must be unique within the process: the
//! `Pkg::&<closure>/N` state-scope package of a closure, hidden locals such as
//! `__do_decl_init_N`, the `site_id` of an `END` phaser, the serial that keeps
//! two textually identical lexical subs' aliases apart. They used to come from
//! process-global counters, so a value meant nothing outside the process that
//! minted it, which is what stands in the way of caching compiled bytecode.
//!
//! A value is now minted as (session, ordinal):
//!
//! - The **ordinal** counts up within one top-level compile. A nested
//!   `Compiler::compile` (a declaration plan's chunk, a sub-compiler) and every
//!   sub-compiler share it, so values stay unique within the compile.
//! - The **session** is chosen per top-level compile. Today it is always drawn
//!   from a process-global counter, so each compile still mints values no other
//!   compile in the process shares, exactly as before. A later slice gives a
//!   cacheable module compile a content-addressed session, from a reserved half
//!   of the session space, so a cached chunk's values are valid in any process.
//!
//! Both halves are packed into one decimal-printable `u64`, because the minted
//! names are parsed elsewhere as `/<digits>` (a function key's arity suffix
//! rule, `dispatch_resolve::function_key_strip_arity_suffix`).

use std::cell::RefCell;
use std::sync::atomic::{AtomicU64, Ordering};

/// Bits of a minted value that hold the ordinal.
const ORDINAL_BITS: u32 = 24;
/// Ordinals per session before the session is renewed (see [`mint`]).
const ORDINALS_PER_SESSION: u32 = 1 << ORDINAL_BITS;
/// Bits a session number may use, so that `session << ORDINAL_BITS` fits a u64.
const SESSION_BITS: u32 = 64 - ORDINAL_BITS;
/// The reserved half of the session space for content-addressed sessions.
/// Counter sessions stay below it.
pub(crate) const CONTENT_SESSION_BIT: u64 = 1 << (SESSION_BITS - 1);

/// Process-global source of counter sessions.
static NEXT_COUNTER_SESSION: AtomicU64 = AtomicU64::new(0);

struct Session {
    id: u64,
    next_ordinal: u32,
}

thread_local! {
    /// The session of the top-level compile in progress on this thread. A
    /// stack only so that a guard can tell whether it opened the session: a
    /// compile started while another is running shares the outer session.
    static SESSIONS: RefCell<Vec<Session>> = const { RefCell::new(Vec::new()) };
}

// Cost: O(1).
fn counter_session() -> u64 {
    let id = NEXT_COUNTER_SESSION.fetch_add(1, Ordering::Relaxed);
    debug_assert!(id < CONTENT_SESSION_BIT, "counter sessions exhausted");
    id
}

/// Keeps a compile session open for the lifetime of the guard.
pub(crate) struct SessionGuard {
    opened: bool,
}

impl Drop for SessionGuard {
    fn drop(&mut self) {
        if self.opened {
            SESSIONS.with(|s| {
                s.borrow_mut().pop();
            });
        }
    }
}

/// Open a session for a top-level compile, unless one is already open on this
/// thread, in which case the caller is a nested compile and shares it.
// Cost: O(1).
pub(crate) fn enter() -> SessionGuard {
    let opened = SESSIONS.with(|s| {
        let mut s = s.borrow_mut();
        if s.is_empty() {
            s.push(Session {
                id: counter_session(),
                next_ordinal: 0,
            });
            true
        } else {
            false
        }
    });
    SessionGuard { opened }
}

/// The occurrences of content-addressed sessions already used in this process,
/// per unit key. Process-wide, because the values a session mints must be
/// unique across threads too.
fn claimed_occurrences() -> &'static std::sync::Mutex<rustc_hash::FxHashMap<u64, Vec<u32>>> {
    static CLAIMED: std::sync::OnceLock<std::sync::Mutex<rustc_hash::FxHashMap<u64, Vec<u32>>>> =
        std::sync::OnceLock::new();
    CLAIMED.get_or_init(Default::default)
}

/// The content-addressed session for the `occurrence`-th compile of the unit
/// identified by `unit_key` (a hash of its source identity). The same in every
/// process, and in the half of the session space counter sessions never reach.
// Cost: O(1).
pub(crate) fn content_session_id(unit_key: u64, occurrence: u32) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::hash::DefaultHasher::new();
    unit_key.hash(&mut hasher);
    occurrence.hash(&mut hasher);
    (hasher.finish() & (CONTENT_SESSION_BIT - 1)) | CONTENT_SESSION_BIT
}

/// The content-addressed session a module's *parse* mints its declaration
/// names and ids from (`anon_names::with_content_unit`): derived like
/// [`content_session_id`] but salted, so parse-time and compile-time values of
/// one module never share a session.
// Cost: O(1).
pub(crate) fn content_parse_session_id(unit_key: u64, occurrence: u32) -> u64 {
    content_session_id(unit_key ^ 0x7061_7273_655f_6964, occurrence)
}

/// Whether the innermost open session is content-addressed: a value minted
/// now ends up in a cacheable compile.
// Cost: O(1).
pub(crate) fn in_content_session() -> bool {
    SESSIONS.with(|s| {
        s.borrow()
            .last()
            .is_some_and(|session| session.id & CONTENT_SESSION_BIT != 0)
    })
}

/// Claim the lowest unused occurrence of `unit_key`, for a fresh compile.
// Cost: O(c), c = occurrences of this unit claimed so far.
pub(crate) fn claim_next_occurrence(unit_key: u64) -> u32 {
    let Ok(mut claimed) = claimed_occurrences().lock() else {
        // A poisoned table: an occurrence no other compile can share.
        return u32::MAX;
    };
    let taken = claimed.entry(unit_key).or_default();
    let next = (0..).find(|n| !taken.contains(n)).unwrap_or(u32::MAX);
    taken.push(next);
    next
}

/// Open the session `id` for the compile about to run, whatever is already
/// open: a cacheable compile mints from its content-addressed session.
// Cost: O(1).
pub(crate) fn enter_session(id: u64) -> SessionGuard {
    SESSIONS.with(|s| {
        s.borrow_mut().push(Session {
            id,
            next_ordinal: 0,
        })
    });
    SessionGuard { opened: true }
}

/// Mint a value no other mint in this process returns.
///
/// Outside any session (a compiler built and used without going through
/// `Compiler::compile`), each call takes a session of its own, which keeps the
/// uniqueness guarantee at the cost of one counter step per value, which is
/// what the old global counter cost too.
// Cost: O(1).
pub(crate) fn mint() -> u64 {
    SESSIONS.with(|s| {
        let mut s = s.borrow_mut();
        let Some(session) = s.last_mut() else {
            return counter_session() << ORDINAL_BITS;
        };
        if session.next_ordinal == ORDINALS_PER_SESSION {
            // A compile that mints 16M values moves to a fresh counter
            // session rather than wrapping into values it already handed out.
            session.id = counter_session();
            session.next_ordinal = 0;
        }
        let ordinal = session.next_ordinal;
        session.next_ordinal += 1;
        (session.id << ORDINAL_BITS) | u64::from(ordinal)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn values_are_unique_within_and_across_sessions() {
        let mut seen = std::collections::HashSet::new();
        for _ in 0..3 {
            let _guard = enter();
            for _ in 0..100 {
                assert!(seen.insert(mint()));
            }
        }
        for _ in 0..10 {
            assert!(seen.insert(mint()));
        }
    }

    #[test]
    fn content_sessions_are_stable_and_claimed_once() {
        let a = content_session_id(42, 0);
        assert_eq!(a, content_session_id(42, 0));
        assert_ne!(a, content_session_id(42, 1));
        assert!(a & CONTENT_SESSION_BIT != 0);
        let key = 0xdead_beef_u64;
        assert_eq!(claim_next_occurrence(key), 0);
        assert_eq!(claim_next_occurrence(key), 1);
        let _guard = enter_session(a);
        assert_eq!(mint() >> ORDINAL_BITS, a);
    }

    #[test]
    fn a_nested_compile_shares_the_open_session() {
        let _outer = enter();
        let a = mint();
        {
            let _inner = enter();
            let b = mint();
            assert_eq!(a >> ORDINAL_BITS, b >> ORDINAL_BITS);
            assert_eq!(b, a + 1);
        }
        assert_eq!(mint(), a + 2);
    }
}
