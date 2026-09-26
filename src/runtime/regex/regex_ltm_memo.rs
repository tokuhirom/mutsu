//! Memo for ADR-0022 declarative-prefix measurements
//! ([#9579](https://github.com/tokuhirom/mutsu/issues/9579)).
//!
//! `ltm_prefix_len_at` measures a `|` branch by running the real matcher in
//! declarative mode. When the branch calls a recursive subrule, that walk
//! meets the subrule's own `|` alternations, and ranking each of them measures
//! its branches again — so `token A { '{' [ <A> | . ]*? '}' }` measured the
//! inner `<A>` at the same position once per path that reached it, and the
//! cost multiplied at every nesting level. Rakudo compiles the declarative
//! prefix into an NFA once per rule, so its cost is linear.
//!
//! This memo makes each `(branch, position)` measurement happen once per
//! *outermost* measurement. The scope is what makes the key sound:
//!
//! - **No user code runs inside it.** A measurement never executes code
//!   (ADR-0009): code atoms, `.wrap`ped tokens, dynamic-parameter
//!   installation and action methods are all neutralized under
//!   `LTM_DECLARATIVE_MODE`. So nothing the walk can read — the rule
//!   registry, lexical regexes, `$*` variables, the match target — changes
//!   between two nested measurements of one scope — except what the walk
//!   itself installs: a subrule's `$*` parameters and a lexical regex's
//!   closure scope are spliced into `env` for the call and removed after it,
//!   and a subrule body resolved under them can differ (`token y { $*S }`
//!   called from two rules with different defaults). Every such install and
//!   removal moves [`note_env_scope_change`]'s epoch, which is part of the
//!   key, so an entry only answers under the very bindings it was taken
//!   under. The token generation is carried the same way, as a cheap guard.
//! - **Identities are pinned.** An entry keeps its pattern's `derived` `Arc`
//!   alive, so the address used as the key cannot be reused by another
//!   pattern while the scope lives; and only measurements over the outermost
//!   call's own `chars` slice are memoized, which outlives the scope.
//! - **Left recursion is excluded.** The one piece of state that does change
//!   inside the scope is the left-recursion table: a re-entry reads a growing
//!   seed, so a measurement taken while a seed is being grown is not a
//!   function of its key. A measurement whose walk consulted any live
//!   left-recursion activation (counted by
//!   [`super::regex_lr_state::lr_consult_count`]) is neither stored nor
//!   allowed to taint an enclosing entry. A stored entry is then safe to reuse:
//!   a walk only moves forward, so the only activation a later re-run could
//!   consult that the stored one did not is one at this very position — and
//!   reaching this measurement from inside such an activation means the rule
//!   reaches this branch without consuming, so the stored walk (which entered
//!   that rule at this position itself) would have re-entered it and been
//!   excluded.

use super::super::*;
use super::regex_helpers::{LTM_PREFIX_TERMINATED, LTM_SEQALT_EPSILON, NamedRegexLookupSpec};
use super::regex_ltm_fate::{ltm_fate_frame_close, ltm_fate_frame_open, ltm_record_fate};
use rustc_hash::FxHashMap as HashMap;
use std::cell::Cell;
use std::cell::RefCell;
use std::sync::Arc;

/// `(pattern identity, position, package, token generation, env epoch)`.
type MemoKey = (usize, usize, Symbol, u64, u64);

struct MemoEntry {
    /// Pins the pattern whose address is part of the key.
    _pattern: Arc<PatternDerived>,
    result: (Option<usize>, bool),
}

/// `(subrule spec identity, position, package, token generation, env epoch,
/// flags)`, where the flags are the caller's `first_only` and `:i`.
type SubruleKey = (usize, usize, Symbol, u64, u64, bool, bool);

/// One `<subrule>` call's result inside a measurement, with everything the
/// walk left behind in the measurement's thread-locals so a hit can replay it.
struct SubruleEntry {
    /// Pins the spec whose address is part of the key.
    _spec: Arc<NamedRegexLookupSpec>,
    ends: Vec<(usize, RegexCaptures)>,
    fate: Option<usize>,
    terminated: bool,
    epsilon: bool,
}

struct LtmMemo {
    chars_ptr: *const char,
    chars_len: usize,
    entries: HashMap<MemoKey, MemoEntry>,
    subrules: HashMap<SubruleKey, SubruleEntry>,
}

thread_local! {
    /// The live scope, if an outermost measurement is in progress.
    static LTM_MEMO: RefCell<Option<LtmMemo>> = const { RefCell::new(None) };

    /// Moves whenever the regex engine splices bindings into `env` or takes
    /// them back out (see the module docs).
    static ENV_SCOPE_EPOCH: Cell<u64> = const { Cell::new(0) };
}

/// The regex engine changed which bindings `env` holds for a subrule call
/// (installed or restored `$*` parameters, a regex closure scope).
// Cost: O(1).
pub(crate) fn note_env_scope_change() {
    ENV_SCOPE_EPOCH.with(|e| e.set(e.get().wrapping_add(1)));
}

fn env_scope_epoch() -> u64 {
    ENV_SCOPE_EPOCH.with(Cell::get)
}

/// What [`ltm_memo_enter`] decided for one measurement.
pub(super) enum LtmMemoSlot {
    /// This measurement opened the scope; dropping the slot closes it (also on
    /// unwind, so a stale scope can never answer for a later subject that
    /// happens to reuse the same buffer address).
    Outermost,
    /// A nested measurement over the scope's subject: look up / store.
    Nested(MemoKey),
    /// A nested measurement over some other subject (a mark-stripped or
    /// folded copy): measured without the memo.
    Bypass,
}

fn memo_key(pattern: &RegexPattern, pos: usize, pkg: Symbol) -> MemoKey {
    let generation =
        crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
    (
        Arc::as_ptr(&pattern.derived) as usize,
        pos,
        pkg,
        generation,
        env_scope_epoch(),
    )
}

/// Open the scope, or classify a nested measurement. Returns the cached result
/// when a nested measurement was already taken.
// Cost: O(1) expected (one hash probe).
pub(super) fn ltm_memo_enter(
    pattern: &RegexPattern,
    chars: &[char],
    pos: usize,
    pkg: Symbol,
) -> Result<LtmMemoSlot, (Option<usize>, bool)> {
    let key = memo_key(pattern, pos, pkg);
    LTM_MEMO.with(|m| {
        let mut m = m.borrow_mut();
        match m.as_ref() {
            None => {
                *m = Some(LtmMemo {
                    chars_ptr: chars.as_ptr(),
                    chars_len: chars.len(),
                    entries: HashMap::default(),
                    subrules: HashMap::default(),
                });
                Ok(LtmMemoSlot::Outermost)
            }
            Some(memo) if memo.chars_ptr == chars.as_ptr() && memo.chars_len == chars.len() => {
                match memo.entries.get(&key) {
                    Some(hit) => Err(hit.result),
                    None => Ok(LtmMemoSlot::Nested(key)),
                }
            }
            Some(_) => Ok(LtmMemoSlot::Bypass),
        }
    })
}

/// Record a finished nested measurement, unless its walk consulted
/// left-recursion state (see the module docs).
// Cost: O(1) expected (one hash insert).
pub(super) fn ltm_memo_store(
    slot: &LtmMemoSlot,
    pattern: &RegexPattern,
    consulted_lr: bool,
    result: (Option<usize>, bool),
) {
    let LtmMemoSlot::Nested(key) = slot else {
        return;
    };
    if consulted_lr {
        return;
    }
    LTM_MEMO.with(|m| {
        if let Some(memo) = m.borrow_mut().as_mut() {
            memo.entries.insert(
                *key,
                MemoEntry {
                    _pattern: Arc::clone(&pattern.derived),
                    result,
                },
            );
        }
    });
}

impl Drop for LtmMemoSlot {
    // Cost: O(e), e = entries stored during the scope (dropped here).
    fn drop(&mut self) {
        if matches!(self, LtmMemoSlot::Outermost) {
            let dropped = LTM_MEMO.with(|m| m.borrow_mut().take());
            drop(dropped);
        }
    }
}

/// A `<subrule>` call made while a measurement is in progress, answered from
/// the scope's memo when the same call at the same position was already
/// walked (#9579).
///
/// Measuring `[ <A> | . ]*?` collects the ends of the inner `<A>` call at every
/// position the loop reaches, and each of those walks reaches the next level's
/// calls again; without this the walk is exponential in the nesting depth even
/// once the rank measurements themselves are memoized. The call's ends are a
/// function of the same key as a measurement (see the module docs), plus the
/// caller's `first_only` and `:i`. Its captures are carried along unchanged:
/// they are keyed by the spec, so they are the ones the walk would build.
///
/// A walk also leaves three things in thread-locals — the furthest fate, and
/// the "terminated" and "`||` epsilon" flags — so the call runs isolated from
/// the enclosing values, and both a miss and a hit fold its own values back
/// in, exactly as the walk would have.
// Cost: O(1) expected on a hit, plus cloning the stored ends; a miss costs the
// walk itself plus one clone of its ends.
#[allow(clippy::too_many_arguments)]
pub(super) fn ltm_memo_subrule_ends(
    interp: &mut Interpreter,
    spec: &Arc<NamedRegexLookupSpec>,
    chars: &[char],
    pos: usize,
    pkg: Symbol,
    first_only: bool,
    ignore_case: bool,
    walk: impl FnOnce(&mut Interpreter) -> Vec<(usize, RegexCaptures)>,
) -> Vec<(usize, RegexCaptures)> {
    let generation =
        crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
    let key: SubruleKey = (
        Arc::as_ptr(spec) as usize,
        pos,
        pkg,
        generation,
        env_scope_epoch(),
        first_only,
        ignore_case,
    );
    // `None`: no scope, or a subject other than the scope's — plain walk.
    let lookup = LTM_MEMO.with(|m| {
        let m = m.borrow();
        let memo = m.as_ref()?;
        if memo.chars_ptr != chars.as_ptr() || memo.chars_len != chars.len() {
            return None;
        }
        Some(
            memo.subrules
                .get(&key)
                .map(|e| (e.ends.clone(), e.fate, e.terminated, e.epsilon)),
        )
    });
    let Some(lookup) = lookup else {
        return walk(interp);
    };
    if let Some((ends, fate, terminated, epsilon)) = lookup {
        fold_walk_state(fate, terminated, epsilon);
        return ends;
    }
    let saved_terminated = LTM_PREFIX_TERMINATED.with(|f| f.replace(false));
    let saved_epsilon = LTM_SEQALT_EPSILON.with(|f| f.replace(false));
    let enclosing_fate = ltm_fate_frame_open();
    let lr_before = super::regex_lr_state::lr_consult_count();
    let ends = walk(interp);
    let consulted_lr = super::regex_lr_state::lr_consult_count() != lr_before;
    let fate = ltm_fate_frame_close(enclosing_fate);
    let terminated = LTM_PREFIX_TERMINATED.with(|f| f.replace(saved_terminated));
    let epsilon = LTM_SEQALT_EPSILON.with(|f| f.replace(saved_epsilon));
    fold_walk_state(fate, terminated, epsilon);
    if !consulted_lr {
        LTM_MEMO.with(|m| {
            if let Some(memo) = m.borrow_mut().as_mut() {
                memo.subrules.insert(
                    key,
                    SubruleEntry {
                        _spec: Arc::clone(spec),
                        ends: ends.clone(),
                        fate,
                        terminated,
                        epsilon,
                    },
                );
            }
        });
    }
    ends
}

/// Fold one walk's fate and flags into the enclosing measurement's, the way
/// the walk itself would have left them.
fn fold_walk_state(fate: Option<usize>, terminated: bool, epsilon: bool) {
    if terminated {
        LTM_PREFIX_TERMINATED.with(|f| f.set(true));
    }
    if epsilon {
        LTM_SEQALT_EPSILON.with(|f| f.set(true));
    }
    if let Some(fate) = fate {
        ltm_record_fate(fate);
    }
}
