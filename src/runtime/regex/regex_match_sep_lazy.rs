//! Demand-driven separated-quantifier candidates (ADR-0073, Slice 3).
//!
//! `match_separated_quantifier` enumerates the whole `atom (sep atom)*` chain
//! tree, records **every** node of that DFS as its own candidate, and only then
//! lets the walk pick one. Each node's atom is matched through the eager
//! producer, so an embedded `{ ... }` block inside the repeated atom ran once
//! per candidate length at every position the DFS visited:
//! `( \V* { ++$n } ) *%% \n` over three lines ran the block 17 times where raku
//! runs it 3.
//!
//! This module is the same DFS driven by the caller's continuation: each chain
//! node's candidates are reported as they are discovered, the atom's own
//! candidates come from `for_each_atom_candidate` (so they are demand-driven
//! too), and the recursion stops as soon as the continuation accepts. The
//! collecting `match_separated_quantifier` stays for the callers that genuinely
//! want the whole set (the no-capture matcher).

use super::super::*;
use super::regex_helpers::count_capture_groups;
use super::regex_match_lazy::AtomCandidateCont;
use super::regex_trail::CapStore;
use std::collections::HashSet;

/// Bound on visited chain nodes, mirroring the collecting path's 20k cap on
/// catastrophic backtracking over a frugal separator.
const SEP_CHAIN_BUDGET: u32 = 20_000;

/// The mutable state of one separated-quantifier chain walk. The token and
/// pattern are deliberately NOT held here: the DFS borrows this struct mutably
/// across the atom driver's callback, so anything read from the token while the
/// callback is live has to be a separate borrow.
struct SepChainWalk {
    min: usize,
    max: Option<usize>,
    atom_stride: usize,
    sep_stride: usize,
    names: HashSet<String>,
    atom_caps: Vec<RegexCaptures>,
    sep_caps: Vec<RegexCaptures>,
    nodes: u32,
}

impl SepChainWalk {
    /// Assemble this chain's capture delta, optionally with a `%%` trailing
    /// separator's captures folded in.
    fn assemble(&self, trailing: Option<&RegexCaptures>) -> RegexCaptures {
        super::regex_match_sep::separated_capture_delta(
            &self.names,
            &self.atom_caps,
            &self.sep_caps,
            trailing,
            self.atom_stride,
            self.sep_stride,
        )
    }

    /// The zero-iteration delta: the names still have to render as an EMPTY
    /// list when the quantifier matched nothing, not as a single empty Match,
    /// and the atom's and separator's positional slots are reserved as empty
    /// lists (`$/.list` is `([],)` for `(\d)* % ','` on "").
    fn names_delta(&self) -> RegexCaptures {
        self.assemble(None)
    }
}

impl Interpreter {
    /// Feed `on` this separated quantifier's candidates at `start`, HIGHEST
    /// PRIORITY FIRST, producing each only when the previous was rejected.
    /// Returns `true` when `on` asked the walk to stop.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn for_each_separated_candidate(
        &mut self,
        token: &RegexToken,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        pattern: &RegexPattern,
        store: &mut CapStore,
        on: &mut AtomCandidateCont<'_>,
    ) -> bool {
        if token.ratchet {
            // The ratcheted scan is possessive and yields at most one
            // candidate, so there is nothing to defer.
            let current_caps = store.caps().clone();
            let cands = self.match_separated_quantifier_ratchet(
                token,
                chars,
                start,
                pkg,
                pattern,
                &current_caps,
            );
            for (next, delta) in cands.into_iter().rev() {
                if on(self, store, next, delta) {
                    return true;
                }
            }
            return false;
        }
        let sep = token.separator.as_ref().expect("separator present");
        let current_caps = store.caps().clone();
        let Some((min, max)) = self.separated_quantifier_bounds(token, start, &current_caps) else {
            return false;
        };
        let sep_stride = super::regex_match_sep::separator_stride(&sep.pattern);
        let mut walk = SepChainWalk {
            min,
            max,
            atom_stride: count_capture_groups(token),
            sep_stride,
            names: Self::collect_quantified_names_for_token(token),
            atom_caps: Vec::new(),
            sep_caps: Vec::new(),
            nodes: 0,
        };
        if token.frugal && min == 0 {
            let delta = walk.names_delta();
            if on(self, store, start, delta) {
                return true;
            }
        }
        // First atom: every match length, highest-priority first. A frugal atom
        // matches as few chars as possible, but an outer anchor following the
        // quantifier may require it to expand, so the shorter/longer variants
        // all stay reachable — they are simply not *computed* until the
        // continuation has rejected the ones before them.
        let stopped = {
            let w = &mut walk;
            let capture_start = super::regex_match_sep_view::sep_iteration_slot(store.caps(), 0);
            let mut atom_store = CapStore::new(super::regex_match_sep_view::sep_chain_view(
                store.caps(),
                w.assemble(None),
            ));
            let _capture_scope =
                super::regex_helpers::InlineCaptureScope::enter(capture_start, w.atom_stride);
            let mut first = |interp: &mut Interpreter,
                             _atom_store: &mut CapStore,
                             end: usize,
                             caps: RegexCaptures| {
                super::regex_helpers::record_regex_farthest_position(end);
                w.atom_caps
                    .push(super::regex_match_sep::with_iteration_capture(
                        token, start, end, caps,
                    ));
                let stop = interp.sep_extend_chain(w, token, pattern, chars, end, pkg, store, on);
                w.atom_caps.pop();
                stop
            };
            self.for_each_atom_candidate(
                &token.atom,
                chars,
                start,
                &mut atom_store,
                pkg,
                pattern.ignore_case,
                false,
                &mut first,
            )
        };
        if stopped {
            return true;
        }
        // Zero iterations is the lowest-priority outcome for a greedy
        // quantifier, so it goes last.
        if !token.frugal && min == 0 {
            let delta = walk.names_delta();
            return on(self, store, start, delta);
        }
        false
    }

    /// Extend the chain at `cur` by trying every separator match (in priority
    /// order) followed by an atom, recursing greedily first, then reporting the
    /// chain that stops at `cur`. Deeper (longer) chains are reported before
    /// the shorter one, which is the greedy quantifier's priority order.
    #[allow(clippy::too_many_arguments)]
    fn sep_extend_chain(
        &mut self,
        walk: &mut SepChainWalk,
        token: &RegexToken,
        pattern: &RegexPattern,
        chars: &[char],
        cur: usize,
        pkg: Symbol,
        store: &mut CapStore,
        on: &mut AtomCandidateCont<'_>,
    ) -> bool {
        if walk.nodes > SEP_CHAIN_BUDGET {
            return false;
        }
        walk.nodes += 1;
        super::regex_helpers::record_regex_farthest_position(cur);
        if token.frugal && self.sep_emit_chain(walk, token, chars, cur, pkg, store, on) {
            return true;
        }
        let can_extend = walk.max.is_none_or(|m| walk.atom_caps.len() < m);
        if can_extend {
            let sep = token.separator.as_ref().unwrap();
            // The separator's own candidates stay eager: the set is what lets
            // the separator's lengths be tried in priority order, and a
            // separator carrying a side-effecting code block is not a shape
            // this ADR set out to fix.
            let sep_ends = self.sep_ends_seeing_chain(
                &sep.pattern,
                chars,
                cur,
                pkg,
                store.caps(),
                || walk.assemble(None),
                walk.atom_stride,
                false,
            );
            for (sep_end, scaps) in sep_ends {
                super::regex_helpers::record_regex_farthest_position(sep_end);
                // The separator is folded before the atom after it matches, so
                // code in that atom sees it (rakudo's `$/[1]` there lists every
                // separator so far).
                walk.sep_caps.push(scaps);
                let stopped = {
                    let w = &mut *walk;
                    let capture_start =
                        super::regex_match_sep_view::sep_iteration_slot(store.caps(), 0);
                    let mut atom_store = CapStore::new(
                        super::regex_match_sep_view::sep_chain_view(store.caps(), w.assemble(None)),
                    );
                    let _capture_scope = super::regex_helpers::InlineCaptureScope::enter(
                        capture_start,
                        w.atom_stride,
                    );
                    let mut next = |interp: &mut Interpreter,
                                    _atom_store: &mut CapStore,
                                    atom_end: usize,
                                    acaps: RegexCaptures| {
                        super::regex_helpers::record_regex_farthest_position(atom_end);
                        if atom_end <= cur {
                            return false;
                        }
                        w.atom_caps
                            .push(super::regex_match_sep::with_iteration_capture(
                                token, sep_end, atom_end, acaps,
                            ));
                        let stop = interp
                            .sep_extend_chain(w, token, pattern, chars, atom_end, pkg, store, on);
                        w.atom_caps.pop();
                        stop
                    };
                    self.for_each_atom_candidate(
                        &token.atom,
                        chars,
                        sep_end,
                        &mut atom_store,
                        pkg,
                        pattern.ignore_case,
                        false,
                        &mut next,
                    )
                };
                walk.sep_caps.pop();
                if stopped {
                    return true;
                }
            }
        }
        !token.frugal && self.sep_emit_chain(walk, token, chars, cur, pkg, store, on)
    }

    /// Report the chain that stops at `cur` as this quantifier's candidate(s):
    /// the `%%` trailing-separator variants first (Rakudo greedily consumes a
    /// trailing separator), then the plain end.
    #[allow(clippy::too_many_arguments)]
    fn sep_emit_chain(
        &mut self,
        walk: &mut SepChainWalk,
        token: &RegexToken,
        chars: &[char],
        cur: usize,
        pkg: Symbol,
        store: &mut CapStore,
        on: &mut AtomCandidateCont<'_>,
    ) -> bool {
        let count = walk.atom_caps.len();
        if count < walk.min {
            return false;
        }
        if let Some(m) = walk.max
            && count > m
        {
            return false;
        }
        let sep = token.separator.as_ref().unwrap();
        if sep.allow_trailing {
            let trailing = self.sep_ends_seeing_chain(
                &sep.pattern,
                chars,
                cur,
                pkg,
                store.caps(),
                || walk.assemble(None),
                walk.atom_stride,
                false,
            );
            for (ts_end, ts_caps) in trailing {
                if ts_end < cur {
                    continue;
                }
                let delta = walk.assemble(Some(&ts_caps));
                if on(self, store, ts_end, delta) {
                    return true;
                }
            }
        }
        let delta = walk.assemble(None);
        on(self, store, cur, delta)
    }
}
