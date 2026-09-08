//! ADR-0073 Slice 2 (streamed half): a `<subrule>` call's candidates are
//! produced on demand.
//!
//! The eager `Named` arm (`regex_match_atom.rs`) collects a subrule's whole end
//! set before the caller descends into any of it, because it carries the
//! left-recursion growing-seed loop, the proto rank-then-match dispatch
//! (ADR-0046) and three `Vec`-returning escape hatches. This module takes the
//! one shape where none of that is in play and drives the subrule's body
//! through a `MatchSink::Cont` instead, so an embedded `{ ... }` block inside it
//! runs once per end the cursor ENTERS rather than once per end the engine
//! COMPUTES.
//!
//! The precondition that makes it sound is `regex_call_graph`'s reachability
//! analysis: with a call to this rule's own name proven unreachable, the
//! growing-seed loop is a formality and there is nothing for the stream to get
//! wrong.

use super::super::*;
use super::regex_match_core::MatchSink;
use super::regex_match_lazy::AtomCandidateCont;
use super::regex_trail::CapStore;

impl Interpreter {
    /// Drive a `<subrule>` call's walk, wrapping each of its ends into this
    /// atom's capture delta as it is produced (ADR-0073 Slice 2's second half).
    ///
    /// `Some(stop)` when the call was streamed; `None` when it is not eligible
    /// and the caller must fall back to the eager producer.
    ///
    /// The eager `Named` arm (`regex_match_atom.rs`) cannot stream because it
    /// carries the left-recursion growing-seed loop, the proto rank-then-match
    /// dispatch and three `Vec`-returning escape hatches. This path takes only
    /// the shape where none of them is in play — an argument-less call with
    /// exactly one non-proto candidate, no custom-HOW dispatch, no `:m`, no
    /// dynamic (`$*`) rule parameters anywhere in the program, and a key that
    /// is not already active — and, crucially, only when the call graph proves
    /// the rule cannot reach a call to its own name
    /// (`regex_call_graph::subrule_call_is_streamable`). With re-entry
    /// impossible, the growing-seed loop is a formality: it runs one iteration,
    /// finds the seed unconsulted, and returns the first result.
    ///
    /// The activation is still registered, because an embedded `{ ... }` block
    /// is user code the call graph does not model: if it re-enters this key by
    /// hand, it reads the empty seed and fails instead of recursing forever.
    /// When that happens and nothing has committed to a candidate yet, the call
    /// falls back to the eager growing-seed path — the block runs again, which
    /// is the same over-firing this slice removes elsewhere, but the match is
    /// the one the seed loop would have produced.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn drive_named_subrule_candidates(
        &mut self,
        name: &str,
        chars: &[char],
        pos: usize,
        store: &mut CapStore,
        pkg: &str,
        ratchet: bool,
        on: &mut AtomCandidateCont<'_>,
    ) -> Option<bool> {
        // A `$*`-twigil rule parameter has to be installed around the call and
        // torn down after it; streaming would keep it installed across the
        // continuation, which is a different dynamic scope. The flag is global
        // and almost always false, so this costs nothing in practice.
        if crate::runtime::regex::regex_dynparams::ANY_DYNAMIC_TOKEN_PARAM
            .load(std::sync::atomic::Ordering::Relaxed)
        {
            return None;
        }
        if !self.registry().grammar_custom_how.is_empty() {
            return None;
        }
        // Memoized per (package, atom text), and asked FIRST: the rule resolves
        // to exactly one plain argument-less candidate AND the call graph proves
        // it cannot reach a call to its own name. A call that fails this costs
        // two borrowed hash lookups — no name parse, no resolution the eager arm
        // would then repeat.
        if !self.subrule_call_is_streamable(name, pkg) {
            return None;
        }
        let spec = Self::parse_named_regex_lookup_spec(name);
        let lr_key = (spec.lookup_name.clone(), chars.len() - pos);
        if super::regex_match_atom::lr_key_is_active(&lr_key) {
            return None;
        }
        // Same resolution the eager arm performs (memoized for a static body,
        // per-call otherwise). The shape was settled above, but a body that is
        // re-parsed per call is re-checked rather than assumed.
        let (candidates, _) = self.parsed_subrule_candidates(&spec, pkg, &[]);
        let [(parsed, sub_pkg, sym_key)] = &candidates[..] else {
            return None;
        };
        if sym_key.is_some() || parsed.ignore_mark {
            return None;
        }
        let parsed = std::sync::Arc::clone(parsed);
        let sub_pkg = sub_pkg.clone();

        let outer_seed_read = super::regex_match_atom::lr_begin_activation(&lr_key);
        // Ends are deduplicated the way the eager arm does it: the first (=
        // highest-priority) path to reach an end wins, later ones are dropped.
        let mut seen_ends: Vec<usize> = Vec::new();
        let mut unwind = false;
        {
            let unwind = &mut unwind;
            let mut cont = |interp: &mut Interpreter, end: usize, inner: RegexCaptures| -> bool {
                if seen_ends.contains(&end) {
                    return false;
                }
                seen_ends.push(end);
                let wrapped = Interpreter::build_named_candidates_from_inner(
                    vec![(end, inner)],
                    pos,
                    &spec,
                    None,
                );
                let Some((end, delta)) = wrapped.into_iter().next() else {
                    return false;
                };
                if on(interp, store, end, delta) {
                    *unwind = true;
                    return true;
                }
                // Ratchet (`:`, and every `token`/`rule` body) commits to the
                // subrule's highest-priority end and forbids backtracking into
                // it, so there is no second candidate to compute.
                ratchet
            };
            self.regex_walk_ends_in_pkg(
                &parsed,
                chars,
                pos,
                &sub_pkg,
                false,
                false,
                &mut MatchSink::Cont(&mut cont),
            );
        }
        let seed_consulted = super::regex_match_atom::lr_end_activation(&lr_key, outer_seed_read);
        if seed_consulted && !unwind {
            // A `{ ... }` block re-entered this key after all, so the single
            // pass above is not the growing-seed loop's answer. Nothing has
            // committed (the continuation rejected every streamed end), so hand
            // the call back to the eager arm.
            return None;
        }
        Some(unwind)
    }
}
