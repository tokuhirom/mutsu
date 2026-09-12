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
use super::regex_call_graph::StreamDecline;
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
            return decline(StreamDecline::DynamicRuleParam);
        }
        if !self.registry().grammar_custom_how.is_empty() {
            return decline(StreamDecline::CustomHow);
        }
        // Memoized per (package, atom text), and asked FIRST: the rule resolves
        // to exactly one plain argument-less candidate AND the call graph proves
        // it cannot reach a call to its own name. A call that fails this costs
        // two borrowed hash lookups — no name parse, no resolution the eager arm
        // would then repeat.
        if let Some(reason) = self.subrule_call_stream_decline(name, pkg) {
            return decline(reason);
        }
        let spec = Self::parse_named_regex_lookup_spec(name);
        let lr_key = super::regex_lr_state::LrKey::new(spec.lookup_sym, None, chars.len() - pos);
        if super::regex_lr_state::lr_key_is_active(&lr_key) {
            return decline(StreamDecline::LrKeyActive);
        }
        // Same resolution the eager arm performs (memoized for a static body,
        // per-call otherwise). The shape was settled above, but a body that is
        // re-parsed per call is re-checked rather than assumed.
        let (candidates, _) = self.parsed_subrule_candidates(&spec, pkg, &[]);
        let [(parsed, sub_pkg, sym_key)] = &candidates[..] else {
            return decline(StreamDecline::SeveralCandidates);
        };
        if sym_key.is_some() {
            return decline(StreamDecline::Proto);
        }
        if parsed.ignore_mark {
            return decline(StreamDecline::IgnoreMark);
        }
        let parsed = std::sync::Arc::clone(parsed);
        let sub_pkg = sub_pkg.clone();

        let outer_seed_read = super::regex_lr_state::lr_begin_activation(&lr_key);
        // Ends are deduplicated the way the eager arm does it: the first (=
        // highest-priority) path to reach an end wins, later ones are dropped.
        let mut seen_ends: Vec<usize> = Vec::new();
        let mut unwind = false;
        let mut seed_consulted_in_cont = false;
        {
            let unwind = &mut unwind;
            let seed_consulted_in_cont = &mut seed_consulted_in_cont;
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
                // The continuation is the CALLER's remaining pattern, not this
                // rule's body, so this activation must not be visible while it
                // runs. A *sibling* call to the same rule at the same position
                // is ordinary, not left recursion — and it is exactly what a
                // `rule` with a bracketed group compiles to, since sigspace puts
                // a `<.ws>` both at the end of the group and right after it
                // (`'[' <.ws> [ <id> <.ws> ] <.ws> ']'`). Leaving the activation
                // up made that second `<.ws>` read this one's empty seed and
                // fail, so every such rule stopped matching as soon as the
                // grammar defined its own `ws` (CSS::Grammar does). Lift it
                // across the continuation and restore it for the rest of the
                // body walk; the code-block re-entry the activation exists to
                // catch happens inside the body, which is still covered.
                *seed_consulted_in_cont |=
                    super::regex_lr_state::lr_end_activation(&lr_key, outer_seed_read);
                let stop = on(interp, store, end, delta);
                super::regex_lr_state::lr_begin_activation(&lr_key);
                if stop {
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
        let seed_consulted = super::regex_lr_state::lr_end_activation(&lr_key, outer_seed_read)
            || seed_consulted_in_cont;
        if seed_consulted && !unwind {
            // A `{ ... }` block re-entered this key after all, so the single
            // pass above is not the growing-seed loop's answer. Nothing has
            // committed (the continuation rejected every streamed end), so hand
            // the call back to the eager arm.
            return decline(StreamDecline::SeedConsulted);
        }
        crate::vm::vm_stats::record_subrule_stream("streamed");
        Some(unwind)
    }
}

/// Count one declined call in the `MUTSU_VM_STATS` histogram and answer `None`,
/// so every `return` in the eligibility cascade above reads as one expression.
///
/// The histogram is the measurement #7548 asks for before any of its six
/// residues is opened: the streamed path is correct today, and what a residue
/// buys is only fewer `{ ... }` block runs on paths raku never enters, so the
/// per-call counts are what say which residue is worth its machinery.
#[inline]
fn decline(reason: StreamDecline) -> Option<bool> {
    crate::vm::vm_stats::record_subrule_stream(reason.as_str());
    None
}
