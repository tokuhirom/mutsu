//! The growing-seed evaluation of a `<subrule>` call (left recursion).
//!
//! A call is evaluated under a left-recursion key `(rule, arguments, chars
//! remaining)`. A re-entry of a live key answers from the key's seed; the
//! owner evaluates the rule's candidates, and while some re-entry read the
//! seed it grows the seed and evaluates again, until the longest end stops
//! growing. The candidates are evaluated through the all-ends entry
//! (`regex_match_ends_from_caps_in_pkg`), which runs their compiled
//! programs. Shared by the walk's eager `Named` arm and the compiled engine's
//! call (ADR-0135 Slice E, `rx_call`), so the loop has one implementation.

use super::super::*;
use super::regex_lr_state::{LrKey, lr_end_activation, lr_seed_was_consulted, lr_store_seed};
use super::regex_token_candidates::TokenCandidates;
use crate::runtime::regex::regex_helpers::NamedRegexLookupSpec;

#[derive(Clone, Copy)]
struct SubruleMatchOptions {
    first_only: bool,
    ignore_case: bool,
}

impl Interpreter {
    /// Every end of the call `spec` at `pos`, over its resolved `candidates`,
    /// as candidates wrapped for the caller (`build_named_candidates_from_inner`),
    /// LOWEST PRIORITY FIRST. `options` is `(first_only, ignore_case)`:
    /// `first_only` asks for the highest-priority end only (a ratcheted
    /// caller), honored when no re-entry can need the full set.
    // Cost: O(i·c) for i growing iterations (1 when the rule is not
    // left-recursive at `pos`) of c = the candidates' evaluation, plus O(e)
    // to wrap e ends.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn subrule_seed_ends(
        &mut self,
        spec: &NamedRegexLookupSpec,
        candidates: &TokenCandidates,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
        arg_values: &[Value],
        custom_how: bool,
        options: (bool, bool),
    ) -> Vec<(usize, RegexCaptures)> {
        let (subrule_first_only, ignore_case) = options;
        // The subrule body is matched against the WHOLE subject starting
        // at `pos` (ADR-0016 P1), not against a `&chars[pos..]` re-slice.
        // Every offset it produces is therefore already absolute, so
        // nothing has to be rebased afterwards — the old re-slice forced
        // a deep copy of the entire descendant capture subtree at every
        // nesting level — and look-behind/`<<`/`^^`/`<at(N)>` see the real
        // text before the subrule instead of a slice boundary.

        // Left-recursion detection using (name+args, remaining_chars_count)
        // as key. remaining = chars.len() - pos.
        // When rule r calls <&r> recursively at the same position, both calls
        // will have the same remaining count, allowing us to detect and break
        // the left-recursion cycle. The argument values are part of the rule
        // identity: `multi rule expr($p)` calling `<expr($p-1)>` at the same
        // position is ordinary recursion toward a base case, NOT left
        // recursion (99problems-41-to-50.t P47).
        let lr_args = (!arg_values.is_empty()).then(|| {
            let mut n = String::new();
            for v in arg_values {
                n.push('\u{0}');
                n.push_str(&Self::format_named_regex_arg_value(v));
            }
            n.into_boxed_str()
        });
        // ... unless nothing can re-enter the key at all. The rule call
        // graph decides that (`subrule_needs_no_lr_bookkeeping`), and
        // when it does the whole activation — create the entry, read
        // `false` out of it, remove it again — is provably a no-op, so
        // the key is never built and the three map operations never
        // happen. An argument-bearing call keeps the bookkeeping: its
        // arguments are part of the key, and they are runtime data the
        // walk does not model.
        let lr_key = (!arg_values.is_empty()
            || custom_how
            || !self.subrule_needs_no_lr_bookkeeping(spec.lookup_sym, pkg))
        .then(|| LrKey::new(spec.lookup_sym, lr_args, chars.len() - pos));

        // Is this call already active (left recursion), and if not,
        // start its activation — one map operation for both, since
        // exactly one of the two answers is acted on.
        //
        // Active: genuine left recursion. This key's evaluation depends
        // on its own seed, so the owner must keep growing it; asking for
        // the seed is what records that. The seed is stored in HIGHEST
        // FIRST order (raw inner matches, absolute positions).
        //
        // Not active: run the growing-seed algorithm from an empty seed
        // (= no match yet). This key starts out un-consulted for THIS
        // activation; a stale entry from an earlier activation at the
        // same key must not be read as "left-recursive" here.
        let mut outer_seed_read = false;
        if let Some(lr_key) = &lr_key {
            match super::regex_lr_state::lr_begin_or_reenter(lr_key) {
                super::regex_lr_state::LrBegin::Reentry(seed) => {
                    // Wrap seed into outer captures.
                    // build_named_candidates_from_inner returns items in
                    // the same order as input (HIGHEST FIRST). Caller
                    // expects LOWEST FIRST, so reverse.
                    let mut result = self.build_named_candidates_from_inner(
                        seed, pos, spec, None, // no sym_key for seed
                    );
                    result.reverse();
                    return result;
                }
                super::regex_lr_state::LrBegin::Began(outer) => outer_seed_read = outer,
            }
        }

        // best_inner_max: max inner_end seen so far (None = nothing matched yet).
        let mut best_inner_max: Option<usize> = None;

        // best_raw: raw inner matches for the best iteration, HIGHEST FIRST.
        let mut best_raw: Vec<(usize, RegexCaptures)> = Vec::new();

        let has_proto = candidates.iter().any(|(_, _, sym)| sym.is_some());
        // Left-recursion escape hatch for the rank-then-match path — see
        // the `seed_was_consulted` handling below.
        let mut lr_match_all = false;
        // ADR-0073 Slice 2: a ratcheted caller cannot backtrack into
        // this subrule, so only its highest-priority end can ever be
        // used and its body may be walked with `first_only` — which is
        // what stops an embedded `{ … }` block from firing once per end
        // the engine merely computed. The guard keeps the growing-seed
        // loop sound: a body that cannot invoke a named rule cannot
        // re-enter this key, so no cut-short walk can hide a
        // left-recursive re-entry (`regex_subrule_lazy`).
        // A body that does call rules is just as safe when none of
        // those calls can come back to this rule at this position
        // (`regex_left_call_graph`, #9579): the seed is then never
        // consulted by a rule call, whether or not the walk stops early.
        let mut first_only = subrule_first_only
            && (candidates.iter().all(|(parsed, _, _)| {
                super::regex_subrule_lazy::pattern_is_rule_call_free(parsed)
            }) || (arg_values.is_empty()
                && !custom_how
                && self.subrule_cannot_left_reenter(spec.lookup_sym, pkg)));

        loop {
            // Evaluate all candidates' patterns directly (unwrapped).
            let mut raw_out: Vec<(usize, RegexCaptures)> = Vec::new();

            if has_proto && !lr_match_all {
                // ADR-0046 Decision 1: rank the proto candidates by
                // MEASUREMENT, then match only the winner. Ranking runs
                // each candidate's NFA, so it executes nothing
                // (ADR-0009, ADR-0125) — which is what keeps a losing candidate's
                // `{ … }` blocks and action methods from firing (ADR-0046
                // §2.3). This is the same `(prefix_len, litlen, decl
                // order)` triple `|` alternation and the `:rule<...>`
                // proto entry point rank by; declaration order comes free
                // from a stable sort over `candidates`, which is already
                // in declaration order.
                let (mut keys, mut ranked) = (Vec::new(), Vec::new());
                self.ltm_rank_proto(candidates, chars, pos, &mut keys, &mut ranked);
                // Attempt the ranked candidates in order and stop at the
                // first that actually matches — Rakudo tries the NFA's
                // fates in order and commits to the first that succeeds,
                // without backtracking into a later fate when what FOLLOWS
                // the subrule call fails (verified against `raku`).
                for idx in ranked {
                    let (parsed, sub_pkg, sym_key) = &candidates[idx];
                    let sym_key = sym_key.clone();
                    let all_matches = self.subrule_candidate_ends_with_frame(
                        &spec.lookup_name,
                        parsed,
                        chars,
                        pos,
                        (*sub_pkg, pkg),
                        SubruleMatchOptions {
                            first_only,
                            ignore_case,
                        },
                    );
                    if all_matches.is_empty() {
                        continue;
                    }
                    // A proto candidate contributes only its greedy end
                    // (see ADR-0046 §4's "residual not closed" note).
                    let matches_to_use: Vec<_> = if sym_key.is_some() {
                        all_matches.into_iter().take(1).collect()
                    } else {
                        all_matches
                    };
                    for (end, mut caps) in matches_to_use {
                        if sym_key.is_some() {
                            caps.set_sym(sym_key.as_deref().map(Symbol::intern));
                        }
                        raw_out.push((end, caps));
                    }
                    break;
                }
            } else {
                for (parsed, sub_pkg, sym_key) in candidates.iter() {
                    let all_matches = self.subrule_candidate_ends_with_frame(
                        &spec.lookup_name,
                        parsed,
                        chars,
                        pos,
                        (*sub_pkg, pkg),
                        SubruleMatchOptions {
                            first_only,
                            ignore_case,
                        },
                    );
                    // all_matches: HIGHEST FIRST.
                    let matches_to_use: Vec<_> = if sym_key.is_some() {
                        all_matches.into_iter().take(1).collect()
                    } else {
                        all_matches
                    };
                    // Preserve sym_key in each match so build_named_candidates_from_inner
                    // can set subcap.sym correctly for action method dispatch.
                    for (end, mut caps) in matches_to_use {
                        if sym_key.is_some() {
                            caps.set_sym(sym_key.as_deref().map(Symbol::intern));
                        }
                        raw_out.push((end, caps));
                    }
                }
            }

            // Sort/dedup into HIGHEST FIRST order.
            let deduped_raw: Vec<(usize, RegexCaptures)> = if has_proto {
                // LTM: stable-sort by end ascending, dedup, then reverse
                // → HIGHEST (longest) FIRST. On an equal-length tie,
                // Rakudo's LTM breaks the tie by candidate declaration
                // order — the FIRST-declared candidate wins. `raw_out`
                // preserves declaration order (candidates.iter()) and
                // the sort is stable, so on a tie the first-declared
                // candidate appears first among equal ends; keep it and
                // skip the rest (do NOT pop/replace with the later one).
                raw_out.sort_by_key(|(e, _)| *e);
                let mut tmp: Vec<(usize, RegexCaptures)> = Vec::new();
                for item in raw_out {
                    if tmp.last().is_some_and(|(e, _)| *e == item.0) {
                        continue;
                    }
                    tmp.push(item);
                }
                tmp.reverse(); // HIGHEST (longest) FIRST
                tmp
            } else {
                // Non-LTM: raw_out is already HIGHEST FIRST (from
                // regex_match_ends_from_caps_in_pkg). Every path is
                // kept, a repeated end included: Rakudo runs the
                // caller's continuation once per path (#10489).
                raw_out
            };

            let new_max: Option<usize> = deduped_raw.iter().map(|(e, _)| *e).max();

            // Nothing re-entered this key, so the evaluation never read
            // the seed and cannot change if the seed grows: this rule is
            // not left-recursive at this position and the first result is
            // already final. Re-running the candidates would recompute the
            // identical set — and since every nested subrule did the same,
            // that redundant second pass compounded to 2^depth over a
            // precedence-climbing grammar (99problems-41-to-50.t P47).
            let seed_was_consulted = lr_key.as_ref().is_some_and(lr_seed_was_consulted);
            if !seed_was_consulted {
                best_raw = deduped_raw;
                break;
            }

            // The rule-call-free guard says this cannot happen — but a
            // `{ … }` block is user code and could re-enter the rule by
            // hand. If it did, the first-only walk's single end is not a
            // sound basis for the growing-seed loop's max-end growth
            // test, so redo the iteration with the full candidate set.
            if first_only {
                first_only = false;
                continue;
            }

            // Left-recursive at this key. ADR-0046 Slice 4: the
            // growing-seed loop discovers re-entry by *evaluating*
            // candidates, so a candidate the rank-then-match path skipped
            // could hide a left-recursive re-entry and make the seed stop
            // growing early. Fall back to evaluating the full candidate
            // set for as long as this activation lives, and redo the
            // current iteration under that rule before judging growth.
            if !lr_match_all {
                lr_match_all = true;
                continue;
            }

            if new_max > best_inner_max {
                // Seed grew: store the raw matches (HIGHEST FIRST) as the seed.
                best_inner_max = new_max;
                best_raw = deduped_raw.clone();
                if let Some(lr_key) = &lr_key {
                    lr_store_seed(lr_key, deduped_raw);
                }
            } else {
                // No growth: done.
                break;
            }
        }

        // Clean up this activation, restoring the enclosing one's
        // consulted flag: an inner activation of the same key must not
        // mask the outer one's.
        if let Some(lr_key) = &lr_key {
            lr_end_activation(lr_key, outer_seed_read);
        }

        // Wrap best_raw into outer captures and return.
        // best_raw is HIGHEST FIRST; build_named_candidates_from_inner returns in
        // the same order (one-to-one), so result is HIGHEST FIRST.
        // Caller expects LOWEST FIRST, so reverse.
        let mut result = self.build_named_candidates_from_inner(best_raw, pos, spec, None);
        result.reverse();
        result
    }

    /// One subrule candidate's end positions, HIGHEST PRIORITY FIRST.
    ///
    /// `first_only` stops the body's walk at its highest-priority complete
    /// match (ADR-0073 Slice 2). It is only ever set when the calling token is
    /// ratcheted, i.e. when no later end could be reached by backtracking
    /// anyway: the eager path's dedup keeps the first end per position and the
    /// atom driver then drains everything but the highest-priority candidate,
    /// so the surviving candidate is the same one either way — the difference
    /// is that the discarded ends are no longer *computed*, and the `{ … }`
    /// blocks inside them no longer run.
    fn subrule_candidate_ends(
        &mut self,
        parsed: &RegexPattern,
        chars: &[char],
        pos: usize,
        sub_pkg: Symbol,
        first_only: bool,
        ignore_case: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        // An inline `:i<subrule>` scopes the modifier over the subrule body,
        // not just over the named-call atom.  The parsed body normally carries
        // its own modifier state, so add the inherited flag at this boundary
        // before walking it.  Keep the original pattern when no inheritance is
        // needed; this is the hot path for ordinary named calls.
        let scoped = if ignore_case && !parsed.ignore_case {
            Some(RegexPattern {
                tokens: parsed.tokens.clone(),
                anchor_start: parsed.anchor_start,
                anchor_end: parsed.anchor_end,
                ignore_case: true,
                ignore_mark: parsed.ignore_mark,
                derived: Default::default(),
            })
        } else {
            None
        };
        let parsed = scoped.as_ref().map_or(parsed, |pattern| pattern);
        // One rule invocation: a grammar method its body calls writes to the
        // invocation's own cursor, which is filed on each end it produces (#9803).
        self.enter_rule_cursor();
        let mut ends = if first_only {
            self.regex_match_end_from_caps_in_pkg(parsed, chars, pos, sub_pkg)
                .into_iter()
                .collect()
        } else {
            self.regex_match_ends_from_caps_in_pkg(parsed, chars, pos, sub_pkg)
        };
        let cursor = self.leave_rule_cursor();
        Self::file_rule_cursor(cursor, &mut ends);
        ends
    }

    /// Keep named grammar-rule frames visible while a rule's pattern is
    /// evaluated. Token/rule bodies are executed by the regex engine rather
    /// than ordinary method dispatch, but `Backtrace` still needs to see the
    /// enclosing rule when a wrapped token records its caller.
    fn subrule_candidate_ends_with_frame(
        &mut self,
        rule_name: &str,
        parsed: &RegexPattern,
        chars: &[char],
        pos: usize,
        packages: (Symbol, Symbol),
        options: SubruleMatchOptions,
    ) -> Vec<(usize, RegexCaptures)> {
        let (sub_pkg, frame_pkg) = packages;
        // The frame is only read by a wrapped token recording its caller, and
        // pushing it costs a routine-stack frame per subrule call (~5% of
        // bench-grammar-parse-big). With no method wrap installed there is no
        // reader, so match without it.
        // TODO: a Backtrace taken from a code block inside a rule should also
        // see this frame (as in Rakudo); make the frame cheap enough to push
        // unconditionally instead of gating it on the wrap table.
        if !self.has_any_wrap_chains() {
            return self.subrule_candidate_ends(
                parsed,
                chars,
                pos,
                sub_pkg,
                options.first_only,
                options.ignore_case,
            );
        }
        self.push_routine_with_location(
            frame_pkg,
            Symbol::intern(rule_name),
            self.current_source_line(),
            self.executing_source_file_sym(),
            None,
        );
        let result = self.subrule_candidate_ends(
            parsed,
            chars,
            pos,
            sub_pkg,
            options.first_only,
            options.ignore_case,
        );
        self.routine_stack.pop();
        result
    }
}
