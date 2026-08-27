use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use super::super::*;
use super::regex_helpers::{
    LTM_DECLARATIVE_MODE, LTM_PREFIX_TERMINATED, NamedRegexLookupSpec, alternation_capture_slots,
    merge_regex_captures,
};
use super::regex_ltm_rank::{LtmAtomMode, ltm_atom_mode};

thread_local! {
    /// Memoization cache for left-recursive named regex calls.
    /// Key: (rule_name, remaining_chars_count) where remaining = chars.len() - pos.
    /// This uniquely identifies "matching rule at this position in the string"
    /// regardless of how deeply the chars slice has been sliced.
    /// Value: current seed matches (in HIGHEST PRIORITY FIRST order, as returned by
    /// regex_match_ends_from_caps_in_pkg). Empty vec means "no match yet" (initial seed).
    #[allow(clippy::type_complexity)]
    static LR_MEMO: RefCell<HashMap<(String, usize), Vec<(usize, RegexCaptures)>>>
        = RefCell::new(HashMap::new());

    /// Set of (rule_name, remaining_chars_count) pairs currently being evaluated.
    /// When a recursive call sees its key here, it returns the current seed.
    static LR_ACTIVE: RefCell<HashMap<(String, usize), ()>>
        = RefCell::new(HashMap::new());

    /// Keys whose seed was actually CONSULTED (read by a recursive re-entry)
    /// while they were active — i.e. the keys that are genuinely
    /// left-recursive at this position. Only those need the seed-growing
    /// loop's second iteration; see the `seed_was_consulted` check below.
    static LR_SEED_READ: RefCell<HashSet<(String, usize)>>
        = RefCell::new(HashSet::new());
}

/// ADR-0022 §4.4(a): one `|` branch's rank key (prefix_len, litlen) paired
/// with its PLURAL ends (highest-priority-first).
type RankedAlternationBranch = ((usize, usize), Vec<(usize, RegexCaptures)>);

impl Interpreter {
    /// An alternation alternative that is a lone plain `{ … }` code block
    /// (`|| { die "no match" }`). Such a branch matches zero-width and exists
    /// for its side effects, so evaluating it eagerly during candidate
    /// collection fires those effects on paths raku never executes — it must
    /// only run when no other alternative matched.
    fn is_pure_code_block_alt(alt: &RegexPattern) -> bool {
        alt.tokens.len() == 1
            && matches!(alt.tokens[0].quant, RegexQuant::One)
            && alt.tokens[0].separator.is_none()
            && matches!(
                &alt.tokens[0].atom,
                RegexAtom::CodeAssertion {
                    is_assertion: false,
                    ..
                }
            )
    }

    /// Try to match `branch` starting at `pos` such that it ends exactly at
    /// `target_end`. Returns the branch's own captures (relative to an empty
    /// baseline) on success. Used by conjunction (`&` / `&&`) matching, where
    /// every branch must cover the same substring.
    fn regex_match_branch_ending_at(
        &mut self,
        branch: &RegexPattern,
        chars: &[char],
        pos: usize,
        target_end: usize,
        pkg: &str,
    ) -> Option<RegexCaptures> {
        for (end, caps) in self.regex_match_ends_from_caps_in_pkg(branch, chars, pos, pkg) {
            if end == target_end {
                return Some(caps);
            }
        }
        None
    }

    /// ADR-0022 §4.4(a) helper: rank each of `alts` by
    /// [`Self::ltm_branch_rank_key`] and collect its PLURAL ends (highest-
    /// priority-first, same convention as `regex_match_ends_from_caps_in_pkg`).
    /// A branch whose real ends are empty (declaratively promising but not
    /// an actual match here) contributes nothing and is dropped — the
    /// rank-only "sound filter" from ADR-0022 §4.1 is subsumed by this
    /// stronger, always-correct check. Caller sorts the returned vec by rank
    /// key descending (stable, so ties keep `alts`' original order) and
    /// flattens branch-major, worst-to-best, each branch's own ends
    /// worst-to-best, to build this arm's lowest-priority-first output.
    fn ltm_rank_and_collect_branches<'a>(
        &mut self,
        alts: impl Iterator<Item = &'a RegexPattern>,
        capture_slots: usize,
        chars: &[char],
        pos: usize,
        pkg: &str,
    ) -> Vec<RankedAlternationBranch> {
        let mut out = Vec::new();
        for alt in alts {
            let raw_ends = self.regex_match_ends_from_caps_in_pkg(alt, chars, pos, pkg);
            if raw_ends.is_empty() {
                continue;
            }
            let ends: Vec<(usize, RegexCaptures)> = raw_ends
                .into_iter()
                .map(|(end, mut inner_caps)| {
                    if !super::regex_helpers::IN_QUANTIFIED_ALTERNATION_MATCH.with(Cell::get) {
                        inner_caps
                            .positional
                            .resize(capture_slots, PosSlot::alternation_padding());
                    }
                    let mut new_caps = RegexCaptures::default();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().merge(v);
                    }
                    new_caps.positional.append(&mut inner_caps.positional);
                    new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                    (end, new_caps)
                })
                .collect();
            let rank = self.ltm_branch_rank_key(alt, chars, pos, pkg);
            out.push((rank, ends));
        }
        out
    }

    /// Matches `atom`, with any dynamically-scoped (`$*`) parameters a subrule
    /// atom declares established for the duration of the call and torn down
    /// afterwards — see `regex_dynparams`. The inner function reports what it
    /// bound through `dyn_saved` (it can only know once the subrule's arguments
    /// are evaluated) and returns from a dozen places, so the teardown lives
    /// here rather than at each of them.
    pub(super) fn regex_match_atom_all_with_capture_in_pkg(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: &str,
        ignore_case: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        let mut dyn_saved = None;
        let out = self.regex_match_atom_all_with_capture_in_pkg_inner(
            atom,
            chars,
            pos,
            current_caps,
            pkg,
            ignore_case,
            &mut dyn_saved,
        );
        if let Some(saved) = dyn_saved {
            self.restore_subrule_dynamic_params(saved);
        }
        out
    }

    #[allow(clippy::too_many_arguments)]
    fn regex_match_atom_all_with_capture_in_pkg_inner(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: &str,
        ignore_case: bool,
        dyn_saved: &mut Option<super::regex_dynparams::SavedDynParams>,
    ) -> Vec<(usize, RegexCaptures)> {
        // Return value convention: LOWEST PRIORITY FIRST, HIGHEST PRIORITY LAST
        // (the engine iterates the vec in reverse, trying the highest-priority
        // candidate first).
        //
        // Each candidate's captures are a DELTA relative to an EMPTY baseline
        // (ADR-0007): the engine merges the chosen delta into its capture
        // store and rewinds it on backtrack. `current_caps` is the engine's
        // accumulated store, passed for READS only (backrefs, code assertions,
        // subrule argument evaluation) — it must never be cloned into results.
        let _vars_seed = Self::arm_inline_vars_seed(atom, current_caps);

        // ADR-0022 §4.2: in LTM declarative-prefix measurement mode, a
        // non-declarative atom either terminates the prefix at its own
        // position (zero-width) or — for a positive lookahead — inlines its
        // inner pattern's consumption first, then terminates. `SequentialAlternation`
        // is intentionally not covered by `ltm_atom_mode` (it needs its own
        // ε-bypass measurement below) and `CodeAssertion` keeps its existing
        // inline handling (ADR-0009), so both fall through to `LtmAtomMode::Normal`
        // here and are unaffected by this guard.
        if LTM_DECLARATIVE_MODE.with(std::cell::Cell::get) {
            match ltm_atom_mode(atom) {
                LtmAtomMode::Terminate => {
                    LTM_PREFIX_TERMINATED.with(|f| f.set(true));
                    return vec![(pos, RegexCaptures::default())];
                }
                LtmAtomMode::TerminateAfter(inner) => {
                    // Measure the inner pattern BEFORE setting TERMINATED: the
                    // inner walk checks the flag at its own entry, so setting
                    // it first would short-circuit the inner measurement to
                    // zero-width instead of letting it consume.
                    let best_end = self
                        .regex_match_ends_from_caps_in_pkg(inner, chars, pos, pkg)
                        .into_iter()
                        .map(|(end, _)| end)
                        .max()
                        .unwrap_or(pos);
                    LTM_PREFIX_TERMINATED.with(|f| f.set(true));
                    return vec![(best_end, RegexCaptures::default())];
                }
                LtmAtomMode::SkipZeroWidth => {
                    return vec![(pos, RegexCaptures::default())];
                }
                LtmAtomMode::Normal => {}
            }
        }

        if let RegexAtom::Alternation(alternatives) = atom {
            // ADR-0022 §4.4(a): rank branches by (prefix_len desc, litlen
            // desc), ties broken by declaration order — free via a stable
            // sort over the branches in their original written order, so no
            // index needs to travel with the rank key. Collects PLURAL ends
            // per branch (fixes ADR-0022 gap #4: backtracking into shorter
            // ends of the chosen branch before falling to the next-ranked
            // branch — `[ a+ | q ] ab` on "aaab" needs `a+`'s shorter ends
            // available once `ab` fails against its greedy longest end).
            //
            // A side-effect-only alternative (`| { die ... }` — a lone plain
            // code block) is deferred: it matches zero-width, so it can only
            // win when NOTHING else matched, and running it eagerly would fire
            // its side effects (a `die`!) on paths raku never executes.
            let capture_slots = alternation_capture_slots(alternatives);
            let mut branches = self.ltm_rank_and_collect_branches(
                alternatives
                    .iter()
                    .filter(|alt| !Self::is_pure_code_block_alt(alt)),
                capture_slots,
                chars,
                pos,
                pkg,
            );
            if branches.is_empty() {
                branches = self.ltm_rank_and_collect_branches(
                    alternatives
                        .iter()
                        .filter(|alt| Self::is_pure_code_block_alt(alt)),
                    capture_slots,
                    chars,
                    pos,
                    pkg,
                );
            }
            branches.sort_by_key(|b| std::cmp::Reverse(b.0));
            let mut out = Vec::new();
            for (_, ends) in branches.into_iter().rev() {
                out.extend(ends.into_iter().rev());
            }
            return out;
        }
        if let RegexAtom::SequentialAlternation(alternatives) = atom {
            if LTM_DECLARATIVE_MODE.with(std::cell::Cell::get) {
                // ADR-0022 §4.2: only the first branch of `X || Y ...`
                // participates in the declarative prefix, plus a zero-width
                // epsilon bypass — see `ltm_seqalt_candidates`.
                return self.ltm_seqalt_candidates(alternatives, chars, pos, pkg);
            }
            // || (sequential alternation): alt0 has higher priority than alt1, etc.
            // All alternatives are included to allow outer-context backtracking,
            // but in priority order: alt0's matches have highest priority.
            //
            // We collect ALL matches from each alternative (using the plural form
            // regex_match_ends_from_caps_in_pkg) to enable backtracking through
            // recursive patterns (e.g. r = <?> || x <r> must expose all lengths
            // of r-matches for the outer $ anchor to find the right one).
            //
            // Return convention: lowest priority first. Order:
            //   [alt_N matches (reversed), ..., alt_1 matches (reversed),
            //    alt_0 matches (reversed)]
            // After pushing to LIFO: alt_0's highest-priority match is on top.
            let mut groups: Vec<Vec<(usize, RegexCaptures)>> = Vec::new();
            let capture_slots = alternation_capture_slots(alternatives);
            for alt in alternatives {
                let earlier_matched = groups.iter().any(|g| !g.is_empty());
                // Defer a side-effect-only alternative (`|| { die ... }`): once
                // an earlier alternative matched, raku never reaches it, so
                // running it here would fire its side effects spuriously.
                if Self::is_pure_code_block_alt(alt) && earlier_matched {
                    groups.push(Vec::new());
                    continue;
                }
                // Same reasoning for a branch that merely *contains* a plain
                // block (`|| . { die ... }`): its candidates are still needed for
                // enclosing backtracking, but raku's cursor never reaches it, so
                // its side effects must not fire. See `SPECULATIVE_ALT_BRANCH`.
                let inner_matches = if earlier_matched {
                    let flag = &super::regex_helpers::SPECULATIVE_ALT_BRANCH;
                    let prev = flag.with(std::cell::Cell::get);
                    flag.with(|f| f.set(true));
                    let r = self.regex_match_ends_from_caps_in_pkg(alt, chars, pos, pkg);
                    flag.with(|f| f.set(prev));
                    r
                } else {
                    self.regex_match_ends_from_caps_in_pkg(alt, chars, pos, pkg)
                };
                // inner_matches is in HIGHEST FIRST order (per regex_match_ends_from_caps_in_pkg
                // convention). Reverse to LOWEST FIRST for our return convention.
                let mut group = Vec::new();
                for (next, mut inner_caps) in inner_matches {
                    if !super::regex_helpers::IN_QUANTIFIED_ALTERNATION_MATCH.with(Cell::get) {
                        inner_caps
                            .positional
                            .resize(capture_slots, PosSlot::alternation_padding());
                    }
                    let mut new_caps = RegexCaptures::default();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().merge(v);
                    }
                    new_caps.positional.append(&mut inner_caps.positional);
                    new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                    group.push((next, new_caps));
                }
                group.reverse(); // now LOWEST FIRST within this alt's group
                groups.push(group);
            }
            // groups[0] = alt0 (highest priority), groups[N] = altN (lowest priority).
            // We want lower-priority alts first in the output (pushed first = bottom of LIFO).
            groups.reverse();
            return groups.into_iter().flatten().collect();
        }
        if let RegexAtom::Conjunction(branches) = atom {
            // ALL branches must match the SAME substring: every branch must
            // succeed and end at the same position. Captures from EVERY branch
            // are merged (Raku keeps all captures from each side of `&` / `&&`),
            // preserving written order. We try the candidate ends of the first
            // branch and, for each, require every other branch to match exactly
            // to that end.
            let Some((first, rest)) = branches.split_first() else {
                return vec![(pos, RegexCaptures::default())];
            };
            let mut out: Vec<(usize, RegexCaptures)> = Vec::new();
            // first-branch candidates: HIGHEST-priority-first from ends fn.
            // Build the output LOWEST-priority-first by reversing.
            let mut first_ends = self.regex_match_ends_from_caps_in_pkg(first, chars, pos, pkg);
            first_ends.reverse();
            for (end, first_caps) in first_ends {
                let mut merged = merge_regex_captures(RegexCaptures::default(), first_caps);
                let mut ok = true;
                for branch in rest {
                    if let Some(bcaps) =
                        self.regex_match_branch_ending_at(branch, chars, pos, end, pkg)
                    {
                        merged = merge_regex_captures(merged, bcaps);
                    } else {
                        ok = false;
                        break;
                    }
                }
                if ok {
                    out.push((end, merged));
                }
            }
            return out;
        }
        if let RegexAtom::Group(pattern) = atom {
            let mut out = Vec::new();
            for (end, mut inner_caps) in
                self.regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg)
            {
                let mut new_caps = RegexCaptures::default();
                for (k, v) in inner_caps.named.drain() {
                    new_caps.named.entry(k).or_default().merge(v);
                }
                new_caps.positional.append(&mut inner_caps.positional);
                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                // A `<(` / `)>` capture marker inside the group sets the match
                // boundaries for the whole pattern; propagate it out of the group.
                if inner_caps.capture_start.is_some() {
                    new_caps.capture_start = inner_caps.capture_start;
                }
                if inner_caps.capture_end.is_some() {
                    new_caps.capture_end = inner_caps.capture_end;
                }
                out.push((end, new_caps));
            }
            // Reverse inner match order so LIFO stack respects frugal/greedy priority.
            out.reverse();
            return out;
        }
        if let RegexAtom::CaptureIsolatedGroup(pattern) = atom {
            // Same shape as the `Group` arm just above (collect ALL candidate
            // ends so the outer pattern can backtrack into a shorter match of
            // the isolated sub-pattern), but discard the inner captures
            // entirely instead of merging them — see the variant's doc
            // comment and `regex_match_capture.rs`'s single-candidate twin.
            let mut out = Vec::new();
            for (end, _inner_caps) in
                self.regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg)
            {
                out.push((end, RegexCaptures::default()));
            }
            out.reverse();
            return out;
        }
        if let RegexAtom::GoalMatch {
            goal,
            inner,
            goal_text,
        } = atom
        {
            let mut out = Vec::new();
            for (inner_end, inner_caps) in
                self.regex_match_ends_from_caps_in_pkg(inner, chars, pos, pkg)
            {
                let goal_matches =
                    self.regex_match_ends_from_caps_in_pkg(goal, chars, inner_end, pkg);
                if goal_matches.is_empty() {
                    Self::record_goal_failure(goal_text, inner_end);
                    continue;
                }
                for (goal_end, goal_caps) in goal_matches {
                    let new_caps = merge_regex_captures(
                        RegexCaptures::default(),
                        merge_regex_captures(goal_caps, inner_caps.clone()),
                    );
                    out.push((goal_end, new_caps));
                }
            }
            // As for `Group` above: the inner/goal enumerations come
            // highest-priority first, while this function's contract is
            // lowest-priority first (the engine iterates in reverse). Without the
            // flip a goalpost stopped at the FIRST possible closer instead of the
            // greedy one — `'ab''cd'` under `"'" ~ "'" [ … | "''" ]*` matched only
            // `'ab'`.
            out.reverse();
            return out;
        }
        if let RegexAtom::CaptureGroup(pattern) = atom {
            let mut out = Vec::new();
            for (end, inner_caps) in
                self.regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg)
            {
                let mut new_caps = RegexCaptures::default();
                let mut inner_caps = inner_caps;
                // Named captures appearing inside a positional capture group belong
                // to that group's sub-Match (`$/[0]<name>`), NOT to the parent
                // Match's top-level named captures (`$/<name>`). They are preserved
                // only in `positional_subcaps` below and are intentionally NOT
                // merged into the parent `named` / `named_subcaps` maps.
                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                // Store inner captures as subcaptures of this group
                let mut subcap = inner_caps;
                subcap.from = pos;
                subcap.to = end;
                new_caps.positional.push(PosSlot {
                    from: pos,
                    to: end,
                    subcap: Some(std::sync::Arc::new(subcap.into_cap_node())),
                    ..Default::default()
                });
                out.push((end, new_caps));
            }
            // Reverse the inner match order so the outer LIFO stack
            // correctly respects frugal (shortest-first) vs greedy (longest-first).
            out.reverse();
            let mut seen = std::collections::HashSet::new();
            out.retain(|(end, _)| seen.insert(*end));
            return out;
        }
        if let RegexAtom::Alternation(alternatives)
        | RegexAtom::SequentialAlternation(alternatives) = atom
        {
            let mut out = Vec::new();
            for alt in alternatives {
                for (end, mut inner_caps) in
                    self.regex_match_ends_from_caps_in_pkg(alt, chars, pos, pkg)
                {
                    let mut new_caps = RegexCaptures::default();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().merge(v);
                    }
                    new_caps.positional.append(&mut inner_caps.positional);
                    new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                    out.push((end, new_caps));
                }
            }
            out
        } else if let RegexAtom::Named(name) = atom {
            let spec = Self::parse_named_regex_lookup_spec(name);
            // Symbolic indirect subrule `<::(EXPR)>`: evaluate EXPR to obtain
            // the rule name dynamically, then dispatch as if it were `<NAME>`.
            // This must resolve through the same path as a literal subrule so
            // that builtin character classes (e.g. `alpha`) and user-defined
            // tokens both work.
            if spec.lookup_name == "::" && spec.arg_exprs.len() == 1 {
                let Some(val) = self.eval_regex_expr_value(&spec.arg_exprs[0], current_caps) else {
                    return Vec::new();
                };
                let dyn_name = val.to_string_value();
                let dyn_atom = RegexAtom::Named(dyn_name);
                return self.regex_match_atom_all_with_capture_in_pkg(
                    &dyn_atom,
                    chars,
                    pos,
                    current_caps,
                    pkg,
                    ignore_case,
                );
            }
            let arg_values = if spec.arg_exprs.is_empty() {
                Vec::new()
            } else {
                let Some(values) = self.eval_regex_arg_list(&spec.arg_exprs, current_caps) else {
                    return Vec::new();
                };
                values
            };
            // A `$*`-twigil parameter of the subrule is established in the
            // dynamic scope *before* its pattern is resolved (the pattern may
            // interpolate it) and stays there for the whole match, so nested
            // subrules and code blocks see it. The caller tears it back down.
            *dyn_saved = self.install_subrule_dynamic_params(&spec.lookup_name, pkg, &arg_values);
            // Resolve + parse the candidates once (memoized for the
            // argument-less common case — see PARSED_TOKEN_CANDIDATES).
            let (candidates, raw_empty) = self.parsed_subrule_candidates(&spec, pkg, &arg_values);
            // A subrule that resolves to no token/regex/rule but names a plain
            // METHOD of the grammar (`rule TOP { <.panic> }` where `method panic`
            // is defined) is a method-call subrule: invoke it. Its exception (e.g.
            // `die` inside the method) must propagate out of the parse rather than
            // being swallowed as a silent non-match.
            if raw_empty
                && let Some(result) = self.try_regex_subrule_as_method(&spec, chars, pos, pkg)
            {
                return result;
            }
            // A grammar declared under a custom EXPORTHOW metaclass with a user
            // `find_method` routes subrule dispatch through it (the
            // Metamodel::GrammarHOW protocol); `None` falls through to the
            // normal engine path.
            if !candidates.is_empty()
                && !self.registry().grammar_custom_how.is_empty()
                && let Some(result) =
                    self.try_custom_how_subrule_dispatch(&spec, chars, pos, pkg, &arg_values)
            {
                return result;
            }
            if !candidates.is_empty() {
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
                let lr_name = if arg_values.is_empty() {
                    spec.lookup_name.clone()
                } else {
                    let mut n = spec.lookup_name.clone();
                    for v in &arg_values {
                        n.push('\u{0}');
                        n.push_str(&Self::format_named_regex_arg_value(v));
                    }
                    n
                };
                let lr_key = (lr_name, chars.len() - pos);

                // Check if this call is currently active (left recursion detected).
                let is_active = LR_ACTIVE.with(|a| a.borrow().contains_key(&lr_key));
                if is_active {
                    // Genuine left recursion: this key's evaluation depends on
                    // its own seed, so the owner must keep growing it.
                    LR_SEED_READ.with(|s| s.borrow_mut().insert(lr_key.clone()));
                    // Return the current seed for this (name, position).
                    // The seed is stored in HIGHEST FIRST order (raw inner
                    // matches, absolute positions).
                    let seed =
                        LR_MEMO.with(|m| m.borrow().get(&lr_key).cloned().unwrap_or_default());
                    // Wrap seed into outer captures. build_named_candidates_from_inner
                    // returns items in the same order as input (HIGHEST FIRST).
                    // Caller expects LOWEST FIRST, so reverse.
                    let mut result = Self::build_named_candidates_from_inner(
                        seed, pos, &spec, None, // no sym_key for seed
                    );
                    result.reverse();
                    return result;
                }

                // Not currently active: run the growing-seed algorithm.
                //
                // Seed storage: raw (un-wrapped) inner matches in HIGHEST FIRST
                // order, with absolute positions.
                // When is_active branch reads the seed, it passes them to
                // build_named_candidates_from_inner which adds the outer wrapping.
                //
                // Initialize with empty seed (= no match yet).
                LR_MEMO.with(|m| m.borrow_mut().insert(lr_key.clone(), Vec::new()));
                LR_ACTIVE.with(|a| a.borrow_mut().insert(lr_key.clone(), ()));
                // This key starts out un-consulted for THIS activation; a stale
                // entry from an earlier activation at the same key must not be
                // read as "left-recursive" here.
                let outer_seed_read = LR_SEED_READ.with(|s| s.borrow_mut().remove(&lr_key));

                // best_inner_max: max inner_end seen so far (None = nothing matched yet).
                let mut best_inner_max: Option<usize> = None;

                // best_raw: raw inner matches for the best iteration, HIGHEST FIRST.
                let mut best_raw: Vec<(usize, RegexCaptures)> = Vec::new();

                let has_proto = candidates.iter().any(|(_, _, sym)| sym.is_some());
                // Left-recursion escape hatch for the rank-then-match path — see
                // the `seed_was_consulted` handling below.
                let mut lr_match_all = false;

                loop {
                    // Evaluate all candidates' patterns directly (unwrapped).
                    let mut raw_out: Vec<(usize, RegexCaptures)> = Vec::new();

                    if has_proto && !lr_match_all {
                        // ADR-0046 Decision 1: rank the proto candidates by
                        // MEASUREMENT, then match only the winner. Ranking runs
                        // under `LTM_DECLARATIVE_MODE`, so it executes nothing
                        // (ADR-0009) — which is what keeps a losing candidate's
                        // `{ … }` blocks and action methods from firing (ADR-0046
                        // §2.3). This is the same `(prefix_len, litlen, decl
                        // order)` triple `|` alternation and the `:rule<...>`
                        // proto entry point rank by; declaration order comes free
                        // from a stable sort over `candidates`, which is already
                        // in declaration order.
                        let mut ranked: Vec<(usize, (usize, usize))> = Vec::new();
                        for (idx, (parsed, sub_pkg, _)) in candidates.iter().enumerate() {
                            let (plen, stopped) =
                                self.ltm_prefix_len_at(parsed, chars, pos, sub_pkg);
                            // ADR-0022 §4.1's contract: `(None, false)` is a sound
                            // "this candidate cannot match here" verdict and may
                            // filter; `(None, true)` only means the measurement was
                            // cut short, so the candidate is kept, ranked at 0.
                            if plen.is_none() && !stopped {
                                continue;
                            }
                            let mut seen = std::collections::HashSet::new();
                            let litlen =
                                self.ltm_litlen_at(parsed, chars, pos, sub_pkg, &mut seen, 0);
                            ranked.push((idx, (plen.unwrap_or(0), litlen)));
                        }
                        ranked.sort_by_key(|(_, rank)| std::cmp::Reverse(*rank));
                        // Attempt the ranked candidates in order and stop at the
                        // first that actually matches — Rakudo tries the NFA's
                        // fates in order and commits to the first that succeeds,
                        // without backtracking into a later fate when what FOLLOWS
                        // the subrule call fails (verified against `raku`).
                        for (idx, _) in ranked {
                            let (parsed, sub_pkg, sym_key) = &candidates[idx];
                            let sym_key = sym_key.clone();
                            let all_matches =
                                self.regex_match_ends_from_caps_in_pkg(parsed, chars, pos, sub_pkg);
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
                                    caps.sym = sym_key.clone();
                                }
                                raw_out.push((end, caps));
                            }
                            break;
                        }
                    } else {
                        for (parsed, sub_pkg, sym_key) in candidates.iter() {
                            let all_matches =
                                self.regex_match_ends_from_caps_in_pkg(parsed, chars, pos, sub_pkg);
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
                                    caps.sym = sym_key.clone();
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
                        // Non-LTM: raw_out is already HIGHEST FIRST (from regex_match_ends_from_caps_in_pkg).
                        // Dedup: keep first occurrence for each end (first = highest priority).
                        let mut tmp: Vec<(usize, RegexCaptures)> = Vec::new();
                        let mut seen_ends = std::collections::HashSet::new();
                        for item in raw_out {
                            if seen_ends.insert(item.0) {
                                tmp.push(item);
                            }
                        }
                        tmp
                    };

                    let new_max: Option<usize> = deduped_raw.iter().map(|(e, _)| *e).max();

                    // Nothing re-entered this key, so the evaluation never read
                    // the seed and cannot change if the seed grows: this rule is
                    // not left-recursive at this position and the first result is
                    // already final. Re-running the candidates would recompute the
                    // identical set — and since every nested subrule did the same,
                    // that redundant second pass compounded to 2^depth over a
                    // precedence-climbing grammar (99problems-41-to-50.t P47).
                    let seed_was_consulted = LR_SEED_READ.with(|s| s.borrow().contains(&lr_key));
                    if !seed_was_consulted {
                        best_raw = deduped_raw;
                        break;
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
                        LR_MEMO.with(|m| m.borrow_mut().insert(lr_key.clone(), deduped_raw));
                    } else {
                        // No growth: done.
                        break;
                    }
                }

                // Clean up active/memo state.
                LR_ACTIVE.with(|a| a.borrow_mut().remove(&lr_key));
                LR_MEMO.with(|m| m.borrow_mut().remove(&lr_key));
                // Restore the enclosing activation's consulted flag: an inner
                // activation of the same key must not mask the outer one's.
                LR_SEED_READ.with(|s| {
                    let mut s = s.borrow_mut();
                    if outer_seed_read {
                        s.insert(lr_key.clone());
                    } else {
                        s.remove(&lr_key);
                    }
                });

                // Wrap best_raw into outer captures and return.
                // best_raw is HIGHEST FIRST; build_named_candidates_from_inner returns in
                // the same order (one-to-one), so result is HIGHEST FIRST.
                // Caller expects LOWEST FIRST, so reverse.
                let mut result =
                    Self::build_named_candidates_from_inner(best_raw, pos, &spec, None);
                result.reverse();
                result
            } else {
                self.regex_match_atom_with_capture_in_pkg(
                    atom,
                    chars,
                    pos,
                    current_caps,
                    pkg,
                    ignore_case,
                )
                .into_iter()
                .collect()
            }
        } else {
            self.regex_match_atom_with_capture_in_pkg(
                atom,
                chars,
                pos,
                current_caps,
                pkg,
                ignore_case,
            )
            .into_iter()
            .collect()
        }
    }

    /// Dispatch a subrule that names a plain grammar METHOD (not a token/regex/rule).
    /// `rule TOP { <.panic> }` where `method panic { die ... }` calls the method;
    /// its exception propagates via `PENDING_REGEX_ERROR` (checked by the grammar
    /// parse driver) instead of being swallowed as a silent non-match. Returns:
    /// - `None` — not a method subrule; caller falls through to the normal path.
    /// - `Some(vec![])` — dispatched but produced no match (method died → pending
    ///   error set, or returned an undefined/false cursor).
    /// - `Some(vec![(end, caps)])` — the method returned a defined Match/Cursor.
    fn try_regex_subrule_as_method(
        &mut self,
        spec: &NamedRegexLookupSpec,
        chars: &[char],
        pos: usize,
        pkg: &str,
    ) -> Option<Vec<(usize, RegexCaptures)>> {
        // Only plain, argument-less identifier subrules dispatched against a real
        // grammar package. `<::>` indirection, char-class specs, and builtin
        // assertions are handled elsewhere.
        if spec.token_lookup
            || !spec.arg_exprs.is_empty()
            || pkg.is_empty()
            || spec.lookup_name.is_empty()
            || spec.lookup_name.contains("::")
            || !spec
                .lookup_name
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
        {
            return None;
        }
        // The name must be a user method declared directly on this grammar (not an
        // inherited Cursor/Grammar builtin, which the normal subrule/builtin paths
        // already cover).
        let is_user_method = self
            .registry()
            .user_method_overloads(pkg, &spec.lookup_name)
            .is_some();
        if !is_user_method {
            return None;
        }
        // Run the method in a scratch interpreter (mirrors `eval_regex_code_assertion`).
        //
        // The invocant is an INSTANCE of the grammar carrying the cursor state
        // (`from`/`pos`/`to`/`orig`), not the bare type object: raku hands such a
        // method the in-progress cursor, which is what makes the documented
        // `method mark(--> ::?CLASS:D) { $!invalid = True; self }` idiom work. A
        // type object made every attribute touch die with "Cannot look up
        // attributes in a G type object", and returning `self` (a type object) read
        // as "no match", which failed the whole parse. Method resolution still
        // finds the grammar's own method because the instance's class IS the
        // grammar.
        let mut cursor_attrs = crate::value::AttrMap::new();
        let orig: String = chars.iter().collect();
        cursor_attrs.insert("orig", Value::str(orig));
        cursor_attrs.insert("from", Value::int(pos as i64));
        cursor_attrs.insert("pos", Value::int(pos as i64));
        cursor_attrs.insert("to", Value::int(pos as i64));
        let invocant = Value::make_instance(crate::symbol::Symbol::intern(pkg), cursor_attrs);
        let mut interp = Interpreter {
            env: self.env.clone(),
            current_package: Arc::new(RwLock::new(pkg.to_string())),
            ..Self::new_regex_scratch()
        };
        // Full registry: the grammar's methods live in `Registry::classes`, which
        // the lean `copy_decl_registry_into` omits.
        self.copy_full_registry_into(&mut interp);
        if self.test_module_loaded() {
            interp.loaded_modules = self.loaded_modules.clone();
            interp.tap.ensure_state();
        }
        match interp.call_method_with_values(invocant, &spec.lookup_name, Vec::new()) {
            Err(e) => {
                // Propagate the method's exception (e.g. `die`) out of the parse.
                crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|slot| {
                    *slot.borrow_mut() = Some(e);
                });
                Some(Vec::new())
            }
            Ok(v) => {
                // A returned grammar INVOCANT (typically `self`) reports an
                // ABSOLUTE position in `pos`, so the parse resumes there — the
                // idiomatic `{ …; self }` is a zero-width success at `pos`.
                //
                // The class-name test alone is not enough: a grammar's parse
                // cursors report the grammar's own class too (raku: `Grammar`
                // IS a `Match` subclass), so a method that returns a real
                // sub-match (`return self.subparse(...)`, `$str ~~ /re/`) would
                // be misread as a zero-width `self` and swallow its extent.
                // A Match carries its own from/to and belongs to the extent
                // branch below; only a non-Match instance of the grammar is the
                // invocant.
                if let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = v.view()
                    && class_name == pkg
                    && !v.is_match_instance()
                {
                    let end = attributes
                        .as_map()
                        .get("pos")
                        .and_then(|p| p.as_int())
                        .filter(|p| *p >= 0)
                        .map(|p| p as usize)
                        .unwrap_or(pos);
                    return (end <= chars.len())
                        .then(|| vec![(end, RegexCaptures::default())])
                        .or(Some(Vec::new()));
                }
                // A defined Match/Cursor return advances the parse by its extent.
                // (Match goes through the seam; a non-Match cursor-like instance
                // with a `to` attribute also counts.)
                if let Some(to) = v
                    .match_to()
                    .or_else(|| {
                        if let ValueView::Instance { attributes, .. } = v.view() {
                            attributes.as_map().get("to").and_then(|t| t.as_int())
                        } else {
                            None
                        }
                    })
                    .filter(|&t| t >= 0)
                {
                    let end = pos + to as usize;
                    if end <= chars.len() {
                        return Some(vec![(end, RegexCaptures::default())]);
                    }
                }
                // Undefined / non-cursor return → treated as a non-match.
                Some(Vec::new())
            }
        }
    }

    /// Build named regex candidates from inner match results. Inner positions are
    /// already absolute (ADR-0016 P1: the subrule body was matched against the whole
    /// subject starting at `pos`, not a re-slice), so nothing is rebased here.
    /// Wraps each inner match in the appropriate capture structure for the named regex call.
    /// `pos` is the position of the named atom in `chars`. Each candidate is a
    /// capture DELTA relative to an empty baseline (ADR-0007).
    pub(super) fn build_named_candidates_from_inner(
        inner_matches: Vec<(usize, RegexCaptures)>,
        pos: usize,
        spec: &NamedRegexLookupSpec,
        sym_key: Option<&String>,
    ) -> Vec<(usize, RegexCaptures)> {
        let mut out = Vec::new();
        for (end, inner_caps) in inner_matches {
            let mut new_caps = RegexCaptures::default();
            let capture_name = spec
                .capture_name
                .as_deref()
                .or_else(|| (!spec.silent).then_some(spec.lookup_name.as_str()));
            if let Some(capture_name) = capture_name {
                // Apply the subrule's own capture markers (`<(` / `)>`): a token
                // like `token foo { 12345 <( 67890 }` restricts its `<foo>`
                // submatch to `67890`. They are already absolute, and `None` when
                // the subrule used no markers, so this is a no-op otherwise.
                let cs = inner_caps.capture_start.unwrap_or(pos).clamp(pos, end);
                let ce = inner_caps.capture_end.unwrap_or(end).clamp(cs, end);
                let mut subcap = inner_caps;
                subcap.from = cs;
                subcap.to = ce;
                // sym is already set on subcap from raw_out collection loop.
                // Fall back to sym_key parameter for the is_active (seed) path.
                if subcap.sym.is_none() && sym_key.is_some() {
                    subcap.sym = sym_key.cloned();
                }
                // The subrule's own inline `{ … }` code blocks stay ON the subcap
                // (a queryable Match node) rather than bubbling into the parent, so
                // the reduce-time walk (`reduce_regex_captures_made`) can run them
                // once at this node — with `$/` bound to this subrule's Match — and
                // commit the produced `make` value to `subcap.ast`. Bubbling them up
                // (the old behaviour) ran them at the top level with the wrong `$/`
                // and dropped the per-node `.made`.
                // A non-suppressing alias `<name=subrule>` (NOT `<name=.subrule>` /
                // `<name=&subrule>`) installs the capture under BOTH the alias name
                // AND the subrule's own name, matching Rakudo (e.g. `<x=num>` yields
                // `$<x>` and `$<num>`; repeated `<num>`/`<offset=count>` aggregate
                // into a list under the rule name). Snapshot the subcap/text before
                // the alias push consumes them so we can also store under the original.
                let also_under_original = spec.capture_name.is_some()
                    && !spec.alias_replaces_original
                    && capture_name != spec.lookup_name;
                let original_subcap = also_under_original.then(|| subcap.clone());
                // For an aliased capture (`<x=rule>`), record the original rule
                // name for grammar action dispatch BEFORE the node is wrapped in
                // an Arc and shared (`record_reduced_subrule` clones the handle):
                // writing it afterwards through `Arc::make_mut` deep-copied the
                // whole descendant subtree for every aliased subrule capture.
                // The alias copy carries it; the original-name copy (cloned just
                // above) keeps `action_name: None`, same as before.
                let is_alias = spec.capture_name.is_some() && capture_name != spec.lookup_name;
                let mut subcap = subcap;
                if is_alias {
                    subcap.action_name = Some(spec.lookup_name.clone());
                }
                let subcap = std::sync::Arc::new(subcap.into_cap_node());
                // This subrule has just REDUCED. Log it so a parse that fails
                // overall can still run its action, the way Rakudo (which
                // dispatches at reduce time) does — see `REDUCED_SUBRULES`.
                super::regex_helpers::record_reduced_subrule(&spec.lookup_name, &subcap);
                new_caps
                    .named
                    .entry(Symbol::intern(capture_name))
                    .or_default()
                    .nodes
                    .push(subcap);
                if is_alias {
                    new_caps
                        .capture_alias_map
                        .insert(capture_name.to_string(), spec.lookup_name.clone());
                }
                if let Some(orig_subcap) = original_subcap {
                    new_caps
                        .named
                        .entry(Symbol::intern(&spec.lookup_name))
                        .or_default()
                        .nodes
                        .push(std::sync::Arc::new(orig_subcap.into_cap_node()));
                }
            } else if !inner_caps.named.is_empty() {
                // Silent subrule (`<.foo>`) that contains nested captures. The
                // subrule is hidden from `.hash`, but its OWN action method must
                // still fire (Rakudo dispatches actions at reduce time regardless
                // of capture), and its nested rules' actions must fire too — with
                // their `.made` set on the SAME nodes the parent action reads
                // (`method header-field { ...$/<field-name>.made... }`). Store the
                // whole subrule match under a HIDDEN MARKER key in `named_subcaps`
                // (the prefix can never be a real capture name). The Match builder
                // routes marker entries into a `silent_caps` attribute instead of
                // `.hash`; the grammar action walk recurses into them. This replaces
                // the older "flatten direct children into the parent" hack, which
                // lost the rule's own action and over-exposed children in `.hash`.
                let cs = inner_caps.capture_start.unwrap_or(pos).clamp(pos, end);
                let ce = inner_caps.capture_end.unwrap_or(end).clamp(cs, end);
                let mut subcap = inner_caps;
                subcap.from = cs;
                subcap.to = ce;
                if subcap.sym.is_none() && sym_key.is_some() {
                    subcap.sym = sym_key.cloned();
                }
                subcap.action_name = Some(spec.lookup_name.clone());
                // Keep the silent subrule's inline blocks on its own (marker) node
                // for the reduce-time walk to run once — see the non-silent branch.
                let marker = format!(
                    "{}{}",
                    crate::runtime::SILENT_ACTION_MARKER_PREFIX,
                    spec.lookup_name
                );
                let subcap = std::sync::Arc::new(subcap.into_cap_node());
                super::regex_helpers::record_reduced_subrule(&spec.lookup_name, &subcap);
                new_caps
                    .named
                    .entry(Symbol::intern(&marker))
                    .or_default()
                    .nodes
                    .push(subcap);
            } else {
                // Childless silent subrule (`<.ws>`, `<.CRLF>`, `<.sym>`, ...): no
                // nested captures and (in practice) no action of interest, so keep
                // the cheap path — just carry its code blocks up. Routing these
                // through the marker channel would store a subcap for every `<.ws>`
                // in a parse for no benefit.
                let mut inner_caps = inner_caps;
                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
            }
            out.push((end, new_caps));
        }
        out
    }
}
