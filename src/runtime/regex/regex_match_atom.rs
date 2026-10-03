use super::super::*;
use super::regex_helpers::{
    AlternationListFlags, LTM_DECLARATIVE_MODE, NamedRegexLookupSpec, alternation_list_flags,
    merge_goal_captures, merge_regex_captures,
};
use super::regex_ltm_fate::ltm_record_fate;
use super::regex_ltm_rank::{LtmAtomMode, ltm_atom_mode};
use super::regex_match_delta::alternation_branch_delta;

/// ADR-0022 §4.4(a): one `|` branch's rank key (prefix_len, litlen) paired
/// with its PLURAL ends (highest-priority-first).
type RankedAlternationBranch = ((usize, usize), Vec<(usize, RegexCaptures)>);

impl Interpreter {
    /// An alternation alternative that is a lone plain `{ … }` code block
    /// (`|| { die "no match" }`). Such a branch matches zero-width and exists
    /// for its side effects, so evaluating it eagerly during candidate
    /// collection fires those effects on paths raku never executes — it must
    /// only run when no other alternative matched.
    /// Candidate ends for ONE branch of a `||` (sequential alternation),
    /// packaged as a capture delta in the alternation's own positional slot
    /// space. Returned LOWEST-PRIORITY FIRST, matching the atom-producer
    /// convention.
    ///
    /// Evaluating a branch runs its embedded `{ ... }` blocks for real, so the
    /// caller decides *when* a branch is evaluated: `walk_seq_alternation`
    /// only reaches branch *k+1* after branch *k*'s candidates have all been
    /// rejected by the rest of the pattern, which is exactly when raku's
    /// cursor would enter it.
    pub(super) fn seqalt_branch_candidates(
        &mut self,
        alt: &RegexPattern,
        flags: &AlternationListFlags,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> Vec<(usize, RegexCaptures)> {
        // HIGHEST FIRST from the walk; reverse to LOWEST FIRST below.
        let inner_matches = self.regex_match_ends_from_caps_in_pkg(alt, chars, pos, pkg);
        let mut group = Vec::with_capacity(inner_matches.len());
        for (next, inner_caps) in inner_matches {
            group.push((next, alternation_branch_delta(flags, inner_caps)));
        }
        group.reverse();
        group
    }

    pub(super) fn is_pure_code_block_alt(alt: &RegexPattern) -> bool {
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

    /// A branch that is only a call of a grammar METHOD (`<.panic('...')>`):
    /// like a code block, running it has side effects (it usually `die`s), so
    /// once an earlier `||` branch matched it must not be run.
    // Cost: O(1) expected, see `subrule_names_user_method`.
    pub(super) fn is_method_subrule_alt(&mut self, alt: &RegexPattern, pkg: Symbol) -> bool {
        alt.tokens.len() == 1
            && matches!(alt.tokens[0].quant, RegexQuant::One)
            && alt.tokens[0].separator.is_none()
            && matches!(&alt.tokens[0].atom, RegexAtom::Named(name)
                if self.subrule_names_user_method(name.spec(), pkg))
    }

    /// Try to match `branch` starting at `pos` such that it ends exactly at
    /// `target_end`. Returns the branch's own captures (relative to an empty
    /// baseline) on success. Used by conjunction (`&` / `&&`) matching, where
    /// every branch must cover the same substring.
    pub(super) fn regex_match_branch_ending_at(
        &mut self,
        branch: &RegexPattern,
        chars: &[char],
        pos: usize,
        target_end: usize,
        pkg: Symbol,
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
        flags: &AlternationListFlags,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> Vec<RankedAlternationBranch> {
        let mut out = Vec::new();
        for alt in alts {
            let raw_ends = self.regex_match_ends_from_caps_in_pkg(alt, chars, pos, pkg);
            if raw_ends.is_empty() {
                continue;
            }
            let ends: Vec<(usize, RegexCaptures)> = raw_ends
                .into_iter()
                .map(|(end, inner_caps)| (end, alternation_branch_delta(flags, inner_caps)))
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
        pkg: Symbol,
        ignore_case: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        self.regex_match_atom_all_with_capture_opts(
            atom,
            chars,
            pos,
            current_caps,
            pkg,
            ignore_case,
            false,
        )
    }

    /// [`Self::regex_match_atom_all_with_capture_in_pkg`] with ADR-0073's
    /// Slice-2 knob. `subrule_first_only` says the caller cannot backtrack into
    /// this atom (it is ratcheted), so a `<subrule>` atom needs only its
    /// highest-priority end and its body may be walked with `first_only` — see
    /// `regex_subrule_lazy`. It is ignored by every other atom kind, whose
    /// candidates the demand-driven driver already produces one at a time.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn regex_match_atom_all_with_capture_opts(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: Symbol,
        ignore_case: bool,
        subrule_first_only: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        self.regex_match_atom_all_with_arg_values(
            atom,
            chars,
            pos,
            current_caps,
            pkg,
            ignore_case,
            subrule_first_only,
            None,
        )
    }

    /// [`Self::regex_match_atom_all_with_capture_opts`] for a `<subrule(…)>`
    /// call whose arguments the caller already evaluated (`evaluated_args`):
    /// the compiled engine evaluates them once at the call and hands them
    /// here when the call bridges, so user code in an argument does not run
    /// twice. `None` evaluates them here, as every other caller wants.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn regex_match_atom_all_with_arg_values(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: Symbol,
        ignore_case: bool,
        subrule_first_only: bool,
        mut evaluated_args: Option<Vec<Value>>,
    ) -> Vec<(usize, RegexCaptures)> {
        let mut dyn_saved = None;
        let mut dyn_installed = false;
        // A named atom is one grammar-rule invocation. Keep its declaration
        // frame around the complete resolve/match operation so a failed proto
        // candidate cannot leave a `$*` binding in the caller. LTM and failure
        // probes return no frame and therefore remain side-effect free.
        let grammar_frame = match atom {
            RegexAtom::Named(name)
                if !LTM_DECLARATIVE_MODE.with(std::cell::Cell::get)
                    && self
                        .regex_state
                        .grammar_rule_dynvar_decls
                        .contains_key(&name.spec().lookup_name) =>
            {
                let spec = name.spec();
                let arg_values = if let Some(values) = evaluated_args.take() {
                    Some(values)
                } else if spec.arg_exprs.is_empty() {
                    Some(Vec::new())
                } else {
                    self.eval_regex_arg_list(&spec.arg_exprs, current_caps)
                };
                if let Some(arg_values) = arg_values {
                    dyn_saved = self.install_subrule_dynamic_params(spec, pkg, &arg_values);
                    dyn_installed = true;
                    evaluated_args = Some(arg_values);
                    self.enter_grammar_rule_dynvars(&spec.lookup_name)
                } else {
                    None
                }
            }
            _ => None,
        };
        let mut out = self.regex_match_atom_all_with_capture_in_pkg_inner(
            atom,
            chars,
            pos,
            current_caps,
            pkg,
            ignore_case,
            subrule_first_only,
            &mut dyn_saved,
            evaluated_args,
            dyn_installed,
        );
        if let Some(frame) = grammar_frame {
            let values = self.exit_grammar_rule_dynvars(frame);
            for (_, caps) in out.iter_mut() {
                Self::attach_grammar_dynvars_to_named_caps(caps, atom, &values);
            }
        }
        // The rule's own dynamic parameters (`token usage($*USAGE)`) end with
        // the rule's match, but its action runs later, in the reduce walk, and
        // must still see them (CSS::Specification's `usage` action reads
        // `$*USAGE`). Record them on the capture node like a `:my $*x`.
        if let Some(saved) = &dyn_saved {
            let values: Vec<(String, Value)> = saved
                .iter()
                .filter_map(|(key, _)| self.env.get(key).cloned().map(|v| (key.clone(), v)))
                .collect();
            if !values.is_empty() {
                for (_, caps) in out.iter_mut() {
                    Self::attach_grammar_dynvars_to_named_caps(caps, atom, &values);
                }
            }
        }
        if let Some(saved) = dyn_saved {
            self.restore_subrule_dynamic_params(saved);
        }
        out
    }

    /// Keep a rule frame's final values on the subrule's own capture node. They
    /// must not remain in the caller's delta: the subrule's action needs them,
    /// but its caller's action runs after the subrule frame has ended.
    pub(super) fn attach_grammar_dynvars_to_named_caps(
        caps: &mut RegexCaptures,
        atom: &RegexAtom,
        values: &[(String, Value)],
    ) {
        let RegexAtom::Named(name) = atom else {
            return;
        };
        let spec = name.spec();
        let capture_symbols = if spec.silent {
            vec![spec.silent_marker_sym]
        } else {
            let mut symbols = Vec::with_capacity(2);
            if let Some(capture_sym) = spec.capture_sym {
                symbols.push(capture_sym);
            }
            if !spec.alias_replaces_original && !symbols.contains(&spec.lookup_sym) {
                symbols.push(spec.lookup_sym);
            }
            symbols
        };
        for capture_sym in capture_symbols {
            let Some(slot) = caps.named.get_mut(&capture_sym) else {
                continue;
            };
            for node in &mut slot.nodes {
                let node = std::sync::Arc::make_mut(node);
                let node_vars = &mut node.kids_mut().regex_vars;
                for (key, value) in values {
                    node_vars
                        .entry(key.clone())
                        .or_insert_with(|| value.clone());
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn regex_match_atom_all_with_capture_in_pkg_inner(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: Symbol,
        ignore_case: bool,
        subrule_first_only: bool,
        dyn_saved: &mut Option<super::regex_dynparams::SavedDynParams>,
        evaluated_args: Option<Vec<Value>>,
        dyn_installed: bool,
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

        // An LTM NFA leaf (ADR-0125) is answered under `LTM_DECLARATIVE_MODE`,
        // and a match nested in one (a `<+name>` class calling a token) must
        // run no user code (ADR-0009): a non-declarative atom there is a fate,
        // as it is in the NFA, recorded into the run's fate frame.
        if LTM_DECLARATIVE_MODE.with(std::cell::Cell::get) {
            match ltm_atom_mode(atom) {
                // A fate ends this path of the measurement: record where, and
                // fail the path so the walk goes on with the others
                // (`regex_ltm_fate`).
                LtmAtomMode::Terminate
                    if !super::regex_ltm_rank::ltm_leading_ws_is_transparent(atom, pos) =>
                {
                    ltm_record_fate(pos);
                    return Vec::new();
                }
                LtmAtomMode::Terminate => {}
                LtmAtomMode::TerminateAfter(inner) => {
                    self.ltm_record_lookahead_fates(inner, chars, pos, pkg);
                    return Vec::new();
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
            let flags = alternation_list_flags(alternatives);
            let mut branches = self.ltm_rank_and_collect_branches(
                alternatives
                    .iter()
                    .filter(|alt| !Self::is_pure_code_block_alt(alt)),
                &flags,
                chars,
                pos,
                pkg,
            );
            if branches.is_empty() {
                branches = self.ltm_rank_and_collect_branches(
                    alternatives
                        .iter()
                        .filter(|alt| Self::is_pure_code_block_alt(alt)),
                    &flags,
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
            let flags = alternation_list_flags(alternatives);
            for alt in alternatives {
                let earlier_matched = groups.iter().any(|g| !g.is_empty());
                // Defer a side-effect-only alternative (`|| { die ... }`): once
                // an earlier alternative matched, raku never reaches it, so
                // running it here would fire its side effects spuriously. This
                // residual guard only applies to the *eager* producer; when the
                // token walk drives the alternation itself
                // (`walk_seq_alternation`) a later branch is never reached at
                // all unless raku's cursor would reach it.
                if earlier_matched
                    && (Self::is_pure_code_block_alt(alt) || self.is_method_subrule_alt(alt, pkg))
                {
                    groups.push(Vec::new());
                    continue;
                }
                groups.push(self.seqalt_branch_candidates(alt, &flags, chars, pos, pkg));
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
            let outer = super::regex_backref_scope::current_outer_caps_seed();
            let mut first_ends = self.regex_match_ends_from_caps_in_pkg(first, chars, pos, pkg);
            first_ends.reverse();
            for (end, first_caps) in first_ends {
                let mut merged = merge_regex_captures(RegexCaptures::default(), first_caps);
                let mut ok = true;
                for branch in rest {
                    // Later branches see the earlier ones' captures, as the
                    // streamed driver's do (`arm_conjunction_branch_seed`).
                    let _seed = super::regex_backref_scope::arm_conjunction_branch_seed(
                        outer.as_ref(),
                        &merged,
                    );
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
        if let RegexAtom::CodeInterp { code, list } = atom {
            return self.regex_code_interp_ends(
                code,
                *list,
                chars,
                pos,
                current_caps,
                pkg,
                ignore_case,
            );
        }
        if let RegexAtom::Group(pattern) = atom {
            // The delta shape lives in `regex_match_lazy.rs` so this eager
            // producer and the demand-driven driver cannot drift (ADR-0073).
            let mut out: Vec<(usize, RegexCaptures)> = self
                .regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg)
                .into_iter()
                .map(|(end, inner_caps)| {
                    (end, super::regex_match_delta::group_merge_delta(inner_caps))
                })
                .collect();
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
        if let RegexAtom::CaptureIsolatedGroupScoped(pattern, scope) = atom {
            // Same as the plain `CaptureIsolatedGroup` arm above, but the
            // interpolated regex closed over a defining scope of its own
            // (issue #8951) — install it for the duration of this atom's
            // match so any embedded code resolves its free variables there,
            // not against whatever is live at the outer match site.
            let saved = self.install_env_scope(scope);
            let mut out = Vec::new();
            for (end, _inner_caps) in
                self.regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg)
            {
                out.push((end, RegexCaptures::default()));
            }
            self.uninstall_regex_closure_scope(Some(saved));
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
                    let new_caps = merge_goal_captures(goal_caps, inner_caps.clone());
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
            // Named captures appearing inside a positional capture group belong
            // to that group's sub-Match (`$/[0]<name>`), NOT to the parent
            // Match's top-level named captures (`$/<name>`) — see
            // `capture_group_delta`, shared with the demand-driven driver.
            let mut out: Vec<(usize, RegexCaptures)> = self
                .regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg)
                .into_iter()
                .map(|(end, inner_caps)| {
                    (
                        end,
                        super::regex_match_delta::capture_group_delta(pos, end, inner_caps),
                    )
                })
                .collect();
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
                        new_caps.named.slot_mut(k).merge(v);
                    }
                    new_caps.extend_capture_alias_map(inner_caps.take_capture_alias_map());
                    new_caps.positional.append(&mut inner_caps.positional);
                    super::regex_helpers::adopt_inline_ast(&mut new_caps, &mut inner_caps);
                    new_caps.extend_regex_vars(inner_caps.take_regex_vars());
                    out.push((end, new_caps));
                }
            }
            out
        } else if let RegexAtom::Named(name) = atom {
            let spec = name.spec().clone();
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
                let dyn_atom = RegexAtom::Named(dyn_name.into());
                return self.regex_match_atom_all_with_capture_opts(
                    &dyn_atom,
                    chars,
                    pos,
                    current_caps,
                    pkg,
                    ignore_case,
                    subrule_first_only,
                );
            }
            let arg_values = if let Some(values) = evaluated_args {
                values
            } else if spec.arg_exprs.is_empty() {
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
            if !dyn_installed {
                *dyn_saved = self.install_subrule_dynamic_params(&spec, pkg, &arg_values);
            }
            // A token/rule/regex returned by `.^find_method(...).wrap(...)`
            // has a class-keyed wrap chain, but the ordinary regex engine
            // evaluates its body directly rather than dispatching a Regex
            // value. Let the wrapper produce the cursor and feed that result
            // back through the normal named-subrule capture builder.
            if let Some(result) =
                self.try_wrapped_token_subrule_dispatch(&spec, chars, pos, pkg, &arg_values)
            {
                return result;
            }
            // Resolve + parse the candidates once (memoized for the
            // argument-less common case — see PARSED_TOKEN_CANDIDATES).
            let (candidates, raw_empty) = self.parsed_subrule_candidates(&spec, pkg, &arg_values);
            // A subrule that resolves to no token/regex/rule but names a plain
            // METHOD of the grammar (`rule TOP { <.panic> }` where `method panic`
            // is defined) is a method-call subrule: invoke it. Its exception (e.g.
            // `die` inside the method) must propagate out of the parse rather than
            // being swallowed as a silent non-match.
            if raw_empty
                && let Some(result) =
                    self.try_regex_subrule_as_method(&spec, chars, pos, pkg, &arg_values)
            {
                return result;
            }
            // A grammar declared under a custom EXPORTHOW metaclass with a user
            // `find_method` routes subrule dispatch through it (the
            // Metamodel::GrammarHOW protocol); `None` falls through to the
            // normal engine path.
            let custom_how =
                !candidates.is_empty() && !self.registry().grammar_custom_how.is_empty();
            if custom_how
                && let Some(result) =
                    self.try_custom_how_subrule_dispatch(&spec, chars, pos, pkg, &arg_values)
            {
                return result;
            }
            if !candidates.is_empty() {
                self.subrule_seed_ends(
                    &spec,
                    &candidates,
                    chars,
                    pos,
                    pkg,
                    &arg_values,
                    custom_how,
                    (subrule_first_only, ignore_case),
                )
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
        pkg: Symbol,
        args: &[Value],
    ) -> Option<Vec<(usize, RegexCaptures)>> {
        // The cursor the engine published for this one call, if any: taken at
        // once so a call nested inside the method never sees it.
        let published = self.regex_state.rx_cursor.take();
        if !self.subrule_names_user_method(spec, pkg) {
            return None;
        }
        // An exception already raised in this match (an earlier `<.panic>`)
        // ends it; a later `||` branch the engine still tries must not call
        // its methods (see `eval_regex_inline_code`).
        if crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| e.borrow().is_some()) {
            return Some(Vec::new());
        }
        // Else the walked rule invocation this call is in the body of.
        let published = published.or_else(|| self.walk_rule_cursor(chars, pos, pkg));
        // Run the method in the grammar's package over an isolated copy of the
        // env (`run_regex_sub_call_here`: only dynamic-variable writes reach
        // the caller).
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
        //
        // When the compiled engine published the calling rule invocation's own
        // cursor, that instance IS the invocant (Rakudo's cursor is the grammar
        // instance): the method's attribute writes land on it and travel onto
        // the rule's Match (#9803). Its positional state moves to this call.
        let invocant = match published {
            Some(cursor) => {
                if let ValueView::Instance { attributes, .. } = cursor.view() {
                    attributes.insert("from", Value::int(pos as i64));
                    attributes.insert("pos", Value::int(pos as i64));
                    attributes.insert("to", Value::int(pos as i64));
                }
                cursor
            }
            None => self.new_grammar_cursor(chars, pos, pkg),
        };
        let called = self.run_regex_sub_call_here(Some(pkg), |interp| {
            interp.call_method_with_values(invocant, &spec.lookup_name, args.to_vec())
        });
        match called {
            Err(e) => {
                // Propagate the method's exception (e.g. `die`) out of the parse.
                // The FIRST exception is the one the parse dies with: a later
                // `||` branch the engine still tries (`'%' <.panic: "a"> ||
                // <.panic: "b">`) must not replace it.
                crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|slot| {
                    slot.borrow_mut().get_or_insert(e);
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
                    // A negative `pos` is a failed cursor (what `callsame`
                    // into a built-in rule answers when it does not match),
                    // not a zero-width success.
                    let end = match attributes.as_map().get("pos").and_then(|p| p.as_int()) {
                        Some(p) if p < 0 => return Some(Vec::new()),
                        Some(p) => p as usize,
                        None => pos,
                    };
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
}
