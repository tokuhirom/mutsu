use std::cell::RefCell;
use std::collections::HashMap;

use super::super::*;
use super::regex_helpers::{NamedRegexLookupSpec, merge_regex_captures};

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
}

impl Interpreter {
    /// Try to match `branch` starting at `pos` such that it ends exactly at
    /// `target_end`. Returns the branch's own captures (relative to an empty
    /// baseline) on success. Used by conjunction (`&` / `&&`) matching, where
    /// every branch must cover the same substring.
    fn regex_match_branch_ending_at(
        &self,
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

    pub(super) fn regex_match_atom_all_with_capture_in_pkg(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: &str,
        ignore_case: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        // Return value convention: LOWEST PRIORITY FIRST, HIGHEST PRIORITY LAST.
        // Callers push this vec to a LIFO stack in order, so the last element
        // is pushed last and sits on top (= tried first = highest priority).

        if let RegexAtom::Alternation(alternatives) = atom {
            // | (LTM): try all alternatives, longest match wins.
            let mut indexed: Vec<(usize, usize, RegexCaptures)> = Vec::new();
            for (i, alt) in alternatives.iter().enumerate() {
                if let Some((next, mut inner_caps)) =
                    self.regex_match_end_from_caps_in_pkg(alt, chars, pos, pkg)
                {
                    let mut new_caps = current_caps.clone();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().extend(v);
                    }
                    for (k, v) in inner_caps.named_subcaps.drain() {
                        new_caps.named_subcaps.entry(k).or_default().extend(v);
                    }
                    new_caps
                        .named_quantified
                        .extend(inner_caps.named_quantified.drain());
                    new_caps.positional.append(&mut inner_caps.positional);
                    new_caps
                        .positional_subcaps
                        .append(&mut inner_caps.positional_subcaps);
                    new_caps
                        .positional_quantified
                        .append(&mut inner_caps.positional_quantified);
                    new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                    indexed.push((i, next, new_caps));
                }
            }
            indexed.sort_by(|a, b| a.1.cmp(&b.1).then(b.0.cmp(&a.0)));
            return indexed
                .into_iter()
                .map(|(_, end, caps)| (end, caps))
                .collect();
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
            for alt in alternatives {
                let inner_matches = self.regex_match_ends_from_caps_in_pkg(alt, chars, pos, pkg);
                // inner_matches is in HIGHEST FIRST order (per regex_match_ends_from_caps_in_pkg
                // convention). Reverse to LOWEST FIRST for our return convention.
                let mut group = Vec::new();
                for (next, mut inner_caps) in inner_matches {
                    let mut new_caps = current_caps.clone();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().extend(v);
                    }
                    for (k, v) in inner_caps.named_subcaps.drain() {
                        new_caps.named_subcaps.entry(k).or_default().extend(v);
                    }
                    new_caps
                        .named_quantified
                        .extend(inner_caps.named_quantified.drain());
                    new_caps.positional.append(&mut inner_caps.positional);
                    new_caps
                        .positional_subcaps
                        .append(&mut inner_caps.positional_subcaps);
                    new_caps
                        .positional_quantified
                        .append(&mut inner_caps.positional_quantified);
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
                return vec![(pos, current_caps.clone())];
            };
            let mut out: Vec<(usize, RegexCaptures)> = Vec::new();
            // first-branch candidates: HIGHEST-priority-first from ends fn.
            // Build the output LOWEST-priority-first by reversing.
            let mut first_ends = self.regex_match_ends_from_caps_in_pkg(first, chars, pos, pkg);
            first_ends.reverse();
            for (end, first_caps) in first_ends {
                let mut merged = merge_regex_captures(current_caps.clone(), first_caps);
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
                let mut new_caps = current_caps.clone();
                for (k, v) in inner_caps.named.drain() {
                    new_caps.named.entry(k).or_default().extend(v);
                }
                for (k, v) in inner_caps.named_subcaps.drain() {
                    new_caps.named_subcaps.entry(k).or_default().extend(v);
                }
                new_caps
                    .named_quantified
                    .extend(inner_caps.named_quantified.drain());
                for v in inner_caps.positional.drain(..) {
                    new_caps.positional.push(v);
                }
                new_caps
                    .positional_subcaps
                    .append(&mut inner_caps.positional_subcaps);
                new_caps
                    .positional_quantified
                    .append(&mut inner_caps.positional_quantified);
                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                out.push((end, new_caps));
            }
            // Reverse inner match order so LIFO stack respects frugal/greedy priority.
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
                        current_caps.clone(),
                        merge_regex_captures(goal_caps, inner_caps.clone()),
                    );
                    out.push((goal_end, new_caps));
                }
            }
            return out;
        }
        if let RegexAtom::CaptureGroup(pattern) = atom {
            let mut out = Vec::new();
            for (end, inner_caps) in
                self.regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg)
            {
                let captured: String = chars[pos..end].iter().collect();
                let mut new_caps = current_caps.clone();
                let mut inner_caps = inner_caps;
                // Named captures appearing inside a positional capture group belong
                // to that group's sub-Match (`$/[0]<name>`), NOT to the parent
                // Match's top-level named captures (`$/<name>`). They are preserved
                // only in `positional_subcaps` below and are intentionally NOT
                // merged into the parent `named` / `named_subcaps` maps.
                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                // Store inner captures as subcaptures of this group
                let mut subcap = inner_caps;
                subcap.matched = captured.clone();
                subcap.from = pos;
                subcap.to = end;
                new_caps.positional.push(captured);
                new_caps.positional_subcaps.push(Some(subcap));
                new_caps.positional_quantified.push(None);
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
                    let mut new_caps = current_caps.clone();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().extend(v);
                    }
                    for v in inner_caps.positional.drain(..) {
                        new_caps.positional.push(v);
                    }
                    new_caps
                        .positional_subcaps
                        .append(&mut inner_caps.positional_subcaps);
                    new_caps
                        .positional_quantified
                        .append(&mut inner_caps.positional_quantified);
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
            let candidates = self.resolve_named_regex_candidates_in_pkg(&spec, pkg, &arg_values);
            if !candidates.is_empty() {
                let tail: Vec<char> = chars[pos..].to_vec();

                // Left-recursion detection using (name, remaining_chars_count) as key.
                // remaining = chars.len() - pos = tail.len().
                // When rule r calls <&r> recursively at the same position, both calls
                // will have the same remaining count, allowing us to detect and break
                // the left-recursion cycle.
                let lr_key = (spec.lookup_name.clone(), tail.len());

                // Check if this call is currently active (left recursion detected).
                let is_active = LR_ACTIVE.with(|a| a.borrow().contains_key(&lr_key));
                if is_active {
                    // Return the current seed for this (name, position).
                    // The seed is stored in HIGHEST FIRST order (raw inner positions relative to tail).
                    let seed =
                        LR_MEMO.with(|m| m.borrow().get(&lr_key).cloned().unwrap_or_default());
                    // Wrap seed into outer captures. build_named_candidates_from_inner
                    // returns items in the same order as input (HIGHEST FIRST).
                    // Caller expects LOWEST FIRST, so reverse.
                    let mut result = Self::build_named_candidates_from_inner(
                        seed,
                        pos,
                        chars,
                        &spec,
                        current_caps,
                        None, // no sym_key for seed
                    );
                    result.reverse();
                    return result;
                }

                // Not currently active: run the growing-seed algorithm.
                //
                // Seed storage: raw (un-wrapped) inner matches in HIGHEST FIRST order,
                // with positions relative to `tail` (0-based from pos).
                // When is_active branch reads the seed, it passes them to
                // build_named_candidates_from_inner which adds the outer wrapping.
                //
                // Initialize with empty seed (= no match yet).
                LR_MEMO.with(|m| m.borrow_mut().insert(lr_key.clone(), Vec::new()));
                LR_ACTIVE.with(|a| a.borrow_mut().insert(lr_key.clone(), ()));

                // best_inner_max: max inner_end seen so far (None = nothing matched yet).
                let mut best_inner_max: Option<usize> = None;

                // best_raw: raw inner matches for the best iteration, HIGHEST FIRST.
                let mut best_raw: Vec<(usize, RegexCaptures)> = Vec::new();

                loop {
                    // Evaluate all candidates' patterns directly (unwrapped).
                    let mut raw_out: Vec<(usize, RegexCaptures)> = Vec::new();
                    let mut has_proto = false;

                    for (sub_pat, sub_pkg, sym_key) in &candidates {
                        if sym_key.is_some() {
                            has_proto = true;
                        }
                        if let Some(parsed) = self.parse_regex(sub_pat) {
                            let all_matches =
                                self.regex_match_ends_from_caps_in_pkg(&parsed, &tail, 0, sub_pkg);
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
                        // LTM: sort by end ascending, dedup, then reverse → HIGHEST (longest) FIRST.
                        raw_out.sort_by_key(|(e, _)| *e);
                        let mut tmp: Vec<(usize, RegexCaptures)> = Vec::new();
                        for item in raw_out {
                            if tmp.last().is_some_and(|(e, _)| *e == item.0) {
                                tmp.pop();
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

                // Wrap best_raw into outer captures and return.
                // best_raw is HIGHEST FIRST; build_named_candidates_from_inner returns in
                // the same order (one-to-one), so result is HIGHEST FIRST.
                // Caller expects LOWEST FIRST, so reverse.
                let mut result = Self::build_named_candidates_from_inner(
                    best_raw,
                    pos,
                    chars,
                    &spec,
                    current_caps,
                    None,
                );
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

    /// Build named regex candidates from inner match results (positions relative to tail).
    /// Wraps each inner match in the appropriate capture structure for the named regex call.
    /// `pos` is the position of the named atom in `chars`.
    fn build_named_candidates_from_inner(
        inner_matches: Vec<(usize, RegexCaptures)>,
        pos: usize,
        chars: &[char],
        spec: &NamedRegexLookupSpec,
        current_caps: &RegexCaptures,
        sym_key: Option<&String>,
    ) -> Vec<(usize, RegexCaptures)> {
        let mut out = Vec::new();
        for (inner_end, inner_caps) in inner_matches {
            let end = pos + inner_end;
            let mut new_caps = current_caps.clone();
            let capture_name = spec
                .capture_name
                .as_deref()
                .or_else(|| (!spec.silent).then_some(spec.lookup_name.as_str()));
            if let Some(capture_name) = capture_name {
                let captured: String = chars[pos..end].iter().collect();
                let mut subcap = inner_caps;
                subcap.matched = captured.clone();
                subcap.from = pos;
                subcap.to = end;
                // sym is already set on subcap from raw_out collection loop.
                // Fall back to sym_key parameter for the is_active (seed) path.
                if subcap.sym.is_none() && sym_key.is_some() {
                    subcap.sym = sym_key.cloned();
                }
                new_caps.code_blocks.extend(subcap.code_blocks.clone());
                new_caps
                    .named_subcaps
                    .entry(capture_name.to_string())
                    .or_default()
                    .push(subcap);
                new_caps
                    .named
                    .entry(capture_name.to_string())
                    .or_default()
                    .push(captured);
                if spec.capture_name.is_some() && capture_name != spec.lookup_name {
                    new_caps
                        .capture_alias_map
                        .insert(capture_name.to_string(), spec.lookup_name.clone());
                    if let Some(subcaps) = new_caps.named_subcaps.get_mut(capture_name)
                        && let Some(last) = subcaps.last_mut()
                    {
                        last.action_name = Some(spec.lookup_name.clone());
                    }
                }
            } else {
                // Silent subrule — merge named captures only.
                let mut inner_caps = inner_caps;
                for (k, v) in inner_caps.named.drain() {
                    new_caps.named.entry(k).or_default().extend(v);
                }
                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
            }
            out.push((end, new_caps));
        }
        out
    }
}
