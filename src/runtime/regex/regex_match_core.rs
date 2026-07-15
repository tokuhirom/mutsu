//! The backtracking regex engine: a depth-first walk over pattern tokens with
//! a single mutable capture store + undo trail (ADR-0007).
//!
//! Atom candidate producers (`regex_match_atom_all_with_capture_in_pkg`,
//! `regex_match_atom_with_capture_in_pkg`, `match_separated_quantifier`)
//! return `(end, delta)` pairs where the delta is a `RegexCaptures` relative
//! to an EMPTY baseline. The walk applies a candidate with
//! `CapStore::merge_delta`, descends to the next token, and rewinds the trail
//! on backtrack — per-step capture cost is O(delta), never O(accumulated).

use super::super::*;
use super::regex_helpers::{
    atom_contains_alternation, count_capture_groups, is_named_atom_no_args, is_silent_named_atom,
    is_simple_atom,
};
use super::regex_trail::CapStore;
use std::collections::HashSet;

/// Read-only context shared by one engine invocation (one pattern level).
struct WalkCtx<'a> {
    pattern: &'a RegexPattern,
    chars: &'a [char],
    pkg: &'a str,
    first_only: bool,
}

/// Node budget for the full-backtracking quantifier expansion over a
/// variable-length alternation (mirrors the old 20k candidate bound).
const QUANT_ALT_BUDGET: u32 = 20_000;

impl Interpreter {
    /// Collect all named capture names inside a regex atom (recursively).
    fn collect_named_captures_in_atom(atom: &RegexAtom, out: &mut HashSet<String>) {
        match atom {
            RegexAtom::Named(name) => {
                let spec = Self::parse_named_regex_lookup_spec(name);
                if !spec.silent {
                    let cap = spec
                        .capture_name
                        .unwrap_or_else(|| spec.lookup_name.clone());
                    if !cap.is_empty() {
                        out.insert(cap);
                    }
                }
            }
            RegexAtom::Group(pat) | RegexAtom::CaptureGroup(pat) => {
                for tok in &pat.tokens {
                    if let Some(name) = tok.named_capture.as_ref() {
                        out.insert(name.clone());
                    }
                    Self::collect_named_captures_in_atom(&tok.atom, out);
                }
            }
            RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
                for alt in alts {
                    for tok in &alt.tokens {
                        if let Some(name) = tok.named_capture.as_ref() {
                            out.insert(name.clone());
                        }
                        Self::collect_named_captures_in_atom(&tok.atom, out);
                    }
                }
            }
            _ => {}
        }
    }

    /// Collect all named capture names from a regex token.
    pub(super) fn collect_quantified_names_for_token(token: &RegexToken) -> HashSet<String> {
        let mut names = HashSet::new();
        if let Some(name) = token.named_capture.as_ref() {
            names.insert(name.clone());
        }
        Self::collect_named_captures_in_atom(&token.atom, &mut names);
        names
    }

    pub(super) fn regex_match_end_from_caps_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> Option<(usize, RegexCaptures)> {
        // Only the first (highest-priority / greedy) complete match is needed
        // here, and the depth-first walk discovers it first, so stop as soon as
        // one is found instead of exploring the whole backtracking tree.
        self.regex_match_ends_from_caps_in_pkg_impl(pattern, chars, start, pkg, true)
            .into_iter()
            .next()
    }

    pub(super) fn regex_match_ends_from_caps_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> Vec<(usize, RegexCaptures)> {
        self.regex_match_ends_from_caps_in_pkg_impl(pattern, chars, start, pkg, false)
    }

    /// Backtracking match returning the complete-match end positions (with
    /// captures) at `start`, in DFS-completion order (highest priority first).
    /// When `first_only` is true the walk stops after the first complete match.
    fn regex_match_ends_from_caps_in_pkg_impl(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
        first_only: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        // When :m (ignoremark) is set on the pattern, strip combining marks
        // from both text and pattern, match on the stripped versions, then
        // map positions back to the original text.
        if pattern.ignore_mark {
            use super::regex_helpers::{map_pos, strip_marks_pattern, strip_marks_text};
            let text_slice = &chars[start..];
            let (stripped_chars, pos_map) = strip_marks_text(text_slice);
            let stripped_pattern = strip_marks_pattern(pattern);
            let mut results = self.regex_match_ends_from_caps_in_pkg_impl(
                &stripped_pattern,
                &stripped_chars,
                0,
                pkg,
                first_only,
            );
            let orig_len = text_slice.len();
            for (end, caps) in &mut results {
                *end = map_pos(*end, &pos_map, orig_len) + start;
                if let Some(cs) = caps.capture_start.as_mut() {
                    *cs = map_pos(*cs, &pos_map, orig_len) + start;
                }
                if let Some(ce) = caps.capture_end.as_mut() {
                    *ce = map_pos(*ce, &pos_map, orig_len) + start;
                }
            }
            return results;
        }
        let mut store = CapStore::new(RegexCaptures {
            match_from: start,
            ..Default::default()
        });
        let mut matches = Vec::new();
        let ctx = WalkCtx {
            pattern,
            chars,
            pkg,
            first_only,
        };
        self.walk_tokens(&ctx, 0, start, &mut store, &mut matches);
        matches
    }

    /// Apply a `$<name>=` / `$N=` capture alias for `token` to the store.
    /// `pos_base` is the store's positional length at token start.
    fn store_apply_named_capture(
        store: &mut CapStore,
        chars: &[char],
        token: &RegexToken,
        from: usize,
        to: usize,
        pos_base: usize,
    ) {
        let Some(name) = token.named_capture.as_ref() else {
            return;
        };
        // Numbered scalar capture alias `$N=<atom>`: the alias name is all
        // digits, so route the capture to positional index N (padding lower
        // indices) and let subsequent groups continue numbering from N+1.
        // (Named captures always start with a letter/underscore, so an
        // all-digit name can only come from `$N=`.)
        if let Ok(forced_idx) = name.parse::<usize>() {
            let captured: String = chars[from..to].iter().collect();
            // Drop the auto-positional entry a capturing group atom produced;
            // the alias decides this capture's index explicitly.
            if matches!(token.atom, RegexAtom::CaptureGroup(_))
                && store.caps().positional.len() > pos_base
            {
                store.truncate_positional_4(pos_base);
            }
            while store.caps().positional.len() < forced_idx {
                store.push_positional(String::new(), None, None, (0, 0));
            }
            if store.caps().positional.len() == forced_idx {
                store.push_positional(captured, None, None, (from, to));
            } else {
                store.overwrite_positional(forced_idx, captured, (from, to));
            }
            return;
        }
        let captured: String = chars[from..to].iter().collect();
        // A named capture group `$<x>=(...)` aliases the group to the name and
        // does NOT consume a positional number (Raku: `/$<x>=(\w)(\d)/` makes
        // `$<x>` the \w and `$0` the \d). When this named token's atom is itself
        // a capturing group, it pushed a parent positional during matching;
        // drop those entries so the following `(...)` keeps the next number.
        // A named NON-capturing group `$<x>=[...]` leaves its inner captures'
        // positional numbers intact (its atom is not a CaptureGroup), so this
        // truncation correctly does not fire for it.
        // When aliasing a capturing group, preserve the group's own inner
        // captures (named subrules etc.) so e.g. `$<family>=(<ident>)` keeps
        // `$<family><ident>` accessible — otherwise truncating the group's
        // positional entry would discard its nested subcapture.
        let mut group_subcap: Option<std::sync::Arc<RegexCaptures>> = None;
        if matches!(token.atom, RegexAtom::CaptureGroup(_))
            && store.caps().positional.len() > pos_base
        {
            group_subcap = store
                .caps()
                .positional_subcaps
                .get(pos_base)
                .cloned()
                .flatten();
            store.truncate_positional_3(pos_base);
        }
        store.push_named(name, captured.clone());
        // `@<name>=` array-sigil alias forces list context: mark the name as
        // quantified so the Match builder always presents it as a List, even
        // for a single non-quantified capture (`@<foo>=(.(.))` → `[«bc»]`).
        if token.force_list_capture {
            store.insert_named_quantified(name.clone());
        }
        // Record a minimal sub-capture carrying the exact (from, to) span so
        // the Match object gets the right offsets even for a zero-width match
        // (e.g. `$<delim>=<[a..z]>*` matching empty). Without this the Match
        // builder falls back to searching for the captured text, which yields
        // offset 0 for an empty string and breaks `.caps`/`.chunks` ordering.
        // Only do this when no richer sub-capture already aligns with this
        // entry, so subrule-aliased captures keep their nested structure.
        let name_count = store.caps().named.get(name).map(Vec::len).unwrap_or(0);
        let sub_count = store
            .caps()
            .named_subcaps
            .get(name)
            .map(Vec::len)
            .unwrap_or(0);
        if sub_count < name_count {
            let sub = if let Some(mut gs) = group_subcap.take() {
                // Keep the group's nested captures, but pin the span/text to
                // the aliased group's extent.
                let gsm = std::sync::Arc::make_mut(&mut gs);
                gsm.from = from;
                gsm.to = to;
                gsm.match_from = from;
                gsm.matched = captured.clone();
                gs
            } else {
                std::sync::Arc::new(RegexCaptures {
                    from,
                    to,
                    matched: captured.clone(),
                    match_from: from,
                    ..Default::default()
                })
            };
            store.push_named_subcap(name, sub);
        }
        // Also capture under the secondary name (e.g., original builtin class name
        // when using `$<alias>=<builtin_class>` syntax).
        if let Some(secondary) = token.secondary_named_capture.as_ref() {
            store.push_named(secondary, captured);
        }
    }

    /// Apply a `%<name>=(...)` hash capture for `token` to the store.
    fn store_apply_hash_capture(
        store: &mut CapStore,
        chars: &[char],
        token: &RegexToken,
        from: usize,
        to: usize,
        pos_base: usize,
    ) {
        let Some(name) = token.hash_capture.as_ref() else {
            return;
        };
        let (key, value) = {
            let caps = store.caps();
            // Count how many new positional captures this atom produced
            let new_count = caps.positional.len().saturating_sub(pos_base);
            // Look for inner subcaptures in positional_subcaps
            let subcap_idx = if new_count >= 1 {
                pos_base
            } else {
                caps.positional_subcaps.len()
            };
            let inner_positionals = if subcap_idx < caps.positional_subcaps.len() {
                caps.positional_subcaps[subcap_idx]
                    .as_ref()
                    .map(|sc| &sc.positional)
            } else {
                None
            };
            if let Some(inner) = inner_positionals {
                if inner.len() >= 2 {
                    // Two+ inner subcaptures: first = key, second = value
                    (inner[0].clone(), Some(inner[1].clone()))
                } else if inner.len() == 1 {
                    // One inner subcapture: it is the key, no value
                    (inner[0].clone(), None)
                } else {
                    // No inner subcaptures in subcaps: use matched text
                    let k: String = chars[from..to].iter().collect();
                    (k, None)
                }
            } else {
                // No subcaptures: use matched text
                let k: String = chars[from..to].iter().collect();
                (k, None)
            }
        };
        store.push_hash_capture(name, (key, value));
    }

    /// Fold the quantified capture block (if the atom captures) and descend to
    /// the token after `idx`, rewinding the fold afterwards.
    #[allow(clippy::too_many_arguments)]
    fn descend_folded(
        &self,
        ctx: &WalkCtx,
        idx: usize,
        at: usize,
        base_len: usize,
        stride: usize,
        store: &mut CapStore,
        matches: &mut Vec<(usize, RegexCaptures)>,
    ) -> bool {
        let mf = store.mark();
        if stride > 0 {
            store.fold_quantified(base_len, stride);
        }
        let stop = self.walk_tokens(ctx, idx + 1, at, store, matches);
        store.rewind(mf);
        stop
    }

    /// Depth-first walk from token `idx` at position `pos`. Returns `true`
    /// when the walk should stop (first_only found a match).
    fn walk_tokens(
        &self,
        ctx: &WalkCtx,
        idx: usize,
        pos: usize,
        store: &mut CapStore,
        matches: &mut Vec<(usize, RegexCaptures)>,
    ) -> bool {
        if idx == ctx.pattern.tokens.len() {
            if !ctx.pattern.anchor_end || pos == ctx.chars.len() {
                matches.push((pos, store.snapshot()));
                if ctx.first_only {
                    return true;
                }
            }
            return false;
        }
        let token = &ctx.pattern.tokens[idx];
        let pos_base = store.caps().positional.len();
        // Separator quantifiers (`atom +% sep`, `atom **N..M %% sep`, ...):
        // match the atom with the separator interleaved between iterations,
        // accumulating each side's captures into its own (folded) group.
        if token.separator.is_some() {
            let cands =
                self.match_separated_quantifier(token, ctx.chars, pos, ctx.pkg, ctx.pattern);
            // Candidates come lowest-priority first; try highest first.
            for (next, delta) in cands.into_iter().rev() {
                let m = store.mark();
                store.merge_delta(delta);
                Self::store_apply_named_capture(store, ctx.chars, token, pos, next, pos_base);
                let stop = self.walk_tokens(ctx, idx + 1, next, store, matches);
                store.rewind(m);
                if stop {
                    return true;
                }
            }
            return false;
        }
        match token.quant {
            RegexQuant::One => {
                let mut candidates = self.regex_match_atom_all_with_capture_in_pkg(
                    &token.atom,
                    ctx.chars,
                    pos,
                    store.caps(),
                    ctx.pkg,
                    ctx.pattern.ignore_case,
                );
                if token.ratchet && candidates.len() > 1 {
                    // Ratchet (`:`) commits to the atom's highest-priority match
                    // and forbids backtracking into it. Candidates are returned in
                    // "lowest priority first, highest priority last" order, so the
                    // atom's preferred match is the last element: for `||` it is the
                    // first alternative, for `|`/greedy quantifiers it is the longest
                    // match — both already sit last. Keep only that one. (Sorting by
                    // length here was wrong: it picked the longest match even for `||`,
                    // letting `( ab || abc ): de` backtrack into the group.)
                    candidates.drain(..candidates.len() - 1);
                }
                for (next, delta) in candidates.into_iter().rev() {
                    let m = store.mark();
                    store.merge_delta(delta);
                    Self::store_apply_named_capture(store, ctx.chars, token, pos, next, pos_base);
                    Self::store_apply_hash_capture(store, ctx.chars, token, pos, next, pos_base);
                    let stop = self.walk_tokens(ctx, idx + 1, next, store, matches);
                    store.rewind(m);
                    if stop {
                        return true;
                    }
                }
                false
            }
            RegexQuant::ZeroOrOne => {
                // An unmatched `(x)?` reserves `zo_stride` Nil positional slots
                // so following captures keep their index (`(a)?(b)` → $0=Nil,$1=b).
                let zo_stride = count_capture_groups(&token.atom);
                let mut candidates = self.regex_match_atom_all_with_capture_in_pkg(
                    &token.atom,
                    ctx.chars,
                    pos,
                    store.caps(),
                    ctx.pkg,
                    ctx.pattern.ignore_case,
                );
                if token.ratchet {
                    if candidates.is_empty() {
                        // Atom didn't match — commit to "zero" (no match).
                        let m = store.mark();
                        store.reserve_nil(zo_stride);
                        let stop = self.walk_tokens(ctx, idx + 1, pos, store, matches);
                        store.rewind(m);
                        return stop;
                    }
                    // Atom matched — commit to the highest-priority match.
                    candidates.drain(..candidates.len() - 1);
                } else if token.frugal {
                    // Frugal: prefer zero matches — try zero first.
                    let m = store.mark();
                    store.reserve_nil(zo_stride);
                    let stop = self.walk_tokens(ctx, idx + 1, pos, store, matches);
                    store.rewind(m);
                    if stop {
                        return true;
                    }
                }
                for (next, delta) in candidates.into_iter().rev() {
                    let m = store.mark();
                    store.merge_delta(delta);
                    Self::store_apply_named_capture(store, ctx.chars, token, pos, next, pos_base);
                    Self::store_apply_hash_capture(store, ctx.chars, token, pos, next, pos_base);
                    let stop = self.walk_tokens(ctx, idx + 1, next, store, matches);
                    store.rewind(m);
                    if stop {
                        return true;
                    }
                }
                if !token.ratchet && !token.frugal {
                    // Greedy: the zero candidate is tried last.
                    let m = store.mark();
                    store.reserve_nil(zo_stride);
                    let stop = self.walk_tokens(ctx, idx + 1, pos, store, matches);
                    store.rewind(m);
                    if stop {
                        return true;
                    }
                }
                false
            }
            RegexQuant::ZeroOrMore => {
                if let Some(stop) = self.walk_ratchet_fast_paths(ctx, idx, pos, 0, store, matches) {
                    return stop;
                }
                if !token.ratchet && atom_contains_alternation(&token.atom) {
                    // Full greedy backtracking so a later constraint can
                    // force a shorter per-iteration alternative. Ratchet
                    // skips this: it commits to the per-iteration
                    // highest-priority choice, which is exactly the linear
                    // chain (the expansion would enumerate an exponential
                    // tree only to keep its first leaf).
                    return self.walk_quant_alt(ctx, idx, pos, 0, store, matches);
                }
                self.walk_quant_chain(ctx, idx, pos, 0, None, true, store, matches)
            }
            RegexQuant::OneOrMore => {
                if let Some(stop) = self.walk_ratchet_fast_paths(ctx, idx, pos, 1, store, matches) {
                    return stop;
                }
                if !token.ratchet && atom_contains_alternation(&token.atom) {
                    // Full greedy backtracking (`+` of a variable-length
                    // alternation): explore every per-iteration choice so a
                    // later constraint can force a shorter one.
                    return self.walk_quant_alt(ctx, idx, pos, 1, store, matches);
                }
                self.walk_quant_chain(ctx, idx, pos, 1, None, true, store, matches)
            }
            RegexQuant::Repeat(..) | RegexQuant::RepeatCode(_) => {
                let (min, max) = match &token.quant {
                    RegexQuant::Repeat(min, max) => {
                        // Block-less empty range: /x ** 2..1/ should throw
                        if let Some(max_val) = *max
                            && *min > max_val
                        {
                            Self::set_quantifier_value_error(
                                "empty-range",
                                "Quantifier range is empty",
                            );
                            return false;
                        }
                        (*min, *max)
                    }
                    RegexQuant::RepeatCode(code) => {
                        match self.eval_regex_repeat_code(code, store.caps()) {
                            Some((min, max)) => (min, max),
                            None => return false, // code eval failed, no match
                        }
                    }
                    _ => unreachable!(),
                };
                self.walk_quant_chain(ctx, idx, pos, min, max, false, store, matches)
            }
        }
    }

    /// The three ratcheted `*`/`+` fast paths (simple atom / silent Named /
    /// non-silent Named): possessive linear scans that avoid the general
    /// chain. Returns `None` when no fast path applies.
    fn walk_ratchet_fast_paths(
        &self,
        ctx: &WalkCtx,
        idx: usize,
        pos: usize,
        min: usize,
        store: &mut CapStore,
        matches: &mut Vec<(usize, RegexCaptures)>,
    ) -> Option<bool> {
        let token = &ctx.pattern.tokens[idx];
        if !token.ratchet || token.named_capture.is_some() {
            return None;
        }
        if is_simple_atom(&token.atom) {
            // Position-only scan: no captures are produced at all.
            let mut current = pos;
            let mut count = 0usize;
            while let Some(next) = self.regex_match_atom_in_pkg(
                &token.atom,
                ctx.chars,
                current,
                ctx.pkg,
                ctx.pattern.ignore_case,
            ) {
                if next == current {
                    break;
                }
                current = next;
                count += 1;
            }
            if count < min {
                return Some(false);
            }
            return Some(self.walk_tokens(ctx, idx + 1, current, store, matches));
        }
        if is_silent_named_atom(&token.atom)
            && let Some((resolved, resolved_pkg)) =
                self.try_resolve_named_to_pattern(&token.atom, ctx.pkg)
        {
            // Ratcheted silent Named token (e.g. <.ws>): resolve the pattern
            // once and match directly; silent atoms produce no captures.
            let mut current = pos;
            let mut count = 0usize;
            while current < ctx.chars.len() {
                if let Some(end) =
                    self.regex_match_end_from_in_pkg(&resolved, ctx.chars, current, &resolved_pkg)
                {
                    if end == current {
                        break;
                    }
                    current = end;
                    count += 1;
                } else {
                    break;
                }
            }
            if count < min {
                return Some(false);
            }
            return Some(self.walk_tokens(ctx, idx + 1, current, store, matches));
        }
        if is_named_atom_no_args(&token.atom)
            && let Some((resolved, resolved_pkg)) =
                self.try_resolve_named_to_pattern(&token.atom, ctx.pkg)
        {
            // Ratcheted non-silent Named token (e.g. <huge>*): resolve the
            // pattern once and loop directly, accumulating named captures on
            // the store without re-parsing per iteration.
            let capture_name = if let RegexAtom::Named(name) = &token.atom {
                name.trim().to_string()
            } else {
                String::new()
            };
            let m = store.mark();
            if !capture_name.is_empty() {
                store.insert_named_quantified(capture_name.clone());
            }
            let mut current = pos;
            let mut count = 0usize;
            while current <= ctx.chars.len() {
                let Some((end, inner_caps)) = self.regex_match_end_from_caps_in_pkg(
                    &resolved,
                    ctx.chars,
                    current,
                    &resolved_pkg,
                ) else {
                    break;
                };
                if end == current {
                    break;
                }
                if !capture_name.is_empty() {
                    let captured: String = ctx.chars[current..end].iter().collect();
                    let mut subcap = inner_caps;
                    subcap.matched = captured.clone();
                    subcap.from = current;
                    subcap.to = end;
                    store.push_named_subcap(&capture_name, std::sync::Arc::new(subcap));
                    store.push_named(&capture_name, captured);
                }
                current = end;
                count += 1;
                if current >= ctx.chars.len() {
                    break;
                }
            }
            if count < min {
                store.rewind(m);
                return Some(false);
            }
            let stop = self.walk_tokens(ctx, idx + 1, current, store, matches);
            store.rewind(m);
            return Some(stop);
        }
        None
    }

    /// General chain quantifier (`*`, `+`, `**min..max`) over a single-match
    /// atom: grow the iteration chain on the store (one mark per iteration),
    /// then descend longest-first (greedy) / shortest-first (frugal) /
    /// longest-only (ratchet). `hash_per_iter` selects the `%<h>=` hash
    /// capture base: per-iteration for `*`/`+` (which also run reduce-time
    /// grammar actions per iteration), token-start for `**`.
    #[allow(clippy::too_many_arguments)]
    fn walk_quant_chain(
        &self,
        ctx: &WalkCtx,
        idx: usize,
        pos: usize,
        min: usize,
        max: Option<usize>,
        hash_per_iter: bool,
        store: &mut CapStore,
        matches: &mut Vec<(usize, RegexCaptures)>,
    ) -> bool {
        let token = &ctx.pattern.tokens[idx];
        let pos_base = store.caps().positional.len();
        let stride = count_capture_groups(&token.atom);
        let m_quant = store.mark();
        for n in Self::collect_quantified_names_for_token(token) {
            store.insert_named_quantified(n);
        }
        let grow_one = |store: &mut CapStore, current: usize| -> Option<usize> {
            let iter_pos_base = store.caps().positional.len();
            let (next, delta) = self.regex_match_atom_with_capture_in_pkg(
                &token.atom,
                ctx.chars,
                current,
                store.caps(),
                ctx.pkg,
                ctx.pattern.ignore_case,
            )?;
            if next == current {
                return None;
            }
            store.merge_delta(delta);
            Self::store_apply_named_capture(store, ctx.chars, token, current, next, pos_base);
            let hash_base = if hash_per_iter {
                iter_pos_base
            } else {
                pos_base
            };
            Self::store_apply_hash_capture(store, ctx.chars, token, current, next, hash_base);
            if hash_per_iter {
                // Reduce-time grammar action: when a `<subrule>` quantifier
                // iteration commits inside an action-driven parse whose
                // matching depends on a `$*` dynamic var, run this iteration's
                // subrule action now so any dyn-var write (e.g. a delimiter
                // finalizer) is visible to the next iteration's pattern
                // interpolation. Gated on the SEEN flag so plain grammars pay
                // nothing.
                self.maybe_run_reduce_time_dynvar_action(token, store.caps());
            }
            Some(next)
        };
        if token.frugal && !token.ratchet {
            // Frugal: try the shortest admissible length first, growing the
            // chain one iteration at a time on demand.
            let mut current = pos;
            let mut count = 0usize;
            loop {
                if count >= min
                    && self.descend_folded(ctx, idx, current, pos_base, stride, store, matches)
                {
                    store.rewind(m_quant);
                    return true;
                }
                if max.is_some_and(|mx| count >= mx) {
                    break;
                }
                let Some(next) = grow_one(store, current) else {
                    break;
                };
                count += 1;
                current = next;
            }
            store.rewind(m_quant);
            return false;
        }
        // Greedy / ratchet: grow the full chain first, then descend.
        let mut ends = vec![pos];
        let mut iter_marks = vec![store.mark()];
        let mut current = pos;
        while max.is_none_or(|mx| ends.len() - 1 < mx) {
            let Some(next) = grow_one(store, current) else {
                break;
            };
            ends.push(next);
            iter_marks.push(store.mark());
            current = next;
        }
        let count = ends.len() - 1;
        if count < min {
            store.rewind(m_quant);
            return false;
        }
        // Ratchet commits to the longest chain; greedy backtracks toward min.
        let lo = if token.ratchet { count } else { min };
        for i in (lo..=count).rev() {
            store.rewind(iter_marks[i]);
            if self.descend_folded(ctx, idx, ends[i], pos_base, stride, store, matches) {
                store.rewind(m_quant);
                return true;
            }
        }
        store.rewind(m_quant);
        false
    }

    /// Full-backtracking quantifier over a variable-length alternation atom
    /// (`(a | b | bc | cde)+»`): explores every per-iteration alternative so a
    /// later constraint can force a shorter choice. Greedy priority: at each
    /// position the highest-priority atom match is tried first, and within a
    /// match *more* iterations (deeper) outrank stopping there. Frugal is the
    /// exact mirror. Bounded to avoid catastrophic backtracking.
    fn walk_quant_alt(
        &self,
        ctx: &WalkCtx,
        idx: usize,
        pos: usize,
        min: usize,
        store: &mut CapStore,
        matches: &mut Vec<(usize, RegexCaptures)>,
    ) -> bool {
        let token = &ctx.pattern.tokens[idx];
        let pos_base = store.caps().positional.len();
        let stride = count_capture_groups(&token.atom);
        let m_quant = store.mark();
        for n in Self::collect_quantified_names_for_token(token) {
            store.insert_named_quantified(n);
        }
        let mut budget = QUANT_ALT_BUDGET;
        let stop = if token.frugal {
            // Frugal: zero iterations first, then shallow-before-deep.
            (min == 0 && self.descend_folded(ctx, idx, pos, pos_base, stride, store, matches))
                || self.quant_alt_dfs(
                    ctx,
                    idx,
                    pos,
                    0,
                    min,
                    pos_base,
                    stride,
                    true,
                    &mut budget,
                    store,
                    matches,
                )
        } else {
            // Greedy: deep-before-shallow, zero iterations last.
            self.quant_alt_dfs(
                ctx,
                idx,
                pos,
                0,
                min,
                pos_base,
                stride,
                false,
                &mut budget,
                store,
                matches,
            ) || (min == 0 && self.descend_folded(ctx, idx, pos, pos_base, stride, store, matches))
        };
        store.rewind(m_quant);
        stop
    }

    /// DFS worker for `walk_quant_alt`. At each node, applies one atom
    /// candidate, then (greedy) recurses deeper before descending past the
    /// quantifier at this length — or the mirror order for frugal.
    #[allow(clippy::too_many_arguments)]
    fn quant_alt_dfs(
        &self,
        ctx: &WalkCtx,
        idx: usize,
        current: usize,
        count: usize,
        min: usize,
        pos_base: usize,
        stride: usize,
        frugal: bool,
        budget: &mut u32,
        store: &mut CapStore,
        matches: &mut Vec<(usize, RegexCaptures)>,
    ) -> bool {
        if *budget == 0 {
            return false;
        }
        let token = &ctx.pattern.tokens[idx];
        let cands = self.regex_match_atom_all_with_capture_in_pkg(
            &token.atom,
            ctx.chars,
            current,
            store.caps(),
            ctx.pkg,
            ctx.pattern.ignore_case,
        );
        // Candidates come lowest-priority first: greedy iterates highest
        // first, frugal keeps the producer order.
        let iter: Box<dyn Iterator<Item = (usize, RegexCaptures)>> = if frugal {
            Box::new(cands.into_iter())
        } else {
            Box::new(cands.into_iter().rev())
        };
        for (next, delta) in iter {
            if next == current {
                continue; // zero-width: would loop forever
            }
            if *budget == 0 {
                return false;
            }
            *budget -= 1;
            let iter_pos_base = store.caps().positional.len();
            let m = store.mark();
            store.merge_delta(delta);
            Self::store_apply_named_capture(store, ctx.chars, token, current, next, pos_base);
            Self::store_apply_hash_capture(store, ctx.chars, token, current, next, iter_pos_base);
            // Greedy: recurse deeper first, then stop at this length.
            // Frugal: the mirror — stop here first, then grow deeper.
            let mut stop = false;
            let order: [bool; 2] = if frugal { [true, false] } else { [false, true] };
            for stop_here in order {
                stop = if stop_here {
                    count + 1 >= min
                        && self.descend_folded(ctx, idx, next, pos_base, stride, store, matches)
                } else {
                    self.quant_alt_dfs(
                        ctx,
                        idx,
                        next,
                        count + 1,
                        min,
                        pos_base,
                        stride,
                        frugal,
                        budget,
                        store,
                        matches,
                    )
                };
                if stop {
                    break;
                }
            }
            store.rewind(m);
            if stop {
                return true;
            }
        }
        false
    }
}
