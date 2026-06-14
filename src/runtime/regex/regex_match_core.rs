use super::super::*;
use super::regex_helpers::{
    count_capture_groups, fold_quantified_captures, is_named_atom_no_args, is_silent_named_atom,
    is_simple_atom,
};
use std::collections::HashSet;

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
    fn collect_quantified_names_for_token(token: &RegexToken) -> HashSet<String> {
        let mut names = HashSet::new();
        if let Some(name) = token.named_capture.as_ref() {
            names.insert(name.clone());
        }
        Self::collect_named_captures_in_atom(&token.atom, &mut names);
        names
    }
    /// Match a separator quantifier (`atom +% sep`, `atom **N..M %% sep`, ...).
    /// The atom is matched repeatedly with `sep` interleaved between iterations.
    /// Each side's captures are accumulated into its own folded group: the atom's
    /// positional captures occupy the first `atom_stride` indices and the
    /// separator's the next `sep_stride` indices, matching Raku's Match layout.
    ///
    /// Returns `(end, caps)` pairs in LOWEST-priority-first order (for the LIFO
    /// matching stack): shortest match first, longest (greedy) last.
    fn match_separated_quantifier(
        &self,
        token: &RegexToken,
        chars: &[char],
        start: usize,
        base_caps: &RegexCaptures,
        pkg: &str,
        pattern: &RegexPattern,
    ) -> Vec<(usize, RegexCaptures)> {
        let sep = token.separator.as_ref().expect("separator present");
        let (min, max) = match &token.quant {
            RegexQuant::OneOrMore => (1usize, None),
            RegexQuant::ZeroOrMore => (0usize, None),
            RegexQuant::Repeat(lo, hi) => (*lo, *hi),
            // `?` / exact-one don't form a separator list; treat as one.
            _ => (1usize, Some(1usize)),
        };
        let atom_stride = count_capture_groups(&token.atom);
        let sep_stride: usize = sep
            .pattern
            .tokens
            .iter()
            .map(|t| count_capture_groups(&t.atom))
            .sum();
        let names = Self::collect_quantified_names_for_token(token);

        // Build the greedy chain of iterations. Each iteration records the atom
        // match end and the captures produced by the atom (and, for iterations
        // after the first, the preceding separator's captures).
        // `atom_ends[i]` = end position after the i-th atom (0-indexed).
        let mut atom_caps: Vec<RegexCaptures> = Vec::new();
        let mut sep_caps: Vec<RegexCaptures> = Vec::new();
        let mut atom_ends: Vec<usize> = Vec::new();

        let empty = RegexCaptures::default();
        // First atom.
        let mut cur = start;
        if let Some((end, caps)) = self.regex_match_atom_with_capture_in_pkg(
            &token.atom,
            chars,
            cur,
            &empty,
            pkg,
            pattern.ignore_case,
        ) && end != cur
        {
            atom_caps.push(caps);
            atom_ends.push(end);
            cur = end;
            // Subsequent: sep then atom.
            loop {
                if let Some(m) = max
                    && atom_ends.len() >= m
                {
                    break;
                }
                let Some((sep_end, scaps)) =
                    self.regex_match_end_from_caps_in_pkg(&sep.pattern, chars, cur, pkg)
                else {
                    break;
                };
                let Some((atom_end, acaps)) = self.regex_match_atom_with_capture_in_pkg(
                    &token.atom,
                    chars,
                    sep_end,
                    &empty,
                    pkg,
                    pattern.ignore_case,
                ) else {
                    break;
                };
                if atom_end == cur {
                    break; // zero-width progress guard
                }
                sep_caps.push(scaps);
                atom_caps.push(acaps);
                atom_ends.push(atom_end);
                cur = atom_end;
            }
        }

        // Produce results for each valid iteration count (>= min, <= max),
        // shortest first. Count 0 (when min==0) ends at `start`.
        let mut results: Vec<(usize, RegexCaptures)> = Vec::new();
        if min == 0 {
            results.push((start, base_caps.clone()));
        }
        for count in 1..=atom_ends.len() {
            if count < min {
                continue;
            }
            if let Some(m) = max
                && count > m
            {
                break;
            }
            let mut end = atom_ends[count - 1];
            let mut caps = base_caps.clone();
            // Optional trailing separator for `%%`. A zero-width trailing
            // separator (e.g. `delim*` matching empty) is still a valid trailing
            // match, so accept `sep_end >= end`.
            let mut trailing_sep: Option<RegexCaptures> = None;
            if sep.allow_trailing
                && let Some((sep_end, scaps)) =
                    self.regex_match_end_from_caps_in_pkg(&sep.pattern, chars, end, pkg)
                && sep_end >= end
            {
                trailing_sep = Some(scaps);
                end = sep_end;
            }
            // Assemble folded captures: atom groups first, then sep groups.
            caps.named_quantified.extend(names.iter().cloned());
            Self::append_separated_captures(
                &mut caps,
                &atom_caps[..count],
                &sep_caps[..count.saturating_sub(1)],
                trailing_sep.as_ref(),
                atom_stride,
                sep_stride,
            );
            results.push((end, caps));
        }
        results
    }

    /// Append captures from a separated quantifier into `caps`, folding each
    /// side into its own positional/named group lists.
    fn append_separated_captures(
        caps: &mut RegexCaptures,
        atom_caps: &[RegexCaptures],
        sep_caps: &[RegexCaptures],
        trailing_sep: Option<&RegexCaptures>,
        atom_stride: usize,
        sep_stride: usize,
    ) {
        // Positional captures: atom groups occupy the first `atom_stride` slots,
        // separator groups the next `sep_stride`.
        for g in 0..atom_stride {
            let mut list: Vec<QuantifiedCaptureEntry> = Vec::new();
            let mut last_text = String::new();
            let mut last_sub: Option<RegexCaptures> = None;
            for ac in atom_caps {
                if let Some(text) = ac.positional.get(g) {
                    let (from, to) = ac.positional_offsets.get(g).copied().unwrap_or((0, 0));
                    let sub = ac.positional_subcaps.get(g).cloned().flatten();
                    list.push((text.clone(), from, to, sub.clone()));
                    last_text = text.clone();
                    last_sub = sub;
                }
            }
            caps.positional.push(last_text);
            caps.positional_subcaps.push(last_sub);
            caps.positional_quantified.push(Some(list));
            caps.positional_offsets.push((0, 0));
        }
        let mut all_sep: Vec<&RegexCaptures> = sep_caps.iter().collect();
        if let Some(ts) = trailing_sep {
            all_sep.push(ts);
        }
        for g in 0..sep_stride {
            let mut list: Vec<QuantifiedCaptureEntry> = Vec::new();
            let mut last_text = String::new();
            let mut last_sub: Option<RegexCaptures> = None;
            for sc in &all_sep {
                if let Some(text) = sc.positional.get(g) {
                    let (from, to) = sc.positional_offsets.get(g).copied().unwrap_or((0, 0));
                    let sub = sc.positional_subcaps.get(g).cloned().flatten();
                    list.push((text.clone(), from, to, sub.clone()));
                    last_text = text.clone();
                    last_sub = sub;
                }
            }
            caps.positional.push(last_text);
            caps.positional_subcaps.push(last_sub);
            caps.positional_quantified.push(Some(list));
            caps.positional_offsets.push((0, 0));
        }
        // Named captures: merge every iteration's named captures (as arrays).
        for src in atom_caps.iter().chain(all_sep.iter().copied()) {
            for (k, v) in &src.named {
                caps.named.entry(k.clone()).or_default().extend(v.clone());
                caps.named_quantified.insert(k.clone());
            }
            for (k, v) in &src.named_subcaps {
                caps.named_subcaps
                    .entry(k.clone())
                    .or_default()
                    .extend(v.clone());
            }
            for (k, v) in &src.hash_captures {
                caps.hash_captures
                    .entry(k.clone())
                    .or_default()
                    .extend(v.clone());
            }
        }
    }

    pub(super) fn regex_match_end_from_caps_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> Option<(usize, RegexCaptures)> {
        // Only the first (highest-priority / greedy) complete match is needed
        // here, and the backtracking DFS already discovers it first (matches are
        // returned in DFS-completion order, unsorted), so stop as soon as one is
        // found instead of exploring the whole backtracking tree. `matches[0]` is
        // byte-identical to the all-ends result's first element, so this is
        // semantics-preserving — it just skips work the `.next()` would discard.
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
    /// When `first_only` is true the DFS stops after the first complete match —
    /// `matches[0]` is unchanged, so callers that only take `.next()` get the
    /// same answer for far less work.
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
        let apply_named_capture = |token: &RegexToken,
                                   from: usize,
                                   to: usize,
                                   pos_base: usize,
                                   caps: RegexCaptures|
         -> RegexCaptures {
            let Some(name) = token.named_capture.as_ref() else {
                return caps;
            };
            // Numbered scalar capture alias `$N=<atom>`: the alias name is all
            // digits, so route the capture to positional index N (padding lower
            // indices) and let subsequent groups continue numbering from N+1.
            // (Named captures always start with a letter/underscore, so an
            // all-digit name can only come from `$N=`.)
            if let Ok(forced_idx) = name.parse::<usize>() {
                let mut updated = caps;
                let captured: String = chars[from..to].iter().collect();
                // Drop the auto-positional entry a capturing group atom produced;
                // the alias decides this capture's index explicitly.
                if matches!(token.atom, RegexAtom::CaptureGroup(_))
                    && updated.positional.len() > pos_base
                {
                    updated.positional.truncate(pos_base);
                    updated.positional_subcaps.truncate(pos_base);
                    updated.positional_quantified.truncate(pos_base);
                    updated.positional_offsets.truncate(pos_base);
                }
                while updated.positional.len() < forced_idx {
                    updated.positional.push(String::new());
                    updated.positional_subcaps.push(None);
                    updated.positional_quantified.push(None);
                    updated.positional_offsets.push((0, 0));
                }
                if updated.positional.len() == forced_idx {
                    updated.positional.push(captured);
                    updated.positional_subcaps.push(None);
                    updated.positional_quantified.push(None);
                    updated.positional_offsets.push((from, to));
                } else {
                    updated.positional[forced_idx] = captured;
                    if forced_idx < updated.positional_offsets.len() {
                        updated.positional_offsets[forced_idx] = (from, to);
                    }
                    if forced_idx < updated.positional_subcaps.len() {
                        updated.positional_subcaps[forced_idx] = None;
                    }
                    if forced_idx < updated.positional_quantified.len() {
                        updated.positional_quantified[forced_idx] = None;
                    }
                }
                return updated;
            }
            let mut updated = caps;
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
            let mut group_subcap: Option<RegexCaptures> = None;
            if matches!(token.atom, RegexAtom::CaptureGroup(_))
                && updated.positional.len() > pos_base
            {
                group_subcap = updated.positional_subcaps.get(pos_base).cloned().flatten();
                updated.positional.truncate(pos_base);
                updated.positional_subcaps.truncate(pos_base);
                updated.positional_quantified.truncate(pos_base);
            }
            updated
                .named
                .entry(name.clone())
                .or_default()
                .push(captured.clone());
            // Record a minimal sub-capture carrying the exact (from, to) span so
            // the Match object gets the right offsets even for a zero-width match
            // (e.g. `$<delim>=<[a..z]>*` matching empty). Without this the Match
            // builder falls back to searching for the captured text, which yields
            // offset 0 for an empty string and breaks `.caps`/`.chunks` ordering.
            // Only do this when no richer sub-capture already aligns with this
            // entry, so subrule-aliased captures keep their nested structure.
            let name_count = updated.named.get(name).map(Vec::len).unwrap_or(0);
            let subcaps = updated.named_subcaps.entry(name.clone()).or_default();
            if subcaps.len() < name_count {
                if let Some(mut gs) = group_subcap.take() {
                    // Keep the group's nested captures, but pin the span/text to
                    // the aliased group's extent.
                    gs.from = from;
                    gs.to = to;
                    gs.match_from = from;
                    gs.matched = captured.clone();
                    subcaps.push(gs);
                } else {
                    subcaps.push(RegexCaptures {
                        from,
                        to,
                        matched: captured.clone(),
                        match_from: from,
                        ..Default::default()
                    });
                }
            }
            // Also capture under the secondary name (e.g., original builtin class name
            // when using `$<alias>=<builtin_class>` syntax).
            if let Some(secondary) = token.secondary_named_capture.as_ref() {
                updated
                    .named
                    .entry(secondary.clone())
                    .or_default()
                    .push(captured);
            }
            updated
        };
        let apply_hash_capture = |token: &RegexToken,
                                  _from: usize,
                                  _to: usize,
                                  pos_base: usize,
                                  caps: RegexCaptures|
         -> RegexCaptures {
            let Some(name) = token.hash_capture.as_ref() else {
                return caps;
            };
            let mut updated = caps;
            // Count how many new positional captures this atom produced
            let new_count = updated.positional.len().saturating_sub(pos_base);
            // Look for inner subcaptures in positional_subcaps
            let subcap_idx = if new_count >= 1 {
                pos_base
            } else {
                updated.positional_subcaps.len()
            };
            let inner_positionals = if subcap_idx < updated.positional_subcaps.len() {
                updated.positional_subcaps[subcap_idx]
                    .as_ref()
                    .map(|sc| &sc.positional)
            } else {
                None
            };
            let (key, value) = if let Some(inner) = inner_positionals {
                if inner.len() >= 2 {
                    // Two+ inner subcaptures: first = key, second = value
                    (inner[0].clone(), Some(inner[1].clone()))
                } else if inner.len() == 1 {
                    // One inner subcapture: it is the key, no value
                    (inner[0].clone(), None)
                } else {
                    // No inner subcaptures in subcaps: use matched text
                    let k: String = chars[_from.._to].iter().collect();
                    (k, None)
                }
            } else {
                // No subcaptures: use matched text
                let k: String = chars[_from.._to].iter().collect();
                (k, None)
            };
            updated
                .hash_captures
                .entry(name.clone())
                .or_default()
                .push((key, value));
            updated
        };
        let mut stack = Vec::new();
        let init_caps = RegexCaptures {
            match_from: start,
            ..Default::default()
        };
        stack.push((0usize, start, init_caps));
        let mut matches = Vec::new();
        while let Some((idx, pos, caps)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if !pattern.anchor_end || pos == chars.len() {
                    matches.push((pos, caps));
                    // Single-match callers (`regex_match_end_from_caps_in_pkg`)
                    // only use `matches[0]`; the DFS visits highest priority
                    // first, so the first complete match IS that element — stop
                    // exploring the rest of the backtracking tree.
                    if first_only {
                        break;
                    }
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            let pos_base = caps.positional.len();
            // Separator quantifiers (`atom +% sep`, `atom **N..M %% sep`, ...):
            // match the atom with the separator interleaved between iterations,
            // accumulating each side's captures into its own (folded) group.
            if token.separator.is_some() {
                for (next, new_caps) in
                    self.match_separated_quantifier(token, chars, pos, &caps, pkg, pattern)
                {
                    stack.push((
                        idx + 1,
                        next,
                        apply_named_capture(token, pos, next, pos_base, new_caps),
                    ));
                }
                continue;
            }
            match token.quant {
                RegexQuant::One => {
                    let mut candidates = self.regex_match_atom_all_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        &caps,
                        pkg,
                        pattern.ignore_case,
                    );
                    if token.ratchet {
                        // Ratchet (`:`) commits to the atom's highest-priority match
                        // and forbids backtracking into it. Candidates are returned in
                        // "lowest priority first, highest priority last" order, so the
                        // atom's preferred match is the last element: for `||` it is the
                        // first alternative, for `|`/greedy quantifiers it is the longest
                        // match — both already sit last. Keep only that one. (Sorting by
                        // length here was wrong: it picked the longest match even for `||`,
                        // letting `( ab || abc ): de` backtrack into the group.)
                        if let Some(best) = candidates.pop() {
                            candidates = vec![best];
                        }
                    }
                    for (next, new_caps) in candidates {
                        stack.push((
                            idx + 1,
                            next,
                            apply_hash_capture(
                                token,
                                pos,
                                next,
                                pos_base,
                                apply_named_capture(token, pos, next, pos_base, new_caps),
                            ),
                        ));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    let mut candidates = self.regex_match_atom_all_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        &caps,
                        pkg,
                        pattern.ignore_case,
                    );
                    if token.ratchet {
                        // Same as the `One` case: commit to the atom's highest-priority
                        // match (the last candidate), not the longest one.
                        if let Some(best) = candidates.pop() {
                            // Atom matched — commit to the match (no backtracking)
                            candidates = vec![best];
                        } else {
                            // Atom didn't match — commit to "zero" (no match)
                            stack.push((idx + 1, pos, caps.clone()));
                            candidates.clear();
                        }
                    } else if token.frugal {
                        // Frugal: prefer zero matches — push match first (low priority),
                        // then zero (high priority, tried first from LIFO stack).
                        for (next, new_caps) in &candidates {
                            stack.push((
                                idx + 1,
                                *next,
                                apply_hash_capture(
                                    token,
                                    pos,
                                    *next,
                                    pos_base,
                                    apply_named_capture(
                                        token,
                                        pos,
                                        *next,
                                        pos_base,
                                        new_caps.clone(),
                                    ),
                                ),
                            ));
                        }
                        stack.push((idx + 1, pos, caps.clone()));
                        candidates.clear();
                    } else {
                        stack.push((idx + 1, pos, caps.clone()));
                    }
                    for (next, new_caps) in candidates {
                        stack.push((
                            idx + 1,
                            next,
                            apply_hash_capture(
                                token,
                                pos,
                                next,
                                pos_base,
                                apply_named_capture(token, pos, next, pos_base, new_caps),
                            ),
                        ));
                    }
                }
                RegexQuant::ZeroOrMore => {
                    // Fast path: ratcheted simple atom with no named capture
                    // avoids all RegexCaptures cloning by only tracking position.
                    if token.ratchet && token.named_capture.is_none() && is_simple_atom(&token.atom)
                    {
                        let mut current = pos;
                        while let Some(next) = self.regex_match_atom_in_pkg(
                            &token.atom,
                            chars,
                            current,
                            pkg,
                            pattern.ignore_case,
                        ) {
                            if next == current {
                                break;
                            }
                            current = next;
                        }
                        stack.push((idx + 1, current, caps));
                    } else if token.ratchet
                        && token.named_capture.is_none()
                        && is_silent_named_atom(&token.atom)
                        && let Some((resolved, resolved_pkg)) =
                            self.try_resolve_named_to_pattern(&token.atom, pkg)
                    {
                        // Fast path for ratcheted Named token: resolve pattern once,
                        // match directly without creating new Interpreter per iteration.
                        // Only used for silent atoms (e.g. <.ws>) that don't produce
                        // implicit named captures.
                        let mut current = pos;
                        while current < chars.len() {
                            if let Some(end) = self.regex_match_end_from_in_pkg(
                                &resolved,
                                chars,
                                current,
                                &resolved_pkg,
                            ) {
                                if end == current {
                                    break;
                                }
                                current = end;
                            } else {
                                break;
                            }
                        }
                        stack.push((idx + 1, current, caps));
                    } else if token.ratchet
                        && token.named_capture.is_none()
                        && is_named_atom_no_args(&token.atom)
                        && let Some((resolved, resolved_pkg)) =
                            self.try_resolve_named_to_pattern(&token.atom, pkg)
                    {
                        // Fast path for ratcheted non-silent Named token (e.g. <huge>*).
                        // Resolve the pattern once and loop directly, accumulating
                        // named captures without re-parsing on each iteration.
                        let capture_name = if let RegexAtom::Named(name) = &token.atom {
                            name.trim().to_string()
                        } else {
                            String::new()
                        };
                        let mut current = pos;
                        let mut current_caps = caps;
                        if !capture_name.is_empty() {
                            current_caps.named_quantified.insert(capture_name.clone());
                        }
                        while current < chars.len() {
                            if let Some((end, inner_caps)) = self.regex_match_end_from_caps_in_pkg(
                                &resolved,
                                chars,
                                current,
                                &resolved_pkg,
                            ) {
                                if end == current {
                                    break;
                                }
                                if !capture_name.is_empty() {
                                    let captured: String = chars[current..end].iter().collect();
                                    let mut subcap = inner_caps;
                                    subcap.matched = captured.clone();
                                    subcap.from = current;
                                    subcap.to = end;
                                    current_caps
                                        .named_subcaps
                                        .entry(capture_name.clone())
                                        .or_default()
                                        .push(subcap);
                                    current_caps
                                        .named
                                        .entry(capture_name.clone())
                                        .or_default()
                                        .push(captured);
                                }
                                current = end;
                            } else {
                                break;
                            }
                        }
                        stack.push((idx + 1, current, current_caps));
                    } else {
                        let base_len = caps.positional.len();
                        let stride = count_capture_groups(&token.atom);
                        let mut caps = caps;
                        caps.named_quantified
                            .extend(Self::collect_quantified_names_for_token(token));
                        let mut positions = Vec::new();
                        positions.push((pos, caps.clone()));
                        let mut current = pos;
                        let mut current_caps = caps.clone();
                        while let Some((next, new_caps)) = self
                            .regex_match_atom_with_capture_in_pkg(
                                &token.atom,
                                chars,
                                current,
                                &current_caps,
                                pkg,
                                pattern.ignore_case,
                            )
                        {
                            if next == current {
                                break;
                            }
                            let iter_pos_base = current_caps.positional.len();
                            let new_caps = apply_hash_capture(
                                token,
                                current,
                                next,
                                iter_pos_base,
                                apply_named_capture(token, current, next, pos_base, new_caps),
                            );
                            current_caps = new_caps.clone();
                            positions.push((next, new_caps));
                            current = next;
                        }
                        if token.ratchet
                            && let Some(last) = positions.last().cloned()
                        {
                            positions = vec![last];
                        }
                        // Frugal: reverse so shortest match is tried first (LIFO)
                        if token.frugal {
                            positions.reverse();
                        }
                        if stride > 0 {
                            for (p, c) in positions {
                                let mut c = c;
                                fold_quantified_captures(&mut c, base_len, stride);
                                stack.push((idx + 1, p, c));
                            }
                        } else {
                            for (p, c) in positions {
                                stack.push((idx + 1, p, c));
                            }
                        }
                    }
                }
                RegexQuant::OneOrMore => {
                    // Fast path: ratcheted simple atom with no named capture
                    if token.ratchet && token.named_capture.is_none() && is_simple_atom(&token.atom)
                    {
                        let Some(mut current) = self.regex_match_atom_in_pkg(
                            &token.atom,
                            chars,
                            pos,
                            pkg,
                            pattern.ignore_case,
                        ) else {
                            continue;
                        };
                        while let Some(next) = self.regex_match_atom_in_pkg(
                            &token.atom,
                            chars,
                            current,
                            pkg,
                            pattern.ignore_case,
                        ) {
                            if next == current {
                                break;
                            }
                            current = next;
                        }
                        stack.push((idx + 1, current, caps));
                    } else if token.ratchet
                        && token.named_capture.is_none()
                        && is_silent_named_atom(&token.atom)
                        && let Some((resolved, resolved_pkg)) =
                            self.try_resolve_named_to_pattern(&token.atom, pkg)
                    {
                        // Fast path for ratcheted Named token.
                        // Only used for silent atoms (e.g. <.ws>) that don't produce
                        // implicit named captures.
                        let Some(mut current) =
                            self.regex_match_end_from_in_pkg(&resolved, chars, pos, &resolved_pkg)
                        else {
                            continue;
                        };
                        while current < chars.len() {
                            if let Some(end) = self.regex_match_end_from_in_pkg(
                                &resolved,
                                chars,
                                current,
                                &resolved_pkg,
                            ) {
                                if end == current {
                                    break;
                                }
                                current = end;
                            } else {
                                break;
                            }
                        }
                        stack.push((idx + 1, current, caps));
                    } else if token.ratchet
                        && token.named_capture.is_none()
                        && is_named_atom_no_args(&token.atom)
                        && let Some((resolved, resolved_pkg)) =
                            self.try_resolve_named_to_pattern(&token.atom, pkg)
                    {
                        // Fast path for ratcheted non-silent Named token (e.g. <huge>+).
                        // Resolve the pattern once and loop directly.
                        let capture_name = if let RegexAtom::Named(name) = &token.atom {
                            name.trim().to_string()
                        } else {
                            String::new()
                        };
                        let Some((first_end, first_inner)) = self.regex_match_end_from_caps_in_pkg(
                            &resolved,
                            chars,
                            pos,
                            &resolved_pkg,
                        ) else {
                            continue;
                        };
                        let mut current = first_end;
                        let mut current_caps = caps;
                        if !capture_name.is_empty() {
                            current_caps.named_quantified.insert(capture_name.clone());
                            let captured: String = chars[pos..first_end].iter().collect();
                            let mut subcap = first_inner;
                            subcap.matched = captured.clone();
                            subcap.from = pos;
                            subcap.to = first_end;
                            current_caps
                                .named_subcaps
                                .entry(capture_name.clone())
                                .or_default()
                                .push(subcap);
                            current_caps
                                .named
                                .entry(capture_name.clone())
                                .or_default()
                                .push(captured);
                        }
                        while current < chars.len() {
                            if let Some((end, inner_caps)) = self.regex_match_end_from_caps_in_pkg(
                                &resolved,
                                chars,
                                current,
                                &resolved_pkg,
                            ) {
                                if end == current {
                                    break;
                                }
                                if !capture_name.is_empty() {
                                    let captured: String = chars[current..end].iter().collect();
                                    let mut subcap = inner_caps;
                                    subcap.matched = captured.clone();
                                    subcap.from = current;
                                    subcap.to = end;
                                    current_caps
                                        .named_subcaps
                                        .entry(capture_name.clone())
                                        .or_default()
                                        .push(subcap);
                                    current_caps
                                        .named
                                        .entry(capture_name.clone())
                                        .or_default()
                                        .push(captured);
                                }
                                current = end;
                            } else {
                                break;
                            }
                        }
                        stack.push((idx + 1, current, current_caps));
                    } else {
                        let base_len = caps.positional.len();
                        let stride = count_capture_groups(&token.atom);
                        let mut caps = caps;
                        caps.named_quantified
                            .extend(Self::collect_quantified_names_for_token(token));
                        let (mut current, mut current_caps) = match self
                            .regex_match_atom_with_capture_in_pkg(
                                &token.atom,
                                chars,
                                pos,
                                &caps,
                                pkg,
                                pattern.ignore_case,
                            ) {
                            Some((next, new_caps)) => {
                                let new_caps = apply_hash_capture(
                                    token,
                                    pos,
                                    next,
                                    pos_base,
                                    apply_named_capture(token, pos, next, pos_base, new_caps),
                                );
                                (next, new_caps)
                            }
                            None => continue,
                        };
                        let mut positions = Vec::new();
                        positions.push((current, current_caps.clone()));
                        while let Some((next, new_caps)) = self
                            .regex_match_atom_with_capture_in_pkg(
                                &token.atom,
                                chars,
                                current,
                                &current_caps,
                                pkg,
                                pattern.ignore_case,
                            )
                        {
                            if next == current {
                                break;
                            }
                            let iter_pos_base = current_caps.positional.len();
                            let new_caps = apply_hash_capture(
                                token,
                                current,
                                next,
                                iter_pos_base,
                                apply_named_capture(token, current, next, pos_base, new_caps),
                            );
                            current_caps = new_caps.clone();
                            positions.push((next, new_caps));
                            current = next;
                        }
                        if token.ratchet
                            && let Some(last) = positions.last().cloned()
                        {
                            positions = vec![last];
                        }
                        // Frugal: reverse so shortest match is tried first (LIFO)
                        if token.frugal {
                            positions.reverse();
                        }
                        // Fold quantified captures for each position
                        if stride > 0 {
                            for (p, c) in positions {
                                let mut c = c;
                                fold_quantified_captures(&mut c, base_len, stride);
                                stack.push((idx + 1, p, c));
                            }
                        } else {
                            for (p, c) in positions {
                                stack.push((idx + 1, p, c));
                            }
                        }
                    }
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
                                continue;
                            }
                            (*min, *max)
                        }
                        RegexQuant::RepeatCode(code) => {
                            match self.eval_regex_repeat_code(code, &caps) {
                                Some((min, max)) => (min, max),
                                None => continue, // code eval failed, no match
                            }
                        }
                        _ => unreachable!(),
                    };
                    let base_len = caps.positional.len();
                    let stride = count_capture_groups(&token.atom);
                    let mut caps = caps;
                    caps.named_quantified
                        .extend(Self::collect_quantified_names_for_token(token));
                    let mut positions = Vec::new();
                    if min == 0 {
                        positions.push((pos, caps.clone()));
                    }
                    let mut current = pos;
                    let mut current_caps = caps.clone();
                    let mut count = 0usize;
                    while max.is_none_or(|m| count < m) {
                        match self.regex_match_atom_with_capture_in_pkg(
                            &token.atom,
                            chars,
                            current,
                            &current_caps,
                            pkg,
                            pattern.ignore_case,
                        ) {
                            Some((next, new_caps)) if next != current => {
                                count += 1;
                                let new_caps = apply_hash_capture(
                                    token,
                                    current,
                                    next,
                                    pos_base,
                                    apply_named_capture(token, current, next, pos_base, new_caps),
                                );
                                current_caps = new_caps.clone();
                                current = next;
                                if count >= min {
                                    positions.push((current, current_caps.clone()));
                                }
                            }
                            _ => break,
                        }
                    }
                    if count < min {
                        continue;
                    }
                    if token.ratchet
                        && let Some(last) = positions.last().cloned()
                    {
                        positions = vec![last];
                    }
                    if token.frugal {
                        positions.reverse();
                    }
                    if stride > 0 {
                        for (p, c) in positions {
                            let mut c = c;
                            fold_quantified_captures(&mut c, base_len, stride);
                            stack.push((idx + 1, p, c));
                        }
                    } else {
                        for (p, c) in positions {
                            stack.push((idx + 1, p, c));
                        }
                    }
                }
            }
        }
        matches
    }
}
