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
    pub(super) fn regex_match_end_from_caps_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> Option<(usize, RegexCaptures)> {
        self.regex_match_ends_from_caps_in_pkg(pattern, chars, start, pkg)
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
        // When :m (ignoremark) is set on the pattern, strip combining marks
        // from both text and pattern, match on the stripped versions, then
        // map positions back to the original text.
        if pattern.ignore_mark {
            use super::regex_helpers::{map_pos, strip_marks_pattern, strip_marks_text};
            let text_slice = &chars[start..];
            let (stripped_chars, pos_map) = strip_marks_text(text_slice);
            let stripped_pattern = strip_marks_pattern(pattern);
            let mut results =
                self.regex_match_ends_from_caps_in_pkg(&stripped_pattern, &stripped_chars, 0, pkg);
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
        let apply_named_capture =
            |token: &RegexToken, from: usize, to: usize, caps: RegexCaptures| -> RegexCaptures {
                let Some(name) = token.named_capture.as_ref() else {
                    return caps;
                };
                let mut updated = caps;
                let captured: String = chars[from..to].iter().collect();
                updated
                    .named
                    .entry(name.clone())
                    .or_default()
                    .push(captured.clone());
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
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            let pos_base = caps.positional.len();
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
                        candidates
                            .sort_by_key(|(next, c)| (*next, c.positional.len(), c.named.len()));
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
                                apply_named_capture(token, pos, next, new_caps),
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
                        candidates
                            .sort_by_key(|(next, c)| (*next, c.positional.len(), c.named.len()));
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
                                    apply_named_capture(token, pos, *next, new_caps.clone()),
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
                                apply_named_capture(token, pos, next, new_caps),
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
                                apply_named_capture(token, current, next, new_caps),
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
                                    apply_named_capture(token, pos, next, new_caps),
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
                                apply_named_capture(token, current, next, new_caps),
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
                                    apply_named_capture(token, current, next, new_caps),
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
