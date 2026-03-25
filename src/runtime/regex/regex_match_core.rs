use super::super::*;
use super::regex_helpers::{count_capture_groups, fold_quantified_captures, is_simple_atom};

impl Interpreter {
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
                    .push(captured);
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
                            apply_named_capture(token, pos, next, new_caps),
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
                                apply_named_capture(token, pos, *next, new_caps.clone()),
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
                            apply_named_capture(token, pos, next, new_caps),
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
                        && let Some((resolved, resolved_pkg)) =
                            self.try_resolve_named_to_pattern(&token.atom, pkg)
                    {
                        // Fast path for ratcheted Named token: resolve pattern once,
                        // match directly without creating new Interpreter per iteration
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
                    } else {
                        let base_len = caps.positional.len();
                        let stride = count_capture_groups(&token.atom);
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
                            let new_caps = apply_named_capture(token, current, next, new_caps);
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
                        && let Some((resolved, resolved_pkg)) =
                            self.try_resolve_named_to_pattern(&token.atom, pkg)
                    {
                        // Fast path for ratcheted Named token
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
                    } else {
                        let base_len = caps.positional.len();
                        let stride = count_capture_groups(&token.atom);
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
                                let new_caps = apply_named_capture(token, pos, next, new_caps);
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
                            let new_caps = apply_named_capture(token, current, next, new_caps);
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
                RegexQuant::Repeat(min, max) => {
                    let base_len = caps.positional.len();
                    let stride = count_capture_groups(&token.atom);
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
                                let new_caps = apply_named_capture(token, current, next, new_caps);
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
