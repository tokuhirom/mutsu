use super::super::*;
use super::regex_helpers::merge_regex_captures;

impl Interpreter {
    pub(super) fn regex_match_atom_all_with_capture_in_pkg(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: &str,
        ignore_case: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        if let RegexAtom::Alternation(alternatives) = atom {
            let mut indexed: Vec<(usize, usize, RegexCaptures)> = Vec::new();
            for (i, alt) in alternatives.iter().enumerate() {
                if let Some((next, mut inner_caps)) =
                    self.regex_match_end_from_caps_in_pkg(alt, chars, pos, pkg)
                {
                    let mut new_caps = current_caps.clone();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().extend(v);
                    }
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
            let mut out = Vec::new();
            for alt in alternatives {
                if let Some((next, mut inner_caps)) =
                    self.regex_match_end_from_caps_in_pkg(alt, chars, pos, pkg)
                {
                    let mut new_caps = current_caps.clone();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().extend(v);
                    }
                    new_caps.positional.append(&mut inner_caps.positional);
                    new_caps
                        .positional_subcaps
                        .append(&mut inner_caps.positional_subcaps);
                    new_caps
                        .positional_quantified
                        .append(&mut inner_caps.positional_quantified);
                    new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                    out.push((next, new_caps));
                }
            }
            out.reverse();
            return out;
        }
        if let RegexAtom::Conjunction(branches) = atom {
            // ALL branches must match at the same position.
            let mut longest_end = 0usize;
            let mut longest_caps = current_caps.clone();
            for branch in branches {
                if let Some((end, caps)) =
                    self.regex_match_end_from_caps_in_pkg(branch, chars, pos, pkg)
                {
                    if end >= longest_end {
                        longest_end = end;
                        longest_caps = caps;
                    }
                } else {
                    return Vec::new();
                }
            }
            let mut new_caps = current_caps.clone();
            for (k, v) in longest_caps.named {
                new_caps.named.entry(k).or_default().extend(v);
            }
            new_caps.positional.append(&mut longest_caps.positional);
            new_caps
                .positional_subcaps
                .append(&mut longest_caps.positional_subcaps);
            new_caps
                .positional_quantified
                .append(&mut longest_caps.positional_quantified);
            new_caps.code_blocks.append(&mut longest_caps.code_blocks);
            return vec![(longest_end, new_caps)];
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
                // Merge inner named captures into parent
                for (k, v) in inner_caps.named.drain() {
                    new_caps.named.entry(k).or_default().extend(v);
                }
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
            out.sort_by_key(|(end, caps)| {
                (
                    *end,
                    caps.code_blocks.len(),
                    caps.positional.len(),
                    caps.named.len(),
                )
            });
            let mut deduped: Vec<(usize, RegexCaptures)> = Vec::new();
            for (end, caps) in out {
                if deduped.last().is_some_and(|(last_end, _)| *last_end == end) {
                    deduped.pop();
                }
                deduped.push((end, caps));
            }
            return deduped;
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
                let mut out = Vec::new();
                for (sub_pat, sub_pkg, sym_key) in candidates {
                    if let Some(parsed) = self.parse_regex(&sub_pat) {
                        for (inner_end, inner_caps) in
                            self.regex_match_ends_from_caps_in_pkg(&parsed, &tail, 0, &sub_pkg)
                        {
                            let end = pos + inner_end;
                            let mut new_caps = current_caps.clone();
                            let capture_name = spec
                                .capture_name
                                .as_deref()
                                .or_else(|| (!spec.silent).then_some(spec.lookup_name.as_str()));
                            if let Some(capture_name) = capture_name {
                                let captured: String = chars[pos..end].iter().collect();
                                // Store inner captures as subcaptures (nested)
                                let mut subcap = inner_caps;
                                subcap.matched = captured.clone();
                                subcap.from = pos;
                                subcap.to = end;
                                // Store :sym<> variant in subcapture
                                if sym_key.is_some() {
                                    subcap.sym = sym_key.clone();
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
                            } else {
                                // Silent subrule — merge named captures only (not positional).
                                let mut inner_caps = inner_caps;
                                for (k, v) in inner_caps.named.drain() {
                                    new_caps.named.entry(k).or_default().extend(v);
                                }
                                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                            }
                            out.push((end, new_caps));
                        }
                    }
                }
                out.sort_by_key(|(end, caps)| {
                    (
                        *end,
                        caps.code_blocks.len(),
                        caps.positional.len(),
                        caps.named.len(),
                    )
                });
                let mut deduped: Vec<(usize, RegexCaptures)> = Vec::new();
                for (end, caps) in out {
                    if deduped.last().is_some_and(|(last_end, _)| *last_end == end) {
                        deduped.pop();
                    }
                    deduped.push((end, caps));
                }
                return deduped;
            }
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
}
