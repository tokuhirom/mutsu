use super::super::*;

/// Queue the candidate end positions of one quantified token onto the
/// depth-first work stack.
///
/// The stack is LIFO, so whatever is pushed LAST is explored FIRST, and
/// `positions` always arrives in ascending order (fewest repetitions first).
/// Pushing it unchanged therefore explores the LONGEST repetition first, which
/// is exactly greedy priority. A frugal quantifier (`*?`, `+?`, `**{...}?`)
/// has the mirror priority — shortest first — so its positions go on in
/// descending order. Without this the no-capture matcher matched every frugal
/// quantifier greedily, which is what made `"a \"b\" c \"d\"".comb(/ \" .*? \" /)`
/// return one match spanning both quoted runs instead of two.
fn push_quant_positions(
    stack: &mut Vec<(usize, usize)>,
    next_idx: usize,
    positions: Vec<usize>,
    frugal: bool,
) {
    if frugal {
        for p in positions.into_iter().rev() {
            stack.push((next_idx, p));
        }
    } else {
        for p in positions {
            stack.push((next_idx, p));
        }
    }
}

impl Interpreter {
    pub(super) fn regex_match_end_from_in_pkg(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<usize> {
        let mut stack = Vec::new();
        stack.push((0usize, start));
        while let Some((idx, pos)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return Some(pos);
                    }
                } else {
                    return Some(pos);
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            // Separator quantifiers (`atom **N %% sep`, `atom +% sep`, ...) whose
            // atom carries a capture are kept as a native separated token by the
            // parser (see `expand_ltm_pattern`); the capturing matcher handles
            // them via `match_separated_quantifier`. This no-capture matcher must
            // do the same, otherwise it silently ignores the separator and treats
            // the token as a plain count (so `(\d) ** 4 % '.'` fails to match
            // "1.2.3.4"). We only need the end positions here, so discard caps.
            if token.separator.is_some() {
                for (next, _caps) in self.match_separated_quantifier(
                    token,
                    chars,
                    pos,
                    pkg,
                    pattern,
                    &RegexCaptures::default(),
                ) {
                    stack.push((idx + 1, next));
                }
                continue;
            }
            match token.quant {
                RegexQuant::One => {
                    if let Some(next) = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    let matched = self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    );
                    // The stack is LIFO, so whatever is pushed LAST is explored
                    // first. Greedy `?` prefers the one-match branch; frugal `??`
                    // prefers zero.
                    if token.frugal {
                        if let Some(next) = matched {
                            stack.push((idx + 1, next));
                        }
                        stack.push((idx + 1, pos));
                    } else {
                        stack.push((idx + 1, pos));
                        if let Some(next) = matched {
                            stack.push((idx + 1, next));
                        }
                    }
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push(pos);
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
                        positions.push(next);
                        current = next;
                    }
                    push_quant_positions(&mut stack, idx + 1, positions, token.frugal);
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let mut current = match self.regex_match_atom_in_pkg(
                        &token.atom,
                        chars,
                        pos,
                        pkg,
                        pattern.ignore_case,
                    ) {
                        Some(next) => next,
                        None => continue,
                    };
                    positions.push(current);
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
                        positions.push(next);
                        current = next;
                    }
                    push_quant_positions(&mut stack, idx + 1, positions, token.frugal);
                }
                RegexQuant::Repeat(..) | RegexQuant::RepeatCode(_) => {
                    let (min, max) = match &token.quant {
                        RegexQuant::Repeat(min, max) => {
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
                            match self.eval_regex_repeat_code(code, &RegexCaptures::default()) {
                                Some((min, max)) => (min, max),
                                None => continue,
                            }
                        }
                        _ => unreachable!(),
                    };
                    // Match atom between min and max times
                    let mut positions = Vec::new();
                    let mut current = pos;
                    let mut count = 0usize;
                    while max.is_none_or(|m| count < m) {
                        match self.regex_match_atom_in_pkg(
                            &token.atom,
                            chars,
                            current,
                            pkg,
                            pattern.ignore_case,
                        ) {
                            Some(next) if next != current => {
                                count += 1;
                                current = next;
                                if count >= min {
                                    positions.push(current);
                                }
                            }
                            _ => break,
                        }
                    }
                    if count < min {
                        continue; // didn't match minimum times
                    }
                    // Push all valid positions (from min to actual count)
                    push_quant_positions(&mut stack, idx + 1, positions, token.frugal);
                }
            }
        }
        None
    }
}
