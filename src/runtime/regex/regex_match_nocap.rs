use super::super::*;

impl Interpreter {
    pub(super) fn regex_match_end_from_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
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
                    stack.push((idx + 1, pos));
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
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
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
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
                RegexQuant::Repeat(..) | RegexQuant::RepeatCode(_) => {
                    let (min, max) = match &token.quant {
                        RegexQuant::Repeat(min, max) => (*min, *max),
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
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
            }
        }
        None
    }
}
