use super::super::*;
use super::regex_helpers::{CaseFoldIter, grapheme_end, is_word_char, matches_named_builtin};

impl Interpreter {
    #[allow(dead_code)]
    pub(super) fn regex_match_from_in_pkg(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: &str,
    ) -> bool {
        let mut stack = Vec::new();
        stack.push((0usize, start));
        while let Some((idx, pos)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return true;
                    }
                } else {
                    return true;
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
                RegexQuant::Repeat(min, max) => {
                    let mut current = pos;
                    let mut count = 0usize;
                    let mut positions = Vec::new();
                    if min == 0 {
                        positions.push(pos);
                    }
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
                    if count >= min {
                        for p in positions {
                            stack.push((idx + 1, p));
                        }
                    }
                }
            }
        }
        false
    }

    pub(super) fn regex_match_atom_in_pkg(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        pkg: &str,
        ignore_case: bool,
    ) -> Option<usize> {
        // Group, CaptureGroup, Alternation, ZeroWidth, CodeAssertion can match zero-width
        match atom {
            RegexAtom::Group(pattern) => {
                return self.regex_match_end_from_in_pkg(pattern, chars, pos, pkg);
            }
            RegexAtom::CaptureGroup(pattern) => {
                return self.regex_match_end_from_in_pkg(pattern, chars, pos, pkg);
            }
            RegexAtom::GoalMatch {
                goal,
                inner,
                goal_text,
            } => {
                if let Some(inner_end) = self.regex_match_end_from_in_pkg(inner, chars, pos, pkg) {
                    if let Some(goal_end) =
                        self.regex_match_end_from_in_pkg(goal, chars, inner_end, pkg)
                    {
                        return Some(goal_end);
                    }
                    Self::record_goal_failure(goal_text, inner_end);
                }
                return None;
            }
            RegexAtom::Alternation(alternatives) => {
                for alt in alternatives {
                    if let Some(end) = self.regex_match_end_from_in_pkg(alt, chars, pos, pkg) {
                        return Some(end);
                    }
                }
                return None;
            }
            RegexAtom::ZeroWidth => {
                return Some(pos);
            }
            RegexAtom::LeftWordBoundary => {
                let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
                let at_is_word = pos < chars.len() && is_word_char(chars[pos]);
                return if !before_is_word && at_is_word {
                    Some(pos)
                } else {
                    None
                };
            }
            RegexAtom::RightWordBoundary => {
                let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
                let at_is_word = pos < chars.len() && is_word_char(chars[pos]);
                return if before_is_word && !at_is_word {
                    Some(pos)
                } else {
                    None
                };
            }
            RegexAtom::StartOfLine => {
                return if pos == 0 || chars[pos - 1] == '\n' {
                    Some(pos)
                } else {
                    None
                };
            }
            RegexAtom::EndOfLine => {
                if pos == chars.len() {
                    return Some(pos);
                }
                if chars[pos] == '\n' {
                    if pos > 0 && chars[pos - 1] == '\r' {
                        return None;
                    }
                    return Some(pos);
                }
                if chars[pos] == '\r' && (pos + 1 >= chars.len() || chars[pos + 1] != '\n') {
                    return Some(pos);
                }
                return None;
            }
            RegexAtom::WsRule => {
                let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
                let mut end = pos;
                while end < chars.len() && chars[end].is_whitespace() {
                    end += 1;
                }
                let after_is_word = end < chars.len() && is_word_char(chars[end]);
                if before_is_word && after_is_word {
                    return if end > pos { Some(end) } else { None };
                }
                return Some(end);
            }
            RegexAtom::CodeAssertion { .. } => {
                return Some(pos);
            }
            RegexAtom::CaptureStartMarker | RegexAtom::CaptureEndMarker => {
                return Some(pos);
            }
            RegexAtom::Backref(_) | RegexAtom::NamedBackref(_) => {
                return None;
            }
            RegexAtom::Lookaround {
                pattern,
                negated,
                is_behind,
            } => {
                let matched = if *is_behind {
                    let mut found = false;
                    for start in 0..=pos {
                        if self
                            .regex_match_end_from_caps_in_pkg(pattern, chars, start, pkg)
                            .is_some_and(|(end, _)| end == pos)
                        {
                            found = true;
                            break;
                        }
                    }
                    found
                } else {
                    self.regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg)
                        .is_some()
                };
                let pass = if *negated { !matched } else { matched };
                return if pass { Some(pos) } else { None };
            }
            RegexAtom::UnicodePropAssert { name, negated } => {
                if pos >= chars.len() {
                    return if *negated { Some(pos) } else { None };
                }
                let c = chars[pos];
                let prop_match = check_unicode_property(name, c);
                let result = if *negated { !prop_match } else { prop_match };
                return if result { Some(pos) } else { None };
            }
            _ => {}
        }
        if let RegexAtom::Named(name) = atom {
            let spec = Self::parse_named_regex_lookup_spec(name);
            let default_caps = RegexCaptures::default();
            let arg_values = if spec.arg_exprs.is_empty() {
                Vec::new()
            } else {
                let mut values = Vec::new();
                for arg in &spec.arg_exprs {
                    let v = self.eval_regex_expr_value(arg, &default_caps)?;
                    values.push(v);
                }
                values
            };
            let candidates = self.resolve_named_regex_candidates_in_pkg(&spec, pkg, &arg_values);
            if !candidates.is_empty() {
                let remaining: String = chars[pos..].iter().collect();
                let mut best_len: Option<usize> = None;
                for (sub_pat, sub_pkg, _sym_key) in candidates {
                    if let Some(len) =
                        self.regex_match_len_at_start_in_pkg(&sub_pat, &remaining, &sub_pkg)
                    {
                        let better = best_len.map(|current| len > current).unwrap_or(true);
                        if better {
                            best_len = Some(len);
                        }
                    }
                }
                return best_len.map(|len| pos + len);
            }
            if spec.lookup_name == "wb" && !spec.token_lookup {
                let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
                let after_is_word = pos < chars.len() && is_word_char(chars[pos]);
                return if before_is_word != after_is_word
                    || (pos == 0 && after_is_word)
                    || (pos == chars.len() && before_is_word)
                {
                    Some(pos)
                } else {
                    None
                };
            }
            if spec.lookup_name == "ws" && !spec.token_lookup {
                let mut next = pos;
                while next < chars.len() && chars[next].is_whitespace() {
                    next += 1;
                }
                let before_is_word = pos > 0 && is_word_char(chars[pos - 1]);
                let after_is_word = next < chars.len() && is_word_char(chars[next]);
                if before_is_word && after_is_word && next == pos {
                    return None;
                }
                return Some(next);
            }
        }
        if pos >= chars.len() {
            return None;
        }
        let c = chars[pos];
        // Newline needs special handling: \r\n is a single logical newline (2 chars)
        match atom {
            RegexAtom::Newline => {
                if c == '\r' && pos + 1 < chars.len() && chars[pos + 1] == '\n' {
                    return Some(pos + 2); // CR/LF
                }
                if c == '\n' || c == '\r' || c == '\u{85}' || c == '\u{2028}' {
                    return Some(pos + 1);
                }
                return None;
            }
            RegexAtom::NotNewline => {
                if c == '\n' || c == '\r' || c == '\u{85}' || c == '\u{2028}' {
                    return None;
                }
                return Some(grapheme_end(chars, pos));
            }
            _ => {}
        }
        let matched = match atom {
            RegexAtom::Literal(ch) => {
                if ignore_case {
                    ch.to_lowercase().to_string() == c.to_lowercase().to_string()
                } else {
                    *ch == c
                }
            }
            RegexAtom::Named(name) => {
                let spec = Self::parse_named_regex_lookup_spec(name);
                if spec.token_lookup || !spec.arg_exprs.is_empty() {
                    return None;
                }
                let literal = spec.lookup_name;
                let name_chars: Vec<char> = literal.chars().collect();
                if pos + name_chars.len() > chars.len() {
                    false
                } else {
                    let slice = &chars[pos..pos + name_chars.len()];
                    if ignore_case {
                        slice.iter().collect::<String>().to_lowercase()
                            == name_chars.iter().collect::<String>().to_lowercase()
                    } else {
                        *slice == name_chars[..]
                    }
                }
            }
            RegexAtom::Any => true,
            RegexAtom::CharClass(class) => {
                // \r\n is a single grapheme cluster in Raku; treat it as matching \n
                if c == '\r'
                    && pos + 1 < chars.len()
                    && chars[pos + 1] == '\n'
                    && self.regex_match_class_ignorecase(class, '\n', ignore_case)
                {
                    return Some(pos + 2);
                }
                self.regex_match_class_ignorecase(class, c, ignore_case)
            }
            RegexAtom::UnicodeProp {
                name,
                negated,
                args,
            } => {
                let prop_match = if let Some(arg_str) = args {
                    check_unicode_property_with_args(name, arg_str, c)
                } else {
                    check_unicode_property(name, c)
                };
                if *negated { !prop_match } else { prop_match }
            }
            RegexAtom::CompositeClass { positive, negative } => {
                // \r\n is a single grapheme cluster in Raku; treat it as matching \n
                let effective_c = if c == '\r' && pos + 1 < chars.len() && chars[pos + 1] == '\n' {
                    '\n'
                } else {
                    c
                };
                let check_char = if ignore_case {
                    effective_c.to_lowercase().next().unwrap_or(effective_c)
                } else {
                    effective_c
                };
                let chars_to_check: Vec<char> = if ignore_case {
                    CaseFoldIter::new(effective_c).collect()
                } else {
                    vec![effective_c]
                };
                let pos_match = positive.iter().any(|item| match item {
                    ClassItem::NamedBuiltin(n) => chars_to_check
                        .iter()
                        .any(|ch| matches_named_builtin(n, *ch)),
                    ClassItem::UnicodePropItem { name, negated } => {
                        let m = chars_to_check
                            .iter()
                            .any(|ch| check_unicode_property(name, *ch));
                        if *negated { !m } else { m }
                    }
                    _ => {
                        let class = CharClass {
                            items: vec![item.clone()],
                            negated: false,
                        };
                        chars_to_check
                            .iter()
                            .any(|ch| self.regex_match_class(&class, *ch))
                    }
                });
                let neg_match = negative.iter().any(|item| match item {
                    ClassItem::NamedBuiltin(n) => chars_to_check
                        .iter()
                        .any(|ch| matches_named_builtin(n, *ch)),
                    ClassItem::UnicodePropItem { name, negated } => {
                        let m = chars_to_check
                            .iter()
                            .any(|ch| check_unicode_property(name, *ch));
                        if *negated { !m } else { m }
                    }
                    _ => {
                        let class = CharClass {
                            items: vec![item.clone()],
                            negated: false,
                        };
                        chars_to_check
                            .iter()
                            .any(|ch| self.regex_match_class(&class, *ch))
                    }
                });
                let _ = check_char;
                pos_match && !neg_match
            }
            RegexAtom::Group(_)
            | RegexAtom::CaptureGroup(_)
            | RegexAtom::Alternation(_)
            | RegexAtom::GoalMatch { .. }
            | RegexAtom::Newline
            | RegexAtom::NotNewline
            | RegexAtom::ZeroWidth
            | RegexAtom::CodeAssertion { .. }
            | RegexAtom::UnicodePropAssert { .. }
            | RegexAtom::Lookaround { .. }
            | RegexAtom::CaptureStartMarker
            | RegexAtom::CaptureEndMarker
            | RegexAtom::Backref(_)
            | RegexAtom::NamedBackref(_)
            | RegexAtom::VarDecl { .. }
            | RegexAtom::ClosureInterpolation { .. }
            | RegexAtom::LeftWordBoundary
            | RegexAtom::RightWordBoundary
            | RegexAtom::StartOfLine
            | RegexAtom::TildeMarker
            | RegexAtom::EndOfLine
            | RegexAtom::WsRule => unreachable!(),
        };
        if matched {
            match atom {
                RegexAtom::Named(name) => {
                    let spec = Self::parse_named_regex_lookup_spec(name);
                    Some(pos + spec.lookup_name.chars().count())
                }
                // Literal matches advance by exactly 1 codepoint — they do
                // NOT consume trailing combining marks, because the regex may
                // need to match those marks explicitly (e.g. `abc \x[5B4] def`).
                RegexAtom::Literal(_) => Some(pos + 1),
                // Character classes (\d, \w, .), Unicode properties, and
                // composite classes match a full grapheme: the base codepoint
                // plus any trailing combining marks.
                _ => Some(grapheme_end(chars, pos)),
            }
        } else {
            None
        }
    }
}
