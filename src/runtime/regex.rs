use super::*;

impl Interpreter {
    #[allow(dead_code)]
    pub(super) fn regex_is_match(&self, pattern: &str, text: &str) -> bool {
        let parsed = match self.parse_regex(pattern) {
            Some(p) => p,
            None => return false,
        };
        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self.regex_match_from(&parsed, &chars, 0);
        }
        for start in 0..=chars.len() {
            if self.regex_match_from(&parsed, &chars, start) {
                return true;
            }
        }
        false
    }

    pub(super) fn regex_match_with_captures(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<HashMap<String, String>> {
        let parsed = self.parse_regex(pattern)?;
        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self
                .regex_match_end_from_caps(&parsed, &chars, 0)
                .map(|(_, caps)| caps);
        }
        for start in 0..=chars.len() {
            if let Some((_, caps)) = self.regex_match_end_from_caps(&parsed, &chars, start) {
                return Some(caps);
            }
        }
        None
    }

    pub(super) fn regex_find_first(&self, pattern: &str, text: &str) -> Option<(usize, usize)> {
        let parsed = self.parse_regex(pattern)?;
        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self
                .regex_match_end_from(&parsed, &chars, 0)
                .map(|end| (0, end));
        }
        for start in 0..=chars.len() {
            if let Some(end) = self.regex_match_end_from(&parsed, &chars, start) {
                return Some((start, end));
            }
        }
        None
    }

    pub(super) fn regex_match_len_at_start(&self, pattern: &str, text: &str) -> Option<usize> {
        let parsed = self.parse_regex(pattern)?;
        let chars: Vec<char> = text.chars().collect();
        self.regex_match_end_from(&parsed, &chars, 0)
    }

    pub(super) fn regex_match_end_from(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
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
                    if let Some(next) = self.regex_match_atom(&token.atom, chars, pos) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    if let Some(next) = self.regex_match_atom(&token.atom, chars, pos) {
                        stack.push((idx + 1, next));
                    }
                    stack.push((idx + 1, pos));
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push(pos);
                    let mut current = pos;
                    while let Some(next) = self.regex_match_atom(&token.atom, chars, current) {
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let mut current = match self.regex_match_atom(&token.atom, chars, pos) {
                        Some(next) => next,
                        None => continue,
                    };
                    positions.push(current);
                    while let Some(next) = self.regex_match_atom(&token.atom, chars, current) {
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
            }
        }
        None
    }

    pub(super) fn regex_match_end_from_caps(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
    ) -> Option<(usize, HashMap<String, String>)> {
        let mut stack = Vec::new();
        stack.push((0usize, start, HashMap::new()));
        while let Some((idx, pos, caps)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return Some((pos, caps));
                    }
                } else {
                    return Some((pos, caps));
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    if let Some((next, cap)) =
                        self.regex_match_atom_with_capture(&token.atom, chars, pos)
                    {
                        let mut next_caps = caps.clone();
                        if let Some((name, value)) = cap {
                            next_caps.insert(name, value);
                        }
                        stack.push((idx + 1, next, next_caps));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    if let Some((next, cap)) =
                        self.regex_match_atom_with_capture(&token.atom, chars, pos)
                    {
                        let mut next_caps = caps.clone();
                        if let Some((name, value)) = cap {
                            next_caps.insert(name, value);
                        }
                        stack.push((idx + 1, next, next_caps));
                    }
                    stack.push((idx + 1, pos, caps.clone()));
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push((pos, caps.clone()));
                    let mut current = pos;
                    let mut current_caps = caps.clone();
                    while let Some((next, cap)) =
                        self.regex_match_atom_with_capture(&token.atom, chars, current)
                    {
                        if let Some((name, value)) = cap {
                            current_caps.insert(name, value);
                        }
                        positions.push((next, current_caps.clone()));
                        current = next;
                    }
                    for (p, c) in positions {
                        stack.push((idx + 1, p, c));
                    }
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let (mut current, mut current_caps) =
                        match self.regex_match_atom_with_capture(&token.atom, chars, pos) {
                            Some((next, cap)) => {
                                let mut caps = caps.clone();
                                if let Some((name, value)) = cap {
                                    caps.insert(name, value);
                                }
                                (next, caps)
                            }
                            None => continue,
                        };
                    positions.push((current, current_caps.clone()));
                    while let Some((next, cap)) =
                        self.regex_match_atom_with_capture(&token.atom, chars, current)
                    {
                        if let Some((name, value)) = cap {
                            current_caps.insert(name, value);
                        }
                        positions.push((next, current_caps.clone()));
                        current = next;
                    }
                    for (p, c) in positions {
                        stack.push((idx + 1, p, c));
                    }
                }
            }
        }
        None
    }

    #[allow(dead_code)]
    pub(super) fn regex_match_from(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
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
                    if let Some(next) = self.regex_match_atom(&token.atom, chars, pos) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    if let Some(next) = self.regex_match_atom(&token.atom, chars, pos) {
                        stack.push((idx + 1, next));
                    }
                    stack.push((idx + 1, pos));
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push(pos);
                    let mut current = pos;
                    while let Some(next) = self.regex_match_atom(&token.atom, chars, current) {
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let mut current = match self.regex_match_atom(&token.atom, chars, pos) {
                        Some(next) => next,
                        None => continue,
                    };
                    positions.push(current);
                    while let Some(next) = self.regex_match_atom(&token.atom, chars, current) {
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
            }
        }
        false
    }

    pub(super) fn regex_match_atom(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
    ) -> Option<usize> {
        // Group, Alternation, and ZeroWidth can match zero-width, so check before pos >= chars.len()
        match atom {
            RegexAtom::Group(pattern) => {
                return self.regex_match_end_from(pattern, chars, pos);
            }
            RegexAtom::Alternation(alternatives) => {
                for alt in alternatives {
                    if let Some(end) = self.regex_match_end_from(alt, chars, pos) {
                        return Some(end);
                    }
                }
                return None;
            }
            RegexAtom::ZeroWidth => {
                return Some(pos); // Matches at any position without consuming
            }
            RegexAtom::UnicodePropAssert { name, negated } => {
                // Zero-width assertion: check next char but don't consume
                if pos >= chars.len() {
                    // At end of string, negated assertion succeeds
                    return if *negated { Some(pos) } else { None };
                }
                let c = chars[pos];
                let prop_match = check_unicode_property(name, c);
                let result = if *negated { !prop_match } else { prop_match };
                return if result { Some(pos) } else { None };
            }
            _ => {}
        }
        if pos >= chars.len() {
            return None;
        }
        let c = chars[pos];
        // Newline needs special handling: \r\n is a single logical newline (2 chars)
        match atom {
            RegexAtom::Newline => {
                // Raku \n matches: \r\n (CR+LF), \n (LF), \r (CR), \x85 (NEL), \x2028 (LINE SEP)
                if c == '\r' && pos + 1 < chars.len() && chars[pos + 1] == '\n' {
                    return Some(pos + 2); // CR/LF
                }
                if c == '\n' || c == '\r' || c == '\u{85}' || c == '\u{2028}' {
                    return Some(pos + 1);
                }
                return None;
            }
            RegexAtom::NotNewline => {
                // Matches anything that is NOT a newline character
                if c == '\n' || c == '\r' || c == '\u{85}' || c == '\u{2028}' {
                    return None;
                }
                return Some(pos + 1);
            }
            _ => {}
        }
        let matched = match atom {
            RegexAtom::Literal(ch) => *ch == c,
            RegexAtom::Named(name) => {
                let name_chars: Vec<char> = name.chars().collect();
                if pos + name_chars.len() > chars.len() {
                    false
                } else {
                    chars[pos..pos + name_chars.len()] == name_chars[..]
                }
            }
            RegexAtom::Any => true,
            RegexAtom::Digit => c.is_ascii_digit(),
            RegexAtom::Word => c.is_ascii_alphanumeric() || c == '_',
            RegexAtom::Space => c.is_whitespace(),
            RegexAtom::CharClass(class) => self.regex_match_class(class, c),
            RegexAtom::UnicodeProp { name, negated } => {
                let prop_match = check_unicode_property(name, c);
                if *negated { !prop_match } else { prop_match }
            }
            RegexAtom::Group(_)
            | RegexAtom::Alternation(_)
            | RegexAtom::Newline
            | RegexAtom::NotNewline
            | RegexAtom::ZeroWidth
            | RegexAtom::UnicodePropAssert { .. } => unreachable!(),
        };
        if matched {
            match atom {
                RegexAtom::Named(name) => Some(pos + name.chars().count()),
                _ => Some(pos + 1),
            }
        } else {
            None
        }
    }

    pub(super) fn regex_match_atom_with_capture(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
    ) -> Option<(usize, Option<(String, String)>)> {
        // Handle zero-width and group atoms before the length check
        match atom {
            RegexAtom::Group(_)
            | RegexAtom::Alternation(_)
            | RegexAtom::ZeroWidth
            | RegexAtom::UnicodePropAssert { .. } => {
                return self
                    .regex_match_atom(atom, chars, pos)
                    .map(|next| (next, None));
            }
            _ => {}
        }
        if pos >= chars.len() {
            return None;
        }
        match atom {
            RegexAtom::Named(name) => {
                let name_chars: Vec<char> = name.chars().collect();
                if pos + name_chars.len() > chars.len() {
                    return None;
                }
                if chars[pos..pos + name_chars.len()] == name_chars[..] {
                    let captured: String = name_chars.iter().collect();
                    return Some((pos + name_chars.len(), Some((name.clone(), captured))));
                }
                None
            }
            _ => self
                .regex_match_atom(atom, chars, pos)
                .map(|next| (next, None)),
        }
    }

    pub(super) fn regex_match_class(&self, class: &CharClass, c: char) -> bool {
        let mut matched = false;
        for item in &class.items {
            match item {
                ClassItem::Range(a, b) => {
                    if *a <= c && c <= *b {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Char(ch) => {
                    if *ch == c {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Digit => {
                    if c.is_ascii_digit() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Word => {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Space => {
                    if c.is_whitespace() {
                        matched = true;
                        break;
                    }
                }
            }
        }
        if class.negated { !matched } else { matched }
    }
}
