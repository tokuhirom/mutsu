use super::*;

impl Interpreter {
    #[allow(dead_code)]
    fn regex_is_match(&self, pattern: &str, text: &str) -> bool {
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

    fn regex_match_end_from(
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

    fn regex_match_end_from_caps(
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
    fn regex_match_from(&self, pattern: &RegexPattern, chars: &[char], start: usize) -> bool {
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

    fn regex_match_atom(&self, atom: &RegexAtom, chars: &[char], pos: usize) -> Option<usize> {
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

    fn regex_match_atom_with_capture(
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

    fn regex_match_class(&self, class: &CharClass, c: char) -> bool {
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

    fn parse_regex(&self, pattern: &str) -> Option<RegexPattern> {
        let mut chars = pattern.chars().peekable();
        let mut tokens = Vec::new();
        let mut anchor_start = false;
        let mut anchor_end = false;
        while let Some(c) = chars.next() {
            // In Raku, unescaped whitespace in regex is insignificant
            if c.is_whitespace() {
                continue;
            }
            // '#' starts a comment until end of line in Raku regex
            if c == '#' {
                for ch in chars.by_ref() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }
            if c == '^' && tokens.is_empty() {
                anchor_start = true;
                continue;
            }
            if c == '$' && chars.clone().all(|ch| ch.is_whitespace()) {
                anchor_end = true;
                break;
            }
            let atom = match c {
                '.' => RegexAtom::Any,
                '\\' => {
                    let esc = chars.next()?;
                    match esc {
                        'd' => RegexAtom::Digit,
                        'w' => RegexAtom::Word,
                        's' => RegexAtom::Space,
                        'n' => RegexAtom::Newline,
                        'N' => RegexAtom::NotNewline,
                        't' => RegexAtom::Literal('\t'),
                        'r' => RegexAtom::Literal('\r'),
                        'x' => {
                            // \x[HEX] hex escape in regex
                            if chars.peek() == Some(&'[') {
                                chars.next(); // skip '['
                                let mut hex = String::new();
                                while let Some(&ch) = chars.peek() {
                                    if ch == ']' {
                                        chars.next();
                                        break;
                                    }
                                    hex.push(ch);
                                    chars.next();
                                }
                                if let Some(c) =
                                    u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
                                {
                                    RegexAtom::Literal(c)
                                } else {
                                    continue;
                                }
                            } else {
                                RegexAtom::Literal('x')
                            }
                        }
                        'o' => {
                            // \o[OCT] octal escape in regex
                            if chars.peek() == Some(&'[') {
                                chars.next(); // skip '['
                                let mut oct = String::new();
                                while let Some(&ch) = chars.peek() {
                                    if ch == ']' {
                                        chars.next();
                                        break;
                                    }
                                    oct.push(ch);
                                    chars.next();
                                }
                                if let Some(c) =
                                    u32::from_str_radix(&oct, 8).ok().and_then(char::from_u32)
                                {
                                    RegexAtom::Literal(c)
                                } else {
                                    continue;
                                }
                            } else {
                                RegexAtom::Literal('o')
                            }
                        }
                        'c' => {
                            // \c[NAME] or \c[NAME1, NAME2] named character escape in regex
                            if chars.peek() == Some(&'[') {
                                chars.next(); // skip '['
                                let mut name = String::new();
                                while let Some(&ch) = chars.peek() {
                                    if ch == ']' {
                                        chars.next();
                                        break;
                                    }
                                    name.push(ch);
                                    chars.next();
                                }
                                // Handle comma-separated names
                                let parts: Vec<&str> = name.split(',').map(|s| s.trim()).collect();
                                let mut resolved: Vec<char> = Vec::new();
                                for part in &parts {
                                    if let Some(c) = crate::lexer::lookup_unicode_char_by_name(part)
                                    {
                                        resolved.push(c);
                                    }
                                }
                                if resolved.is_empty() {
                                    continue;
                                }
                                // Push all but last as separate literal tokens
                                for &c in &resolved[..resolved.len() - 1] {
                                    tokens.push(RegexToken {
                                        atom: RegexAtom::Literal(c),
                                        quant: RegexQuant::One,
                                    });
                                }
                                RegexAtom::Literal(*resolved.last().unwrap())
                            } else {
                                RegexAtom::Literal('c')
                            }
                        }
                        'C' => {
                            // \C[NAME] matches any char that is NOT the named char
                            if chars.peek() == Some(&'[') {
                                chars.next();
                                let mut name = String::new();
                                while let Some(&ch) = chars.peek() {
                                    if ch == ']' {
                                        chars.next();
                                        break;
                                    }
                                    name.push(ch);
                                    chars.next();
                                }
                                if let Some(c) = crate::lexer::lookup_unicode_char_by_name(&name) {
                                    RegexAtom::CharClass(CharClass {
                                        negated: true,
                                        items: vec![ClassItem::Char(c)],
                                    })
                                } else {
                                    continue;
                                }
                            } else {
                                RegexAtom::Literal('C')
                            }
                        }
                        'X' => {
                            // \X[HEX] matches any char that is NOT the given hex char
                            if chars.peek() == Some(&'[') {
                                chars.next();
                                let mut hex = String::new();
                                while let Some(&ch) = chars.peek() {
                                    if ch == ']' {
                                        chars.next();
                                        break;
                                    }
                                    hex.push(ch);
                                    chars.next();
                                }
                                if let Some(c) =
                                    u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
                                {
                                    RegexAtom::CharClass(CharClass {
                                        negated: true,
                                        items: vec![ClassItem::Char(c)],
                                    })
                                } else {
                                    continue;
                                }
                            } else {
                                RegexAtom::Literal('X')
                            }
                        }
                        other => RegexAtom::Literal(other),
                    }
                }
                '\'' => {
                    // Quoted literal string in Raku regex: 'foo-bar' matches literally
                    let mut literal = String::new();
                    for ch in chars.by_ref() {
                        if ch == '\'' {
                            break;
                        }
                        literal.push(ch);
                    }
                    // Expand to a sequence of literal atoms
                    let lit_chars: Vec<char> = literal.chars().collect();
                    if lit_chars.is_empty() {
                        // Empty string literal matches zero-width
                        continue;
                    }
                    // Push all but the last char as One-quantified literals
                    for &lc in &lit_chars[..lit_chars.len() - 1] {
                        tokens.push(RegexToken {
                            atom: RegexAtom::Literal(lc),
                            quant: RegexQuant::One,
                        });
                    }
                    // The last char will get any quantifier attached below
                    RegexAtom::Literal(*lit_chars.last().unwrap())
                }
                '<' => {
                    // Read content between < and >, handling nested <...>
                    let mut name = String::new();
                    let mut angle_depth = 1usize;
                    for ch in chars.by_ref() {
                        if ch == '<' {
                            angle_depth += 1;
                            name.push(ch);
                        } else if ch == '>' {
                            angle_depth -= 1;
                            if angle_depth == 0 {
                                break;
                            }
                            name.push(ch);
                        } else {
                            name.push(ch);
                        }
                    }
                    // Check for Raku character class: <[...]>, <-[...]>, <+[...]>
                    let trimmed = name.trim();
                    if (trimmed.starts_with('[')
                        || trimmed.starts_with("-[")
                        || trimmed.starts_with("+["))
                        && trimmed.ends_with(']')
                    {
                        let negated;
                        let inner;
                        if trimmed.starts_with("-[") {
                            negated = true;
                            inner = &trimmed[2..trimmed.len() - 1];
                        } else if trimmed.starts_with("+[") {
                            negated = false;
                            inner = &trimmed[2..trimmed.len() - 1];
                        } else {
                            negated = false;
                            inner = &trimmed[1..trimmed.len() - 1];
                        }
                        // Parse Raku-style character class content
                        if let Some(class) = self.parse_raku_char_class(inner, negated) {
                            RegexAtom::CharClass(class)
                        } else {
                            continue;
                        }
                    } else if trimmed == "?" {
                        // <?>  null assertion: matches zero-width at any position
                        RegexAtom::ZeroWidth
                    } else if let Some(prop_name) = trimmed.strip_prefix("!:") {
                        // <!:PropName> — zero-width negative Unicode property assertion
                        RegexAtom::UnicodePropAssert {
                            name: prop_name.to_string(),
                            negated: true,
                        }
                    } else if trimmed.starts_with(":!") || trimmed.starts_with("-:") {
                        // <:!PropName> or <-:PropName> — negated Unicode property
                        let prop_name = &trimmed[2..];
                        RegexAtom::UnicodeProp {
                            name: prop_name.to_string(),
                            negated: true,
                        }
                    } else if let Some(prop_name) = trimmed.strip_prefix(':') {
                        // <:PropName> — Unicode property assertion
                        RegexAtom::UnicodeProp {
                            name: prop_name.to_string(),
                            negated: false,
                        }
                    } else {
                        // Check for named character classes
                        match trimmed {
                            "alpha" => RegexAtom::CharClass(CharClass {
                                items: vec![
                                    ClassItem::Range('a', 'z'),
                                    ClassItem::Range('A', 'Z'),
                                    ClassItem::Char('_'),
                                ],
                                negated: false,
                            }),
                            "upper" => RegexAtom::CharClass(CharClass {
                                items: vec![ClassItem::Range('A', 'Z')],
                                negated: false,
                            }),
                            "lower" => RegexAtom::CharClass(CharClass {
                                items: vec![ClassItem::Range('a', 'z')],
                                negated: false,
                            }),
                            "digit" => RegexAtom::CharClass(CharClass {
                                items: vec![ClassItem::Digit],
                                negated: false,
                            }),
                            "xdigit" => RegexAtom::CharClass(CharClass {
                                items: vec![
                                    ClassItem::Range('0', '9'),
                                    ClassItem::Range('a', 'f'),
                                    ClassItem::Range('A', 'F'),
                                ],
                                negated: false,
                            }),
                            "space" | "ws" => RegexAtom::CharClass(CharClass {
                                items: vec![ClassItem::Space],
                                negated: false,
                            }),
                            "alnum" => RegexAtom::CharClass(CharClass {
                                items: vec![
                                    ClassItem::Range('a', 'z'),
                                    ClassItem::Range('A', 'Z'),
                                    ClassItem::Range('0', '9'),
                                ],
                                negated: false,
                            }),
                            _ => RegexAtom::Named(name),
                        }
                    }
                }
                '[' => {
                    // In Raku regex, [...] is a non-capturing group (alternation)
                    // Parse as alternation: [a|b|c]
                    let mut group_pattern = String::new();
                    let mut depth = 1;
                    for ch in chars.by_ref() {
                        if ch == '[' {
                            depth += 1;
                            group_pattern.push(ch);
                        } else if ch == ']' {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                            group_pattern.push(ch);
                        } else {
                            group_pattern.push(ch);
                        }
                    }
                    // Parse the group as alternation
                    let alternatives: Vec<&str> = group_pattern.split('|').collect();
                    if alternatives.len() > 1 {
                        let mut alt_patterns = Vec::new();
                        for alt in alternatives {
                            if let Some(p) = self.parse_regex(alt) {
                                alt_patterns.push(p);
                            }
                        }
                        RegexAtom::Alternation(alt_patterns)
                    } else if let Some(p) = self.parse_regex(&group_pattern) {
                        RegexAtom::Group(p)
                    } else {
                        continue;
                    }
                }
                other => RegexAtom::Literal(other),
            };
            let mut quant = RegexQuant::One;
            if let Some(q) = chars.peek().copied() {
                quant = match q {
                    '*' => {
                        chars.next();
                        RegexQuant::ZeroOrMore
                    }
                    '+' => {
                        chars.next();
                        RegexQuant::OneOrMore
                    }
                    '?' => {
                        chars.next();
                        RegexQuant::ZeroOrOne
                    }
                    _ => RegexQuant::One,
                };
            }
            tokens.push(RegexToken { atom, quant });
        }
        Some(RegexPattern {
            tokens,
            anchor_start,
            anchor_end,
        })
    }

    fn parse_raku_char_class(&self, inner: &str, negated: bool) -> Option<CharClass> {
        // Parse Raku-style character class: a..z, \n, \t, \c[NAME], \x[HEX], etc.
        let mut items = Vec::new();
        let mut chars = inner.chars().peekable();
        // Track whether all items are from negated escapes (\C, \X)
        let mut all_negated_escapes = true;
        let mut has_items = false;
        while let Some(c) = chars.next() {
            if c == ' ' {
                // Raku regex ignores spaces in char classes
                continue;
            }
            if c == '\\' {
                let esc = chars.next()?;
                match esc {
                    'n' => {
                        items.push(ClassItem::Char('\n'));
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    't' => {
                        items.push(ClassItem::Char('\t'));
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    'r' => {
                        items.push(ClassItem::Char('\r'));
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    'd' => {
                        items.push(ClassItem::Digit);
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    'w' => {
                        items.push(ClassItem::Word);
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    's' => {
                        items.push(ClassItem::Space);
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    'c' | 'C' => {
                        // \c[NAME] or \C[NAME] (negated char) inside character class
                        let is_neg = esc == 'C';
                        if chars.peek() == Some(&'[') {
                            chars.next(); // skip '['
                            let mut name = String::new();
                            let mut bracket_depth = 1;
                            while let Some(&ch) = chars.peek() {
                                if ch == '[' {
                                    bracket_depth += 1;
                                    name.push(ch);
                                    chars.next();
                                } else if ch == ']' {
                                    bracket_depth -= 1;
                                    if bracket_depth == 0 {
                                        chars.next();
                                        break;
                                    }
                                    name.push(ch);
                                    chars.next();
                                } else {
                                    name.push(ch);
                                    chars.next();
                                }
                            }
                            // Handle comma-separated names: \c[NAME1, NAME2]
                            for part in name.split(',') {
                                let part = part.trim();
                                if let Some(ch) = crate::lexer::lookup_unicode_char_by_name(part) {
                                    items.push(ClassItem::Char(ch));
                                    has_items = true;
                                }
                            }
                            if !is_neg {
                                all_negated_escapes = false;
                            }
                        } else {
                            items.push(ClassItem::Char(esc));
                            all_negated_escapes = false;
                            has_items = true;
                        }
                    }
                    'x' | 'X' => {
                        // \x[HEX] or \X[HEX] inside character class
                        let is_neg = esc == 'X';
                        if chars.peek() == Some(&'[') {
                            chars.next(); // skip '['
                            let mut hex = String::new();
                            while let Some(&ch) = chars.peek() {
                                if ch == ']' {
                                    chars.next();
                                    break;
                                }
                                hex.push(ch);
                                chars.next();
                            }
                            if let Some(ch) =
                                u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
                            {
                                items.push(ClassItem::Char(ch));
                                has_items = true;
                            }
                            if !is_neg {
                                all_negated_escapes = false;
                            }
                        } else {
                            items.push(ClassItem::Char(esc));
                            all_negated_escapes = false;
                            has_items = true;
                        }
                    }
                    'o' => {
                        // \o[OCT] inside character class
                        if chars.peek() == Some(&'[') {
                            chars.next(); // skip '['
                            let mut oct = String::new();
                            while let Some(&ch) = chars.peek() {
                                if ch == ']' {
                                    chars.next();
                                    break;
                                }
                                oct.push(ch);
                                chars.next();
                            }
                            if let Some(ch) =
                                u32::from_str_radix(&oct, 8).ok().and_then(char::from_u32)
                            {
                                items.push(ClassItem::Char(ch));
                            }
                        } else {
                            items.push(ClassItem::Char('o'));
                        }
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    other => {
                        items.push(ClassItem::Char(other));
                        all_negated_escapes = false;
                        has_items = true;
                    }
                }
            } else if chars.peek() == Some(&'.') {
                // Check for '..' range syntax
                let mut peek_chars = chars.clone();
                peek_chars.next(); // consume first '.'
                if peek_chars.peek() == Some(&'.') {
                    // It's a range: a..z
                    chars.next(); // consume first '.'
                    chars.next(); // consume second '.'
                    if let Some(end) = chars.next() {
                        items.push(ClassItem::Range(c, end));
                    }
                } else {
                    items.push(ClassItem::Char(c));
                }
                all_negated_escapes = false;
                has_items = true;
            } else {
                items.push(ClassItem::Char(c));
                all_negated_escapes = false;
                has_items = true;
            }
        }
        // If all items came from negated escapes (\C, \X), flip the negation
        // e.g., <[\C[FF]]> means "everything except FF" = negated class of {FF}
        let final_negated = if has_items && all_negated_escapes {
            !negated
        } else {
            negated
        };
        Some(CharClass {
            items,
            negated: final_negated,
        })
    }
}
