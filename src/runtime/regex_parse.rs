use super::*;

impl Interpreter {
    pub(super) fn parse_regex(&self, pattern: &str) -> Option<RegexPattern> {
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
                                    if let Some(c) =
                                        crate::token_kind::lookup_unicode_char_by_name(part)
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
                                if let Some(c) =
                                    crate::token_kind::lookup_unicode_char_by_name(&name)
                                {
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
                    } else if trimmed.starts_with('+') || trimmed.starts_with('-') {
                        // Combined character class: <+ xdigit - lower>
                        if let Some(atom) = self.parse_combined_class(trimmed) {
                            atom
                        } else {
                            continue;
                        }
                    } else {
                        // Check for named character classes
                        match trimmed {
                            "alpha" | "upper" | "lower" | "digit" | "xdigit" | "space" | "ws"
                            | "alnum" | "blank" | "cntrl" | "punct" => {
                                RegexAtom::CharClass(CharClass {
                                    items: vec![ClassItem::NamedBuiltin(trimmed.to_string())],
                                    negated: false,
                                })
                            }
                            "ident" => {
                                // <ident> = <alpha> <alnum>*
                                RegexAtom::Group(RegexPattern {
                                    tokens: vec![
                                        RegexToken {
                                            atom: RegexAtom::CharClass(CharClass {
                                                items: vec![ClassItem::NamedBuiltin(
                                                    "alpha".to_string(),
                                                )],
                                                negated: false,
                                            }),
                                            quant: RegexQuant::One,
                                        },
                                        RegexToken {
                                            atom: RegexAtom::CharClass(CharClass {
                                                items: vec![ClassItem::NamedBuiltin(
                                                    "alnum".to_string(),
                                                )],
                                                negated: false,
                                            }),
                                            quant: RegexQuant::ZeroOrMore,
                                        },
                                    ],
                                    anchor_start: false,
                                    anchor_end: false,
                                })
                            }
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

    pub(super) fn parse_raku_char_class(&self, inner: &str, negated: bool) -> Option<CharClass> {
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
                                if let Some(ch) =
                                    crate::token_kind::lookup_unicode_char_by_name(part)
                                {
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

    /// Parse combined character class like `+ xdigit - lower` or `+ :HexDigit - :Upper`.
    fn parse_combined_class(&self, input: &str) -> Option<RegexAtom> {
        let mut positive_items: Vec<ClassItem> = Vec::new();
        let mut negative_items: Vec<ClassItem> = Vec::new();
        let mut remaining = input.trim();
        while !remaining.is_empty() {
            let adding;
            if remaining.starts_with('+') {
                adding = true;
                remaining = remaining[1..].trim_start();
            } else if remaining.starts_with('-') {
                adding = false;
                remaining = remaining[1..].trim_start();
            } else {
                break;
            }
            let class_end = remaining.find(['+', '-']).unwrap_or(remaining.len());
            let class_name = remaining[..class_end].trim();
            remaining = remaining[class_end..].trim_start();
            let item = if let Some(prop) = class_name.strip_prefix(':') {
                ClassItem::UnicodePropItem {
                    name: prop.to_string(),
                    negated: false,
                }
            } else {
                ClassItem::NamedBuiltin(class_name.to_string())
            };
            if adding {
                positive_items.push(item);
            } else {
                negative_items.push(item);
            }
        }
        if positive_items.is_empty() {
            return None;
        }
        Some(RegexAtom::CompositeClass {
            positive: positive_items,
            negative: negative_items,
        })
    }
}
