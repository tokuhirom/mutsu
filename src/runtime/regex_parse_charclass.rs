use super::regex_parse::*;
use super::*;

impl Interpreter {
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
                // Determine what this escape produces: either a ClassItem (for class
                // escapes like \d, \w) or a simple char (for \n, \t, \[, etc.)
                enum EscResult {
                    Char(char),
                    NegChar(char), // char from \X[HEX] — negated escape
                    Item(ClassItem),
                    Multi, // handled inline (e.g. \c[NAME,NAME])
                }
                let esc_result = match esc {
                    'n' => EscResult::Char('\n'),
                    't' => EscResult::Char('\t'),
                    'r' => EscResult::Char('\r'),
                    'e' => EscResult::Char('\u{001B}'), // escape (ESC)
                    'f' => EscResult::Char('\u{000C}'),
                    'b' => EscResult::Char('\u{0008}'), // backspace
                    'B' => EscResult::NegChar('\u{0008}'), // not backspace
                    '0' => EscResult::Char('\0'),
                    'd' => EscResult::Item(ClassItem::Digit),
                    'D' => EscResult::Item(ClassItem::NegDigit),
                    'w' => EscResult::Item(ClassItem::Word),
                    'W' => EscResult::Item(ClassItem::NegWord),
                    's' => EscResult::Item(ClassItem::Space),
                    'S' => EscResult::Item(ClassItem::NegSpace),
                    'h' => EscResult::Item(ClassItem::HorizSpace),
                    'H' => EscResult::Item(ClassItem::NegHorizSpace),
                    'v' => EscResult::Item(ClassItem::VertSpace),
                    'V' => EscResult::Item(ClassItem::NegVertSpace),
                    'N' => EscResult::Item(ClassItem::NotNewline),
                    'c' | 'C' => {
                        let is_neg = esc == 'C';
                        if chars.peek() == Some(&'[') {
                            chars.next();
                            let mut cname = String::new();
                            let mut bracket_depth = 1;
                            while let Some(&ch) = chars.peek() {
                                if ch == '[' {
                                    bracket_depth += 1;
                                    cname.push(ch);
                                    chars.next();
                                } else if ch == ']' {
                                    bracket_depth -= 1;
                                    if bracket_depth == 0 {
                                        chars.next();
                                        break;
                                    }
                                    cname.push(ch);
                                    chars.next();
                                } else {
                                    cname.push(ch);
                                    chars.next();
                                }
                            }
                            for part in cname.split(',') {
                                let part = part.trim();
                                if let Some(ch) =
                                    crate::token_kind::lookup_unicode_char_by_name(part)
                                {
                                    items.push(ClassItem::Char(ch));
                                }
                            }
                            if !is_neg {
                                all_negated_escapes = false;
                            }
                            EscResult::Multi
                        } else if chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                            // \c32 — decimal codepoint
                            let mut num_str = String::new();
                            while chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                                num_str.push(chars.next().unwrap());
                            }
                            if let Ok(cp) = num_str.parse::<u32>() {
                                if let Some(ch) = char::from_u32(cp) {
                                    if is_neg {
                                        EscResult::NegChar(ch)
                                    } else {
                                        EscResult::Char(ch)
                                    }
                                } else {
                                    EscResult::Char(esc)
                                }
                            } else {
                                EscResult::Char(esc)
                            }
                        } else {
                            EscResult::Char(esc)
                        }
                    }
                    'x' | 'X' => {
                        let is_neg = esc == 'X';
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
                            let result =
                                u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32);
                            if !is_neg {
                                all_negated_escapes = false;
                            }
                            if let Some(ch) = result {
                                if is_neg {
                                    EscResult::NegChar(ch)
                                } else {
                                    EscResult::Char(ch)
                                }
                            } else {
                                EscResult::Multi // couldn't parse, skip
                            }
                        } else if chars.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                            let mut hex = String::new();
                            while chars.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                                hex.push(chars.next().unwrap());
                            }
                            if !is_neg {
                                all_negated_escapes = false;
                            }
                            if let Some(ch) =
                                u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
                            {
                                if is_neg {
                                    EscResult::NegChar(ch)
                                } else {
                                    EscResult::Char(ch)
                                }
                            } else {
                                EscResult::Multi
                            }
                        } else {
                            EscResult::Char(esc)
                        }
                    }
                    'o' => {
                        if chars.peek() == Some(&'[') {
                            chars.next();
                            let mut oct = String::new();
                            while let Some(&ch) = chars.peek() {
                                if ch == ']' {
                                    chars.next();
                                    break;
                                }
                                oct.push(ch);
                                chars.next();
                            }
                            all_negated_escapes = false;
                            if let Some(ch) =
                                u32::from_str_radix(&oct, 8).ok().and_then(char::from_u32)
                            {
                                EscResult::Char(ch)
                            } else {
                                EscResult::Multi
                            }
                        } else {
                            EscResult::Char('o')
                        }
                    }
                    other => EscResult::Char(other),
                };
                match esc_result {
                    EscResult::Char(ch) => {
                        // Check for '..' range with this char as start
                        if Self::peek_dotdot(&chars) {
                            // Skip spaces before '..'
                            while chars.peek() == Some(&' ') {
                                chars.next();
                            }
                            chars.next(); // consume first '.'
                            chars.next(); // consume second '.'
                            while chars.peek() == Some(&' ') {
                                chars.next();
                            }
                            let end = Self::read_cc_char(&mut chars).unwrap_or(ch);
                            if end < ch {
                                PENDING_REGEX_ERROR.with(|e| {
                                    *e.borrow_mut() = Some(RuntimeError::new(format!(
                                        "Illegal reversed character range in regex: {}..{}",
                                        ch, end
                                    )));
                                });
                                return None;
                            }
                            items.push(ClassItem::Range(ch, end));
                        } else {
                            items.push(ClassItem::Char(ch));
                        }
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    EscResult::NegChar(ch) => {
                        // From \X[HEX] — the char itself, but the negation is tracked
                        // via all_negated_escapes (which stays true for \X)
                        items.push(ClassItem::Char(ch));
                        // Don't set all_negated_escapes = false — it stays true
                        has_items = true;
                    }
                    EscResult::Item(item) => {
                        items.push(item);
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    EscResult::Multi => {
                        // Already handled inline
                        has_items = true;
                    }
                }
            } else if chars
                .peek()
                .is_some_and(|ch| unicode_normalization::char::is_combining_mark(*ch))
            {
                // NFG synthetic (base char + combining marks) — cannot be used as range endpoint
                PENDING_REGEX_ERROR.with(|e| {
                    let mut grapheme = c.to_string();
                    while chars
                        .peek()
                        .is_some_and(|ch| unicode_normalization::char::is_combining_mark(*ch))
                    {
                        grapheme.push(chars.next().unwrap());
                    }
                    *e.borrow_mut() = Some(RuntimeError::new(format!(
                        "Cannot use {} as a range endpoint, as it is not a single codepoint",
                        grapheme
                    )));
                });
                return None;
            } else if Self::peek_dotdot(&chars) {
                // Check for '..' range syntax: c..end
                while chars.peek() == Some(&' ') {
                    chars.next();
                } // skip spaces before '..'
                chars.next(); // consume first '.'
                chars.next(); // consume second '.'
                while chars.peek() == Some(&' ') {
                    chars.next();
                }
                let end = Self::read_cc_char(&mut chars).unwrap_or(c);
                if end < c {
                    PENDING_REGEX_ERROR.with(|e| {
                        *e.borrow_mut() = Some(RuntimeError::new(format!(
                            "Illegal reversed character range in regex: {}..{}",
                            c, end
                        )));
                    });
                    return None;
                }
                items.push(ClassItem::Range(c, end));
                all_negated_escapes = false;
                has_items = true;
            } else if c == '-'
                && has_items
                && chars.peek().is_some_and(|&ch| ch != ']' && ch != ' ')
            {
                // Bare '-' between characters in a bracket class is a Perl 5 range syntax error
                PENDING_REGEX_ERROR.with(|e| {
                    *e.borrow_mut() = Some(RuntimeError::new(
                        "Unsupported use of - as character range. In Raku please use: .. for range"
                            .to_string(),
                    ));
                });
                return None;
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

    /// Parse bracket character class expressions like `[a..z]-[aeiou]` or `+[a..z]-[aeiou]-[y]`.
    /// These contain one or more `[...]` parts separated by `+` or `-` operators.
    /// Returns a simple `CharClass` for single parts, or `CompositeClass` for subtraction/union.
    pub(super) fn parse_bracket_char_class(&self, input: &str) -> Option<RegexAtom> {
        // Split input into parts: each part is (+/-) followed by [content]
        let mut positive_items: Vec<ClassItem> = Vec::new();
        let mut negative_items: Vec<ClassItem> = Vec::new();
        let mut remaining = input.trim();

        // First part may be just [content] (implicitly positive) or +[content] or -[content]
        let mut first = true;
        while !remaining.is_empty() {
            let adding;
            if remaining.starts_with('+') {
                adding = true;
                remaining = &remaining[1..];
                // Skip whitespace and embedded comments between + and [
                remaining = Self::skip_charclass_whitespace_and_comments(remaining);
            } else if remaining.starts_with('-') {
                adding = false;
                remaining = &remaining[1..];
                remaining = Self::skip_charclass_whitespace_and_comments(remaining);
            } else if first && remaining.starts_with('[') {
                // Implicit positive for first bare [...]
                adding = true;
            } else {
                break;
            }
            first = false;

            if remaining.starts_with('[') {
                // Find the matching ']', handling backslash escapes
                remaining = &remaining[1..]; // skip '['
                let bracket_end = Self::find_bracket_end(remaining);
                let bracket_content = &remaining[..bracket_end];
                remaining = if bracket_end < remaining.len() {
                    &remaining[bracket_end + 1..] // skip ']'
                } else {
                    ""
                };
                // Skip whitespace after ']'
                remaining = Self::skip_charclass_whitespace_and_comments(remaining);
                // Parse the bracket content as a char class
                if let Some(class) = self.parse_raku_char_class(bracket_content, false) {
                    // If the class itself is negated (e.g. due to \C/\X escapes),
                    // swap the positive/negative assignment
                    let effective_adding = if class.negated { !adding } else { adding };
                    if effective_adding {
                        positive_items.extend(class.items);
                    } else {
                        negative_items.extend(class.items);
                    }
                }
            } else {
                break;
            }
        }

        if positive_items.is_empty() && negative_items.is_empty() {
            // <[]> or <-[]> — empty bracket class: always fails (matches no character)
            return Some(RegexAtom::CharClass(CharClass {
                items: vec![],
                negated: false,
            }));
        }

        if negative_items.is_empty() {
            // Simple character class with no subtraction
            Some(RegexAtom::CharClass(CharClass {
                items: positive_items,
                negated: false,
            }))
        } else if positive_items.is_empty() {
            // Purely negated class: <-[aeiou]> = match anything NOT in [aeiou]
            Some(RegexAtom::CharClass(CharClass {
                items: negative_items,
                negated: true,
            }))
        } else {
            Some(RegexAtom::CompositeClass {
                positive: positive_items,
                negative: negative_items,
            })
        }
    }

    /// Find the position of the closing ']' in a bracket character class,
    /// handling backslash escapes so that `\]` doesn't end the class,
    /// and `\c[...]`, `\x[...]`, `\C[...]`, `\X[...]`, `\o[...]` nested brackets.
    fn find_bracket_end(s: &str) -> usize {
        let mut chars = s.chars().peekable();
        let mut pos = 0;
        while let Some(c) = chars.next() {
            if c == '\\' {
                pos += c.len_utf8();
                // Skip the escaped character
                if let Some(esc) = chars.next() {
                    pos += esc.len_utf8();
                    // Handle \c[...], \C[...], \x[...], \X[...], \o[...] nested brackets
                    if matches!(esc, 'c' | 'C' | 'x' | 'X' | 'o') && chars.peek() == Some(&'[') {
                        let bracket = chars.next().unwrap();
                        pos += bracket.len_utf8();
                        let mut depth = 1;
                        for ch in chars.by_ref() {
                            pos += ch.len_utf8();
                            if ch == '[' {
                                depth += 1;
                            } else if ch == ']' {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                        }
                    }
                }
            } else if c == ']' {
                return pos;
            } else {
                pos += c.len_utf8();
            }
        }
        s.len()
    }

    /// Skip whitespace and embedded comments (#`[...]) in character class expressions.
    fn skip_charclass_whitespace_and_comments(s: &str) -> &str {
        let mut remaining = s;
        loop {
            let before = remaining;
            remaining = remaining.trim_start();
            // Handle embedded comments: #`[...]
            if remaining.starts_with("#`[") {
                let mut depth = 0;
                let mut chars = remaining.chars();
                let mut count = 0;
                for ch in chars.by_ref() {
                    count += ch.len_utf8();
                    if ch == '[' {
                        depth += 1;
                    } else if ch == ']' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                }
                remaining = &remaining[count..];
            }
            if remaining.len() == before.len() {
                break;
            }
        }
        remaining
    }

    /// Check if the next non-space chars in a peekable iterator are `..` (range syntax).
    fn peek_dotdot(chars: &std::iter::Peekable<std::str::Chars<'_>>) -> bool {
        let mut peek = chars.clone();
        // Skip spaces (insignificant in Raku char classes)
        while peek.peek() == Some(&' ') {
            peek.next();
        }
        peek.next() == Some('.') && peek.peek() == Some(&'.')
    }

    /// Read a single character from a character class, handling escape sequences.
    /// Used for reading range endpoints like the `z` in `a..z` or `\]` in `\[..\]`.
    fn read_cc_char(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Option<char> {
        let ch = chars.next()?;
        if ch == '\\' {
            let esc = chars.next()?;
            match esc {
                'n' => Some('\n'),
                't' => Some('\t'),
                'r' => Some('\r'),
                'e' => Some('\u{001B}'),
                'f' => Some('\u{000C}'),
                '0' => Some('\0'),
                'c' => {
                    if chars.peek() == Some(&'[') {
                        chars.next();
                        let mut cname = String::new();
                        while let Some(&c) = chars.peek() {
                            if c == ']' {
                                chars.next();
                                break;
                            }
                            cname.push(c);
                            chars.next();
                        }
                        crate::token_kind::lookup_unicode_char_by_name(cname.trim())
                    } else if chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                        let mut num_str = String::new();
                        while chars.peek().is_some_and(|c| c.is_ascii_digit()) {
                            num_str.push(chars.next().unwrap());
                        }
                        num_str.parse::<u32>().ok().and_then(char::from_u32)
                    } else {
                        Some(esc)
                    }
                }
                'x' => {
                    if chars.peek() == Some(&'[') {
                        chars.next();
                        let mut hex = String::new();
                        while let Some(&c) = chars.peek() {
                            if c == ']' {
                                chars.next();
                                break;
                            }
                            hex.push(c);
                            chars.next();
                        }
                        u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
                    } else if chars.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                        let mut hex = String::new();
                        while chars.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                            hex.push(chars.next().unwrap());
                        }
                        u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
                    } else {
                        Some(esc)
                    }
                }
                other => Some(other),
            }
        } else {
            Some(ch)
        }
    }

    /// Parse combined character class like `+ xdigit - lower` or `+ :HexDigit - :Upper`.
    /// Also handles bracket classes: `+ [a..z] - [aeiou]`.
    pub(super) fn parse_combined_class(
        &self,
        input: &str,
        mode: RegexParseMode,
    ) -> Option<RegexAtom> {
        let mut positive_items: Vec<ClassItem> = Vec::new();
        let mut negative_items: Vec<ClassItem> = Vec::new();
        let mut remaining = input.trim();
        // A leading bracket class with no sign (`[a..z] +digit`) is an implicit
        // positive first item: the `+`/`-` only separates the subsequent parts.
        let mut implicit_first = remaining.starts_with('[');
        while !remaining.is_empty() {
            let adding;
            if implicit_first {
                adding = true;
                implicit_first = false;
            } else if remaining.starts_with('+') {
                adding = true;
                remaining = remaining[1..].trim_start();
            } else if remaining.starts_with('-') {
                adding = false;
                remaining = remaining[1..].trim_start();
            } else {
                break;
            }

            // Check if this part is a bracket class [...]
            if remaining.starts_with('[') {
                remaining = &remaining[1..]; // skip '['
                let bracket_end = Self::find_bracket_end(remaining);
                let bracket_content = &remaining[..bracket_end];
                remaining = if bracket_end < remaining.len() {
                    &remaining[bracket_end + 1..] // skip ']'
                } else {
                    ""
                };
                remaining = remaining.trim_start();
                if let Some(class) = self.parse_raku_char_class(bracket_content, false) {
                    if adding {
                        positive_items.extend(class.items);
                    } else {
                        negative_items.extend(class.items);
                    }
                }
            } else {
                // Named class or Unicode property
                let class_end = Self::find_combined_class_part_end(remaining);
                let class_name = remaining[..class_end].trim();
                remaining = remaining[class_end..].trim_start();
                let item = if let Some(prop) = class_name.strip_prefix(':') {
                    ClassItem::UnicodePropItem {
                        name: prop.to_string(),
                        negated: false,
                    }
                } else {
                    // Check if this is a known built-in character class name
                    let is_known_builtin = matches!(
                        class_name,
                        "alpha"
                            | "upper"
                            | "lower"
                            | "digit"
                            | "xdigit"
                            | "space"
                            | "alnum"
                            | "blank"
                            | "cntrl"
                            | "punct"
                            | "graph"
                            | "print"
                            | "ws"
                    );
                    if !is_known_builtin {
                        // Check if the name resolves as a grammar token in the current package
                        let is_grammar_token = !self.current_package().is_empty()
                            && self.resolve_token_defs(class_name).is_some();
                        // "No such method" is a runtime resolution failure, not a
                        // parse-time syntax error: such patterns are meant to die
                        // at match time (`dies-ok`), not at compile time. In
                        // `Validate` mode treat the unknown name as opaque.
                        if !is_grammar_token && mode == RegexParseMode::Match {
                            let msg = format!(
                                "No such method '{}' for invocant of type 'Match'",
                                class_name
                            );
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(RuntimeError::new(msg));
                            });
                            return None;
                        }
                    }
                    ClassItem::NamedBuiltin(class_name.to_string())
                };
                if adding {
                    positive_items.push(item);
                } else {
                    negative_items.push(item);
                }
            }
        }
        if positive_items.is_empty() && negative_items.is_empty() {
            return None;
        }
        if positive_items.is_empty() {
            // Purely negated: <-alpha> means "any character NOT matching alpha"
            Some(RegexAtom::CharClass(CharClass {
                items: negative_items,
                negated: true,
            }))
        } else {
            Some(RegexAtom::CompositeClass {
                positive: positive_items,
                negative: negative_items,
            })
        }
    }

    /// Whether a bracket-class string (`[a..z] +digit`) is followed by a
    /// `+`/`-` named-class combination after its closing `]`. Used to route
    /// `<[...] +name>` enumerated classes to `parse_combined_class`.
    pub(super) fn bracket_class_has_combination_tail(s: &str) -> bool {
        let Some(after_open) = s.strip_prefix('[') else {
            return false;
        };
        let bracket_end = Self::find_bracket_end(after_open);
        if bracket_end >= after_open.len() {
            return false; // unterminated bracket
        }
        let tail = after_open[bracket_end + 1..].trim_start();
        tail.starts_with('+') || tail.starts_with('-')
    }

    /// Find the end of a named class part in a combined class expression.
    /// Stops at `+`, `-`, or end of string, but handles kebab-case names.
    fn find_combined_class_part_end(s: &str) -> usize {
        // Look for + or - that isn't part of a kebab-case name
        // A kebab-case name uses - between word characters
        let bytes = s.as_bytes();
        for i in 0..bytes.len() {
            if bytes[i] == b'+' {
                return i;
            }
            if bytes[i] == b'-' {
                // Check if this is a kebab separator (between word chars)
                let prev_is_word =
                    i > 0 && (bytes[i - 1].is_ascii_alphanumeric() || bytes[i - 1] == b'_');
                let next_is_word = i + 1 < bytes.len()
                    && (bytes[i + 1].is_ascii_alphanumeric() || bytes[i + 1] == b'_');
                if prev_is_word && next_is_word {
                    // It's a kebab separator, continue
                    continue;
                }
                return i;
            }
        }
        s.len()
    }

    /// Evaluate a string as Raku source code and return the result value.
    /// Used for @(expr) interpolation in regex patterns.
    pub(super) fn eval_string_as_source(&self, code: &str) -> Value {
        let parsed = crate::parse_dispatch::parse_source(code);
        let (stmts, _) = match parsed {
            Ok(v) => v,
            Err(_) => return Value::Nil,
        };
        let mut interp = Interpreter {
            env: self.env.clone(),
            current_package: Arc::new(RwLock::new(self.current_package())),
            ..Default::default()
        };
        self.copy_decl_registry_into(&mut interp);
        match interp.eval_block_value(&stmts) {
            Ok(v) => v,
            Err(e) => e.return_value.unwrap_or(Value::Nil),
        }
    }
}
