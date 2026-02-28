use super::*;
use ::regex::Regex;

fn regex_single_quote_closes(open: char, ch: char) -> bool {
    match open {
        '\'' => ch == '\'',
        '\u{2018}' => ch == '\u{2019}',                     // ‘...’
        '\u{201A}' => ch == '\u{2019}' || ch == '\u{2018}', // ‚...’ and ‚...‘
        '\u{FF62}' => ch == '\u{FF63}',                     // ｢...｣
        _ => false,
    }
}

fn regex_single_quote_atom(literal: String, ignore_case: bool) -> RegexAtom {
    let lit_chars: Vec<char> = literal.chars().collect();
    if lit_chars.is_empty() {
        // Empty string literal matches zero-width
        RegexAtom::ZeroWidth
    } else if lit_chars.len() == 1 {
        RegexAtom::Literal(lit_chars[0])
    } else {
        let tokens = lit_chars
            .into_iter()
            .map(|ch| RegexToken {
                atom: RegexAtom::Literal(ch),
                quant: RegexQuant::One,
                named_capture: None,
                ratchet: false,
            })
            .collect();
        RegexAtom::Group(RegexPattern {
            tokens,
            anchor_start: false,
            anchor_end: false,
            ignore_case,
            ignore_mark: false,
        })
    }
}

/// Split a property name like "NumericValue(0 ^..^ 1)" into ("NumericValue", Some("0 ^..^ 1")).
fn split_prop_args(s: &str) -> (&str, Option<&str>) {
    if let Some(open) = s.find('(')
        && let Some(close) = s.rfind(')')
    {
        return (&s[..open], Some(&s[open + 1..close]));
    }
    (s, None)
}

impl Interpreter {
    /// Split a regex pattern on top-level `|` or `||` alternation operators.
    /// Respects grouping: `(...)`, `[...]`, `{...}`, `<...>` and escapes.
    fn split_top_level_alternation(pattern: &str) -> Vec<String> {
        let mut parts = Vec::new();
        let mut current = String::new();
        let mut depth_paren = 0i32;
        let mut depth_bracket = 0i32;
        let mut depth_brace = 0i32;
        let mut depth_angle = 0i32;
        let mut escaped = false;
        let mut in_single_quote = false;
        let mut in_double_quote = false;
        let mut chars = pattern.chars().peekable();

        while let Some(ch) = chars.next() {
            if escaped {
                current.push(ch);
                escaped = false;
                continue;
            }
            if ch == '\\' {
                current.push(ch);
                escaped = true;
                continue;
            }
            if ch == '\'' && !in_double_quote {
                in_single_quote = !in_single_quote;
                current.push(ch);
                continue;
            }
            if ch == '"' && !in_single_quote {
                in_double_quote = !in_double_quote;
                current.push(ch);
                continue;
            }
            if in_single_quote || in_double_quote {
                current.push(ch);
                continue;
            }
            match ch {
                '(' => {
                    depth_paren += 1;
                    current.push(ch);
                }
                ')' => {
                    depth_paren -= 1;
                    current.push(ch);
                }
                '[' => {
                    depth_bracket += 1;
                    current.push(ch);
                }
                ']' => {
                    depth_bracket -= 1;
                    current.push(ch);
                }
                '{' => {
                    depth_brace += 1;
                    current.push(ch);
                }
                '}' => {
                    depth_brace -= 1;
                    current.push(ch);
                }
                '<' => {
                    depth_angle += 1;
                    current.push(ch);
                }
                '>' => {
                    depth_angle -= 1;
                    current.push(ch);
                }
                '|' if depth_paren == 0
                    && depth_bracket == 0
                    && depth_brace == 0
                    && depth_angle == 0 =>
                {
                    // Skip second | for ||
                    if chars.peek() == Some(&'|') {
                        chars.next();
                    }
                    parts.push(std::mem::take(&mut current));
                }
                _ => current.push(ch),
            }
        }
        if !current.is_empty() || !parts.is_empty() {
            parts.push(current);
        }
        parts
    }

    fn has_unquoted_ltm_separator(pattern: &str) -> bool {
        let mut in_single = false;
        let mut in_double = false;
        let mut escaped = false;
        for ch in pattern.chars() {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == '\'' && !in_double {
                in_single = !in_single;
                continue;
            }
            if ch == '"' && !in_single {
                in_double = !in_double;
                continue;
            }
            if !in_single && !in_double && ch == '%' {
                return true;
            }
        }
        false
    }

    fn split_simple_quantified_atom(atom: &str) -> Option<(String, String)> {
        let quant = atom.chars().last()?;
        if !matches!(quant, '?' | '+' | '*') {
            return None;
        }
        let body = &atom[..atom.len() - quant.len_utf8()];
        if body.is_empty() {
            return None;
        }
        let base = body.chars().last()?;
        // Do not split when the quantified part is likely a grouped/paired atom,
        // a quoted atom, or an escaped atom like \w.
        if matches!(
            base,
            ']' | ')' | '}' | '>' | '\'' | '"' | '\u{2019}' | '\u{201D}' | '\u{00BB}' | '\u{FF63}'
        ) {
            return None;
        }
        let prefix = &body[..body.len() - base.len_utf8()];
        if prefix.ends_with('\\') {
            return None;
        }
        Some((prefix.to_string(), format!("{base}{quant}")))
    }

    fn expand_ltm_pattern(pattern: &str) -> String {
        let compact: String = pattern.chars().filter(|ch| !ch.is_whitespace()).collect();
        if compact.is_empty() {
            return pattern.to_string();
        }

        let with_count = Regex::new(r"^(.+?)\*\*([0-9]+(?:\.\.(?:[0-9]+|\*))?)(?:(%%|%)(.+))?$")
            .expect("ltm count regex is valid");
        let bare_sep = Regex::new(r"^(.+?)(%%|%)(.+)$").expect("ltm sep regex is valid");

        if let Some(caps) = with_count.captures(&compact) {
            let atom = caps.get(1).map(|m| m.as_str()).unwrap_or_default();
            let count_spec = caps.get(2).map(|m| m.as_str()).unwrap_or_default();
            let sep_mode = caps.get(3).map(|m| m.as_str());
            let sep = caps.get(4).map(|m| m.as_str());
            return Self::build_ltm_expansion(atom, count_spec, sep_mode, sep);
        }
        if !Self::has_unquoted_ltm_separator(pattern) {
            return pattern.to_string();
        }
        if let Some(caps) = bare_sep.captures(&compact) {
            let atom = caps
                .get(1)
                .map(|m| m.as_str())
                .unwrap_or_default()
                .to_string();
            let sep_mode = caps.get(2).map(|m| m.as_str());
            let sep = caps.get(3).map(|m| m.as_str());
            if let Some((prefix, quantified_tail)) = Self::split_simple_quantified_atom(&atom) {
                return format!(
                    "{}{}",
                    prefix,
                    Self::build_ltm_expansion(&quantified_tail, "1..*", sep_mode, sep)
                );
            }
            return Self::build_ltm_expansion(&atom, "1..*", sep_mode, sep);
        }
        pattern.to_string()
    }

    fn build_ltm_expansion(
        atom: &str,
        count_spec: &str,
        sep_mode: Option<&str>,
        sep: Option<&str>,
    ) -> String {
        let (min, max) = if let Some((min_str, max_str)) = count_spec.split_once("..") {
            let min = min_str.parse::<usize>().unwrap_or(0);
            let max = if max_str == "*" {
                None
            } else {
                max_str.parse::<usize>().ok()
            };
            (min, max)
        } else {
            let exact = count_spec.parse::<usize>().unwrap_or(1);
            (exact, Some(exact))
        };

        let allow_trailing_sep = matches!(sep_mode, Some("%%"));
        let sep = sep.unwrap_or_default();

        let repeat_atom = |count: usize| -> String { atom.repeat(count) };
        let build_exact_list = |count: usize| -> String {
            if count == 0 {
                return String::new();
            }
            let mut out = atom.to_string();
            for _ in 1..count {
                out.push_str(sep);
                out.push_str(atom);
            }
            if allow_trailing_sep {
                out.push_str(&format!("({sep})?"));
            }
            out
        };

        if sep_mode.is_none() {
            return match max {
                Some(max) if max == min => repeat_atom(min),
                Some(max) => {
                    let alts: Vec<String> = (min..=max).rev().map(repeat_atom).collect();
                    if alts.len() == 1 {
                        alts.into_iter().next().unwrap_or_default()
                    } else {
                        format!("({})", alts.join("|"))
                    }
                }
                None => format!("{}({atom})*", repeat_atom(min)),
            };
        }

        if max.is_none() && min == 1 && atom.ends_with('?') && !sep.is_empty() {
            if allow_trailing_sep {
                return format!("{atom}({sep})?");
            }
            return atom.to_string();
        }

        match max {
            Some(max) if max == min => build_exact_list(min),
            Some(max) => {
                let alts: Vec<String> = (min..=max).map(build_exact_list).collect();
                if alts.len() == 1 {
                    alts.into_iter().next().unwrap_or_default()
                } else {
                    format!("({})", alts.join("|"))
                }
            }
            None => {
                let mut out = build_exact_list(min);
                out.push_str(&format!("({sep}{atom})*"));
                if allow_trailing_sep {
                    out.push_str(&format!("({sep})?"));
                }
                out
            }
        }
    }

    pub(super) fn parse_regex(&self, pattern: &str) -> Option<RegexPattern> {
        fn named_lookup_is_ws(name: &str) -> bool {
            let mut raw = name.trim();
            if let Some(stripped) = raw.strip_prefix('.') {
                raw = stripped.trim();
            }
            if let Some(stripped) = raw.strip_prefix('&') {
                raw = stripped.trim();
            }
            raw == "ws"
        }

        fn token_is_ws_like(token: &RegexToken) -> bool {
            match &token.atom {
                RegexAtom::CharClass(class) => {
                    !class.negated
                        && class.items.len() == 1
                        && class
                            .items
                            .first()
                            .is_some_and(|item| matches!(item, ClassItem::Space))
                }
                RegexAtom::Named(name) => named_lookup_is_ws(name),
                _ => false,
            }
        }

        let interpolated = self.interpolate_regex_scalars(pattern);
        let mut source = interpolated.trim_start();
        let mut ignore_case = false;
        let mut ignore_mark = false;
        let mut sigspace = false;
        let mut ratchet = false;
        loop {
            if let Some(rest) = source.strip_prefix(":ignorecase") {
                ignore_case = true;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":ignoremark") {
                ignore_mark = true;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":ratchet") {
                ratchet = true;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":i") {
                ignore_case = true;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":sigspace") {
                sigspace = true;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":s") {
                sigspace = true;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":r") {
                // Make sure it's :r and not :ratchet (already handled) or other identifiers
                if rest.is_empty()
                    || rest.starts_with(' ')
                    || rest.starts_with(':')
                    || rest.starts_with('/')
                {
                    ratchet = true;
                    source = rest.trim_start();
                    continue;
                }
            }
            if let Some(rest) = source.strip_prefix(":m") {
                // Make sure it's :m and not :mm or :my or other identifiers
                if rest.is_empty()
                    || rest.starts_with(' ')
                    || rest.starts_with(':')
                    || rest.starts_with('/')
                {
                    ignore_mark = true;
                    source = rest.trim_start();
                    continue;
                }
            }
            break;
        }
        source = if sigspace {
            source.trim_start()
        } else {
            source.trim()
        };
        // For multiline :s patterns, trailing indentation before the closing
        // delimiter is layout whitespace, not a semantic sigspace token.
        if sigspace && source.contains('\n') {
            source = source.trim_end();
        }
        // Quote-word delimiters in regex patterns (e.g., «word») represent
        // literal text content, not literal guillemet characters.
        if let Some(inner) = source.strip_prefix('«').and_then(|s| s.strip_suffix('»')) {
            source = inner.trim();
        } else if let Some(inner) = source.strip_prefix("<<").and_then(|s| s.strip_suffix(">>")) {
            source = inner.trim();
        }

        // Handle top-level alternation (| or ||)
        let top_alts = Self::split_top_level_alternation(source);
        if top_alts.len() > 1 {
            let mut alt_patterns = Vec::new();
            for alt in &top_alts {
                let alt_src = alt.trim();
                if alt_src.is_empty() {
                    continue;
                }
                // Re-apply inline adverbs for each alternative
                let alt_pat = if ignore_case && !alt_src.starts_with(":i") {
                    format!(":i {}", alt_src)
                } else {
                    alt_src.to_string()
                };
                if let Some(p) = self.parse_regex(&alt_pat) {
                    alt_patterns.push(p);
                }
            }
            if alt_patterns.len() > 1 {
                return Some(RegexPattern {
                    tokens: vec![RegexToken {
                        atom: RegexAtom::Alternation(alt_patterns),
                        quant: RegexQuant::One,
                        named_capture: None,
                        ratchet: false,
                    }],
                    anchor_start: false,
                    anchor_end: false,
                    ignore_case,
                    ignore_mark,
                });
            } else if alt_patterns.len() == 1 {
                return alt_patterns.into_iter().next();
            }
        }

        let expanded = Self::expand_ltm_pattern(source);
        let mut chars = expanded.chars().peekable();
        let mut tokens = Vec::new();
        let mut anchor_start = false;
        let mut anchor_end = false;
        let mut pending_named_capture: Option<String> = None;
        let mut pending_builtin_named_capture: Option<String> = None;
        while let Some(c) = chars.next() {
            // In Raku, unescaped whitespace in regex is insignificant
            if c.is_whitespace() {
                if sigspace {
                    while chars.peek().is_some_and(|next| next.is_whitespace()) {
                        chars.next();
                    }
                    if tokens.is_empty() {
                        if anchor_start {
                            tokens.push(RegexToken {
                                atom: RegexAtom::CharClass(CharClass {
                                    negated: false,
                                    items: vec![ClassItem::Space],
                                }),
                                quant: RegexQuant::ZeroOrMore,
                                named_capture: None,
                                ratchet,
                            });
                        }
                    } else {
                        if tokens.last().is_some_and(token_is_ws_like) {
                            continue;
                        }
                        let next_is_anchor_end = {
                            let mut lookahead = chars.clone();
                            while lookahead.next_if(|ch| ch.is_whitespace()).is_some() {}
                            lookahead.peek().is_some_and(|ch| *ch == '$')
                                && lookahead.clone().skip(1).all(|ch| ch.is_whitespace())
                        };
                        tokens.push(RegexToken {
                            atom: RegexAtom::CharClass(CharClass {
                                negated: false,
                                items: vec![ClassItem::Space],
                            }),
                            quant: if next_is_anchor_end {
                                RegexQuant::ZeroOrMore
                            } else {
                                RegexQuant::OneOrMore
                            },
                            named_capture: None,
                            ratchet,
                        });
                    }
                }
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
            if c == '$' && chars.peek() == Some(&'<') {
                chars.next();
                let mut capture_name = String::new();
                for ch in chars.by_ref() {
                    if ch == '>' {
                        break;
                    }
                    capture_name.push(ch);
                }
                if !capture_name.is_empty() {
                    while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                        chars.next();
                    }
                    if chars.peek() == Some(&'=') {
                        chars.next();
                        while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                            chars.next();
                        }
                        pending_named_capture = Some(capture_name);
                        continue;
                    }
                }
                tokens.push(RegexToken {
                    atom: RegexAtom::Literal('$'),
                    quant: RegexQuant::One,
                    named_capture: None,
                    ratchet: false,
                });
                continue;
            }
            // Handle :my, :our, :constant variable declarations in regex
            if c == ':' {
                let remaining: String = chars.clone().collect();
                if remaining.starts_with("my ")
                    || remaining.starts_with("our ")
                    || remaining.starts_with("constant ")
                {
                    // Collect everything up to and including the semicolon
                    let mut decl_code = String::new();
                    for ch in chars.by_ref() {
                        if ch == ';' {
                            break;
                        }
                        decl_code.push(ch);
                    }
                    tokens.push(RegexToken {
                        atom: RegexAtom::VarDecl { code: decl_code },
                        quant: RegexQuant::One,
                        named_capture: pending_named_capture.take(),
                        ratchet,
                    });
                    continue;
                }
            }
            let atom = match c {
                '.' => RegexAtom::Any,
                '\\' => {
                    let esc = chars.next()?;
                    match esc {
                        'd' => RegexAtom::CharClass(CharClass {
                            negated: false,
                            items: vec![ClassItem::Digit],
                        }),
                        'D' => RegexAtom::CharClass(CharClass {
                            negated: true,
                            items: vec![ClassItem::Digit],
                        }),
                        'w' => RegexAtom::CharClass(CharClass {
                            negated: false,
                            items: vec![ClassItem::Word],
                        }),
                        'W' => RegexAtom::CharClass(CharClass {
                            negated: true,
                            items: vec![ClassItem::Word],
                        }),
                        's' => RegexAtom::CharClass(CharClass {
                            negated: false,
                            items: vec![ClassItem::Space],
                        }),
                        'S' => RegexAtom::CharClass(CharClass {
                            negated: true,
                            items: vec![ClassItem::Space],
                        }),
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
                                        named_capture: None,
                                        ratchet: false,
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
                '\'' | '\u{2018}' | '\u{201A}' | '\u{FF62}' => {
                    // Quoted literal string in Raku regex: 'foo-bar' matches literally
                    let mut literal = String::new();
                    for ch in chars.by_ref() {
                        if regex_single_quote_closes(c, ch) {
                            break;
                        }
                        literal.push(ch);
                    }
                    regex_single_quote_atom(literal, ignore_case)
                }
                '"' | '\u{201C}' | '\u{201E}' => {
                    // Double-quoted literal string in Raku regex: "foo" matches literally
                    // TODO: support interpolation inside double-quoted regex strings
                    let close = match c {
                        '"' => '"',
                        '\u{201C}' | '\u{201E}' => '\u{201D}',
                        _ => unreachable!(),
                    };
                    let mut literal = String::new();
                    loop {
                        match chars.next() {
                            Some('\\') => match chars.next() {
                                Some('n') => literal.push('\n'),
                                Some('t') => literal.push('\t'),
                                Some('r') => literal.push('\r'),
                                Some('0') => literal.push('\0'),
                                Some(other) => literal.push(other),
                                None => break,
                            },
                            Some(ch) if ch == close => break,
                            Some(ch) => literal.push(ch),
                            None => break,
                        }
                    }
                    regex_single_quote_atom(literal, ignore_case)
                }
                '<' => {
                    if chars.peek() == Some(&'(') {
                        chars.next();
                        RegexAtom::CaptureStartMarker
                    } else {
                        // Check for code assertion: <?{...}> or <!{...}>
                        // These need special handling because code may contain < and >
                        let peek_str: String = chars.clone().collect();
                        if peek_str.starts_with("?{")
                            || peek_str.starts_with("!{")
                            || peek_str.starts_with('{')
                        {
                            let is_closure_interp = peek_str.starts_with('{');
                            let negated = !is_closure_interp && peek_str.starts_with('!');
                            if !is_closure_interp {
                                // Skip '?' or '!'
                                chars.next();
                            }
                            // Skip '{'
                            chars.next();
                            let mut code = String::new();
                            let mut brace_depth = 1usize;
                            for ch in chars.by_ref() {
                                if ch == '{' {
                                    brace_depth += 1;
                                    code.push(ch);
                                } else if ch == '}' {
                                    brace_depth -= 1;
                                    if brace_depth == 0 {
                                        break;
                                    }
                                    code.push(ch);
                                } else {
                                    code.push(ch);
                                }
                            }
                            // Consume the closing '>'
                            if chars.peek() == Some(&'>') {
                                chars.next();
                            }
                            if is_closure_interp {
                                RegexAtom::ClosureInterpolation { code }
                            } else {
                                RegexAtom::CodeAssertion {
                                    code,
                                    negated,
                                    is_assertion: true,
                                }
                            }
                        } else {
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
                                let (pname, pargs) = split_prop_args(prop_name);
                                RegexAtom::UnicodeProp {
                                    name: pname.to_string(),
                                    negated: true,
                                    args: pargs.map(|s| s.to_string()),
                                }
                            } else if let Some(prop_name) = trimmed.strip_prefix(':') {
                                // <:PropName> — Unicode property assertion
                                let (pname, pargs) = split_prop_args(prop_name);
                                RegexAtom::UnicodeProp {
                                    name: pname.to_string(),
                                    negated: false,
                                    args: pargs.map(|s| s.to_string()),
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
                                    "alpha" | "upper" | "lower" | "digit" | "xdigit" | "space"
                                    | "alnum" | "blank" | "cntrl" | "punct" => {
                                        // Set builtin named capture so $<alpha>, $<digit>, etc. work
                                        pending_builtin_named_capture = Some(trimmed.to_string());
                                        RegexAtom::CharClass(CharClass {
                                            items: vec![ClassItem::NamedBuiltin(
                                                trimmed.to_string(),
                                            )],
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
                                                    named_capture: None,
                                                    ratchet: false,
                                                },
                                                RegexToken {
                                                    atom: RegexAtom::CharClass(CharClass {
                                                        items: vec![ClassItem::NamedBuiltin(
                                                            "alnum".to_string(),
                                                        )],
                                                        negated: false,
                                                    }),
                                                    quant: RegexQuant::ZeroOrMore,
                                                    named_capture: None,
                                                    ratchet: false,
                                                },
                                            ],
                                            anchor_start: false,
                                            anchor_end: false,
                                            ignore_case,
                                            ignore_mark,
                                        })
                                    }
                                    _ => RegexAtom::Named(name),
                                }
                            }
                        } // close else for code assertion special case
                    }
                }
                ')' if chars.peek() == Some(&'>') => {
                    chars.next();
                    RegexAtom::CaptureEndMarker
                }
                '(' => {
                    // Capture group: (...)
                    let mut group_pattern = String::new();
                    let mut depth = 1;
                    for ch in chars.by_ref() {
                        if ch == '(' {
                            depth += 1;
                            group_pattern.push(ch);
                        } else if ch == ')' {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                            group_pattern.push(ch);
                        } else {
                            group_pattern.push(ch);
                        }
                    }
                    let alternatives = Self::split_top_level_alternation(&group_pattern);
                    if alternatives.len() > 1 {
                        let mut alt_patterns = Vec::new();
                        for alt in alternatives {
                            let parsed_alt = if ignore_case || sigspace {
                                let mut scoped = String::new();
                                if ignore_case {
                                    scoped.push_str(":i ");
                                }
                                if sigspace {
                                    scoped.push_str(":s ");
                                }
                                scoped.push_str(alt.trim_end());
                                self.parse_regex(&scoped)
                            } else {
                                self.parse_regex(&alt)
                            };
                            if let Some(p) = parsed_alt {
                                alt_patterns.push(p);
                            }
                        }
                        let group_pat = RegexPattern {
                            tokens: vec![RegexToken {
                                atom: RegexAtom::Alternation(alt_patterns),
                                quant: RegexQuant::One,
                                named_capture: None,
                                ratchet: false,
                            }],
                            anchor_start: false,
                            anchor_end: false,
                            ignore_case,
                            ignore_mark,
                        };
                        RegexAtom::CaptureGroup(group_pat)
                    } else {
                        let parsed_group = if ignore_case || sigspace {
                            let mut scoped = String::new();
                            if ignore_case {
                                scoped.push_str(":i ");
                            }
                            if sigspace {
                                scoped.push_str(":s ");
                            }
                            scoped.push_str(group_pattern.trim_end());
                            self.parse_regex(&scoped)
                        } else {
                            self.parse_regex(&group_pattern)
                        };
                        if let Some(p) = parsed_group {
                            RegexAtom::CaptureGroup(p)
                        } else {
                            continue;
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
                    let alternatives = Self::split_top_level_alternation(&group_pattern);
                    if alternatives.len() > 1 {
                        let mut alt_patterns = Vec::new();
                        for alt in alternatives {
                            let parsed_alt = if ignore_case || sigspace {
                                let mut scoped = String::new();
                                if ignore_case {
                                    scoped.push_str(":i ");
                                }
                                if sigspace {
                                    scoped.push_str(":s ");
                                }
                                scoped.push_str(alt.trim_end());
                                self.parse_regex(&scoped)
                            } else {
                                self.parse_regex(&alt)
                            };
                            if let Some(p) = parsed_alt {
                                alt_patterns.push(p);
                            }
                        }
                        RegexAtom::Alternation(alt_patterns)
                    } else {
                        let parsed_group = if ignore_case || sigspace {
                            let mut scoped = String::new();
                            if ignore_case {
                                scoped.push_str(":i ");
                            }
                            if sigspace {
                                scoped.push_str(":s ");
                            }
                            scoped.push_str(group_pattern.trim_end());
                            self.parse_regex(&scoped)
                        } else {
                            self.parse_regex(&group_pattern)
                        };
                        if let Some(p) = parsed_group {
                            RegexAtom::Group(p)
                        } else {
                            continue;
                        }
                    }
                }
                '{' => {
                    // Code block in regex: { ... }
                    let mut code = String::new();
                    let mut depth = 1usize;
                    for ch in chars.by_ref() {
                        if ch == '{' {
                            depth += 1;
                            code.push(ch);
                        } else if ch == '}' {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                            code.push(ch);
                        } else {
                            code.push(ch);
                        }
                    }
                    RegexAtom::CodeAssertion {
                        code,
                        negated: false,
                        is_assertion: false,
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
            // Handle ':' ratchet modifier (prevents backtracking)
            let token_ratchet = if chars.peek() == Some(&':') {
                chars.next();
                true
            } else {
                ratchet // inherit from pattern-level :ratchet flag
            };
            tokens.push(RegexToken {
                atom,
                quant,
                named_capture: pending_named_capture
                    .take()
                    .or(pending_builtin_named_capture.take()),
                ratchet: token_ratchet,
            });
        }
        Some(RegexPattern {
            tokens,
            anchor_start,
            anchor_end,
            ignore_case,
            ignore_mark,
        })
    }

    pub(super) fn interpolate_regex_scalars(&self, pattern: &str) -> String {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        while i < chars.len() {
            let ch = chars[i];
            // Skip code blocks { ... } — don't interpolate variables inside them
            if ch == '{' {
                let mut depth = 1usize;
                out.push(ch);
                i += 1;
                while i < chars.len() && depth > 0 {
                    let c = chars[i];
                    if c == '{' {
                        depth += 1;
                    } else if c == '}' {
                        depth -= 1;
                    }
                    out.push(c);
                    i += 1;
                }
                continue;
            }
            // Array interpolation in regex groups: (@name) / ( @name )
            // Expand to an alternation group from the current array value.
            if ch == '(' {
                let mut j = i + 1;
                while j < chars.len() && chars[j].is_whitespace() {
                    j += 1;
                }
                if j < chars.len() && chars[j] == '@' {
                    j += 1;
                    let name_start = j;
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    if j > name_start {
                        let mut k = j;
                        while k < chars.len() && chars[k].is_whitespace() {
                            k += 1;
                        }
                        if k < chars.len() && chars[k] == ')' {
                            let bare_name: String = chars[name_start..j].iter().collect();
                            let sigiled_name = format!("@{}", bare_name);
                            let value = self
                                .env
                                .get(&sigiled_name)
                                .cloned()
                                .or_else(|| self.env.get(&bare_name).cloned())
                                .unwrap_or(Value::Nil);
                            let entries: Vec<String> = match value {
                                Value::Array(items, ..)
                                | Value::Seq(items)
                                | Value::Slip(items) => items
                                    .iter()
                                    .map(|v| {
                                        Self::escape_regex_scalar_literal(&v.to_string_value())
                                    })
                                    .collect(),
                                Value::Nil => Vec::new(),
                                other => {
                                    vec![Self::escape_regex_scalar_literal(
                                        &other.to_string_value(),
                                    )]
                                }
                            };
                            if entries.is_empty() {
                                out.push_str("()");
                            } else {
                                out.push('(');
                                out.push_str(&entries.join("|"));
                                out.push(')');
                            }
                            i = k + 1;
                            continue;
                        }
                    }
                }
            }
            if ch == '\\' {
                out.push(ch);
                i += 1;
                if i < chars.len() {
                    out.push(chars[i]);
                    i += 1;
                }
                continue;
            }
            if ch == '$' {
                let mut j = i + 1;
                if j < chars.len() && chars[j] == '{' {
                    j += 1;
                    let name_start = j;
                    while j < chars.len() && chars[j] != '}' {
                        j += 1;
                    }
                    if j < chars.len() && j > name_start {
                        let name: String = chars[name_start..j].iter().collect();
                        let value = self.env.get(&name).cloned().unwrap_or(Value::Nil);
                        match value {
                            Value::Regex(pat) => out.push_str(&pat),
                            Value::RegexWithAdverbs { pattern, .. } => out.push_str(&pattern),
                            other => out.push_str(&Self::escape_regex_scalar_literal(
                                &other.to_string_value(),
                            )),
                        }
                        i = j + 1;
                        continue;
                    }
                } else if j < chars.len() && (chars[j].is_alphabetic() || chars[j] == '_') {
                    let name_start = j;
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    let name: String = chars[name_start..j].iter().collect();
                    let value = self.env.get(&name).cloned().unwrap_or(Value::Nil);
                    match value {
                        Value::Regex(pat) => out.push_str(&pat),
                        Value::RegexWithAdverbs { pattern, .. } => out.push_str(&pattern),
                        other => out
                            .push_str(&Self::escape_regex_scalar_literal(&other.to_string_value())),
                    }
                    i = j;
                    continue;
                }
            }
            out.push(ch);
            i += 1;
        }
        out
    }

    fn escape_regex_scalar_literal(input: &str) -> String {
        let mut out = String::new();
        for ch in input.chars() {
            if ch.is_whitespace()
                || matches!(
                    ch,
                    '\\' | '.'
                        | '^'
                        | '$'
                        | '*'
                        | '+'
                        | '?'
                        | '('
                        | ')'
                        | '['
                        | ']'
                        | '{'
                        | '}'
                        | '<'
                        | '>'
                        | '|'
                        | ':'
                        | '#'
                        | '\''
                )
            {
                out.push('\\');
            }
            out.push(ch);
        }
        out
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
