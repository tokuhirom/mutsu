use super::regex_parse::*;
use super::*;

impl Interpreter {
    /// Owned-`RegexPattern` parse used by the parser's own recursion (sub-pattern
    /// parsing while building the token tree) and by non-`Match` (`Validate`)
    /// callers. Sub-patterns live inside their parent's tree, so they are not
    /// top-level cached here; the parent as a whole is cached by `parse_regex`.
    pub(super) fn parse_regex_with_mode(
        &self,
        pattern: &str,
        mode: RegexParseMode,
    ) -> Option<RegexPattern> {
        self.parse_regex_uncached(pattern, mode)
    }

    pub(super) fn parse_regex_uncached(
        &self,
        pattern: &str,
        mode: RegexParseMode,
    ) -> Option<RegexPattern> {
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
                RegexAtom::WsRule => true,
                _ => false,
            }
        }

        // `Match` mode interpolates `$`/`@`/`%` variable values into the pattern
        // before structural parsing. `Validate` mode (parse-time dry run) has no
        // variable values available, so it skips interpolation and treats sigil
        // references as opaque atoms further down.
        let interpolated = if mode == RegexParseMode::Match {
            match self.interpolate_regex_scalars(pattern) {
                Ok(s) => s,
                Err(e) => {
                    PENDING_REGEX_ERROR.with(|err| {
                        *err.borrow_mut() = Some(e);
                    });
                    return None;
                }
            }
        } else {
            pattern.to_string()
        };
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
            if let Some(rest) = source.strip_prefix(":!ratchet") {
                ratchet = false;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":ratchet") {
                ratchet = true;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":!ignorecase") {
                ignore_case = false;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":!ignoremark") {
                ignore_mark = false;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":!sigspace") {
                sigspace = false;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":!i")
                && (rest.is_empty()
                    || rest.starts_with(' ')
                    || rest.starts_with(':')
                    || rest.starts_with('/'))
            {
                ignore_case = false;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":!s")
                && (rest.is_empty()
                    || rest.starts_with(' ')
                    || rest.starts_with(':')
                    || rest.starts_with('/'))
            {
                sigspace = false;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":!r")
                && (rest.is_empty()
                    || rest.starts_with(' ')
                    || rest.starts_with(':')
                    || rest.starts_with('/'))
            {
                ratchet = false;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":!m")
                && (rest.is_empty()
                    || rest.starts_with(' ')
                    || rest.starts_with(':')
                    || rest.starts_with('/'))
            {
                ignore_mark = false;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":i")
                && (rest.is_empty()
                    || rest.starts_with(|c: char| c.is_whitespace())
                    || rest.starts_with(':')
                    || rest.starts_with('/'))
            {
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
            if let Some(rest) = source.strip_prefix(":r")
                && (rest.is_empty()
                    || rest.starts_with(' ')
                    || rest.starts_with(':')
                    || rest.starts_with('/'))
            {
                // Make sure it's :r and not :ratchet (already handled) or other identifiers
                ratchet = true;
                source = rest.trim_start();
                continue;
            }
            if let Some(rest) = source.strip_prefix(":m")
                && (rest.is_empty()
                    || rest.starts_with(' ')
                    || rest.starts_with(':')
                    || rest.starts_with('/'))
            {
                // Make sure it's :m and not :mm or :my or other identifiers
                ignore_mark = true;
                source = rest.trim_start();
                continue;
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
        // A regex whose entire body (or a recursed branch/group body) is empty
        // or whitespace-only is a null regex; Raku rejects these at parse time
        // with X::Syntax::Regex::NullRegex (e.g. `/ /`, `s//b/`, an empty
        // `regex foo { }` body, or an empty `()`/`[]` group).
        if source.trim().is_empty() {
            PENDING_REGEX_ERROR.with(|e| *e.borrow_mut() = Some(make_null_regex_error()));
            return None;
        }
        // Handle top-level alternation (| or ||)
        let (top_alts, is_sequential) = Self::split_top_level_alternation(source);
        if top_alts.len() > 1 {
            // A trailing or interior empty branch (`/ a | /`, `/ | /`) is a null
            // regex. A single leading empty branch is allowed for alignment
            // (`/ | a /`, `/ || a /`).
            if let Some(err) = null_regex_if_empty_branch(&top_alts, true) {
                PENDING_REGEX_ERROR.with(|e| *e.borrow_mut() = Some(err));
                return None;
            }
            let mut alt_patterns = Vec::new();
            for alt in &top_alts {
                let alt_src = alt.trim();
                if alt_src.is_empty() {
                    continue;
                }
                // Re-apply inline adverbs for each alternative. Both `:i`
                // (ignore-case) and `:s` (sigspace) must propagate to the
                // re-parsed sub-pattern, otherwise an alternative like `(a) (b)`
                // loses its sigspace whitespace matchers and fails to match.
                let mut alt_pat = alt_src.to_string();
                if ignore_case && !alt_src.starts_with(":i") {
                    alt_pat = format!(":i {}", alt_pat);
                }
                if sigspace {
                    alt_pat = format!(":s {}", alt_pat);
                }
                if let Some(p) = self.parse_regex_with_mode(&alt_pat, mode) {
                    alt_patterns.push(p);
                }
            }
            if alt_patterns.len() > 1 {
                let atom = if is_sequential {
                    RegexAtom::SequentialAlternation(alt_patterns)
                } else {
                    try_collapse_alternation_to_charclass(&alt_patterns)
                        .unwrap_or(RegexAtom::Alternation(alt_patterns))
                };
                return Some(RegexPattern {
                    tokens: vec![RegexToken {
                        atom,
                        quant: RegexQuant::One,
                        named_capture: None,
                        hash_capture: None,
                        secondary_named_capture: None,
                        force_list_capture: false,
                        ratchet: false,
                        frugal: false,
                        separator: None,
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

        // Check for conjunction (`&` or `&&`) at the top level.
        // Conjunction has higher precedence than alternation, so this runs
        // after alternation splitting found no `|`.
        let conj_parts = Self::split_top_level_conjunction(source);
        if conj_parts.len() > 1 {
            // As with alternation, a trailing/interior empty conjunct (`/ a & /`)
            // is null; a single leading empty conjunct is allowed (`/ & a /`).
            if let Some(err) = null_regex_if_empty_branch(&conj_parts, true) {
                PENDING_REGEX_ERROR.with(|e| *e.borrow_mut() = Some(err));
                return None;
            }
            let mut conj_patterns = Vec::new();
            for part in &conj_parts {
                let part_src = part.trim();
                if part_src.is_empty() {
                    continue;
                }
                let part_pat = if ignore_case && !part_src.starts_with(":i") {
                    format!(":i {}", part_src)
                } else {
                    part_src.to_string()
                };
                if let Some(p) = self.parse_regex_with_mode(&part_pat, mode) {
                    conj_patterns.push(p);
                }
            }
            if conj_patterns.len() > 1 {
                return Some(RegexPattern {
                    tokens: vec![RegexToken {
                        atom: RegexAtom::Conjunction(conj_patterns),
                        quant: RegexQuant::One,
                        named_capture: None,
                        hash_capture: None,
                        secondary_named_capture: None,
                        force_list_capture: false,
                        ratchet: false,
                        frugal: false,
                        separator: None,
                    }],
                    anchor_start: false,
                    anchor_end: false,
                    ignore_case,
                    ignore_mark,
                });
            } else if conj_patterns.len() == 1 {
                return conj_patterns.into_iter().next();
            }
        }

        // LTM (longest-token-match) expansion is a Match-time rewrite used by the
        // matching engine. In Validate mode we syntax-check the raw pattern (as the
        // former standalone validator did); expanding first can produce
        // intermediate forms whose quantifiers trip the parse-time checks.
        let expanded = if mode == RegexParseMode::Match {
            Self::expand_ltm_pattern(source, sigspace)
        } else {
            source.to_string()
        };
        let mut chars = expanded.chars().peekable();
        let mut tokens = Vec::new();
        let mut anchor_start = false;
        let mut anchor_end = false;
        let mut pending_named_capture: Option<String> = None;
        // Set when `pending_named_capture` came from an angle-bracket alias of a
        // char class / Unicode property (`<foo=[bao]>`, `<bar=:Letter>`) rather
        // than a sigil-prefix alias (`$<foo>=...`). The angle form quantifies
        // per-iteration (Raku yields a List of Matches for `<foo=[bao]>+`), like
        // a builtin subrule; the sigil form captures the whole quantified span.
        let mut pending_named_capture_is_angle_alias = false;
        // Set when `pending_named_capture` came from an array-sigil alias
        // (`@<name>=...`). The `@` sigil forces the named capture into list
        // context, so even a single non-quantified match becomes a one-element
        // List rather than a bare Match.
        let mut pending_named_capture_is_array = false;
        let mut pending_builtin_named_capture: Option<String> = None;
        let mut pending_hash_capture: Option<String> = None;
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
                                hash_capture: None,
                                secondary_named_capture: None,
                                force_list_capture: false,
                                ratchet,
                                frugal: false,
                                separator: None,
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
                        // Use WsRule atom which implements <.ws> semantics:
                        // requires \s+ between word characters, \s* otherwise.
                        tokens.push(RegexToken {
                            atom: RegexAtom::WsRule,
                            quant: if next_is_anchor_end {
                                RegexQuant::ZeroOrMore
                            } else {
                                RegexQuant::One
                            },
                            named_capture: None,
                            hash_capture: None,
                            secondary_named_capture: None,
                            force_list_capture: false,
                            ratchet,
                            frugal: false,
                            separator: None,
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
            if c == '^' {
                if chars.peek() == Some(&'^') {
                    // ^^ — start of line anchor (zero-width assertion)
                    chars.next();
                    tokens.push(RegexToken {
                        atom: RegexAtom::StartOfLine,
                        quant: RegexQuant::One,
                        named_capture: None,
                        hash_capture: None,
                        secondary_named_capture: None,
                        force_list_capture: false,
                        ratchet,
                        frugal: false,
                        separator: None,
                    });
                    continue;
                } else if tokens.is_empty() {
                    anchor_start = true;
                    continue;
                }
            }
            if c == '$' && chars.peek() == Some(&'$') {
                // $$ — end of line anchor (zero-width assertion)
                chars.next();
                tokens.push(RegexToken {
                    atom: RegexAtom::EndOfLine,
                    quant: RegexQuant::One,
                    named_capture: None,
                    hash_capture: None,
                    secondary_named_capture: None,
                    force_list_capture: false,
                    ratchet,
                    frugal: false,
                    separator: None,
                });
                continue;
            }
            if c == '$' && chars.clone().all(|ch| ch.is_whitespace()) {
                anchor_end = true;
                break;
            }
            // $0, $1, ... — either a numbered scalar capture alias (`$0=(...)`)
            // or a backreference to a positional capture group (`$0`).
            if c == '$' && chars.peek().is_some_and(|ch| ch.is_ascii_digit()) {
                let mut digits = String::new();
                while chars.peek().is_some_and(|ch| ch.is_ascii_digit()) {
                    digits.push(chars.next().unwrap());
                }
                // A trailing `=` (after optional whitespace) makes this a numbered
                // capture alias: `$N=<atom>` stores the atom's capture at index N
                // and continues auto-numbering from N+1. We reuse the
                // `named_capture` channel with an all-digit "name", which
                // `apply_named_capture` recognizes and routes to the positional
                // slot instead of the named hash.
                let mut lookahead = chars.clone();
                while lookahead.peek().is_some_and(|ch| ch.is_whitespace()) {
                    lookahead.next();
                }
                if lookahead.peek() == Some(&'=') {
                    chars = lookahead;
                    chars.next(); // consume '='
                    while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                        chars.next();
                    }
                    pending_named_capture = Some(digits);
                    continue;
                }
                if let Ok(idx) = digits.parse::<usize>() {
                    tokens.push(RegexToken {
                        atom: RegexAtom::Backref(idx),
                        quant: RegexQuant::One,
                        named_capture: pending_named_capture.take(),
                        hash_capture: None,
                        secondary_named_capture: None,
                        force_list_capture: false,
                        ratchet,
                        frugal: false,
                        separator: None,
                    });
                    continue;
                }
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
                // $<name> without `=` is a backreference to a named capture
                tokens.push(RegexToken {
                    atom: RegexAtom::NamedBackref(capture_name),
                    quant: RegexQuant::One,
                    named_capture: None,
                    hash_capture: None,
                    secondary_named_capture: None,
                    force_list_capture: false,
                    ratchet,
                    frugal: false,
                    separator: None,
                });
                continue;
            }
            // `@<name>=(...)` — array capture alias. Behaves like the scalar
            // `$<name>=` alias (routes the following atom's capture to `name`),
            // but the `@` sigil is the list-context form: applied to a capturing
            // group (`@<foo>=(.(.))+`) it yields a List of the group's Matches,
            // one per iteration. This is recognized only when a trailing `=`
            // follows `<name>`; a bare `@var` is array interpolation (handled
            // below / during interpolation).
            if c == '@' && chars.peek() == Some(&'<') {
                let mut probe = chars.clone();
                probe.next(); // consume '<'
                let mut capture_name = String::new();
                let mut closed = false;
                for ch in probe.by_ref() {
                    if ch == '>' {
                        closed = true;
                        break;
                    }
                    capture_name.push(ch);
                }
                if closed && !capture_name.is_empty() {
                    while probe.peek().is_some_and(|ch| ch.is_whitespace()) {
                        probe.next();
                    }
                    if probe.peek() == Some(&'=') {
                        probe.next(); // consume '='
                        while probe.peek().is_some_and(|ch| ch.is_whitespace()) {
                            probe.next();
                        }
                        chars = probe;
                        pending_named_capture = Some(capture_name);
                        pending_named_capture_is_array = true;
                        continue;
                    }
                }
                // Not an alias — fall through to generic `@` handling below.
            }
            // Validate mode handling of `$` / `@` that was NOT recognized as an
            // anchor (`$$`, trailing `$`) or backreference (`$0`, `$<name>`)
            // above. In `Match` mode interpolation already substituted these, so
            // they only reach here at parse time.
            if mode == RegexParseMode::Validate && (c == '$' || c == '@') {
                let next = chars.peek().copied();
                let placeholder = |toks: &mut Vec<RegexToken>| {
                    toks.push(RegexToken {
                        atom: RegexAtom::ZeroWidth,
                        quant: RegexQuant::One,
                        named_capture: None,
                        hash_capture: None,
                        secondary_named_capture: None,
                        force_list_capture: false,
                        ratchet,
                        frugal: false,
                        separator: None,
                    });
                };
                if c == '$' {
                    // `$!attr` — interpolating an attribute into a regex is prohibited.
                    if next == Some('!') {
                        let mut symbol = String::from("$!");
                        let mut peeked = chars.clone();
                        peeked.next(); // skip '!'
                        while peeked
                            .peek()
                            .is_some_and(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-')
                        {
                            symbol.push(peeked.next().unwrap());
                        }
                        if symbol.chars().count() > 2 {
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(make_attribute_regex_error(&symbol));
                            });
                            return None;
                        }
                    }
                    // `$` followed by a variable-introducing char is interpolation
                    // (`$name`, `${...}`, `$(...)`, `$*dyn`, `$.attr`, ...). `$$`,
                    // `$<name>`, `$0`, and trailing `$` were handled above; what
                    // remains that is NOT interpolation is the end-of-string anchor.
                    let is_var = next.is_some_and(|ch| {
                        ch.is_alphabetic()
                            || ch == '_'
                            || ch == '{'
                            || ch == '('
                            || ch == '*'
                            || ch == '?'
                            || ch == '^'
                            || ch == '.'
                    });
                    if is_var {
                        skip_opaque_var_ref(&mut chars);
                        placeholder(&mut tokens);
                        continue;
                    }
                    // Bare `$` end-of-string anchor; quantifying it is NonQuantifiable.
                    if next == Some('+') {
                        PENDING_REGEX_ERROR
                            .with(|e| *e.borrow_mut() = Some(make_non_quantifiable_error()));
                        return None;
                    }
                    tokens.push(RegexToken {
                        atom: RegexAtom::EndOfLine,
                        quant: RegexQuant::One,
                        named_capture: None,
                        hash_capture: None,
                        secondary_named_capture: None,
                        force_list_capture: false,
                        ratchet,
                        frugal: false,
                        separator: None,
                    });
                    continue;
                }
                // `@...` is always an array interpolation (e.g. `@var`, `@$aref`,
                // `@var[0]`). Consume the reference opaquely and leave any trailing
                // construct (like `[0]` / `$aref`) for subsequent iterations.
                skip_opaque_var_ref(&mut chars);
                placeholder(&mut tokens);
                continue;
            }
            // Handle %<name>= and %ident= hash aliasing in regex
            if c == '%' {
                if chars.peek() == Some(&'<') {
                    chars.next();
                    let mut hash_name = String::new();
                    for ch in chars.by_ref() {
                        if ch == '>' {
                            break;
                        }
                        hash_name.push(ch);
                    }
                    if !hash_name.is_empty() {
                        while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                            chars.next();
                        }
                        if chars.peek() == Some(&'=') {
                            chars.next();
                            while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                                chars.next();
                            }
                            pending_hash_capture = Some(hash_name);
                            continue;
                        }
                    }
                    // `%<name>` without `=` is a bare hash variable — reserved.
                    if mode == RegexParseMode::Validate {
                        PENDING_REGEX_ERROR
                            .with(|e| *e.borrow_mut() = Some(make_hash_reserved_error()));
                        return None;
                    }
                    continue;
                } else if chars
                    .peek()
                    .is_some_and(|ch| ch.is_alphabetic() || *ch == '_')
                {
                    let mut hash_name = String::new();
                    while chars
                        .peek()
                        .is_some_and(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-')
                    {
                        hash_name.push(chars.next().unwrap());
                    }
                    if !hash_name.is_empty() {
                        while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                            chars.next();
                        }
                        if chars.peek() == Some(&'=') {
                            chars.next();
                            while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                                chars.next();
                            }
                            pending_hash_capture = Some(hash_name);
                            continue;
                        }
                    }
                    // Bare `%var` (no `=` aliasing) is a reserved hash interpolation.
                    if mode == RegexParseMode::Validate {
                        PENDING_REGEX_ERROR
                            .with(|e| *e.borrow_mut() = Some(make_hash_reserved_error()));
                        return None;
                    }
                    continue;
                }
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
                        hash_capture: None,
                        secondary_named_capture: None,
                        force_list_capture: false,
                        ratchet,
                        frugal: false,
                        separator: None,
                    });
                    continue;
                }
                // Handle inline scope modifiers: :ratchet, :!ratchet, :r, :!r,
                // :ignorecase, :!ignorecase, :i, :!i, :sigspace, :!sigspace, :s, :!s,
                // :ignoremark, :!ignoremark, :m, :!m
                if let Some(modifier_rest) = Self::try_parse_inline_modifier(
                    &remaining,
                    &mut ratchet,
                    &mut ignore_case,
                    &mut ignore_mark,
                    &mut sigspace,
                ) {
                    // Advance chars by the number of characters consumed
                    let consumed = remaining.len() - modifier_rest.len();
                    for _ in 0..consumed {
                        chars.next();
                    }
                    continue;
                }
                // Validate mode: the `:my`/inline-modifier forms above were not
                // matched. Reproduce the validator's remaining `:` checks —
                // solitary backtrack control and unrecognized modifiers.
                if mode == RegexParseMode::Validate {
                    if chars.peek() == Some(&'!') {
                        chars.next();
                        continue;
                    }
                    // Bare `:` with no preceding atom -> solitary backtrack control.
                    if tokens.is_empty() && !anchor_start {
                        let mut lookahead = chars.clone();
                        while lookahead.peek().is_some_and(|ch| ch.is_whitespace()) {
                            lookahead.next();
                        }
                        if lookahead
                            .peek()
                            .is_none_or(|ch| !ch.is_alphanumeric() && *ch != '_')
                        {
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(make_solitary_backtrack_control_error());
                            });
                            return None;
                        }
                    }
                    // `:digits` with no following modifier name -> unrecognized.
                    let mut digits = String::new();
                    while chars.peek().is_some_and(|ch| ch.is_ascii_digit()) {
                        digits.push(chars.next().unwrap());
                    }
                    if !digits.is_empty() {
                        let mut name = String::new();
                        while chars
                            .peek()
                            .is_some_and(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-')
                        {
                            name.push(chars.next().unwrap());
                        }
                        if name.is_empty() {
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(make_unrecognized_modifier_error(&digits));
                            });
                            return None;
                        }
                        continue;
                    }
                    // `:name` — unknown adverb (e.g. `:iabc`), or `:name(...)`.
                    let mut name = String::new();
                    {
                        let mut lookahead = chars.clone();
                        while lookahead
                            .peek()
                            .is_some_and(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-')
                        {
                            name.push(lookahead.next().unwrap());
                        }
                    }
                    if !name.is_empty() {
                        for _ in 0..name.chars().count() {
                            chars.next();
                        }
                        if chars.peek() == Some(&'(') {
                            // `:name(...)` optional argument — consume balanced parens.
                            chars.next();
                            let mut depth = 1u32;
                            for ch in chars.by_ref() {
                                if ch == '(' {
                                    depth += 1;
                                } else if ch == ')' {
                                    depth -= 1;
                                    if depth == 0 {
                                        break;
                                    }
                                }
                            }
                            continue;
                        }
                        if !is_known_regex_adverb(&name) {
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(make_unrecognized_modifier_error(&name));
                            });
                            return None;
                        }
                        continue;
                    }
                }
            }
            // Validate mode: a quantifier metacharacter (`*`, `+`, `?`) reaching
            // atom position usually has nothing valid to quantify — any quantifier
            // that legitimately follows a normal atom is consumed by the quantifier
            // peek below. Reproduces the former validator's solitary/non-quantifiable
            // checks (e.g. `/ * /`, `/ a+ + /`, `/ ^+ /`, `/ ~? /`).
            if mode == RegexParseMode::Validate && matches!(c, '*' | '+' | '?') {
                // Backreferences and interpolation placeholders are pushed WITHOUT
                // a quantifier peek (they `continue` immediately), so a quantifier
                // here is a valid first quantifier on that atom (e.g. `$0*`,
                // `@var+`). Consume it (and any `**`-range / frugal marker) instead
                // of treating it as solitary.
                if matches!(
                    tokens.last().map(|t| &t.atom),
                    Some(RegexAtom::Backref(_))
                        | Some(RegexAtom::NamedBackref(_))
                        | Some(RegexAtom::ZeroWidth)
                ) {
                    if c == '*' && chars.peek() == Some(&'*') {
                        chars.next();
                        while chars.peek().is_some_and(|ch| {
                            ch.is_ascii_digit() || matches!(ch, '.' | '*' | '^' | '_' | ' ')
                        }) {
                            chars.next();
                        }
                    }
                    if chars.peek() == Some(&'?') {
                        chars.next();
                    }
                    continue;
                }
                let err = quantifier_context_error(&tokens, anchor_start);
                PENDING_REGEX_ERROR.with(|e| *e.borrow_mut() = Some(err));
                return None;
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
                        'h' => RegexAtom::CharClass(CharClass {
                            negated: false,
                            items: vec![ClassItem::HorizSpace],
                        }),
                        'H' => RegexAtom::CharClass(CharClass {
                            negated: true,
                            items: vec![ClassItem::HorizSpace],
                        }),
                        'v' => RegexAtom::CharClass(CharClass {
                            negated: false,
                            items: vec![ClassItem::VertSpace],
                        }),
                        'V' => RegexAtom::CharClass(CharClass {
                            negated: true,
                            items: vec![ClassItem::VertSpace],
                        }),
                        'n' => RegexAtom::Newline,
                        'N' => RegexAtom::NotNewline,
                        't' => RegexAtom::Literal('\t'),
                        'T' => RegexAtom::CharClass(CharClass {
                            negated: true,
                            items: vec![ClassItem::Char('\t')],
                        }),
                        'r' => RegexAtom::Literal('\r'),
                        'R' => RegexAtom::Newline, // \R matches any newline sequence
                        'e' => RegexAtom::Literal('\u{001B}'), // escape (ESC)
                        'f' => RegexAtom::Literal('\u{000C}'), // form feed
                        'F' => RegexAtom::CharClass(CharClass {
                            negated: true,
                            items: vec![ClassItem::Char('\u{000C}')],
                        }),
                        'x' => {
                            // \x[HEX] or \xHH hex escape in regex
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
                            } else if chars.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                                // \x followed by hex digits without brackets
                                let mut hex = String::new();
                                while chars.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                                    hex.push(chars.next().unwrap());
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
                            // \o[OCT] or \o### octal escape in regex
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
                            } else if chars.peek().is_some_and(|c| ('0'..='7').contains(c)) {
                                // \o followed by octal digits without brackets
                                let mut oct = String::new();
                                while chars.peek().is_some_and(|c| ('0'..='7').contains(c)) {
                                    oct.push(chars.next().unwrap());
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
                        'O' => {
                            // \O[OCT] or \O### matches any char NOT the given octal char
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
                                    RegexAtom::CharClass(CharClass {
                                        negated: true,
                                        items: vec![ClassItem::Char(c)],
                                    })
                                } else {
                                    continue;
                                }
                            } else if chars.peek().is_some_and(|c| ('0'..='7').contains(c)) {
                                let mut oct = String::new();
                                while chars.peek().is_some_and(|c| ('0'..='7').contains(c)) {
                                    oct.push(chars.next().unwrap());
                                }
                                if let Some(c) =
                                    u32::from_str_radix(&oct, 8).ok().and_then(char::from_u32)
                                {
                                    RegexAtom::CharClass(CharClass {
                                        negated: true,
                                        items: vec![ClassItem::Char(c)],
                                    })
                                } else {
                                    continue;
                                }
                            } else {
                                RegexAtom::Literal('O')
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
                                        hash_capture: None,
                                        secondary_named_capture: None,
                                        force_list_capture: false,
                                        ratchet: false,
                                        frugal: false,
                                        separator: None,
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
                            // \X[HEX] or \XHH matches any char that is NOT the given hex char
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
                            } else if chars.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                                let mut hex = String::new();
                                while chars.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                                    hex.push(chars.next().unwrap());
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
                        'b' => {
                            // Bare \b is obsolete Perl 5 syntax — reject with X::Obsolete
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(RuntimeError::obsolete(
                                    "\\b as a word boundary",
                                    "<?wb> (word boundary) or <!wb> (not a word boundary)",
                                ));
                            });
                            return None;
                        }
                        'B' => {
                            // Bare \B is obsolete Perl 5 syntax — reject with X::Obsolete
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(RuntimeError::obsolete(
                                    "\\B as a word boundary",
                                    "<?wb> (word boundary) or <!wb> (not a word boundary)",
                                ));
                            });
                            return None;
                        }
                        // Obsolete Perl 5 anchors — reject at parse time (Validate).
                        'A' if mode == RegexParseMode::Validate => {
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(RuntimeError::obsolete(
                                    "\\A as beginning-of-string matcher",
                                    "^",
                                ));
                            });
                            return None;
                        }
                        'Z' if mode == RegexParseMode::Validate => {
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(RuntimeError::obsolete(
                                    "\\Z as end-of-string matcher",
                                    "\\n?$",
                                ));
                            });
                            return None;
                        }
                        'z' if mode == RegexParseMode::Validate => {
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(RuntimeError::obsolete(
                                    "\\z as end-of-string matcher",
                                    "$",
                                ));
                            });
                            return None;
                        }
                        // `\ ` (backslash + whitespace) is an "unspace", a main-slang
                        // construct not allowed in a regex -> X::Syntax::Regex::Unspace.
                        ws if ws.is_whitespace() => {
                            if mode == RegexParseMode::Validate {
                                PENDING_REGEX_ERROR
                                    .with(|e| *e.borrow_mut() = Some(make_unspace_error(ws)));
                                return None;
                            }
                            RegexAtom::Literal(ws)
                        }
                        other => {
                            // Validate mode: an unknown *alphabetic* backslash escape
                            // is invalid metasyntax (e.g. `\a`, `\q`). Non-alphabetic
                            // escapes are always valid (escaping a metacharacter).
                            if mode == RegexParseMode::Validate && other.is_ascii_alphabetic() {
                                PENDING_REGEX_ERROR.with(|e| {
                                    *e.borrow_mut() =
                                        Some(make_backslash_unrecognized_error(other));
                                });
                                return None;
                            }
                            RegexAtom::Literal(other)
                        }
                    }
                }
                '\'' | '\u{2018}' | '\u{201A}' | '\u{FF62}' => {
                    // Quoted literal string in Raku regex: 'foo-bar' matches literally
                    // In single-quoted regex strings, \\ matches a literal backslash
                    // and \' matches a literal single quote.
                    let mut literal = String::new();
                    loop {
                        match chars.next() {
                            Some('\\') => match chars.peek() {
                                Some(&next_ch)
                                    if next_ch == '\\' || regex_single_quote_closes(c, next_ch) =>
                                {
                                    literal.push(next_ch);
                                    chars.next();
                                }
                                _ => literal.push('\\'),
                            },
                            Some(ch) if regex_single_quote_closes(c, ch) => break,
                            Some(ch) => literal.push(ch),
                            None => break,
                        }
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
                                Some('f') => literal.push('\u{000C}'),
                                Some('b') => literal.push('\u{0008}'), // backspace
                                Some('0') => literal.push('\0'),
                                Some('c') | Some('C') => {
                                    // \c[NAME] or \c[NAME1, NAME2] inside double-quoted regex string
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
                                        let parts: Vec<&str> =
                                            name.split(',').map(|s| s.trim()).collect();
                                        for part in &parts {
                                            if let Some(resolved_char) =
                                                crate::token_kind::lookup_unicode_char_by_name(part)
                                            {
                                                literal.push(resolved_char);
                                            }
                                        }
                                    } else {
                                        literal.push('c');
                                    }
                                }
                                Some('x') => {
                                    // \x[HEX] or bracketless \xHH inside a double-quoted
                                    // regex string (e.g. `token SP { "\x20" }`).
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
                                        if let Ok(cp) = u32::from_str_radix(hex.trim(), 16)
                                            && let Some(ch) = char::from_u32(cp)
                                        {
                                            literal.push(ch);
                                        }
                                    } else if chars.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                                        let mut hex = String::new();
                                        while chars.peek().is_some_and(|c| c.is_ascii_hexdigit()) {
                                            hex.push(chars.next().unwrap());
                                        }
                                        if let Ok(cp) = u32::from_str_radix(&hex, 16)
                                            && let Some(ch) = char::from_u32(cp)
                                        {
                                            literal.push(ch);
                                        }
                                    } else {
                                        literal.push('x');
                                    }
                                }
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
                '\u{00AB}' => {
                    // « — left word boundary
                    RegexAtom::LeftWordBoundary
                }
                '\u{00BB}' => {
                    // » — right word boundary
                    RegexAtom::RightWordBoundary
                }
                '<' if chars.peek() == Some(&'<') => {
                    // << — left word boundary
                    chars.next();
                    RegexAtom::LeftWordBoundary
                }
                '>' if chars.peek() == Some(&'>') => {
                    // >> — right word boundary
                    chars.next();
                    RegexAtom::RightWordBoundary
                }
                '<' => {
                    if chars.peek() == Some(&'(') {
                        chars.next();
                        RegexAtom::CaptureStartMarker
                    } else {
                        // Check for lookaround assertions: <?before ...>, <!before ...>,
                        // <?after ...>, <!after ...>
                        let peek_str: String = chars.clone().collect();
                        if peek_str.starts_with("?[")
                            || peek_str.starts_with("![")
                            || peek_str.starts_with("?-[")
                            || peek_str.starts_with("!-[")
                        {
                            // <?[a]> or <![a]> — zero-width character class assertion
                            let negated = peek_str.starts_with('!');
                            // Skip '?' or '!'
                            chars.next();
                            // Read content between current position and closing '>'
                            let mut cc_content = String::new();
                            let mut angle_depth = 1usize;
                            for ch in chars.by_ref() {
                                if ch == '<' {
                                    angle_depth += 1;
                                    cc_content.push(ch);
                                } else if ch == '>' {
                                    angle_depth -= 1;
                                    if angle_depth == 0 {
                                        break;
                                    }
                                    cc_content.push(ch);
                                } else {
                                    cc_content.push(ch);
                                }
                            }
                            // Parse the character class content (e.g., [a], -[a], [\n])
                            let cc_trimmed = cc_content.trim();
                            let (cc_negated, cc_inner) =
                                if let Some(rest) = cc_trimmed.strip_prefix("-[") {
                                    (true, rest.strip_suffix(']').unwrap_or(rest))
                                } else if let Some(rest) = cc_trimmed.strip_prefix('[') {
                                    (false, rest.strip_suffix(']').unwrap_or(rest))
                                } else {
                                    (false, cc_trimmed)
                                };
                            let class_negated = if negated { !cc_negated } else { cc_negated };
                            if let Some(class) = self.parse_raku_char_class(cc_inner, class_negated)
                            {
                                // Build a lookahead with the char class as the inner pattern
                                let inner_pattern = RegexPattern {
                                    tokens: vec![RegexToken {
                                        atom: RegexAtom::CharClass(class),
                                        quant: RegexQuant::One,
                                        named_capture: None,
                                        hash_capture: None,
                                        secondary_named_capture: None,
                                        force_list_capture: false,
                                        ratchet: false,
                                        frugal: false,
                                        separator: None,
                                    }],
                                    anchor_start: false,
                                    anchor_end: false,
                                    ignore_case,
                                    ignore_mark,
                                };
                                RegexAtom::Lookaround {
                                    pattern: inner_pattern,
                                    negated,
                                    is_behind: false,
                                }
                            } else {
                                continue;
                            }
                        } else if peek_str.starts_with("before ")
                            || peek_str.starts_with(".before ")
                            || peek_str.starts_with("?before ")
                            || peek_str.starts_with("!before ")
                            || peek_str.starts_with("after ")
                            || peek_str.starts_with(".after ")
                            || peek_str.starts_with("?after ")
                            || peek_str.starts_with("!after ")
                        {
                            let (negated, is_behind, keyword) = if peek_str.starts_with("before ") {
                                (false, false, "before ")
                            } else if peek_str.starts_with(".before ") {
                                chars.next();
                                (false, false, "before ")
                            } else if peek_str.starts_with("after ") {
                                (false, true, "after ")
                            } else if peek_str.starts_with(".after ") {
                                chars.next();
                                (false, true, "after ")
                            } else {
                                let negated = peek_str.starts_with('!');
                                // Skip '?' or '!'
                                chars.next();
                                let is_behind = peek_str[1..].starts_with("after ");
                                let keyword = if is_behind { "after " } else { "before " };
                                (negated, is_behind, keyword)
                            };
                            // Skip keyword
                            for _ in 0..keyword.len() {
                                chars.next();
                            }
                            // Read the inner pattern up to the closing '>'
                            let mut inner = String::new();
                            let mut angle_depth = 1usize;
                            for ch in chars.by_ref() {
                                if ch == '<' {
                                    angle_depth += 1;
                                    inner.push(ch);
                                } else if ch == '>' {
                                    angle_depth -= 1;
                                    if angle_depth == 0 {
                                        break;
                                    }
                                    inner.push(ch);
                                } else {
                                    inner.push(ch);
                                }
                            }
                            // Parse the inner pattern as a regex
                            let Some(inner_pattern) = self.parse_regex_with_mode(&inner, mode)
                            else {
                                continue;
                            };
                            RegexAtom::Lookaround {
                                pattern: inner_pattern,
                                negated,
                                is_behind,
                            }
                        } else if peek_str.starts_with("?{")
                            || peek_str.starts_with("!{")
                            || peek_str.starts_with('{')
                        {
                            // Check for code assertion: <?{...}> or <!{...}>
                            // These need special handling because code may contain < and >
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
                            // Read content between < and >, handling nested <...>.
                            // Also balance parens/brackets/braces and skip quoted
                            // strings so `<.foo(a => 1)>` and `<.foo(|[3,4,5])>`
                            // are not terminated by the inner `>` or by close
                            // brackets that match opens inside the args list.
                            let mut name = String::new();
                            let mut angle_depth = 1usize;
                            let mut paren_depth: usize = 0;
                            let mut bracket_depth: usize = 0;
                            let mut brace_depth: usize = 0;
                            let mut quote: Option<char> = None;
                            let mut escaped = false;
                            while let Some(ch) = chars.next() {
                                if let Some(q) = quote {
                                    name.push(ch);
                                    if escaped {
                                        escaped = false;
                                    } else if ch == '\\' {
                                        escaped = true;
                                    } else if ch == q {
                                        quote = None;
                                    }
                                    continue;
                                }
                                // Handle backslash escapes: \< and \> should not
                                // affect angle_depth, \[ and \] should not affect
                                // bracket_depth, etc.
                                if escaped {
                                    escaped = false;
                                    name.push(ch);
                                    continue;
                                }
                                if ch == '\\' {
                                    escaped = true;
                                    name.push(ch);
                                    continue;
                                }
                                // An apostrophe that sits between two identifier
                                // characters is part of a Raku long identifier
                                // (e.g. `with'hyphen`), not the start of a quoted
                                // literal. Only treat `'` as a quote opener when it
                                // is not flanked by word characters.
                                let apostrophe_in_ident = ch == '\''
                                    && name
                                        .chars()
                                        .last()
                                        .is_some_and(|p| p.is_alphanumeric() || p == '_')
                                    && chars
                                        .peek()
                                        .is_some_and(|n| n.is_alphanumeric() || *n == '_');
                                match ch {
                                    '\'' if apostrophe_in_ident => {
                                        name.push(ch);
                                    }
                                    '\'' | '"' if bracket_depth == 0 => {
                                        quote = Some(ch);
                                        name.push(ch);
                                    }
                                    '(' => {
                                        paren_depth += 1;
                                        name.push(ch);
                                    }
                                    ')' => {
                                        paren_depth = paren_depth.saturating_sub(1);
                                        name.push(ch);
                                    }
                                    '[' => {
                                        bracket_depth += 1;
                                        name.push(ch);
                                    }
                                    ']' => {
                                        bracket_depth = bracket_depth.saturating_sub(1);
                                        name.push(ch);
                                    }
                                    '{' => {
                                        brace_depth += 1;
                                        name.push(ch);
                                    }
                                    '}' => {
                                        brace_depth = brace_depth.saturating_sub(1);
                                        name.push(ch);
                                    }
                                    '<' if paren_depth == 0
                                        && bracket_depth == 0
                                        && brace_depth == 0 =>
                                    {
                                        angle_depth += 1;
                                        name.push(ch);
                                    }
                                    '>' if paren_depth == 0
                                        && bracket_depth == 0
                                        && brace_depth == 0 =>
                                    {
                                        angle_depth -= 1;
                                        if angle_depth == 0 {
                                            break;
                                        }
                                        name.push(ch);
                                    }
                                    _ => name.push(ch),
                                }
                            }
                            // Check for word alternation: < word1 word2 ... >
                            // In Raku, when the first character after `<` is
                            // whitespace (space or tab), the contents are treated
                            // as a list of quoted alternatives rather than a method
                            // call. (`<a aa>` with no leading whitespace is a call.)
                            if name.starts_with(|c: char| c.is_whitespace()) {
                                let words: Vec<&str> = name.split_whitespace().collect();
                                if !words.is_empty() {
                                    let alternatives: Vec<RegexPattern> = words
                                        .iter()
                                        .map(|w| {
                                            // Unescape backslash sequences: \< → <, \> → >, etc.
                                            let mut word_chars: Vec<char> = Vec::new();
                                            let mut wchars = w.chars().peekable();
                                            while let Some(wch) = wchars.next() {
                                                if wch == '\\' {
                                                    if let Some(&next) = wchars.peek() {
                                                        word_chars.push(next);
                                                        wchars.next();
                                                    } else {
                                                        word_chars.push(wch);
                                                    }
                                                } else {
                                                    word_chars.push(wch);
                                                }
                                            }
                                            let toks: Vec<RegexToken> = word_chars
                                                .iter()
                                                .map(|&ch| RegexToken {
                                                    atom: RegexAtom::Literal(ch),
                                                    quant: RegexQuant::One,
                                                    named_capture: None,
                                                    hash_capture: None,
                                                    secondary_named_capture: None,
                                                    force_list_capture: false,
                                                    ratchet: false,
                                                    frugal: false,
                                                    separator: None,
                                                })
                                                .collect();
                                            RegexPattern {
                                                tokens: toks,
                                                anchor_start: false,
                                                anchor_end: false,
                                                ignore_case,
                                                ignore_mark,
                                            }
                                        })
                                        .collect();
                                    RegexAtom::Alternation(alternatives)
                                } else {
                                    RegexAtom::ZeroWidth
                                }
                            } else {
                                // Handle aliasing of a char-class / Unicode-property
                                // assertion to a named capture, e.g. `<foo=[bao]>`,
                                // `<bar=-[bao]>`, `<foo=:Letter>`, `<bar=:!Letter>`,
                                // `<baz=-:Letter>`. The general `<name=subrule>` aliasing
                                // (for named rules) is resolved at match time via
                                // parse_named_regex_lookup_spec, but char classes and
                                // Unicode properties are parsed into dedicated atoms here,
                                // so we strip the `ident=` prefix and record the alias as
                                // the pending named capture before dispatching on the RHS.
                                {
                                    let t = name.trim();
                                    if let Some(eq_pos) = t.find('=') {
                                        let lhs = t[..eq_pos].trim();
                                        let rhs = t[eq_pos + 1..].trim();
                                        let lhs_is_ident = !lhs.is_empty()
                                            && lhs
                                                .chars()
                                                .next()
                                                .is_some_and(|c| c.is_alphabetic() || c == '_')
                                            && lhs.chars().all(|c| {
                                                c.is_alphanumeric()
                                                    || c == '_'
                                                    || c == '-'
                                                    || c == '\''
                                            });
                                        let rhs_is_class_or_prop = rhs.starts_with('[')
                                            || rhs.starts_with("-[")
                                            || rhs.starts_with("+[")
                                            || rhs.starts_with(':')
                                            || rhs.starts_with("-:")
                                            || rhs.starts_with(":!")
                                            || rhs.starts_with("!:");
                                        if lhs_is_ident && rhs_is_class_or_prop {
                                            pending_named_capture = Some(lhs.to_string());
                                            pending_named_capture_is_angle_alias = true;
                                            name = rhs.to_string();
                                        }
                                    }
                                }
                                // Check for Raku character class: <[...]>, <-[...]>, <+[...]>
                                // Also handles composite: <[a..z]-[aeiou]>, <+[a..z]-[aeiou]-[y]>
                                let trimmed = name.trim();
                                // Validate mode: reject a compound character class
                                // assertion that is missing a `+`/`-` operator
                                // between its parts (e.g. `<[abc] [def]>`,
                                // `<:Kata :Hira>`).
                                if mode == RegexParseMode::Validate
                                    && let Err(err) = check_missing_class_operator(trimmed)
                                {
                                    PENDING_REGEX_ERROR.with(|e| *e.borrow_mut() = Some(err));
                                    return None;
                                }
                                // Validate mode: a long name (`::`) used as a regex
                                // alias (`<Name::Path=alias>`, `<::IO::File=bar>`) is
                                // illegal. Exclude embedded code (`<{...}>`,
                                // `<?{...}>`, `<!{...}>`) which may contain `::`/`=`.
                                if mode == RegexParseMode::Validate
                                    && !trimmed.starts_with('{')
                                    && !trimmed.starts_with("?{")
                                    && !trimmed.starts_with("!{")
                                    && trimmed.contains("::")
                                    && trimmed.contains('=')
                                {
                                    PENDING_REGEX_ERROR.with(|e| {
                                        *e.borrow_mut() = Some(Self::make_longname_alias_error());
                                    });
                                    return None;
                                }
                                if (trimmed.starts_with('[')
                                    || trimmed.starts_with("-[")
                                    || trimmed.starts_with("+["))
                                    && trimmed.ends_with(']')
                                {
                                    if let Some(atom) = self.parse_bracket_char_class(trimmed) {
                                        atom
                                    } else {
                                        continue;
                                    }
                                } else if trimmed.starts_with('[')
                                    && Self::bracket_class_has_combination_tail(trimmed)
                                {
                                    // A bracket class combined with named classes:
                                    // `[a..z] +digit`, `[\-._~] +alpha +digit`. The
                                    // leading bracket is an implicit positive item.
                                    if let Some(atom) = self.parse_combined_class(trimmed, mode) {
                                        atom
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
                                } else if let Some(negated_name) = trimmed.strip_prefix('!') {
                                    if negated_name.is_empty() {
                                        // <!> — always-fail (handled as Named("!") downstream)
                                        RegexAtom::Named(name)
                                    } else if negated_name == "same" || negated_name == ".same" {
                                        // <!same> — zero-width assertion: next two chars are different
                                        RegexAtom::SameAssertion { negated: true }
                                    } else {
                                        // <!alpha>, <!digit>, etc. — zero-width negative assertion for named class
                                        let clean_name =
                                            negated_name.strip_prefix('.').unwrap_or(negated_name);
                                        let is_known = matches!(
                                            clean_name,
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
                                                | "ident"
                                        );
                                        if is_known {
                                            let inner_atom = if clean_name == "ident" {
                                                RegexAtom::Group(RegexPattern {
                                                    tokens: vec![
                                                        RegexToken {
                                                            atom: RegexAtom::CharClass(CharClass {
                                                                items: vec![
                                                                    ClassItem::NamedBuiltin(
                                                                        "alpha".to_string(),
                                                                    ),
                                                                ],
                                                                negated: false,
                                                            }),
                                                            quant: RegexQuant::One,
                                                            named_capture: None,
                                                            hash_capture: None,
                                                            secondary_named_capture: None,
                                                            force_list_capture: false,
                                                            ratchet: false,
                                                            frugal: false,
                                                            separator: None,
                                                        },
                                                        RegexToken {
                                                            atom: RegexAtom::CharClass(CharClass {
                                                                items: vec![
                                                                    ClassItem::NamedBuiltin(
                                                                        "alnum".to_string(),
                                                                    ),
                                                                ],
                                                                negated: false,
                                                            }),
                                                            quant: RegexQuant::ZeroOrMore,
                                                            named_capture: None,
                                                            hash_capture: None,
                                                            secondary_named_capture: None,
                                                            force_list_capture: false,
                                                            ratchet: false,
                                                            frugal: false,
                                                            separator: None,
                                                        },
                                                    ],
                                                    anchor_start: false,
                                                    anchor_end: false,
                                                    ignore_case,
                                                    ignore_mark,
                                                })
                                            } else {
                                                RegexAtom::CharClass(CharClass {
                                                    items: vec![ClassItem::NamedBuiltin(
                                                        clean_name.to_string(),
                                                    )],
                                                    negated: false,
                                                })
                                            };
                                            let inner_pattern = RegexPattern {
                                                tokens: vec![RegexToken {
                                                    atom: inner_atom,
                                                    quant: RegexQuant::One,
                                                    named_capture: None,
                                                    hash_capture: None,
                                                    secondary_named_capture: None,
                                                    force_list_capture: false,
                                                    ratchet: false,
                                                    frugal: false,
                                                    separator: None,
                                                }],
                                                anchor_start: false,
                                                anchor_end: false,
                                                ignore_case,
                                                ignore_mark,
                                            };
                                            RegexAtom::Lookaround {
                                                pattern: inner_pattern,
                                                negated: true,
                                                is_behind: false,
                                            }
                                        } else {
                                            // Not a known builtin — pass through as Named
                                            RegexAtom::Named(name)
                                        }
                                    } // close else (non-empty negated_name)
                                } else if trimmed.starts_with("::") {
                                    // <::($expr)> — symbolic indirect subrule. The
                                    // double colon distinguishes it from a `<:PropName>`
                                    // Unicode-property assertion; keep it as a Named atom
                                    // so the dynamic name is resolved at match time.
                                    RegexAtom::Named(name)
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
                                    if let Some(atom) = self.parse_combined_class(trimmed, mode) {
                                        atom
                                    } else {
                                        continue;
                                    }
                                } else if trimmed.starts_with('$')
                                    && mode == RegexParseMode::Validate
                                {
                                    // <$!attr> — attribute interpolation is prohibited.
                                    if let Some(rest) = trimmed.strip_prefix("$!") {
                                        let symbol = format!("$!{}", rest.trim_end_matches('>'));
                                        PENDING_REGEX_ERROR.with(|e| {
                                            *e.borrow_mut() =
                                                Some(make_attribute_regex_error(&symbol));
                                        });
                                        return None;
                                    }
                                    // <$var> interpolation — opaque at parse time (the variable's
                                    // value is unavailable). Accept it as a syntactically-valid
                                    // assertion; the runtime `Match` path below resolves it.
                                    RegexAtom::Named(name.clone())
                                } else if let Some(var_name) = trimmed.strip_prefix('$') {
                                    // <$var> — look up scalar variable and compile as regex
                                    let value = match self.env.get(var_name).cloned() {
                                        Some(v) => v,
                                        None => {
                                            // Variable not declared — X::Undeclared
                                            let symbol = format!("${var_name}");
                                            let msg =
                                                format!("Variable '{symbol}' is not declared");
                                            let mut attrs = std::collections::HashMap::new();
                                            attrs.insert("symbol".to_string(), Value::str(symbol));
                                            attrs.insert(
                                                "message".to_string(),
                                                Value::str(msg.clone()),
                                            );
                                            let ex = Value::make_instance(
                                                Symbol::intern("X::Undeclared"),
                                                attrs,
                                            );
                                            let mut err = RuntimeError::new(&msg);
                                            err.exception = Some(Box::new(ex));
                                            PENDING_REGEX_ERROR.with(|e| {
                                                *e.borrow_mut() = Some(err);
                                            });
                                            return None;
                                        }
                                    };
                                    let pat_str = match &value {
                                        Value::Regex(pat) => pat.to_string(),
                                        Value::RegexWithAdverbs(a) => a.pattern.to_string(),
                                        other => other.to_string_value(),
                                    };
                                    // Check for longname alias first
                                    if Self::contains_longname_alias(&pat_str) {
                                        PENDING_REGEX_ERROR.with(|e| {
                                            *e.borrow_mut() =
                                                Some(Self::make_longname_alias_error());
                                        });
                                        return None;
                                    }
                                    // Security check: reject dangerous patterns
                                    if Self::contains_dangerous_regex_code(&pat_str) {
                                        PENDING_REGEX_ERROR.with(|e| {
                                            *e.borrow_mut() =
                                                Some(Self::make_security_policy_error());
                                        });
                                        return None;
                                    }
                                    // Check for undeclared variables in the resolved string
                                    if let Some(err) =
                                        self.check_undeclared_vars_in_pattern(&pat_str)
                                    {
                                        PENDING_REGEX_ERROR.with(|e| {
                                            *e.borrow_mut() = Some(err);
                                        });
                                        return None;
                                    }
                                    // Parse as regex, propagating outer modifiers (:i, :m)
                                    let scoped_pat = if ignore_case || ignore_mark {
                                        let mut s = String::new();
                                        if ignore_case {
                                            s.push_str(":i ");
                                        }
                                        if ignore_mark {
                                            s.push_str(":m ");
                                        }
                                        s.push_str(&pat_str);
                                        s
                                    } else {
                                        pat_str
                                    };
                                    if let Some(parsed) =
                                        self.parse_regex_with_mode(&scoped_pat, mode)
                                    {
                                        RegexAtom::Group(parsed)
                                    } else {
                                        continue;
                                    }
                                } else if trimmed.starts_with('@')
                                    && mode == RegexParseMode::Validate
                                {
                                    // <@var> interpolation — opaque at parse time.
                                    RegexAtom::Named(name.clone())
                                } else if trimmed.starts_with('@') {
                                    // <@var> — look up array variable and compile
                                    // each element as a regex pattern (alternation)
                                    let env_key = trimmed.to_string(); // includes @
                                    let value =
                                        self.env.get(&env_key).cloned().unwrap_or(Value::Nil);
                                    let elements = match &value {
                                        Value::Array(arr, _) => arr.as_ref().clone(),
                                        Value::Seq(items) | Value::Slip(items) => {
                                            crate::value::ArrayData::new((**items).clone())
                                        }
                                        _ => crate::value::ArrayData::new(vec![value]),
                                    };
                                    let mut alt_patterns = Vec::new();
                                    for elt in &elements {
                                        let pat_str = match elt {
                                            Value::Regex(pat) => pat.to_string(),
                                            Value::RegexWithAdverbs(a) => a.pattern.to_string(),
                                            other => other.to_string_value(),
                                        };
                                        if let Some(parsed) =
                                            self.parse_regex_with_mode(&pat_str, mode)
                                        {
                                            alt_patterns.push(parsed);
                                        }
                                    }
                                    if alt_patterns.is_empty() {
                                        continue;
                                    }
                                    try_collapse_alternation_to_charclass(&alt_patterns)
                                        .unwrap_or(RegexAtom::Alternation(alt_patterns))
                                } else if trimmed == "?same" || trimmed == "?.same" {
                                    // <?same> — zero-width assertion: next two chars are the same
                                    RegexAtom::SameAssertion { negated: false }
                                } else if trimmed.starts_with("at(") && trimmed.ends_with(')') {
                                    // <at(N)> — zero-width assertion: match at position N
                                    let inner = &trimmed[3..trimmed.len() - 1];
                                    if let Ok(pos) = inner.trim().parse::<usize>() {
                                        RegexAtom::AtPosition(pos)
                                    } else {
                                        RegexAtom::Named(name)
                                    }
                                } else {
                                    // Strip dot prefix for non-capturing named calls
                                    // <.alpha> is the same as <alpha> but without named capture
                                    let (class_name, is_dot_call) =
                                        if let Some(stripped) = trimmed.strip_prefix('.') {
                                            (stripped, true)
                                        } else {
                                            (trimmed, false)
                                        };
                                    // If this name matches a builtin char class but
                                    // the current grammar defines a token with the
                                    // same name, the grammar token takes precedence.
                                    let is_builtin_name = matches!(
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
                                            | "ident"
                                    );
                                    let grammar_overrides_builtin = is_builtin_name
                                        && !self.current_package().is_empty()
                                        && self.resolve_token_defs(class_name).is_some();
                                    if grammar_overrides_builtin {
                                        RegexAtom::Named(name)
                                    } else {
                                        // Check for named character classes
                                        match class_name {
                                            "alpha" | "upper" | "lower" | "digit" | "xdigit"
                                            | "space" | "alnum" | "blank" | "cntrl" | "punct"
                                            | "graph" | "print" => {
                                                // Set builtin named capture so $<alpha>, $<digit>, etc. work
                                                // (only for non-dot calls)
                                                if !is_dot_call {
                                                    pending_builtin_named_capture =
                                                        Some(class_name.to_string());
                                                }
                                                RegexAtom::CharClass(CharClass {
                                                    items: vec![ClassItem::NamedBuiltin(
                                                        class_name.to_string(),
                                                    )],
                                                    negated: false,
                                                })
                                            }
                                            "ident" => {
                                                // <ident> = <alpha> <alnum>*
                                                if !is_dot_call {
                                                    pending_builtin_named_capture =
                                                        Some("ident".to_string());
                                                }
                                                RegexAtom::Group(RegexPattern {
                                                    tokens: vec![
                                                        RegexToken {
                                                            atom: RegexAtom::CharClass(CharClass {
                                                                items: vec![
                                                                    ClassItem::NamedBuiltin(
                                                                        "alpha".to_string(),
                                                                    ),
                                                                ],
                                                                negated: false,
                                                            }),
                                                            quant: RegexQuant::One,
                                                            named_capture: None,
                                                            hash_capture: None,
                                                            secondary_named_capture: None,
                                                            force_list_capture: false,
                                                            ratchet: false,
                                                            frugal: false,
                                                            separator: None,
                                                        },
                                                        RegexToken {
                                                            atom: RegexAtom::CharClass(CharClass {
                                                                items: vec![
                                                                    ClassItem::NamedBuiltin(
                                                                        "alnum".to_string(),
                                                                    ),
                                                                ],
                                                                negated: false,
                                                            }),
                                                            quant: RegexQuant::ZeroOrMore,
                                                            named_capture: None,
                                                            hash_capture: None,
                                                            secondary_named_capture: None,
                                                            force_list_capture: false,
                                                            ratchet: false,
                                                            frugal: false,
                                                            separator: None,
                                                        },
                                                    ],
                                                    anchor_start: false,
                                                    anchor_end: false,
                                                    ignore_case,
                                                    ignore_mark,
                                                })
                                            }
                                            _ => {
                                                // <sym> / <.sym> can only be used in a proto regex
                                                // with :sym<> adverb — it should have been replaced
                                                // by instantiate_token_pattern before reaching here
                                                if class_name == "sym" {
                                                    PENDING_REGEX_ERROR.with(|e| {
                                                    let msg = "Can only use \"<sym>\" token in a proto regex";
                                                    let mut err = RuntimeError::new(msg);
                                                    let mut attrs = std::collections::HashMap::new();
                                                    attrs.insert("message".to_string(), Value::str(msg.to_string()));
                                                    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::Proto"), attrs);
                                                    err.exception = Some(Box::new(ex));
                                                    *e.borrow_mut() = Some(err);
                                                });
                                                    return None;
                                                }
                                                // Check for longname aliases
                                                if trimmed.contains("::") && trimmed.contains('=') {
                                                    PENDING_REGEX_ERROR.with(|e| {
                                                        *e.borrow_mut() =
                                                            Some(Self::make_longname_alias_error());
                                                    });
                                                    return None;
                                                }
                                                // No other characters are allowed after the
                                                // initial identifier of a subrule assertion
                                                // (S05). e.g. `<test*>`, `<test|>`, `<test&>`
                                                // are malformed and must be rejected at compile
                                                // time. Validate that the leading identifier is
                                                // followed only by an allowed continuation
                                                // (whitespace, `=`, `:`, `(`) or end of name.
                                                if let Some(err) =
                                                    Self::check_subrule_name_tail(class_name)
                                                {
                                                    PENDING_REGEX_ERROR.with(|e| {
                                                        *e.borrow_mut() = Some(err);
                                                    });
                                                    return None;
                                                }
                                                RegexAtom::Named(name)
                                            }
                                        }
                                    } // close else of grammar_overrides_builtin
                                }
                            } // close word-alternation else
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
                    let mut in_comment = false;
                    let mut angle_depth = 0u32;
                    while let Some(ch) = chars.next() {
                        if in_comment {
                            group_pattern.push(ch);
                            if ch == '\n' {
                                in_comment = false;
                            }
                            continue;
                        }
                        if ch == '<' {
                            angle_depth += 1;
                            group_pattern.push(ch);
                            continue;
                        }
                        if ch == '>' && angle_depth > 0 {
                            angle_depth -= 1;
                            group_pattern.push(ch);
                            continue;
                        }
                        if ch == '#' && angle_depth == 0 {
                            in_comment = true;
                            group_pattern.push(ch);
                            continue;
                        }
                        if ch == '\\' {
                            // Backslash escape — push both chars without interpreting
                            group_pattern.push(ch);
                            if let Some(next) = chars.next() {
                                group_pattern.push(next);
                            }
                            continue;
                        }
                        if ch == '\'' && angle_depth == 0 {
                            // Single-quoted string — skip until closing quote.
                            // Inside a `<...>` assertion / char class (angle_depth>0)
                            // a quote is a literal member (e.g. `<-['"]>`), not a
                            // string delimiter, so it must not swallow the group.
                            group_pattern.push(ch);
                            for sq in chars.by_ref() {
                                group_pattern.push(sq);
                                if sq == '\'' {
                                    break;
                                }
                            }
                            continue;
                        }
                        if ch == '"' && angle_depth == 0 {
                            // Double-quoted string — skip until closing quote.
                            // Inside a `<...>` assertion / char class (angle_depth>0)
                            // a `"` is a literal member (e.g. `<-["]>`), not a string
                            // delimiter — without this guard it swallowed the closing
                            // `)` and produced a spurious "Unmatched ( in regex".
                            group_pattern.push(ch);
                            for dq in chars.by_ref() {
                                group_pattern.push(dq);
                                if dq == '"' {
                                    break;
                                }
                            }
                            continue;
                        }
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
                    // If depth > 0, the group was never closed — parse error
                    if depth > 0 {
                        PENDING_REGEX_ERROR.with(|e| {
                            *e.borrow_mut() = Some(RuntimeError::typed("X::Comp::Group", {
                                let mut attrs = std::collections::HashMap::new();
                                attrs.insert(
                                    "message".to_string(),
                                    Value::str("Unmatched ( in regex".to_string()),
                                );
                                attrs
                            }));
                        });
                        return None;
                    }
                    // An empty capture group `()` is a null regex.
                    if group_pattern.trim().is_empty() {
                        PENDING_REGEX_ERROR
                            .with(|e| *e.borrow_mut() = Some(make_null_regex_error()));
                        return None;
                    }
                    let (alternatives, cap_is_sequential) =
                        Self::split_top_level_alternation(&group_pattern);
                    // A trailing/interior empty branch inside the group (`(a|)`)
                    // is null; a leading empty branch is allowed (`(|a)`).
                    if alternatives.len() > 1
                        && let Some(err) = null_regex_if_empty_branch(&alternatives, true)
                    {
                        PENDING_REGEX_ERROR.with(|e| *e.borrow_mut() = Some(err));
                        return None;
                    }
                    let needs_capture_scope = ignore_case || sigspace || ratchet || ignore_mark;
                    if alternatives.len() > 1 {
                        let mut alt_patterns = Vec::new();
                        for (alt_idx, alt) in alternatives.iter().enumerate() {
                            // A leading null alternative is ignored in Raku: `( || X )`
                            // and `( | X )` behave like `( X )`. Skip a whitespace-only
                            // first alternative so it does not contribute a spurious
                            // empty-matching (and, under ratchet, empty-winning) branch.
                            if alt_idx == 0 && alt.trim().is_empty() {
                                continue;
                            }
                            let parsed_alt = if needs_capture_scope {
                                let mut scoped = String::new();
                                if ignore_case {
                                    scoped.push_str(":i ");
                                }
                                if sigspace {
                                    scoped.push_str(":s ");
                                }
                                if ratchet {
                                    scoped.push_str(":ratchet ");
                                }
                                if ignore_mark {
                                    scoped.push_str(":m ");
                                }
                                if sigspace {
                                    scoped.push_str(alt);
                                } else {
                                    scoped.push_str(alt.trim_end());
                                }
                                self.parse_regex_with_mode(&scoped, mode)
                            } else {
                                self.parse_regex_with_mode(alt, mode)
                            };
                            if let Some(p) = parsed_alt {
                                alt_patterns.push(p);
                            }
                        }
                        if alt_patterns.len() == 1 {
                            // Only one real alternative remained after dropping the
                            // leading null: treat it as a plain (capturing) group.
                            RegexAtom::CaptureGroup(alt_patterns.into_iter().next().unwrap())
                        } else {
                            let group_atom = if cap_is_sequential {
                                RegexAtom::SequentialAlternation(alt_patterns)
                            } else {
                                try_collapse_alternation_to_charclass(&alt_patterns)
                                    .unwrap_or(RegexAtom::Alternation(alt_patterns))
                            };
                            let group_pat = RegexPattern {
                                tokens: vec![RegexToken {
                                    atom: group_atom,
                                    quant: RegexQuant::One,
                                    named_capture: None,
                                    hash_capture: None,
                                    secondary_named_capture: None,
                                    force_list_capture: false,
                                    ratchet: false,
                                    frugal: false,
                                    separator: None,
                                }],
                                anchor_start: false,
                                anchor_end: false,
                                ignore_case,
                                ignore_mark,
                            };
                            RegexAtom::CaptureGroup(group_pat)
                        }
                    } else {
                        let parsed_group = if needs_capture_scope {
                            let mut scoped = String::new();
                            if ignore_case {
                                scoped.push_str(":i ");
                            }
                            if sigspace {
                                scoped.push_str(":s ");
                            }
                            if ratchet {
                                scoped.push_str(":ratchet ");
                            }
                            if ignore_mark {
                                scoped.push_str(":m ");
                            }
                            if sigspace {
                                scoped.push_str(&group_pattern);
                            } else {
                                scoped.push_str(group_pattern.trim_end());
                            }
                            self.parse_regex_with_mode(&scoped, mode)
                        } else {
                            self.parse_regex_with_mode(&group_pattern, mode)
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
                    // An empty non-capturing group `[]` is a null regex.
                    if group_pattern.trim().is_empty() {
                        PENDING_REGEX_ERROR
                            .with(|e| *e.borrow_mut() = Some(make_null_regex_error()));
                        return None;
                    }
                    // Parse the group as top-level alternation, including `||`.
                    let (alternatives, bracket_is_sequential) =
                        Self::split_top_level_alternation(&group_pattern);
                    // A trailing/interior empty branch inside the group (`[a|]`)
                    // is null; a leading empty branch is allowed (`[|a]`).
                    if alternatives.len() > 1
                        && let Some(err) = null_regex_if_empty_branch(&alternatives, true)
                    {
                        PENDING_REGEX_ERROR.with(|e| *e.borrow_mut() = Some(err));
                        return None;
                    }
                    let needs_scope = ignore_case || sigspace || ratchet || ignore_mark;
                    if alternatives.len() > 1 {
                        let mut alt_patterns = Vec::new();
                        for (alt_idx, alt) in alternatives.iter().enumerate() {
                            // A leading null alternative is ignored in Raku: `[ || X ]`
                            // and `[ | X ]` behave like `[ X ]`. Skip a whitespace-only
                            // first alternative so it does not contribute a spurious
                            // empty-matching (and, under ratchet, empty-winning) branch.
                            if alt_idx == 0 && alt.trim().is_empty() {
                                continue;
                            }
                            let parsed_alt = if needs_scope {
                                let mut scoped = String::new();
                                if ignore_case {
                                    scoped.push_str(":i ");
                                }
                                if sigspace {
                                    scoped.push_str(":s ");
                                }
                                if ratchet {
                                    scoped.push_str(":ratchet ");
                                }
                                if ignore_mark {
                                    scoped.push_str(":m ");
                                }
                                // In sigspace mode, preserve trailing whitespace so it
                                // becomes \s* — needed for quantified groups.
                                if sigspace {
                                    scoped.push_str(alt);
                                } else {
                                    scoped.push_str(alt.trim_end());
                                }
                                self.parse_regex_with_mode(&scoped, mode)
                            } else {
                                self.parse_regex_with_mode(alt, mode)
                            };
                            if let Some(p) = parsed_alt {
                                alt_patterns.push(p);
                            }
                        }
                        if alt_patterns.len() == 1 {
                            // Only one real alternative remained after dropping the
                            // leading null: treat it as a plain non-capturing group.
                            RegexAtom::Group(alt_patterns.into_iter().next().unwrap())
                        } else if bracket_is_sequential {
                            RegexAtom::SequentialAlternation(alt_patterns)
                        } else {
                            try_collapse_alternation_to_charclass(&alt_patterns)
                                .unwrap_or(RegexAtom::Alternation(alt_patterns))
                        }
                    } else {
                        let parsed_group = if needs_scope {
                            let mut scoped = String::new();
                            if ignore_case {
                                scoped.push_str(":i ");
                            }
                            if sigspace {
                                scoped.push_str(":s ");
                            }
                            if ratchet {
                                scoped.push_str(":ratchet ");
                            }
                            if ignore_mark {
                                scoped.push_str(":m ");
                            }
                            if sigspace {
                                scoped.push_str(&group_pattern);
                            } else {
                                scoped.push_str(group_pattern.trim_end());
                            }
                            self.parse_regex_with_mode(&scoped, mode)
                        } else {
                            self.parse_regex_with_mode(&group_pattern, mode)
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
                    // Validate mode: an embedded code block may not interpolate
                    // an attribute (`{ $!attr }`).
                    if mode == RegexParseMode::Validate
                        && let Some(symbol) = find_attribute_interpolation(&code)
                    {
                        PENDING_REGEX_ERROR
                            .with(|e| *e.borrow_mut() = Some(make_attribute_regex_error(&symbol)));
                        return None;
                    }
                    // Detect P5-style {N,M} or {N,} quantifiers
                    let trimmed_code = code.trim();
                    if !trimmed_code.is_empty() {
                        let is_p5_quant = if let Some((left, right)) = trimmed_code.split_once(',')
                        {
                            left.trim().chars().all(|c| c.is_ascii_digit())
                                && !left.trim().is_empty()
                                && (right.is_empty()
                                    || right.trim().chars().all(|c| c.is_ascii_digit()))
                        } else {
                            false
                        };
                        if is_p5_quant {
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(RuntimeError::obsolete(
                                    "{N,M} as general quantifier",
                                    "** N..M (or ** N..*)",
                                ));
                            });
                            return None;
                        }
                    }
                    RegexAtom::CodeAssertion {
                        code,
                        negated: false,
                        is_assertion: false,
                    }
                }
                '~' => RegexAtom::TildeMarker,
                other => {
                    // Validate mode: an unhandled non-identifier glyph here is an
                    // unrecognized regex metacharacter (e.g. `-`, `!`, `;`). The
                    // validator accepts `=`, `,`, `|`, `&` as bare metacharacters
                    // (residual alternation/conjunction markers); `.`, `~`, `«`,
                    // `»` are handled in their own arms above. Reproduces the
                    // former validator's UnrecognizedMetachar check.
                    if mode == RegexParseMode::Validate
                        && !other.is_alphanumeric()
                        && other != '_'
                        && !matches!(other, '=' | ',' | '|' | '&')
                    {
                        // If an earlier sorrow was already recorded for this
                        // pattern (e.g. a malformed `**` range), this metachar is
                        // a follow-on sorrow rather than the top-level error: the
                        // fatal panic becomes "couldn't find final '/'", and both
                        // sorrows are bundled into the resulting X::Comp::Group.
                        let has_sorrows = REGEX_SORROWS.with(|s| !s.borrow().is_empty());
                        if has_sorrows {
                            push_regex_sorrow(unrecognized_metachar_exception(other));
                            let mut panic_err =
                                RuntimeError::new("Unable to parse regex; couldn't find final '/'");
                            panic_err.exception = Some(Box::new(regex_unparseable_panic_value()));
                            PENDING_REGEX_ERROR.with(|e| *e.borrow_mut() = Some(panic_err));
                        } else {
                            PENDING_REGEX_ERROR.with(|e| {
                                *e.borrow_mut() = Some(make_unrecognized_metachar_error(other));
                            });
                        }
                        return None;
                    }
                    RegexAtom::Literal(other)
                }
            };
            let mut quant = RegexQuant::One;
            // In Raku regex, whitespace between an atom and its quantifier is
            // insignificant. Peek past whitespace to find quantifier characters.
            {
                let mut lookahead = chars.clone();
                while lookahead.peek().is_some_and(|ch| ch.is_whitespace()) {
                    lookahead.next();
                }
                if lookahead
                    .peek()
                    .is_some_and(|ch| *ch == '*' || *ch == '+' || *ch == '?')
                {
                    // Consume the whitespace before the quantifier
                    while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                        chars.next();
                    }
                }
            }
            let mut starstar_frugal = false;
            if let Some(q) = chars.peek().copied() {
                quant = match q {
                    '*' => {
                        chars.next();
                        // Skip whitespace between `*` and potential second `*`
                        while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                            chars.next();
                        }
                        if chars.peek() == Some(&'*') {
                            // `**` quantifier: parse count or range
                            chars.next();
                            // Handle frugal modifier: **? means non-greedy
                            starstar_frugal = if chars.peek() == Some(&'?') {
                                chars.next();
                                true
                            } else {
                                false
                            };
                            // Skip whitespace after ** or **?
                            while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
                                chars.next();
                            }
                            if chars.peek() == Some(&'{') {
                                // `** {code}` — code block quantifier
                                chars.next(); // skip '{'
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
                                RegexQuant::RepeatCode(code)
                            } else {
                                // Parse the count/range: N, N..M, N..*, with
                                // optional exclusion markers (^) and underscore
                                // separators in numeric literals.
                                let mut count_str = String::new();
                                while chars.peek().is_some_and(|ch| {
                                    ch.is_ascii_digit()
                                        || *ch == '.'
                                        || *ch == '*'
                                        || *ch == '^'
                                        || *ch == '_'
                                }) {
                                    count_str.push(chars.next().unwrap());
                                }
                                // A `** N..M` range whose upper endpoint is not a
                                // digit / `*` / `^` (e.g. a negative literal
                                // `1..-1`) is malformed. Record a `MalformedRange`
                                // sorrow and let parsing continue so the trailing
                                // metachar (`-`) and the unterminated regex panic
                                // are accumulated into one `X::Comp::Group`.
                                if mode == RegexParseMode::Validate
                                    && count_str.ends_with("..")
                                    && chars.peek().is_some_and(|ch| {
                                        !ch.is_ascii_digit() && *ch != '*' && *ch != '^'
                                    })
                                {
                                    push_regex_sorrow(malformed_range_exception());
                                }
                                let (min, max) = Self::parse_quantifier_range(&count_str);
                                RegexQuant::Repeat(min, max)
                            }
                        } else {
                            RegexQuant::ZeroOrMore
                        }
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
            // Validate mode: a code block / code assertion / variable declaration
            // produces no match and cannot be quantified (e.g. `/ {}* /`,
            // `/ <?{1}>? /`). Reproduces the validator's NonQuantifiable check.
            if mode == RegexParseMode::Validate
                && !matches!(quant, RegexQuant::One)
                && matches!(
                    atom,
                    RegexAtom::CodeAssertion { .. }
                        | RegexAtom::VarDecl { .. }
                        | RegexAtom::ClosureInterpolation { .. }
                )
            {
                PENDING_REGEX_ERROR.with(|e| *e.borrow_mut() = Some(make_non_quantifiable_error()));
                return None;
            }
            // Handle frugal (non-greedy) modifier: `*?`, `+?`, `??`
            let token_frugal = if starstar_frugal {
                true
            } else if !matches!(quant, RegexQuant::One) && chars.peek() == Some(&'?') {
                chars.next();
                true
            } else {
                false
            };
            // Handle per-token backtracking control.
            // `:` enables ratchet on this token; `:!` disables it.
            // In Validate mode a `:` immediately followed by an identifier/digit
            // is a (possibly unrecognized) modifier such as `:11` — not a ratchet
            // control — so leave it for the `:` handler at the loop top to report
            // (reproduces the validator's `/00:11:22/` -> UnrecognizedModifier).
            let colon_is_modifier =
                mode == RegexParseMode::Validate && chars.peek() == Some(&':') && {
                    let mut la = chars.clone();
                    la.next();
                    la.peek()
                        .is_some_and(|ch| ch.is_alphanumeric() || *ch == '_')
                };
            let token_ratchet = if !colon_is_modifier && chars.peek() == Some(&':') {
                chars.next();
                if chars.peek() == Some(&'!') {
                    chars.next();
                    false
                } else {
                    true
                }
            } else {
                ratchet // inherit from pattern-level :ratchet flag
            };
            // Handle `%` / `%%` separator quantifier modifier, e.g.
            // `<thing>+ % ','`. Only meaningful for repeating quantifiers. The
            // separator is a single atom (the next atom in the stream); the rest
            // of the line is matched after the quantified group. `%%` permits an
            // optional trailing separator.
            let token_separator: Option<Box<RegexSeparatorSpec>> =
                if !matches!(quant, RegexQuant::One | RegexQuant::ZeroOrOne) {
                    // Skip whitespace before the `%`.
                    let mut lookahead = chars.clone();
                    while lookahead.peek().is_some_and(|c| c.is_whitespace()) {
                        lookahead.next();
                    }
                    // A `%` that begins a hash-alias capture for the FOLLOWING atom
                    // (`%<name>=...` or `%name=...`) is NOT a separator. Detect that
                    // shape and skip separator handling so the alias parses normally.
                    let is_hash_alias = {
                        let mut la = lookahead.clone();
                        if la.peek() == Some(&'%') {
                            la.next();
                            // Reject `%%` (always a separator marker, never an alias).
                            if la.peek() == Some(&'%') {
                                false
                            } else if la.peek() == Some(&'<') {
                                // `%<...>=` — scan to `>` then require `=`.
                                la.next();
                                while la.peek().is_some_and(|&c| c != '>') {
                                    la.next();
                                }
                                la.next(); // '>'
                                la.peek() == Some(&'=')
                            } else if la.peek().is_some_and(|&c| c.is_alphabetic() || c == '_') {
                                // `%name=` — scan identifier then require `=`.
                                while la.peek().is_some_and(|&c| c.is_alphanumeric() || c == '_') {
                                    la.next();
                                }
                                la.peek() == Some(&'=')
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    };
                    if lookahead.peek() == Some(&'%') && !is_hash_alias {
                        // Commit: consume up to and including the `%`/`%%`.
                        while chars.peek().is_some_and(|c| c.is_whitespace()) {
                            chars.next();
                        }
                        chars.next(); // first '%'
                        let allow_trailing = if chars.peek() == Some(&'%') {
                            chars.next();
                            true
                        } else {
                            false
                        };
                        // Skip whitespace before the separator atom.
                        while chars.peek().is_some_and(|c| c.is_whitespace()) {
                            chars.next();
                        }
                        // Collect the separator atom text (a single atom, which may
                        // carry a `$<name>=` / `%<name>=` / `@<name>=` alias prefix
                        // and/or a trailing quantifier).
                        let remaining: String = chars.clone().collect();
                        let sep_atom_str = Self::split_separator_atom(&remaining);
                        // Advance the main iterator past the separator atom.
                        for _ in 0..sep_atom_str.chars().count() {
                            chars.next();
                        }
                        self.parse_regex_with_mode(sep_atom_str.trim(), mode)
                            .map(|pattern| {
                                Box::new(RegexSeparatorSpec {
                                    pattern,
                                    allow_trailing,
                                })
                            })
                    } else {
                        None
                    }
                } else {
                    None
                };
            // When both a user alias ($<name>=) and a builtin class name are pending,
            // the alias becomes the primary capture and the builtin name becomes secondary.
            let user_alias = pending_named_capture.take();
            let user_alias_is_angle = pending_named_capture_is_angle_alias;
            pending_named_capture_is_angle_alias = false;
            let user_alias_is_array = pending_named_capture_is_array;
            pending_named_capture_is_array = false;
            let primary_is_user_alias = user_alias.is_some();
            let secondary_named = if primary_is_user_alias {
                // Alias takes precedence; builtin name (if any) becomes secondary capture.
                pending_builtin_named_capture.take()
            } else {
                None
            };
            let primary_named = user_alias.or_else(|| pending_builtin_named_capture.take());
            let hash_capture = pending_hash_capture.take();
            // A USER alias on a *quantified* atom (`$<x>=<[a..z]>*`, `$<x>=\w+`)
            // captures the WHOLE quantified span as a single Match (e.g. "abc"),
            // and a zero-width match still produces an (empty) capture — matching
            // Raku. mutsu's quantifier loop otherwise applies the named capture
            // per-iteration (yielding `[a, b, c]`) and drops the zero-match case.
            // Wrap the quantified atom in a non-capturing group so the named
            // capture sits on a `quant: One` token spanning the entire run.
            //
            // A *builtin subrule* (`<alpha>+`, `<digit>**3`) is the opposite: Raku
            // quantifies the subrule itself, producing a LIST with one Match per
            // repetition. So only wrap when the primary name is a user alias; a
            // bare builtin keeps its per-iteration captures via the quantifier loop.
            //
            // An *angle-bracket* alias of a char class / property (`<foo=[bao]>+`,
            // `<bar=:Letter>+`) is the builtin case, not the sigil case: Raku
            // quantifies the class per-iteration and yields a List, so it must NOT
            // be wrapped (only the sigil-prefix alias `$<foo>=...` wraps).
            //
            // A sigil alias on a *capturing group* (`$<x>=(\w)+`, `@<x>=(.(.))+`)
            // is ALSO the per-iteration-list case: Raku quantifies the group and
            // aliases each iteration's Match, so `$<x>` is a List of group Matches
            // (`[«a» «b» «c»]`), each preserving the group's inner captures. Only a
            // sigil alias on a non-grouping quantified atom (`$<x>=\w+`,
            // `$<x>=[\w]+`) wraps to capture the whole span as one Match.
            let wrap_named_quant = primary_is_user_alias
                && !user_alias_is_angle
                && !matches!(atom, RegexAtom::CaptureGroup(_))
                && hash_capture.is_none()
                && token_separator.is_none()
                && matches!(
                    quant,
                    RegexQuant::ZeroOrMore | RegexQuant::OneOrMore | RegexQuant::Repeat(..)
                );
            if wrap_named_quant {
                let inner = RegexToken {
                    atom,
                    quant,
                    named_capture: None,
                    hash_capture: None,
                    secondary_named_capture: None,
                    force_list_capture: false,
                    ratchet: token_ratchet,
                    frugal: token_frugal,
                    separator: None,
                };
                tokens.push(RegexToken {
                    atom: RegexAtom::Group(RegexPattern {
                        tokens: vec![inner],
                        anchor_start: false,
                        anchor_end: false,
                        ignore_case,
                        ignore_mark,
                    }),
                    quant: RegexQuant::One,
                    named_capture: primary_named,
                    hash_capture: None,
                    secondary_named_capture: secondary_named,
                    force_list_capture: user_alias_is_array,
                    ratchet: token_ratchet,
                    frugal: token_frugal,
                    separator: None,
                });
            } else {
                tokens.push(RegexToken {
                    atom,
                    quant,
                    named_capture: primary_named,
                    hash_capture,
                    secondary_named_capture: secondary_named,
                    force_list_capture: user_alias_is_array,
                    ratchet: token_ratchet,
                    frugal: token_frugal,
                    separator: token_separator,
                });
            }
        }
        let tokens = match rewrite_tilde_tokens(tokens, ignore_case, ignore_mark) {
            Ok(tokens) => tokens,
            Err(err) => {
                PENDING_REGEX_ERROR.with(|e| {
                    *e.borrow_mut() = Some(err);
                });
                return None;
            }
        };
        Some(RegexPattern {
            tokens,
            anchor_start,
            anchor_end,
            ignore_case,
            ignore_mark,
        })
    }
}
