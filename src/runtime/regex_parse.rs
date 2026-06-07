use super::*;
use crate::symbol::Symbol;
use ::regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    /// Thread-local storage for regex security errors that need to propagate
    /// from inside `parse_regex` (which takes `&self`) to callers that can throw.
    pub(super) static PENDING_REGEX_ERROR: RefCell<Option<RuntimeError>> = const { RefCell::new(None) };

    /// Memoization cache for parsing *static* regex patterns (patterns whose
    /// parse result does not depend on runtime state). Parsing a pattern is a
    /// recursive-descent operation that is pure for patterns containing no
    /// scalar/array/hash interpolation sigils (`$`, `@`, `%`); for such patterns
    /// the parsed `RegexPattern` is a deterministic function of the source string,
    /// so it is safe to cache and reuse across calls. This avoids re-parsing the
    /// same (potentially large) pattern on every step of a match — e.g. a token
    /// with dozens of alternations called once per input character.
    static REGEX_PARSE_CACHE: RefCell<HashMap<String, RegexPattern>> = RefCell::new(HashMap::new());
}

/// A pattern is cacheable iff parsing it does not depend on runtime variable
/// state. Interpolation (`interpolate_regex_scalars`) only substitutes when the
/// pattern contains a `$`, `@`, or `%` sigil, so a pattern free of those parses
/// deterministically.
fn regex_pattern_is_static(pattern: &str) -> bool {
    !pattern.contains(['$', '@', '%'])
}

fn single_token_pattern(token: RegexToken, ignore_case: bool, ignore_mark: bool) -> RegexPattern {
    RegexPattern {
        tokens: vec![token],
        anchor_start: false,
        anchor_end: false,
        ignore_case,
        ignore_mark,
    }
}

fn goal_text_for_token(token: &RegexToken) -> String {
    match &token.atom {
        RegexAtom::Literal(ch) => format!("{ch:?}"),
        RegexAtom::Named(name) => format!("<{name}>"),
        _ => "goal".to_string(),
    }
}

fn make_solitary_tilde_quantifier_error() -> RuntimeError {
    let msg = "Quantifier quantifies nothing";
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.to_string()));
    let ex = Value::make_instance(
        Symbol::intern("X::Syntax::Regex::SolitaryQuantifier"),
        attrs,
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Check if a regex token is a whitespace-like token (WsRule or <.ws> subrule call).
fn is_ws_like_token(token: &RegexToken) -> bool {
    match &token.atom {
        RegexAtom::WsRule => true,
        RegexAtom::Named(name) => {
            let mut raw = name.trim();
            if let Some(stripped) = raw.strip_prefix('.') {
                raw = stripped.trim();
            }
            raw == "ws" || raw == "ws?"
        }
        _ => false,
    }
}

fn rewrite_tilde_tokens(
    tokens: Vec<RegexToken>,
    ignore_case: bool,
    ignore_mark: bool,
) -> Result<Vec<RegexToken>, RuntimeError> {
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < tokens.len() {
        if matches!(tokens[i].atom, RegexAtom::TildeMarker) {
            if !matches!(tokens[i].quant, RegexQuant::One) {
                return Err(make_solitary_tilde_quantifier_error());
            }
            if out.is_empty() {
                return Ok(tokens);
            }
            // Remove trailing ws-like token from out (before tilde) — inserted
            // by sigspace between the opener and `~`.
            let had_pre_ws = out.last().is_some_and(is_ws_like_token);
            if had_pre_ws {
                out.pop();
            }
            // Skip ws-like tokens after the tilde to find the goal (closer)
            let mut j = i + 1;
            while j < tokens.len() && is_ws_like_token(&tokens[j]) {
                j += 1;
            }
            if j >= tokens.len() {
                return Ok(tokens);
            }
            let goal_token = tokens[j].clone();
            // Skip ws-like tokens after the goal to find the inner (content)
            let mut k = j + 1;
            while k < tokens.len() && is_ws_like_token(&tokens[k]) {
                k += 1;
            }
            if k >= tokens.len() {
                return Ok(tokens);
            }
            let inner_token = tokens[k].clone();
            // Build the inner pattern: the single content token, optionally
            // surrounded by WsRule so `rule` sigspace allows whitespace between
            // opener/content and content/closer.
            let mut inner_tokens = Vec::new();
            if had_pre_ws {
                let ws_tok = RegexToken {
                    atom: RegexAtom::WsRule,
                    quant: RegexQuant::One,
                    named_capture: None,
                    hash_capture: None,
                    secondary_named_capture: None,
                    ratchet: false,
                    frugal: false,
                    separator: None,
                };
                inner_tokens.push(ws_tok.clone());
                inner_tokens.push(inner_token);
                inner_tokens.push(ws_tok);
            } else {
                inner_tokens.push(inner_token);
            }
            let inner_pattern = RegexPattern {
                tokens: inner_tokens,
                anchor_start: false,
                anchor_end: false,
                ignore_case,
                ignore_mark,
            };
            out.push(RegexToken {
                atom: RegexAtom::GoalMatch {
                    goal: single_token_pattern(goal_token.clone(), ignore_case, ignore_mark),
                    inner: inner_pattern,
                    goal_text: goal_text_for_token(&goal_token),
                },
                quant: RegexQuant::One,
                named_capture: None,
                hash_capture: None,
                secondary_named_capture: None,
                ratchet: false,
                frugal: false,
                separator: None,
            });
            // Skip past the inner token and any trailing ws-like tokens
            i = k + 1;
            while i < tokens.len() && is_ws_like_token(&tokens[i]) {
                i += 1;
            }
            continue;
        }
        out.push(tokens[i].clone());
        i += 1;
    }
    Ok(out)
}

/// Optimize an Alternation: if all branches are single-token CharClass or Literal
/// patterns (no captures, quantifier One), merge them into a single CharClass atom.
/// This dramatically speeds up grammar tokens with many Unicode range alternations.
fn try_collapse_alternation_to_charclass(alt_patterns: &[RegexPattern]) -> Option<RegexAtom> {
    if alt_patterns.len() < 2 {
        return None;
    }
    let mut merged_items: Vec<ClassItem> = Vec::new();
    for pat in alt_patterns {
        if pat.anchor_start || pat.anchor_end {
            return None;
        }
        // Allow single-token patterns, or patterns where all extra tokens are
        // optional ws (sigspace-inserted whitespace matchers)
        let effective_tokens: Vec<&RegexToken> = pat
            .tokens
            .iter()
            .filter(|t| {
                // Keep non-ws tokens; skip optional ws tokens
                if matches!(t.quant, RegexQuant::ZeroOrOne | RegexQuant::ZeroOrMore) {
                    if let RegexAtom::CharClass(class) = &t.atom
                        && !class.negated
                        && class.items.len() == 1
                        && matches!(class.items[0], ClassItem::Space)
                    {
                        return false; // skip this ws token
                    }
                    if let RegexAtom::Named(name) = &t.atom {
                        let raw = name.trim().trim_start_matches('.').trim_start_matches('&');
                        if raw == "ws" {
                            return false;
                        }
                    }
                }
                true
            })
            .collect();
        if effective_tokens.len() != 1 {
            return None;
        }
        let token = effective_tokens[0];
        if !matches!(token.quant, RegexQuant::One) || token.named_capture.is_some() {
            return None;
        }
        match &token.atom {
            RegexAtom::CharClass(class) if !class.negated => {
                merged_items.extend(class.items.iter().cloned());
            }
            RegexAtom::Literal(ch) => {
                merged_items.push(ClassItem::Char(*ch));
            }
            _ => return None,
        }
    }
    Some(RegexAtom::CharClass(CharClass {
        negated: false,
        items: merged_items,
    }))
}

fn regex_single_quote_closes(open: char, ch: char) -> bool {
    match open {
        '\'' => ch == '\'',
        '\u{2018}' => ch == '\u{2019}',                     // ‘...’
        '\u{201A}' => ch == '\u{2019}' || ch == '\u{2018}', // ‚...’ and ‚...‘
        '\u{FF62}' => ch == '\u{FF63}',                     // ｢...｣
        _ => false,
    }
}

fn is_inside_single_quoted_regex_literal(chars: &[char], pos: usize) -> bool {
    let mut open: Option<char> = None;
    let mut escaped = false;
    for &ch in chars.iter().take(pos) {
        if escaped {
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            continue;
        }
        if let Some(open_ch) = open {
            if regex_single_quote_closes(open_ch, ch) {
                open = None;
            }
        } else if matches!(ch, '\'' | '\u{2018}' | '\u{201A}' | '\u{FF62}') {
            open = Some(ch);
        }
    }
    open.is_some()
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
                hash_capture: None,
                secondary_named_capture: None,
                ratchet: false,
                frugal: false,
                separator: None,
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
    // Also handle angle-bracket args: Numeric_Type<Digit> → ("Numeric_Type", Some("Digit"))
    if let Some(open) = s.find('<')
        && let Some(close) = s.rfind('>')
    {
        return (&s[..open], Some(&s[open + 1..close]));
    }
    (s, None)
}

/// Skip `<[...]>` character class content where quotes are literal.
fn skip_char_class_content(
    chars: &mut std::iter::Peekable<std::str::Chars>,
    current: &mut String,
    open_ch: char,
) -> bool {
    let is_char_class = {
        let tmp = chars.clone();
        let mut it = tmp;
        let c1 = it.next();
        let c2 = it.next();
        matches!(
            (c1, c2),
            (Some('['), _) | (Some('-' | '+' | '!'), Some('['))
        )
    };
    if !is_char_class {
        return false;
    }
    current.push(open_ch);
    // Scan bracket groups sequentially. Inside each [...], an unescaped '['
    // is a literal character. After ']', check for compound operators (+[, -[)
    // or the closing '>'.
    let mut entered_first = false;
    'char_class: loop {
        if !entered_first {
            // Consume chars until we hit the first '['
            for c in chars.by_ref() {
                current.push(c);
                if c == '[' {
                    break;
                }
            }
            entered_first = true;
        }
        // Scan inside a [...] group until unescaped ']'
        loop {
            match chars.next() {
                Some('\\') => {
                    current.push('\\');
                    if let Some(esc) = chars.next() {
                        current.push(esc);
                    }
                }
                Some(']') => {
                    current.push(']');
                    break;
                }
                Some(c) => current.push(c),
                None => break 'char_class,
            }
        }
        // After ']', check for compound class or closing '>'
        match chars.peek() {
            Some(&'>') => {
                current.push(chars.next().unwrap());
                break;
            }
            Some(&('+' | '-')) => {
                let op = chars.next().unwrap();
                current.push(op);
                if chars.peek() == Some(&'[') {
                    current.push(chars.next().unwrap());
                    continue 'char_class;
                }
                // Not a compound group
                if chars.peek() == Some(&'>') {
                    current.push(chars.next().unwrap());
                }
                break;
            }
            Some(&'[') => {
                current.push(chars.next().unwrap());
                continue 'char_class;
            }
            _ => break,
        }
    }
    true
}

impl Interpreter {
    fn regex_alternation_separator(out: &str) -> Option<&'static str> {
        let trimmed = out.trim_end_matches(char::is_whitespace);
        if trimmed.ends_with("||") {
            Some("||")
        } else if trimmed.ends_with('|') {
            Some("|")
        } else {
            None
        }
    }

    fn push_regex_interpolated_alternation(out: &mut String, alts: &[String]) {
        if alts.is_empty() {
            out.push_str("[]");
            return;
        }
        if alts.len() == 1 {
            out.push_str(&alts[0]);
            return;
        }
        if let Some(separator) = Self::regex_alternation_separator(out) {
            out.push_str(&alts.join(separator));
            return;
        }
        let mut ordered = alts.to_vec();
        ordered.sort_by_key(|alt| alt.len());
        out.push('[');
        out.push_str(&ordered.into_iter().rev().collect::<Vec<_>>().join("|"));
        out.push(']');
    }

    /// Split a regex pattern on top-level `|` or `||` alternation operators.
    /// Respects grouping: `(...)`, `[...]`, `{...}`, `<...>` and escapes.
    fn split_top_level_alternation(pattern: &str) -> (Vec<String>, bool) {
        let mut parts = Vec::new();
        let mut current = String::new();
        let mut depth_paren = 0i32;
        let mut depth_bracket = 0i32;
        let mut depth_brace = 0i32;
        let mut depth_angle = 0i32;
        let mut escaped = false;
        let mut in_single_quote = false;
        let mut in_double_quote = false;
        let mut is_sequential = false;
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
                    if chars.peek() == Some(&'<') {
                        current.push(ch);
                        current.push(chars.next().unwrap());
                        continue;
                    }
                    if skip_char_class_content(&mut chars, &mut current, ch) {
                        continue;
                    }
                    depth_angle += 1;
                    current.push(ch);
                }
                '>' => {
                    if chars.peek() == Some(&'>') {
                        current.push(ch);
                        current.push(chars.next().unwrap());
                        continue;
                    }
                    if depth_angle > 0 {
                        depth_angle -= 1;
                    }
                    current.push(ch);
                }
                '|' if depth_paren == 0
                    && depth_bracket == 0
                    && depth_brace == 0
                    && depth_angle == 0 =>
                {
                    // Check for || (sequential alternation)
                    if chars.peek() == Some(&'|') {
                        chars.next();
                        is_sequential = true;
                    }
                    parts.push(std::mem::take(&mut current));
                }
                _ => current.push(ch),
            }
        }
        if !current.is_empty() || !parts.is_empty() {
            parts.push(current);
        }
        (parts, is_sequential)
    }

    /// Split a regex pattern on top-level `&` (conjunction) or `&&`.
    /// Returns the parts; if there's only one part, no conjunction was present.
    fn split_top_level_conjunction(pattern: &str) -> Vec<String> {
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
                    if skip_char_class_content(&mut chars, &mut current, ch) {
                        continue;
                    }
                    depth_angle += 1;
                    current.push(ch);
                }
                '>' => {
                    if depth_angle > 0 {
                        depth_angle -= 1;
                    }
                    current.push(ch);
                }
                '&' if depth_paren == 0
                    && depth_bracket == 0
                    && depth_brace == 0
                    && depth_angle == 0 =>
                {
                    // Skip && (also conjunction, same semantics for now)
                    if chars.peek() == Some(&'&') {
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
        let chars_vec: Vec<char> = pattern.chars().collect();
        let mut i = 0;
        while i < chars_vec.len() {
            let ch = chars_vec[i];
            if escaped {
                escaped = false;
                i += 1;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                i += 1;
                continue;
            }
            if ch == '\'' && !in_double {
                in_single = !in_single;
                i += 1;
                continue;
            }
            if ch == '"' && !in_single {
                in_double = !in_double;
                i += 1;
                continue;
            }
            // Skip <...> angle brackets — % inside assertions/character classes
            // is not a separator
            if !in_single && !in_double && ch == '<' {
                i += 1;
                let mut angle_depth = 1u32;
                while i < chars_vec.len() && angle_depth > 0 {
                    let c = chars_vec[i];
                    if c == '\\' {
                        i += 2; // skip escaped char
                        continue;
                    }
                    if c == '<' {
                        angle_depth += 1;
                    } else if c == '>' {
                        angle_depth -= 1;
                    }
                    i += 1;
                }
                continue;
            }
            // Skip [...] bracket groups — % inside bracket character classes
            // is not a separator
            if !in_single && !in_double && ch == '[' {
                i += 1;
                let mut bracket_depth = 1u32;
                while i < chars_vec.len() && bracket_depth > 0 {
                    let c = chars_vec[i];
                    if c == '\\' {
                        i += 2; // skip escaped char
                        continue;
                    }
                    if c == '[' {
                        bracket_depth += 1;
                    } else if c == ']' {
                        bracket_depth -= 1;
                    }
                    i += 1;
                }
                continue;
            }
            if !in_single && !in_double && ch == '%' {
                // Check if this is hash aliasing: %<name>= or %ident=
                let mut j = i + 1;
                if j < chars_vec.len() && chars_vec[j] == '<' {
                    // Skip to >
                    j += 1;
                    while j < chars_vec.len() && chars_vec[j] != '>' {
                        j += 1;
                    }
                    if j < chars_vec.len() {
                        j += 1;
                    } // skip >
                    // Skip whitespace
                    while j < chars_vec.len() && chars_vec[j].is_whitespace() {
                        j += 1;
                    }
                    if j < chars_vec.len() && chars_vec[j] == '=' {
                        // This is hash aliasing, not a separator
                        i += 1;
                        continue;
                    }
                } else if j < chars_vec.len()
                    && (chars_vec[j].is_alphabetic() || chars_vec[j] == '_')
                {
                    while j < chars_vec.len()
                        && (chars_vec[j].is_alphanumeric()
                            || chars_vec[j] == '_'
                            || chars_vec[j] == '-')
                    {
                        j += 1;
                    }
                    while j < chars_vec.len() && chars_vec[j].is_whitespace() {
                        j += 1;
                    }
                    if j < chars_vec.len() && chars_vec[j] == '=' {
                        // This is hash aliasing, not a separator
                        i += 1;
                        continue;
                    }
                }
                return true;
            }
            i += 1;
        }
        false
    }

    /// Strip a quantifier from a bracket-delimited atom like `<value>*`.
    fn strip_bracket_quantifier(atom: &str) -> Option<(String, String)> {
        let quant = atom.chars().last()?;
        if !matches!(quant, '?' | '+' | '*') {
            return None;
        }
        let body = &atom[..atom.len() - quant.len_utf8()];
        if body.is_empty() {
            return None;
        }
        let last_body = body.chars().last()?;
        // Only handle angle-bracket subrule atoms like `<value>*`.
        // Do NOT strip quantifiers from bracket groups like `[\w+]+`.
        if last_body != '>' {
            return None;
        }
        let count_spec = match quant {
            '*' => "0..*",
            '+' => "1..*",
            '?' => "0..1",
            _ => return None,
        };
        Some((body.to_string(), count_spec.to_string()))
    }

    /// Split `'u'<cp>+` into `("'u'", "<cp>", "1..*")`.
    /// Returns `(prefix, bracket_atom, count_spec)` when the atom ends with a
    /// bracket-delimited sub-rule + simple quantifier, and there is a non-empty
    /// prefix before the opening `<`.  Used by `expand_ltm_pattern` so that a
    /// pattern like `'u' <utf16_codepoint>+ % '\u'` expands to
    /// `'u'<utf16_codepoint>('\u'<utf16_codepoint>)*` rather than incorrectly
    /// repeating the prefix with every separator element.
    fn split_prefix_and_quantified_bracket(atom: &str) -> Option<(String, String, String)> {
        let quant = atom.chars().last()?;
        if !matches!(quant, '?' | '+' | '*') {
            return None;
        }
        let body = &atom[..atom.len() - quant.len_utf8()];
        if body.is_empty() || !body.ends_with('>') {
            return None;
        }
        let char_indices: Vec<(usize, char)> = body.char_indices().collect();
        let len = char_indices.len();
        let mut depth = 0usize;
        let mut open_byte: Option<usize> = None;
        let mut i = len;
        while i > 0 {
            i -= 1;
            let (byte_pos, ch) = char_indices[i];
            match ch {
                '>' => depth += 1,
                '<' => {
                    if depth == 1 {
                        open_byte = Some(byte_pos);
                        break;
                    }
                    depth = depth.saturating_sub(1);
                }
                _ => {}
            }
        }
        let open = open_byte?;
        if open == 0 {
            return None; // no prefix before the bracket atom
        }
        let prefix = body[..open].to_string();
        let bracket_atom = body[open..].to_string();
        let count_spec = match quant {
            '*' => "0..*",
            '+' => "1..*",
            '?' => "0..1",
            _ => return None,
        };
        Some((prefix, bracket_atom, count_spec.to_string()))
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
        if let Some(esc_prefix) = prefix.strip_suffix('\\') {
            // The base char is part of a backslash escape: `\d`, `\w`, etc.
            // Treat `\X` as the atom and everything before `\` as the prefix.
            return Some((esc_prefix.to_string(), format!("\\{base}{quant}")));
        }
        Some((prefix.to_string(), format!("{base}{quant}")))
    }

    /// Parse a quantifier range string like "3", "2..4", "2..*", "2..^5",
    /// "1^..4", "1^..^5", "^5", "1_0", handling exclusive markers (^) and
    /// underscore separators in numeric literals.
    fn parse_quantifier_range(count_str: &str) -> (usize, Option<usize>) {
        fn parse_num(s: &str) -> usize {
            let cleaned: String = s.chars().filter(|c| *c != '_').collect();
            cleaned.parse::<usize>().unwrap_or(0)
        }

        // Handle ^N (shorthand for 0..^N, i.e. 0..N-1)
        if let Some(rest) = count_str.strip_prefix('^') {
            let n = parse_num(rest);
            return (0, Some(if n > 0 { n - 1 } else { 0 }));
        }

        if let Some((min_str, max_str)) = count_str.split_once("..") {
            // Handle exclusive min: N^..M -> (N+1)..M
            let min_val = if let Some(stripped) = min_str.strip_suffix('^') {
                parse_num(stripped) + 1
            } else {
                parse_num(min_str)
            };
            if max_str == "*" {
                (min_val, None)
            } else if let Some(stripped) = max_str.strip_prefix('^') {
                // Exclusive max: N..^M -> N..(M-1)
                let m = parse_num(stripped);
                (min_val, Some(if m > 0 { m - 1 } else { 0 }))
            } else {
                (min_val, Some(parse_num(max_str)))
            }
        } else {
            let exact = parse_num(count_str);
            let exact = if exact == 0 { 1 } else { exact };
            (exact, Some(exact))
        }
    }

    /// Split a compact regex string into the first atom and the remainder.
    /// Returns (first_atom, rest). Used to extract the single-atom separator
    /// for `%` / `%%` operators (Raku's `%` only takes a single atom).
    /// Extract a full separator atom from the start of `s`, including an optional
    /// `$<name>=` / `$name=` / `%<name>=` / `@<name>=` capture-alias prefix and a
    /// trailing quantifier. Returns the matched prefix (the part to consume).
    fn split_separator_atom(s: &str) -> String {
        let chars: Vec<char> = s.chars().collect();
        let mut prefix_len = 0usize;
        // Optional sigil-alias prefix: `$<name>=`, `$name=`, `%<name>=`, `@<name>=`.
        if let Some(&first) = chars.first()
            && matches!(first, '$' | '%' | '@')
        {
            let mut j = 1;
            if chars.get(j) == Some(&'<') {
                // `<name>` — scan to closing '>'
                while j < chars.len() && chars[j] != '>' {
                    j += 1;
                }
                if j < chars.len() {
                    j += 1; // consume '>'
                }
            } else {
                // bare `name`
                while j < chars.len() && (chars[j].is_alphanumeric() || chars[j] == '_') {
                    j += 1;
                }
            }
            // Require a trailing `=` to treat this as an alias prefix.
            if j > 1 && chars.get(j) == Some(&'=') {
                prefix_len = j + 1;
            }
        }
        let rest: String = chars[prefix_len..].iter().collect();
        let (atom, _) = Self::split_first_atom(&rest);
        let prefix: String = chars[..prefix_len].iter().collect();
        format!("{prefix}{atom}")
    }

    fn split_first_atom(s: &str) -> (String, String) {
        if s.is_empty() {
            return (String::new(), String::new());
        }
        let chars: Vec<char> = s.chars().collect();
        let end = match chars[0] {
            // Balanced bracket groups
            '[' | '(' | '<' => {
                let (open, close) = match chars[0] {
                    '[' => ('[', ']'),
                    '(' => ('(', ')'),
                    _ => ('<', '>'),
                };
                let mut depth = 1u32;
                let mut j = 1;
                while j < chars.len() {
                    if chars[j] == open {
                        depth += 1;
                    } else if chars[j] == close {
                        depth -= 1;
                        if depth == 0 {
                            j += 1;
                            break;
                        }
                    }
                    j += 1;
                }
                j
            }
            // Quoted strings: '...' / "..." and unicode quote pairs
            '\'' | '"' | '\u{2018}' | '\u{201A}' | '\u{201C}' | '\u{201E}' | '\u{FF62}' => {
                let close = match chars[0] {
                    '\'' => '\'',
                    '"' => '"',
                    '\u{2018}' | '\u{201A}' => '\u{2019}',
                    '\u{201C}' | '\u{201E}' => '\u{201D}',
                    '\u{FF62}' => '\u{FF63}',
                    _ => chars[0],
                };
                let mut j = 1;
                while j < chars.len() {
                    if chars[j] == '\\' {
                        j += 2;
                        continue;
                    }
                    if chars[j] == close {
                        j += 1;
                        break;
                    }
                    j += 1;
                }
                j
            }
            // Backslash escape: \x
            '\\' if chars.len() > 1 => 2,
            // Single character atom
            _ => 1,
        };
        // Also consume a trailing quantifier (+, *, ?) if present,
        // since it's part of the atom (e.g., \s+ is one quantified atom).
        let mut atom_end = end;
        if atom_end < chars.len() && matches!(chars[atom_end], '+' | '*' | '?') {
            atom_end += 1;
        }
        let first: String = chars[..atom_end].iter().collect();
        let rest: String = chars[atom_end..].iter().collect();
        (first, rest)
    }

    fn expand_ltm_pattern(pattern: &str, sigspace: bool) -> String {
        let compact: String = pattern.chars().filter(|ch| !ch.is_whitespace()).collect();
        if compact.is_empty() {
            return pattern.to_string();
        }

        static WITH_COUNT: std::sync::LazyLock<Regex> = std::sync::LazyLock::new(|| {
            Regex::new(r"^(.+?)\*\*(\^?[0-9_]+(?:\^?\.\.(?:\^?[0-9_]+|\*))?)(?:(%%|%)(.+))?$")
                .expect("ltm count regex is valid")
        });
        static BARE_SEP: std::sync::LazyLock<Regex> = std::sync::LazyLock::new(|| {
            Regex::new(r"^(.+?)(%%|%)(.+)$").expect("ltm sep regex is valid")
        });
        let with_count = &*WITH_COUNT;
        let bare_sep = &*BARE_SEP;

        if let Some(caps) = with_count.captures(&compact) {
            let atom = caps.get(1).map(|m| m.as_str()).unwrap_or_default();
            let count_spec = caps.get(2).map(|m| m.as_str()).unwrap_or_default();
            let sep_mode = caps.get(3).map(|m| m.as_str());
            let full_sep_str = caps.get(4).map(|m| m.as_str()).unwrap_or_default();
            // `%` takes only a single atom as separator
            let (sep_atom_str, sep_rest_str) = if sep_mode.is_some() {
                Self::split_first_atom(full_sep_str)
            } else {
                (String::new(), String::new())
            };
            let sep = if sep_mode.is_some() {
                Some(sep_atom_str.as_str())
            } else {
                None
            };
            let is_single_atom = Self::is_single_regex_atom(atom);
            // The string-based LTM expansion (which duplicates the atom text and
            // wraps alternations in `(...)`) renumbers captures and introduces
            // spurious positional groups. It is only needed for the separator
            // form `**N..M %sep`. For a plain `**N..M` whose atom contains a
            // capture (positional `(...)` or named `<name>`), defer to the
            // normal parser, which produces a proper `Repeat` quantifier with
            // capture folding that preserves the Match structure.
            let atom_has_capture =
                Self::atom_contains_capture(atom) || Self::atom_contains_named_capture(atom);
            if is_single_atom && (sep_mode.is_some() || !atom_has_capture) {
                // Detect empty range (e.g. 2..1) before LTM expansion
                let (parsed_min, parsed_max) = Self::parse_quantifier_range(count_spec);
                if let Some(max_val) = parsed_max
                    && parsed_min > max_val
                {
                    return pattern.to_string();
                }
                let use_spaced = sigspace && pattern.len() != compact.len();
                let expanded = if use_spaced {
                    Self::build_ltm_expansion_spaced(atom, count_spec, sep_mode, sep)
                } else {
                    Self::build_ltm_expansion(atom, count_spec, sep_mode, sep)
                };
                return if sep_rest_str.is_empty() {
                    expanded
                } else if use_spaced {
                    format!("{expanded} {sep_rest_str}")
                } else {
                    format!("{expanded}{sep_rest_str}")
                };
            }
            // Fall through to let the normal parser handle ** quantifiers
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
            let full_sep = caps.get(3).map(|m| m.as_str()).unwrap_or_default();
            // `%` takes only a single atom as separator; split off the remainder.
            let (sep_atom, sep_rest) = Self::split_first_atom(full_sep);
            let sep = Some(sep_atom.as_str());
            // When the quantified atom or the separator contains a capture, the
            // string-based expansion (`atom[sep atom]*`) would renumber captures
            // and break the Match structure. Leave the `%`/`%%` text in place so
            // the per-token parser builds a proper separator-quantifier instead.
            //
            // Under sigspace (`:s`), however, the string expansion correctly
            // inserts `<ws>` matchers around the separator, which the native
            // separator-quantifier path does not yet handle. So only defer to the
            // native path when sigspace is NOT active.
            //
            // The native path also uses a greedy chain without full backtracking
            // coordination, so it cannot correctly handle separators/atoms that
            // require backtracking against an outer anchor — namely sequential
            // alternation (`||`) or frugal quantifiers (`+?`/`*?`/`??`). For those
            // we keep the string expansion (which backtracks correctly, at the
            // cost of capture renumbering).
            let needs_backtracking = |s: &str| -> bool {
                s.contains("||") || s.contains("+?") || s.contains("*?") || s.contains("??")
            };
            if !sigspace
                && !needs_backtracking(&atom)
                && !needs_backtracking(&sep_atom)
                && (Self::atom_contains_capture(&atom)
                    || Self::atom_contains_named_capture(&atom)
                    || Self::atom_contains_capture(&sep_atom)
                    || Self::atom_contains_named_capture(&sep_atom))
            {
                return pattern.to_string();
            }
            let use_spaced = sigspace && pattern.len() != compact.len();
            let build_with_rest = |expanded: String| -> String {
                if sep_rest.is_empty() {
                    expanded
                } else if use_spaced {
                    format!("{expanded} {sep_rest}")
                } else {
                    format!("{expanded}{sep_rest}")
                }
            };
            let expand = if use_spaced {
                Self::build_ltm_expansion_spaced
            } else {
                Self::build_ltm_expansion
            };
            let sp = if use_spaced { " " } else { "" };
            if let Some((prefix, quantified_tail)) = Self::split_simple_quantified_atom(&atom) {
                return build_with_rest(format!(
                    "{prefix}{sp}{}",
                    expand(&quantified_tail, "1..*", sep_mode, sep)
                ));
            }
            // Handle prefix + quantified bracket: e.g. `'u'<cp>+` where 'u' is a
            // non-repeating prefix and only the bracket atom repeats with the separator.
            if let Some((prefix, bracket_atom, count_spec)) =
                Self::split_prefix_and_quantified_bracket(&atom)
            {
                return build_with_rest(format!(
                    "{prefix}{sp}{}",
                    expand(&bracket_atom, &count_spec, sep_mode, sep)
                ));
            }
            // Handle bracket-delimited atoms with quantifiers (e.g. `<value>*`)
            if let Some((base, count_spec)) = Self::strip_bracket_quantifier(&atom) {
                return build_with_rest(expand(&base, &count_spec, sep_mode, sep));
            }
            return build_with_rest(expand(&atom, "1..*", sep_mode, sep));
        }
        pattern.to_string()
    }

    /// Check whether an atom string contains a positional capture group `(...)`.
    /// Used to guard LTM string-expansion of `**N..M`, which duplicates the atom
    /// text and would otherwise renumber captures.
    fn atom_contains_capture(atom: &str) -> bool {
        let mut escaped = false;
        let mut in_single = false;
        let mut in_double = false;
        for ch in atom.chars() {
            if escaped {
                escaped = false;
                continue;
            }
            match ch {
                '\\' => escaped = true,
                '\'' if !in_double => in_single = !in_single,
                '"' if !in_single => in_double = !in_double,
                '(' if !in_single && !in_double => return true,
                _ => {}
            }
        }
        false
    }

    /// Check whether an atom string contains a named subrule capture `<name>`
    /// that would contribute a named entry to the Match (i.e. not a `<.foo>`,
    /// `<?...>`, `<!...>`, character class `<[...]>`/`<+...>`/`<-...>`, or
    /// modifier `<:...>`). Used alongside `atom_contains_capture` to guard
    /// LTM string-expansion of `**N..M`.
    fn atom_contains_named_capture(atom: &str) -> bool {
        let bytes: Vec<char> = atom.chars().collect();
        let mut i = 0;
        let mut escaped = false;
        let mut in_single = false;
        let mut in_double = false;
        while i < bytes.len() {
            let ch = bytes[i];
            if escaped {
                escaped = false;
                i += 1;
                continue;
            }
            match ch {
                '\\' => escaped = true,
                '\'' if !in_double => in_single = !in_single,
                '"' if !in_single => in_double = !in_double,
                '<' if !in_single && !in_double => {
                    if let Some(&next) = bytes.get(i + 1) {
                        // Capturing named subrules begin with a letter, digit,
                        // underscore, or `&` (e.g. `<&rule>`, `<name=...>`).
                        if next.is_alphanumeric() || next == '_' || next == '&' {
                            return true;
                        }
                    }
                }
                _ => {}
            }
            i += 1;
        }
        false
    }

    /// Check if a string represents a single regex atom (used to guard LTM expansion).
    /// Returns true for single characters, bracket groups, angle-bracket assertions,
    /// quoted strings, backslash escapes, and dot (any).
    fn is_single_regex_atom(s: &str) -> bool {
        if s.is_empty() {
            return false;
        }
        let chars: Vec<char> = s.chars().collect();
        // Single character
        if chars.len() == 1 {
            return true;
        }
        // Backslash escape: \x
        if chars[0] == '\\' && chars.len() == 2 {
            return true;
        }
        // Bracket groups: [...], (...), <...>, '...'
        match chars[0] {
            '[' => *chars.last().unwrap_or(&' ') == ']',
            '(' => *chars.last().unwrap_or(&' ') == ')',
            '<' => *chars.last().unwrap_or(&' ') == '>',
            '\'' => chars.len() >= 2 && *chars.last().unwrap_or(&' ') == '\'',
            '"' => chars.len() >= 2 && *chars.last().unwrap_or(&' ') == '"',
            _ => false,
        }
    }

    fn build_ltm_expansion(
        atom: &str,
        count_spec: &str,
        sep_mode: Option<&str>,
        sep: Option<&str>,
    ) -> String {
        Self::build_ltm_expansion_inner(atom, count_spec, sep_mode, sep, false)
    }

    fn build_ltm_expansion_spaced(
        atom: &str,
        count_spec: &str,
        sep_mode: Option<&str>,
        sep: Option<&str>,
    ) -> String {
        Self::build_ltm_expansion_inner(atom, count_spec, sep_mode, sep, true)
    }

    fn build_ltm_expansion_inner(
        atom: &str,
        count_spec: &str,
        sep_mode: Option<&str>,
        sep: Option<&str>,
        spaced: bool,
    ) -> String {
        let (min, max) = Self::parse_quantifier_range(count_spec);

        let allow_trailing_sep = matches!(sep_mode, Some("%%"));
        let sep = sep.unwrap_or_default();
        let sp = if spaced { " " } else { "" };
        // In spaced mode, insert explicit <ws> around the separator inside
        // repetition groups so sigspace works at iteration boundaries.
        let sws = if spaced { "<ws>" } else { "" };

        let repeat_atom = |count: usize| -> String {
            if spaced {
                let atoms: Vec<&str> = (0..count).map(|_| atom).collect();
                atoms.join(" ")
            } else {
                atom.repeat(count)
            }
        };
        let build_exact_list_inner = |count: usize, trailing: bool| -> String {
            if count == 0 {
                return String::new();
            }
            let mut out = atom.to_string();
            for _ in 1..count {
                out.push_str(&format!("{sws}{sep}{sws}{atom}"));
            }
            if trailing && allow_trailing_sep {
                out.push_str(&format!("[{sws}{sep}]?"));
            }
            out
        };
        let build_exact_list = |count: usize| -> String { build_exact_list_inner(count, true) };

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
                None => format!("{}[{sp}{atom}]*", repeat_atom(min)),
            };
        }

        if max.is_none() && min == 1 && atom.ends_with('?') && !sep.is_empty() {
            if allow_trailing_sep {
                return format!("{atom}[{sws}{sep}]?");
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
                if min == 0 {
                    // 0..* with separator: [atom[sep atom]*]?
                    let mut inner = format!("{atom}[{sws}{sep}{sws}{atom}]*");
                    if allow_trailing_sep {
                        inner.push_str(&format!("[{sws}{sep}]?"));
                    }
                    format!("[{inner}]?")
                } else {
                    // Use inner without trailing sep — the trailing sep is
                    // added after the unbounded repetition group below.
                    let mut out = build_exact_list_inner(min, false);
                    out.push_str(&format!("[{sws}{sep}{sws}{atom}]*"));
                    if allow_trailing_sep {
                        out.push_str(&format!("[{sws}{sep}]?"));
                    }
                    out
                }
            }
        }
    }

    pub(super) fn parse_regex(&self, pattern: &str) -> Option<RegexPattern> {
        // Fast path: reuse a previously-parsed result for static patterns.
        if regex_pattern_is_static(pattern) {
            if let Some(cached) = REGEX_PARSE_CACHE.with(|c| c.borrow().get(pattern).cloned()) {
                return Some(cached);
            }
            let parsed = self.parse_regex_uncached(pattern);
            if let Some(ref p) = parsed {
                REGEX_PARSE_CACHE.with(|c| {
                    c.borrow_mut().insert(pattern.to_string(), p.clone());
                });
            }
            return parsed;
        }
        self.parse_regex_uncached(pattern)
    }

    fn parse_regex_uncached(&self, pattern: &str) -> Option<RegexPattern> {
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

        let interpolated = match self.interpolate_regex_scalars(pattern) {
            Ok(s) => s,
            Err(e) => {
                PENDING_REGEX_ERROR.with(|err| {
                    *err.borrow_mut() = Some(e);
                });
                return None;
            }
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
        // Handle top-level alternation (| or ||)
        let (top_alts, is_sequential) = Self::split_top_level_alternation(source);
        if top_alts.len() > 1 {
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
                if let Some(p) = self.parse_regex(&alt_pat) {
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
                if let Some(p) = self.parse_regex(&part_pat) {
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

        let expanded = Self::expand_ltm_pattern(source, sigspace);
        let mut chars = expanded.chars().peekable();
        let mut tokens = Vec::new();
        let mut anchor_start = false;
        let mut anchor_end = false;
        let mut pending_named_capture: Option<String> = None;
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
                    ratchet,
                    frugal: false,
                    separator: None,
                });
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
                        other => RegexAtom::Literal(other),
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
                                    // \x[HEX] inside double-quoted regex string
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
                            let Some(inner_pattern) = self.parse_regex(&inner) else {
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
                                            name = rhs.to_string();
                                        }
                                    }
                                }
                                // Check for Raku character class: <[...]>, <-[...]>, <+[...]>
                                // Also handles composite: <[a..z]-[aeiou]>, <+[a..z]-[aeiou]-[y]>
                                let trimmed = name.trim();
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
                                    if let Some(atom) = self.parse_combined_class(trimmed) {
                                        atom
                                    } else {
                                        continue;
                                    }
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
                                        Value::RegexWithAdverbs { pattern, .. } => {
                                            pattern.to_string()
                                        }
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
                                    if let Some(parsed) = self.parse_regex(&scoped_pat) {
                                        RegexAtom::Group(parsed)
                                    } else {
                                        continue;
                                    }
                                } else if trimmed.starts_with('@') {
                                    // <@var> — look up array variable and compile
                                    // each element as a regex pattern (alternation)
                                    let env_key = trimmed.to_string(); // includes @
                                    let value =
                                        self.env.get(&env_key).cloned().unwrap_or(Value::Nil);
                                    let elements = match &value {
                                        Value::Array(arr, _) => arr.as_ref().clone(),
                                        _ => vec![value],
                                    };
                                    let mut alt_patterns = Vec::new();
                                    for elt in &elements {
                                        let pat_str = match elt {
                                            Value::Regex(pat) => pat.to_string(),
                                            Value::RegexWithAdverbs { pattern, .. } => {
                                                pattern.to_string()
                                            }
                                            other => other.to_string_value(),
                                        };
                                        if let Some(parsed) = self.parse_regex(&pat_str) {
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
                                        && !self.current_package.is_empty()
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
                        if ch == '\'' {
                            // Single-quoted string — skip until closing quote
                            group_pattern.push(ch);
                            for sq in chars.by_ref() {
                                group_pattern.push(sq);
                                if sq == '\'' {
                                    break;
                                }
                            }
                            continue;
                        }
                        if ch == '"' {
                            // Double-quoted string — skip until closing quote
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
                    let (alternatives, cap_is_sequential) =
                        Self::split_top_level_alternation(&group_pattern);
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
                                self.parse_regex(&scoped)
                            } else {
                                self.parse_regex(alt)
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
                    // Parse the group as top-level alternation, including `||`.
                    let (alternatives, bracket_is_sequential) =
                        Self::split_top_level_alternation(&group_pattern);
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
                                self.parse_regex(&scoped)
                            } else {
                                self.parse_regex(alt)
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
                other => RegexAtom::Literal(other),
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
            let token_ratchet = if chars.peek() == Some(&':') {
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
                        Self::parse_regex(self, sep_atom_str.trim()).map(|pattern| {
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
            let primary_named = pending_named_capture.take();
            let secondary_named = if primary_named.is_some() {
                // Alias takes precedence; builtin name (if any) becomes secondary capture.
                pending_builtin_named_capture.take()
            } else {
                None
            };
            let primary_named = primary_named.or_else(|| pending_builtin_named_capture.take());
            let hash_capture = pending_hash_capture.take();
            // A named capture on a *quantified* atom (`$<x>=<[a..z]>*`, `$<x>=\w+`)
            // captures the WHOLE quantified span as a single Match (e.g. "abc"),
            // and a zero-width match still produces an (empty) capture — matching
            // Raku. mutsu's quantifier loop otherwise applies the named capture
            // per-iteration (yielding `[a, b, c]`) and drops the zero-match case.
            // Wrap the quantified atom in a non-capturing group so the named
            // capture sits on a `quant: One` token spanning the entire run.
            let wrap_named_quant = primary_named.is_some()
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

    /// Try to parse an inline scope modifier from the remaining source after ':'.
    /// Returns `Some(remaining)` if a modifier was recognized (and flags updated),
    /// or `None` if no modifier matched.
    fn try_parse_inline_modifier<'a>(
        remaining: &'a str,
        ratchet: &mut bool,
        ignore_case: &mut bool,
        ignore_mark: &mut bool,
        sigspace: &mut bool,
    ) -> Option<&'a str> {
        fn is_word_boundary(rest: &str) -> bool {
            rest.is_empty() || !rest.starts_with(|c: char| c.is_ascii_alphanumeric() || c == '_')
        }
        fn is_short_boundary(rest: &str) -> bool {
            rest.is_empty()
                || rest.starts_with(' ')
                || rest.starts_with(':')
                || rest.starts_with('/')
        }
        // Check negated long forms first
        if let Some(rest) = remaining.strip_prefix("!ratchet")
            && is_word_boundary(rest)
        {
            *ratchet = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!ignorecase")
            && is_word_boundary(rest)
        {
            *ignore_case = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!ignoremark")
            && is_word_boundary(rest)
        {
            *ignore_mark = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!sigspace")
            && is_word_boundary(rest)
        {
            *sigspace = false;
            return Some(rest);
        }
        // Negated short forms
        if let Some(rest) = remaining.strip_prefix("!r")
            && is_short_boundary(rest)
        {
            *ratchet = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!i")
            && is_short_boundary(rest)
        {
            *ignore_case = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!s")
            && is_short_boundary(rest)
        {
            *sigspace = false;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("!m")
            && is_short_boundary(rest)
        {
            *ignore_mark = false;
            return Some(rest);
        }
        // Positive long forms
        if let Some(rest) = remaining.strip_prefix("ratchet")
            && is_word_boundary(rest)
        {
            *ratchet = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("ignorecase")
            && is_word_boundary(rest)
        {
            *ignore_case = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("ignoremark")
            && is_word_boundary(rest)
        {
            *ignore_mark = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("sigspace")
            && is_word_boundary(rest)
        {
            *sigspace = true;
            return Some(rest);
        }
        // Positive short forms
        if let Some(rest) = remaining.strip_prefix("r")
            && is_short_boundary(rest)
        {
            *ratchet = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("i")
            && is_short_boundary(rest)
        {
            *ignore_case = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("s")
            && is_short_boundary(rest)
        {
            *sigspace = true;
            return Some(rest);
        }
        if let Some(rest) = remaining.strip_prefix("m")
            && is_short_boundary(rest)
        {
            *ignore_mark = true;
            return Some(rest);
        }
        None
    }

    pub(super) fn interpolate_regex_scalars(&self, pattern: &str) -> Result<String, RuntimeError> {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        while i < chars.len() {
            let ch = chars[i];
            // # starts a comment — skip without interpolation.
            // #`[...] is an embedded comment; plain # is a line comment.
            if ch == '#' {
                if i + 1 < chars.len() && chars[i + 1] == '`' {
                    out.push(chars[i]);
                    i += 1;
                    out.push(chars[i]); // `
                    i += 1;
                    if i < chars.len() {
                        let bracket = chars[i];
                        let close = match bracket {
                            '[' => ']',
                            '(' => ')',
                            '{' => '}',
                            '<' => '>',
                            _ => bracket,
                        };
                        out.push(bracket);
                        i += 1;
                        let mut embed_depth = 1u32;
                        while i < chars.len() && embed_depth > 0 {
                            let c = chars[i];
                            if c == bracket && bracket != close {
                                embed_depth += 1;
                            } else if c == close {
                                embed_depth -= 1;
                            }
                            out.push(c);
                            i += 1;
                        }
                    }
                } else {
                    while i < chars.len() && chars[i] != '\n' {
                        out.push(chars[i]);
                        i += 1;
                    }
                }
                continue;
            }
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
            // Skip <...> angle brackets — don't interpolate variables inside them.
            // The tokenizer handles <$var>, <@var>, <{code}>, etc. directly.
            if ch == '<' {
                let mut depth = 1usize;
                out.push(ch);
                i += 1;
                while i < chars.len() && depth > 0 {
                    let c = chars[i];
                    if c == '\\' {
                        out.push(c);
                        i += 1;
                        if i < chars.len() {
                            out.push(chars[i]);
                            i += 1;
                        }
                        continue;
                    }
                    if c == '<' {
                        depth += 1;
                    } else if c == '>' {
                        depth -= 1;
                    }
                    out.push(c);
                    i += 1;
                }
                continue;
            }
            if ch == '$' {
                let inside_sq = is_inside_single_quoted_regex_literal(&chars, i);
                let mut j = i + 1;
                if j < chars.len() && chars[j] == '{' {
                    j += 1;
                    let name_start = j;
                    while j < chars.len() && chars[j] != '}' {
                        j += 1;
                    }
                    if j < chars.len() && j > name_start {
                        // Inside single-quoted regex literals, $ is not interpolated
                        if inside_sq {
                            out.push('$');
                            i += 1;
                            continue;
                        }
                        let name: String = chars[name_start..j].iter().collect();
                        let value = self
                            .env
                            .get(&name)
                            .cloned()
                            .or_else(|| self.env.get(&format!("${name}")).cloned())
                            .unwrap_or(Value::Nil);
                        Self::check_hash_in_regex(&value)?;
                        Self::push_value_as_regex_pattern(&value, &mut out);
                        i = j + 1;
                        continue;
                    }
                } else if j < chars.len()
                    && (chars[j].is_alphabetic()
                        || chars[j] == '_'
                        || matches!(chars[j], '*' | '?' | '^' | '.'))
                {
                    let name_start = j;
                    // Skip twigil if present
                    if matches!(chars[j], '*' | '?' | '^' | '.') {
                        j += 1;
                    }
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    // Inside single-quoted regex literals, $ is not interpolated
                    if inside_sq {
                        out.push('$');
                        i += 1;
                        continue;
                    }
                    let name: String = chars[name_start..j].iter().collect();
                    let value = self
                        .env
                        .get(&name)
                        .cloned()
                        .or_else(|| self.env.get(&format!("${name}")).cloned())
                        .unwrap_or(Value::Nil);
                    Self::check_hash_in_regex(&value)?;
                    Self::push_value_as_regex_pattern(&value, &mut out);
                    i = j;
                    continue;
                } else if j < chars.len() && chars[j] == '(' {
                    // $( expr ) — scalar contextualizer: evaluate expr
                    // and match the result as a literal string.
                    if inside_sq {
                        out.push('$');
                        i += 1;
                        continue;
                    }
                    j += 1; // skip '('
                    let mut depth = 1usize;
                    let expr_start = j;
                    while j < chars.len() && depth > 0 {
                        if chars[j] == '(' {
                            depth += 1;
                        } else if chars[j] == ')' {
                            depth -= 1;
                        }
                        if depth > 0 {
                            j += 1;
                        }
                    }
                    let expr_str: String = chars[expr_start..j].iter().collect();
                    j += 1; // skip closing ')'
                    let val = self.eval_string_as_source(&expr_str);
                    let literal = val.to_string_value();
                    out.push_str(&Self::escape_regex_scalar_literal(&literal));
                    i = j;
                    continue;
                }
            }
            if ch == '@' {
                // Inside single-quoted regex literals, @ is not interpolated
                if is_inside_single_quoted_regex_literal(&chars, i) {
                    out.push('@');
                    i += 1;
                    continue;
                }
                let mut j = i + 1;
                // @$var — dereference scalar as array for alternation
                if j < chars.len() && chars[j] == '$' {
                    j += 1; // skip '$'
                    let name_start = j;
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    if j > name_start {
                        let bare_name: String = chars[name_start..j].iter().collect();
                        let value = self
                            .env
                            .get(&bare_name)
                            .cloned()
                            .or_else(|| self.env.get(&format!("${bare_name}")).cloned())
                            .unwrap_or(Value::Nil);
                        let elements = match &value {
                            Value::Array(arr, _) => arr.as_ref().clone(),
                            _ => vec![value],
                        };
                        let mut alts = Vec::new();
                        for elt in &elements {
                            match elt {
                                Value::Regex(pat) => alts.push(pat.to_string()),
                                Value::RegexWithAdverbs { pattern, .. } => {
                                    alts.push(pattern.to_string())
                                }
                                other => alts.push(Self::escape_regex_scalar_literal(
                                    &other.to_string_value(),
                                )),
                            }
                        }
                        Self::push_regex_interpolated_alternation(&mut out, &alts);
                        i = j;
                        continue;
                    }
                }
                if j < chars.len() && (chars[j].is_alphabetic() || chars[j] == '_') {
                    let name_start = j;
                    while j < chars.len()
                        && (chars[j].is_alphanumeric() || chars[j] == '_' || chars[j] == '-')
                    {
                        j += 1;
                    }
                    let bare_name: String = chars[name_start..j].iter().collect();
                    let sigiled_name = format!("@{bare_name}");
                    let value = self
                        .env
                        .get(&sigiled_name)
                        .cloned()
                        .or_else(|| self.env.get(&bare_name).cloned())
                        .unwrap_or(Value::Nil);
                    let elements = match &value {
                        Value::Array(arr, _) => arr.as_ref().clone(),
                        _ => vec![value],
                    };
                    let mut alts = Vec::new();
                    for elt in &elements {
                        match elt {
                            Value::Regex(pat) => alts.push(pat.to_string()),
                            Value::RegexWithAdverbs { pattern, .. } => {
                                alts.push(pattern.to_string())
                            }
                            other => alts
                                .push(Self::escape_regex_scalar_literal(&other.to_string_value())),
                        }
                    }
                    Self::push_regex_interpolated_alternation(&mut out, &alts);
                    i = j;
                    continue;
                } else if j < chars.len() && chars[j] == '(' {
                    j += 1; // skip '('
                    let mut depth = 1usize;
                    let expr_start = j;
                    while j < chars.len() && depth > 0 {
                        if chars[j] == '(' {
                            depth += 1;
                        } else if chars[j] == ')' {
                            depth -= 1;
                        }
                        if depth > 0 {
                            j += 1;
                        }
                    }
                    let expr_str: String = chars[expr_start..j].iter().collect();
                    j += 1; // skip closing ')'
                    let val = self.eval_string_as_source(&expr_str);
                    let elements = match &val {
                        Value::Array(arr, _) => arr.as_ref().clone(),
                        _ => vec![val],
                    };
                    let mut alts = Vec::new();
                    for elt in elements.iter() {
                        match elt {
                            Value::Regex(pat) => alts.push(pat.to_string()),
                            Value::RegexWithAdverbs { pattern, .. } => {
                                alts.push(pattern.to_string())
                            }
                            other => alts
                                .push(Self::escape_regex_scalar_literal(&other.to_string_value())),
                        }
                    }
                    Self::push_regex_interpolated_alternation(&mut out, &alts);
                    i = j;
                    continue;
                }
            }
            out.push(ch);
            i += 1;
        }
        Ok(out)
    }

    /// Convert a Value to its regex pattern representation and push to output.
    /// Handles Nil (always-fail), Regex, Junction (alternation), and literals.
    /// Check if a pattern string contains `$varname` references to undeclared
    /// variables. Used by the `<$var>` handler to detect undeclared variables
    /// in the resolved pattern content (one level of reinterpretation).
    fn check_undeclared_vars_in_pattern(&self, pattern: &str) -> Option<RuntimeError> {
        let chars: Vec<char> = pattern.chars().collect();
        let mut i = 0;
        while i < chars.len() {
            if chars[i] == '$' {
                let mut j = i + 1;
                // Skip twigil if present
                if j < chars.len() && matches!(chars[j], '*' | '?' | '^' | '.') {
                    j += 1;
                }
                if j < chars.len() && (chars[j].is_alphabetic() || chars[j] == '_') {
                    let name_start = i + 1; // include twigil in name
                    let mut end = j;
                    while end < chars.len()
                        && (chars[end].is_alphanumeric() || chars[end] == '_' || chars[end] == '-')
                    {
                        end += 1;
                    }
                    let name: String = chars[name_start..end].iter().collect();
                    if self.env.get(&name).is_none() && self.env.get(&format!("${name}")).is_none()
                    {
                        let symbol = format!("${name}");
                        let msg = format!("Variable '{symbol}' is not declared");
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("symbol".to_string(), Value::str(symbol));
                        attrs.insert("message".to_string(), Value::str(msg.clone()));
                        let ex = Value::make_instance(Symbol::intern("X::Undeclared"), attrs);
                        let mut err = RuntimeError::new(&msg);
                        err.exception = Some(Box::new(ex));
                        return Some(err);
                    }
                    i = end;
                    continue;
                }
            }
            i += 1;
        }
        None
    }

    fn push_value_as_regex_pattern(value: &Value, out: &mut String) {
        match value {
            Value::Nil => out.push_str("<!>"),
            Value::Regex(pat) => out.push_str(pat),
            Value::RegexWithAdverbs { pattern, .. } => out.push_str(pattern),
            Value::Junction { values, .. } => {
                // Expand junction values as alternation [v1|v2|...]
                out.push('[');
                for (idx, v) in values.iter().enumerate() {
                    if idx > 0 {
                        out.push('|');
                    }
                    match v {
                        Value::Regex(pat) => out.push_str(pat),
                        Value::RegexWithAdverbs { pattern, .. } => out.push_str(pattern),
                        other => out
                            .push_str(&Self::escape_regex_scalar_literal(&other.to_string_value())),
                    }
                }
                out.push(']');
            }
            other => out.push_str(&Self::escape_regex_scalar_literal(&other.to_string_value())),
        }
    }

    /// Check if a value is a Hash and throw X::Syntax::Reserved if so.
    fn check_hash_in_regex(value: &Value) -> Result<(), RuntimeError> {
        if matches!(value, Value::Hash(_)) {
            let msg = "The use of hashes in regexes is reserved";
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.to_string()));
            let ex = Value::make_instance(Symbol::intern("X::Syntax::Reserved"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        Ok(())
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

    /// Check if a regex pattern string contains dangerous code that could
    /// be used for injection attacks. Returns true if the pattern is dangerous.
    pub(super) fn contains_dangerous_regex_code(pattern: &str) -> bool {
        let s = pattern.trim();
        // Check for nested assertions: <$var>, <@var> inside a reinterpreted string
        if s.contains("<$") || s.contains("<@") {
            return true;
        }
        // Check for code interpolation patterns
        if s.contains("$(") || s.contains("@(") {
            return true;
        }
        // Check for braces: { or } could indicate code blocks
        if s.contains('{') || s.contains('}') {
            return true;
        }
        // Check for dynamic lookups: <::(...)>
        if s.contains("::(") {
            return true;
        }
        // Check for double-quoted strings with interpolation
        if s.contains('"') {
            let in_dq: Vec<&str> = s.split('"').collect();
            for (i, chunk) in in_dq.iter().enumerate() {
                if i % 2 == 1
                    && (chunk.contains('$')
                        || chunk.contains('@')
                        || chunk.contains('%')
                        || chunk.contains('&'))
                {
                    return true;
                }
            }
        }
        // Check for named rule with parens containing code: <alpha(...)>
        static NAMED_RULE_RE: std::sync::LazyLock<Regex> =
            std::sync::LazyLock::new(|| Regex::new(r"<\w+\(.*\)>").expect("valid regex"));
        if NAMED_RULE_RE.find(s).is_some() {
            return true;
        }
        // Check for :my variable declaration
        if s.contains(":my ") || s.contains(":our ") {
            return true;
        }
        // Check for "$x:(..." extended colonpair syntax
        if s.contains(":(") {
            return true;
        }
        false
    }

    /// Check if a regex pattern string contains a longname alias
    /// (e.g., `<IO::File=bar>` or `<::IO::File=bar>`). Returns true if so.
    pub(super) fn contains_longname_alias(pattern: &str) -> bool {
        // Look for <...::...=...> pattern
        let s = pattern.trim();
        if s.contains("::") && s.contains('=') {
            return true;
        }
        false
    }

    /// Validate the tail of a subrule-assertion name. Per S05, no characters
    /// other than the recognised continuations may follow the initial
    /// identifier of a `<ident...>` subrule. Returns a malformed-regex error
    /// when, for example, a bare regex metacharacter (`*`, `|`, `&`, ...)
    /// immediately follows the identifier (e.g. `<test*>`).
    pub(super) fn check_subrule_name_tail(name: &str) -> Option<RuntimeError> {
        let mut chars = name.chars().peekable();
        // The leading identifier must start with an alphabetic char or `_`.
        match chars.peek() {
            Some(c) if c.is_alphabetic() || *c == '_' => {}
            _ => return None,
        }
        // Consume the (possibly long) identifier: word characters plus the
        // intra-identifier connectors `-`, `'`, and `::` package separators.
        while let Some(&c) = chars.peek() {
            if c.is_alphanumeric() || c == '_' || c == '-' || c == '\'' || c == ':' {
                chars.next();
            } else {
                break;
            }
        }
        // Whatever remains is the tail. An empty tail or a tail beginning with
        // an allowed continuation is fine; anything else is malformed.
        match chars.peek() {
            // End of name, argument list, alias, method-args, or a passed regex.
            None | Some('(') | Some('=') => None,
            Some(c) if c.is_whitespace() => None,
            Some(_) => {
                let msg = "Unable to parse regex; couldn't find delimiter";
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("message".to_string(), Value::str(msg.to_string()));
                let ex =
                    Value::make_instance(Symbol::intern("X::Syntax::Regex::Unterminated"), attrs);
                let mut err = RuntimeError::new(msg);
                err.exception = Some(Box::new(ex));
                Some(err)
            }
        }
    }

    /// Create an X::Syntax::Regex::Alias::LongName error.
    pub(super) fn make_longname_alias_error() -> RuntimeError {
        let msg = "Can't use a long name as a regex alias";
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.to_string()));
        let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::Alias::LongName"), attrs);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(ex));
        err
    }

    /// Create an X::SecurityPolicy error for prohibited regex interpolation.
    pub(super) fn make_security_policy_error() -> RuntimeError {
        let msg = "Prohibited regex interpolation";
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.to_string()));
        let ex = Value::make_instance(Symbol::intern("X::SecurityPolicy"), attrs);
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(ex));
        err
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
    fn parse_bracket_char_class(&self, input: &str) -> Option<RegexAtom> {
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
                        let is_grammar_token = !self.current_package.is_empty()
                            && self.resolve_token_defs(class_name).is_some();
                        if !is_grammar_token {
                            // Unknown name outside grammar context — set error
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
    fn eval_string_as_source(&self, code: &str) -> Value {
        let parsed = crate::parse_dispatch::parse_source(code);
        let (stmts, _) = match parsed {
            Ok(v) => v,
            Err(_) => return Value::Nil,
        };
        let mut interp = Interpreter {
            env: self.env.clone(),
            functions: self.functions.clone(),
            token_defs: self.token_defs.clone(),
            current_package: self.current_package.clone(),
            ..Default::default()
        };
        match interp.eval_block_value(&stmts) {
            Ok(v) => v,
            Err(e) => e.return_value.unwrap_or(Value::Nil),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interpolate_array_var_into_alternation() {
        let mut interp = Interpreter::default();
        interp.env.insert(
            "@list".to_string(),
            Value::array(vec![
                Value::str_from("x"),
                Value::str_from("xx"),
                Value::str_from("xxxx"),
            ]),
        );
        let interpolated = interp.interpolate_regex_scalars(" ||@list ").unwrap();
        assert_eq!(interpolated, " ||x||xx||xxxx ");
    }

    #[test]
    fn scalar_inside_single_quoted_regex_atom_is_literal() {
        let interp = Interpreter::default();
        let interpolated = interp.interpolate_regex_scalars("'$param'").unwrap();
        // $ inside single-quoted regex atoms should not be interpolated —
        // the $ is kept literal per Raku spec.
        assert_eq!(interpolated, "'$param'");
    }
}
