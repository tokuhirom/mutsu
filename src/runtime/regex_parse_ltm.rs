use super::regex_parse::*;
use super::*;
use ::regex::Regex;

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

    pub(super) fn push_regex_interpolated_alternation(out: &mut String, alts: &[String]) {
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
    pub(super) fn split_top_level_alternation(pattern: &str) -> (Vec<String>, bool) {
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
    pub(super) fn split_top_level_conjunction(pattern: &str) -> Vec<String> {
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
    pub(super) fn parse_quantifier_range(count_str: &str) -> (usize, Option<usize>) {
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
    pub(super) fn split_separator_atom(s: &str) -> String {
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

    pub(super) fn expand_ltm_pattern(pattern: &str, sigspace: bool) -> String {
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
            // The native separator-quantifier path now backtracks the separator
            // and an optional trailing separator against an outer anchor (see
            // `match_separated_quantifier`), so it correctly handles sequential
            // alternation (`||`) atoms and frugal separators (`(.+?)`) too — and
            // unlike the string expansion it preserves the per-iteration capture
            // structure. So whenever a capture is present (and sigspace is off),
            // defer to the native path regardless of backtracking shape.
            if !sigspace
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
                // `**0` matches exactly zero repetitions — an empty match, not a
                // null regex. Emit an empty string literal (a zero-width atom)
                // rather than a bare empty pattern, which would be rejected as
                // X::Syntax::Regex::NullRegex.
                Some(max) if max == min => {
                    if min == 0 {
                        "''".to_string()
                    } else {
                        repeat_atom(min)
                    }
                }
                Some(max) => {
                    // For `**0..max`, build the 1..=max alternatives and make the
                    // whole group optional (`[...]?`) for the zero-rep case,
                    // instead of appending an empty trailing alternation branch
                    // like `(aa|a|)` — that empty branch would be misdetected as
                    // a null regex.
                    let lo = if min == 0 { 1 } else { min };
                    let alts: Vec<String> = (lo..=max).rev().map(repeat_atom).collect();
                    let core = if alts.len() == 1 {
                        alts.into_iter().next().unwrap_or_default()
                    } else {
                        // Non-capturing `[...]`: a plain `**N..M` on a
                        // non-capturing atom must not introduce a positional
                        // capture (`(...)` would). Capture-bearing atoms never
                        // reach this string-expansion path.
                        format!("[{}]", alts.join("|"))
                    };
                    if min == 0 { format!("[{core}]?") } else { core }
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

    /// Parse a runtime (`Match`-mode) regex pattern, returning a shared
    /// `Arc<RegexPattern>`. For a static pattern the compiled tree is memoized
    /// in `REGEX_PARSE_CACHE`, so a repeat call (the hot match loop re-fetches
    /// the same pattern on every step / iteration) is a refcount bump rather
    /// than a deep clone of the whole token tree (the previous owned-`RegexPattern`
    /// cache cloned the tree on every hit — ANALYSIS §8.4).
    pub(super) fn parse_regex(&self, pattern: &str) -> Option<std::sync::Arc<RegexPattern>> {
        if regex_pattern_is_static(pattern) {
            if let Some(cached) = REGEX_PARSE_CACHE.with(|c| c.borrow().get(pattern).cloned()) {
                return Some(cached);
            }
            let parsed = self
                .parse_regex_uncached(pattern, RegexParseMode::Match)
                .map(std::sync::Arc::new);
            if let Some(ref p) = parsed {
                REGEX_PARSE_CACHE.with(|c| {
                    c.borrow_mut()
                        .insert(pattern.to_string(), std::sync::Arc::clone(p));
                });
            }
            return parsed;
        }
        self.parse_regex_uncached(pattern, RegexParseMode::Match)
            .map(std::sync::Arc::new)
    }
}
