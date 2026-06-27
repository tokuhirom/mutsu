/// The QuantHash family of a hyper-op operand, used to round-trip Set/Bag/Mix
/// values through the plain-Hash hyper logic and back to their original type.
#[derive(Clone, Copy)]
pub(super) enum QuantKind {
    Set,
    Bag,
    Mix,
}

/// Detect the case pattern of a word.
/// Returns one of: "uc" (all upper), "lc" (all lower), "ucfirst" (first upper, rest lower),
/// "lcfirst" (first lower, rest upper).
fn detect_word_case(word: &str) -> &'static str {
    let chars: Vec<char> = word.chars().filter(|c| c.is_alphabetic()).collect();
    if chars.is_empty() {
        return "lc";
    }
    let all_upper = chars.iter().all(|c| c.is_uppercase());
    let all_lower = chars.iter().all(|c| c.is_lowercase());
    if all_upper {
        return "uc";
    }
    if all_lower {
        return "lc";
    }
    if chars[0].is_uppercase() && chars[1..].iter().all(|c| c.is_lowercase()) {
        return "ucfirst";
    }
    if chars[0].is_lowercase() && chars[1..].iter().all(|c| c.is_uppercase()) {
        return "lcfirst";
    }
    // Mixed case - fall back to char-by-char
    "mixed"
}

/// Apply a case function to a word.
fn apply_case_function(word: &str, case_fn: &str) -> String {
    match case_fn {
        "uc" => word.to_uppercase(),
        "lc" => word.to_lowercase(),
        "ucfirst" => {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => {
                    let mut s = first.to_uppercase().to_string();
                    for ch in chars {
                        for c in ch.to_lowercase() {
                            s.push(c);
                        }
                    }
                    s
                }
            }
        }
        "lcfirst" => {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => {
                    let mut s = first.to_lowercase().to_string();
                    for ch in chars {
                        for c in ch.to_uppercase() {
                            s.push(c);
                        }
                    }
                    s
                }
            }
        }
        _ => word.to_string(),
    }
}

/// Apply the `:samecase`/`:samemark`/`:samespace` substitution transforms to a
/// computed replacement string against the matched text. Shared by the `s///`
/// operator and the `.subst` method so both behave identically. `sigspace`
/// selects per-word vs whole-string samecase (as `s///` does).
pub(crate) fn apply_subst_case_transforms(
    replacement: &str,
    matched: &str,
    samecase: bool,
    samemark: bool,
    sigspace: bool,
    samespace: bool,
) -> String {
    // In Raku, `:sigspace` (and therefore `:samespace`, which implies `:sigspace`,
    // and the `ss///` operator) also implies `:samemark`: the matched text's
    // combining marks are copied onto the replacement. `:samecase` is NOT implied
    // by sigspace, so only fold samemark in here.
    let samemark = samemark || sigspace;
    // `:samecase` and `:samemark` are independent transforms and can be combined
    // (e.g. `s:ii:mm///`). Apply case first, then transfer marks onto the
    // case-adjusted text. Using `else if` here would silently drop samemark
    // whenever samecase was also requested.
    let mut repl = replacement.to_string();
    if samecase {
        repl = if sigspace {
            samecase_per_word(&repl, matched)
        } else {
            crate::builtins::samecase_string(&repl, matched)
        };
    }
    if samemark {
        repl = if matched.contains(char::is_whitespace) && repl.contains(char::is_whitespace) {
            samemark_per_word(&repl, matched)
        } else {
            crate::builtins::samemark_string(&repl, matched)
        };
    }
    if samespace {
        repl = samespace_replace(&repl, matched);
    }
    repl
}

/// Apply samecase on a per-word basis: detect the case pattern of each word in the
/// matched text, then apply that pattern to each corresponding word in the replacement.
fn samecase_per_word(replacement: &str, matched: &str) -> String {
    let matched_words: Vec<&str> = matched.split_whitespace().collect();
    if matched_words.is_empty() {
        return replacement.to_string();
    }

    // Detect case function for each matched word
    let case_fns: Vec<&str> = matched_words.iter().map(|w| detect_word_case(w)).collect();

    // Check if all words have the same case function (non-wordcase mode)
    // Only use the "entire string" shortcut for uc/lc where it makes sense.
    // For ucfirst/lcfirst, we still need per-word application.
    let all_same = case_fns.windows(2).all(|w| w[0] == w[1]);
    if all_same && (case_fns[0] == "uc" || case_fns[0] == "lc") {
        return apply_case_function(replacement, case_fns[0]);
    }

    // Word-case mode: apply per-word case functions
    let mut result = String::new();
    let mut word_idx = 0;
    let mut chars = replacement.chars().peekable();
    while chars.peek().is_some() {
        // Collect leading whitespace
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                result.push(ch);
                chars.next();
            } else {
                break;
            }
        }
        // Collect word
        let mut word = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                break;
            }
            word.push(ch);
            chars.next();
        }
        if !word.is_empty() {
            let case_fn = if word_idx < case_fns.len() {
                case_fns[word_idx]
            } else {
                case_fns.last().unwrap()
            };
            result.push_str(&apply_case_function(&word, case_fn));
            word_idx += 1;
        }
    }
    result
}

/// Apply samemark on a per-word basis: split both source and target by whitespace,
/// apply samemark to each word pair, then reassemble with the replacement's whitespace.
fn samemark_per_word(target: &str, source: &str) -> String {
    let src_words: Vec<&str> = source.split_whitespace().collect();
    if src_words.is_empty() {
        return target.to_string();
    }

    // Split target into words and whitespace segments
    let mut result = String::new();
    let mut word_idx = 0;
    let mut chars = target.chars().peekable();
    while chars.peek().is_some() {
        // Collect leading whitespace
        let mut ws = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                ws.push(ch);
                chars.next();
            } else {
                break;
            }
        }
        result.push_str(&ws);
        // Collect word
        let mut word = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() {
                break;
            }
            word.push(ch);
            chars.next();
        }
        if !word.is_empty() {
            let src_word = if word_idx < src_words.len() {
                src_words[word_idx]
            } else {
                src_words.last().unwrap()
            };
            result.push_str(&crate::builtins::samemark_string(&word, src_word));
            word_idx += 1;
        }
    }
    result
}

/// Apply samespace: replace whitespace runs in the replacement with corresponding
/// whitespace runs from the matched text.
fn samespace_replace(replacement: &str, matched: &str) -> String {
    // Split matched text into whitespace runs
    let mut ws_runs: Vec<&str> = Vec::new();
    let mut i = 0;
    let bytes = matched.as_bytes();
    while i < bytes.len() {
        // Skip non-whitespace
        while i < bytes.len() && !matched[i..].starts_with(|c: char| c.is_whitespace()) {
            i += matched[i..].chars().next().map_or(1, |c| c.len_utf8());
        }
        if i >= bytes.len() {
            break;
        }
        let start = i;
        while i < bytes.len() && matched[i..].starts_with(|c: char| c.is_whitespace()) {
            i += matched[i..].chars().next().map_or(1, |c| c.len_utf8());
        }
        ws_runs.push(&matched[start..i]);
    }

    // Now replace whitespace runs in the replacement with runs from the matched text
    let mut result = String::new();
    let mut ws_idx = 0;
    let mut j = 0;
    let repl_bytes = replacement.as_bytes();
    while j < repl_bytes.len() {
        let ch = replacement[j..].chars().next().unwrap();
        if ch.is_whitespace() {
            // Skip the whitespace run in the replacement
            while j < repl_bytes.len() && replacement[j..].starts_with(|c: char| c.is_whitespace())
            {
                j += replacement[j..].chars().next().map_or(1, |c| c.len_utf8());
            }
            // Insert the corresponding whitespace run from the matched text
            if ws_idx < ws_runs.len() {
                result.push_str(ws_runs[ws_idx]);
                ws_idx += 1;
            } else {
                result.push(' ');
            }
        } else {
            result.push(ch);
            j += ch.len_utf8();
        }
    }
    result
}

/// Expand positional capture references ($0, $1, ...) in a substitution
/// replacement string.  Called after the regex match so capture values are known.
pub(crate) fn expand_capture_refs(template: &str, captures: &[String]) -> String {
    let mut out = String::new();
    let bytes = template.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'$' && i + 1 < bytes.len() && bytes[i + 1].is_ascii_digit() {
            // Parse the digit(s) following $
            let mut j = i + 1;
            while j < bytes.len() && bytes[j].is_ascii_digit() {
                j += 1;
            }
            if let Ok(idx) = template[i + 1..j].parse::<usize>()
                && let Some(cap) = captures.get(idx)
            {
                out.push_str(cap);
            }
            i = j;
        } else {
            out.push(template[i..].chars().next().unwrap());
            i += template[i..].chars().next().unwrap().len_utf8();
        }
    }
    out
}

/// Extract a variable name (identifier chars: alpha, digit, _, -, ::)
pub(super) fn take_var_name(input: &str) -> Option<usize> {
    let mut chars = input.char_indices();
    let (_, first) = chars.next()?;
    if !first.is_ascii_alphabetic()
        && first != '_'
        && first != '*'
        && first != '?'
        && first != '!'
        && first != '^'
    {
        return None;
    }
    let mut end = first.len_utf8();
    for (idx, ch) in chars {
        if ch.is_ascii_alphanumeric() || ch == '_' || ch == '-' || ch == ':' {
            end = idx + ch.len_utf8();
        } else {
            break;
        }
    }
    Some(end)
}

pub(super) fn normalize_subst_replacement(template: &str) -> String {
    let mut out = String::new();
    let mut chars = template.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }
        let Some(next) = chars.peek().copied() else {
            out.push('\\');
            continue;
        };
        match next {
            '\\' => {
                out.push('\\');
                chars.next();
            }
            '&' => {
                out.push('&');
                chars.next();
            }
            'n' => {
                out.push('\n');
                chars.next();
            }
            'r' => {
                out.push('\r');
                chars.next();
            }
            't' => {
                out.push('\t');
                chars.next();
            }
            '0' => {
                out.push('\0');
                chars.next();
            }
            'a' => {
                out.push('\x07'); // BEL
                chars.next();
            }
            'b' => {
                out.push('\x08'); // BS
                chars.next();
            }
            'e' => {
                out.push('\x1B'); // ESC
                chars.next();
            }
            'f' => {
                out.push('\x0C'); // FF
                chars.next();
            }
            'x' => {
                chars.next(); // consume 'x'
                if chars.peek() == Some(&'[') {
                    chars.next(); // consume '['
                    let mut hex = String::new();
                    while let Some(&c) = chars.peek() {
                        if c == ']' {
                            chars.next();
                            break;
                        }
                        hex.push(c);
                        chars.next();
                    }
                    // Support space-separated multi-codepoint: \x[48 65 6C]
                    for part in hex.split_whitespace() {
                        if let Ok(cp) = u32::from_str_radix(part, 16)
                            && let Some(c) = char::from_u32(cp)
                        {
                            out.push(c);
                        }
                    }
                } else {
                    let mut hex = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_ascii_hexdigit() && hex.len() < 2 {
                            hex.push(c);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    if let Ok(cp) = u32::from_str_radix(&hex, 16)
                        && let Some(c) = char::from_u32(cp)
                    {
                        out.push(c);
                    }
                }
            }
            _ => out.push('\\'),
        }
    }
    out
}
