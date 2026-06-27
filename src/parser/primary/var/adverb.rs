/// Adverb-suffix and bracket-value parsing for variable names.
///
/// Handles `:adverb<value>` suffixes on variable names and the various bracket
/// forms (`<...>`, `«...»`, `[...]`, `(...)`) used to spell adverb values.
use super::ident::parse_ident_with_hyphens;

/// Consume any `:adverb<value>` suffixes that follow a variable name.
/// Stops before postfix adverbs (`delete`, `exists`, `v`, `kv`, `k`, `p`).
pub(crate) fn parse_var_name_adverb_suffixes(mut rest: &str, mut name: String) -> (&str, String) {
    while rest.starts_with(':') && !rest.starts_with("::") {
        let after_colon = &rest[1..];
        if let Ok((r2, suffix)) = parse_ident_with_hyphens(after_colon) {
            // Keep postfix adverb names available to postfix parsing.
            // This avoids treating `$a:delete` as a variable named `a:delete`.
            if matches!(suffix, "delete" | "exists" | "v" | "kv" | "k" | "p") {
                break;
            }
            name.push(':');
            name.push_str(suffix);
            rest = r2;
            // Parse adverb value: <...>, «...», [...], (...)
            if let Some(canonical) = parse_adverb_value(rest) {
                name.push_str(&canonical.0);
                rest = canonical.1;
            }
        } else {
            break;
        }
    }
    (rest, name)
}

/// Public wrapper for `parse_adverb_value` used by `stmt/idents.rs`.
pub(crate) fn parse_adverb_value_pub(input: &str) -> Option<(String, &str)> {
    parse_adverb_value(input)
}

/// Parse adverb value brackets and canonicalize to `<word1 word2>` form.
/// Returns (canonical_string, remaining_input) or None if no adverb value follows.
fn parse_adverb_value(input: &str) -> Option<(String, &str)> {
    let first_char = input.chars().next()?;
    match first_char {
        '<' => {
            // Angle brackets: <a b>
            let close = input.find('>')?;
            let content = &input[1..close];
            let rest = &input[close + 1..];
            Some((format!("<{}>", content), rest))
        }
        '\u{00AB}' => {
            // French quotes: « »
            let close_char = '\u{00BB}';
            let close = input[first_char.len_utf8()..].find(close_char)?;
            let content = &input[first_char.len_utf8()..first_char.len_utf8() + close];
            let rest = &input[first_char.len_utf8() + close + close_char.len_utf8()..];
            Some((format!("<{}>", content), rest))
        }
        '[' => {
            // Square brackets: ['a','b']
            let close = find_matching_bracket(input, '[', ']')?;
            let content = &input[1..close];
            let rest = &input[close + 1..];
            let words = parse_comma_separated_values(content);
            Some((format!("<{}>", words.join(" ")), rest))
        }
        '(' => {
            // Parentheses: ('a','b')
            let close = find_matching_bracket(input, '(', ')')?;
            let content = &input[1..close];
            let rest = &input[close + 1..];
            let words = parse_comma_separated_values(content);
            Some((format!("<{}>", words.join(" ")), rest))
        }
        _ => None,
    }
}

/// Find the position of a matching closing bracket, handling nesting.
fn find_matching_bracket(input: &str, open: char, close: char) -> Option<usize> {
    let mut depth = 0;
    let mut in_string = false;
    let mut string_char = ' ';
    for (i, c) in input.char_indices() {
        if in_string {
            if c == string_char {
                in_string = false;
            }
            continue;
        }
        if c == '\'' || c == '"' {
            in_string = true;
            string_char = c;
        } else if c == open {
            depth += 1;
        } else if c == close {
            depth -= 1;
            if depth == 0 {
                return Some(i);
            }
        }
    }
    None
}

/// Parse comma-separated values, stripping quotes.
/// `'a','b'` → vec!["a", "b"]
fn parse_comma_separated_values(input: &str) -> Vec<String> {
    input
        .split(',')
        .map(|s| {
            let s = s.trim();
            // Strip surrounding quotes
            if (s.starts_with('\'') && s.ends_with('\''))
                || (s.starts_with('"') && s.ends_with('"'))
            {
                s[1..s.len() - 1].to_string()
            } else {
                s.to_string()
            }
        })
        .collect()
}
