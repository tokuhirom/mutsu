/// Adverb-suffix and bracket-value parsing for variable names.
///
/// Handles `:adverb<value>` suffixes on variable names and the various bracket
/// forms (`<...>`, `«...»`, `[...]`, `(...)`) used to spell adverb values.
use super::ident::parse_ident_with_hyphens;
use crate::adverb_name;

/// Consume any `:adverb<value>` suffixes that follow a variable name.
/// Stops before postfix adverbs (`delete`, `exists`, `v`, `kv`, `k`, `p`).
pub(crate) fn parse_var_name_adverb_suffixes(mut rest: &str, mut name: String) -> (&str, String) {
    while rest.starts_with(':') && !rest.starts_with("::") {
        let after_colon = &rest[1..];
        // Key-less colon pair: `$take-me:<home>`, `:«home»`, `:['home']`.
        if let Some((canonical, r2)) = parse_anon_adverb_value(after_colon) {
            name.push(':');
            name.push_str(&canonical);
            rest = r2;
            continue;
        }
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

/// Parse the value of a *key-less* colon pair in an extended identifier, i.e.
/// the `<home>` of `$take-me:<home>` (S02, `Language/syntax.rakudoc`). The value
/// alone spells the pair, so `$take-me:<home>`, `$take-me:«home»` and
/// `$take-me:['home']` all name the same variable.
///
/// Unlike the keyed form this deliberately does **not** accept `(...)`: `:(...)`
/// is a signature literal, and raku rejects `my $t:("home")` outright with
/// "You can't adverb $t".
pub(crate) fn parse_anon_adverb_value(input: &str) -> Option<(String, &str)> {
    match input.chars().next()? {
        '<' | '\u{00AB}' | '[' => parse_adverb_value(input),
        _ => None,
    }
}

/// Parse adverb value brackets and canonicalize to `<word1 word2>` form.
///
/// A spelling whose value is only known once the compile-time `constant`
/// environment is available (`«$c»`, `(1+1)`) cannot be canonicalized here --
/// the parser is a pure `&str -> AST` pass and its results are memoized. Those
/// keep their source spelling, wrapped in [`adverb_name::INTERP_MARK`], and
/// `compiler::adverb_interp` finishes the job at BEGIN time.
///
/// Returns (canonical_string, remaining_input) or None if no adverb value follows.
fn parse_adverb_value(input: &str) -> Option<(String, &str)> {
    let first_char = input.chars().next()?;
    match first_char {
        '<' => {
            // Double angle brackets: <<a b>> (same name as <a b>, per S02 —
            // "the bracketing characters used do not count as part of it").
            if let Some(inner) = input.strip_prefix("<<")
                && let Some(close) = inner.find(">>")
            {
                return Some((canonical(&inner[..close]), &inner[close + 2..]));
            }
            // Angle brackets: <a b>. Single-quote-like: never interpolates.
            let close = input.find('>')?;
            let content = &input[1..close];
            let rest = &input[close + 1..];
            Some((canonical(content), rest))
        }
        '\u{00AB}' => {
            // French quotes: « » — `qqw`, so a sigil in there interpolates.
            let close_char = '\u{00BB}';
            let close = input[first_char.len_utf8()..].find(close_char)?;
            let content = &input[first_char.len_utf8()..first_char.len_utf8() + close];
            let rest = &input[first_char.len_utf8() + close + close_char.len_utf8()..];
            if adverb_name::guillemet_interpolates(content) {
                return Some((adverb_name::mark_unevaluated('\u{00AB}', content), rest));
            }
            Some((canonical(content), rest))
        }
        '[' | '(' => {
            // Square brackets / parentheses: ['a','b'], (1+1). The content is
            // an expression list; only the all-quoted-words spelling can be
            // canonicalized without evaluating anything.
            let close_delim = if first_char == '[' { ']' } else { ')' };
            let close = find_matching_bracket(input, first_char, close_delim)?;
            let content = &input[1..close];
            let rest = &input[close + 1..];
            if content.trim().is_empty() || adverb_name::is_all_quoted_items(content) {
                return Some((
                    format!("<{}>", adverb_name::literal_paren_words(content)),
                    rest,
                ));
            }
            Some((adverb_name::mark_unevaluated('(', content), rest))
        }
        _ => None,
    }
}

/// Wrap already-literal adverb-value words in the canonical `<...>` spelling.
fn canonical(content: &str) -> String {
    format!("<{}>", adverb_name::normalize_words(content))
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
