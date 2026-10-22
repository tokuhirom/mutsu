use super::*;

/// Process an escape sequence starting at `rest` (which begins with `\`).
/// `extra_escapes` lists additional simple single-char escapes (e.g., `'"'`, `'{'`).
/// Returns `Ok(Some((remaining_input, true)))` if a continuation-style escape was handled
/// (caller should `continue`), or `Ok(Some((remaining_input, false)))` for simple escapes
/// (caller should advance past `\c`), or `Ok(None)` if the escape char is unknown (caller
/// pushes `\` + char and advances).
/// Returns `Err(message)` for malformed escape sequences that should be compile-time errors.
pub(crate) fn process_escape_sequence<'a>(
    rest: &'a str,
    current: &mut String,
    extra_escapes: &[char],
) -> Result<Option<(&'a str, bool)>, String> {
    let Some(after_backslash) = rest.strip_prefix('\\') else {
        return Ok(None);
    };
    let Some(c) = after_backslash.chars().next() else {
        return Ok(None);
    };
    let after_escape = &after_backslash[c.len_utf8()..];
    match c {
        'n' => current.push('\n'),
        't' => current.push('\t'),
        'r' => current.push('\r'),
        'b' => current.push('\u{0008}'),
        'a' => current.push('\u{0007}'),
        'e' => current.push('\u{001B}'),
        'f' => current.push('\u{000C}'),
        '0' => current.push('\0'),
        '\\' => current.push('\\'),
        '$' => current.push('$'),
        '@' => current.push('@'),
        '%' => current.push('%'),
        '&' => current.push('&'),
        '"' => current.push('"'),
        '\'' => current.push('\''),
        ' ' => current.push(' '),
        'x' | 'o' => {
            let (remaining, text) = super::escape_codepoint::numeric_escape(after_escape, c)?;
            current.push_str(&text);
            if c == 'x' {
                // Preserve qq's normalization of combining codepoints.
                use unicode_normalization::UnicodeNormalization;
                let normalized: String = current.nfc().collect();
                current.clear();
                current.push_str(&normalized);
            }
            return Ok(Some((remaining, true)));
        }
        'c' => {
            let r = after_escape;
            if let Some(r_inner) = r.strip_prefix('[') {
                if let Some((s, after)) = parse_backslash_c_bracket(r_inner)? {
                    current.push_str(&s);
                    // NFC-normalize: combining characters from \c[...] should
                    // compose with the preceding character (e.g. "a\c[COMBINING DIAERESIS]" → "ä")
                    use unicode_normalization::UnicodeNormalization;
                    let normalized: String = current.nfc().collect();
                    current.clear();
                    current.push_str(&normalized);
                    return Ok(Some((after, true)));
                }
                // Distinguish missing ] from unknown name
                if r_inner.find(']').is_none() {
                    return Err("Missing closing bracket for \\c[".to_string());
                }
                // Unknown character name with valid brackets — skip content
                let end = r_inner.find(']').unwrap();
                return Ok(Some((&r_inner[end + 1..], true)));
            }
            // \c followed by decimal digits → character by codepoint (e.g. \c10 = LF)
            let digits: String = r.chars().take_while(|ch| ch.is_ascii_digit()).collect();
            if !digits.is_empty() {
                let len = digits.len();
                if let Ok(n) = digits.parse::<u32>() {
                    current.push(super::escape_codepoint::scalar(i64::from(n))?);
                }
                return Ok(Some((&r[len..], true)));
            }
            // \c followed by a single character → control character
            // \c@ = NUL (0), \cA = SOH (1), ..., \cZ = SUB (26)
            // \c? = DEL (127), \cI = TAB (9)
            if let Some(ch) = r.chars().next() {
                let ctrl = match ch {
                    '@' => Some(0u32),
                    'A'..='Z' => Some(ch as u32 - 'A' as u32 + 1),
                    'a'..='z' => Some(ch as u32 - 'a' as u32 + 1),
                    '?' => Some(127),
                    _ => None,
                };
                if let Some(code) = ctrl {
                    if let Some(ctrl_ch) = char::from_u32(code) {
                        current.push(ctrl_ch);
                    }
                    return Ok(Some((&r[ch.len_utf8()..], true)));
                }
                // \c followed by space is an error
                if ch == ' ' {
                    return Err("Unrecognized backslash sequence: '\\c'".to_string());
                }
            }
            return Ok(Some((r, true)));
        }
        // \1 through \9 followed by digits is no longer valid in Raku
        '1'..='9' => {
            return Err(format!("Unrecognized backslash sequence: '\\{}'", c));
        }
        _ => {
            if extra_escapes.contains(&c) {
                current.push(c);
            } else if !c.is_alphanumeric() {
                // Non-alphanumeric chars after \ produce themselves in Raku
                current.push(c);
            } else {
                return Ok(None);
            }
        }
    }
    Ok(Some((after_escape, false)))
}
