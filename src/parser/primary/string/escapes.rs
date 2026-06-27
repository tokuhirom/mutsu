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
        'x' => {
            let r = after_escape;
            if r.starts_with('[') {
                if let Some(end) = r.find(']') {
                    let content = &r[1..end];
                    // Handle comma-separated hex values: \x[0041,0300]
                    for part in content.split(',') {
                        let hex = part.trim();
                        if let Ok(n) = u32::from_str_radix(hex, 16)
                            && let Some(ch) = char::from_u32(n)
                        {
                            current.push(ch);
                        }
                    }
                    // NFC-normalize: combining characters from \x[...] should
                    // compose with the preceding character
                    use unicode_normalization::UnicodeNormalization;
                    let normalized: String = current.nfc().collect();
                    current.clear();
                    current.push_str(&normalized);
                    return Ok(Some((&r[end + 1..], true)));
                }
                // \x[ without closing ] is a compile error
                return Err("Missing closing bracket for \\x[".to_string());
            } else {
                let hex_chars: String = r.chars().take_while(|ch| ch.is_ascii_hexdigit()).collect();
                let len = hex_chars.len();
                if len == 0 {
                    // \x not followed by hex digits or [ is unrecognized
                    return Err(
                        "X::Backslash::UnrecognizedSequence: Unrecognized backslash sequence: '\\x'"
                            .to_string(),
                    );
                }
                if let Ok(n) = u32::from_str_radix(&hex_chars, 16)
                    && let Some(ch) = char::from_u32(n)
                {
                    current.push(ch);
                }
                // NFC-normalize: \xHHHH escapes may introduce characters that
                // should compose with surrounding text
                use unicode_normalization::UnicodeNormalization;
                let normalized: String = current.nfc().collect();
                current.clear();
                current.push_str(&normalized);
                return Ok(Some((&r[len..], true)));
            }
        }
        'o' => {
            let r = after_escape;
            if r.starts_with('[') {
                if let Some(end) = r.find(']') {
                    let content = &r[1..end];
                    // Handle comma-separated octal values: \o[101,102,103]
                    for part in content.split(',') {
                        let oct = part.trim();
                        if let Ok(n) = u32::from_str_radix(oct, 8)
                            && let Some(ch) = char::from_u32(n)
                        {
                            current.push(ch);
                        }
                    }
                    return Ok(Some((&r[end + 1..], true)));
                }
                // \o[ without closing ] is a compile error
                return Err("Missing closing bracket for \\o[".to_string());
            } else {
                let oct_chars: String =
                    r.chars().take_while(|ch| matches!(ch, '0'..='7')).collect();
                let len = oct_chars.len();
                if len == 0 {
                    // \o not followed by octal digits or [ is unrecognized
                    return Err(
                        "X::Backslash::UnrecognizedSequence: Unrecognized backslash sequence: '\\o'"
                            .to_string(),
                    );
                }
                if let Ok(n) = u32::from_str_radix(&oct_chars, 8)
                    && let Some(ch) = char::from_u32(n)
                {
                    current.push(ch);
                }
                return Ok(Some((&r[len..], true)));
            }
        }
        'c' => {
            let r = after_escape;
            if let Some(r_inner) = r.strip_prefix('[') {
                if let Some((s, after)) = parse_backslash_c_bracket(r_inner) {
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
                if let Ok(n) = digits.parse::<u32>()
                    && let Some(ch) = char::from_u32(n)
                {
                    current.push(ch);
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
