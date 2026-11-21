//! How a character in a regex character class is written when it is not
//! plain: `\x[22]`, `\x41`, `\o[101]`, `\c[65]` (ADR-10723 Stage 2, #15780),
//! for the origin walk of a regex body (`regex_origins`).

/// The length of the escape `text` starts with that spells `ch`: a
/// hexadecimal (`\x41`, `\x[41]`), octal (`\o101`, `\o[101]`) or decimal
/// (`\c[65]`) codepoint. `None` when `text` does not spell `ch` so.
// Cost: O(n), n = length of the escape.
pub(super) fn escaped_len(text: &str, ch: char) -> Option<usize> {
    let rest = text.strip_prefix('\\')?;
    let (radix, rest) = match rest.chars().next()? {
        'x' => (16, &rest[1..]),
        'o' => (8, &rest[1..]),
        'c' => (10, &rest[1..]),
        _ => return None,
    };
    let (digits, len) = match rest.strip_prefix('[') {
        Some(inner) => {
            let close = inner.find(']')?;
            (&inner[..close], close + 4)
        }
        None if radix != 10 => {
            let end = rest
                .find(|c: char| !c.is_digit(radix))
                .unwrap_or(rest.len());
            (&rest[..end], 2 + end)
        }
        None => return None,
    };
    let code = u32::from_str_radix(digits.trim(), radix).ok()?;
    (char::from_u32(code) == Some(ch)).then_some(len)
}

#[cfg(test)]
mod tests {
    use super::escaped_len;

    #[test]
    fn escapes() {
        assert_eq!(escaped_len("\\x[22]]", '"'), Some(6));
        assert_eq!(escaped_len("\\x41..", 'A'), Some(4));
        assert_eq!(escaped_len("\\o[101]", 'A'), Some(7));
        assert_eq!(escaped_len("\\c[65]", 'A'), Some(6));
        assert_eq!(escaped_len("\\x41", 'B'), None);
        assert_eq!(escaped_len("\\n", '\n'), None);
    }
}
