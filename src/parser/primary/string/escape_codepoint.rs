//! Numeric codepoint escapes shared by ordinary qq and regex quote decoding.

fn invalid(kind: char) -> String {
    format!("X::Backslash::UnrecognizedSequence: Unrecognized backslash sequence: '\\{kind}'")
}

/// Decode one ASCII numeral with single internal underscore separators.
// Cost: O(n), n = numeral length; checked arithmetic bounds the codepoint.
fn codepoint(digits: &str, radix: u32, kind: char) -> Result<char, String> {
    let mut value = 0u32;
    let mut previous_digit = false;
    for ch in digits.chars() {
        if ch == '_' {
            if !previous_digit {
                return Err(invalid(kind));
            }
            previous_digit = false;
            continue;
        }
        let digit = ch.to_digit(radix).ok_or_else(|| invalid(kind))?;
        value = value
            .checked_mul(radix)
            .and_then(|value| value.checked_add(digit))
            .ok_or_else(|| invalid(kind))?;
        previous_digit = true;
    }
    if !previous_digit {
        return Err(invalid(kind));
    }
    scalar(i64::from(value))
}

/// The scalar value of a decoded escape: a surrogate is `X::InvalidCodepoint`
/// (#14416), a value past Unicode is out of bounds like `chr`'s.
// Cost: O(1).
pub(super) fn scalar(value: i64) -> Result<char, String> {
    use crate::builtins::str_prim::codepoint;
    if codepoint::is_surrogate(value) {
        return Err(format!(
            "X::InvalidCodepoint: {}",
            codepoint::surrogate_message(value)
        ));
    }
    u32::try_from(value)
        .ok()
        .and_then(char::from_u32)
        .ok_or_else(|| format!("chr codepoint {value} (0x{value:X}) is out of bounds"))
}

/// Parse the tail of `\x` or `\o`, rejecting an entire malformed escape
/// before its caller appends any of its codepoints.
// Cost: O(n), n = source consumed by the escape.
pub(super) fn numeric_escape(rest: &str, kind: char) -> Result<(&str, String), String> {
    let radix = if kind == 'x' { 16 } else { 8 };
    let mut text = String::new();
    if let Some(inner) = rest.strip_prefix('[') {
        let end = inner
            .find(']')
            .ok_or_else(|| format!("Missing closing bracket for \\{kind}["))?;
        for digits in inner[..end].split(',') {
            text.push(codepoint(digits.trim(), radix, kind)?);
        }
        Ok((&inner[end + 1..], text))
    } else {
        let mut end = 0;
        let mut chars = rest.char_indices().peekable();
        while let Some((index, ch)) = chars.next() {
            let separator =
                ch == '_' && end > 0 && chars.peek().is_some_and(|(_, next)| next.is_digit(radix));
            if !ch.is_digit(radix) && !separator {
                break;
            }
            end = index + ch.len_utf8();
        }
        text.push(codepoint(&rest[..end], radix, kind)?);
        Ok((&rest[end..], text))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numeric_escape_validates_complete_lists() {
        assert_eq!(
            numeric_escape("[0_41,42]tail", 'x').unwrap(),
            ("tail", "AB".to_string())
        );
        assert_eq!(
            numeric_escape("[1_01,102]tail", 'o').unwrap(),
            ("tail", "AB".to_string())
        );
        assert_eq!(
            numeric_escape("41_42tail", 'x').unwrap(),
            ("tail", "\u{4142}".to_string())
        );
        assert_eq!(
            numeric_escape("41__42", 'x').unwrap(),
            ("__42", "A".to_string())
        );
        for source in [
            "[]",
            "[41,QQ]",
            "[41,,42]",
            "[41_]",
            "[_41]",
            "[4__1]",
            "[110000]",
            "[D800]",
            "[fffffffff]",
        ] {
            assert!(numeric_escape(source, 'x').is_err(), "{source}");
        }
        assert!(numeric_escape("[101,9]", 'o').is_err());
    }
}
