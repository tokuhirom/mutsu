//! tr/// escape helpers and adverb parsing.

/// Process escape sequences in a tr/// from/to string.
/// Handles \n, \t, \r, \x.., \o.., \\, etc.
pub(super) fn process_trans_escapes(raw: &str) -> String {
    let mut result = String::new();
    let mut rest = raw;
    while !rest.is_empty() {
        if rest.starts_with('\\') && rest.len() >= 2 {
            match crate::parser::primary::string::process_escape_sequence(rest, &mut result, &[]) {
                Ok(Some((remaining, _))) => {
                    rest = remaining;
                }
                Ok(None) | Err(_) => {
                    // Unknown escape: keep as-is
                    result.push('\\');
                    rest = &rest[1..];
                }
            }
        } else {
            let ch = rest.chars().next().unwrap();
            result.push(ch);
            rest = &rest[ch.len_utf8()..];
        }
    }
    result
}

/// Parse tr/TR adverbs and the opening delimiter.
/// Returns (remaining_after_open, delimiter_char, close_delimiter_char, is_paired, delete, complement, squash).
pub(super) fn parse_trans_adverbs(
    input: &str,
) -> Option<(&str, char, char, bool, bool, bool, bool)> {
    let mut rest = input;
    let mut delete = false;
    let mut complement = false;
    let mut squash = false;

    while let Some(after_colon) = rest.strip_prefix(':') {
        let name_len = after_colon
            .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '-'))
            .unwrap_or(after_colon.len());
        if name_len == 0 {
            return None;
        }
        let name = &after_colon[..name_len];
        match name {
            "d" | "delete" => delete = true,
            "c" | "complement" => complement = true,
            "s" | "squash" => squash = true,
            _ => {}
        }
        rest = &after_colon[name_len..];
    }

    let open_ch = rest.chars().next()?;
    if open_ch.is_alphanumeric() || open_ch == '_' || open_ch.is_whitespace() {
        return None;
    }
    let (close_ch, is_paired) = match open_ch {
        '{' => ('}', true),
        '[' => (']', true),
        '(' => (')', true),
        '<' => ('>', true),
        other => (other, false),
    };
    let after_open = &rest[open_ch.len_utf8()..];
    Some((
        after_open, open_ch, close_ch, is_paired, delete, complement, squash,
    ))
}
