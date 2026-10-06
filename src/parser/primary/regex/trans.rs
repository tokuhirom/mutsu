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

/// The adverbs and the opening delimiter of a `tr` / `TR`.
pub(super) struct TransHead<'a> {
    /// What follows the opening delimiter.
    pub(super) rest: &'a str,
    pub(super) open: char,
    pub(super) close: char,
    pub(super) is_paired: bool,
    pub(super) delete: bool,
    pub(super) complement: bool,
    pub(super) squash: bool,
    /// The adverbs as written, in order.
    pub(super) adverbs: Vec<String>,
}

/// Parse tr/TR adverbs and the opening delimiter.
pub(super) fn parse_trans_adverbs(input: &str) -> Option<TransHead<'_>> {
    let mut rest = input;
    let mut written = Vec::new();
    let mut delete = false;
    let mut complement = false;
    let mut squash = false;

    loop {
        // Raku allows whitespace between `tr`/`TR` and its adverbs
        // (`tr :d/b//`), and between consecutive adverbs.
        let after_colon = match rest.strip_prefix(':') {
            Some(r) => r,
            None => match super::adverbs::skip_ws_before_adverb(rest) {
                Some(after_ws) => {
                    rest = after_ws;
                    &rest[1..]
                }
                None => break,
            },
        };
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
        written.push(name.to_string());
        rest = &after_colon[name_len..];
    }

    let open_ch = rest.chars().next()?;
    // `(` directly after `tr`/`TR` (or its adverbs) is call syntax, never a
    // delimiter — Raku requires whitespace before a paren delimiter.
    if open_ch.is_alphanumeric()
        || open_ch == '_'
        || open_ch.is_whitespace()
        || open_ch == '('
        || crate::parser::helpers::delim_is_identifier_continuation(rest)
    {
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
    Some(TransHead {
        rest: after_open,
        open: open_ch,
        close: close_ch,
        is_paired,
        delete,
        complement,
        squash,
        adverbs: written,
    })
}
