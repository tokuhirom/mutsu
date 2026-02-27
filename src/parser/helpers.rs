use super::parse_result::{PError, PResult, take_while_opt, take_while1};
use unicode_normalization::UnicodeNormalization;

/// Skip whitespace, line comments (`#` to end of line), and Pod blocks.
pub(super) fn ws(input: &str) -> PResult<'_, ()> {
    let mut rest = input;
    let mut at_line_start = true; // conservatively true for start of input
    loop {
        // Try whitespace
        let (r, matched) = take_while_opt(rest, |c| c.is_whitespace());
        if !matched.is_empty() {
            at_line_start = matched.contains('\n');
            rest = r;
            continue;
        }
        // Try embedded comment #`[...] or line comment
        if r.starts_with('#') {
            if let Some(after) = skip_embedded_comment(r) {
                rest = after;
                at_line_start = false;
                continue;
            }
            let end = r.find('\n').unwrap_or(r.len());
            rest = &r[end..];
            at_line_start = true;
            continue;
        }
        // Pod blocks only appear at the start of a line
        if at_line_start && let Ok((r, _)) = pod_block(r) {
            rest = r;
            at_line_start = true;
            continue;
        }
        break;
    }
    Ok((rest, ()))
}

/// Parse and skip a Pod block.
/// Handles `=begin ... =end`, `=for ...`, `=head ...`, `=item ...`, `=comment ...`, etc.
fn pod_block(input: &str) -> PResult<'_, &str> {
    // Must start with `=`
    let rest = input
        .strip_prefix('=')
        .ok_or_else(|| PError::expected("="))?;

    // Pod keywords must start with a letter
    if rest.is_empty() || !rest.as_bytes()[0].is_ascii_alphabetic() {
        return Err(PError::expected("pod directive"));
    }

    // Read the directive keyword
    let (rest, keyword) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '-')?;
    if let Some(ch) = rest.chars().next() {
        if !ch.is_whitespace() {
            return Err(PError::expected("pod directive"));
        }
    } else {
        return Err(PError::expected("pod directive"));
    }

    // In Raku, any =word at the start of a line is a Pod directive.
    // We handle "begin" specially (paired with =end), all others skip to end of paragraph.

    if keyword == "begin" {
        // =begin IDENTIFIER ... =end IDENTIFIER
        // Match the identifier and ignore nested matching begin/end blocks.
        let begin_line_end = rest.find('\n').unwrap_or(rest.len());
        let begin_line = &rest[..begin_line_end];
        let target = begin_line.split_whitespace().next().unwrap_or("");
        let mut remaining = rest.get(begin_line_end + 1..).unwrap_or_default();
        let mut depth = 1usize;

        while !remaining.is_empty() {
            let line_end = remaining.find('\n').unwrap_or(remaining.len());
            let line = &remaining[..line_end];
            let next = remaining.get(line_end + 1..).unwrap_or_default();

            if let Some((directive, directive_target)) = parse_pod_directive_line(line) {
                if directive == "begin" && directive_target == target {
                    depth += 1;
                } else if directive == "end" && directive_target == target {
                    depth -= 1;
                    if depth == 0 {
                        return Ok((next, ""));
                    }
                }
            }

            remaining = next;
        }

        Ok(("", ""))
    } else {
        // =for, =head1, =item, =comment, etc. — skip to end of paragraph (blank line)
        let end = rest.find('\n').unwrap_or(rest.len());
        let mut rest = &rest[end..];
        loop {
            if rest.is_empty() {
                break;
            }
            // Consume newline
            if rest.starts_with('\n') {
                rest = &rest[1..];
            } else {
                break;
            }
            // If blank line or another Pod directive, stop
            if rest.is_empty() || rest.starts_with('\n') || rest.starts_with('=') {
                break;
            }
            // Skip continuation line
            if let Some(nl) = rest.find('\n') {
                rest = &rest[nl..];
            } else {
                rest = "";
                break;
            }
        }
        Ok((rest, ""))
    }
}

fn parse_pod_directive_line(line: &str) -> Option<(&str, &str)> {
    let trimmed = line.trim_start();
    let rest = trimmed.strip_prefix('=')?;
    if rest.is_empty() || !rest.as_bytes()[0].is_ascii_alphabetic() {
        return None;
    }

    let mut end = 0usize;
    for (idx, ch) in rest.char_indices() {
        if ch.is_alphanumeric() || ch == '-' {
            end = idx + ch.len_utf8();
        } else {
            break;
        }
    }
    if end == 0 {
        return None;
    }
    let directive = &rest[..end];
    let after = &rest[end..];
    if let Some(ch) = after.chars().next()
        && !ch.is_whitespace()
    {
        return None;
    }

    let target = after.split_whitespace().next().unwrap_or("");
    Some((directive, target))
}

/// Skip an embedded comment `#`<bracket>...<close>`.
/// Returns the remaining input after the comment, or None if not an embedded comment.
fn skip_embedded_comment(input: &str) -> Option<&str> {
    // Must start with #`
    let after_hash_backtick = input.strip_prefix("#`")?;
    if after_hash_backtick.is_empty() {
        return None;
    }
    // The next character(s) must be an opening bracket (no space allowed)
    let mut chars = after_hash_backtick.chars();
    let open_char = chars.next()?;
    let close_char = matching_bracket(open_char)?;

    // Count how many consecutive identical open brackets
    let mut count = 1usize;
    let mut rest = chars.as_str();
    while rest.starts_with(open_char) {
        count += 1;
        rest = &rest[open_char.len_utf8()..];
    }

    // Build the closing sequence
    let close_seq: String = std::iter::repeat_n(close_char, count).collect();

    // Find the closing sequence (brackets may nest if count == 1)
    let open_seq: String = std::iter::repeat_n(open_char, count).collect();
    if count == 1 {
        // Track nesting depth for single-char brackets
        let mut depth = 1i32;
        let mut scan = rest;
        while !scan.is_empty() {
            let c = scan.chars().next().unwrap();
            if c == open_char {
                depth += 1;
            } else if c == close_char {
                depth -= 1;
                if depth == 0 {
                    return Some(&scan[close_char.len_utf8()..]);
                }
            }
            scan = &scan[c.len_utf8()..];
        }
        None
    } else {
        // Multi-char delimiters: count nesting of same-length bracket pairs
        let mut depth = 1i32;
        let mut scan = rest;
        while !scan.is_empty() {
            if scan.starts_with(&close_seq[..]) {
                depth -= 1;
                if depth == 0 {
                    return Some(&scan[close_seq.len()..]);
                }
                scan = &scan[close_seq.len()..];
            } else if scan.starts_with(&open_seq[..]) {
                depth += 1;
                scan = &scan[open_seq.len()..];
            } else {
                let c = scan.chars().next().unwrap();
                scan = &scan[c.len_utf8()..];
            }
        }
        None
    }
}

/// Return the matching closing bracket for an opening bracket.
fn matching_bracket(c: char) -> Option<char> {
    match c {
        '(' => Some(')'),
        '[' => Some(']'),
        '{' => Some('}'),
        '<' => Some('>'),
        '\u{00AB}' => Some('\u{00BB}'), // « »
        '\u{2018}' => Some('\u{2019}'), // ' '
        '\u{201C}' => Some('\u{201D}'), // " "
        '\u{300C}' => Some('\u{300D}'), // 「 」
        '\u{300E}' => Some('\u{300F}'), // 『 』
        '\u{FF08}' => Some('\u{FF09}'), // （ ）
        '\u{300A}' => Some('\u{300B}'), // 《 》
        '\u{3008}' => Some('\u{3009}'), // 〈 〉
        '\u{169B}' => Some('\u{169C}'), // ᚛ ᚜
        '\u{2045}' => Some('\u{2046}'), // ⁅ ⁆
        '\u{207D}' => Some('\u{207E}'), // ⁽ ⁾
        '\u{2768}' => Some('\u{2769}'), // ❨ ❩
        '\u{276E}' => Some('\u{276F}'), // ❮ ❯
        '\u{2770}' => Some('\u{2771}'), // ❰ ❱
        '\u{2772}' => Some('\u{2773}'), // ❲ ❳
        '\u{27E6}' => Some('\u{27E7}'), // ⟦ ⟧
        '\u{2985}' => Some('\u{2986}'), // ⦅ ⦆
        '\u{2993}' => Some('\u{2994}'), // ⦓ ⦔
        '\u{2995}' => Some('\u{2996}'), // ⦕ ⦖
        _ => None,
    }
}

/// Check if a byte is a valid identifier continuation character.
pub(super) fn is_ident_char(b: Option<u8>) -> bool {
    match b {
        Some(c) => c.is_ascii_alphanumeric() || c == b'_' || c == b'-',
        None => false,
    }
}

/// Raku identifier start: underscore or Unicode alphabetic character.
pub(super) fn is_raku_identifier_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

/// Raku identifier continuation: start chars, decimal digits, and combining marks.
pub(super) fn is_raku_identifier_continue(c: char) -> bool {
    is_raku_identifier_start(c)
        || c.is_numeric()
        || unicode_normalization::char::is_combining_mark(c)
}

/// Normalize an identifier using canonical composition only (NFC).
pub(super) fn normalize_raku_identifier(name: &str) -> String {
    name.nfc().collect()
}

/// Skip balanced parentheses starting from an opening `(`.
/// Returns the remaining input after the closing `)`, or the original input if it
/// doesn't start with `(`.
pub(super) fn skip_balanced_parens(input: &str) -> &str {
    if let Some(inner) = input.strip_prefix('(') {
        let mut depth = 1u32;
        let mut rr = inner;
        while depth > 0 && !rr.is_empty() {
            if rr.starts_with('(') {
                depth += 1;
            } else if rr.starts_with(')') {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            rr = &rr[rr.chars().next().unwrap().len_utf8()..];
        }
        rr.strip_prefix(')').unwrap_or(inner)
    } else {
        input
    }
}

/// Require at least one whitespace character (or comment).
pub(super) fn ws1(input: &str) -> PResult<'_, ()> {
    let (rest, _) = ws(input)?;
    if rest.len() == input.len() {
        return Err(PError::expected("whitespace"));
    }
    Ok((rest, ()))
}

/// Consume unspace: backslash followed by whitespace collapses to nothing.
/// Returns the remaining input after any unspace, or the original input if no unspace.
pub(super) fn consume_unspace(input: &str) -> &str {
    if let Some(after_bs) = input.strip_prefix('\\')
        && let Some(c) = after_bs.chars().next()
        && c.is_whitespace()
    {
        let mut scan = &after_bs[c.len_utf8()..];
        while let Some(c2) = scan.chars().next() {
            if c2.is_whitespace() {
                scan = &scan[c2.len_utf8()..];
            } else {
                break;
            }
        }
        return scan;
    }
    input
}

/// Returns true for non-breaking space characters that should not split words in `<...>`.
pub(super) fn is_non_breaking_space(c: char) -> bool {
    matches!(c, '\u{00A0}' | '\u{2007}' | '\u{202F}' | '\u{FEFF}')
}

/// Split angle-list content into words, using breaking whitespace only.
///
/// In Raku `<...>` list quotes, non-breaking spaces are not separators.
pub(super) fn split_angle_words(content: &str) -> Vec<&str> {
    let mut words = Vec::new();
    let mut rest = content;
    loop {
        let (r, _) = take_while_opt(rest, |c: char| {
            c.is_whitespace() && !is_non_breaking_space(c)
        });
        rest = r;
        if rest.is_empty() {
            break;
        }
        let Ok((r, word)) = take_while1(rest, |c: char| {
            !c.is_whitespace() || is_non_breaking_space(c)
        }) else {
            break;
        };
        words.push(word);
        rest = r;
    }
    words
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ws_comment() {
        let (rest, _) = ws("  # comment\nhello").unwrap();
        assert_eq!(rest, "hello");
    }

    #[test]
    fn test_pod_begin_end() {
        let input = "=begin pod\nsome docs\n=end pod\ncode";
        let (rest, _) = ws(input).unwrap();
        assert_eq!(rest, "code");
    }

    #[test]
    fn test_pod_begin_end_ignores_inner_end_with_different_target() {
        let input = "=begin pod\n=begin item\nfoo\n=end item\nbar\n=end pod\ncode";
        let (rest, _) = ws(input).unwrap();
        assert_eq!(rest, "code");
    }

    #[test]
    fn test_pod_begin_end_supports_nested_same_target() {
        let input = "=begin pod\n=begin pod\ninner\n=end pod\nouter\n=end pod\ncode";
        let (rest, _) = ws(input).unwrap();
        assert_eq!(rest, "code");
    }

    #[test]
    fn test_pod_for() {
        let input = "=for comment\nsome comment\n\ncode";
        let (rest, _) = ws(input).unwrap();
        assert_eq!(rest, "code");
    }

    #[test]
    fn split_angle_words_splits_breaking_whitespace() {
        let words = split_angle_words("a\tb c\nd");
        assert_eq!(words, vec!["a", "b", "c", "d"]);
    }

    #[test]
    fn split_angle_words_keeps_non_breaking_space_inside_word() {
        let words = split_angle_words("a\u{00A0}b");
        assert_eq!(words, vec!["a\u{00A0}b"]);
    }
}
