use super::parse_result::{PError, PResult, take_while_opt, take_while1};

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
        // Try line comment
        if r.starts_with('#') {
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

    // Only match known pod directives
    const POD_KEYWORDS: &[&str] = &[
        "begin", "end", "for", "head", "head1", "head2", "head3", "head4", "item", "comment",
        "finish", "pod", "config", "table", "TITLE", "SUBTITLE", "para", "code", "input", "output",
        "defn", "nested", "data",
    ];
    if !POD_KEYWORDS
        .iter()
        .any(|&k| keyword == k || keyword.starts_with("head"))
    {
        return Err(PError::expected("known pod directive"));
    }

    if keyword == "begin" {
        // =begin IDENTIFIER ... =end IDENTIFIER
        // Skip the rest of the =begin line
        let end = rest.find('\n').unwrap_or(rest.len());
        let rest = &rest[end..];
        if rest.is_empty() {
            return Ok((rest, ""));
        }
        if let Some(idx) = rest.find("\n=end") {
            let after = &rest[idx + 1..];
            // skip =end line
            let nl = after.find('\n').unwrap_or(after.len());
            let rest = &after[nl..];
            Ok((rest, ""))
        } else {
            Ok(("", ""))
        }
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

/// Require at least one whitespace character (or comment).
pub(super) fn ws1(input: &str) -> PResult<'_, ()> {
    let (rest, _) = ws(input)?;
    if rest.len() == input.len() {
        return Err(PError::expected("whitespace"));
    }
    Ok((rest, ()))
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
    fn test_pod_for() {
        let input = "=for comment\nsome comment\n\ncode";
        let (rest, _) = ws(input).unwrap();
        assert_eq!(rest, "code");
    }
}
