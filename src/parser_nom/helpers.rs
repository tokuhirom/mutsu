use nom::IResult;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{multispace1, not_line_ending};
use nom::combinator::value;
use nom::multi::many0;
use nom::sequence::preceded;

/// Skip whitespace, line comments (`#` to end of line), and Pod blocks.
pub(super) fn ws(input: &str) -> IResult<&str, ()> {
    let (input, _) = many0(alt((
        value((), multispace1),
        value((), preceded(tag("#"), not_line_ending)),
        value((), pod_block),
    )))(input)?;
    Ok((input, ()))
}

/// Parse and skip a Pod block.
/// Handles `=begin ... =end`, `=for ...`, `=head ...`, `=item ...`, `=comment ...`, etc.
fn pod_block(input: &str) -> IResult<&str, &str> {
    // Must start at beginning of line (or at start of input)
    // Pod directives start with `=`
    let (input, _) = tag("=")(input)?;

    // Read the directive keyword
    let (input, keyword) = take_while1(|c: char| c.is_alphanumeric() || c == '-')(input)?;

    if keyword == "begin" {
        // =begin IDENTIFIER ... =end IDENTIFIER
        // Skip the rest of the =begin line
        let (input, _) = not_line_ending(input)?;
        // Find matching =end
        let rest = input;
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
        let (input, _) = not_line_ending(input)?;
        let mut rest = input;
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
pub(super) fn ws1(input: &str) -> IResult<&str, ()> {
    let (input, _) = alt((
        value((), multispace1),
        value((), preceded(tag("#"), not_line_ending)),
    ))(input)?;
    let (input, _) = ws(input)?;
    Ok((input, ()))
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
