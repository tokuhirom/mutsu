//! Skipping a delimited Pod block (`=begin TYPE` ... `=end TYPE`) (#18369).
//!
//! The parser skips Pod; what it skips decides what executes. As in rakudo,
//! the `=end TYPE` that closes a delimited block is written at the `=begin`'s
//! own indentation:
//!
//! - inside a verbatim block (`code`, `input`, `output`, `comment`) an
//!   indented `=end`/`=begin` is text, so the block runs to the `=end` at its
//!   own indentation;
//! - inside any other block, a same-type `=begin` at the block's indentation
//!   nests, and a same-type `=end` at another indentation is the RakuDoc syntax
//!   error rakudo raises (`X::Syntax::InfixInTermPosition`).

use super::helpers::{parse_pod_directive_line, validate_pod_table_not_empty};
use super::parse_result::{PError, PResult};

/// The blocks whose body is text: a directive inside one is not a directive.
/// The same list as the RakuAST view's (`rakuast::pod_blocks`).
const VERBATIM: &[&str] = &["code", "input", "output", "comment"];

/// Skip the body of a `=begin target` block, from `body` (the line after the
/// `=begin` line) to past its closing `=end target`; `margin` is the
/// `=begin`'s indentation. Runs to the end of the source when no `=end`
/// closes it.
// Cost: O(n), n = length of the block's body.
pub(super) fn skip_body<'a>(body: &'a str, target: &str, margin: &str) -> PResult<'a, &'a str> {
    let verbatim = VERBATIM.contains(&target);
    let is_table = target == "table";
    let mut remaining = body;
    let mut depth = 1usize;
    let mut content_lines: Vec<&str> = Vec::new();
    while !remaining.is_empty() {
        let line_end = remaining.find('\n').unwrap_or(remaining.len());
        let line = &remaining[..line_end];
        let next = remaining.get(line_end + 1..).unwrap_or_default();
        if let Some((directive, directive_target)) = parse_pod_directive_line(line)
            && directive_target == target
        {
            let indent = &line[..line.len() - line.trim_start().len()];
            let at_margin = indent == margin;
            match directive {
                "end" if at_margin => {
                    depth -= 1;
                    if depth == 0 {
                        if is_table {
                            // Only an empty table is checked here: cell
                            // content may hold arbitrary whitespace.
                            validate_pod_table_not_empty(&content_lines)?;
                        }
                        return Ok((next, ""));
                    }
                }
                "end" if !verbatim => return Err(misplaced_end_error(remaining, target)),
                "begin" if at_margin && !verbatim => depth += 1,
                _ => {}
            }
        }
        if is_table && depth == 1 {
            content_lines.push(line);
        }
        remaining = next;
    }
    Ok(("", ""))
}

/// An `=end target` with no open `=begin target`: rakudo's RakuDoc syntax
/// error, reported at that line (`construct` starts at its `=`).
// Cost: O(1).
pub(super) fn orphan_end_error(construct: &str, target: &str) -> PError {
    syntax_error(
        construct,
        format!(
            "An error in RakuDoc syntax is suspected. Perhaps a '=begin {target}' marker was \
             forgotten or was improperly nested? Alternately, an infix '=' was seen where a \
             term was expected."
        ),
    )
}

/// An `=end target` at another indentation than its block's `=begin`:
/// rakudo's RakuDoc syntax error, reported at that line.
// Cost: O(1).
fn misplaced_end_error(construct: &str, target: &str) -> PError {
    syntax_error(
        construct,
        format!(
            "An error in RakuDoc syntax is suspected. Perhaps an '=end {target}' marker was \
             forgotten or was improperly nested? Or such a marker was absorbed by an unclosed \
             '=begin ignore' marker? Alternately, an infix '=' was seen where a term was \
             expected."
        ),
    )
}

/// The RakuDoc syntax error rakudo raises as `X::Syntax::InfixInTermPosition`.
// Cost: O(1).
fn syntax_error(construct: &str, msg: String) -> PError {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
    let ex = crate::value::Value::make_instance(
        crate::symbol::Symbol::intern("X::Syntax::InfixInTermPosition"),
        attrs,
    );
    let mut err = PError::fatal_with_exception(msg, Box::new(ex));
    err.remaining_len = Some(construct.len());
    err
}
