//! Render a [`RuntimeError`] the way the CLI prints it to stderr.
//!
//! This lives in the library (not `main.rs`) so the in-process `is_run` /
//! `run` test helpers can produce byte-identical stderr to a real subprocess:
//! parse errors render as `===SORRY!===` with a caret snippet, runtime errors
//! with a backtrace render Raku-style (`message\n  in block <unit> at ...`).

use crate::value::RuntimeError;

/// Extract a short, human-readable summary from a verbose parse error message.
/// The parser produces messages like:
///   "Confused. parse error at line 1, column 1: expected expected statement ..."
/// We extract a cleaner version for display.
fn short_parse_message(msg: &str) -> String {
    // Remember the "Confused" marker: rakudo renders "Confused" as the SORRY
    // message body and roast checks stderr for it (S02-one-pass-parsing/misc.t),
    // so it is re-prefixed onto the simplified message below.
    let confused = msg.starts_with("Confused.");
    let body = msg.strip_prefix("Confused. ").unwrap_or(msg);

    // Strip " \u{2014} near: ..." suffix (em-dash followed by near)
    let body = body
        .find("\u{2014} near:")
        .map(|i| body[..i].trim_end())
        .or_else(|| body.find(" — near:").map(|i| body[..i].trim_end()))
        .unwrap_or(body);

    // Strip "parse error at line N, column M: " prefix to get the core message
    let core = if let Some(pos) = body.find(": expected ") {
        &body[pos + 2..] // skip ": " to get "expected ..."
    } else if let Some(pos) = body.find(": unparsed ") {
        &body[pos + 2..]
    } else {
        body
    };

    // Remove duplicate "expected expected" from Display wrapping
    let core = core
        .strip_prefix("expected expected ")
        .map(|rest| format!("expected {}", rest))
        .unwrap_or_else(|| core.to_string());

    // Strip internal location details like "at line N (after M stmts)"
    let core = strip_internal_location(&core);

    // Simplify long alternative lists
    let simplified = simplify_expected_list(&core);
    if confused {
        format!("Confused. {}", simplified)
    } else {
        simplified
    }
}

/// Strip internal location details like "at line N (after M stmts)" from parse messages.
fn strip_internal_location(msg: &str) -> String {
    let mut result = msg.to_string();
    while let Some(start) = result.find(" at line ") {
        let rest = &result[start + " at line ".len()..];
        let digit_end = rest
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(rest.len());
        if digit_end == 0 {
            break;
        }
        let mut end = start + " at line ".len() + digit_end;
        // Check for optional " (after N stmts)"
        let after_digits = &result[end..];
        if after_digits.starts_with(" (after ")
            && let Some(paren_close) = after_digits.find(')')
        {
            end += paren_close + 1;
        }
        result = format!("{}{}", &result[..start], &result[end..]);
    }
    result
}

/// Trim long "expected X or Y or Z or ..." lists to a reasonable length.
fn simplify_expected_list(msg: &str) -> String {
    if let Some(pos) = msg.rfind(": expected ") {
        let prefix = &msg[..pos];
        let alternatives = &msg[pos + 11..]; // after ": expected "
        let parts: Vec<&str> = alternatives.split(" or ").collect();
        if parts.len() > 5 {
            let kept: Vec<&str> = parts[..5].to_vec();
            return format!("{}: expected {} or ...", prefix, kept.join(" or "));
        }
    }
    msg.to_string()
}

/// Format a parse error with a source code snippet and caret indicator,
/// matching Raku's ===SORRY!=== format.
pub fn format_parse_error(err: &RuntimeError, source: &str, program_name: &str) -> String {
    let mut out = String::new();

    out.push_str(&format!(
        "===SORRY!=== Error while compiling {}\n",
        program_name
    ));

    let short_msg = short_parse_message(&err.message);
    out.push_str(&short_msg);
    out.push('\n');

    if let Some(line) = err.line() {
        out.push_str(&format!("at {}:{}\n", program_name, line));

        let source_lines: Vec<&str> = source.lines().collect();
        if line >= 1 && line <= source_lines.len() {
            let src_line = source_lines[line - 1];
            let col = err.column().unwrap_or(1);
            // `col` is a 1-based character column, so split the line by
            // character position (not byte offset) to avoid slicing inside a
            // multi-byte UTF-8 character.
            let col_idx = col.saturating_sub(1).min(src_line.chars().count());
            let before: String = src_line.chars().take(col_idx).collect();
            let after: String = src_line.chars().skip(col_idx).collect();
            out.push_str(&format!("------>{}{}\n", before, after));
            let padding = " ".repeat(7 + col_idx);
            out.push_str(&format!("{}^", padding));
        }
    }

    if let Some(hint) = err.hint() {
        out.push('\n');
        out.push_str(hint);
    }

    out
}

/// Render an error exactly as the CLI prints it to stderr (without the
/// trailing newline). `prefix` is "Parse error" or "Runtime error".
pub fn render_error(
    prefix: &str,
    err: &RuntimeError,
    source: Option<&str>,
    program_name: Option<&str>,
) -> String {
    // For runtime errors with a backtrace, use the Raku-style format:
    // message\n  in sub foo at file line N\n  in block <unit> at file line N
    if err.backtrace().is_some() && err.code().is_none() {
        let mut out = err.message.clone();
        if let Some(bt) = err.backtrace() {
            out.push('\n');
            out.push_str(bt);
        }
        return out;
    }

    // For parse errors with source context, show a nice snippet
    if let (Some(code), Some(src), Some(name)) = (err.code(), source, program_name)
        && code.is_parse()
        && err.line().is_some()
    {
        return format_parse_error(err, src, name);
    }

    let mut out = format!("{}: {}", prefix, err.message);
    if let Some(line) = err.line() {
        if let Some(col) = err.column() {
            out.push_str(&format!("\n  at line {}, column {}", line, col));
        } else {
            out.push_str(&format!("\n  at line {}", line));
        }
    }
    if let Some(hint) = err.hint() {
        out.push_str(&format!("\n  hint: {}", hint));
    }
    out
}
