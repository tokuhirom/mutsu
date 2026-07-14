use super::*;
use crate::ast::Expr;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::value::Value;
use crate::value::ValueView;

/// Find the newline that ends the *logical* source line starting at `r`.
///
/// A heredoc body begins after the line on which the heredoc was introduced, but a
/// newline that a string literal or a comment has already swallowed does not end that
/// line. `q:to/END/, "a\nb";` therefore keeps its body below the `b";` line, not below
/// the `a` line. Returns the byte offset of the terminating newline.
fn logical_line_end(r: &str) -> Option<usize> {
    let bytes = r.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        match bytes[i] {
            b'\n' => return Some(i),
            // A comment runs to the end of the line, so the next newline ends the line.
            b'#' => return r[i..].find('\n').map(|nl| i + nl),
            q @ (b'"' | b'\'') => {
                // An apostrophe between word characters is part of an identifier
                // (`isn't-ok`), not the start of a string.
                let ident_tick = q == b'\''
                    && i > 0
                    && (bytes[i - 1].is_ascii_alphanumeric() || bytes[i - 1] == b'_')
                    && bytes.get(i + 1).is_some_and(|c| c.is_ascii_alphabetic());
                i += 1;
                if ident_tick {
                    continue;
                }
                while i < bytes.len() && bytes[i] != q {
                    // Skip the escaped character so `"\""` does not close early.
                    i += if bytes[i] == b'\\' { 2 } else { 1 };
                }
                i += 1;
            }
            _ => i += 1,
        }
    }
    None
}

pub(crate) fn parse_to_heredoc_delimiter(input: &str) -> PResult<'_, &'_ str> {
    // Raku allows optional whitespace before the delimiter in q:to/Q:to
    // forms, e.g. q:to /END/;
    let input = input.trim_start_matches(' ');
    let open = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("heredoc delimiter"))?;
    if open.is_alphanumeric() || open.is_whitespace() {
        return Err(PError::expected("heredoc delimiter"));
    }

    if let Some(close) = unicode_bracket_close(open) {
        let (rest, delimiter) = read_bracketed(input, open, close, false)?;
        return Ok((rest, delimiter));
    }

    // Symmetric non-bracket delimiter such as /.../
    let body = &input[open.len_utf8()..];
    let end = body
        .find(open)
        .ok_or_else(|| PError::expected("closing heredoc delimiter"))?;
    let delimiter = &body[..end];
    let rest = &body[end + open.len_utf8()..];
    Ok((rest, delimiter))
}

/// Parse heredoc with flags (supports :c, :w adverbs on heredoc).
pub(crate) fn parse_to_heredoc_with_flags<'a>(
    input: &'a str,
    flags: &crate::parser::primary::quote_adverbs::QuoteFlags,
    interpolate: bool,
) -> PResult<'a, Expr> {
    use crate::parser::primary::quote_adverbs::process_content_with_flags;

    let (r, delimiter) = parse_to_heredoc_delimiter(input)?;
    let (rest_of_line, heredoc_start) = if let Some(nl) = logical_line_end(r) {
        (&r[..nl], &r[nl + 1..])
    } else {
        return Err(PError::expected("heredoc body after newline"));
    };
    // Find the terminator line
    let mut content_end = None;
    let mut terminator_end = None;
    let mut terminator_indent = 0usize;
    let mut search_pos = 0;
    while search_pos <= heredoc_start.len() {
        let line = &heredoc_start[search_pos..];
        let leading_ws_bytes: usize = line
            .chars()
            .take_while(|c| matches!(c, ' ' | '\t'))
            .map(char::len_utf8)
            .sum();
        let term_pos = search_pos + leading_ws_bytes;
        if heredoc_start[term_pos..].starts_with(delimiter) {
            let after_delim = &heredoc_start[term_pos + delimiter.len()..];
            if after_delim.is_empty()
                || after_delim.starts_with('\n')
                || after_delim.starts_with('\r')
                || after_delim.starts_with(';')
            {
                content_end = Some(search_pos);
                terminator_end = Some(term_pos + delimiter.len());
                terminator_indent = ws_column_width(line);
                break;
            }
        }
        if let Some(nl) = heredoc_start[search_pos..].find('\n') {
            search_pos += nl + 1;
        } else {
            break;
        }
    }
    if let Some(end) = content_end {
        let content = &heredoc_start[..end];
        let content = if terminator_indent == 0 {
            content.to_string()
        } else {
            dedent_heredoc(content, terminator_indent)
        };
        let after_terminator = &heredoc_start[terminator_end.expect("terminator end")..];
        let after_terminator = after_terminator
            .strip_prefix('\n')
            .unwrap_or(after_terminator);

        // Process content based on flags
        let expr = if interpolate {
            Expr::HeredocInterpolation(content)
        } else if flags.has_interpolation() || flags.words || flags.backslash {
            // Use flags-based processing for heredoc with adverbs
            process_content_with_flags(&content, flags)
        } else if content.contains("\\qq") {
            parse_single_quote_qq(&content)
        } else {
            // q:heredoc processes \\ → \ (same as single-quoted strings)
            let processed = process_q_escapes(&content, '\0');
            Expr::Literal(Value::str(processed))
        };

        // Apply word splitting if :w
        let expr = if flags.words {
            if let Expr::Literal(lit) = &expr
                && let ValueView::Str(s) = lit.view()
            {
                let words: Vec<Expr> = s
                    .split_whitespace()
                    .map(|w| Expr::Literal(Value::str(w.to_string())))
                    .collect();
                Expr::ArrayLiteral(words)
            } else {
                Expr::MethodCall {
                    target: Box::new(expr),
                    name: Symbol::intern("words"),
                    args: vec![],
                    modifier: None,
                    quoted: false,
                }
            }
        } else {
            expr
        };

        if rest_of_line.trim().is_empty() {
            return Ok((after_terminator, expr));
        }
        let decl_line = crate::parser::primary::current_line_number(rest_of_line);
        let heredoc_body_lines = heredoc_start[..terminator_end.expect("terminator end")]
            .matches('\n')
            .count() as i64;
        let rest_of_line_lines = rest_of_line.matches('\n').count() as i64;
        let after_term_line = decl_line + rest_of_line_lines + 1 + heredoc_body_lines + 1;
        let combined = format!("{}\n{}", rest_of_line, after_terminator);
        let leaked: &'static str = Box::leak(combined.into_boxed_str());
        crate::parser::primary::register_leaked_region_with_jump(
            leaked,
            rest_of_line.len() + 1,
            decl_line,
            after_term_line,
        );
        return Ok((leaked, expr));
    }
    Err(PError::expected("heredoc terminator"))
}

/// Compute the visual column width of leading whitespace, treating tabs as
/// stops at every 8 columns.
pub(crate) fn ws_column_width(s: &str) -> usize {
    let mut col = 0usize;
    for ch in s.chars() {
        match ch {
            ' ' => col += 1,
            '\t' => col = (col / 8 + 1) * 8,
            _ => break,
        }
    }
    col
}

/// Dedent heredoc content by removing leading whitespace matching the terminator's
/// visual column width. Tabs are treated as stops at every 8 columns so that
/// mixed tab/space indentation is handled correctly.
pub(crate) fn dedent_heredoc(content: &str, terminator_indent_bytes: usize) -> String {
    // First, compute the terminator indent as a column width.
    // We receive the raw byte length of the terminator's leading whitespace,
    // so reconstruct the whitespace string to measure its column width.
    // However, the caller only passes byte length, so we need the actual
    // whitespace chars. Instead, accept the byte-based indent and re-derive
    // the column width from the terminator line.
    // Since we can't access the terminator line here, we use an alternative:
    // compute column width from the content's context.
    //
    // Actually, the cleanest approach: take column-based indent directly.
    // For now, we'll use the byte-based indent for the simple (spaces-only
    // or tabs-only) case and fall through to column-based for mixed.
    dedent_heredoc_by_columns(content, terminator_indent_bytes)
}

pub(crate) fn dedent_heredoc_by_columns(content: &str, target_cols: usize) -> String {
    let mut dedented = String::new();
    for segment in content.split_inclusive('\n') {
        let mut col = 0usize;
        let mut strip_pos = 0usize;
        for ch in segment.chars() {
            if col >= target_cols {
                break;
            }
            match ch {
                ' ' => {
                    col += 1;
                    strip_pos += 1;
                }
                '\t' => {
                    let next_col = (col / 8 + 1) * 8;
                    if next_col > target_cols {
                        // Tab overshoots the target — stop here without
                        // consuming it (the tab will remain in the output).
                        break;
                    }
                    col = next_col;
                    strip_pos += 1;
                }
                _ => break,
            }
        }
        dedented.push_str(&segment[strip_pos..]);
    }
    dedented
}
