mod expr;
mod helpers;
mod parse_result;
mod primary;
mod stmt;
use std::sync::OnceLock;

use crate::ast::Stmt;
use crate::value::RuntimeError;

static PARSE_MEMO_ENABLED: OnceLock<bool> = OnceLock::new();

pub(super) fn parse_memo_enabled() -> bool {
    *PARSE_MEMO_ENABLED.get_or_init(|| {
        std::env::var("MUTSU_PARSE_MEMO")
            .map(|v| v != "0")
            .unwrap_or(true)
    })
}

fn line_col_at_offset(source: &str, offset: usize) -> (usize, usize) {
    let offset = offset.min(source.len());
    let prefix = &source[..offset];
    let line = prefix.matches('\n').count() + 1;
    let col = prefix
        .rsplit('\n')
        .next()
        .map(|segment| segment.chars().count() + 1)
        .unwrap_or(1);
    (line, col)
}

fn leading_ws_bytes(input: &str) -> usize {
    input.len().saturating_sub(input.trim_start().len())
}

fn near_snippet(input: &str, max_chars: usize) -> Option<String> {
    let trimmed = input.trim_start();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.chars().take(max_chars).collect())
    }
}

/// Parse a full program using the nom-based parser.
/// Returns `(statements, Option<finish_content>)`.
#[allow(clippy::result_large_err)]
pub(crate) fn parse_program(input: &str) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    let memo_enabled = parse_memo_enabled();
    if memo_enabled {
        expr::reset_expression_memo();
        primary::reset_primary_memo();
        stmt::reset_statement_memo();
    }
    crate::trace::trace_log!("parse", "parser_nom start memo={}", memo_enabled);
    // Split off =finish content before parsing
    let (source, finish_content) = if let Some(idx) = input.find("\n=finish") {
        let content = &input[idx + "\n=finish".len()..];
        // Skip to next newline
        let content = if let Some(nl) = content.find('\n') {
            &content[nl + 1..]
        } else {
            ""
        };
        (&input[..idx], Some(content.to_string()))
    } else {
        (input, None)
    };

    let result = match stmt::program(source) {
        Ok((rest, stmts)) => {
            let rest_trimmed = rest.trim();
            if !rest_trimmed.is_empty() {
                let consumed = source.len() - rest.len();
                let near_offset = consumed + leading_ws_bytes(rest);
                let (line_num, col_num) = line_col_at_offset(source, near_offset);
                let context: String = rest_trimmed.chars().take(60).collect();
                Err(RuntimeError::new(format!(
                    "parse error: unparsed input at line {}, column {}: {:?}",
                    line_num, col_num, context
                )))
            } else {
                Ok((stmts, finish_content))
            }
        }
        Err(e) => {
            if let Some(consumed) = e.consumed_from(source.len()) {
                let tail = &source[consumed..];
                let near_offset = consumed + leading_ws_bytes(tail);
                let (line_num, col_num) = line_col_at_offset(source, near_offset);
                if let Some(context) = near_snippet(tail, 60) {
                    Err(RuntimeError::new(format!(
                        "parse error at line {}, column {}: {} — near: {:?}",
                        line_num, col_num, e, context
                    )))
                } else {
                    Err(RuntimeError::new(format!(
                        "parse error at line {}, column {}: {}",
                        line_num, col_num, e
                    )))
                }
            } else {
                Err(RuntimeError::new(format!("parse error: {}", e)))
            }
        }
    };

    if memo_enabled && crate::trace::is_enabled("parse") {
        let (stmt_hits, stmt_misses, stmt_stores) = stmt::statement_memo_stats();
        let (expr_hits, expr_misses, expr_stores) = expr::expression_memo_stats();
        let (primary_hits, primary_misses, primary_stores) = primary::primary_memo_stats();
        crate::trace::trace_log!(
            "parse",
            "memo stats stmt[h/m/s]={}/{}/{} expr[h/m/s]={}/{}/{} primary[h/m/s]={}/{}/{}",
            stmt_hits,
            stmt_misses,
            stmt_stores,
            expr_hits,
            expr_misses,
            expr_stores,
            primary_hits,
            primary_misses,
            primary_stores
        );
    }

    result
}

#[cfg(test)]
mod tests {
    use super::parse_program;

    #[test]
    fn parse_program_reports_line_and_column_for_unparsed_input() {
        let err = parse_program("}").unwrap_err();
        assert!(err.message.contains("line 1, column 1"));
        assert!(err.message.contains("unparsed input"));
    }

    #[test]
    fn parse_program_reports_line_and_column_for_parse_error() {
        let err = parse_program("say 1;\nok(,)").unwrap_err();
        assert!(err.message.contains("line 2"));
        assert!(err.message.contains("column"));
        assert!(err.message.contains("parse error"));
    }

    #[test]
    fn parse_program_unparsed_column_skips_leading_whitespace() {
        let err = parse_program("say 1;\n   }").unwrap_err();
        assert!(err.message.contains("line 2, column 4"));
    }
}
