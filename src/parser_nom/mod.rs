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
                // Show context around where parsing stopped
                let consumed = source.len() - rest.len();
                let line_num = source[..consumed].matches('\n').count() + 1;
                let context: String = rest_trimmed.chars().take(60).collect();
                Err(RuntimeError::new(format!(
                    "parse error: unparsed input at line {}: {:?}",
                    line_num, context
                )))
            } else {
                Ok((stmts, finish_content))
            }
        }
        Err(e) => {
            if let Some(consumed) = e.consumed_from(source.len()) {
                let line_num = source[..consumed].matches('\n').count() + 1;
                let context_start = &source[consumed..];
                let context_trimmed = context_start.trim_start();
                if !context_trimmed.is_empty() {
                    let context: String = context_trimmed.chars().take(60).collect();
                    Err(RuntimeError::new(format!(
                        "parse error at line {}: {} — near: {:?}",
                        line_num, e, context
                    )))
                } else {
                    Err(RuntimeError::new(format!(
                        "parse error at line {}: {}",
                        line_num, e
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
