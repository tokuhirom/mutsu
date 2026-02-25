mod expr;
mod helpers;
mod memo;
mod parse_result;
mod primary;
mod stmt;
use std::sync::OnceLock;

pub(crate) fn is_imported_function(name: &str) -> bool {
    stmt::simple::is_imported_function(name)
}

pub use stmt::simple::{clear_parser_lib_paths, set_parser_lib_paths, set_parser_program_path};

use crate::ast::Stmt;
use crate::value::RuntimeError;
use crate::value::RuntimeErrorCode;

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

fn parse_error_hint(message: &str) -> Option<&'static str> {
    if message.contains("method name") {
        Some("check method-call syntax after '.' (for example: '$obj.method').")
    } else if message.contains("identifier after '::'") {
        Some("qualified names require an identifier after each '::'.")
    } else if message.contains("after ','") {
        Some("a comma usually requires another expression or argument after it.")
    } else if message.contains("after comparison operator")
        || message.contains("after additive operator")
        || message.contains("after multiplicative operator")
    {
        Some("binary operators require a right-hand expression.")
    } else {
        None
    }
}

fn with_parse_hint(mut err: RuntimeError) -> RuntimeError {
    if let Some(hint) = parse_error_hint(&err.message) {
        err.hint = Some(hint.to_string());
    }
    err
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
    stmt::reset_user_subs();
    crate::trace::trace_log!("parse", "parser start memo={}", memo_enabled);
    primary::set_original_source(input);
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
                Err(RuntimeError::with_location(
                    format!(
                        "Confused. parse error: unparsed input at line {}, column {}: {:?}",
                        line_num, col_num, context
                    ),
                    RuntimeErrorCode::ParseUnparsed,
                    line_num,
                    col_num,
                ))
            } else {
                Ok((stmts, finish_content))
            }
        }
        Err(e) => {
            if e.is_fatal() {
                // Fatal parse errors (e.g. bare say/print/put) pass through directly
                let mut err = RuntimeError::new(format!("{}", e));
                err.code = Some(RuntimeErrorCode::ParseGeneric);
                return Err(err);
            }
            if let Some(consumed) = e.consumed_from(source.len()) {
                let tail = &source[consumed..];
                let near_offset = consumed + leading_ws_bytes(tail);
                let (line_num, col_num) = line_col_at_offset(source, near_offset);
                if let Some(context) = near_snippet(tail, 60) {
                    Err(with_parse_hint(RuntimeError::with_location(
                        format!(
                            "Confused. parse error at line {}, column {}: {} — near: {:?}",
                            line_num, col_num, e, context
                        ),
                        RuntimeErrorCode::ParseExpected,
                        line_num,
                        col_num,
                    )))
                } else {
                    Err(with_parse_hint(RuntimeError::with_location(
                        format!(
                            "Confused. parse error at line {}, column {}: {}",
                            line_num, col_num, e
                        ),
                        RuntimeErrorCode::ParseExpected,
                        line_num,
                        col_num,
                    )))
                }
            } else {
                let mut err = RuntimeError::new(format!("Confused. parse error: {}", e));
                err.code = Some(RuntimeErrorCode::ParseGeneric);
                Err(with_parse_hint(err))
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

/// Best-effort parse: returns all statements that could be parsed before the
/// first error.  Used for loading `.rakumod` modules that may contain syntax
/// mutsu does not yet support.
pub(crate) fn parse_program_partial(input: &str) -> (Vec<Stmt>, Option<String>) {
    let memo_enabled = parse_memo_enabled();
    if memo_enabled {
        expr::reset_expression_memo();
        primary::reset_primary_memo();
        stmt::reset_statement_memo();
    }
    stmt::reset_user_subs();
    primary::set_original_source(input);
    let (source, finish_content) = if let Some(idx) = input.find("\n=finish") {
        let content = &input[idx + "\n=finish".len()..];
        let content = if let Some(nl) = content.find('\n') {
            &content[nl + 1..]
        } else {
            ""
        };
        (&input[..idx], Some(content.to_string()))
    } else {
        (input, None)
    };
    let (stmts, _) = stmt::stmt_list_partial(source);
    (stmts, finish_content)
}

#[cfg(test)]
mod tests {
    use super::parse_program;
    use crate::ast::{Expr, Stmt};
    use crate::value::RuntimeErrorCode;

    #[test]
    fn parse_program_reports_line_and_column_for_unparsed_input() {
        let err = parse_program("}").unwrap_err();
        assert!(err.message.contains("line 1, column 1"));
        assert!(err.message.contains("unparsed input"));
        assert!(matches!(err.code, Some(RuntimeErrorCode::ParseUnparsed)));
        assert_eq!(err.line, Some(1));
        assert_eq!(err.column, Some(1));
    }

    #[test]
    fn parse_program_reports_line_and_column_for_parse_error() {
        let err = parse_program("say 1;\nok(,)").unwrap_err();
        assert!(err.message.contains("line 2"));
        assert!(err.message.contains("column"));
        assert!(err.message.contains("parse error"));
        assert!(matches!(err.code, Some(RuntimeErrorCode::ParseExpected)));
        assert_eq!(err.line, Some(2));
    }

    #[test]
    fn parse_program_unparsed_column_skips_leading_whitespace() {
        let err = parse_program("say 1;\n   }").unwrap_err();
        assert!(err.message.contains("line 2, column 4"));
    }

    #[test]
    fn parse_program_includes_hint_for_common_method_error() {
        let err = parse_program("$x.").unwrap_err();
        assert!(err.message.contains("parse error"));
        assert!(
            err.hint
                .as_deref()
                .is_some_and(|hint| hint.contains("method-call syntax"))
        );
    }

    #[test]
    fn parse_program_accepts_corner_bracket_string_in_listop_call() {
        let src = "sub f($a, $b, $c) { }\nf ｢say 42｣, {:out(\"ok\")}, 'msg';";
        let (stmts, _) = parse_program(src).unwrap();
        assert_eq!(stmts.len(), 2);
        match &stmts[1] {
            Stmt::Expr(Expr::Call { name, args }) => {
                assert_eq!(name, "f");
                assert_eq!(args.len(), 3);
                assert!(
                    matches!(&args[0], Expr::Literal(crate::value::Value::Str(s)) if s == "say 42")
                );
                assert!(matches!(&args[1], Expr::Hash(_)));
                assert!(
                    matches!(&args[2], Expr::Literal(crate::value::Value::Str(s)) if s == "msg")
                );
            }
            other => panic!("expected function call expression, got {other:?}"),
        }
    }

    #[test]
    fn parse_program_accepts_french_quote_word_list() {
        let src = "my @target = $*DISTRO.is-win ?? «/c \"\"» !! '/dev/null';";
        let (stmts, _) = parse_program(src).unwrap();
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn parse_program_accepts_double_angle_quote_word_list_with_quoted_word() {
        let src = "my @str = <<do gjump sover \"\\r\\nth\" elaz yfo x>>;";
        let (stmts, _) = parse_program(src).unwrap();
        assert_eq!(stmts.len(), 1);
        let Stmt::VarDecl { expr, .. } = &stmts[0] else {
            panic!("expected VarDecl")
        };
        let Expr::ArrayLiteral(items) = expr else {
            panic!("expected array literal")
        };
        assert_eq!(items.len(), 7);
        assert!(matches!(
            &items[3],
            Expr::Literal(crate::value::Value::Str(s)) if s == "\r\nth"
        ));
    }

    #[test]
    fn parse_program_accepts_unicode_single_quoted_regex_atoms() {
        let src = r#"
ok("ab/cd" ~~ m/ab ‘/’ c d/, "curly single quote");
ok("ab/cd" ~~ m/ab ‚/’ c d/, "low-high single quote");
ok("ab/cd" ~~ m/ab ‚/‘ c d/, "low-curly single quote");
ok("ab/cd" ~~ m/ab ｢/｣ c d/, "corner quote");
"#;
        let (stmts, _) = parse_program(src).unwrap();
        assert_eq!(stmts.len(), 4);
    }
}
