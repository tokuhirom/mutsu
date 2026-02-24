use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PError, PResult, merge_expected_messages, opt_char};

use crate::ast::{Expr, Stmt};
use crate::token_kind::TokenKind;

use super::keyword;

pub(super) fn is_stmt_modifier_keyword(input: &str) -> bool {
    for kw in &[
        "if", "unless", "for", "while", "until", "given", "when", "with", "without",
    ] {
        if keyword(kw, input).is_some() {
            return true;
        }
    }
    false
}

/// Parse statement modifier (postfix if/unless/for/while/until/given/when).
pub(crate) fn parse_statement_modifier(input: &str, stmt: Stmt) -> PResult<'_, Stmt> {
    let (rest, _) = ws(input)?;

    // If there's a semicolon, the statement is terminated — no modifiers
    if let Some(stripped) = rest.strip_prefix(';') {
        return Ok((stripped, stmt));
    }

    // If at end of input or block, return as-is
    if rest.is_empty() || rest.starts_with('}') {
        return Ok((rest, stmt));
    }

    // Try statement modifiers
    if let Some(r) = keyword("if", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'if'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
        })?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::If {
                cond,
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            },
        ));
    }
    if let Some(r) = keyword("unless", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'unless'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
        })?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::If {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            },
        ));
    }
    // Check for do { } with loop modifiers (X::Obsolete in Raku)
    let is_do_block = matches!(
        &stmt,
        Stmt::Expr(Expr::DoBlock { .. }) | Stmt::Expr(Expr::DoStmt(_))
    );
    if is_do_block {
        for kw in &["while", "until", "for", "given"] {
            if keyword(kw, rest).is_some() {
                return Err(PError::raw(
                    format!(
                        "X::Obsolete: Unsupported use of do...{kw}. In Raku please use: repeat...while or repeat...until."
                    ),
                    Some(rest.len()),
                ));
            }
        }
    }
    if let Some(r) = keyword("for", rest) {
        let (r, _) = ws1(r)?;
        let (r, first) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected iterable expression after 'for'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
        })?;
        // Parse comma-separated list for `for` modifier: `expr for 1, 2, 3`
        let (r, iterable) = {
            let mut items = vec![first];
            let mut r = r;
            loop {
                let (r2, _) = ws(r)?;
                if !r2.starts_with(',') {
                    break;
                }
                let r2 = &r2[1..];
                let (r2, _) = ws(r2)?;
                let (r2, next) = expression(r2)?;
                items.push(next);
                r = r2;
            }
            if items.len() == 1 {
                (r, items.into_iter().next().unwrap())
            } else {
                (r, Expr::ArrayLiteral(items))
            }
        };
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::For {
                iterable,
                param: None,
                param_def: Box::new(None),
                params: Vec::new(),
                body: vec![stmt],
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'while'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
        })?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::While {
                cond,
                body: vec![stmt],
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'until'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
        })?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                body: vec![stmt],
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("given", rest) {
        let (r, _) = ws1(r)?;
        let (r, topic) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected topic expression after 'given'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
        })?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Given {
                topic,
                body: vec![stmt],
            },
        ));
    }

    if let Some(r) = keyword("with", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        // `stmt with expr` is like `given expr { if .defined { stmt } }`.
        // When the modified statement is an expression statement, preserve
        // expression semantics via do-given.
        if matches!(stmt, Stmt::Expr(_)) {
            let if_stmt = Stmt::If {
                cond: Expr::MethodCall {
                    target: Box::new(Expr::Var("_".to_string())),
                    name: "defined".to_string(),
                    args: Vec::new(),
                    modifier: None,
                },
                then_branch: vec![stmt.clone()],
                else_branch: Vec::new(),
            };
            let given_stmt = Stmt::Given {
                topic: cond,
                body: vec![Stmt::Expr(Expr::DoStmt(Box::new(if_stmt)))],
            };
            return Ok((r, Stmt::Expr(Expr::DoStmt(Box::new(given_stmt)))));
        }
        let given_stmt = Stmt::Given {
            topic: cond,
            body: vec![Stmt::If {
                cond: Expr::MethodCall {
                    target: Box::new(Expr::Var("_".to_string())),
                    name: "defined".to_string(),
                    args: Vec::new(),
                    modifier: None,
                },
                then_branch: vec![stmt.clone()],
                else_branch: Vec::new(),
            }],
        };
        return Ok((r, given_stmt));
    }
    if let Some(r) = keyword("without", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::If {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::MethodCall {
                        target: Box::new(cond),
                        name: "defined".to_string(),
                        args: Vec::new(),
                        modifier: None,
                    }),
                },
                then_branch: vec![stmt],
                else_branch: Vec::new(),
            },
        ));
    }

    Ok((rest, stmt))
}
