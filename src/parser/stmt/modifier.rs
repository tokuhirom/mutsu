use super::super::expr::expression;
use super::super::helpers::{ws, ws1};
use super::super::parse_result::{PError, PResult, merge_expected_messages};

use crate::ast::{CallArg, Expr, Stmt};
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::{keyword, parse_comma_or_expr};

fn rewrite_placeholder_block_modifier_stmt(stmt: Stmt, cond: &Expr) -> Stmt {
    if let Stmt::Block(body) = &stmt
        && let placeholders = crate::ast::collect_placeholders(body)
        && !placeholders.is_empty()
    {
        let mut rewritten = Vec::new();
        for (idx, name) in placeholders.into_iter().enumerate() {
            rewritten.push(Stmt::VarDecl {
                name,
                expr: if idx == 0 {
                    cond.clone()
                } else {
                    Expr::Literal(Value::Nil)
                },
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
            });
        }
        rewritten.extend(body.clone());
        return Stmt::Block(rewritten);
    }
    stmt
}

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
/// Supports chaining: `expr if cond for list` parses as `for list { expr if cond }`.
pub(crate) fn parse_statement_modifier(input: &str, stmt: Stmt) -> PResult<'_, Stmt> {
    let (rest, _) = ws(input)?;
    let mut current_stmt = stmt;
    let mut rest = rest;

    loop {
        // If there's a semicolon, the statement is terminated — no more modifiers
        if let Some(stripped) = rest.strip_prefix(';') {
            return Ok((stripped, current_stmt));
        }

        // If at end of input or block, return as-is
        if rest.is_empty() || rest.starts_with('}') {
            return Ok((rest, current_stmt));
        }

        match parse_single_modifier(rest, current_stmt.clone())? {
            Some((r, modified)) => {
                current_stmt = modified;
                let (r, _) = ws(r)?;
                rest = r;
            }
            None => {
                return Ok((rest, current_stmt));
            }
        }
    }
}

/// Try to parse a single statement modifier. Returns None if no modifier matched.
fn parse_single_modifier(rest: &str, stmt: Stmt) -> Result<Option<(&str, Stmt)>, PError> {
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
        let then_stmt = rewrite_placeholder_block_modifier_stmt(stmt, &cond);
        return Ok(Some((
            r,
            Stmt::If {
                cond,
                then_branch: vec![then_stmt],
                else_branch: Vec::new(),
                binding_var: None,
            },
        )));
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
        let then_stmt = rewrite_placeholder_block_modifier_stmt(stmt, &cond);
        return Ok(Some((
            r,
            Stmt::If {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                then_branch: vec![then_stmt],
                else_branch: Vec::new(),
                binding_var: None,
            },
        )));
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
        // Do not consume full loop headers as statement modifiers.
        // This preserves parsing of:
        //   { ... }
        //   for @values -> $v { ... }
        // as two statements, rather than block + postfix modifier + lambda.
        if r.starts_with("->") || r.starts_with('{') {
            return Ok(None);
        }
        return Ok(Some((
            r,
            Stmt::For {
                iterable,
                param: None,
                param_def: Box::new(None),
                params: Vec::new(),
                body: vec![stmt],
                label: None,
                mode: crate::ast::ForMode::Normal,
            },
        )));
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
        return Ok(Some((
            r,
            Stmt::While {
                cond,
                body: vec![stmt],
                label: None,
            },
        )));
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
        return Ok(Some((
            r,
            Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                body: vec![stmt],
                label: None,
            },
        )));
    }
    if let Some(r) = keyword("given", rest) {
        let (r, _) = ws1(r)?;
        let (r, topic) = parse_comma_or_expr(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected topic expression after 'given'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
        })?;
        let given_stmt = rewrite_placeholder_block_modifier_stmt(stmt, &topic);
        return Ok(Some((
            r,
            Stmt::Given {
                topic,
                body: vec![given_stmt],
            },
        )));
    }

    if let Some(r) = keyword("with", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let mut stmt_for_branch = stmt.clone();
        let mut r_tail = r;
        if let Stmt::Call { name, args } = &stmt_for_branch {
            let mut call_args = args.clone();
            loop {
                let (r_ws, _) = ws(r_tail)?;
                if !r_ws.starts_with(',') {
                    r_tail = r_ws;
                    break;
                }
                let after_comma = &r_ws[1..];
                let (after_comma, _) = ws(after_comma)?;
                let (after_comma, arg_expr) = expression(after_comma)?;
                call_args.push(CallArg::Positional(arg_expr));
                r_tail = after_comma;
            }
            stmt_for_branch = Stmt::Call {
                name: name.clone(),
                args: call_args,
            };
        }
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
                    quoted: false,
                },
                then_branch: vec![stmt_for_branch.clone()],
                else_branch: Vec::new(),
                binding_var: None,
            };
            let given_stmt = Stmt::Given {
                topic: cond,
                body: vec![Stmt::Expr(Expr::DoStmt(Box::new(if_stmt)))],
            };
            return Ok(Some((
                r_tail,
                Stmt::Expr(Expr::DoStmt(Box::new(given_stmt))),
            )));
        }
        let given_stmt = Stmt::Given {
            topic: cond,
            body: vec![Stmt::If {
                cond: Expr::MethodCall {
                    target: Box::new(Expr::Var("_".to_string())),
                    name: "defined".to_string(),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                },
                then_branch: vec![stmt_for_branch],
                else_branch: Vec::new(),
                binding_var: None,
            }],
        };
        return Ok(Some((r_tail, given_stmt)));
    }
    if let Some(r) = keyword("without", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        let mut stmt_for_branch = stmt.clone();
        let mut r_tail = r;
        if let Stmt::Call { name, args } = &stmt_for_branch {
            let mut call_args = args.clone();
            loop {
                let (r_ws, _) = ws(r_tail)?;
                if !r_ws.starts_with(',') {
                    r_tail = r_ws;
                    break;
                }
                let after_comma = &r_ws[1..];
                let (after_comma, _) = ws(after_comma)?;
                let (after_comma, arg_expr) = expression(after_comma)?;
                call_args.push(CallArg::Positional(arg_expr));
                r_tail = after_comma;
            }
            stmt_for_branch = Stmt::Call {
                name: name.clone(),
                args: call_args,
            };
        }
        // `stmt without expr` is like `given expr { unless .defined { stmt } }`.
        // Sets $_ to the condition value, then runs stmt if $_ is not defined.
        let not_defined = Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name: "defined".to_string(),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            }),
        };
        let modified_stmt = rewrite_placeholder_block_modifier_stmt(stmt_for_branch.clone(), &cond);
        if matches!(stmt, Stmt::Expr(_)) {
            let if_stmt = Stmt::If {
                cond: not_defined,
                then_branch: vec![modified_stmt],
                else_branch: Vec::new(),
                binding_var: None,
            };
            let given_stmt = Stmt::Given {
                topic: cond,
                body: vec![Stmt::Expr(Expr::DoStmt(Box::new(if_stmt)))],
            };
            return Ok(Some((
                r_tail,
                Stmt::Expr(Expr::DoStmt(Box::new(given_stmt))),
            )));
        }
        let given_stmt = Stmt::Given {
            topic: cond,
            body: vec![Stmt::If {
                cond: not_defined,
                then_branch: vec![modified_stmt],
                else_branch: Vec::new(),
                binding_var: None,
            }],
        };
        return Ok(Some((r_tail, given_stmt)));
    }

    Ok(None)
}
