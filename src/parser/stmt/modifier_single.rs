use super::*;

pub(super) fn parse_single_modifier(
    rest: &str,
    stmt: Stmt,
) -> Result<Option<(&str, Stmt)>, PError> {
    // Try statement modifiers
    if let Some(r) = keyword("if", rest) {
        let (r, _) = ws1(r)?;
        let cond_input = r;
        let (r, cond) = parse_comma_or_expr(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'if'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        check_two_terms_across_lines(cond_input, r)?;
        let then_stmt = rewrite_placeholder_block_modifier_stmt(stmt, &cond);
        if let Some(split) =
            conditional_declaration(&then_stmt, &cond, false).map_err(PError::from_typed)?
        {
            return Ok(Some((r, split)));
        }
        return Ok(Some((
            r,
            Stmt::If {
                cond,
                then_branch: vec![then_stmt],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            },
        )));
    }
    if let Some(r) = keyword("unless", rest) {
        let (r, _) = ws1(r)?;
        let cond_input = r;
        let (r, cond) = parse_comma_or_expr(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'unless'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        check_two_terms_across_lines(cond_input, r)?;
        let then_stmt = rewrite_placeholder_block_modifier_stmt(stmt, &cond);
        if let Some(split) =
            conditional_declaration(&then_stmt, &cond, true).map_err(PError::from_typed)?
        {
            return Ok(Some((r, split)));
        }
        let neg_cond = Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cond),
            word: false,
        };
        return Ok(Some((
            r,
            Stmt::If {
                cond: neg_cond,
                then_branch: vec![then_stmt],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: true,
                with_kind: None,
            },
        )));
    }
    if keyword("for", rest).is_some() {
        return parse_for_modifier(rest, stmt);
    }
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected condition expression after 'while'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        return Ok(Some((
            r,
            Stmt::While {
                cond,
                body: vec![while_modifier_operand(stmt)],
                label: None,
                is_statement_modifier: true,
                is_until: false,
                is_bare_term: false,
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
            exception: None,
        })?;
        return Ok(Some((
            r,
            Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                    word: false,
                },
                body: vec![while_modifier_operand(stmt)],
                label: None,
                is_statement_modifier: true,
                is_until: true,
                is_bare_term: false,
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
            exception: None,
        })?;
        let given_stmt = rewrite_placeholder_block_modifier_stmt(stmt, &topic);
        return Ok(Some((
            r,
            Stmt::Given {
                topic,
                body: vec![given_stmt],
                is_statement_modifier: true,
                with_kind: None,
            },
        )));
    }

    if let Some(r) = keyword("when", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = parse_comma_or_expr(r).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected match expression after 'when'",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?;
        // `When` signals a match by raising `succeed`, which only an enclosing topicalizer
        // catches. Wrapping in a `given $_` gives it that catcher without changing the topic,
        // and composes with a following `given` modifier, which re-topicalizes `$_` first.
        return Ok(Some((
            r,
            Stmt::Given {
                topic: Expr::Var("_".to_string()),
                body: vec![Stmt::When {
                    cond,
                    body: vec![stmt],
                    is_statement_modifier: true,
                }],
                is_statement_modifier: true,
                with_kind: None,
            },
        )));
    }

    if let Some(r) = keyword("with", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        // Do not consume a full `with EXPR -> $param { ... }` block header
        // as a statement modifier. This preserves parsing of:
        //   subtest 'sub' => { ... }
        //   with make-temp-dir() -> $dir { ... }
        // as two statements, rather than stmt + postfix modifier + lambda.
        let (r_ws, _) = ws(r)?;
        if r_ws.starts_with("->") || r_ws.starts_with('{') {
            return Ok(None);
        }
        let mut stmt_for_branch = stmt.clone();
        let mut r_tail = r;
        if let Stmt::Call { name, args, listop } = &stmt_for_branch {
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
                name: *name,
                args: call_args,
                listop: *listop,
            };
        }
        // `stmt with expr` is like `given expr { if .defined { stmt } }`.
        // When the statement is a block with placeholders, rewrite them.
        let stmt_for_branch =
            rewrite_placeholder_block_modifier_stmt(stmt_for_branch, &Expr::Var("_".to_string()));
        // When the modified statement is an expression statement, preserve
        // expression semantics via do-given.
        if matches!(stmt, Stmt::Expr(_)) {
            let if_stmt = Stmt::If {
                cond: Expr::MethodCall {
                    target: Box::new(Expr::Var("_".to_string())),
                    name: Symbol::intern("defined"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                    sugar: false,
                },
                then_branch: vec![stmt_for_branch],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            };
            let given_stmt = Stmt::Given {
                topic: cond,
                body: vec![Stmt::Expr(Expr::DoStmt(Box::new(if_stmt)))],
                is_statement_modifier: true,
                with_kind: Some(GivenWithKind::With),
            };
            return Ok(Some((
                r_tail,
                Stmt::Expr(Expr::DoStmt(Box::new(given_stmt))),
            )));
        }
        if let Some(expanded) =
            topical_declaration(&stmt_for_branch, &cond, false).map_err(PError::from_typed)?
        {
            return Ok(Some((r_tail, expanded)));
        }
        let given_stmt = Stmt::Given {
            topic: cond,
            body: vec![Stmt::If {
                cond: Expr::MethodCall {
                    target: Box::new(Expr::Var("_".to_string())),
                    name: Symbol::intern("defined"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                    sugar: false,
                },
                then_branch: vec![stmt_for_branch],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            }],
            is_statement_modifier: true,
            with_kind: Some(GivenWithKind::With),
        };
        return Ok(Some((r_tail, given_stmt)));
    }
    if let Some(r) = keyword("without", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = expression(r)?;
        // Same as `with` — do not consume a full block header as modifier.
        let (r_ws, _) = ws(r)?;
        if r_ws.starts_with("->") || r_ws.starts_with('{') {
            return Ok(None);
        }
        let mut stmt_for_branch = stmt.clone();
        let mut r_tail = r;
        if let Stmt::Call { name, args, listop } = &stmt_for_branch {
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
                name: *name,
                args: call_args,
                listop: *listop,
            };
        }
        // `stmt without expr` is like `given expr { unless .defined { stmt } }`.
        // Sets $_ to the condition value, then runs stmt if $_ is not defined.
        let not_defined = Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(Expr::MethodCall {
                target: Box::new(Expr::Var("_".to_string())),
                name: Symbol::intern("defined"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
                sugar: false,
            }),
            word: false,
        };
        let modified_stmt = rewrite_placeholder_block_modifier_stmt(stmt_for_branch, &cond);
        if matches!(stmt, Stmt::Expr(_)) {
            let if_stmt = Stmt::If {
                cond: not_defined,
                then_branch: vec![modified_stmt],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            };
            let given_stmt = Stmt::Given {
                topic: cond,
                body: vec![Stmt::Expr(Expr::DoStmt(Box::new(if_stmt)))],
                is_statement_modifier: true,
                with_kind: Some(GivenWithKind::Without),
            };
            return Ok(Some((
                r_tail,
                Stmt::Expr(Expr::DoStmt(Box::new(given_stmt))),
            )));
        }
        if let Some(expanded) =
            topical_declaration(&modified_stmt, &cond, true).map_err(PError::from_typed)?
        {
            return Ok(Some((r_tail, expanded)));
        }
        let given_stmt = Stmt::Given {
            topic: cond,
            body: vec![Stmt::If {
                cond: not_defined,
                then_branch: vec![modified_stmt],
                else_branch: Vec::new(),
                binding_var: None,
                is_statement_modifier: true,
                is_unless: false,
                with_kind: None,
            }],
            is_statement_modifier: true,
            with_kind: Some(GivenWithKind::Without),
        };
        return Ok(Some((r_tail, given_stmt)));
    }

    Ok(None)
}
