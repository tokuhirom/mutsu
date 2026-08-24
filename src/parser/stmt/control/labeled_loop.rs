use super::*;

/// Parse labeled statement: `LABEL: <statement>`.
/// Loop-like statements keep the label on the loop node for last/next/redo.
/// Other statements are wrapped in `Stmt::Label`.
pub(crate) fn labeled_loop_stmt(input: &str) -> PResult<'_, Stmt> {
    let (rest, label) = ident(input)?;
    // A statement label requires the colon immediately after the identifier
    // (`LABEL:`). Whitespace before the colon means this is not a label but a
    // call with a colonpair/object-hash argument (e.g. `is-deeply :{...}, ...`).
    if !rest.starts_with(':') || rest.starts_with("::") {
        return Err(PError::expected("labeled loop"));
    }
    let rest = &rest[1..]; // consume ':'
    let has_space_after_colon = rest.chars().next().is_some_and(char::is_whitespace);
    if !has_space_after_colon
        && !rest.starts_with('{')
        && !rest.starts_with("for")
        && !rest.starts_with("while")
        && !rest.starts_with("until")
        && !rest.starts_with("loop")
        && !rest.starts_with("repeat")
        && !rest.starts_with("do")
    {
        return Err(PError::expected("labeled loop"));
    }
    let (rest, _) = ws(rest)?;

    // Check which loop keyword follows
    if let Some(r) = keyword("for", rest) {
        let (r, _) = ws1(r)?;
        // Perl 5 `for my $x (LIST) { }` foreach syntax.
        if looks_like_p5_foreach(r) {
            return Err(p5_foreach_error());
        }
        let (r, iterable) = parse_comma_or_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, (param, param_def, params, params_def, rw_block, explicit_zero_params)) =
            parse_for_params(r)?;
        let (r, _) = ws(r)?;
        let block_input = r;
        let (r, body) = block(r)?;
        let uses_block_magic = block_input[..block_input.len() - r.len()].contains("&?BLOCK");
        return Ok((
            r,
            Stmt::For {
                iterable,
                param,
                param_def: Box::new(param_def),
                params,
                params_def,
                body,
                label: Some(label),
                mode: crate::ast::ForMode::Normal,
                rw_block,
                explicit_zero_params,
                is_statement_modifier: false,
                uses_block_magic,
            },
        ));
    }
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::While {
                cond,
                body,
                label: Some(label),
            },
        ));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::While {
                cond: Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                },
                body,
                label: Some(label),
            },
        ));
    }
    if keyword("loop", rest).is_some() {
        // Delegate to `loop_stmt` so both the C-style `loop (init; cond; step)
        // { ... }` header and the infinite `loop { ... }` form are handled, then
        // stamp the label onto the returned loop node.
        let (r, stmt) = loop_stmt(rest)?;
        if let Stmt::Loop {
            init,
            cond,
            step,
            body,
            repeat,
            ..
        } = stmt
        {
            return Ok((
                r,
                Stmt::Loop {
                    init,
                    cond,
                    step,
                    body,
                    repeat,
                    label: Some(label),
                },
            ));
        }
        return Ok((r, stmt));
    }
    if keyword("repeat", rest).is_some() {
        // Delegate to `repeat_stmt` so both `repeat while/until` and
        // `repeat { ... } while/until` forms are handled, then stamp the
        // label onto the returned loop node.
        let (r, stmt) = repeat_stmt(rest)?;
        if let Stmt::Loop {
            init,
            cond,
            step,
            body,
            repeat,
            ..
        } = stmt
        {
            return Ok((
                r,
                Stmt::Loop {
                    init,
                    cond,
                    step,
                    body,
                    repeat,
                    label: Some(label),
                },
            ));
        }
        return Ok((r, stmt));
    }

    // Label before `do` block: `A: do { ... }`, and label before bare block:
    // `A: { ... }`. Neither is a loop construct in rakudo — `last`/`next`/
    // `redo A` inside either must raise `X::ControlFlow` ("labeled ... without
    // loop construct"), not iterate or silently exit — so both lower to a
    // labelled `Expr::DoBlock` (the same node plain expression-position
    // `do { ... }` uses), not a loop, which already has exactly that
    // not-a-loop-but-`leave`-targetable behaviour
    // (`vm/vm_misc_block.rs::exec_do_block_expr_op`).
    // Used to lower to a dummy `Stmt::For` carrying `Nil`, which made both
    // forms wrongly behave as a one-iteration loop
    // (`todo/tickets/labelled-bare-block-is-not-a-loop-construct.md`).
    if keyword("do", rest).is_some() {
        let r = keyword("do", rest).unwrap();
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        return Ok((
            r,
            Stmt::Expr(Expr::DoBlock {
                body,
                label: Some(label),
            }),
        ));
    }
    if rest.starts_with('{') {
        let (r, body) = block(rest)?;
        return Ok((
            r,
            Stmt::Expr(Expr::DoBlock {
                body,
                label: Some(label),
            }),
        ));
    }

    // `foo: args` where `foo` is a declared subroutine is a colon-listop call,
    // not a label (Raku: `trim: "x"` calls `trim("x")`, while `LOOP: for ...`
    // is a label). The loop/block label forms are handled above; only the
    // generic `IDENT: <expr>` case is ambiguous, and a declared-sub name
    // resolves it to a call. Parse the colon-listop argument list directly.
    if crate::parser::stmt::simple::is_user_declared_sub(&label) {
        let (r, call) = crate::parser::primary::ident::parse_expr_listop_args(rest, label.clone())?;
        return Ok((r, Stmt::Expr(call)));
    }

    // Generic labeled statement: LABEL: <statement>
    let (r, stmt) = statement(rest)?;
    Ok((
        r,
        Stmt::Label {
            name: label,
            stmt: Box::new(stmt),
        },
    ))
}
