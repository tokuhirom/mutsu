use super::*;

/// Check if `say`/`print`/`put` is used bare (no arguments) — this is a compile error in Raku.
fn check_bare_io_func<'a>(name: &str, rest: &'a str) -> PResult<'a, ()> {
    let trimmed = rest.trim_start();
    if trimmed.is_empty() || trimmed.starts_with(';') || trimmed.starts_with('}') {
        return Err(PError::fatal(format!(
            "X::Comp: Unsupported use of bare \"{}\". \
             In Raku please use: .{} if you meant to call it as a method on $_, \
             or use an explicit invocant or argument, \
             or use &{} to refer to the function as a noun.",
            name, name, name
        )));
    }
    Ok((rest, ()))
}

/// Check if `say`/`print`/`put` is followed by `for`/`while`/`until` — X::Obsolete error.
fn check_io_func_followed_by_loop<'a>(name: &str, rest_after_ws: &'a str) -> PResult<'a, ()> {
    for kw in &["for", "while", "until"] {
        if let Some(r) = keyword(kw, rest_after_ws) {
            let next_char = r.chars().next();
            if next_char.is_none()
                || next_char == Some(' ')
                || next_char == Some('\t')
                || next_char == Some('\n')
            {
                return Err(PError::fatal(format!(
                    "X::Obsolete: Unsupported use of bare \"{}\". \
                     In Raku please use: .{} if you meant to call it as a method on $_, \
                     or use an explicit invocant or argument, \
                     or use &{} to refer to the function as a noun.",
                    name, name, name
                )));
            }
        }
    }
    Ok((rest_after_ws, ()))
}

/// Parse a `say` statement.
pub(crate) fn say_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("say", input).ok_or_else(|| PError::expected("say statement"))?;
    check_bare_io_func("say", rest)?;
    let (rest, _) = ws1(rest)?;
    check_io_func_followed_by_loop("say", rest)?;
    if let Ok((rest, stmt)) = parse_io_colon_invocant_stmt(rest, "say") {
        return parse_statement_modifier(rest, stmt);
    }
    let (rest, args) = parse_io_expr_list(rest)?;
    let stmt = Stmt::Say(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `print` statement.
pub(crate) fn print_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("print", input).ok_or_else(|| PError::expected("print statement"))?;
    check_bare_io_func("print", rest)?;
    let (rest, _) = ws1(rest)?;
    check_io_func_followed_by_loop("print", rest)?;
    if let Ok((rest, stmt)) = parse_io_colon_invocant_stmt(rest, "print") {
        return parse_statement_modifier(rest, stmt);
    }
    let (rest, args) = parse_io_expr_list(rest)?;
    let stmt = Stmt::Print(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `put` statement.
pub(crate) fn put_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("put", input).ok_or_else(|| PError::expected("put statement"))?;
    check_bare_io_func("put", rest)?;
    let (rest, _) = ws1(rest)?;
    check_io_func_followed_by_loop("put", rest)?;
    if let Ok((rest, stmt)) = parse_io_colon_invocant_stmt(rest, "put") {
        return parse_statement_modifier(rest, stmt);
    }
    let (rest, args) = parse_io_expr_list(rest)?;
    let stmt = Stmt::Put(args);
    parse_statement_modifier(rest, stmt)
}

/// Parse a `note` statement.
pub(crate) fn note_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("note", input).ok_or_else(|| PError::expected("note statement"))?;
    // `note` with no arguments is valid (prints "Noted\n")
    if let Ok((rest2, _)) = ws1(rest) {
        // Check for colon invocant syntax: `note EXPR:` or `note EXPR: arg1, arg2`
        if let Ok((rest3, stmt)) = parse_io_colon_invocant_stmt(rest2, "note") {
            return parse_statement_modifier(rest3, stmt);
        }
        if let Ok((rest3, args)) = parse_io_expr_list(rest2) {
            return parse_statement_modifier(rest3, Stmt::Note(args));
        }
    }
    // Bare `note` with no args
    parse_statement_modifier(rest, Stmt::Note(vec![]))
}

/// Parse a comma-separated expression list.
pub(crate) fn parse_expr_list(input: &str) -> PResult<'_, Vec<Expr>> {
    let (input, first) = expression(input)?;
    let mut items = vec![first];
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        if !r.starts_with(',') {
            let gap = &rest[..rest.len() - r.len()];
            if !gap.contains('\n')
                && !r.is_empty()
                && !r.starts_with(';')
                && !r.starts_with('}')
                && !r.starts_with(')')
                && !is_stmt_modifier_keyword(r)
                && r.chars()
                    .next()
                    .is_some_and(crate::parser::helpers::is_raku_identifier_start)
            {
                return Err(PError::expected("comma or statement end after argument"));
            }
            return Ok((r, items));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        // Check for end of list
        if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
            return Ok((r, items));
        }
        // Check for statement modifier keywords
        if is_stmt_modifier_keyword(r) {
            return Ok((r, items));
        }
        let (r, next) = expression(r)?;
        items.push(next);
        rest = r;
    }
}

fn parse_io_expr_list(input: &str) -> PResult<'_, Vec<Expr>> {
    match parse_expr_list(input) {
        Ok(ok) => Ok(ok),
        Err(err)
            if err
                .messages
                .iter()
                .any(|msg| msg.contains("comma or statement end after argument")) =>
        {
            Err(PError::fatal(err.messages.first().cloned().unwrap_or_else(
                || "comma or statement end after argument".to_string(),
            )))
        }
        Err(err) => Err(err),
    }
}

fn parse_io_colon_invocant_stmt<'a>(input: &'a str, method_name: &str) -> PResult<'a, Stmt> {
    let (rest_after_target, target) = expression(input)?;
    let (rest_after_target, _) = ws(rest_after_target)?;
    if !rest_after_target.starts_with(':') || rest_after_target.starts_with("::") {
        return Err(PError::expected("io colon invocant call"));
    }
    let mut rest = &rest_after_target[1..];
    let (r, _) = ws(rest)?;
    rest = r;

    // Check for no-args form: `say EXPR:` with nothing after colon
    let args = if rest.is_empty()
        || rest.starts_with(';')
        || rest.starts_with('\n')
        || rest.starts_with('}')
        || rest.starts_with(')')
        || rest.starts_with('#')
        || is_stmt_modifier_keyword(rest)
    {
        vec![]
    } else {
        let (first_rest, first_arg) = expression(rest).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected expression after ':' in io invocant call",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(rest.len())),
            exception: None,
        })?;
        let mut args = vec![first_arg];
        rest = first_rest;
        loop {
            let (r, _) = ws(rest)?;
            if !r.starts_with(',') {
                break;
            }
            let r = &r[1..];
            let (r, _) = ws(r)?;
            if r.starts_with(';')
                || r.is_empty()
                || r.starts_with('}')
                || r.starts_with(')')
                || is_stmt_modifier_keyword(r)
            {
                break;
            }
            let (r, next) = expression(r)?;
            args.push(next);
            rest = r;
        }
        args
    };
    Ok((
        rest,
        Stmt::Expr(Expr::MethodCall {
            target: Box::new(target),
            name: Symbol::intern(method_name),
            args,
            modifier: None,
            quoted: false,
        }),
    ))
}
