use super::*;

/// Check if `say`/`print`/`put` is used bare (no arguments) — this is a compile error in Raku.
fn check_bare_io_func<'a>(name: &str, rest: &'a str) -> PResult<'a, ()> {
    let trimmed = rest.trim_start();
    if trimmed.is_empty() || trimmed.starts_with(';') || trimmed.starts_with('}') {
        // rakudo collects TWO complaints here: a *worry* explaining the bare
        // form, and then a panic because the argument list is missing. Two
        // collected complaints is exactly the condition for `X::Comp::Group`,
        // which is what roast/S16-io/bare-say.t asks for — a lone `X::Comp`
        // would be right only if rakudo had stopped at the worry.
        let advice = format!(
            "Unsupported use of bare \"{name}\". \
             In Raku please use: .{name} if you meant to call it as a method on $_, \
             or use an explicit invocant or argument, \
             or use &{name} to refer to the function as a noun."
        );
        let worry = Value::make_exception(
            "X::Comp::AdHoc",
            &[
                ("message", Value::str(advice.clone())),
                ("payload", Value::str(advice.clone())),
            ],
        );
        return Err(PError::comp_group(
            worry,
            true,
            &format!("Argument to \"{name}\" seems to be malformed"),
            advice,
        ));
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
                return Err(PError::obsolete(
                    &format!("bare \"{name}\""),
                    &format!(
                        ".{name} if you meant to call it as a method on $_, or use \
                         an explicit invocant or argument, or use &{name} to refer \
                         to the function as a noun"
                    ),
                ));
            }
        }
    }
    Ok((rest_after_ws, ()))
}

/// A `sub` named like an IO builtin (`say`/`print`/`put`/`note`) shadows the
/// builtin listop form in its lexical scope: `sub say(...) {...}; say $x` must
/// call the sub, not the builtin. Bail out of the builtin-statement parse so
/// statement dispatch falls through to the general listop-call parser.
///
/// An *imported* sub shadows it exactly as a locally-declared one does — the
/// lexical scope does not care where the binding came from. `Cro::HTTP::Router`
/// exports `put` (the HTTP verb), so `put -> 'product' { … }` inside a `route`
/// block is a route declaration, not a print.
fn shadowed_by_user_sub(name: &str) -> PResult<'_, ()> {
    if is_user_declared_sub(name) || is_imported_function(name) {
        return Err(PError::expected("io builtin shadowed by user sub"));
    }
    Ok(("", ()))
}

/// Parse a `say` statement.
pub(crate) fn say_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("say", input).ok_or_else(|| PError::expected("say statement"))?;
    shadowed_by_user_sub("say")?;
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
    shadowed_by_user_sub("print")?;
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
    shadowed_by_user_sub("put")?;
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
    shadowed_by_user_sub("note")?;
    // `note(...)` — parenthesis attached, no space — is an ordinary call, not the
    // listop statement. Bail out so statement dispatch reaches the general call
    // parser, exactly as `say(...)` does (`say_stmt` gets there by failing its
    // `ws1`). Without this, `note` fell through to the no-argument form below and
    // left `("hi")` behind as a separate statement, so `note("hi")` printed
    // "Noted" and warned about a string in sink context.
    if rest.starts_with('(') {
        return Err(PError::expected("note call with parentheses"));
    }
    // `note` with no arguments is valid (prints "Noted\n")
    if let Ok((rest2, _)) = ws1(rest) {
        // Check for colon invocant syntax: `note EXPR:` or `note EXPR: arg1, arg2`
        if let Ok((rest3, stmt)) = parse_io_colon_invocant_stmt(rest2, "note") {
            return parse_statement_modifier(rest3, stmt);
        }
        match parse_io_expr_list(rest2) {
            Ok((rest3, args)) => return parse_statement_modifier(rest3, Stmt::Note(args)),
            // A trailing word-logical bails the whole statement to the general
            // listop-call parser (`note 0 or die` must not become bare `note`).
            Err(err)
                if err
                    .messages
                    .iter()
                    .any(|m| m.contains("io listop followed by word logical")) =>
            {
                return Err(err);
            }
            Err(_) => {}
        }
    }
    // Bare `note` with no args
    parse_statement_modifier(rest, Stmt::Note(vec![]))
}

/// Parse a comma-separated expression list.
///
/// Items parse at list-prefix precedence: the loose word-logicals
/// (`and`/`or`/`andthen`/`orelse`/`xor`) are looser than an IO listop's
/// argument list, so they terminate it (`say 0 or die` is `(say 0) or die`,
/// not `say(0 or die)`).
pub(crate) fn parse_expr_list(input: &str) -> PResult<'_, Vec<Expr>> {
    let (input, first) = crate::parser::expr::listop_arg_expr_list_infix(input)?;
    let mut items = vec![first];
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        // Adjacent colonpairs without commas: `say :a :b, $x` / `say :a:b, $x`
        // are `say(:a, :b, $x)` — the argument list continues past them.
        if let Some((r2, arg)) = crate::parser::primary::ident::try_adjacent_colonpair_arg(r) {
            items.push(arg);
            rest = r2;
            continue;
        }
        if !r.starts_with(',') {
            let gap = &rest[..rest.len() - r.len()];
            let is_legitimate_continuation = gap.contains('\n')
                || r.is_empty()
                || r.starts_with(';')
                || r.starts_with('}')
                || r.starts_with(')')
                || is_stmt_modifier_keyword(r)
                || crate::parser::expr::parse_word_logical_op(r).is_some();
            // A completed argument directly followed by another unambiguous
            // term on the same line (`say 1 1`, `say "a" "b"`), with no
            // infix operator between them and no legitimate continuation
            // (comma, statement end, modifier, ...): "Two terms in a row".
            // Digits/quotes can never start an infix or a legitimate
            // continuation, so this is always fatal, unlike the softer
            // "missing comma" guess below.
            if !is_legitimate_continuation
                && crate::parser::term_boundary::starts_with_unambiguous_term(r)
            {
                return Err(PError::fatal_at(
                    "Confused. Two terms in a row".to_string(),
                    r,
                ));
            }
            if !is_legitimate_continuation
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
        let (r, next) = crate::parser::expr::listop_arg_expr_list_infix(r)?;
        items.push(next);
        rest = r;
    }
}

fn parse_io_expr_list(input: &str) -> PResult<'_, Vec<Expr>> {
    // A sequence operator (`...`/`…`) is looser than comma, so `say a, b ... limit`
    // is ONE sequence argument (seed `a, b`), not `a` plus `b ... limit`. Absorb the
    // whole comma level like the parenthesized-list parser does.
    if let Some(result) = crate::parser::primary::try_parse_sequence_arg_list(input) {
        let (rest, seq) = result?;
        return Ok((rest, vec![seq]));
    }
    match parse_expr_list(input) {
        // A trailing loose word-logical (`say 0 or die`) is looser than the
        // whole listop statement: bail out (non-fatal) so statement dispatch
        // falls through to the general listop-call parser, which parses the
        // call as an expression and leaves the operator to the statement level.
        Ok((rest, _)) if crate::parser::expr::parse_word_logical_op(rest).is_some() => {
            Err(PError::expected("io listop followed by word logical"))
        }
        // A top-level list-infix meta-op (`Z`/`X`) or `minmax` is looser than the
        // comma separating arguments, so `say 100, 200 Z+ 42, 23` is
        // `say((100,200) Z+ (42,23))` — the whole comma level is one operand. Lift
        // it across the argument list (mirrors the sequence-op absorption above and
        // the parenthesized-list finalizer).
        Ok((rest, items)) => Ok((
            rest,
            crate::parser::primary::lift_list_infix_in_arg_list(items),
        )),
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
    // `say :!d:r, "x"` is `say(:!d, :r, "x")`, not `(:!d).say(r, "x")`: a colon
    // that opens a colonpair, or that follows one, is never the invocant colon.
    // These are the same two guards `try_parse_no_paren_invocant_colon_call`
    // applies for the general listop form. `say $*OUT: "hi"` is unaffected — its
    // colon is followed by whitespace, not by a name or a sigil.
    if crate::parser::primary::ident::colon_starts_colonpair(rest_after_target)
        || crate::parser::primary::ident::expr_is_colonpair(&target)
    {
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
