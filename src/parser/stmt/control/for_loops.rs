use super::*;

/// Reject `foreach` with X::Obsolete error (Perl 5 remnant).
pub(crate) fn foreach_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest =
        keyword("foreach", input).ok_or_else(|| PError::expected("foreach (obsolete) check"))?;
    // If followed by ' or - and then a letter, it's actually an identifier
    // like `foreach'rest` or `foreach-bar`, not the obsolete keyword.
    if let Some(rest2) = rest.strip_prefix('\'').or_else(|| rest.strip_prefix('-'))
        && rest2.starts_with(|c: char| c.is_alphabetic() || c == '_')
    {
        return Err(PError::expected("foreach (obsolete) check"));
    }
    // `foreach(...)` or `foreach;` with no intervening whitespace could be a
    // call of a user-defined `sub foreach {}` (see #2440). The obsolete loop
    // form always has whitespace before its term (`foreach (1..10) { }`,
    // `foreach @x { }`), so only bail when `(`/`;` follow immediately.
    if rest.starts_with('(') || rest.starts_with(';') || rest.trim_start().is_empty() {
        return Err(PError::expected("foreach (obsolete) check"));
    }
    Err(make_obsolete_error(
        "'foreach'",
        Some("'for'"),
        "X::Obsolete: Unsupported use of 'foreach'. In Raku please use: 'for'.",
    ))
}

/// Return true if `expr` is (or ends in) a bare brace block — used to detect a
/// block gobbled by a comma list / list-op, e.g. the trailing `{ say 3 }` in
/// `for 1, 2, 3, { say 3 }`. A brace block parses as `AnonSub`/`AnonSubParams`/
/// `Block`; in a comma list it lands as the final element of an `ArrayLiteral`.
fn expr_ends_with_block(expr: &Expr) -> bool {
    match expr {
        Expr::AnonSub { .. } | Expr::AnonSubParams { .. } | Expr::Block(_) => true,
        Expr::ArrayLiteral(items) => items.last().is_some_and(expr_ends_with_block),
        _ => false,
    }
}

/// Given input starting at `(`, return true if its matching `)` group contains
/// a top-level `;` (the C-style `for (init; test; incr)` obsolete form).
fn paren_has_toplevel_semicolon(input: &str) -> bool {
    let mut depth = 0i32;
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => {
                depth -= 1;
                if depth == 0 {
                    return false;
                }
            }
            ';' if depth == 1 => return true,
            // Skip a backslash-escaped char so an escaped delimiter is ignored.
            '\\' => {
                chars.next();
            }
            _ => {}
        }
    }
    false
}

pub(crate) fn for_stmt(input: &str) -> PResult<'_, Stmt> {
    for_stmt_with_mode(input, crate::ast::ForMode::Normal)
}

/// Parse `race for ...` statement prefix.
pub(crate) fn race_for_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("race", input).ok_or_else(|| PError::expected("race for statement"))?;
    let (rest, _) = ws1(rest)?;
    for_stmt_with_mode(rest, crate::ast::ForMode::Race)
}

/// Parse `hyper for ...` statement prefix.
pub(crate) fn hyper_for_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("hyper", input).ok_or_else(|| PError::expected("hyper for statement"))?;
    let (rest, _) = ws1(rest)?;
    for_stmt_with_mode(rest, crate::ast::ForMode::Hyper)
}

/// Parse `for ...` with Lazy mode (input starts at `for`, not `lazy`).
pub(crate) fn lazy_for_body(input: &str) -> PResult<'_, Stmt> {
    for_stmt_with_mode(input, crate::ast::ForMode::Lazy)
}

/// Scan for `<->` (rw pointy block) in the input, returning its byte offset.
/// This must handle nesting to avoid matching inside strings, brackets, etc.
fn find_rw_pointy_block(input: &str) -> Option<usize> {
    let bytes = input.as_bytes();
    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let depth_angle = 0i32;
    let mut in_single_quote = false;
    let mut in_double_quote = false;
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        if in_single_quote {
            if b == b'\'' {
                in_single_quote = false;
            } else if b == b'\\' {
                i += 1; // skip escaped char
            }
            i += 1;
            continue;
        }
        if in_double_quote {
            if b == b'"' {
                in_double_quote = false;
            } else if b == b'\\' {
                i += 1;
            }
            i += 1;
            continue;
        }
        match b {
            b'\'' => in_single_quote = true,
            b'"' => in_double_quote = true,
            b'(' => depth_paren += 1,
            b')' => depth_paren -= 1,
            b'[' => depth_bracket += 1,
            b']' => depth_bracket -= 1,
            b'{' if depth_paren == 0 && depth_bracket == 0 && depth_angle == 0 => {
                // Start of block — stop scanning
                return None;
            }
            // Check for `<->`
            b'<' if depth_paren == 0
                && depth_bracket == 0
                && depth_angle == 0
                && i + 2 < bytes.len()
                && bytes[i + 1] == b'-'
                && bytes[i + 2] == b'>' =>
            {
                return Some(i);
            }
            _ => {}
        }
        i += 1;
    }
    None
}

fn for_stmt_with_mode(input: &str, mode: crate::ast::ForMode) -> PResult<'_, Stmt> {
    let rest = keyword("for", input).ok_or_else(|| PError::expected("for statement"))?;
    let (rest, _) = ws1(rest)?;
    // Perl 5 `for my $x (LIST) { }` foreach syntax.
    if looks_like_p5_foreach(rest) {
        return Err(p5_foreach_error());
    }
    // C-style `for (init; test; incr) { }` is obsolete — Raku uses `loop`.
    // Detected by a top-level `;` inside the parenthesized iterable.
    if rest.starts_with('(') && paren_has_toplevel_semicolon(rest) {
        return Err(make_obsolete_error(
            "C-style \"for\"",
            Some("\"loop\""),
            "X::Obsolete: Unsupported use of C-style \"for\". In Raku please use: \"loop\".",
        ));
    }
    // Try to detect `<->` (rw pointy block) before the expression parser
    // consumes the `<` as a comparison operator.
    let (rest, iterable, rw_detected) = if let Some(rw_pos) = find_rw_pointy_block(rest) {
        let expr_part = &rest[..rw_pos];
        let (leftover, iterable) = parse_comma_or_expr(expr_part)?;
        let (leftover, _) = ws(leftover)?;
        if leftover.is_empty() {
            (&rest[rw_pos..], iterable, true)
        } else {
            // Expression didn't consume everything before `<->`, fall back
            let (r, iterable) = parse_comma_or_expr(rest)?;
            (r, iterable, false)
        }
    } else {
        let (r, iterable) = parse_comma_or_expr(rest)?;
        (r, iterable, false)
    };
    let (rest, _) = ws(rest)?;
    let (rest, (param, param_def, params, params_def, rw_block, explicit_zero_params)) =
        parse_for_params(rest)?;
    let rw_block = rw_block || rw_detected;
    let (rest, _) = ws(rest)?;
    // A `for` loop always requires a brace block. Having parsed the iterable and
    // any pointy parameters, if no `{` follows then the block is missing
    // (`for 1, 2` → X::Syntax::Missing, what => 'block'). Fail fatally so the
    // statement dispatcher does not fall back to reparsing `for` as a term.
    if !rest.starts_with('{') {
        let panic = Value::make_exception(
            "X::Syntax::Missing",
            &[
                ("what", Value::str("block".to_string())),
                ("message", Value::str("Missing block".to_string())),
            ],
        );
        // When the iterable expression itself ended with a brace block (e.g.
        // `for 1, 2, 3, { say 3 }`), the block was gobbled by the comma list, so
        // raku reports an additional X::Syntax::BlockGobbled sorrow alongside the
        // X::Syntax::Missing panic, bundled in an X::Comp::Group.
        if expr_ends_with_block(&iterable) {
            let sorrow = Value::make_exception(
                "X::Syntax::BlockGobbled",
                &[(
                    "message",
                    Value::str("Expression needs parens to avoid gobbling block".to_string()),
                )],
            );
            let group = Value::make_comp_group(
                "Expression needs parens to avoid gobbling block\nMissing block (apparently claimed by expression)".to_string(),
                Some(panic),
                vec![sorrow],
                vec![],
            );
            return Err(PError::fatal_with_exception(
                "X::Comp::Group: Missing block".to_string(),
                Box::new(group),
            ));
        }
        return Err(PError::fatal_with_exception(
            "X::Syntax::Missing: Missing block".to_string(),
            Box::new(panic),
        ));
    }
    // Collect &-sigil parameter names so they can be registered as user subs
    // inside the block scope — this prevents `m()` from being misread as `m//`
    // (a regex match) when `m` is bound via `-> &m { m() }`.
    // Also collect sigilless `\name` params (stored with a leading `\`) so they
    // can be registered as term symbols, shadowing any outer `&name` sub — keeps
    // `my &mapper = {...}; for ... -> \mapper { mapper.WHAT }` reading `mapper` as
    // the loop variable, not as a call to the outer `&mapper`.
    let mut code_param_names: Vec<String> = Vec::new();
    let mut sigilless_param_names: Vec<String> = Vec::new();
    for p in param.iter().chain(params.iter()) {
        if let Some(bare) = p.strip_prefix('&') {
            code_param_names.push(bare.to_string());
        } else if let Some(bare) = p.strip_prefix('\\') {
            sigilless_param_names.push(bare.to_string());
        }
    }
    // The single-param form stores a sigilless `\name` without the leading `\`
    // (multi-param form keeps it), so consult the ParamDef sigilless flags too.
    for pd in param_def.iter().chain(params_def.iter()) {
        if pd.sigilless && !sigilless_param_names.contains(&pd.name) {
            sigilless_param_names.push(pd.name.clone());
        }
    }
    let (rest, body) = if !code_param_names.is_empty() || !sigilless_param_names.is_empty() {
        let (r, _) = parse_char(rest, '{')?;
        super::super::simple::push_scope();
        for name in &code_param_names {
            super::super::simple::register_user_sub(name);
        }
        for name in &sigilless_param_names {
            super::super::simple::register_user_term_symbol(name);
        }
        let result = block_inner(r);
        super::super::simple::pop_scope();
        result?
    } else {
        block(rest)?
    };
    // When no explicit params, collect placeholder variables from the body
    let (param, params) = if param.is_none() && params.is_empty() {
        let placeholders = collect_placeholders_shallow(&body);
        if placeholders.is_empty() {
            (param, params)
        } else {
            // Use the first placeholder as the loop param, rest as extra params
            let first = placeholders[0].clone();
            let rest_params = placeholders[1..].to_vec();
            (Some(first), rest_params)
        }
    } else {
        (param, params)
    };
    Ok((
        rest,
        Stmt::For {
            iterable,
            param,
            param_def: Box::new(param_def),
            params,
            params_def,
            body,
            label: None,
            mode,
            rw_block,
            explicit_zero_params,
        },
    ))
}
