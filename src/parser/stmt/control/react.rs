use super::*;

/// Parse `react` block.
/// Supports both `react { ... }` and `react whenever ... { ... }` (shorthand).
pub(crate) fn react_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("react", input).ok_or_else(|| PError::expected("react block"))?;
    let (rest, _) = ws(rest)?;
    // Try `react whenever ...` shorthand first
    if let Ok((rest2, whenever)) = whenever_stmt(rest) {
        return Ok((
            rest2,
            Stmt::React {
                body: vec![whenever],
            },
        ));
    }
    // `react { ... }` block form.
    if let Ok((rest, body)) = block(rest) {
        return Ok((rest, Stmt::React { body }));
    }
    // `react STATEMENT` blorst form (e.g. `react foo`). A `react` takes a
    // block-or-statement; when it's a bare statement, parse a single expression
    // as the body. This is mainly needed so that programs like
    // `sub foo { whenever ... }; react foo` parse at all — the actual
    // "whenever outside react/supply scope" compile error is diagnosed by the
    // post-parse whenever-scope check, matching rakudo.
    let (rest, expr) = expression(rest)?;
    Ok((
        rest,
        Stmt::React {
            body: vec![Stmt::Expr(expr)],
        },
    ))
}

/// Parse `whenever` block.
pub(crate) fn whenever_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("whenever", input).ok_or_else(|| PError::expected("whenever block"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, supply) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param) = if let Some(stripped) = rest.strip_prefix("->") {
        let (r, _) = ws(stripped)?;
        // Optional type constraint before the variable name
        // (`whenever $s -> Int $x { }`, `-> IO::Socket::Async:D $c { }`). Reuse
        // the signature type parser so qualified names and `:D`/`:U` smileys are
        // handled. The constraint is not enforced on the whenever binding (as in
        // the untyped form); we only consume it so the variable name parses.
        // Without this, a typed pointy param made `whenever_stmt` fail, so the
        // whole `whenever ... -> Type $x { ... }` fragmented into a bare
        // `whenever` word + a standalone pointy block, which then tripped the
        // out-of-scope-`whenever` check (SSH::LibSSH::Tunnel).
        let r = match crate::parser::stmt::sub_param::parse_type_constraint_expr(r) {
            Some((r2, _tc)) => {
                let (r2, _) = ws(r2)?;
                r2
            }
            None => r,
        };
        match var_name(r) {
            Ok((r, name)) => (r, Some(name)),
            // Type-only pointy block (`-> Int { }`) binds no variable.
            Err(_) => (r, None),
        }
    } else {
        (rest, None)
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::Whenever {
            supply,
            param,
            body,
        },
    ))
}
