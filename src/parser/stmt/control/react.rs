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
    let (rest, param, param_type) = if let Some(stripped) = rest.strip_prefix("->") {
        let (r, _) = ws(stripped)?;
        // Optional type constraint before the variable name
        // (`whenever $s -> Int $x { }`, `-> IO::Socket::Async:D $c { }`). Reuse
        // the signature type parser so qualified names and `:D`/`:U` smileys are
        // handled, and carried through so the binding can enforce it (as an
        // ordinary typed block parameter would) — see
        // news/2026-08/whenever-parameter-type-constraint-enforced.md.
        // Without consuming it here, a typed pointy param made `whenever_stmt`
        // fail, so the whole `whenever ... -> Type $x { ... }` fragmented into a
        // bare `whenever` word + a standalone pointy block, which then tripped
        // the out-of-scope-`whenever` check (SSH::LibSSH::Tunnel).
        let (r, param_type) = match crate::parser::stmt::sub_param::parse_type_constraint_expr(r) {
            Some((r2, tc)) => {
                let (r2, _) = ws(r2)?;
                (r2, Some(tc))
            }
            None => (r, None),
        };
        match var_name(r) {
            Ok((r, name)) => (r, Some(name), param_type),
            Err(_) => {
                // Sigilless pointy param (`whenever $ch -> \row { }`,
                // Text::CSV's Channel/Supply in-format loops): binds the raw
                // value under the bare name — the same env key a sigil-less
                // read resolves. Without this the whole statement failed to
                // parse and fragmented into a bare `whenever` word plus a
                // standalone pointy block, so the subscription never
                // registered and the react saw zero events.
                if let Some(stripped) = r.strip_prefix('\\')
                    && let Ok((r2, name)) = crate::parser::stmt::idents::ident(stripped)
                {
                    (r2, Some(name), param_type)
                } else {
                    // Type-only pointy block (`-> Int { }`) binds no variable.
                    (r, None, param_type)
                }
            }
        }
    } else {
        (rest, None, None)
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::Whenever {
            supply,
            param,
            param_type,
            body,
        },
    ))
}
