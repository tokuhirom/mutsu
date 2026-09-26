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
    // A pointy block (`-> $x`, `-> Int $x`, `-> \row`, `-> ($cmd, $arg?)`,
    // `-> $a, $b`, ...) is parsed by the ordinary pointy-block parser, so a
    // `whenever` accepts every signature a `.tap(-> ... { })` block does.
    // A hand-rolled single-parameter parser here used to reject anything
    // else (a sub-signature like Temp::Path's `-> ($_, $path?)`), and the
    // statement then fragmented into a bare `whenever` word + a standalone
    // pointy block, tripping the out-of-scope-`whenever` check.
    if rest.starts_with("->") || rest.starts_with("<->") {
        let (rest, lambda) = crate::parser::primary::arrow_lambda_pub(rest)?;
        let (params, param_defs, body) = match lambda {
            Expr::Lambda { param, body, .. } => (vec![param], Vec::new(), body),
            Expr::AnonSubParams {
                params,
                param_defs,
                body,
                ..
            } => (params, param_defs, body),
            _ => return Err(PError::expected("whenever pointy block")),
        };
        return Ok((
            rest,
            Stmt::Whenever {
                supply,
                params,
                param_defs,
                body,
            },
        ));
    }
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::Whenever {
            supply,
            params: Vec::new(),
            param_defs: Vec::new(),
            body,
        },
    ))
}
