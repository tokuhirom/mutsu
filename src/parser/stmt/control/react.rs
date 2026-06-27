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
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::React { body }))
}

/// Parse `whenever` block.
pub(crate) fn whenever_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("whenever", input).ok_or_else(|| PError::expected("whenever block"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, supply) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param) = if let Some(stripped) = rest.strip_prefix("->") {
        let (r, _) = ws(stripped)?;
        let (r, name) = var_name(r)?;
        (r, Some(name))
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
