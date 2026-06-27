use super::*;

/// Parse `given`/`when`/`default`.
pub(crate) fn given_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("given", input).ok_or_else(|| PError::expected("given statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, topic) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    // Check for pointy block: given EXPR -> $param { ... }
    let (rest, pointy_param) = if let Some(r) = rest.strip_prefix("->") {
        let (r, _) = ws(r)?;
        let (r, pd) = parse_pointy_param(r)?;
        let (r, _) = ws(r)?;
        (r, Some(pd))
    } else {
        (rest, None)
    };
    let (rest, mut body) = block(rest)?;
    if let Some(pd) = pointy_param {
        body.insert(0, pointy_topic_bind(&pd));
    }
    Ok((rest, Stmt::Given { topic, body }))
}

pub(crate) fn when_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("when", input).ok_or_else(|| PError::expected("when statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::When { cond, body }))
}

pub(crate) fn default_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("default", input).ok_or_else(|| PError::expected("default statement"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Default(body)))
}
