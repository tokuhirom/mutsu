use super::*;

/// Parse a while clause, retaining its written signature.
pub(crate) fn while_stmt(input: &str) -> PResult<'_, Stmt> {
    parse_loop(input, false, None)
}

/// Parse an until clause, retaining its written signature.
pub(crate) fn until_stmt(input: &str) -> PResult<'_, Stmt> {
    parse_loop(input, true, None)
}

pub(super) fn parse_loop(input: &str, is_until: bool, label: Option<String>) -> PResult<'_, Stmt> {
    let rest = keyword(if is_until { "until" } else { "while" }, input)
        .ok_or_else(|| PError::expected("while/until statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let start = rest;
    let (rest, params) = if rest.starts_with("->") || rest.starts_with("<->") {
        let (rest, (name, def, params, _, _, _)) = parse_for_params(rest)?;
        if !params.is_empty() {
            return Err(PError::expected_at(
                "single while/until pointy parameter",
                rest,
            ));
        }
        let (rest, _) = ws(rest)?;
        let params = crate::ast::parsed::BlockParams::Topic {
            name,
            def: def.map(Box::new),
        };
        (rest, params)
    } else {
        (rest, crate::ast::parsed::BlockParams::None)
    };
    let (rest, block) = super::parsed_block::parsed_block(start, rest, params)?;
    if block.is_pointy()
        && let Some(err) =
            crate::parser::stmt::sub::placeholder_overrides_signature_error(&block.body, &[])
    {
        return Err(err);
    }
    let node = crate::ast::parsed::ParsedStmt::WhileLoop(crate::ast::parsed::WhileLoop {
        until: is_until,
        condition: cond,
        block,
        label,
        bare_term: false,
    });
    let clause = super::parsed_block::finish(node)
        .map_err(|error| PError::fatal(error.message.into_owned()))?;
    Ok((rest, clause))
}
