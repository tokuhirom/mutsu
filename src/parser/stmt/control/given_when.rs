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
    // An undeclared bareword in term position immediately followed by a block is
    // treated by raku as a function call that gobbles the block (e.g.
    // `when X::Y {}` parses as `when X::Y({...})`), which then leaves the `when`
    // without its required block. This produces an X::Syntax::BlockGobbled sorrow
    // plus an X::Syntax::Missing(block) panic, bundled in an X::Comp::Group.
    //
    // We can only reliably detect this at parse time for the reserved `X::` and
    // `CX::` exception/control namespaces: any name there that is neither a known
    // builtin exception nor a user-declared type is genuinely undeclared. Other
    // compound names are ambiguous at parse time (e.g. `Day::Mon` enum values, or
    // imported types the parser has not seen), so we leave them alone to avoid
    // false positives — mutsu registers user/imported types at run time, not at
    // parse time.
    if rest.starts_with('{')
        && let Expr::BareWord(name) = &cond
        && (name.starts_with("X::") || name.starts_with("CX::"))
        && !crate::runtime::utils::is_known_type_constraint(name)
        && !crate::runtime::utils::is_known_compound_type(name)
        && !crate::parser::stmt::simple::is_user_declared_type(name)
    {
        return Err(gobbled_block_error(name));
    }
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::When { cond, body }))
}

/// Build the `X::Comp::Group` raised when an undeclared bareword gobbles the
/// block that a surrounding construct (e.g. `when`) required.
fn gobbled_block_error(name: &str) -> PError {
    let sorrow = Value::make_exception(
        "X::Syntax::BlockGobbled",
        &[
            ("what", Value::str(name.to_string())),
            (
                "message",
                Value::str(format!(
                    "Function '{name}' needs parens to avoid gobbling block \
                     (or perhaps it's a class that's not declared or available in this scope?)"
                )),
            ),
        ],
    );
    let panic = Value::make_exception(
        "X::Syntax::Missing",
        &[
            ("what", Value::str("block".to_string())),
            (
                "message",
                Value::str(format!("Missing block (apparently claimed by '{name}')")),
            ),
        ],
    );
    let group = Value::make_comp_group(
        format!(
            "Function '{name}' needs parens to avoid gobbling block \
             (or perhaps it's a class that's not declared or available in this scope?)\n\
             Missing block (apparently claimed by '{name}')"
        ),
        Some(panic),
        vec![sorrow],
        vec![],
    );
    PError::fatal_with_exception("X::Comp::Group: Missing block".to_string(), Box::new(group))
}

pub(crate) fn default_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("default", input).ok_or_else(|| PError::expected("default statement"))?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Default(body)))
}
