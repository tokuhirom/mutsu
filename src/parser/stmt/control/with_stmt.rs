use super::*;

/// Parse `with`/`without` statement.
pub(crate) fn with_stmt(input: &str) -> PResult<'_, Stmt> {
    let is_without = keyword("without", input).is_some();
    let rest = keyword("with", input)
        .or_else(|| keyword("without", input))
        .ok_or_else(|| PError::expected("with/without statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, condition) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    // An optional pointy block: -> $param { ... }, -> \param { ... }, or
    // -> (SIGNATURE) { ... } (sub-signature destructuring).
    let (rest, then) = super::conditionals::topic_clause_block(rest, None)?;

    // Parse orwith / else chains
    let rest_before_ws = rest;
    let (rest, _) = ws(rest)?;

    // `without` cannot have `else`, `orwith`, or `elsif` chains (Raku spec)
    if is_without {
        for kw in &["else", "orwith", "elsif"] {
            if keyword(kw, rest).is_some() {
                return Err(PError::from_typed(
                    crate::value::RuntimeError::without_else(kw),
                ));
            }
        }
    }

    let (rest, clauses, else_block) =
        if keyword("orwith", rest).is_some() || keyword("elsif", rest).is_some() {
            // The whole `orwith`/`elsif` continuation — *multiple* `orwith`
            // clauses and a trailing `else` included — is the shared if-chain
            // parser's.
            let (r, (clauses, else_block)) = parse_elsif_chain(rest)?;
            (r, clauses, else_block)
        } else if let Some(r) = keyword("else", rest) {
            let (r, _) = ws(r)?;
            let (r, block) =
                super::conditionals::topic_clause_block(r, Some("single else pointy parameter"))?;
            (r, Vec::new(), Some(block))
        } else {
            // No orwith/else found — don't consume whitespace/newlines past the
            // closing brace, so the caller sees the statement boundary correctly
            // (e.g. `do with X { ... }\nsay ...` should not absorb `say`).
            (rest_before_ws, Vec::new(), None)
        };

    let node = crate::ast::parsed::Conditional {
        kind: if is_without {
            crate::ast::parsed::ConditionalKind::Without
        } else {
            crate::ast::parsed::ConditionalKind::With
        },
        condition,
        then,
        clauses,
        else_block,
    };
    let stmt = super::parsed_block::finish(crate::ast::parsed::ParsedStmt::Conditional(node))
        .map_err(PError::from_typed)?;
    Ok((rest, stmt))
}
