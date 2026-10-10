use super::super::control_signature;
use super::*;

/// Lower `if COND { … } elsif … { … } else { … }` to `Stmt::If`. Each `elsif`
/// clause becomes a nested `Stmt::If` in the enclosing `else` branch, folded
/// innermost-last so the source order is preserved.
pub(super) fn lower_if(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let cond = lower_expr(named_child(node, "condition")?)?;
    let (then_branch, mut binding_var) = lower_clause_block(named_child(node, "then")?)?;
    let else_branch = lower_conditional_chain(node, None, &mut binding_var)?;
    Ok(Stmt::If {
        cond,
        then_branch,
        else_branch,
        binding_var,
        is_statement_modifier: false,
        is_unless: false,
        with_kind: None,
    })
}

/// The block of an `if` / `elsif` clause: a plain block, or the pointy block
/// `-> $v { }` of a clause that binds the tested value, whose parameter the
/// parser keeps as the clause's `binding_var`.
// Cost: O(n), n = size of the block.
pub(super) fn lower_clause_block(
    block: &RakuAstNode,
) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    if block.class != RakuAstClass::PointyBlock {
        return Ok((lower_block(block)?, None));
    }
    let (mut names, mut defs) = signature_positional_params(block)?;
    name_for_unpack_params(&mut names, &mut defs);
    // The parser's own expansion binds the parameters to the tested value.
    let (binding_var, body) = crate::parser::if_pointy_clause(defs, lower_block(block)?);
    Ok((body, binding_var))
}

/// Lower `with COND { … }` / `without COND { … }` to the conditional mutsu's
/// parser desugars them into (`with_desugar`), so execution reuses the existing
/// path and the converter renders the same node back.
pub(super) fn lower_with_block(
    node: &RakuAstNode,
    kind: WithBlockKind,
) -> Result<Stmt, RuntimeError> {
    let cond_expr = lower_expr(named_child(node, "condition")?)?;
    let tmp_name = crate::with_desugar::next_tmp_name();
    let (body_field, else_branch) = if matches!(kind, WithBlockKind::Without) {
        // rakudo rejects `without … else` at compile time, so a node carrying
        // one is not a shape it could have produced.
        if node
            .fields
            .iter()
            .any(|f| f.name == Some("else") || f.name == Some("elsifs"))
        {
            return Err(unsupported(node));
        }
        ("body", Vec::new())
    } else {
        // The `else` of a `with` continues a topicalizing clause, so it runs
        // under the last tested value -- the hidden temp when no `orwith`
        // intervened.
        (
            "then",
            lower_conditional_chain(node, Some(Expr::Var(tmp_name.clone())), &mut None)?,
        )
    };
    let block = named_child(node, body_field)?;
    if block.class == RakuAstClass::PointyBlock {
        // `with X -> PARAM { … }`: the parser's own expansion binds the
        // parameter to the tested value.
        let (mut names, mut defs) = signature_positional_params(block)?;
        name_for_unpack_params(&mut names, &mut defs);
        if defs.len() != 1 {
            return Err(unsupported(node));
        }
        let (Some(name), Some(def)) = (names.pop(), defs.pop()) else {
            return Err(unsupported(node));
        };
        let body = lower_block(block)?;
        let tmp_var = Expr::Var(tmp_name.clone());
        let then_branch =
            crate::parser::with_then_branch(&cond_expr, &tmp_var, &Some(name), &Some(def), body);
        return Ok(crate::with_desugar::with_conditional_branch(
            kind,
            &tmp_name,
            cond_expr,
            then_branch,
            else_branch,
        ));
    }
    let body = lower_block(block)?;
    Ok(crate::with_desugar::with_conditional(
        kind,
        &tmp_name,
        cond_expr,
        body,
        else_branch,
    ))
}

/// The `else` branch of a conditional: its `elsifs` clauses folded
/// innermost-last into nested `if`s, with the `else` block at the bottom.
///
/// `topic` is the value the head clause tested when that clause topicalizes
/// (`with`), because a trailing `else` runs under the *last* tested value --
/// which an intervening `orwith` replaces and a plain `elsif` clears.
pub(super) fn lower_conditional_chain(
    node: &RakuAstNode,
    topic: Option<Expr>,
    head_binding: &mut Option<String>,
) -> Result<Vec<Stmt>, RuntimeError> {
    let clauses = match node.fields.iter().find(|f| f.name == Some("elsifs")) {
        Some(field) => match &field.value {
            RakuAstFieldValue::List(items) => items.as_slice(),
            _ => return Err(unsupported(node)),
        },
        None => &[],
    };
    let mut lowered = Vec::with_capacity(clauses.len());
    let mut else_topic = topic;
    for item in clauses {
        let ValueView::RakuAst(clause) = item.view() else {
            return Err(unsupported(node));
        };
        let mut cond = lower_expr(named_child(clause, "condition")?)?;
        let block = named_child(clause, "then")?;
        let (body, binding_var) = match clause.class {
            RakuAstClass::StatementElsif => {
                else_topic = None;
                lower_clause_block(block)?
            }
            RakuAstClass::StatementOrwith => {
                let tmp = crate::with_desugar::next_tmp_name();
                let tmp_var = Expr::Var(tmp.clone());
                let body = if block.class == RakuAstClass::PointyBlock {
                    control_signature::with_body(&cond, &tmp_var, block)?
                } else {
                    vec![crate::with_desugar::topic_given(
                        crate::with_desugar::body_topic(&cond, &tmp_var),
                        lower_block(block)?,
                    )]
                };
                else_topic = Some(tmp_var);
                cond = crate::with_desugar::defined_condition(false, &tmp, cond);
                (body, None)
            }
            _ => return Err(unsupported(clause)),
        };
        lowered.push((clause.class, cond, body, binding_var));
    }

    let mut else_branch = match node.fields.iter().find(|f| f.name == Some("else")) {
        Some(_) => {
            let block = named_child(node, "else")?;
            if block.class == RakuAstClass::PointyBlock {
                match else_topic {
                    Some(topic) => control_signature::with_body(&topic, &topic, block)?,
                    None => {
                        let binding = match lowered.last_mut() {
                            Some((_, _, _, binding)) => binding,
                            None => head_binding,
                        };
                        crate::parser::else_pointy_clause(
                            binding,
                            control_signature::params(block)?,
                            lower_block(block)?,
                        )
                    }
                }
            } else {
                let body = lower_block(block)?;
                match else_topic {
                    Some(topic) => vec![crate::with_desugar::topic_given(topic, body)],
                    None => body,
                }
            }
        }
        None => Vec::new(),
    };
    for (class, cond, body, binding_var) in lowered.into_iter().rev() {
        else_branch = if class == RakuAstClass::StatementOrwith {
            vec![Stmt::If {
                cond,
                then_branch: body,
                else_branch,
                binding_var: None,
                is_statement_modifier: false,
                is_unless: false,
                with_kind: Some(WithBlockKind::Orwith),
            }]
        } else {
            vec![Stmt::If {
                cond,
                then_branch: body,
                else_branch,
                binding_var,
                is_statement_modifier: false,
                is_unless: false,
                with_kind: None,
            }]
        };
    }
    Ok(else_branch)
}
