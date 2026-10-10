use super::*;

/// `with X { ... }` / `without X { ... }` -> `Statement::With` / `::Without`.
///
/// The parser desugars the block forms into
/// `if (my $tmp = X).defined { given X { ... } }` (condition negated for
/// `without`), so everything raku models is wrapped in scaffolding: the once-
/// evaluated temp around the condition, and the topicalizing `given` around the
/// body, which raku spells as the block's own `implicit-topic` flag. `kind`
/// says which keyword the source wrote -- see `Stmt::If`'s `with_kind`.
/// Measured against rakudo 2026.07: `Q[with 1 { say 2 }].AST`.
pub(super) fn with_block_node(
    kind: WithBlockKind,
    cond: &Expr,
    then_branch: &[Stmt],
    else_branch: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    let condition = node_field(
        Some("condition"),
        convert_expr(with_block_condition(kind, cond)?)?,
    );
    // A pointy body (`with X -> $a { }`) binds its parameter inside the same
    // `given`, which raku spells as a `PointyBlock` rather than an
    // implicit-topic `Block`; the parser records the written parameter and body.
    let block = match topic_given_body(then_branch) {
        Some(body) => topic_block_node(body)?,
        None => with_pointy_block(then_branch)?,
    };
    match kind {
        // `without` takes no `else`/`orwith`/`elsif` (rakudo rejects them at
        // compile time), so its node is condition + `body` -- the same naming
        // difference `Statement::Unless` has against `::If`.
        WithBlockKind::Without => {
            if !else_branch.is_empty() {
                return Err(unsupported("`without` with an else branch"));
            }
            Ok(RakuAstNode {
                class: RakuAstClass::StatementWithout,
                fields: vec![condition, node_field(Some("body"), block)],
            })
        }
        WithBlockKind::With => {
            let mut fields = vec![condition, node_field(Some("then"), block)];
            fields.extend(conditional_chain_fields(else_branch)?);
            Ok(RakuAstNode {
                class: RakuAstClass::StatementWith,
                fields,
            })
        }
        // An `orwith` clause is only ever reached through the chain walk of the
        // conditional it continues.
        WithBlockKind::Orwith => Err(unsupported("`orwith` outside a conditional chain")),
    }
}

/// The `PointyBlock` of `with X -> PARAM { BODY }`, from the record the parser
/// leaves at the head of the then-branch.
// Cost: O(n), n = size of the body.
pub(super) fn with_pointy_block(then_branch: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let Some(Stmt::SourceForm(form)) = then_branch.iter().find(|s| !matches!(s, Stmt::SetLine(_)))
    else {
        return Err(unsupported("with/without block with an explicit signature"));
    };
    let crate::ast::SourceForm::WithPointy {
        param_def, body, ..
    } = form.as_ref()
    else {
        return Err(unsupported("with/without block with an explicit signature"));
    };
    let Some(param_def) = param_def else {
        return Err(unsupported("with/without block with an explicit signature"));
    };
    pointy_block(std::slice::from_ref(param_def), body, None)
}

/// One `orwith` clause -> `Statement::Orwith(condition, then => topic Block)`.
pub(super) fn orwith_node(cond: &Expr, then_branch: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    let block = match topic_given_body(then_branch) {
        Some(body) => topic_block_node(body)?,
        None => with_pointy_block(then_branch)?,
    };
    Ok(RakuAstNode {
        class: RakuAstClass::StatementOrwith,
        fields: vec![
            node_field(
                Some("condition"),
                convert_expr(with_block_condition(WithBlockKind::Orwith, cond)?)?,
            ),
            node_field(Some("then"), block),
        ],
    })
}

/// The condition a `with`-family conditional was written with, recovered from
/// the `.defined` test the parser built around it.
pub(super) fn with_block_condition(
    kind: WithBlockKind,
    cond: &Expr,
) -> Result<&Expr, RuntimeError> {
    let tested = match kind {
        WithBlockKind::Without => match cond {
            Expr::Unary {
                op: crate::token_kind::TokenKind::Bang,
                expr,
                ..
            } => expr.as_ref(),
            _ => return Err(unsupported("`without` condition")),
        },
        WithBlockKind::With | WithBlockKind::Orwith => cond,
    };
    let Expr::MethodCall { target, name, .. } = tested else {
        return Err(unsupported("with/without condition"));
    };
    if name.as_str() != "defined" {
        return Err(unsupported("with/without condition"));
    }
    Ok(match target.as_ref() {
        // The block forms evaluate the condition once into a hidden temp; an
        // `orwith` clause tests its expression directly.
        Expr::DoStmt(inner) => match inner.as_ref() {
            Stmt::VarDecl { expr, .. } => expr,
            _ => return Err(unsupported("with/without condition")),
        },
        other => other,
    })
}

/// The body of the topicalizing `given` the `with`-family block forms wrap a
/// `{ ... }` in, when `stmts` is exactly that scaffold. `None` for anything
/// else, including the untagged `given` a pointy body produces and a
/// hand-written `given` that happens to sit in the same position.
pub(super) fn topic_given_body(stmts: &[Stmt]) -> Option<&[Stmt]> {
    let mut real = stmts.iter().filter(|s| !matches!(s, Stmt::SetLine(_)));
    let (Some(first), None) = (real.next(), real.next()) else {
        return None;
    };
    match first {
        Stmt::Given {
            body,
            with_kind: Some(GivenWithKind::BlockTopic),
            ..
        } => Some(body),
        _ => None,
    }
}

/// The `elsifs` and `else` fields of a conditional chain. mutsu nests every
/// continuation clause as a single `if` inside the else branch; raku flattens
/// them into one `elsifs` list, with whatever remains as the `else` block.
/// Shared by `Statement::If` and `Statement::With`, both of which accept
/// `elsif` and `orwith` clauses.
pub(super) fn conditional_chain_fields(
    else_branch: &[Stmt],
) -> Result<Vec<RakuAstField>, RuntimeError> {
    let mut fields = Vec::new();
    let mut elsifs: Vec<Value> = Vec::new();
    let mut tail: &[Stmt] = else_branch;
    while let Some(Stmt::If {
        cond,
        then_branch,
        else_branch,
        binding_var,
        with_kind,
        ..
    }) = single_if_stmt(tail)
    {
        // A `with`/`without` BLOCK statement written inside an `else` is a
        // statement of its own, not a continuation clause: stop and let it be
        // converted as part of the else block.
        if matches!(
            with_kind,
            Some(WithBlockKind::With | WithBlockKind::Without)
        ) {
            break;
        }
        if binding_var.is_some() && with_kind.is_some() {
            return Err(unsupported("`orwith EXPR -> $var` topic binding"));
        }
        let node = match with_kind {
            Some(WithBlockKind::Orwith) => orwith_node(cond, then_branch)?,
            _ => elsif_node(cond, then_branch, binding_var)?,
        };
        elsifs.push(Value::rakuast(Box::new(node)));
        tail = else_branch;
    }
    if !elsifs.is_empty() {
        fields.push(RakuAstField {
            name: Some("elsifs"),
            value: RakuAstFieldValue::List(elsifs),
        });
    }
    if tail.iter().any(|s| !matches!(s, Stmt::SetLine(_))) {
        // An `else` continuing a `with`/`orwith` topicalizes on the last tested
        // value, which raku records on the block itself.
        let block = match topic_given_body(tail) {
            Some(body) => topic_block_node(body)?,
            None => {
                if matches!(tail.first(), Some(Stmt::SourceForm(form)) if matches!(form.as_ref(), crate::ast::SourceForm::WithPointy { .. }))
                {
                    with_pointy_block(tail)?
                } else {
                    clause_block_node(tail, &None)?
                }
            }
        };
        fields.push(node_field(Some("else"), block));
    }
    Ok(fields)
}

/// One `elsif` clause -> `Statement::Elsif(condition, then => Block)`.
pub(super) fn elsif_node(
    cond: &Expr,
    then_branch: &[Stmt],
    binding_var: &Option<String>,
) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::StatementElsif,
        fields: vec![
            node_field(Some("condition"), convert_expr(cond)?),
            node_field(Some("then"), clause_block_node(then_branch, binding_var)?),
        ],
    })
}

/// The block of an `if` / `elsif` clause: a plain block, or the pointy block
/// `-> $v { }` when the clause binds the tested value (the parser's
/// `binding_var`, a bare scalar name; measured on rakudo 2026.09).
// Cost: O(n), n = size of the block.
pub(super) fn clause_block_node(
    then_branch: &[Stmt],
    binding_var: &Option<String>,
) -> Result<RakuAstNode, RuntimeError> {
    // The parser's record of `-> PARAMS { BODY }`, as written.
    if let Some(Stmt::SourceForm(form)) = then_branch.first()
        && let crate::ast::SourceForm::IfPointy { param_defs, body } = form.as_ref()
    {
        return pointy_block(param_defs, body, None);
    }
    match binding_var {
        None => signature_block_node(then_branch),
        // A plain then block may save its condition solely for an else
        // signature. This compiler temporary is not a written then parameter.
        Some(name) if name.starts_with("$__mutsu_if_bind_") => signature_block_node(then_branch),
        Some(name) if is_plain_scalar_name(name) => pointy_block(
            &[super::super::lower::positional_param(name)],
            then_branch,
            None,
        ),
        Some(_) => Err(unsupported(
            "`if EXPR -> $var` topic binding of another form",
        )),
    }
}

/// A bare scalar variable name: `v`, not `$v` / `@a` / an internal `__...`.
pub(super) fn is_plain_scalar_name(name: &str) -> bool {
    !name.is_empty()
        && !name.starts_with("__")
        && name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
}
