use super::*;

/// The `(param, param_def, params, params_def, rw_block, explicit_zero_params)`
/// shape `Stmt::For` uses to describe a loop's own signature.
type ForParamShape = (
    Option<String>,
    Box<Option<ParamDef>>,
    Vec<String>,
    Vec<ParamDef>,
    bool,
    bool,
);

/// Turn a closure's own explicit signature into the shape `Stmt::For` uses, so
/// a pointy-block/`sub (...) { ... }` operand of the `for` statement modifier
/// consumes N elements per iteration according to its own arity -- exactly like
/// `for LIST -> SIG { ... }` already does for the identical signature written
/// the other way round. Mirrors how `parse_for_params` (for_params.rs) and
/// `arrow_lambda_inner` (lambda.rs) shape a single named param into the
/// singular `param`/`param_def` slot and a multi-param signature into the
/// plural `params`/`params_def` slot; an explicit empty signature (`-> {}`,
/// `sub () { }`) sets `explicit_zero_params` instead.
fn closure_signature_as_for_params(
    params: Vec<String>,
    param_defs: Vec<ParamDef>,
    is_rw: bool,
) -> ForParamShape {
    match params.len() {
        0 => (None, Box::new(None), Vec::new(), Vec::new(), is_rw, true),
        // A lone sigil'd slurpy (`*@all { ... } for @list`) binds a *list* of the
        // iteration chunk rather than the chunk element itself, so it needs the
        // plural shape whose binder knows about slurpies — exactly the routing
        // `parse_for_params` uses for the `for @list -> *@all { }` spelling.
        1 if param_defs[0].is_variadic() && !param_defs[0].sigilless => {
            (None, Box::new(None), params, param_defs, is_rw, false)
        }
        1 => {
            let param = params.into_iter().next();
            let param_def = param_defs.into_iter().next();
            (
                param,
                Box::new(param_def),
                Vec::new(),
                Vec::new(),
                is_rw,
                false,
            )
        }
        _ => (None, Box::new(None), params, param_defs, is_rw, false),
    }
}

/// Try to parse a single statement modifier. Returns None if no modifier matched.
pub(super) fn parse_for_modifier(rest: &str, stmt: Stmt) -> Result<Option<(&str, Stmt)>, PError> {
    if let Some(r) = keyword("for", rest) {
        if matches!(stmt, Stmt::For { .. }) {
            return Err(PError::raw(
                "double statement-modifying for is not allowed".to_string(),
                Some(rest.len()),
            ));
        }
        let (hoisted_decl, stmt) = split_compound_decl_for_modifier(stmt);
        let (r, _) = ws1(r)?;
        // Sequence operators absorb a comma-separated seed list on their left:
        // `for 1, { $_ + 1 } ... 3` iterates one sequence, not an array whose
        // second item is another sequence.  This is the same special case used
        // by listop and assignment parsing.
        let (r, iterable) =
            if let Some(result) = crate::parser::primary::try_parse_sequence_arg_list(r) {
                result?
            } else {
                let (r, first) = expression(r).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected iterable expression after 'for'",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r.len())),
                    exception: None,
                })?;
                // Parse comma-separated list for `for` modifier: `expr for 1, 2, 3`
                let (r, iterable) = {
                    let mut items = vec![first];
                    let mut r = r;
                    let mut trailing_comma = false;
                    loop {
                        let (r2, _) = ws(r)?;
                        if !r2.starts_with(',') {
                            break;
                        }
                        let r2 = &r2[1..];
                        let (r2, _) = ws(r2)?;
                        if r2.is_empty() || r2.starts_with('}') || r2.starts_with(';') {
                            r = r2;
                            trailing_comma = true;
                            break;
                        }
                        let (r2, next) = expression(r2)?;
                        items.push(next);
                        r = r2;
                    }
                    // A trailing comma builds a 1-element list even with a single item:
                    // `for @a,` iterates once over `(@a,)` (the array itemized), matching
                    // the parenthesized `for (@a,)` form, rather than flattening `@a`.
                    if items.len() == 1 && !trailing_comma {
                        (r, items.into_iter().next().unwrap())
                    } else {
                        (r, Expr::ArrayLiteral(items))
                    }
                };
                (r, iterable)
            };
        let (r, _) = ws(r)?;
        // Detect "Two terms in a row" on the same line after the for iterable.
        // e.g., `.say for (1, 2, 3)«~» "!"` — the `"!"` is a term without
        // an infix operator separating it from the iterable.
        if !r.is_empty()
            && !r.starts_with(';')
            && !r.starts_with('}')
            && !r.starts_with(')')
            && !r.starts_with("->")
            && !r.starts_with('{')
            && !is_stmt_modifier_keyword(r)
            && starts_with_term_char(r)
        {
            return Err(PError::fatal_at(
                "Confused. Two terms in a row".to_string(),
                r,
            ));
        }
        // Do not consume full loop headers as statement modifiers.
        // This preserves parsing of:
        //   { ... }
        //   for @values -> $v { ... }
        // as two statements, rather than block + postfix modifier + lambda.
        if r.starts_with("->") || r.starts_with('{') {
            return Ok(None);
        }
        let (param, param_def, params, params_def, rw_block, explicit_zero_params, body) =
            match stmt {
                // `{ ... } for LIST` is the very same loop as `for LIST { ... }`,
                // so the bare block gives the loop its implicit placeholder
                // signature exactly as a `for LIST { ... }` body block does.
                // Without this the loop stayed signature-less and `$^a`/`$^b` never
                // got bound (`{ $^a ~ $^b } for (1,2),(3,4)` yielded `True/True`).
                Stmt::Block(_) => {
                    let (param, params) = for_modifier_loop_params(&stmt);
                    (
                        param,
                        Box::new(None),
                        params,
                        Vec::new(),
                        false,
                        false,
                        vec![stmt],
                    )
                }
                // A Whatever-curried statement (`* + 1 for @a`) is an ordinary
                // expression evaluated once per iteration, so the loop yields one
                // WhateverCode per element and calls nothing (#13308); it falls
                // through to the generic arm below. A pointy block is the
                // statement itself and is called per element.
                //
                // The single-param pointy block form (`-> $x { ... }`) is
                // already exactly arity 1 under a plain call with the topic, so
                // it keeps working unchanged.
                Stmt::Expr(
                    expr @ Expr::Lambda {
                        is_whatever_code: false,
                        ..
                    },
                ) => {
                    let target = Expr::CallOn {
                        target: Box::new(expr),
                        args: vec![Expr::Var("_".to_string())],
                    };
                    (
                        None,
                        Box::new(None),
                        Vec::new(),
                        Vec::new(),
                        false,
                        false,
                        vec![Stmt::Expr(target)],
                    )
                }
                // A genuine multi-/zero-/slurpy-param signature written as a
                // pointy block or `sub (...) { ... }`: make the closure's own
                // signature the loop's signature and its body the loop's body,
                // so `Stmt::For`'s existing multi-param handling consumes N
                // elements per iteration -- the same lowering the bare
                // placeholder-block case above already uses. Excludes the
                // implicit-`@_` bare-block shape (`{ @_ } for LIST`, guarded
                // below): rakudo invokes that one element at a time even though
                // its only parameter is a synthesized slurpy `*@_`, so its
                // signature must NOT become the loop's own (mirrors
                // `bare_block_body` in meta_ops.rs, which excludes this exact
                // shape from the placeholder-block conversion for the same
                // reason).
                Stmt::Expr(Expr::AnonSubParams {
                    params,
                    param_defs,
                    body,
                    is_rw,
                    ..
                }) if !(params.len() == 1
                    && params[0] == "@_"
                    && param_defs.first().is_some_and(|d| d.block_param)) =>
                {
                    let (param, param_def, params, params_def, rw_block, explicit_zero_params) =
                        closure_signature_as_for_params(params, param_defs, is_rw);
                    (
                        param,
                        param_def,
                        params,
                        params_def,
                        rw_block,
                        explicit_zero_params,
                        body,
                    )
                }
                Stmt::Expr(expr @ Expr::AnonSubParams { .. }) => {
                    let target = Expr::CallOn {
                        target: Box::new(expr),
                        args: vec![Expr::Var("_".to_string())],
                    };
                    (
                        None,
                        Box::new(None),
                        Vec::new(),
                        Vec::new(),
                        false,
                        false,
                        vec![Stmt::Expr(target)],
                    )
                }
                other => (
                    None,
                    Box::new(None),
                    Vec::new(),
                    Vec::new(),
                    false,
                    false,
                    vec![other],
                ),
            };
        let loop_stmt = Stmt::For {
            iterable,
            param,
            param_def,
            params,
            params_def,
            body,
            label: None,
            mode: crate::ast::ForMode::Normal,
            rw_block,
            explicit_zero_params,
            is_statement_modifier: true,
            uses_block_magic: false,
        };
        let stmt = match hoisted_decl {
            // A recorded compound declaration hoists its record with it.
            Some(Stmt::SyntheticBlock(mut hoisted)) => {
                hoisted.push(loop_stmt);
                Stmt::SyntheticBlock(hoisted)
            }
            Some(declaration) => Stmt::SyntheticBlock(vec![declaration, loop_stmt]),
            None => loop_stmt,
        };
        return Ok(Some((r, stmt)));
    }
    Ok(None)
}
