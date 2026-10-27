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

/// The then-branch of `with COND [-> PARAM] { BODY }`: the topicalization and
/// parameter binds the parser expands the block into. `lower` calls it too, so
/// a hand-built `Statement::With` over a `PointyBlock` expands the same way.
// Cost: O(n), n = size of the body.
pub(crate) fn with_then_branch(
    cond_expr: &Expr,
    tmp_var: &Expr,
    param_name: &Option<String>,
    param_def: &Option<ParamDef>,
    body: Vec<Stmt>,
) -> Result<Vec<Stmt>, crate::value::RuntimeError> {
    let cond_expr = cond_expr.clone();
    let tmp_var = tmp_var.clone();
    let param_name = param_name.clone();
    let param_def = param_def.clone();
    // The body uses $_ which was set in the condition block.
    // We still prepend topicalize(&tmp_var) as a no-op marker so that
    // $_ is visible in the body scope (but it's already set by the condition).
    // If the topic source is a literal value, wrap $_ in a Mixin marked as
    // read-only so that mutating ops like tr/// throw X::Assignment::RO.
    // Encoding the read-only flag in the value itself avoids leaking marker
    // variables across exception boundaries.
    // When the topic is a plain variable and there is no pointy parameter, run
    // the body under a `given` so `$_` aliases that variable (matching `given`'s
    // semantics). This lets in-place mutations through `$_` (e.g. `.=uc`,
    // `$_ = ...`) write back to the original container, while `$_` stays writable
    // exactly as in `given`. The `.defined` condition is still evaluated
    // separately via `$tmp`. Container variables (`@a` / `%h`) also alias their
    // container topic through `given`, so `with @a { .push }` propagates the
    // mutation back to `@a` (the `given` opcode treats `@`/`%` topics as
    // read-only-for-reassign but writes container mutations back to the source).
    // A container *element* subscript on a simple variable (`with %h<k>` /
    // `with @a[i]`) is an lvalue: routing it through `given` makes the `given`
    // element-source writeback propagate both `$_ = ...` and `.push` back to the
    // element (the element topic is rw, unlike a whole-container topic).
    let topic_is_element_lvalue = matches!(
        &cond_expr,
        Expr::Index { target, .. }
            if matches!(
                target.as_ref(),
                Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_)
            )
    );
    let cond_is_lvalue = matches!(
        &cond_expr,
        Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_)
    ) || topic_is_element_lvalue;
    // A container/scalar pointy parameter on an lvalue condition is routed
    // through `given` so it shares `given`'s topic semantics: a plain `-> @p`
    // aliases the source (writeback of `@p.push` / `@p[0]=v`), while `-> @p is
    // copy` becomes a flattened fresh copy (see `pointy_topic_bind`). Exclude
    // sub-signatures, sigilless `\x`, callables, and attributive (`$!x`/`$.x`)
    // params — those keep the copy-style VarDecl binding below.
    let pointy_routes_through_given = param_def
        .as_ref()
        .map(|pd| {
            pd.sub_signature.is_none()
                && !pd.sigilless
                && !pd.name.is_empty()
                && !pd.name.starts_with('&')
                && !pd.name.starts_with('!')
                && !pd.name.starts_with('.')
        })
        .unwrap_or(false);
    // An explicit `-> $_` pointy parameter names the TOPIC. Binding it as an
    // ordinary lexical (`my $_ = $tmp`) writes the enclosing scope's topic slot
    // and leaves the value behind after the block, so it is routed through
    // `given` instead — the same fresh-topic-scope treatment the parameterless
    // form already gets, and the same fix as `if COND -> $_` in
    // `conditionals.rs`. Sub-signature / sigilless / attributive params keep
    // their own binding.
    let pointy_is_topic = param_name
        .as_deref()
        .is_some_and(|p| p.trim_start_matches('$') == "_")
        && param_def
            .as_ref()
            .is_none_or(|pd| !pd.sigilless && pd.sub_signature.is_none());
    let use_given_alias = cond_is_lvalue && (param_name.is_none() || pointy_routes_through_given);
    // A non-lvalue, non-literal topic (`with foo()`) with no pointy parameter is
    // run under `given $tmp` (the once-evaluated condition value), so `$_` is
    // properly scoped: it is saved/restored around the body and the enclosing
    // `given`/`with`'s topic-source writeback is suspended for the inner body.
    // Without this, the flat `$_ = $tmp` topicalization runs in the outer scope
    // and leaks back into the outer topic source — e.g. nested
    // `with $x { with foo() { } }` clobbered `$x` with `foo()`'s value.
    let is_literal_topic = matches!(&cond_expr, Expr::Literal(_));
    let route_through_given_tmp = !cond_is_lvalue && !is_literal_topic && param_name.is_none();
    // A literal topic (`with 5 { }`) runs under `given <literal>`: that yields a
    // fresh read-only `$_` topic scope (matching `given`'s immutable-literal
    // semantics) and, crucially, establishes the topic via the `given` opcode
    // rather than a plain `$_ = <literal>` assignment — so it nests correctly
    // inside a `for ^N { }`, whose `$_` readonly mark would otherwise make that
    // assignment throw X::Assignment::RO.
    let route_literal_through_given = is_literal_topic && param_name.is_none();
    // Topicalize `$_` for the body. For a non-literal, non-`given` topic reuse the
    // `$tmp` holding the (once-evaluated) condition value.
    let topic_assign = topicalize(&tmp_var);
    // A block with an explicit signature does not bind `$_`: `with 1 -> $a { $_ }` leaves
    // the outer topic alone, exactly as `for 1 -> $b { $_ }` does. Only the parameterless
    // form topicalizes.
    let mut with_body = if use_given_alias || route_literal_through_given || param_name.is_some() {
        Vec::new()
    } else {
        vec![topic_assign]
    };
    // If a named parameter was given (-> $param), also assign it. A
    // container/scalar pointy param is handled by the `given` routing below
    // (it prepends the alias/copy bind to the given body), so skip the VarDecl
    // here for those.
    if let Some(ref pname) = param_name
        && !(use_given_alias && pointy_routes_through_given)
        && !pointy_is_topic
    {
        // Attributive parameter (`-> $!foo` / `-> $.foo`): bind the value to
        // self's attribute via an attribute assignment. This must be checked
        // first (an attributive param never has a sub-signature), and it must be
        // an Assign (not a VarDecl) so it writes through to the attribute even
        // when the body runs inside a nested `given` topic scope.
        if pname.starts_with('!') || pname.starts_with('.') {
            let attr_name = &pname[1..];
            with_body.push(Stmt::Assign {
                name: format!("!{}", attr_name),
                expr: tmp_var.clone(),
                op: crate::ast::AssignOp::Assign,
                target_is_sigilless: false,
            });
        } else if let Some(ref pdef) = param_def {
            if let Some(ref sub_params) = pdef.sub_signature {
                // A destructuring parameter unpacks the condition value into the
                // sub-signature's lexicals. The unpack is the shared lowering
                // `for` and `given` use; the copy that used to live here called a
                // method named after the sub-parameter *with its sigil attached*
                // (`$obj.@list`), and knew nothing of hash fallback, `|`
                // captures, renames or defaults.
                with_body.push(Stmt::VarDecl {
                    marks: Default::default(),
                    anonymous: false,
                    name: pname.clone(),
                    expr: tmp_var.clone(),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    has_initializer: false,
                    constant: None,
                    bind: None,
                    where_constraint: None,
                });
                crate::param_destructure::destructure_declaring(pname, sub_params, &mut with_body);
            } else {
                // Simple parameter with possible traits from parse_for_params
                with_body.push(pointy_param_bind_decl(pdef, &tmp_var));
            }
        } else {
            with_body.push(simple_pointy_bind(pname, &tmp_var, false));
        }
    }
    // Tag the scaffold `given` so the RakuAST converter can tell it from a
    // source-level one. A pointy body prepends its own binding to the same
    // `given`, a shape raku spells as a `PointyBlock` rather than an
    // implicit-topic `Block`, so it gets the distinct tag that makes the
    // converter report the boundary instead of swallowing the parameter.
    let block_topic = Some(if param_name.is_none() {
        GivenWithKind::BlockTopic
    } else {
        GivenWithKind::BlockTopicPointy
    });
    if use_given_alias {
        let mut given_body = body;
        // For a container/scalar pointy param, prepend the alias/copy bind
        // (`@p := $_` for aliasing, `@p = $_.list` for `is copy`) so the `given`
        // mechanism handles topic-source writeback (alias) or a flattened fresh
        // copy (matching `given @a -> @p`).
        // `-> $_` needs no bind at all: `given` already installs the topic.
        if pointy_routes_through_given
            && !pointy_is_topic
            && let Some(ref pd) = param_def
        {
            given_body.insert(0, pointy_topic_bind(pd));
        }
        let given_topic = if topic_is_element_lvalue {
            tmp_var.clone()
        } else {
            cond_expr.clone()
        };
        with_body = vec![Stmt::Given {
            // Plain variables can be topicalized directly, preserving their
            // source container. An element subscript uses the hidden temp;
            // its already-evaluated source is tagged while compiling the
            // condition so the body still writes back without re-running an
            // effectful index expression.
            topic: given_topic,
            body: given_body,
            is_statement_modifier: false,
            with_kind: block_topic,
        }];
    } else if route_through_given_tmp || pointy_is_topic {
        with_body = vec![Stmt::Given {
            topic: tmp_var.clone(),
            body,
            is_statement_modifier: false,
            with_kind: block_topic,
        }];
    } else if route_literal_through_given {
        with_body = vec![Stmt::Given {
            topic: cond_expr.clone(),
            body,
            is_statement_modifier: false,
            with_kind: block_topic,
        }];
    } else {
        with_body.extend(body);
    }

    Ok(with_body)
}
