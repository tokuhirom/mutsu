use super::*;

/// Parse `with`/`without` statement.
pub(crate) fn with_stmt(input: &str) -> PResult<'_, Stmt> {
    let is_without = keyword("without", input).is_some();
    let rest = keyword("with", input)
        .or_else(|| keyword("without", input))
        .ok_or_else(|| PError::expected("with/without statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond_expr) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;

    // Check for optional pointy block: -> $param { ... }, -> \param { ... },
    // or -> (SIGNATURE) { ... } (sub-signature destructuring)
    let (rest, param_name, param_def) = if rest.starts_with("->") || rest.starts_with("<->") {
        let (r, (param, param_def, _params, _params_def, _rw_block, _explicit_zero)) =
            parse_for_params(rest)?;
        (r, param, param_def)
    } else {
        (rest, None, None)
    };

    let (rest, body) = block(rest)?;

    // Use a temp variable in the condition to evaluate cond_expr exactly once.
    // This is important for expressions like `Failure.new` where evaluating
    // twice would create two distinct objects, and the `.defined` call on the
    // condition would not mark the same instance that `$_` receives.
    static WITH_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let with_id = WITH_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let tmp_name = format!("__with_tmp_{}", with_id);
    let tmp_var = Expr::Var(tmp_name.clone());

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
    // Topicalize `$_` for the body. For a literal, wrap it in a Mixin marked
    // read-only so in-place mutation of `$_` throws X::Assignment::RO. Otherwise
    // reuse the `$tmp` holding the (once-evaluated) condition value.
    let topic_assign = if let Expr::Literal(lit) = &cond_expr {
        let mut overrides = std::collections::HashMap::new();
        overrides.insert("__mutsu_topic_ro__".to_string(), Value::Bool(true));
        let wrapped = Value::mixin(lit.clone(), overrides);
        Stmt::Assign {
            name: "_".to_string(),
            op: AssignOp::Assign,
            expr: Expr::Literal(wrapped),
        }
    } else {
        topicalize(&tmp_var)
    };
    let mut with_body = if use_given_alias {
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
            });
        } else if let Some(ref pdef) = param_def {
            if let Some(ref sub_params) = pdef.sub_signature {
                // Declare the unpack variable holding the condition value
                with_body.push(Stmt::VarDecl {
                    name: pname.clone(),
                    expr: tmp_var.clone(),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
                });
                // Destructure each positional parameter from the unpack variable
                let mut positional_index = 0usize;
                for sub in sub_params {
                    if sub.name.is_empty() {
                        continue;
                    }
                    let extract_expr = if sub.named {
                        Expr::MethodCall {
                            target: Box::new(Expr::Var(pname.clone())),
                            name: Symbol::intern(&sub.name),
                            args: Vec::new(),
                            modifier: None,
                            quoted: false,
                        }
                    } else {
                        let idx_expr = Expr::Index {
                            target: Box::new(Expr::Var(pname.clone())),
                            index: Box::new(Expr::Literal(Value::Int(positional_index as i64))),
                            is_positional: true,
                        };
                        positional_index += 1;
                        idx_expr
                    };
                    // Apply type coercion if present (e.g. Int())
                    let coerced_expr = if let Some(ref tc) = sub.type_constraint {
                        // Strip trailing "()" from coercion types like "Int()"
                        let method_name = tc.strip_suffix("()").unwrap_or(tc);
                        Expr::MethodCall {
                            target: Box::new(extract_expr),
                            name: Symbol::intern(method_name),
                            args: Vec::new(),
                            modifier: None,
                            quoted: false,
                        }
                    } else {
                        extract_expr
                    };
                    // `is copy` on a sub-signature parameter is handled
                    // implicitly: the VarDecl creates a fresh writable variable,
                    // so no explicit trait is needed.
                    with_body.push(Stmt::VarDecl {
                        name: sub.name.clone(),
                        expr: coerced_expr,
                        type_constraint: None,
                        is_state: false,
                        is_our: false,
                        is_dynamic: false,
                        is_export: false,
                        export_tags: Vec::new(),
                        custom_traits: Vec::new(),
                        where_constraint: None,
                    });
                }
            } else {
                // Simple parameter with possible traits from parse_for_params
                with_body.push(Stmt::VarDecl {
                    name: pname.clone(),
                    expr: tmp_var.clone(),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
                });
            }
        } else {
            with_body.push(Stmt::VarDecl {
                name: pname.clone(),
                expr: tmp_var.clone(),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            });
        }
    }
    if use_given_alias {
        let mut given_body = body;
        // For a container/scalar pointy param, prepend the alias/copy bind
        // (`@p := $_` for aliasing, `@p = $_.list` for `is copy`) so the `given`
        // mechanism handles topic-source writeback (alias) or a flattened fresh
        // copy (matching `given @a -> @p`).
        if pointy_routes_through_given && let Some(ref pd) = param_def {
            given_body.insert(0, pointy_topic_bind(pd));
        }
        with_body = vec![Stmt::Given {
            topic: cond_expr.clone(),
            body: given_body,
        }];
    } else if route_through_given_tmp {
        with_body = vec![Stmt::Given {
            topic: tmp_var.clone(),
            body,
        }];
    } else {
        with_body.extend(body);
    }

    // Build the condition as (my $tmp = cond_expr).defined() (or
    // !(my $tmp = cond_expr).defined() for without). The DoStmt(VarDecl)
    // evaluates cond_expr once, declares $tmp in the current scope, and
    // returns the value. Then .defined() checks it. The body uses $tmp
    // for topicalization instead of re-evaluating cond_expr.
    let var_decl_expr = Expr::DoStmt(Box::new(Stmt::VarDecl {
        name: tmp_name.clone(),
        expr: cond_expr.clone(),
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    }));
    let defined_check = Expr::MethodCall {
        target: Box::new(var_decl_expr),
        name: Symbol::intern("defined"),
        args: Vec::new(),
        modifier: None,
        quoted: false,
    };
    let cond = if is_without {
        Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(defined_check),
        }
    } else {
        defined_check
    };
    // Parse orwith / else chains
    let rest_before_ws = rest;
    let (rest, _) = ws(rest)?;

    // `without` cannot have `else`, `orwith`, or `elsif` chains (Raku spec)
    if is_without {
        for kw in &["else", "orwith", "elsif"] {
            if keyword(kw, rest).is_some() {
                return Err(PError::fatal(format!(
                    "X::Syntax::WithoutElse: '{}' is not allowed on 'without'",
                    kw
                )));
            }
        }
    }

    let (rest, else_branch) = if keyword("orwith", rest).is_some() {
        let r = keyword("orwith", rest).unwrap();
        let (r, _) = ws1(r)?;
        let (r, orwith_cond_expr) = condition_expr(r)?;
        let (r, _) = ws(r)?;

        // Check for optional pointy block on orwith: orwith EXPR -> $param { ... }
        let (r, orwith_param_name) = if let Some(r2) = r.strip_prefix("->") {
            let (r2, _) = ws(r2)?;
            if let Some(r_after_sigil) = r2.strip_prefix('$') {
                let end = r_after_sigil
                    .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                    .unwrap_or(r_after_sigil.len());
                let name = &r_after_sigil[..end];
                let r2 = &r_after_sigil[end..];
                let (r2, _) = ws(r2)?;
                (r2, Some(name.to_string()))
            } else if let Some(r_after_backslash) = r2.strip_prefix('\\') {
                let end = r_after_backslash
                    .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                    .unwrap_or(r_after_backslash.len());
                let name = &r_after_backslash[..end];
                let r2 = &r_after_backslash[end..];
                let (r2, _) = ws(r2)?;
                (r2, Some(name.to_string()))
            } else {
                (r, None)
            }
        } else {
            (r, None)
        };

        let (r, orwith_body) = block(r)?;

        // Prepend $_ = <orwith_cond_expr> to the body
        let mut orwith_with_body = vec![topicalize(&orwith_cond_expr)];
        if let Some(ref pname) = orwith_param_name {
            orwith_with_body.push(Stmt::VarDecl {
                name: pname.clone(),
                expr: orwith_cond_expr.clone(),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            });
        }
        orwith_with_body.extend(orwith_body);

        let orwith_cond_expr_clone = orwith_cond_expr.clone();
        let orwith_cond = Expr::MethodCall {
            target: Box::new(orwith_cond_expr),
            name: Symbol::intern("defined"),
            args: Vec::new(),
            modifier: None,
            quoted: false,
        };
        let r_before_else_ws = r;
        let (r, _) = ws(r)?;
        let (r, orwith_else) = if keyword("else", r).is_some() {
            let r2 = keyword("else", r).unwrap();
            let (r2, _) = ws(r2)?;
            // Check for optional pointy block on else: else -> $param { ... }
            let (r2, else_param) = if let Some(r3) = r2.strip_prefix("->") {
                let (r3, _) = ws(r3)?;
                if let Some(r_after_sigil) = r3.strip_prefix('$') {
                    let end = r_after_sigil
                        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                        .unwrap_or(r_after_sigil.len());
                    let name = &r_after_sigil[..end];
                    let r3 = &r_after_sigil[end..];
                    let (r3, _) = ws(r3)?;
                    (r3, Some(name.to_string()))
                } else {
                    (r2, None)
                }
            } else {
                (r2, None)
            };
            let (r2, else_body) = block(r2)?;
            // Topicalize $_ in else branch to the orwith condition
            let mut else_with_topic = vec![topicalize(&orwith_cond_expr_clone)];
            if let Some(ref pname) = else_param {
                else_with_topic.push(Stmt::VarDecl {
                    name: pname.clone(),
                    expr: orwith_cond_expr_clone.clone(),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
                });
            }
            else_with_topic.extend(else_body);
            (r2, else_with_topic)
        } else {
            (r_before_else_ws, Vec::new())
        };
        (
            r,
            vec![Stmt::If {
                cond: orwith_cond,
                then_branch: orwith_with_body,
                else_branch: orwith_else,
                binding_var: None,
            }],
        )
    } else if keyword("elsif", rest).is_some() {
        // Handle elsif/else chains (reuse if-chain infrastructure)
        let (r, (elsif_clauses, else_clause)) = parse_elsif_chain(rest)?;
        let elsif_stmt = lower_if_chain(elsif_clauses, else_clause);
        (r, vec![elsif_stmt])
    } else if keyword("else", rest).is_some() {
        let r = keyword("else", rest).unwrap();
        let (r, _) = ws(r)?;
        // Check for optional pointy block on else: else -> $param { ... }
        let (r, else_param) = if let Some(r2) = r.strip_prefix("->") {
            let (r2, _) = ws(r2)?;
            if let Some(r_after_sigil) = r2.strip_prefix('$') {
                let end = r_after_sigil
                    .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                    .unwrap_or(r_after_sigil.len());
                let name = &r_after_sigil[..end];
                let r2 = &r_after_sigil[end..];
                let (r2, _) = ws(r2)?;
                (r2, Some(name.to_string()))
            } else {
                (r, None)
            }
        } else {
            (r, None)
        };
        let (r, else_body) = block(r)?;
        // Topicalize $_ in else branch to the with/without condition
        let mut else_with_topic = vec![topicalize(&tmp_var)];
        if let Some(ref pname) = else_param {
            else_with_topic.push(Stmt::VarDecl {
                name: pname.clone(),
                expr: tmp_var.clone(),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            });
        }
        else_with_topic.extend(else_body);
        (r, else_with_topic)
    } else {
        // No orwith/else found — don't consume whitespace/newlines past the
        // closing brace, so the caller sees the statement boundary correctly
        // (e.g. `do with X { ... }\nsay ...` should not absorb `say`).
        (rest_before_ws, Vec::new())
    };

    Ok((
        rest,
        Stmt::If {
            cond,
            then_branch: with_body,
            else_branch,
            binding_var: None,
        },
    ))
}
