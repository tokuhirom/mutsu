use super::*;

pub(crate) fn parse_indirect_decl_name(input: &str) -> PResult<'_, (String, Expr)> {
    let rest = input
        .strip_prefix("::")
        .ok_or_else(|| PError::expected("indirect declarator name"))?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = parse_char(rest, '(')?;
    let (rest, _) = ws(rest)?;
    let (rest, expr) = expression(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = parse_char(rest, ')')?;
    let name = match &expr {
        Expr::Literal(Value::Str(s)) => s.to_string(),
        Expr::BareWord(s) => s.clone(),
        _ => "__INDIRECT_DECL_NAME__".to_string(),
    };
    Ok((rest, (name, expr)))
}

/// Parse `sub` declaration.
pub(crate) fn sub_decl(input: &str) -> PResult<'_, Stmt> {
    sub_decl_with_semicolon_mode(input, false)
}

pub(crate) fn top_level_main_semicolon_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest_after_prefix, _) = if let Some(r) = keyword("unit", input) {
        let (r, _) = ws1(r)?;
        (r, true)
    } else {
        (input, false)
    };
    let input_len = rest_after_prefix.len();
    let (rest, stmt) = sub_decl_with_semicolon_mode(rest_after_prefix, true)?;
    let consumed_len = input_len.saturating_sub(rest.len());
    let consumed = &rest_after_prefix[..consumed_len];
    let has_semicolon_terminator = consumed.trim_end().ends_with(';');
    if has_semicolon_terminator
        && matches!(&stmt, Stmt::SubDecl { name, multi, .. } if name == "MAIN" && !*multi)
    {
        Ok((rest, stmt))
    } else {
        Err(PError::expected("unit-scoped MAIN sub declaration"))
    }
}

/// Detect anonymous routines declared with `only`, `multi`, or `proto` and
/// throw a fatal X::Anon::Multi parse error, matching Raku's behavior.
pub(crate) fn anon_multi_check(input: &str) -> PResult<'_, Stmt> {
    // Try each declarator keyword
    let declarators = ["only", "multi", "proto"];
    for declarator in &declarators {
        if let Some(after_kw) = keyword(declarator, input) {
            // After the declarator, optionally consume whitespace + routine type
            let after_kw_ws = ws(after_kw).map(|(r, _)| r).unwrap_or(after_kw);
            let after_type = if let Some(r) = keyword("sub", after_kw_ws) {
                ws(r).map(|(r, _)| r).unwrap_or(r)
            } else if let Some(r) = keyword("method", after_kw_ws) {
                ws(r).map(|(r, _)| r).unwrap_or(r)
            } else {
                after_kw_ws
            };
            // If what follows is `{`, this is an anonymous routine — fatal error.
            if after_type.starts_with('{') {
                return Err(PError::fatal(format!(
                    "FATAL:X::Anon::Multi: An anonymous routine may not take a {} declarator",
                    declarator
                )));
            }
            // If followed by `(`, it could be a function call like `multi()` after
            // `sub multi {}` was defined. Only emit fatal if the `after_type` is
            // the same as `after_kw_ws` (i.e., there was no routine type keyword
            // like `sub`/`method` between the declarator and `(`). When a routine
            // type IS present (e.g. `multi sub (...)`), it's definitely a declaration.
            if after_type.starts_with('(') && after_type != after_kw_ws {
                return Err(PError::fatal(format!(
                    "FATAL:X::Anon::Multi: An anonymous routine may not take a {} declarator",
                    declarator
                )));
            }
            // `(` directly after the declarator keyword could be a function call;
            // don't emit fatal — allow fallback to expression parsing.
        }
    }
    Err(PError::expected("anonymous multi check"))
}

pub(crate) fn sub_decl_with_semicolon_mode(
    input: &str,
    allow_main_semicolon_decl: bool,
) -> PResult<'_, Stmt> {
    let (input, supersede) = if let Some(r) = keyword("supersede", input) {
        let (r, _) = ws1(r)?;
        (r, true)
    } else {
        (input, false)
    };
    let (rest, multi) = if let Some(r) = keyword("multi", input) {
        let (r, _) = ws1(r)?;
        let r = keyword("sub", r).unwrap_or(r);
        let (r, _) = ws(r)?;
        (r, true)
    } else if let Some(r) = keyword("only", input) {
        // `only` is functionally equivalent to a regular sub declaration
        let (r, _) = ws1(r)?;
        let r = keyword("sub", r).unwrap_or(r);
        let (r, _) = ws(r)?;
        (r, false)
    } else {
        let r = keyword("sub", input).ok_or_else(|| PError::expected("sub declaration"))?;
        let (r, _) = ws1(r)?;
        (r, false)
    };
    sub_decl_body(rest, multi, supersede, allow_main_semicolon_decl)
}

pub(crate) fn sub_decl_body(
    input: &str,
    multi: bool,
    supersede: bool,
    allow_main_semicolon_decl: bool,
) -> PResult<'_, Stmt> {
    let (rest, name, name_expr) = if input.starts_with("::") {
        let (rest, (name, expr)) = parse_indirect_decl_name(input)?;
        (rest, name, Some(expr))
    } else {
        let (rest, name) = parse_sub_name(input)?;
        // Validate circumfix/postcircumfix operator part count
        validate_categorical_parts(&name)?;
        // Register user-declared sub so it can be called without parens later
        super::super::simple::register_user_sub(&name);
        super::super::simple::register_user_callable_term_symbol(&name);
        (rest, name, None)
    };
    let (rest, _) = ws(rest)?;

    // Parse params
    let has_explicit_signature = rest.starts_with('(');
    let (rest, (params, param_defs, return_type)) = if has_explicit_signature {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, (pd, rt)) = parse_param_list_with_return(r)?;
        validate_signature_params(&pd)?;
        reject_invocant_in_sub(&pd)?;
        reject_attr_params_in_sub(&pd)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd, rt))
    } else {
        (rest, (Vec::new(), Vec::new(), None))
    };

    let (rest, _) = ws(rest)?;
    // Parse traits (is test-assertion, is export, returns ..., etc.)
    let (rest, traits) = parse_sub_traits(rest)?;
    if let Some(assoc) = traits.associativity.as_ref() {
        super::super::simple::register_user_infix_assoc(&name, assoc);
    }
    if traits.is_test_assertion {
        super::super::simple::register_user_test_assertion_sub(&name);
    }
    // Register precedence trait if present
    if let Some((trait_name, ref_op)) = &traits.precedence_trait
        && let Some(ref_level) = super::super::simple::resolve_op_precedence(ref_op)
    {
        let level = match trait_name.as_str() {
            "tighter" => ref_level + 5,
            "looser" => ref_level - 5,
            _ => ref_level,
        };
        super::super::simple::register_op_precedence(&name, level);
    }
    let (rest, _) = ws(rest)?;
    let mut signature_alternates: Vec<(Vec<String>, Vec<ParamDef>)> = Vec::new();
    let rest = if multi {
        // Multi declarators can chain additional signatures with `| (...)`.
        let mut r = rest;
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with('|') {
                r = r2;
                break;
            }
            let (r3, _) = ws(&r2[1..])?;
            if !r3.starts_with('(') {
                return Err(PError::expected("alternate signature after '|'"));
            }
            let (r4, _) = parse_char(r3, '(')?;
            let (r4, _) = ws(r4)?;
            let (r4, alt_param_defs) = parse_param_list(r4)?;
            validate_signature_params(&alt_param_defs)?;
            let (r4, _) = ws(r4)?;
            let (r4, _) = parse_char(r4, ')')?;
            let alt_params: Vec<String> = alt_param_defs.iter().map(|p| p.name.clone()).collect();
            signature_alternates.push((alt_params, alt_param_defs));
            r = r4;
        }
        // Validate: all alternate signatures must have the same set of
        // formal variable names as the primary signature (S13).
        if !signature_alternates.is_empty() {
            let mut primary_names: Vec<&str> = params.iter().map(|s| s.as_str()).collect();
            primary_names.sort();
            for (alt_params, _alt_defs) in &signature_alternates {
                let mut alt_names: Vec<&str> = alt_params.iter().map(|s| s.as_str()).collect();
                alt_names.sort();
                if alt_names != primary_names {
                    return Err(PError::raw(
                        "Multis with multiple signatures must have the same set of formal variable names".to_string(),
                        None,
                    ));
                }
            }
        }
        r
    } else {
        rest
    };
    // Detect forward declaration: `sub name(...);` or the same declaration at
    // end-of-input with no body.
    if rest.trim().is_empty() || rest.starts_with(';') {
        let rest = if let Some(rest) = rest.strip_prefix(';') {
            rest
        } else {
            rest
        };
        // Merge return type for forward declarations too
        let fwd_return_type = return_type.clone().or(traits.return_type.clone());
        if allow_main_semicolon_decl && name == "MAIN" && !multi {
            return Ok((
                rest,
                Stmt::SubDecl {
                    name: Symbol::intern(&name),
                    name_expr,
                    params,
                    param_defs,
                    return_type: fwd_return_type,
                    associativity: traits.associativity.clone(),
                    precedence_trait: traits.precedence_trait.clone(),
                    signature_alternates,
                    body: Vec::new(),
                    multi,
                    is_rw: traits.is_rw,
                    is_raw: traits.is_raw,
                    is_export: traits.is_export,
                    export_tags: traits.export_tags.clone(),
                    is_test_assertion: traits.is_test_assertion,
                    supersede,
                    custom_traits: traits.custom_traits.clone(),
                },
            ));
        }
        if !has_explicit_signature {
            return Err(PError::raw(
                "X::UnitScope::Invalid: A unit-scoped sub definition is not allowed except on a MAIN sub; \
                 Please use the block form. If you did not mean to declare a unit-scoped sub, \
                 perhaps you accidentally placed a semicolon after routine's definition?"
                    .to_string(),
                Some(rest.len()),
            ));
        }
        if name == "MAIN" && !allow_main_semicolon_decl {
            return Err(PError::raw(
                "X::UnitScope::Invalid: A unit-scoped sub definition is not allowed except on a MAIN sub; \
                 Please use the block form. If you did not mean to declare a unit-scoped sub, \
                 perhaps you accidentally placed a semicolon after routine's definition?"
                    .to_string(),
                Some(rest.len()),
            ));
        }
        return Ok((
            rest,
            Stmt::SubDecl {
                name: Symbol::intern(&name),
                name_expr,
                params,
                param_defs,
                return_type: fwd_return_type,
                associativity: traits.associativity.clone(),
                precedence_trait: traits.precedence_trait.clone(),
                signature_alternates,
                body: Vec::new(),
                multi,
                is_rw: traits.is_rw,
                is_raw: traits.is_raw,
                is_export: traits.is_export,
                export_tags: traits.export_tags.clone(),
                is_test_assertion: traits.is_test_assertion,
                supersede,
                custom_traits: traits.custom_traits.clone(),
            },
        ));
    }
    if rest.is_empty() {
        return Err(PError::expected("sub body '{ ... }'"));
    }
    let (rest, body) = if param_defs.iter().any(|p| p.sigilless) {
        // When there are sigilless params, we need to register them as term
        // symbols in the block scope so they shadow keywords (e.g. \return).
        let (r, _) = parse_char(rest, '{')?;
        super::super::simple::push_scope();
        for pd in &param_defs {
            if pd.sigilless {
                super::super::simple::register_user_term_symbol(&pd.name);
            }
        }
        let result = block_inner(r);
        super::super::simple::pop_scope();
        result?
    } else {
        match block(rest) {
            Ok(ok) => ok,
            Err(_) if name.starts_with("trait_auxiliary:<") => consume_raw_sub_body(rest)?,
            Err(err) => return Err(err),
        }
    };
    // A routine declared with an explicit signature (`sub f() { ... }`, even an
    // empty one) cannot also use placeholder variables in its body — that would
    // override the existing signature. This is X::Signature::Placeholder.
    if has_explicit_signature
        && let Some(err) = placeholder_overrides_signature_error(&body, &param_defs)
    {
        return Err(err);
    }
    // When no explicit signature is given, collect placeholder variables
    // ($^a, $^b, &^c, etc.) from the body as implicit parameters.
    let (params, param_defs) = if params.is_empty() && param_defs.is_empty() {
        let placeholders = collect_placeholders_shallow(&body);
        if placeholders.is_empty() {
            (params, param_defs)
        } else {
            (placeholders, Vec::new())
        }
    } else {
        (params, param_defs)
    };
    // Merge return type: `-->` from inside params has priority, then `returns`/`of` traits.
    // Declaring the return type twice (e.g. `sub f(--> List) returns Str { }`) is
    // X::Redeclaration of the return type.
    if let (Some(inner), Some(trait_rt)) = (&return_type, &traits.return_type) {
        let msg = format!(
            "X::Redeclaration: Redeclaration of return type for '{}' (previous return type was {}).",
            name, inner
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("symbol".to_string(), Value::str(name.clone()));
        attrs.insert("what".to_string(), Value::str("return type".to_string()));
        let _ = trait_rt;
        let ex = Value::make_instance(Symbol::intern("X::Redeclaration"), attrs);
        return Err(PError::fatal_with_exception(msg, Box::new(ex)));
    }
    let merged_return_type = return_type.or(traits.return_type);
    let sub_name_sym = Symbol::intern(&name);
    let sub_decl = Stmt::SubDecl {
        name: sub_name_sym,
        name_expr,
        params,
        param_defs,
        return_type: merged_return_type,
        associativity: traits.associativity,
        precedence_trait: traits.precedence_trait,
        signature_alternates,
        body,
        multi,
        is_rw: traits.is_rw,
        is_raw: traits.is_raw,
        is_export: traits.is_export,
        export_tags: traits.export_tags,
        is_test_assertion: traits.is_test_assertion,
        supersede,
        custom_traits: traits.custom_traits,
    };
    // Check for immediate invocation: `sub foo(...) { ... }(args)`.
    // Only if `(` follows without a newline (same-line invocation).
    let has_newline = rest[..rest.len().min(rest.find('(').unwrap_or(rest.len()))].contains('\n');
    if !has_newline
        && rest.starts_with('(')
        && let Ok((r, call_args)) = super::super::parse_stmt_call_args(rest)
    {
        let positional_args: Vec<Expr> = call_args
            .into_iter()
            .filter_map(|arg| match arg {
                crate::ast::CallArg::Positional(e) => Some(e),
                crate::ast::CallArg::Slip(e) => Some(Expr::Unary {
                    op: crate::token_kind::TokenKind::Pipe,
                    expr: Box::new(e),
                }),
                _ => None,
            })
            .collect();
        let call_expr = Stmt::Expr(Expr::Call {
            name: sub_name_sym,
            args: positional_args,
        });
        return Ok((r, Stmt::SyntheticBlock(vec![sub_decl, call_expr])));
    }
    Ok((rest, sub_decl))
}

pub(crate) fn consume_raw_sub_body(input: &str) -> PResult<'_, Vec<Stmt>> {
    if !input.starts_with('{') {
        return Err(PError::expected("sub body"));
    }
    let mut depth = 0u32;
    let mut i = 0usize;
    while i < input.len() {
        let ch = input[i..]
            .chars()
            .next()
            .ok_or_else(|| PError::expected("closing '}'"))?;
        let len = ch.len_utf8();
        match ch {
            '{' => depth += 1,
            '}' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Ok((&input[i + len..], Vec::new()));
                }
            }
            '\\' => {
                i += len;
                if i < input.len() {
                    let next_len = input[i..].chars().next().map(|c| c.len_utf8()).unwrap_or(0);
                    i += next_len;
                    continue;
                }
            }
            '\'' | '"' => {
                let quote = ch;
                i += len;
                while i < input.len() {
                    let c = input[i..]
                        .chars()
                        .next()
                        .ok_or_else(|| PError::expected("string close"))?;
                    let c_len = c.len_utf8();
                    if c == '\\' {
                        i += c_len;
                        if i < input.len() {
                            let n_len =
                                input[i..].chars().next().map(|n| n.len_utf8()).unwrap_or(0);
                            i += n_len;
                            continue;
                        }
                    }
                    i += c_len;
                    if c == quote {
                        break;
                    }
                }
                continue;
            }
            _ => {}
        }
        i += len;
    }
    Err(PError::expected("closing '}'"))
}
