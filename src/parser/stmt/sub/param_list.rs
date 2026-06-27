use super::*;

/// Build an `X::Parameter::InvalidType` for a `:D`/`:U`/`:_` smiley that trails
/// a parameter after whitespace (`$x :D`) instead of attaching to a type
/// (`Int:D $x`). Raku reports the smiley letter as a bogus typename.
pub(crate) fn invalid_param_smiley_error(smiley: &str) -> PError {
    let msg = format!("Invalid typename '{}' in parameter declaration.", smiley);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("typename".to_string(), Value::str(smiley.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Parameter::InvalidType"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

/// Strip sigil prefix from a parameter name, returning the bare name.
pub(crate) fn strip_param_sigil(name: &str) -> &str {
    name.strip_prefix('@')
        .or_else(|| name.strip_prefix('%'))
        .or_else(|| name.strip_prefix('&'))
        .unwrap_or(name)
}

/// Check for duplicate parameter names in a signature.
/// Anonymous parameters (e.g. `$`, `@`, `%`) are excluded.
pub(crate) fn check_duplicate_params(params: &[ParamDef]) -> Result<(), PError> {
    let mut seen = std::collections::HashSet::new();
    // Track named parameter base names (without sigils) to detect NameClash
    let mut seen_named_bases = std::collections::HashMap::<String, String>::new();

    // Collect all variable names including those from sub-signatures of renamed params
    // (e.g. :foo($x) produces variable $x which must not clash with :$x)
    let mut all_var_names: Vec<String> = Vec::new();

    for p in params {
        let name = &p.name;
        let name_without_sigil = strip_param_sigil(name);
        if name.is_empty()
            || name_without_sigil.starts_with("__ANON_")
            || name == "__type_only__"
            || name == "__literal__"
            || name == "__subsig__"
        {
            continue;
        }
        // Reconstruct the display name with sigil for error message
        let display_name = if name.starts_with("__type_capture__") {
            name.strip_prefix("__type_capture__")
                .unwrap_or(name)
                .to_string()
        } else if name.starts_with('@')
            || name.starts_with('%')
            || name.starts_with('&')
            || p.sigilless
        {
            name.clone()
        } else {
            format!("${}", name)
        };
        if !seen.insert(display_name.clone()) {
            let msg = format!(
                "X::Redeclaration: Redeclaration of symbol '{}'",
                display_name
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("symbol".to_string(), Value::str(display_name.clone()));
            attrs.insert("what".to_string(), Value::str("symbol".to_string()));
            let ex = Value::make_instance(Symbol::intern("X::Redeclaration"), attrs);
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }

        // For named params, check that the base name (without sigil) is unique
        // e.g. :$a and :@a share base name "a" → X::Signature::NameClash
        if p.named {
            let base = name_without_sigil.to_string();
            if !base.is_empty() {
                if let Some(prev_display) = seen_named_bases.get(&base) {
                    if *prev_display != display_name {
                        let msg = format!(
                            "X::Signature::NameClash: Name {} used for more than one named parameter",
                            base
                        );
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("name".to_string(), Value::str(base));
                        attrs.insert("message".to_string(), Value::str(msg.clone()));
                        let ex =
                            Value::make_instance(Symbol::intern("X::Signature::NameClash"), attrs);
                        return Err(PError::fatal_with_exception(msg, Box::new(ex)));
                    }
                } else {
                    seen_named_bases.insert(base, display_name.clone());
                }
            }
        }

        // Collect the variable name for this param. For a *renamed* named param
        // (`:key($key)`), the outer `p.name` ("key") is only the external
        // argument key — the bound variable is the inner sub-signature param
        // (`$key`), collected below. Counting both made `:key($key)` look like a
        // self-redeclaration of `$key`.
        if !(p.named && p.sub_signature.is_some()) {
            all_var_names.push(display_name);
        }

        // For named params with sub-signature (renamed params like :foo($x)),
        // also collect the inner variable names
        if p.named
            && let Some(sub_params) = &p.sub_signature
        {
            // The outer param's rename name (e.g. `:in` in `:in(:$in)`) is an
            // external named-argument key. If a sub-param is ALSO named with the
            // same key (`:$in` -> external name "in"), the key is used for more
            // than one named parameter -> X::Signature::NameClash (not a plain
            // variable Redeclaration).
            let outer_base = strip_param_sigil(&p.name);
            for sp in sub_params {
                if sp.named && !outer_base.is_empty() && strip_param_sigil(&sp.name) == outer_base {
                    let msg = format!(
                        "X::Signature::NameClash: Name {} used for more than one named parameter",
                        outer_base
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("name".to_string(), Value::str(outer_base.to_string()));
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    let ex = Value::make_instance(Symbol::intern("X::Signature::NameClash"), attrs);
                    return Err(PError::fatal_with_exception(msg, Box::new(ex)));
                }
            }
            for sp in sub_params {
                let sp_name = &sp.name;
                let sp_without_sigil = strip_param_sigil(sp_name);
                if sp_name.is_empty() || sp_without_sigil.starts_with("__ANON_") {
                    continue;
                }
                let sp_display = if sp_name.starts_with('@')
                    || sp_name.starts_with('%')
                    || sp_name.starts_with('&')
                    || sp.sigilless
                {
                    sp_name.clone()
                } else {
                    format!("${}", sp_name)
                };
                all_var_names.push(sp_display);
            }
        }
    }

    // Check for duplicate variable names across all params including sub-signature vars
    let mut var_seen = std::collections::HashSet::new();
    for vn in &all_var_names {
        if !var_seen.insert(vn.clone()) {
            let msg = format!("X::Redeclaration: Redeclaration of symbol '{}'", vn);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("symbol".to_string(), Value::str(vn.clone()));
            attrs.insert("what".to_string(), Value::str("symbol".to_string()));
            let ex = Value::make_instance(Symbol::intern("X::Redeclaration"), attrs);
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
    }

    Ok(())
}

/// Parse parameter list inside parens.
pub(crate) fn parse_param_list(input: &str) -> PResult<'_, Vec<ParamDef>> {
    let (rest, params) = parse_param_list_inner(input)?;
    check_duplicate_params(&params)?;
    Ok((rest, params))
}

pub(crate) fn parse_param_list_inner(input: &str) -> PResult<'_, Vec<ParamDef>> {
    let mut params = Vec::new();
    let mut multi_invocant = true;
    let mut rest = input;
    if rest.starts_with(')') || rest.starts_with(']') {
        return Ok((rest, params));
    }
    // Handle --> return type at the start (no params, just return type)
    if let Some(stripped) = rest.strip_prefix("-->") {
        let r = skip_return_type_annotation(stripped)?;
        return Ok((r, params));
    }
    if let Some(r) = rest.strip_prefix(";;") {
        multi_invocant = false;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, params));
        }
        rest = r;
    }
    if let Some((r, _invocant_type)) = parse_implicit_invocant_marker(rest) {
        rest = r;
        if rest.starts_with(')') {
            return Ok((rest, params));
        }
        let (r, mut p) = parse_single_param(rest)?;
        p.multi_invocant = multi_invocant;
        params.push(p);
        rest = r;
    } else {
        let (r, mut p) = parse_single_param(rest)?;
        p.multi_invocant = multi_invocant;
        params.push(p);
        rest = r;
    }
    loop {
        let (r, _) = ws(rest)?;
        if let Some(r) = r.strip_prefix(";;") {
            multi_invocant = false;
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                mark_params_as_invocant(&mut params);
                return Ok((r, params));
            }
            let (r, mut p) = parse_single_param(r)?;
            p.multi_invocant = multi_invocant;
            params.push(p);
            rest = r;
            continue;
        }
        // A `:D`/`:U`/`:_` definedness smiley must attach to a TYPE
        // (`Int:D $x`), never trail a parameter after whitespace (`$x :D` or
        // `Int $x :D`). Such a trailing smiley is X::Parameter::InvalidType
        // ("Invalid typename '<X>'"), not an invocant `:` marker — catch it
        // before the invocant handling below would mis-read the `:`.
        if (r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_"))
            && r[2..]
                .chars()
                .next()
                .is_none_or(|c| !c.is_alphanumeric() && c != '_')
        {
            return Err(invalid_param_smiley_error(&r[1..2]));
        }
        // Handle invocant marker ':'
        if let Some(r) = r.strip_prefix(':') {
            // Mark all params parsed so far as invocant
            for p in params.iter_mut() {
                p.is_invocant = true;
            }
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                mark_params_as_invocant(&mut params);
                return Ok((r, params));
            }
            // Handle --> return type after invocant marker
            if let Some(stripped) = r.strip_prefix("-->") {
                mark_params_as_invocant(&mut params);
                let r = skip_return_type_annotation(stripped)?;
                return Ok((r, params));
            }
            mark_params_as_invocant(&mut params);
            let (r, p) = parse_single_param(r)?;
            params.push(p);
            rest = r;
            continue;
        }
        // Handle invocant separator ';'
        if let Some(r) = r.strip_prefix(';') {
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                return Ok((r, params));
            }
            let (r, mut p) = parse_single_param(r)?;
            p.multi_invocant = multi_invocant;
            params.push(p);
            rest = r;
            continue;
        }
        if !r.starts_with(',')
            && starts_with_sigil_param(r)
            && params.last().is_some_and(is_anonymous_sigil_param)
        {
            let (r, mut p) = parse_single_param(r)?;
            p.multi_invocant = multi_invocant;
            params.push(p);
            rest = r;
            continue;
        }
        if !r.starts_with(',') {
            // Check for --> return type annotation at end of param list
            if let Some(stripped) = r.strip_prefix("-->") {
                let r = skip_return_type_annotation(stripped)?;
                return Ok((r, params));
            }
            return Ok((r, params));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, params));
        }
        // Handle --> return type after comma
        if let Some(stripped) = r.strip_prefix("-->") {
            let r = skip_return_type_annotation(stripped)?;
            return Ok((r, params));
        }
        let (r, mut p) = parse_single_param(r)?;
        p.multi_invocant = multi_invocant;
        params.push(p);
        rest = r;
    }
}
