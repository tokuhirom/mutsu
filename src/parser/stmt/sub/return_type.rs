use super::*;

/// Skip a return type annotation (--> Type) in a signature.
/// Consumes whitespace, a type name (possibly with :D/:U), and any trailing whitespace.
pub(crate) fn skip_return_type_annotation(input: &str) -> Result<&str, PError> {
    let (rest, _type_name) = parse_return_type_annotation(input)?;
    Ok(rest)
}

/// Parse a return type annotation (--> Type) and return both remainder and type name.
pub(crate) fn parse_return_type_annotation(input: &str) -> PResult<'_, String> {
    let (rest, _) = ws(input)?;
    let bytes = rest.as_bytes();
    let mut idx = 0usize;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut brace_depth = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;

    while idx < bytes.len() {
        let b = bytes[idx];

        if in_single {
            if b == b'\'' {
                in_single = false;
            }
            idx += 1;
            continue;
        }
        if in_double {
            if escaped {
                escaped = false;
            } else if b == b'\\' {
                escaped = true;
            } else if b == b'"' {
                in_double = false;
            }
            idx += 1;
            continue;
        }

        match b {
            b'\'' => {
                in_single = true;
                idx += 1;
            }
            b'"' => {
                in_double = true;
                idx += 1;
            }
            b'(' => {
                paren_depth += 1;
                idx += 1;
            }
            b')' => {
                if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
                    break;
                }
                paren_depth = paren_depth.saturating_sub(1);
                idx += 1;
            }
            b'[' => {
                bracket_depth += 1;
                idx += 1;
            }
            b']' => {
                bracket_depth = bracket_depth.saturating_sub(1);
                idx += 1;
            }
            b'{' => {
                if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
                    break;
                }
                brace_depth += 1;
                idx += 1;
            }
            b'}' => {
                if brace_depth == 0 {
                    break;
                }
                brace_depth -= 1;
                idx += 1;
            }
            // A `,` or `;` at signature-depth ends the return constraint. Since a
            // `-->` return constraint must be the LAST element of the signature,
            // anything after the break is a stray parameter — the caller then
            // fails with a malformed-signature error (return constraints only
            // allowed at the end), matching Raku's X::Syntax::Malformed.
            b',' | b';' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => break,
            _ => idx += 1,
        }
    }

    let annotation = rest[..idx].trim().to_string();
    if annotation.is_empty() {
        return Err(PError::expected("return type annotation"));
    }
    // Detect multiple prefix constraints in return type (e.g. `--> Int Str`)
    if let Some(space_pos) = annotation.find(' ') {
        let first = &annotation[..space_pos];
        let second = annotation[space_pos..].trim();
        if !first.is_empty()
            && !second.is_empty()
            && first.chars().next().is_some_and(|c| c.is_ascii_uppercase())
            && second
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
            && first
                .chars()
                .all(|c| c.is_alphanumeric() || c == ':' || c == '_')
            && second
                .chars()
                .all(|c| c.is_alphanumeric() || c == ':' || c == '_')
        {
            return Err(PError::raw(
                "FATAL:Multiple prefix constraints not yet implemented. Sorry.".to_string(),
                Some(rest[idx..].len()),
            ));
        }
    }
    // Normalize parameterizing `of` in the return type, e.g.
    // `--> Array of Str` means return type `Array[Str]`.
    let annotation = if let Some(of_pos) = annotation.find(" of ")
        && !annotation.contains('[')
    {
        let base = annotation[..of_pos].trim();
        let inner = annotation[of_pos + 4..].trim();
        if !base.is_empty() && !inner.is_empty() {
            format!("{base}[{inner}]")
        } else {
            annotation
        }
    } else {
        annotation
    };
    // Validate type smiley in return type annotation
    super::super::sub_param::check_invalid_type_smiley(&Some(annotation.clone()))?;
    let (tail, _) = ws(&rest[idx..])?;
    Ok((tail, annotation))
}

/// Parse parameter list with return type annotation.
/// Returns (remaining input, params, optional return type).
pub(crate) fn parse_param_list_with_return(
    input: &str,
) -> PResult<'_, (Vec<ParamDef>, Option<String>)> {
    let (rest, (params, return_type)) = parse_param_list_with_return_inner(input)?;
    check_duplicate_params(&params)?;
    Ok((rest, (params, return_type)))
}

pub(crate) fn parse_param_list_with_return_inner(
    input: &str,
) -> PResult<'_, (Vec<ParamDef>, Option<String>)> {
    let mut params = Vec::new();
    let mut return_type = None;
    let mut multi_invocant = true;
    let mut rest = input;
    if rest.starts_with(')') {
        return Ok((rest, (params, None)));
    }
    if let Some(stripped) = rest.strip_prefix("-->") {
        let (r, rt) = parse_return_type_annotation(stripped)?;
        return Ok((r, (params, Some(rt))));
    }
    if let Some(r) = rest.strip_prefix(";;") {
        multi_invocant = false;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, (params, return_type)));
        }
        rest = r;
    }
    if let Some((r, _invocant_type)) = parse_implicit_invocant_marker(rest) {
        let (r, _) = ws(r)?;
        rest = r;
        if rest.starts_with(')') {
            return Ok((rest, (params, return_type)));
        }
        // A typed invocant may be followed directly by the return constraint:
        // `method m(Foo:D: --> Str)` has no positional params after the invocant.
        if let Some(stripped) = rest.strip_prefix("-->") {
            let (r, rt) = parse_return_type_annotation(stripped)?;
            return Ok((r, (params, Some(rt))));
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
                return Ok((r, (params, return_type)));
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
        if let Some(r) = r.strip_prefix(':') {
            // Mark all params parsed so far as invocant
            for p in params.iter_mut() {
                p.is_invocant = true;
            }
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                mark_params_as_invocant(&mut params);
                return Ok((r, (params, return_type)));
            }
            // Handle --> return type after invocant marker
            if let Some(stripped) = r.strip_prefix("-->") {
                mark_params_as_invocant(&mut params);
                let (r, rt) = parse_return_type_annotation(stripped)?;
                return_type = Some(rt);
                return Ok((r, (params, return_type)));
            }
            mark_params_as_invocant(&mut params);
            let (r, p) = parse_single_param(r)?;
            params.push(p);
            rest = r;
            continue;
        }
        if let Some(r) = r.strip_prefix(';') {
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                return Ok((r, (params, return_type)));
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
            if let Some(stripped) = r.strip_prefix("-->") {
                let (r, rt) = parse_return_type_annotation(stripped)?;
                return_type = Some(rt);
                return Ok((r, (params, return_type)));
            }
            return Ok((r, (params, return_type)));
        }
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(')') {
            return Ok((r, (params, return_type)));
        }
        if let Some(stripped) = r.strip_prefix("-->") {
            let (r, rt) = parse_return_type_annotation(stripped)?;
            return_type = Some(rt);
            return Ok((r, (params, return_type)));
        }
        let (r, mut p) = parse_single_param(r)?;
        p.multi_invocant = multi_invocant;
        params.push(p);
        rest = r;
    }
}
