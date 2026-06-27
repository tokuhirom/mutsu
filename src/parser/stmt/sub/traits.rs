use super::*;

/// Result of parsing sub traits.
pub(crate) struct SubTraits {
    pub is_export: bool,
    pub export_tags: Vec<String>,
    pub is_test_assertion: bool,
    pub is_rw: bool,
    pub is_raw: bool,
    pub return_type: Option<String>,
    pub associativity: Option<String>,
    /// Non-builtin trait names (e.g. `me'd`) for custom `trait_mod:<is>` dispatch,
    /// with optional argument expression.
    pub custom_traits: Vec<(String, Option<crate::ast::Expr>)>,
    /// Precedence trait: (trait_name, reference_operator).
    /// trait_name is one of "tighter", "looser", "equiv".
    /// reference_operator is the operator symbol or full name (e.g. "*", "+", "infix:<+>", "prefix:<foo>").
    pub precedence_trait: Option<(String, String)>,
    /// `handles` specifications on a method declaration, e.g.
    /// `method Str() handles 'uc' { ... }`.
    pub handles: Vec<crate::ast::HandleSpec>,
}

/// Parse sub/method traits like `is test-assertion`, `is export`, `returns Str`, `of Num`, etc.
/// Returns `SubTraits` indicating which traits were found.
pub(crate) fn parse_sub_traits(mut input: &str) -> PResult<'_, SubTraits> {
    let mut is_export = false;
    let mut export_tags: Vec<String> = Vec::new();
    let mut is_test_assertion = false;
    let mut is_rw = false;
    let mut is_raw = false;
    let mut return_type = None;
    let mut associativity = None;
    let mut custom_traits: Vec<(String, Option<crate::ast::Expr>)> = Vec::new();
    let mut seen_traits: Vec<String> = Vec::new();
    let mut precedence_trait: Option<(String, String)> = None;
    let mut handles: Vec<crate::ast::HandleSpec> = Vec::new();
    loop {
        let (r, _) = ws(input)?;
        if r.starts_with('{') || r.is_empty() {
            return Ok((
                r,
                SubTraits {
                    is_export,
                    export_tags,
                    is_test_assertion,
                    is_rw,
                    is_raw,
                    return_type,
                    associativity,
                    custom_traits: custom_traits.clone(),
                    precedence_trait: precedence_trait.clone(),
                    handles: handles.clone(),
                },
            ));
        }
        if let Some(r_after) = keyword("handles", r) {
            let (r_after, _) = ws1(r_after)?;
            let mut rest_out = r_after;
            super::super::decl::parse_handle_specs(r_after, &mut handles, &mut rest_out)?;
            input = rest_out;
            continue;
        }
        if let Some(r) = keyword("is", r) {
            let (r, _) = ws(r)?;
            // Parse the trait name (Raku identifier: may include hyphens and apostrophes)
            let (r, trait_name) = parse_raku_ident(r)?;
            if seen_traits.contains(&trait_name.to_string()) {
                add_parse_warning(format!(
                    "Potential difficulties:\n    Duplicate 'is {}' trait",
                    trait_name
                ));
            }
            seen_traits.push(trait_name.to_string());
            if trait_name == "export" {
                is_export = true;
                let (r2, tags) = parse_export_trait_tags(r)?;
                if tags.is_empty() {
                    if !export_tags.iter().any(|t| t == "DEFAULT") {
                        export_tags.push("DEFAULT".to_string());
                    }
                } else {
                    for tag in tags {
                        if !export_tags.iter().any(|t| t == &tag) {
                            export_tags.push(tag);
                        }
                    }
                }
                input = r2;
                continue;
            } else if trait_name == "test-assertion" {
                is_test_assertion = true;
            } else if trait_name == "rw" {
                is_rw = true;
            } else if trait_name == "raw" {
                is_raw = true;
            } else if trait_name == "looser" || trait_name == "tighter" || trait_name == "equiv" {
                associativity = Some(trait_name.to_string());
            } else if trait_name == "DEPRECATED" {
                // Will capture parenthesized arg below
                custom_traits.push(("DEPRECATED".to_string(), None));
            } else if trait_name != "assoc"
                && trait_name != "equiv"
                && trait_name != "tighter"
                && trait_name != "looser"
                && trait_name != "readonly"
                && trait_name != "hidden-from-backtrace"
                && trait_name != "implementation-detail"
                && trait_name != "nodal"
                && trait_name != "pure"
            {
                // Placeholder — will be updated with arg below if present
                custom_traits.push((trait_name.to_string(), None));
            }
            let (mut r, _) = ws(r)?;
            if r.starts_with('<') {
                let (r2, arg) = parse_trait_angle_arg(r)?;
                if trait_name == "assoc" {
                    associativity = Some(arg);
                } else if trait_name == "tighter" || trait_name == "looser" || trait_name == "equiv"
                {
                    precedence_trait = Some((trait_name.to_string(), arg));
                } else if trait_name == "DEPRECATED" {
                    // `is DEPRECATED<msg>` — set the deprecation message
                    if let Some(pos) = custom_traits.iter().position(|(t, _)| t == "DEPRECATED") {
                        custom_traits[pos] = (format!("DEPRECATED:{}", arg), None);
                    }
                }
                r = r2;
            }
            // Parse optional parenthesized trait args: is export(:DEFAULT), is equiv(&prefix:<+>)
            if r.starts_with('(') {
                let before_parens = r;
                r = skip_balanced_parens(r);
                if trait_name == "DEPRECATED" {
                    // Extract the deprecation message from parenthesized form
                    let paren_content = &before_parens[1..before_parens.len() - r.len() - 1];
                    let msg = paren_content.trim();
                    // Strip surrounding quotes from the message
                    let msg = if (msg.starts_with('"') && msg.ends_with('"'))
                        || (msg.starts_with('\'') && msg.ends_with('\''))
                    {
                        &msg[1..msg.len() - 1]
                    } else {
                        msg
                    };
                    // Replace the plain "DEPRECATED" with "DEPRECATED:msg"
                    if let Some(pos) = custom_traits.iter().position(|(t, _)| t == "DEPRECATED") {
                        custom_traits[pos] = (format!("DEPRECATED:{}", msg), None);
                    }
                } else if (trait_name == "tighter"
                    || trait_name == "looser"
                    || trait_name == "equiv")
                    && precedence_trait.is_none()
                {
                    // Extract the reference operator from parenthesized form
                    let paren_content = &before_parens[1..before_parens.len() - r.len() - 1];
                    let ref_op = paren_content.trim().to_string();
                    precedence_trait = Some((trait_name.to_string(), ref_op));
                } else {
                    // For custom traits, parse the parenthesized content as an expression
                    let paren_content = &before_parens[1..before_parens.len() - r.len() - 1];
                    let paren_content = paren_content.trim();
                    if !paren_content.is_empty()
                        && let Ok((_, expr)) = expression(paren_content)
                    {
                        // Update the last custom trait entry with the parsed argument
                        if let Some(pos) = custom_traits.iter().rposition(|(t, _)| t == trait_name)
                        {
                            custom_traits[pos].1 = Some(expr);
                        }
                    }
                }
            }
            input = r;
            continue;
        }
        if let Some(r) = keyword("returns", r) {
            let (r, _) = ws(r)?;
            let (r, type_name) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == ':')?;
            return_type = Some(type_name.to_string());
            // Mark that the return type came from a `returns`/`of` trait (not a
            // `-->` signature arrow): an undeclared one is X::InvalidType, while
            // an undeclared `-->` type is X::Undeclared.
            if !custom_traits.iter().any(|(t, _)| t == "__return_via_trait") {
                custom_traits.push(("__return_via_trait".to_string(), None));
            }
            input = r;
            continue;
        }
        if let Some(r) = keyword("of", r) {
            let (r, _) = ws(r)?;
            let (r, type_name) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == ':')?;
            // `of` parameterizes a preceding `returns`/role type, e.g.
            // `returns Positional of Int` means return type `Positional[Int]`.
            return_type = Some(match return_type {
                Some(base) if !base.contains('[') => format!("{base}[{type_name}]"),
                _ => type_name.to_string(),
            });
            if !custom_traits.iter().any(|(t, _)| t == "__return_via_trait") {
                custom_traits.push(("__return_via_trait".to_string(), None));
            }
            input = r;
            continue;
        }
        if let Some(r) = r.strip_prefix("-->") {
            let (r, _) = ws(r)?;
            let (r, type_name) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == ':')?;
            return_type = Some(type_name.to_string());
            input = r;
            continue;
        }
        return Ok((
            r,
            SubTraits {
                is_export,
                export_tags,
                is_test_assertion,
                is_rw,
                is_raw,
                return_type,
                associativity,
                custom_traits: custom_traits.clone(),
                precedence_trait: precedence_trait.clone(),
                handles: handles.clone(),
            },
        ));
    }
}

pub(crate) fn parse_trait_angle_arg(input: &str) -> PResult<'_, String> {
    let after_open = input
        .strip_prefix('<')
        .ok_or_else(|| PError::expected("trait angle argument"))?;
    let mut depth = 1u32;
    let mut chars = after_open.char_indices();
    while let Some((i, c)) = chars.next() {
        match c {
            '>' => {
                depth -= 1;
                if depth == 0 {
                    let arg = after_open[..i].trim().to_string();
                    let after_close = &after_open[i + 1..];
                    return Ok((after_close, arg));
                }
            }
            '<' => depth += 1,
            '\\' => {
                chars.next();
            }
            _ => {}
        }
    }
    Err(PError::expected("closing '>' in trait argument"))
}

pub(crate) fn parse_export_trait_tags(input: &str) -> PResult<'_, Vec<String>> {
    let mut tags = Vec::new();
    let (mut rest, _) = ws(input)?;
    if !rest.starts_with('(') {
        return Ok((rest, tags));
    }

    let after_open = &rest[1..];
    let mut depth = 1usize;
    let mut end: Option<usize> = None;
    for (i, ch) in after_open.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    end = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }
    let end = end.ok_or_else(|| PError::expected("closing ')' in export trait"))?;
    let inner = &after_open[..end];
    rest = &after_open[end + 1..];

    let mut i = 0usize;
    while i < inner.len() {
        let c = inner[i..].chars().next().unwrap_or('\0');
        let c_len = c.len_utf8();
        if c.is_whitespace() || c == ',' {
            i += c_len;
            continue;
        }
        if c == ':' {
            i += c_len;
            if let Some(next) = inner[i..].chars().next()
                && next == '!'
            {
                i += next.len_utf8();
            }
            let start = i;
            while i < inner.len() {
                let ch = inner[i..].chars().next().unwrap_or('\0');
                if ch.is_alphanumeric() || ch == '_' || ch == '-' {
                    i += ch.len_utf8();
                } else {
                    break;
                }
            }
            if i > start {
                let tag = inner[start..i].to_string();
                if !tags.iter().any(|t| t == &tag) {
                    tags.push(tag);
                }
            }
            continue;
        }
        // A bare identifier (no `:` adverb prefix) inside `export(...)` is a term
        // reference, not an export tag (`export(:FOO)` is the tag form). An
        // undeclared bare name there is X::Undeclared::Symbols, matching rakudo
        // ("Undeclared name: WTF").
        if c.is_alphabetic() || c == '_' {
            let start = i;
            while i < inner.len() {
                let ch = inner[i..].chars().next().unwrap_or('\0');
                if ch.is_alphanumeric() || ch == '_' || ch == '-' || ch == ':' {
                    i += ch.len_utf8();
                } else {
                    break;
                }
            }
            let name = &inner[start..i];
            return Err(PError::fatal(format!(
                "X::Undeclared::Symbols: Undeclared name:\n    {} used at line 1",
                name
            )));
        }
        i += c_len;
    }

    let (rest, _) = ws(rest)?;
    Ok((rest, tags))
}

/// Reject invocant markers (':') in non-method signatures (sub, pointy block).
/// Reject attribute twigil parameters ($!x, $.x, @!a, @.a, %!h, %.h) in sub signatures.
/// These require `self`, which subs don't have.
pub(crate) fn reject_attr_params_in_sub(params: &[ParamDef]) -> Result<(), PError> {
    for p in params {
        // $! is the error variable, not an attribute; only reject $!name (attribute twigil)
        if (p.name.starts_with('!') && p.name != "!") || p.name.starts_with('.') {
            let variable = format!("${}", &p.name);
            let msg = format!(
                "X::Syntax::NoSelf: Variable {} used where no 'self' is available",
                variable
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            attrs.insert("variable".to_string(), Value::str(variable));
            let ex = Value::make_instance(Symbol::intern("X::Syntax::NoSelf"), attrs);
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
    }
    Ok(())
}

pub(crate) fn reject_invocant_in_sub(params: &[ParamDef]) -> Result<(), PError> {
    if params
        .iter()
        .any(|p| p.is_invocant || p.traits.iter().any(|t| t == "invocant"))
    {
        let msg = "X::Syntax::Signature::InvocantNotAllowed: Invocant not allowed in sub signature"
            .to_string();
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let ex = Value::make_instance(
            Symbol::intern("X::Syntax::Signature::InvocantNotAllowed"),
            attrs,
        );
        return Err(PError::fatal_with_exception(msg, Box::new(ex)));
    }
    Ok(())
}
