use super::super::expr::expression;
use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{PError, PResult, parse_char, take_while1};
use crate::token_kind::lookup_unicode_char_by_name;

use crate::ast::{Expr, ParamDef, Stmt, collect_placeholders};
use crate::value::Value;

use super::{block, ident, keyword, qualified_ident, var_name};

/// Parse a sub name, which can be a regular identifier or an operator-style name
/// like `infix:<+>`, `prefix:<->`, `postfix:<++>`, `circumfix:<[ ]>`.
pub(super) fn parse_sub_name(input: &str) -> PResult<'_, String> {
    let (rest, base) = if let Ok((rest, base)) = ident(input) {
        (rest, base)
    } else {
        let (rest, base) =
            take_while1(input, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        (rest, base.to_string())
    };
    // Check for operator category names followed by :<...>
    let is_op_category = matches!(
        base.as_str(),
        "infix"
            | "prefix"
            | "postfix"
            | "circumfix"
            | "postcircumfix"
            | "trait_mod"
            | "trait_auxiliary"
    );
    if is_op_category && rest.starts_with(":<") {
        // Check for :<<...>> (French-quotes / double-angle-bracket) delimiter
        // In Raku, <<>> is an alternate quoting form; the content is the operator symbol.
        // e.g. infix:<< - >> is the same as infix:<->
        if let Some(after_open) = rest.strip_prefix(":<<")
            && let Some(end_pos) = after_open.find(">>")
        {
            let op_symbol = after_open[..end_pos].trim();
            let after_close = &after_open[end_pos + 2..];
            let full_name = format!("{}:<{}>", base, op_symbol);
            return Ok((after_close, full_name));
        }
        // Scan for matching '>' — handle nested <> pairs
        let after_open = &rest[2..];
        let mut depth = 1u32;
        let mut chars = after_open.char_indices();
        while let Some((i, c)) = chars.next() {
            match c {
                '>' => {
                    depth -= 1;
                    if depth == 0 {
                        let op_symbol = &after_open[..i];
                        let after_close = &after_open[i + 1..];
                        let full_name = format!("{}:<{}>", base, op_symbol);
                        return Ok((after_close, full_name));
                    }
                }
                '<' => depth += 1,
                '\\' => {
                    chars.next();
                }
                _ => {}
            }
        }
        // If we can't find the closing '>', fall through to return the base name
    }
    if is_op_category
        && rest.starts_with(":[")
        && let Some(after_open) = rest.strip_prefix(":['")
        && let Some(end_pos) = after_open.find("']")
    {
        let op_symbol = unescape_operator_single_quoted(&after_open[..end_pos]);
        let after_close = &after_open[end_pos + 2..];
        let full_name = format!("{}:<{}>", base, op_symbol);
        return Ok((after_close, full_name));
    }
    if is_op_category
        && rest.starts_with(":[")
        && let Some(after_open) = rest.strip_prefix(":[\"")
        && let Some(end_pos) = after_open.find("\"]")
    {
        let op_symbol = unescape_operator_double_quoted(&after_open[..end_pos]);
        let after_close = &after_open[end_pos + 2..];
        let full_name = format!("{}:<{}>", base, op_symbol);
        return Ok((after_close, full_name));
    }
    Ok((rest, base))
}

fn unescape_operator_single_quoted(s: &str) -> String {
    s.replace("\\'", "'").replace("\\\\", "\\")
}

fn unescape_operator_double_quoted(s: &str) -> String {
    let mut out = String::new();
    let mut rest = s;
    while !rest.is_empty() {
        if rest.starts_with('\\') && rest.len() > 1 {
            let c = rest.as_bytes()[1] as char;
            match c {
                'n' => {
                    out.push('\n');
                    rest = &rest[2..];
                    continue;
                }
                't' => {
                    out.push('\t');
                    rest = &rest[2..];
                    continue;
                }
                'r' => {
                    out.push('\r');
                    rest = &rest[2..];
                    continue;
                }
                '0' => {
                    out.push('\0');
                    rest = &rest[2..];
                    continue;
                }
                '"' => {
                    out.push('"');
                    rest = &rest[2..];
                    continue;
                }
                '\\' => {
                    out.push('\\');
                    rest = &rest[2..];
                    continue;
                }
                'x' => {
                    let r = &rest[2..];
                    if let Some(r2) = r.strip_prefix('[')
                        && let Some(end) = r2.find(']')
                    {
                        for part in r2[..end].split(',') {
                            if let Ok(n) = u32::from_str_radix(part.trim(), 16)
                                && let Some(ch) = char::from_u32(n)
                            {
                                out.push(ch);
                            }
                        }
                        rest = &r2[end + 1..];
                        continue;
                    }
                }
                'c' => {
                    let r = &rest[2..];
                    if let Some(r2) = r.strip_prefix('[')
                        && let Some(end) = r2.find(']')
                    {
                        let names = &r2[..end];
                        let mut ok = true;
                        for part in names.split(',') {
                            let name = part.trim();
                            if name.is_empty() {
                                continue;
                            }
                            if let Some(ch) = lookup_unicode_char_by_name(name) {
                                out.push(ch);
                            } else {
                                ok = false;
                                break;
                            }
                        }
                        if ok {
                            rest = &r2[end + 1..];
                            continue;
                        }
                    }
                }
                _ => {}
            }
            out.push('\\');
            out.push(c);
            rest = &rest[2..];
            continue;
        }
        let ch = rest
            .chars()
            .next()
            .expect("rest is non-empty when decoding operator name");
        out.push(ch);
        rest = &rest[ch.len_utf8()..];
    }
    out
}

pub(super) fn parse_indirect_decl_name(input: &str) -> PResult<'_, (String, Expr)> {
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
        Expr::Literal(Value::Str(s)) => s.clone(),
        Expr::BareWord(s) => s.clone(),
        _ => "__INDIRECT_DECL_NAME__".to_string(),
    };
    Ok((rest, (name, expr)))
}

/// Parse `sub` declaration.
pub(super) fn sub_decl(input: &str) -> PResult<'_, Stmt> {
    sub_decl_with_semicolon_mode(input, false)
}

pub(super) fn top_level_main_semicolon_decl(input: &str) -> PResult<'_, Stmt> {
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

pub(super) fn sub_decl_with_semicolon_mode(
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
    } else {
        let r = keyword("sub", input).ok_or_else(|| PError::expected("sub declaration"))?;
        let (r, _) = ws1(r)?;
        (r, false)
    };
    sub_decl_body(rest, multi, supersede, allow_main_semicolon_decl)
}

pub(super) fn sub_decl_body(
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
        // Register user-declared sub so it can be called without parens later
        super::simple::register_user_sub(&name);
        (rest, name, None)
    };
    let (rest, _) = ws(rest)?;

    // Parse params
    let (rest, (params, param_defs)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd))
    } else {
        (rest, (Vec::new(), Vec::new()))
    };

    let (rest, _) = ws(rest)?;
    // Parse traits (is test-assertion, is export, returns ..., etc.)
    let (rest, traits) = parse_sub_traits(rest)?;
    if traits.is_test_assertion {
        super::simple::register_user_test_assertion_sub(&name);
    }
    let (rest, _) = ws(rest)?;
    let mut signature_alternates: Vec<(Vec<String>, Vec<ParamDef>)> = Vec::new();
    let rest = if multi {
        // Multi declarators can chain additional signatures with `| (...)`.
        let mut r = rest;
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with('|') {
                break r2;
            }
            let (r3, _) = ws(&r2[1..])?;
            if !r3.starts_with('(') {
                return Err(PError::expected("alternate signature after '|'"));
            }
            let (r4, _) = parse_char(r3, '(')?;
            let (r4, _) = ws(r4)?;
            let (r4, alt_param_defs) = parse_param_list(r4)?;
            let (r4, _) = ws(r4)?;
            let (r4, _) = parse_char(r4, ')')?;
            let alt_params: Vec<String> = alt_param_defs.iter().map(|p| p.name.clone()).collect();
            signature_alternates.push((alt_params, alt_param_defs));
            r = r4;
        }
    } else {
        rest
    };
    // Detect `sub name;` without a block body.
    if rest.starts_with(';') || rest.is_empty() {
        if allow_main_semicolon_decl && name == "MAIN" && !multi {
            let rest = if let Some(stripped) = rest.strip_prefix(';') {
                stripped
            } else {
                rest
            };
            return Ok((
                rest,
                Stmt::SubDecl {
                    name,
                    name_expr,
                    params,
                    param_defs,
                    signature_alternates,
                    body: Vec::new(),
                    multi,
                    is_export: traits.is_export,
                    is_test_assertion: traits.is_test_assertion,
                    supersede,
                },
            ));
        }
        return Err(PError::raw(
            "X::UnitScope::Invalid: A unit-scoped sub definition is not allowed except on a MAIN sub; \
             Please use the block form. If you did not mean to declare a unit-scoped sub, \
             perhaps you accidentally placed a semicolon after routine's definition?".to_string(),
            Some(rest.len()),
        ));
    }
    let (rest, body) = match block(rest) {
        Ok(ok) => ok,
        Err(_) if name.starts_with("trait_auxiliary:<") => consume_raw_sub_body(rest)?,
        Err(err) => return Err(err),
    };
    // When no explicit signature is given, collect placeholder variables
    // ($^a, $^b, &^c, etc.) from the body as implicit parameters.
    let (params, param_defs) = if params.is_empty() && param_defs.is_empty() {
        let placeholders = collect_placeholders(&body);
        if placeholders.is_empty() {
            (params, param_defs)
        } else {
            (placeholders, Vec::new())
        }
    } else {
        (params, param_defs)
    };
    Ok((
        rest,
        Stmt::SubDecl {
            name,
            name_expr,
            params,
            param_defs,
            signature_alternates,
            body,
            multi,
            is_export: traits.is_export,
            is_test_assertion: traits.is_test_assertion,
            supersede,
        },
    ))
}

fn consume_raw_sub_body(input: &str) -> PResult<'_, Vec<Stmt>> {
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

/// Result of parsing sub traits.
pub(super) struct SubTraits {
    pub is_export: bool,
    pub is_test_assertion: bool,
    pub is_rw: bool,
}

/// Parse sub/method traits like `is test-assertion`, `is export`, `returns Str`, etc.
/// Returns `SubTraits` indicating which traits were found.
pub(super) fn parse_sub_traits(mut input: &str) -> PResult<'_, SubTraits> {
    let mut is_export = false;
    let mut is_test_assertion = false;
    let mut is_rw = false;
    loop {
        let (r, _) = ws(input)?;
        if r.starts_with('{') || r.is_empty() {
            return Ok((
                r,
                SubTraits {
                    is_export,
                    is_test_assertion,
                    is_rw,
                },
            ));
        }
        if let Some(r) = keyword("is", r) {
            let (r, _) = ws(r)?;
            // Parse the trait name (may include hyphens like test-assertion)
            let (r, trait_name) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
            if trait_name == "export" {
                is_export = true;
            } else if trait_name == "test-assertion" {
                is_test_assertion = true;
            } else if trait_name == "rw" {
                is_rw = true;
            }
            // Skip optional parenthesized trait args: is export(:DEFAULT)
            let r = skip_balanced_parens(r);
            input = r;
            continue;
        }
        if let Some(r) = keyword("returns", r) {
            let (r, _) = ws(r)?;
            let (r, _type_name) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == ':')?;
            input = r;
            continue;
        }
        if let Some(r) = r.strip_prefix("-->") {
            let (r, _) = ws(r)?;
            let (r, _type_name) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == ':')?;
            input = r;
            continue;
        }
        return Ok((
            r,
            SubTraits {
                is_export,
                is_test_assertion,
                is_rw,
            },
        ));
    }
}

/// Parse parameter list inside parens.
pub(super) fn parse_param_list(input: &str) -> PResult<'_, Vec<ParamDef>> {
    let mut params = Vec::new();
    let mut rest = input;
    if rest.starts_with(')') {
        return Ok((rest, params));
    }
    // Handle --> return type at the start (no params, just return type)
    if let Some(stripped) = rest.strip_prefix("-->") {
        let r = skip_return_type_annotation(stripped)?;
        return Ok((r, params));
    }
    if let Some((r, _invocant_type)) = parse_implicit_invocant_marker(rest) {
        rest = r;
        if rest.starts_with(')') {
            return Ok((rest, params));
        }
        let (r, p) = parse_single_param(rest)?;
        params.push(p);
        rest = r;
    } else {
        let (r, p) = parse_single_param(rest)?;
        params.push(p);
        rest = r;
    }
    loop {
        let (r, _) = ws(rest)?;
        // Handle invocant marker ':'
        if let Some(r) = r.strip_prefix(':') {
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                mark_params_as_invocant(&mut params);
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
            let (r, p) = parse_single_param(r)?;
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
        let (r, p) = parse_single_param(r)?;
        params.push(p);
        rest = r;
    }
}

/// Skip a return type annotation (--> Type) in a signature.
/// Consumes whitespace, a type name (possibly with :D/:U), and any trailing whitespace.
pub(super) fn skip_return_type_annotation(input: &str) -> Result<&str, PError> {
    let (rest, _type_name) = parse_return_type_annotation(input)?;
    Ok(rest)
}

/// Parse a return type annotation (--> Type) and return both remainder and type name.
pub(super) fn parse_return_type_annotation(input: &str) -> PResult<'_, String> {
    let (rest, _) = ws(input)?;
    let start = rest;
    let mut rest = rest;
    while !rest.is_empty() && !rest.starts_with(')') && !rest.starts_with(',') {
        let ch = rest.chars().next().unwrap();
        if ch.is_alphanumeric() || ch == ':' || ch == '_' || ch == '-' {
            rest = &rest[ch.len_utf8()..];
        } else {
            break;
        }
    }
    let type_name = start[..start.len() - rest.len()].to_string();
    let (rest, _) = ws(rest)?;
    Ok((rest, type_name))
}

/// Parse parameter list with return type annotation.
/// Returns (remaining input, params, optional return type).
pub(super) fn parse_param_list_with_return(
    input: &str,
) -> PResult<'_, (Vec<ParamDef>, Option<String>)> {
    let mut params = Vec::new();
    let mut return_type = None;
    let mut rest = input;
    if rest.starts_with(')') {
        return Ok((rest, (params, None)));
    }
    if let Some(stripped) = rest.strip_prefix("-->") {
        let (r, rt) = parse_return_type_annotation(stripped)?;
        return Ok((r, (params, Some(rt))));
    }
    if let Some((r, _invocant_type)) = parse_implicit_invocant_marker(rest) {
        rest = r;
        if rest.starts_with(')') {
            return Ok((rest, (params, return_type)));
        }
        let (r, p) = parse_single_param(rest)?;
        params.push(p);
        rest = r;
    } else {
        let (r, p) = parse_single_param(rest)?;
        params.push(p);
        rest = r;
    }
    loop {
        let (r, _) = ws(rest)?;
        if let Some(r) = r.strip_prefix(':') {
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                mark_params_as_invocant(&mut params);
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
            let (r, p) = parse_single_param(r)?;
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
        let (r, p) = parse_single_param(r)?;
        params.push(p);
        rest = r;
    }
}

fn mark_params_as_invocant(params: &mut [ParamDef]) {
    for param in params {
        if !param.traits.iter().any(|t| t == "invocant") {
            param.traits.push("invocant".to_string());
        }
    }
}

/// Helper to construct a default ParamDef with only required fields.
fn make_param(name: String) -> ParamDef {
    ParamDef {
        name,
        default: None,
        required: false,
        named: false,
        slurpy: false,
        double_slurpy: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
    }
}

fn parse_implicit_invocant_marker(input: &str) -> Option<(&str, String)> {
    if input.starts_with('$')
        || input.starts_with('@')
        || input.starts_with('%')
        || input.starts_with('&')
        || input.starts_with('*')
        || input.starts_with(':')
    {
        return None;
    }
    let (mut rest, mut type_name) = qualified_ident(input).ok()?;
    while rest.starts_with('[') {
        let (r2, suffix) = parse_generic_suffix(rest).ok()?;
        type_name.push_str(&suffix);
        rest = r2;
    }
    if rest.starts_with(":D") || rest.starts_with(":U") || rest.starts_with(":_") {
        type_name.push_str(&rest[..2]);
        rest = &rest[2..];
    }
    let after_colon = rest.strip_prefix(':')?;
    if after_colon.starts_with(':') {
        return None;
    }
    let (after_colon, _) = ws(after_colon).ok()?;
    Some((after_colon, type_name))
}

/// Returns (rest, required, optional_marker).
/// `!` → required=true, optional_marker=false
/// `?` → required=false, optional_marker=true
/// neither → required=false, optional_marker=false
fn parse_required_suffix(input: &str) -> (&str, bool, bool) {
    if let Some(rest) = input.strip_prefix('!') {
        (rest, true, false)
    } else if let Some(rest) = input.strip_prefix('?') {
        (rest, false, true)
    } else {
        (input, false, false)
    }
}

fn parse_where_constraint_expr(input: &str) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    if r.starts_with('{') {
        let (r, body) = crate::parser::primary::parse_block_body(r)?;
        return Ok((r, Expr::AnonSub(body)));
    }
    expression(input)
}

fn parse_generic_suffix(input: &str) -> PResult<'_, String> {
    if !input.starts_with('[') {
        return Ok((input, String::new()));
    }
    let mut depth = 0usize;
    let mut end = 0usize;
    for (idx, ch) in input.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => {
                if depth == 0 {
                    return Err(PError::expected("matching ']'"));
                }
                depth -= 1;
                if depth == 0 {
                    end = idx + ch.len_utf8();
                    break;
                }
            }
            _ => {}
        }
    }
    if end == 0 {
        return Err(PError::expected("matching ']'"));
    }
    Ok((&input[end..], input[..end].to_string()))
}

pub(super) fn parse_single_param(input: &str) -> PResult<'_, ParamDef> {
    let mut rest = input;
    let mut named = false;
    let mut slurpy = false;
    let mut type_constraint = None;

    // Array sub-signature: [Type1, Type2, ...] → anonymous @ with sub-sig
    if rest.starts_with('[') {
        let (r, _) = parse_char(rest, '[')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ']')?;
        let mut p = make_param("@".to_string());
        p.sub_signature = Some(sub_params);
        return Ok((r, p));
    }

    // Capture-all: (|), (|$c), (|c), or capture sub-signature forms like | ($x)
    if let Some(stripped) = rest.strip_prefix('|') {
        let (r, _) = ws(stripped)?;
        if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            let (r, sub_params) = parse_param_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            let (r, _) = ws(r)?;
            if sub_params.len() == 1 {
                return Ok((r, sub_params[0].clone()));
            }
            let mut p = make_param("__subsig__".to_string());
            p.sub_signature = Some(sub_params);
            return Ok((r, p));
        }
        let (r, _) = ws(r)?;
        // Optional capture variable name with sigil
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
            let (r, name) = var_name(r)?;
            let mut p = make_param(name);
            p.slurpy = true;
            return Ok((r, p));
        }
        // Sigilless capture variable name: |c, |args
        if let Ok((r_ident, name)) = ident(r)
            && !matches!(name.as_str(), "where" | "is")
        {
            let mut p = make_param(name);
            p.slurpy = true;
            p.sigilless = true;
            return Ok((r_ident, p));
        }
        // Bare |, possibly followed by traits/where
        let mut p = make_param("_capture".to_string());
        p.slurpy = true;
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = ident(r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let (r, where_constraint) = if let Some(r2) = keyword("where", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, constraint) = parse_where_constraint_expr(r2)?;
            (r2, Some(Box::new(constraint)))
        } else {
            (r, None)
        };
        p.traits = param_traits;
        p.where_constraint = where_constraint;
        return Ok((r, p));
    }

    // Slurpy: *@arr or *%hash or *$scalar
    let mut slurpy_sigil = None;
    let mut double_slurpy = false;
    if rest.starts_with('*')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@'
            || rest.as_bytes()[1] == b'%'
            || rest.as_bytes()[1] == b'$'
            || rest.as_bytes()[1] == b'&')
    {
        slurpy = true;
        slurpy_sigil = Some(rest.as_bytes()[1] as char);
        rest = &rest[1..];
    }

    // Handle ::?CLASS and ::?ROLE pseudo-types in signatures (must come before named check)
    if rest.starts_with("::?CLASS") || rest.starts_with("::?ROLE") {
        let end = if rest.starts_with("::?CLASS") { 8 } else { 7 };
        let pseudo_type = &rest[..end];
        let r = &rest[end..];
        let (r, tc) = if r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_") {
            let smiley = &r[..2];
            (&r[2..], format!("{}{}", pseudo_type, smiley))
        } else {
            (r, pseudo_type.to_string())
        };
        if let Some(r) = r.strip_prefix(':') {
            // This invocant marker is handled at parse_param_list level.
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                return Ok((r, make_param("self".to_string())));
            }
            return parse_single_param(r);
        }
        let (r, _) = ws(r)?;
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
            type_constraint = Some(tc);
            rest = r;
        } else {
            let mut p = make_param("self".to_string());
            p.type_constraint = Some(tc);
            return Ok((r, p));
        }
    }

    // Named param marker: :$name (but not :: which is a parametric type prefix)
    if rest.starts_with(':') && !rest.starts_with("::") {
        named = true;
        rest = &rest[1..];
    }

    // Type-capture parameter: ::T $x  or bare ::T
    if let Some(after_capture) = rest.strip_prefix("::")
        && let Ok((r, capture_name)) = ident(after_capture)
    {
        type_constraint = Some(format!("::{}", capture_name));
        let (r, _) = ws(r)?;
        rest = r;
        if rest.starts_with(')') || rest.starts_with(',') || rest.starts_with(';') {
            let mut p = make_param(format!("__type_capture__{}", capture_name));
            p.type_constraint = type_constraint;
            p.named = named;
            p.slurpy = slurpy;
            return Ok((rest, p));
        }
        if rest.starts_with(':') {
            named = true;
            rest = &rest[1..];
        }
    }

    // Type constraint (may be qualified: IO::Path)
    // Skip type constraint parsing for named params with lowercase identifiers followed by '('
    // Parametric type constraint: ::T
    if let Some(after_colon) = rest.strip_prefix("::")
        && let Ok((r, tc_name)) = ident(after_colon)
    {
        let tc = format!("::{}", tc_name);
        let (r2, _) = ws(r)?;
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
            type_constraint = Some(tc);
            rest = r2;
        }
    }

    // — those are named aliases like :x($r), not type constraints.
    let skip_type_for_named_alias = named
        && rest
            .as_bytes()
            .first()
            .is_some_and(|b| b.is_ascii_lowercase())
        && rest.contains('(');
    if type_constraint.is_none()
        && !skip_type_for_named_alias
        && let Ok((r, tc)) = qualified_ident(rest)
    {
        let mut tc_full = tc;
        let mut r = r;
        while r.starts_with('[') {
            let (r2, suffix) = parse_generic_suffix(r)?;
            tc_full.push_str(&suffix);
            r = r2;
        }
        // Preserve type smileys :D, :U, :_ as part of the type constraint
        let (r, tc) = if r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_") {
            let smiley = &r[..2];
            (&r[2..], format!("{}{}", tc_full, smiley))
        } else {
            (r, tc_full)
        };
        let (r2, _) = ws(r)?;

        // Check for coercion type: Int() or Int(Rat)
        // Also handles sub-signature: Type (inner-params)
        if let Some(after_open) = r2.strip_prefix('(') {
            let (after_ws, _) = ws(after_open)?;
            // Coercion type: Int() — empty parens
            if let Some(r3) = after_ws.strip_prefix(')') {
                let (r3, _) = ws(r3)?;
                // This is a coercion type like Int()
                type_constraint = Some(format!("{}()", tc));
                rest = r3;
                // Re-check named after type
                if rest.starts_with(':') {
                    named = true;
                    rest = &rest[1..];
                }
            } else if let Ok((inner_r, source_type)) = qualified_ident(after_ws) {
                let (inner_r, _) = ws(inner_r)?;
                if let Some(r3) = inner_r.strip_prefix(')') {
                    let (r3, _) = ws(r3)?;
                    // This is a coercion type like Int(Rat)
                    type_constraint = Some(format!("{}({})", tc, source_type));
                    rest = r3;
                    if rest.starts_with(':') {
                        named = true;
                        rest = &rest[1..];
                    }
                } else {
                    // Fall through to sub-signature parsing
                    let (r3, _) = parse_char(r2, '(')?;
                    let (r3, _) = ws(r3)?;
                    let (r3, sub_params) = parse_param_list(r3)?;
                    let (r3, _) = ws(r3)?;
                    let (r3, _) = parse_char(r3, ')')?;
                    let (r3, _) = ws(r3)?;
                    let mut p = make_param("__subsig__".to_string());
                    p.type_constraint = Some(tc);
                    p.sub_signature = Some(sub_params);
                    p.named = named;
                    p.slurpy = slurpy;
                    return Ok((r3, p));
                }
            } else {
                // Parse sub-signature (destructuring)
                let (r3, _) = parse_char(r2, '(')?;
                let (r3, _) = ws(r3)?;
                let (r3, sub_params) = parse_param_list(r3)?;
                let (r3, _) = ws(r3)?;
                let (r3, _) = parse_char(r3, ')')?;
                let (r3, _) = ws(r3)?;
                let mut p = make_param("__subsig__".to_string());
                p.type_constraint = Some(tc);
                p.sub_signature = Some(sub_params);
                p.named = named;
                p.slurpy = slurpy;
                // Handle optional (?) / required (!) suffix after sub-signature
                let (rest, required, opt_marker) = parse_required_suffix(r3);
                // Skip whitespace
                let (rest, _) = ws(rest)?;
                // Handle is copy, is rw, is readonly, is raw traits
                let mut param_traits = Vec::new();
                let (mut rest, _) = ws(rest)?;
                while let Some(r) = keyword("is", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, trait_name) = ident(r)?;
                    param_traits.push(trait_name);
                    let (r, _) = ws(r)?;
                    rest = r;
                }
                p.traits = param_traits;
                p.required = required;
                p.optional_marker = opt_marker;
                // Handle where constraint
                let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, constraint) = parse_where_constraint_expr(r)?;
                    (r, Some(Box::new(constraint)))
                } else {
                    (rest, None)
                };
                p.where_constraint = where_constraint;
                return Ok((rest, p));
            }
        }

        if r2.starts_with('$')
            || r2.starts_with('@')
            || r2.starts_with('%')
            || (r2.starts_with('*')
                && r2.len() > 1
                && (r2.as_bytes()[1] == b'$'
                    || r2.as_bytes()[1] == b'@'
                    || r2.as_bytes()[1] == b'%'
                    || r2.as_bytes()[1] == b'&'))
            || r2.starts_with(':')
            || r2.starts_with('|')
            || r2.starts_with('\\')
        {
            type_constraint = Some(tc);
            rest = r2;
            // Re-check named after type
            if rest.starts_with(':') {
                named = true;
                rest = &rest[1..];
            }
        } else if r2.starts_with(')')
            || r2.starts_with(',')
            || r2.starts_with(']')
            || r2.starts_with("-->")
        {
            // Bare identifier as type-only parameter (e.g., enum values in multi dispatch)
            // multi infix:<->(e1, e2) { ... }
            let mut p = make_param("__type_only__".to_string());
            p.type_constraint = Some(tc);
            p.named = named;
            p.slurpy = slurpy;
            return Ok((r2, p));
        }
    }

    // Typed capture parameter, e.g. `Capture |cap`
    if type_constraint.is_some() && rest.starts_with('|') {
        let (r, mut p) = parse_single_param(rest)?;
        if p.type_constraint.is_none() {
            p.type_constraint = type_constraint.clone();
        }
        p.named = named;
        if slurpy {
            p.slurpy = true;
        }
        return Ok((r, p));
    }

    // Double slurpy marker may appear after a type constraint:
    // e.g. `Array **@AoA`
    if rest.starts_with("**")
        && rest.len() > 2
        && (rest.as_bytes()[2] == b'@' || rest.as_bytes()[2] == b'%')
    {
        slurpy = true;
        double_slurpy = true;
        slurpy_sigil = Some(rest.as_bytes()[2] as char);
        rest = &rest[2..];
    }
    // Slurpy marker may appear after a type constraint:
    // e.g. `Code *$block`, `Int *@xs`, `Hash *%h`.
    else if rest.starts_with('*')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@'
            || rest.as_bytes()[1] == b'%'
            || rest.as_bytes()[1] == b'$'
            || rest.as_bytes()[1] == b'&')
    {
        slurpy = true;
        slurpy_sigil = Some(rest.as_bytes()[1] as char);
        rest = &rest[1..];
    }

    // Handle literal value parameters: multi sub foo(0) { ... }
    if rest.starts_with(|c: char| c.is_ascii_digit())
        || (rest.starts_with('"') || rest.starts_with('\''))
    {
        let (rest, lit_expr) = expression(rest)?;
        let literal_value = match &lit_expr {
            Expr::Literal(v) => Some(v.clone()),
            _ => None,
        };
        let mut p = make_param("__literal__".to_string());
        p.type_constraint = type_constraint;
        p.literal_value = literal_value;
        return Ok((rest, p));
    }

    // Sigilless parameter: \name
    if let Some(r) = rest.strip_prefix('\\') {
        let (r, name) = ident(r)?;
        let (r, _) = ws(r)?;
        // Default value
        let (r, default) = if r.starts_with('=') && !r.starts_with("==") {
            let (r, _) = ws(r)?;
            let (r, expr) = expression(r)?;
            (r, Some(expr))
        } else {
            (r, None)
        };
        let mut p = make_param(name);
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.sigilless = true;
        p.default = default;
        p.type_constraint = type_constraint;
        return Ok((r, p));
    }

    // Sub-signature without type: just (inner-params)
    if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let mut p = make_param("__subsig__".to_string());
        p.sub_signature = Some(sub_params);
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.type_constraint = type_constraint;
        return Ok((r, p));
    }

    // Named parameter with alias: :key($var) or :value(&callback)
    if named && let Ok((r, alias_name)) = ident(rest) {
        let (r2, _) = ws(r)?;
        if r2.starts_with('(') {
            let (r3, _) = parse_char(r2, '(')?;
            let (r3, _) = ws(r3)?;
            // Inside could be a sub-signature or a single variable
            // Try to parse as a sub-signature first (handles nested params)
            let (r3, sub_params) = parse_param_list(r3)?;
            let (r3, _) = ws(r3)?;
            let (r3, _) = parse_char(r3, ')')?;
            // If it's a single simple variable param, use its name; otherwise sub-signature
            if sub_params.len() == 1
                && sub_params[0].sub_signature.is_none()
                && !sub_params[0].name.starts_with("__")
            {
                let mut p = make_param(alias_name.clone());
                p.named = true;
                p.slurpy = slurpy;
                p.double_slurpy = double_slurpy;
                p.type_constraint = type_constraint;
                p.sub_signature = Some(sub_params.clone());
                // Check for a sub-signature after the alias: :x($r) (Str $g, Any $i)
                let (r3, _) = ws(r3)?;
                if r3.starts_with('(') {
                    let (r4, _) = parse_char(r3, '(')?;
                    let (r4, _) = ws(r4)?;
                    let (r4, outer_sub) = parse_param_list(r4)?;
                    let (r4, _) = ws(r4)?;
                    let (r3_new, _) = parse_char(r4, ')')?;
                    p.outer_sub_signature = Some(outer_sub);
                    let (rest, alias_required, alias_opt_marker) = parse_required_suffix(r3_new);
                    p.required = alias_required;
                    p.optional_marker = alias_opt_marker;
                    let (rest, _) = ws(rest)?;
                    let mut param_traits = Vec::new();
                    let (mut rest, _) = ws(rest)?;
                    while let Some(r) = keyword("is", rest) {
                        let (r, _) = ws1(r)?;
                        let (r, trait_name) = ident(r)?;
                        param_traits.push(trait_name);
                        let (r, _) = ws(r)?;
                        rest = r;
                    }
                    p.traits = param_traits;
                    return Ok((rest, p));
                }
                // Handle optional (?) / required (!) suffix after alias
                let (rest, alias_required, alias_opt_marker) = parse_required_suffix(r3);
                p.required = alias_required;
                p.optional_marker = alias_opt_marker;
                // Skip whitespace
                let (rest, _) = ws(rest)?;
                // Handle is copy, is rw, is readonly, is raw traits
                let mut param_traits = Vec::new();
                let (mut rest, _) = ws(rest)?;
                while let Some(r) = keyword("is", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, trait_name) = ident(r)?;
                    param_traits.push(trait_name);
                    let (r, _) = ws(r)?;
                    rest = r;
                }
                p.traits = param_traits;
                // Handle where constraint
                let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                    let (r, _) = ws1(r)?;
                    let (r, constraint) = parse_where_constraint_expr(r)?;
                    (r, Some(Box::new(constraint)))
                } else {
                    (rest, None)
                };
                p.where_constraint = where_constraint;
                return Ok((rest, p));
            }
            // Multiple params or complex: treat as sub-signature
            let mut p = make_param(alias_name);
            p.named = true;
            p.slurpy = slurpy;
            p.double_slurpy = double_slurpy;
            p.type_constraint = type_constraint;
            p.sub_signature = Some(sub_params);
            // Handle optional (?) / required (!) suffix after sub-signature
            let (rest, required, opt_marker) = parse_required_suffix(r3);
            // Skip whitespace
            let (rest, _) = ws(rest)?;
            // Handle is copy, is rw, is readonly, is raw traits
            let mut param_traits = Vec::new();
            let (mut rest, _) = ws(rest)?;
            while let Some(r) = keyword("is", rest) {
                let (r, _) = ws1(r)?;
                let (r, trait_name) = ident(r)?;
                param_traits.push(trait_name);
                let (r, _) = ws(r)?;
                rest = r;
            }
            p.traits = param_traits;
            p.required = required;
            p.optional_marker = opt_marker;
            // Handle where constraint
            let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
                let (r, _) = ws1(r)?;
                let (r, constraint) = parse_where_constraint_expr(r)?;
                (r, Some(Box::new(constraint)))
            } else {
                (rest, None)
            };
            p.where_constraint = where_constraint;
            return Ok((rest, p));
        }
    }

    // Anonymous optional scalar parameter: $?
    if let Some(after_q) = rest.strip_prefix("$?")
        && (after_q.is_empty()
            || after_q.starts_with(',')
            || after_q.starts_with(')')
            || after_q.starts_with(' ')
            || after_q.starts_with('\t')
            || after_q.starts_with('\n'))
    {
        let mut p = make_param("__ANON_OPTIONAL__".to_string());
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.type_constraint = type_constraint;
        return Ok((after_q, p));
    }

    // Bare & (anonymous callable parameter)
    if rest.starts_with('&')
        && (rest.len() == 1
            || rest.as_bytes()[1] == b','
            || rest.as_bytes()[1] == b')'
            || rest.as_bytes()[1] == b' '
            || rest.as_bytes()[1] == b'\t'
            || rest.as_bytes()[1] == b'\n'
            || rest.as_bytes()[1] == b'?'
            || rest.as_bytes()[1] == b'!')
    {
        let rest = &rest[1..];
        let (rest, required, opt_marker) = parse_required_suffix(rest);
        let mut p = make_param("&".to_string());
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.type_constraint = type_constraint;
        p.required = required;
        p.optional_marker = opt_marker;
        return Ok((rest, p));
    }

    // Capture the original sigil before var_name strips it
    let original_sigil = rest.as_bytes().first().copied().unwrap_or(b'$');
    let param_sigil = rest.as_bytes().first().copied();
    let (rest, name) = var_name(rest)?;

    // Code signature constraint: &foo:(Str --> Bool)
    let mut code_sig = None;
    let rest = if original_sigil == b'&' && rest.starts_with(":(") {
        let r = &rest[1..]; // skip ':'
        let (r, _) = parse_char(r, '(')?;
        let (r, _) = ws(r)?;
        let (r, (sig_params, sig_ret)) = parse_param_list_with_return(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        code_sig = Some((sig_params, sig_ret));
        r
    } else {
        rest
    };

    // Optional (?) / required (!) suffix
    let (rest, required, opt_marker) = parse_required_suffix(rest);
    let (rest, _) = ws(rest)?;

    // Sub-signature after variable: $x ($a, $b) or $ ($a, $b)
    if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, sub_params) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let (r, _) = ws(r)?;
        // Handle optional (?) / required (!) suffix after sub-signature
        let (r, post_required, post_opt_marker) = parse_required_suffix(r);
        let (r, _) = ws(r)?;
        let mut param_traits = Vec::new();
        let (mut r, _) = ws(r)?;
        while let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, trait_name) = ident(r2)?;
            param_traits.push(trait_name);
            let (r2, _) = ws(r2)?;
            r = r2;
        }
        let param_name = if slurpy {
            match slurpy_sigil {
                Some('%') => format!("%{}", name),
                Some('@') => format!("@{}", name),
                _ => name,
            }
        } else {
            match original_sigil {
                b'@' => format!("@{}", name),
                b'%' => format!("%{}", name),
                b'&' => format!("&{}", name),
                _ => name,
            }
        };
        let mut p = make_param(param_name);
        p.required = required || post_required;
        p.optional_marker = opt_marker || post_opt_marker;
        p.named = named;
        p.slurpy = slurpy;
        p.double_slurpy = double_slurpy;
        p.type_constraint = type_constraint;
        p.sub_signature = Some(sub_params);
        p.traits = param_traits;
        return Ok((r, p));
    }

    // Default value
    let (rest, mut default) = if rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = expression(rest)?;
        (rest, Some(expr))
    } else {
        (rest, None)
    };

    // `is copy`, `is rw`, `is readonly`, `is raw` traits (may have multiple)
    let (mut rest, _) = ws(rest)?;
    let mut param_traits = Vec::new();
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        param_traits.push(trait_name);
        let (r, _) = ws(r)?;
        rest = r;
    }

    // `where` constraint
    let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, constraint) = parse_where_constraint_expr(r)?;
        (r, Some(Box::new(constraint)))
    } else {
        (rest, None)
    };
    let (rest_ws, _) = ws(rest)?;
    let (rest, late_default) =
        if default.is_none() && rest_ws.starts_with('=') && !rest_ws.starts_with("==") {
            let rest = &rest_ws[1..];
            let (rest, _) = ws(rest)?;
            let (rest, expr) = expression(rest)?;
            (rest, Some(expr))
        } else {
            (rest_ws, None)
        };
    if late_default.is_some() {
        default = late_default;
    }

    // Prefix the name with the sigil so runtime can distinguish types.
    // For slurpy params, use the slurpy_sigil; for non-slurpy, use original_sigil.
    let param_name = if slurpy {
        match slurpy_sigil {
            Some('%') => format!("%{}", name),
            Some('@') => format!("@{}", name),
            _ => name,
        }
    } else if param_sigil == Some(b'@') {
        format!("@{}", name)
    } else if param_sigil == Some(b'%') {
        format!("%{}", name)
    } else {
        match original_sigil {
            b'@' => format!("@{}", name),
            b'%' => format!("%{}", name),
            b'&' => format!("&{}", name),
            _ => name,
        }
    };
    let mut p = make_param(param_name);
    p.default = default;
    p.required = required;
    p.optional_marker = opt_marker;
    p.named = named;
    p.slurpy = slurpy;
    p.double_slurpy = double_slurpy;
    p.type_constraint = type_constraint;
    p.where_constraint = where_constraint;
    p.traits = param_traits;
    p.code_signature = code_sig;
    Ok((rest, p))
}

/// Parse `method` declaration.
pub(super) fn method_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest, multi) = if let Some(r) = keyword("multi", input) {
        let (r, _) = ws1(r)?;
        let r = keyword("method", r).ok_or_else(|| PError::expected("method declaration"))?;
        let (r, _) = ws1(r)?;
        (r, true)
    } else {
        let r = keyword("method", input).ok_or_else(|| PError::expected("method declaration"))?;
        let (r, _) = ws1(r)?;
        (r, false)
    };
    method_decl_body(rest, multi)
}

pub(super) fn method_decl_body(input: &str, multi: bool) -> PResult<'_, Stmt> {
    let (rest, is_private) = if let Some(rest) = input.strip_prefix('!') {
        (rest, true)
    } else {
        (input, false)
    };
    let (rest, name, name_expr) = if rest.starts_with("::") {
        let (rest, (name, expr)) = parse_indirect_decl_name(rest)?;
        (rest, name, Some(expr))
    } else {
        let (rest, name) = parse_sub_name(rest)?;
        (rest, name, None)
    };
    let (rest, _) = ws(rest)?;

    let (rest, (params, param_defs)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd))
    } else {
        (rest, (Vec::new(), Vec::new()))
    };

    let (rest, traits) = parse_sub_traits(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::MethodDecl {
            name,
            name_expr,
            params,
            param_defs,
            body,
            multi,
            is_rw: traits.is_rw,
            is_private,
        },
    ))
}
