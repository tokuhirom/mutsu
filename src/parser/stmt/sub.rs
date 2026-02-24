use super::super::expr::expression;
use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{PError, PResult, parse_char, take_while1};

use crate::ast::{Expr, ParamDef, Stmt};

use super::{block, ident, keyword, qualified_ident, var_name};

/// Parse a sub name, which can be a regular identifier or an operator-style name
/// like `infix:<+>`, `prefix:<->`, `postfix:<++>`, `circumfix:<[ ]>`.
pub(super) fn parse_sub_name(input: &str) -> PResult<'_, String> {
    let (rest, base) = ident(input)?;
    // Check for operator category names followed by :<...>
    let is_op_category = matches!(
        base.as_str(),
        "infix" | "prefix" | "postfix" | "circumfix" | "postcircumfix" | "trait_mod"
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
    Ok((rest, base))
}

/// Parse `sub` declaration.
pub(super) fn sub_decl(input: &str) -> PResult<'_, Stmt> {
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
    sub_decl_body(rest, multi, supersede)
}

pub(super) fn sub_decl_body(input: &str, multi: bool, supersede: bool) -> PResult<'_, Stmt> {
    let (rest, name) = parse_sub_name(input)?;
    // Register user-declared sub so it can be called without parens later
    super::simple::register_user_sub(&name);
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
    // Detect `sub name;` without a block body — this is a unit-scoped sub declaration error
    if rest.starts_with(';') || rest.is_empty() {
        return Err(PError::raw(
            "X::UnitScope::Invalid: A unit-scoped sub definition is not allowed except on a MAIN sub; \
             Please use the block form. If you did not mean to declare a unit-scoped sub, \
             perhaps you accidentally placed a semicolon after routine's definition?".to_string(),
            Some(rest.len()),
        ));
    }
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::SubDecl {
            name,
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
    let (r, p) = parse_single_param(rest)?;
    params.push(p);
    rest = r;
    loop {
        let (r, _) = ws(rest)?;
        // Handle invocant marker ':'
        if let Some(r) = r.strip_prefix(':') {
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                return Ok((r, params));
            }
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
    let (rest, _) = ws(input)?;
    // Consume the type name (identifier, possibly with :: qualifications)
    let mut rest = rest;
    while !rest.is_empty() && !rest.starts_with(')') && !rest.starts_with(',') {
        let ch = rest.chars().next().unwrap();
        if ch.is_alphanumeric() || ch == ':' || ch == '_' || ch == '-' {
            rest = &rest[ch.len_utf8()..];
        } else {
            break;
        }
    }
    let (rest, _) = ws(rest)?;
    Ok(rest)
}

/// Helper to construct a default ParamDef with only required fields.
fn make_param(name: String) -> ParamDef {
    ParamDef {
        name,
        default: None,
        named: false,
        slurpy: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
    }
}

pub(super) fn parse_single_param(input: &str) -> PResult<'_, ParamDef> {
    let mut rest = input;
    let mut named = false;
    let mut slurpy = false;
    let mut type_constraint = None;

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
            let (r2, constraint) = expression(r2)?;
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

    // Named param marker: :$name
    if rest.starts_with(':') {
        named = true;
        rest = &rest[1..];
    }

    // Type constraint (may be qualified: IO::Path)
    if let Ok((r, tc)) = qualified_ident(rest) {
        // Preserve type smileys :D, :U, :_ as part of the type constraint
        let (r, tc) = if r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_") {
            let smiley = &r[..2];
            (&r[2..], format!("{}{}", tc, smiley))
        } else {
            (r, tc)
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
                let mut rest = r3;
                if rest.starts_with('?') || rest.starts_with('!') {
                    rest = &rest[1..];
                }
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
                    let (r, constraint) = expression(r)?;
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
            || r2.starts_with('\\')
        {
            type_constraint = Some(tc);
            rest = r2;
            // Re-check named after type
            if rest.starts_with(':') {
                named = true;
                rest = &rest[1..];
            }
        } else if r2.starts_with(')') || r2.starts_with(',') {
            // Bare identifier as type-only parameter (e.g., enum values in multi dispatch)
            // multi infix:<->(e1, e2) { ... }
            let mut p = make_param("__type_only__".to_string());
            p.type_constraint = Some(tc);
            p.named = named;
            p.slurpy = slurpy;
            return Ok((r2, p));
        }
    }

    // Slurpy marker may appear after a type constraint:
    // e.g. `Code *$block`, `Int *@xs`, `Hash *%h`.
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
                let mut p = make_param(sub_params[0].name.clone());
                p.named = true;
                p.slurpy = slurpy;
                p.type_constraint = type_constraint;
                // Handle optional (?) / required (!) suffix after alias
                let mut rest = r3;
                if rest.starts_with('?') || rest.starts_with('!') {
                    rest = &rest[1..];
                }
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
                    let (r, constraint) = expression(r)?;
                    (r, Some(Box::new(constraint)))
                } else {
                    (rest, None)
                };
                p.where_constraint = where_constraint;
                return Ok((rest, p));
            }
            // Multiple params or complex: treat as sub-signature
            let mut p = make_param(format!("__{}", alias_name));
            p.named = true;
            p.slurpy = slurpy;
            p.type_constraint = type_constraint;
            p.sub_signature = Some(sub_params);
            // Handle optional (?) / required (!) suffix after sub-signature
            let mut rest = r3;
            if rest.starts_with('?') || rest.starts_with('!') {
                rest = &rest[1..];
            }
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
                let (r, constraint) = expression(r)?;
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
        let mut p = make_param("__ANON_STATE__".to_string());
        p.named = named;
        p.slurpy = slurpy;
        p.type_constraint = type_constraint;
        return Ok((after_q, p));
    }

    let (rest, name) = var_name(rest)?;
    // Optional (?) / required (!) suffix
    let rest = if rest.starts_with('?') || rest.starts_with('!') {
        &rest[1..]
    } else {
        rest
    };
    let (rest, _) = ws(rest)?;

    // Default value
    let (rest, default) = if rest.starts_with('=') && !rest.starts_with("==") {
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
        let (r, constraint) = expression(r)?;
        (r, Some(Box::new(constraint)))
    } else {
        (rest, None)
    };

    // For slurpy params, prefix the name with the sigil so runtime can
    // distinguish *@array from *%hash
    let param_name = if slurpy {
        match slurpy_sigil {
            Some('%') => format!("%{}", name),
            Some('@') => format!("@{}", name),
            _ => name,
        }
    } else {
        name
    };
    let mut p = make_param(param_name);
    p.default = default;
    p.named = named;
    p.slurpy = slurpy;
    p.type_constraint = type_constraint;
    p.where_constraint = where_constraint;
    p.traits = param_traits;
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
    let (rest, name) = ident(rest)?;
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
            params,
            param_defs,
            body,
            multi,
            is_rw: traits.is_rw,
            is_private,
        },
    ))
}
