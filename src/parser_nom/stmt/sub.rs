use super::super::expr::expression;
use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{PError, PResult, parse_char, take_while1};

use crate::ast::{Expr, ParamDef, Stmt};

use super::{block, ident, keyword, var_name};

/// Parse `sub` declaration.
pub(super) fn sub_decl(input: &str) -> PResult<'_, Stmt> {
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
    sub_decl_body(rest, multi)
}

pub(super) fn sub_decl_body(input: &str, multi: bool) -> PResult<'_, Stmt> {
    let (rest, name) = ident(input)?;
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
    // Skip traits (is test-assertion, is export, returns ..., etc.)
    let (rest, _) = skip_sub_traits(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::SubDecl {
            name,
            params,
            param_defs,
            body,
            multi,
        },
    ))
}

/// Skip sub/method traits like `is test-assertion`, `is export`, `returns Str`, etc.
pub(super) fn skip_sub_traits(mut input: &str) -> PResult<'_, ()> {
    loop {
        let (r, _) = ws(input)?;
        if r.starts_with('{') || r.is_empty() {
            return Ok((r, ()));
        }
        if let Some(r) = keyword("is", r) {
            let (r, _) = ws(r)?;
            // Skip the trait name (may include hyphens like test-assertion)
            let (r, _trait_name) =
                take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
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
        return Ok((r, ()));
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

pub(super) fn parse_single_param(input: &str) -> PResult<'_, ParamDef> {
    let mut rest = input;
    let mut named = false;
    let mut slurpy = false;
    let mut type_constraint = None;

    // Capture-all: (|) or (|$c)
    if let Some(stripped) = rest.strip_prefix('|') {
        let (r, _) = ws(stripped)?;
        // Optional capture variable name
        if r.starts_with('$') || r.starts_with('@') || r.starts_with('%') {
            let (r, name) = var_name(r)?;
            return Ok((
                r,
                ParamDef {
                    name,
                    named: false,
                    slurpy: true,
                    sigilless: false,
                    default: None,
                    type_constraint: None,
                    literal_value: None,
                },
            ));
        }
        // Bare |
        return Ok((
            r,
            ParamDef {
                name: "_capture".to_string(),
                named: false,
                slurpy: true,
                sigilless: false,
                default: None,
                type_constraint: None,
                literal_value: None,
            },
        ));
    }

    // Slurpy: *@arr or *%hash
    if rest.starts_with('*')
        && rest.len() > 1
        && (rest.as_bytes()[1] == b'@' || rest.as_bytes()[1] == b'%' || rest.as_bytes()[1] == b'$')
    {
        slurpy = true;
        rest = &rest[1..];
    }

    // Named param marker: :$name
    if rest.starts_with(':') {
        named = true;
        rest = &rest[1..];
    }

    // Type constraint
    if let Ok((r, tc)) = ident(rest) {
        // Skip type smileys :D, :U, :_
        let r = if r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_") {
            &r[2..]
        } else {
            r
        };
        let (r2, _) = ws(r)?;
        if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') || r2.starts_with(':')
        {
            type_constraint = Some(tc);
            rest = r2;
            // Re-check named after type
            if rest.starts_with(':') {
                named = true;
                rest = &rest[1..];
            }
        }
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
        return Ok((
            rest,
            ParamDef {
                name: "__literal__".to_string(),
                named: false,
                slurpy: false,
                sigilless: false,
                default: None,
                type_constraint,
                literal_value,
            },
        ));
    }

    // Sigilless parameter: \name
    if let Some(r) = rest.strip_prefix('\\') {
        let (r, name) = ident(r)?;
        let (r, _) = ws(r)?;
        // Default value
        let (r, default) = if r.starts_with('=') && !r.starts_with("==") {
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, expr) = expression(r)?;
            (r, Some(expr))
        } else {
            (r, None)
        };
        return Ok((
            r,
            ParamDef {
                name,
                named,
                slurpy,
                sigilless: true,
                default,
                type_constraint,
                literal_value: None,
            },
        ));
    }

    let (rest, name) = var_name(rest)?;
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

    // `is copy`, `is rw`, `is readonly` traits
    let (rest, _) = ws(rest)?;
    let rest = if let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, _trait) = ident(r)?;
        r
    } else {
        rest
    };

    // `where` constraint
    let (rest, _) = ws(rest)?;
    let rest = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, _constraint) = expression(r)?;
        r
    } else {
        rest
    };

    Ok((
        rest,
        ParamDef {
            name,
            default,
            named,
            slurpy,
            sigilless: false,
            type_constraint,
            literal_value: None,
        },
    ))
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
    let (rest, name) = ident(input)?;
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

    let (rest, _) = skip_sub_traits(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::MethodDecl {
            name,
            params,
            param_defs,
            body,
            multi,
        },
    ))
}
