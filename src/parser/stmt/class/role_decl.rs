use super::*;

use crate::ast::{Expr, ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use crate::parser::expr::expression;
use crate::parser::helpers::{skip_balanced_parens, ws, ws1};
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::parser::stmt::sub::parse_single_param;
use crate::parser::stmt::{block, ident, keyword, parse_param_list, qualified_ident};

pub(crate) fn role_type_param_constraint_is_known(type_name: &str) -> bool {
    let stripped = type_name
        .strip_suffix(":D")
        .or_else(|| type_name.strip_suffix(":U"))
        .or_else(|| type_name.strip_suffix(":_"))
        .unwrap_or(type_name);
    let base = stripped
        .split_once('[')
        .map(|(head, _)| head)
        .unwrap_or(stripped)
        .split_once('(')
        .map(|(head, _)| head)
        .unwrap_or(stripped);
    crate::runtime::utils::is_known_type_constraint(base)
        || super::super::simple::is_user_declared_type(base)
}

pub(crate) fn split_role_param_parts(input: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut start = 0usize;
    let mut paren = 0u32;
    let mut bracket = 0u32;
    let mut brace = 0u32;
    let mut quote: Option<char> = None;
    let mut escaped = false;
    for (idx, ch) in input.char_indices() {
        if let Some(q) = quote {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' && q == '"' {
                escaped = true;
                continue;
            }
            if ch == q {
                quote = None;
            }
            continue;
        }
        match ch {
            '\'' | '"' => quote = Some(ch),
            '(' => paren += 1,
            ')' => paren = paren.saturating_sub(1),
            '[' => bracket += 1,
            ']' => bracket = bracket.saturating_sub(1),
            '{' => brace += 1,
            '}' => brace = brace.saturating_sub(1),
            ',' if paren == 0 && bracket == 0 && brace == 0 => {
                parts.push(input[start..idx].trim());
                start = idx + ch.len_utf8();
            }
            _ => {}
        }
    }
    parts.push(input[start..].trim());
    parts
}

/// Parse optional role type parameters like `[::T]`, `[Str $x]`, or
/// `[Int $x where { ... }]`.
/// Returns both full parameter defs and plain names used for substitution.
pub(crate) fn parse_optional_role_type_params(
    input: &str,
) -> PResult<'_, (Vec<String>, Vec<ParamDef>)> {
    let (r, _) = ws(input)?;
    if !r.starts_with('[') {
        return Ok((r, (Vec::new(), Vec::new())));
    }
    let mut depth = 0u32;
    let mut end = 0usize;
    for (i, ch) in r.char_indices() {
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                end = i;
                break;
            }
        }
    }
    if end == 0 {
        return Err(PError::expected("']'"));
    }
    let content = &r[1..end];
    let normalized_content = content
        .replace("= my role {", "= role {")
        .replace("= my class {", "= class {")
        .replace("= my grammar {", "= grammar {");
    for part in normalized_content.split(',') {
        let trimmed = part.trim();
        if let Some(stripped) = trimmed.strip_prefix("::")
            && let Ok((rest_after_ident, _)) = ident(stripped)
            && rest_after_ident.starts_with('?')
        {
            return Err(PError::fatal("X::Syntax::Malformed".to_string()));
        }
    }
    if normalized_content == content
        && let Ok((after_params, param_defs)) = parse_param_list(&r[1..])
        && let Ok((rest, _)) = parse_char(after_params, ']')
    {
        for pd in &param_defs {
            if pd.name == "__type_only__"
                && let Some(tc) = pd.type_constraint.as_deref()
                && !tc.starts_with("::")
                && !role_type_param_constraint_is_known(tc)
            {
                let msg = format!(
                    "X::Parameter::InvalidType: Invalid type '{}' in role parameter list",
                    tc
                );
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("type".to_string(), Value::str(tc.to_string()));
                attrs.insert("message".to_string(), Value::str(msg.clone()));
                let ex = Value::make_instance(Symbol::intern("X::Parameter::InvalidType"), attrs);
                return Err(PError::fatal_with_exception(msg, Box::new(ex)));
            }
        }
        let params = param_defs
            .iter()
            .map(|pd| {
                if let Some(captured) = pd
                    .type_constraint
                    .as_deref()
                    .and_then(|t| t.strip_prefix("::"))
                {
                    captured.to_string()
                } else {
                    pd.name.trim_start_matches(['$', '@', '%', '&']).to_string()
                }
            })
            .collect::<Vec<_>>();
        let (rest, _) = ws(rest)?;
        return Ok((rest, (params, param_defs)));
    }

    // Fallback: parse each top-level parameter item independently so defaults
    // like `::T = 42` or `::T = my role { ... }` still preserve ParamDef data.
    let mut params = Vec::new();
    let mut param_defs = Vec::new();
    for part in split_role_param_parts(&normalized_content) {
        if part.is_empty() {
            continue;
        }
        if let Some((constraint_part, capture_part)) = part.split_once("::") {
            let constraint = constraint_part.trim();
            let capture_part = capture_part.trim();
            if !constraint.is_empty()
                && role_type_param_constraint_is_known(constraint)
                && let Ok((rest_after_name, name)) = ident(capture_part)
            {
                let rest_after_name = rest_after_name.trim_start();
                let default = if let Some(rhs) = rest_after_name.strip_prefix('=') {
                    let rhs = rhs.trim_start();
                    if let Ok((rest_after_expr, default_expr)) = expression(rhs) {
                        if rest_after_expr.trim().is_empty() {
                            Some(default_expr)
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                } else if rest_after_name.is_empty() {
                    None
                } else {
                    continue;
                };
                params.push(name.clone());
                param_defs.push(ParamDef {
                    name,
                    default,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: false,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: false,
                    type_constraint: Some(constraint.to_string()),
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                    shape_constraints: None,
                });
                continue;
            }
        }
        if let Some(after_capture) = part.strip_prefix("::")
            && let Ok((rest_after_name, name)) = ident(after_capture)
        {
            let rest_after_name = rest_after_name.trim_start();
            if let Some(rhs) = rest_after_name.strip_prefix('=') {
                let rhs = rhs.trim_start();
                if let Ok((rest_after_expr, default_expr)) = expression(rhs)
                    && rest_after_expr.trim().is_empty()
                {
                    params.push(name.clone());
                    param_defs.push(ParamDef {
                        name: format!("__type_capture__{}", name),
                        default: Some(default_expr),
                        multi_invocant: true,
                        required: false,
                        named: false,
                        slurpy: false,
                        double_slurpy: false,
                        onearg: false,
                        sigilless: false,
                        type_constraint: Some(format!("::{}", name)),
                        literal_value: None,
                        sub_signature: None,
                        where_constraint: None,
                        traits: Vec::new(),
                        optional_marker: false,
                        outer_sub_signature: None,
                        code_signature: None,
                        is_invocant: false,
                        shape_constraints: None,
                    });
                    continue;
                }
            }
        }
        if let Ok((rest, pd)) = parse_single_param(part)
            && rest.trim().is_empty()
        {
            let name = if let Some(captured) = pd
                .type_constraint
                .as_deref()
                .and_then(|t| t.strip_prefix("::"))
            {
                captured.to_string()
            } else {
                pd.name.trim_start_matches(['$', '@', '%', '&']).to_string()
            };
            params.push(name);
            param_defs.push(pd);
            continue;
        }
        if let Some(stripped) = part.strip_prefix("::") {
            let name_part = stripped.trim();
            if let Ok((_, name)) = ident(name_part) {
                params.push(name);
            }
        }
    }
    let (rest, _) = ws(&r[end + 1..])?;
    Ok((rest, (params, param_defs)))
}

/// Skip optional role args like `[Str:D(Numeric)]` in a `does` clause.
pub(crate) fn skip_optional_role_args(input: &str) -> PResult<'_, ()> {
    let (mut r, _) = ws(input)?;
    if !r.starts_with('[') {
        return Ok((r, ()));
    }
    let mut depth = 0u32;
    while let Some(ch) = r.chars().next() {
        let len = ch.len_utf8();
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                r = &r[len..];
                break;
            }
        }
        r = &r[len..];
    }
    let (r, _) = ws(r)?;
    Ok((r, ()))
}

/// Parse `role` declaration.
pub(crate) fn role_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("role", input).ok_or_else(|| PError::expected("role declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    check_pseudo_package_in_decl(&name)?;
    let (mut rest, (type_params, type_param_defs)) = parse_optional_role_type_params(rest)?;
    // Parse optional type adverbs (:ver<...>, :auth<...>, :api<...>)
    let (rest2, traits) = parse_declarator_traits(rest)?;
    let (rest2, _) = ws(rest2)?;
    rest = rest2;
    let mut parent_roles: Vec<String> = Vec::new();
    let mut is_hidden_role = false;
    let mut role_is_rw = false;
    let mut is_export = false;
    let mut export_tags: Vec<String> = Vec::new();
    let mut custom_traits: Vec<(String, Option<Expr>)> = Vec::new();

    // Optional parent/trait clauses in any order.
    loop {
        if let Some(r) = keyword("does", rest) {
            let (r, _) = ws1(r)?;
            let (r, role_name) = qualified_ident(r)?;
            if role_name == name {
                return Err(PError::fatal(format!(
                    "X::InvalidType: role '{}' cannot compose itself",
                    name
                )));
            }
            let (r, _) = ws(r)?;
            let (r, bracket_suffix) = parse_optional_bracket_suffix(r)?;
            let (r, _) = ws(r)?;
            parent_roles.push(format!("{}{}", role_name, bracket_suffix));
            rest = r;
            continue;
        }
        if let Some(r) = keyword("is", rest) {
            let (r, _) = ws1(r)?;
            let (r, trait_name) = ident(r)?;
            if trait_name == "hidden" {
                is_hidden_role = true;
                let r = skip_balanced_parens(r);
                let (r, _) = ws(r)?;
                rest = r;
            } else if trait_name == "rw" {
                role_is_rw = true;
                let r = skip_balanced_parens(r);
                let (r, _) = ws(r)?;
                rest = r;
            } else if trait_name == "export" {
                is_export = true;
                if !export_tags.iter().any(|t| t == "DEFAULT") {
                    export_tags.push("DEFAULT".to_string());
                }
                let r = skip_balanced_parens(r);
                let (r, _) = ws(r)?;
                rest = r;
            } else if matches!(
                trait_name.as_str(),
                "ok" | "required"
                    | "readonly"
                    | "repr"
                    | "default"
                    | "raw"
                    | "built"
                    | "copy"
                    | "DEPRECATED"
                    | "nodal"
                    | "pure"
            ) {
                // Known lowercase trait keywords are skipped
                let r = skip_balanced_parens(r);
                let (r, _) = ws(r)?;
                rest = r;
            } else {
                // Unknown trait — check for parenthesized argument for
                // custom trait_mod:<is> dispatch.
                if let Some(inner_start) = r.strip_prefix('(') {
                    if let Ok((after_expr, expr)) = expression(inner_start) {
                        let after_expr = after_expr.trim_start();
                        if let Some(after_paren) = after_expr.strip_prefix(')') {
                            custom_traits.push((trait_name.clone(), Some(expr)));
                            let (r2, _) = ws(after_paren)?;
                            rest = r2;
                            continue;
                        }
                    }
                    let r2 = skip_balanced_parens(r);
                    custom_traits.push((trait_name.clone(), None));
                    let (r2, _) = ws(r2)?;
                    rest = r2;
                } else {
                    // No parens — treat as parent role. If it's actually
                    // a custom trait, the runtime will handle it via
                    // trait_mod:<is> when the role name is not found.
                    parent_roles.push(trait_name);
                    let (r, _) = ws(r)?;
                    rest = r;
                }
            }
            continue;
        }
        if let Some(r) = keyword("hides", rest) {
            let (r, _) = ws1(r)?;
            let (r, hidden_name) = qualified_ident(r)?;
            let (r, _) = ws(r)?;
            // Track as a parent relationship
            parent_roles.push(hidden_name.clone());
            // Also mark the hidden relationship with a special marker
            parent_roles.push(format!("__mutsu_role_hides__{}", hidden_name));
            rest = r;
            continue;
        }
        break;
    }

    let (rest, mut body) = match block(rest) {
        Ok(ok) => ok,
        Err(e) if e.is_fatal() => return Err(e),
        Err(_) => consume_raw_braced_body(rest)?,
    };
    // Handle `also is rw;` in the role body
    body.retain(|stmt| {
        if stmt_is_also_is_rw(stmt) {
            role_is_rw = true;
            false
        } else {
            true
        }
    });
    if is_hidden_role {
        body.insert(
            0,
            Stmt::DoesDecl {
                name: Symbol::intern("__mutsu_role_hidden__"),
            },
        );
    }
    for role_name in parent_roles.into_iter().rev() {
        body.insert(
            0,
            Stmt::DoesDecl {
                name: Symbol::intern(&role_name),
            },
        );
    }
    super::super::simple::register_user_type(&name);

    let role_stmt = Stmt::RoleDecl {
        name: Symbol::intern(&name),
        type_params,
        type_param_defs,
        is_export,
        export_tags,
        body,
        is_rw: role_is_rw,
        language_version: super::super::simple::current_language_version(),
        custom_traits,
    };
    // Emit __MUTSU_SET_META__ calls for ver/auth traits (like class declarations)
    let mut meta_stmts = Vec::new();
    for (trait_name, trait_value) in traits {
        if trait_name == "ver" || trait_name == "auth" {
            meta_stmts.push(meta_setter_stmt(&name, &trait_name, trait_value));
        }
    }
    if meta_stmts.is_empty() {
        return Ok((rest, role_stmt));
    }
    meta_stmts.push(role_stmt);
    Ok((rest, Stmt::Block(meta_stmts)))
}
