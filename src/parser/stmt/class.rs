use super::super::helpers::{skip_balanced_parens, ws, ws1};
use super::super::parse_result::{PError, PResult, opt_char, parse_char, take_while1};
use super::super::primary::regex::scan_to_delim;

use crate::ast::{Expr, Stmt};
use crate::value::Value;

use super::{block, ident, keyword, qualified_ident};

use super::sub::{parse_indirect_decl_name, parse_sub_name};
use super::{parse_param_list, parse_sub_traits};

fn parse_declarator_traits(input: &str) -> PResult<'_, Vec<(String, Value)>> {
    let mut traits = Vec::new();
    let (mut rest, _) = ws(input)?;
    loop {
        if !rest.starts_with(':') || rest.starts_with("::") {
            break;
        }
        rest = &rest[1..];
        let (r, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        rest = r;
        let mut value = Value::Bool(true);
        if let Some(inner) = rest.strip_prefix('<') {
            if let Some(end) = inner.find('>') {
                value = Value::Str(inner[..end].to_string());
                rest = &inner[end + 1..];
            } else {
                return Err(PError::expected("closing '>' in trait value"));
            }
        } else if rest.starts_with('(') {
            let after = skip_balanced_parens(rest);
            let body = &rest[1..rest.len() - after.len() - 1];
            value = Value::Str(body.trim().to_string());
            rest = after;
        }
        traits.push((name.to_string(), value));
        let (r, _) = ws(rest)?;
        rest = r;
    }
    Ok((rest, traits))
}

fn meta_setter_stmt(type_name: &str, key: &str, value: Value) -> Stmt {
    Stmt::Expr(Expr::Call {
        name: "__MUTSU_SET_META__".to_string(),
        args: vec![
            Expr::Literal(Value::Str(type_name.to_string())),
            Expr::Literal(Value::Str(key.to_string())),
            Expr::Literal(value),
        ],
    })
}

fn consume_raw_braced_body(input: &str) -> PResult<'_, Vec<Stmt>> {
    if !input.starts_with('{') {
        return Err(PError::expected("raw braced body"));
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
                    let rest = &input[i + len..];
                    return Ok((rest, Vec::new()));
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

fn parse_token_like_name(input: &str) -> PResult<'_, String> {
    let (mut rest, mut name) = ident(input)?;
    loop {
        if !rest.starts_with(':') {
            break;
        }
        let r = &rest[1..];
        let (r, part) = take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        name.push(':');
        name.push_str(part);
        let mut r2 = r;
        if r2.starts_with('<')
            && let Some(end) = r2.find('>')
        {
            name.push_str(&r2[..=end]);
            r2 = &r2[end + 1..];
        }
        rest = r2;
    }
    Ok((rest, name))
}

fn parse_raw_braced_regex_body(input: &str) -> PResult<'_, String> {
    let after_open = input
        .strip_prefix('{')
        .ok_or_else(|| PError::expected("regex body"))?;
    if let Some((body, rest)) = scan_to_delim(after_open, '{', '}', true) {
        return Ok((rest, body.trim().to_string()));
    }
    Err(PError::expected("regex closing delimiter"))
}

fn inject_implicit_rule_ws(pattern: &str) -> String {
    fn should_insert(prev: char, next: char) -> bool {
        !matches!(
            (prev, next),
            ('|', _)
                | (_, '|')
                | ('(', _)
                | (_, ')')
                | ('[', _)
                | (_, ']')
                | ('{', _)
                | (_, '}')
                | ('^', _)
                | (_, '$')
                | ('<', _)
                | (_, '>')
        )
    }

    let chars: Vec<char> = pattern.chars().collect();
    let mut out = String::new();
    let mut i = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;
    while i < chars.len() {
        let c = chars[i];
        if escaped {
            out.push(c);
            escaped = false;
            i += 1;
            continue;
        }
        if c == '\\' {
            out.push(c);
            escaped = true;
            i += 1;
            continue;
        }
        if c == '\'' && !in_double {
            in_single = !in_single;
            out.push(c);
            i += 1;
            continue;
        }
        if c == '"' && !in_single {
            in_double = !in_double;
            out.push(c);
            i += 1;
            continue;
        }
        if !in_single && !in_double && c.is_whitespace() {
            let mut j = i;
            while j < chars.len() && chars[j].is_whitespace() {
                j += 1;
            }
            let prev = out.chars().rev().find(|ch| !ch.is_whitespace());
            let next = chars[j..].iter().copied().find(|ch| !ch.is_whitespace());
            if let (Some(p), Some(n)) = (prev, next) {
                if should_insert(p, n) {
                    if !out.ends_with(' ') && !out.is_empty() {
                        out.push(' ');
                    }
                    out.push_str("<.ws>");
                    out.push(' ');
                } else if !out.ends_with(' ') && !out.is_empty() {
                    out.push(' ');
                }
            }
            i = j;
            continue;
        }
        out.push(c);
        i += 1;
    }
    out.trim().to_string()
}

fn normalize_token_pattern(pattern: &str) -> String {
    let trimmed = pattern.trim();
    if trimmed.len() >= 2 && trimmed.starts_with('/') && trimmed.ends_with('/') {
        trimmed[1..trimmed.len() - 1].to_string()
    } else {
        trimmed.to_string()
    }
}

/// Parse `class` declaration.
pub(crate) fn class_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("class", input).ok_or_else(|| PError::expected("class declaration"))?;
    let (rest, _) = ws1(rest)?;
    class_decl_body(rest)
}

/// Parse the body of a class declaration (after `class` keyword and whitespace).
pub(super) fn class_decl_body(input: &str) -> PResult<'_, Stmt> {
    let (rest, name, name_expr) = if input.starts_with("::") {
        let (rest, (name, expr)) = parse_indirect_decl_name(input)?;
        (rest, name, Some(expr))
    } else {
        let (rest, name) = qualified_ident(input)?;
        (rest, name, None)
    };
    let (rest, traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;

    // Parent classes: is Parent
    let mut parents = Vec::new();
    let mut r = rest;
    while let Some(r2) = keyword("is", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, parent) = qualified_ident(r2)?;
        parents.push(parent);
        let (r2, _) = ws(r2)?;
        r = r2;
    }
    // does Role or does Role[TypeArgs]
    while let Some(r2) = keyword("does", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, role_name) = qualified_ident(r2)?;
        let (r2, _) = ws(r2)?;
        // Check for type arguments [...]
        if r2.starts_with('[') {
            // Capture the full bracket content
            let mut depth = 0u32;
            let mut end = 0;
            for (i, ch) in r2.char_indices() {
                if ch == '[' {
                    depth += 1;
                } else if ch == ']' {
                    depth -= 1;
                    if depth == 0 {
                        end = i + 1;
                        break;
                    }
                }
            }
            let bracket_content = &r2[..end];
            parents.push(format!("{}{}", role_name, bracket_content));
            let (r2, _) = ws(&r2[end..])?;
            r = r2;
        } else {
            parents.push(role_name);
            r = r2;
        }
    }

    let (rest, body) = block(r)?;
    let class_stmt = Stmt::ClassDecl {
        name: name.clone(),
        name_expr,
        parents,
        body,
    };
    let mut stmts = Vec::new();
    for (trait_name, trait_value) in traits {
        if trait_name == "ver" || trait_name == "auth" {
            stmts.push(meta_setter_stmt(&name, &trait_name, trait_value));
        }
    }
    if stmts.is_empty() {
        return Ok((rest, class_stmt));
    }
    stmts.push(class_stmt);
    Ok((rest, Stmt::Block(stmts)))
}

/// Parse optional role type parameters like `[::T]` or `[::T1, ::T2]`.
/// Returns the type parameter names (without the `::` prefix).
fn parse_optional_role_type_params(input: &str) -> PResult<'_, Vec<String>> {
    let (r, _) = ws(input)?;
    if !r.starts_with('[') {
        return Ok((r, Vec::new()));
    }
    // Find the matching closing bracket to extract the content
    let mut depth = 0u32;
    let mut end = 0;
    for (i, ch) in r.char_indices() {
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
            depth -= 1;
            if depth == 0 {
                end = i;
                break;
            }
        }
    }
    let content = &r[1..end]; // content between [ and ]
    let rest = &r[end + 1..];
    // Parse type params: split by comma and look for ::Name patterns
    let mut params = Vec::new();
    for part in content.split(',') {
        let trimmed = part.trim();
        if let Some(stripped) = trimmed.strip_prefix("::") {
            // May have additional constraints like "Cool ::T" — take just the ident
            let name_part = stripped.trim();
            if let Ok((_, name)) = ident(name_part) {
                params.push(name);
            }
        } else {
            // Could be "Cool ::T" pattern — look for :: within
            if let Some(pos) = trimmed.find("::") {
                let after = &trimmed[pos + 2..];
                if let Ok((_, name)) = ident(after.trim()) {
                    params.push(name);
                }
            }
        }
    }
    let (rest, _) = ws(rest)?;
    Ok((rest, params))
}

/// Skip optional role args like `[Str:D(Numeric)]` in a `does` clause.
fn skip_optional_role_args(input: &str) -> PResult<'_, ()> {
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
pub(super) fn role_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("role", input).ok_or_else(|| PError::expected("role declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (mut rest, type_params) = parse_optional_role_type_params(rest)?;

    // Optional `does Role[...]` clauses.
    while let Some(r) = keyword("does", rest) {
        let (r, _) = ws1(r)?;
        let (r, role_name) = qualified_ident(r)?;
        if role_name == name {
            return Err(PError::fatal(format!(
                "X::InvalidType: role '{}' cannot compose itself",
                name
            )));
        }
        let (r, _) = skip_optional_role_args(r)?;
        rest = r;
    }

    // Optional `is trait` clauses (ignore trait args for parse compatibility).
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, _trait_name) = ident(r)?;
        let r = skip_balanced_parens(r);
        let (r, _) = ws(r)?;
        rest = r;
    }

    let (rest, body) = match block(rest) {
        Ok(ok) => ok,
        Err(_) => consume_raw_braced_body(rest)?,
    };
    Ok((
        rest,
        Stmt::RoleDecl {
            name,
            type_params,
            body,
        },
    ))
}

/// Parse `does` declaration.
pub(super) fn does_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("does", input).ok_or_else(|| PError::expected("does declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = parse_token_like_name(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::DoesDecl { name }))
}

/// Parse `trusts` declaration.
pub(super) fn trusts_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("trusts", input).ok_or_else(|| PError::expected("trusts declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::TrustsDecl { name }))
}

/// Parse a `token`, `regex`, or `rule` declaration.
pub(super) fn token_decl(input: &str) -> PResult<'_, Stmt> {
    let is_rule = keyword("rule", input).is_some();
    let rest = keyword("token", input)
        .or_else(|| keyword("rule", input))
        .or_else(|| keyword("regex", input))
        .ok_or_else(|| PError::expected("token/regex/rule declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = parse_token_like_name(rest)?;
    let (rest, _) = ws(rest)?;

    // Optional params
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
    let (rest, mut pattern) = parse_raw_braced_regex_body(rest)?;
    pattern = normalize_token_pattern(&pattern);
    if is_rule {
        pattern = inject_implicit_rule_ws(&pattern);
        if name.contains(":sym<") {
            if !pattern.ends_with(' ') {
                pattern.push(' ');
            }
            pattern.push_str("<.ws>?");
        }
    }
    let body = vec![Stmt::Expr(Expr::Literal(Value::Regex(pattern)))];

    if is_rule {
        Ok((
            rest,
            Stmt::RuleDecl {
                name,
                params,
                param_defs,
                body,
                multi: false,
            },
        ))
    } else {
        Ok((
            rest,
            Stmt::TokenDecl {
                name,
                params,
                param_defs,
                body,
                multi: false,
            },
        ))
    }
}

/// Parse `grammar` declaration.
pub(super) fn grammar_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("grammar", input).ok_or_else(|| PError::expected("grammar declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Package { name, body }))
}

/// Parse `module Name { ... }` declaration (non-unit form).
pub(super) fn module_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("module", input).ok_or_else(|| PError::expected("module declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    let mut stmts = Vec::new();
    for (trait_name, trait_value) in traits {
        if trait_name == "ver" || trait_name == "auth" {
            stmts.push(meta_setter_stmt(&name, &trait_name, trait_value));
        }
    }
    let package_stmt = Stmt::Package {
        name: name.clone(),
        body,
    };
    if stmts.is_empty() {
        return Ok((rest, package_stmt));
    }
    stmts.push(package_stmt);
    Ok((rest, Stmt::Block(stmts)))
}

/// Parse `unit module` or `unit class` statement.
pub(super) fn unit_module_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unit", input).ok_or_else(|| PError::expected("unit statement"))?;
    let (rest, _) = ws1(rest)?;
    // unit class Name;
    if let Some(r) = keyword("class", rest) {
        let (r, _) = ws1(r)?;
        let (r, name) = qualified_ident(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::ClassDecl {
                name,
                name_expr: None,
                parents: Vec::new(),
                body: Vec::new(),
            },
        ));
    }
    let rest = keyword("module", rest).ok_or_else(|| PError::expected("'module' after 'unit'"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::Package {
            name,
            body: Vec::new(),
        },
    ))
}

/// Parse `package` declaration.
pub(super) fn package_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("package", input).ok_or_else(|| PError::expected("package declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    let (rest, _traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((rest, Stmt::Package { name, body }))
}

/// Parse `proto` declaration.
pub(super) fn proto_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("proto", input).ok_or_else(|| PError::expected("proto declaration"))?;
    let (rest, _) = ws1(rest)?;
    // proto token | proto rule | proto sub | proto method
    let _is_token = keyword("token", rest).is_some() || keyword("rule", rest).is_some();
    let rest = if let Some(r) = keyword("token", rest)
        .or_else(|| keyword("rule", rest))
        .or_else(|| keyword("sub", rest))
        .or_else(|| keyword("method", rest))
    {
        let (r, _) = ws1(r)?;
        r
    } else {
        rest
    };
    let (rest, name) = parse_sub_name(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param_defs) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, pd) = parse_param_list(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        (r, pd)
    } else {
        (rest, Vec::new())
    };
    let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
    let (rest, _) = ws(rest)?;
    // Parse traits (is export, etc.)
    let (rest, traits) = parse_sub_traits(rest)?;
    let (rest, _) = ws(rest)?;
    // May have body or just semicolon
    let mut body = Vec::new();
    if rest.starts_with('{') {
        let (rest, parsed_body) = match block(rest) {
            Ok(ok) => ok,
            Err(_) => consume_raw_braced_body(rest)?,
        };
        body = parsed_body;
        return Ok((
            rest,
            Stmt::ProtoDecl {
                name,
                params,
                param_defs,
                body,
                is_export: traits.is_export,
            },
        ));
    }
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::ProtoDecl {
            name,
            params,
            param_defs,
            body,
            is_export: traits.is_export,
        },
    ))
}
