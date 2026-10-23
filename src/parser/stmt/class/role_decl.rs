use super::*;
#[path = "role_header.rs"]
mod header;
use crate::meta_ns::MetaNs;

use crate::ast::{Expr, ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use crate::parser::expr::expression;
use crate::parser::helpers::{parse_trait_angle_arg, skip_balanced_parens, ws, ws1};
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::parser::stmt::sub::parse_single_param;
use crate::parser::stmt::{
    ident, keyword, package_body_block_with_terms, parse_param_list, qualified_ident,
};

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

/// The tail of a `Type ::Name` role parameter after the capture name: nothing
/// (no default) or `= <expr>`. `None` means the part is not that form at all —
/// e.g. `K::R :$r = C`, a qualified type constraint whose first segment `K`
/// happens to be a declared type — so the caller must try the general
/// single-parameter parser instead of dropping the parameter.
fn type_capture_default(rest: &str) -> Option<Option<Expr>> {
    if rest.is_empty() {
        return Some(None);
    }
    let rhs = rest.strip_prefix('=')?.trim_start();
    let (rest_after_expr, default_expr) = expression(rhs).ok()?;
    rest_after_expr
        .trim()
        .is_empty()
        .then_some(Some(default_expr))
}

/// The names a parameterized role substitutes for its parameters: a type
/// capture's name (`T` for `::T`), else the parameter's name without its sigil.
// Cost: O(p), p = number of parameters.
pub(crate) fn role_type_param_names(param_defs: &[ParamDef]) -> Vec<String> {
    param_defs
        .iter()
        .map(|pd| match pd.captured_type_name() {
            Some(captured) => captured.to_string(),
            None => pd.name.trim_start_matches(['$', '@', '%', '&']).to_string(),
        })
        .collect()
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
        && let Ok((params_start, _)) = ws(&r[1..])
        && let Ok((after_params, param_defs)) = parse_param_list(params_start)
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
        let params = role_type_param_names(&param_defs);
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
        // A nameless capture `role R[::]` still takes one argument: it binds
        // as an anonymous positional so `R[Int]` resolves a candidate.
        if part == "::"
            && let Ok((rest, pd)) = parse_single_param("$")
            && rest.trim().is_empty()
        {
            params.push(pd.name.trim_start_matches('$').to_string());
            param_defs.push(pd);
            continue;
        }
        if let Some((constraint_part, capture_part)) = part.split_once("::") {
            let constraint = constraint_part.trim();
            let capture_part = capture_part.trim();
            if !constraint.is_empty()
                && role_type_param_constraint_is_known(constraint)
                && let Ok((rest_after_name, name)) = ident(capture_part)
                && let Some(default) = type_capture_default(rest_after_name.trim_start())
            {
                params.push(name.clone());
                param_defs.push(ParamDef {
                    spans: Default::default(),
                    trait_meta: Default::default(),
                    type_capture: None,
                    name,
                    default,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    named_alias: false,
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
                    block_param: false,
                    code: Default::default(),
                    trait_args: Vec::new(),
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
                        spans: Default::default(),
                        trait_meta: Default::default(),
                        type_capture: None,
                        name: format!("__type_capture__{}", name),
                        default: Some(default_expr),
                        multi_invocant: true,
                        required: false,
                        named: false,
                        named_alias: false,
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
                        block_param: false,
                        code: Default::default(),
                        trait_args: Vec::new(),
                    });
                    continue;
                }
            }
        }
        if let Ok((rest, pd)) = parse_single_param(part)
            && rest.trim().is_empty()
        {
            let name = if let Some(captured) = pd.captured_type_name() {
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

/// Parse `role` declaration.
pub(crate) fn role_decl(input: &str) -> PResult<'_, Stmt> {
    role_decl_with_keyword(input, "role")
}

/// Parse a role declaration introduced by `kw` instead of the literal `role`.
///
/// A slang can add a package declarator that builds a role (`$*PKGDECL` is
/// `'role'`) under its own keyword — `test-bundle`, from Test::Async
/// (ADR-0091). Such a declarator is a peer of `role`, so it accepts exactly
/// the same syntax; only the marker trait naming the keyword differs, which
/// is what tells registration which metaclass to attach.
pub(crate) fn role_decl_with_keyword<'a>(input: &'a str, kw: &str) -> PResult<'a, Stmt> {
    let rest = keyword(kw, input).ok_or_else(|| PError::expected("role declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    check_pseudo_package_in_decl(&name)?;
    let (mut rest, (mut type_params, mut type_param_defs)) = parse_optional_role_type_params(rest)?;
    // Parse optional type adverbs (:ver<...>, :auth<...>, :api<...>)
    let (rest2, traits) = parse_declarator_traits(rest)?;
    let (rest2, _) = ws(rest2)?;
    rest = rest2;
    // The signature may also FOLLOW the adverbs — the order Raku itself uses:
    // `role JSON::Class:ver<0.0.21>:auth<zef:jonathanstowe>[Bool :$opt-in]`.
    if type_params.is_empty() && rest.starts_with('[') {
        let (rest2, (tp, tpd)) = parse_optional_role_type_params(rest)?;
        let (rest2, _) = ws(rest2)?;
        type_params = tp;
        type_param_defs = tpd;
        rest = rest2;
    }
    let (
        after_header,
        header::Header {
            parent_roles,
            is_hidden_role,
            mut role_is_rw,
            is_export,
            export_tags,
            mut custom_traits,
            header_order,
        },
    ) = header::parse(rest, &name, &type_params)?;
    rest = after_header;

    // A role can refer to itself from its own methods. Register its name before
    // parsing the body so `when RoleName { ... }` is recognized as a type
    // matcher instead of a bareword call that gobbles the block.
    super::super::simple::register_user_type(&name);
    // No package path is pushed for a role body: the scope inside a role is
    // generic, so Raku refuses to install an `our`-scoped declaration there and
    // there is no composed name to register.
    // A role body parses like any other block. It used to fall back to
    // `consume_raw_braced_body` on a non-fatal failure, which DISCARDED the
    // whole body: `role R { method m(::T R:D: $x) { 1 } }` compiled to an empty
    // role and the call site then died with "No such method", with no
    // diagnostic pointing at the signature that had not parsed (#7984). A role
    // body that does not parse is a parse error, exactly as a class body is.
    let sigilless_terms: Vec<&str> = type_param_defs
        .iter()
        .filter(|pd| pd.sigilless)
        .map(|pd| pd.name.as_str())
        .collect();
    let (rest, mut body) = package_body_block_with_terms(rest, &sigilless_terms)?;
    crate::parser::stmt::nested_block_methods::hoist(&mut body);
    // Handle `also is rw;` in the role body
    body.retain(|stmt| {
        if stmt_is_also_is_rw(stmt) {
            role_is_rw = true;
            false
        } else {
            true
        }
    });
    // `also is Parent;` in the role body is the header's `role R is Parent`
    // (Gnome::Gio::EmblemedIcon: `unit role ...; also is Gnome::GObject::Object;`).
    let mut also_parents: Vec<String> = Vec::new();
    body.retain(|stmt| {
        if let Some(parent) = stmt_also_is_parent(stmt) {
            also_parents.push(parent);
            false
        } else {
            true
        }
    });
    for parent in also_parents.into_iter().rev() {
        body.insert(
            0,
            Stmt::DoesDecl {
                name: Symbol::intern(&parent),
                args: None,
                from_is: true,
                also: false,
            },
        );
    }
    if is_hidden_role {
        body.insert(
            0,
            Stmt::DoesDecl {
                name: Symbol::intern("__mutsu_role_hidden__"),
                args: None,
                from_is: false,
                also: false,
            },
        );
    }
    for (role_name, args, from_is) in parent_roles.into_iter().rev() {
        body.insert(
            0,
            Stmt::DoesDecl {
                name: Symbol::intern(&role_name),
                args,
                from_is,
                also: false,
            },
        );
    }
    // `role R { ... }.^name` is one expression; see `reject_trailing_postfix`.
    super::reject_trailing_postfix(rest)?;

    if kw != "role" {
        custom_traits.push(crate::parser::declare_how_marker(kw.to_string()));
    } else if let Some(marker) = super::super::simple::supersede_marker("role") {
        custom_traits.push(marker);
    }
    let role_stmt = Stmt::RoleDecl {
        name: Symbol::intern(&name),
        header_order,
        type_params,
        type_param_defs,
        is_export,
        export_tags,
        body,
        is_rw: role_is_rw,
        language_version: super::super::simple::current_language_version(),
        custom_traits,
        decl_id: crate::ast::next_class_decl_id(),
    };
    // Emit __MUTSU_SET_META__ calls for ver/auth/api traits (like class declarations)
    let mut meta_stmts = Vec::new();
    for (trait_name, trait_value) in traits {
        if trait_name == "ver" || trait_name == "auth" || trait_name == "api" {
            meta_stmts.push(meta_setter_stmt(&name, &trait_name, trait_value));
        }
    }
    if meta_stmts.is_empty() {
        return Ok((rest, role_stmt));
    }
    meta_stmts.push(role_stmt);
    Ok((rest, Stmt::SyntheticBlock(meta_stmts)))
}
