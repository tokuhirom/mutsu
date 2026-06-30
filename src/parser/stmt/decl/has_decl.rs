use super::super::super::expr::expression;
use super::super::super::helpers::{ws, ws1};
use super::super::super::parse_result::{PError, PResult, opt_char, parse_char, take_while1};
use super::super::{ident, keyword};
use super::handles::parse_handle_specs;
use super::helpers::{
    parse_array_shape_suffix, shaped_array_new_expr, shaped_array_new_with_data_expr,
};
use super::method_decl_body;
use super::parse_colon_method_arg;
use super::parse_decl_type_constraint;
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use super::super::sub::parse_type_constraint_expr;

/// Parse parenthesized list form of `has`: `has ($a, $.b, $!c)`
/// Desugars into a SyntheticBlock containing multiple HasDecl statements.
fn has_decl_list(input: &str) -> PResult<'_, Stmt> {
    let (mut rest, _) = parse_char(input, '(')?;
    let mut stmts = Vec::new();
    loop {
        let (r, _) = ws(rest)?;
        rest = r;
        if rest.starts_with(')') {
            rest = &rest[1..];
            break;
        }
        // Parse sigil
        let sigil = rest.as_bytes().first().copied().unwrap_or(0);
        if sigil != b'$' && sigil != b'@' && sigil != b'%' {
            return Err(PError::expected("sigil ($, @, %) in has list"));
        }
        rest = &rest[1..];
        // Parse optional twigil (. or !)
        let (r, is_public, is_alias) = if let Some(stripped) = rest.strip_prefix('.') {
            (stripped, true, false)
        } else if let Some(stripped) = rest.strip_prefix('!') {
            (stripped, false, false)
        } else {
            (rest, false, true)
        };
        rest = r;
        // Parse name
        let (r, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let name = name.to_string();
        rest = r;
        stmts.push(Stmt::HasDecl {
            name: Symbol::intern(&name),
            is_public,
            default: None,
            handles: Vec::new(),
            is_rw: false,
            is_readonly: false,
            type_constraint: None,
            type_smiley: None,
            is_required: None,
            sigil: sigil as char,
            where_constraint: None,
            is_alias,
            is_our: false,
            is_my: false,
            is_default: None,
            is_type: None,
            deprecated_message: None,
            is_built: None,
            unknown_traits: Vec::new(),
        });
        let (r, _) = ws(rest)?;
        rest = r;
        // Expect comma or closing paren
        if rest.starts_with(',') {
            rest = &rest[1..];
        }
    }
    let (mut rest, _) = ws(rest)?;

    // Parse optional traits on grouped has declaration: `is rw`, `is default(expr)`, etc.
    loop {
        let (r, _) = ws(rest)?;
        if let Some(r) = keyword("is", r)
            && let Ok((r, _)) = ws1(r)
        {
            if let Some(r) = keyword("rw", r) {
                // Apply `is rw` to all attributes in the list
                for stmt in &mut stmts {
                    if let Stmt::HasDecl { is_rw, .. } = stmt {
                        *is_rw = true;
                    }
                }
                rest = r;
            } else if let Some(r) = keyword("readonly", r) {
                // Apply `is readonly` to all attributes in the list
                for stmt in &mut stmts {
                    if let Stmt::HasDecl { is_readonly, .. } = stmt {
                        *is_readonly = true;
                    }
                }
                rest = r;
            } else if let Some(r) = keyword("default", r) {
                let (r, _) = ws(r)?;
                if let Some(inner) = r.strip_prefix('(') {
                    let (inner, _) = ws(inner)?;
                    let (inner, default_expr) = expression(inner)?;
                    let (inner, _) = ws(inner)?;
                    let inner = inner
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is default"))?;
                    // Apply the default to all attributes in the list
                    for stmt in &mut stmts {
                        if let Stmt::HasDecl {
                            default,
                            is_default,
                            ..
                        } = stmt
                        {
                            *default = Some(default_expr.clone());
                            *is_default = Some(default_expr.clone());
                        }
                    }
                    let (r2, _) = ws(inner)?;
                    rest = r2;
                } else {
                    break;
                }
            } else {
                break;
            }
        } else {
            break;
        }
    }

    let (rest, _) = opt_char(rest, ';');
    Ok((rest, Stmt::SyntheticBlock(stmts)))
}

/// Detect `<scope> <declaration>` combinations that are illegal in Raku, e.g.
/// `has sub`, `has package`. Returns a fatal `X::Declaration::Scope` (or
/// `X::Declaration::Scope::Multi` for `multi`) parse error when `rest` begins
/// with such a declarator keyword.
pub(in crate::parser::stmt) fn scope_declaration_error(scope: &str, rest: &str) -> Option<PError> {
    // `proto` is reported by Rakudo as a `sub` declaration.
    const DECLARATORS: &[(&str, &str)] = &[
        ("sub", "sub"),
        ("proto", "sub"),
        ("package", "package"),
        ("class", "class"),
        ("module", "module"),
        ("role", "role"),
        ("grammar", "grammar"),
        ("constant", "constant"),
        ("subset", "subset"),
        ("enum", "enum"),
    ];

    if keyword("multi", rest).is_some() {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("scope".to_string(), Value::str(scope.to_string()));
        let message = format!("Cannot use '{}' with a multi declaration", scope);
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(Symbol::intern("X::Declaration::Scope::Multi"), attrs);
        return Some(PError::fatal_with_exception(message, Box::new(ex)));
    }

    for (kw, decl) in DECLARATORS {
        if keyword(kw, rest).is_some() {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("scope".to_string(), Value::str(scope.to_string()));
            attrs.insert("declaration".to_string(), Value::str((*decl).to_string()));
            let message = format!("Cannot use '{}' with {} declaration", scope, decl);
            attrs.insert("message".to_string(), Value::str(message.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Declaration::Scope"), attrs);
            return Some(PError::fatal_with_exception(message, Box::new(ex)));
        }
    }

    None
}

/// If `tc` is a coercion type constraint (`Int()`, `Int(Str)`), return its
/// target type name (`Int`). A parametric type such as `Positional[Int]` (which
/// contains `[`) is not a coercion type.
fn coercion_target_type(tc: &str) -> Option<&str> {
    if tc.ends_with(')')
        && !tc.contains('[')
        && let Some(open) = tc.find('(')
        && open > 0
    {
        Some(&tc[..open])
    } else {
        None
    }
}

/// Parse `has` attribute declaration.
pub(in crate::parser::stmt) fn has_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("has", input).ok_or_else(|| PError::expected("has declaration"))?;
    let (rest, _) = ws1(rest)?;

    // `has` cannot be used as the scope for a routine/package-style declaration.
    // `has sub`, `has package`, `has class`, etc. are X::Declaration::Scope.
    // (`has method`/`has submethod` are valid and not rejected here.)
    if let Some(err) = scope_declaration_error("has", rest) {
        return Err(err);
    }

    // Handle parenthesized list form: has ($a, $.b, $!c)
    // This desugars into a Block containing multiple HasDecl statements.
    if rest.starts_with('(') {
        return has_decl_list(rest);
    }

    // Optional type constraint.
    let (rest, mut type_constraint, type_smiley) = {
        let saved = rest;
        if let Some((r, tc)) = parse_decl_type_constraint(rest) {
            let (r2, _) = ws(r)?;
            if r2.starts_with("method") || r2.starts_with("submethod") {
                return has_type_method_decl(r2, &tc);
            }
            if r2.starts_with('$') || r2.starts_with('@') || r2.starts_with('%') {
                // Extract smiley suffix and strip it for the type_constraint name
                let (base, smiley) = if let Some(b) = tc.strip_suffix(":D") {
                    (b.to_string(), Some("D".to_string()))
                } else if let Some(b) = tc.strip_suffix(":U") {
                    (b.to_string(), Some("U".to_string()))
                } else if let Some(b) = tc.strip_suffix(":_") {
                    (b.to_string(), Some("_".to_string()))
                } else {
                    (tc.to_string(), None)
                };
                (r2, Some(base), smiley)
            } else {
                (saved, None, None)
            }
        } else {
            (saved, None, None)
        }
    };

    let sigil = rest.as_bytes().first().copied().unwrap_or(0);
    let (rest, _) = if sigil == b'$' || sigil == b'@' || sigil == b'%' || sigil == b'&' {
        (&rest[1..], ())
    } else {
        return Err(PError::expected("sigil ($, @, %, &)"));
    };

    // Check for public accessor marker '.'
    let (rest, is_public, is_alias) = if let Some(stripped) = rest.strip_prefix('.') {
        (stripped, true, false)
    } else if let Some(stripped) = rest.strip_prefix('!') {
        (stripped, false, false)
    } else {
        // No twigil: `has $x` creates a private attribute with an alias
        (rest, false, true)
    };

    let (rest, name) = take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let name = name.to_string();

    // Optional shaped-array declaration suffix: has @.a[3, 3]
    let (rest, shape_dims) = if sigil == b'@' && rest.starts_with('[') {
        let (r, dims) = parse_array_shape_suffix(rest)?;
        (r, Some(dims))
    } else {
        (rest, None)
    };

    // Optional object-hash key-type suffix: has %.a{Str:D}. The key type folds
    // into the attribute's type constraint as `ValueType{KeyType}`, exactly as a
    // lexical `my %h{Str:D}` does (see my_decl.rs).
    let rest = if sigil == b'%'
        && rest.starts_with('{')
        && !rest.starts_with("{{")
        && let Some(end) = rest.find('}')
    {
        let key_type = rest[1..end].trim().to_string();
        let value_tc = type_constraint.take();
        let combined = match value_tc {
            Some(v) => format!("{}{{{}}}", v, key_type),
            None => format!("Any{{{}}}", key_type),
        };
        type_constraint = Some(combined);
        &rest[end + 1..]
    } else {
        rest
    };

    let (mut rest, _) = ws(rest)?;

    // `of` type constraint can appear before `is` traits: `has $.a of Int is rw`
    if type_constraint.is_none()
        && let Some(r) = keyword("of", rest)
        && let Ok((r, _)) = ws1(r)
        && let Some((r, tc)) = parse_type_constraint_expr(r)
    {
        let (r, _) = ws(r)?;
        type_constraint = Some(tc);
        rest = r;
    }

    // `is` traits (may have multiple: `is rw is required`)
    // Traits come before default value: `has $.x is rw = 42`
    let mut is_rw = false;
    let mut is_readonly = false;
    let mut is_required: Option<Option<String>> = None;
    let mut is_default_trait: Option<Expr> = None;
    let mut is_type: Option<String> = None;
    let mut deprecated_message: Option<String> = None;
    let mut is_built: Option<bool> = None;
    let mut unknown_traits: Vec<(String, String, Option<Expr>)> = Vec::new();
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = ident(r)?;
        if trait_name == "rw" {
            is_rw = true;
        } else if trait_name == "readonly" {
            is_readonly = true;
        } else if trait_name == "default" {
            // `is default(expr)` — set the attribute's default value
            let (r_ws, _) = ws(r)?;
            if let Some(inner) = r_ws.strip_prefix('(') {
                let (inner, _) = ws(inner)?;
                let (inner, default_expr) = expression(inner)?;
                let (inner, _) = ws(inner)?;
                let inner = inner
                    .strip_prefix(')')
                    .ok_or_else(|| PError::expected("closing paren in is default"))?;
                is_default_trait = Some(default_expr);
                let (r2, _) = ws(inner)?;
                rest = r2;
                continue;
            }
            // `is default` without parens — just ignore (no-op)
        } else if trait_name == "required" {
            // Check for optional reason: `is required("reason")`
            let (r_ws, _) = ws(r)?;
            if let Some(inner) = r_ws.strip_prefix('(') {
                let (inner, _) = ws(inner)?;
                // Parse string literal for the reason
                if let Some(double_quoted) = inner.strip_prefix('"') {
                    let end = double_quoted
                        .find('"')
                        .ok_or_else(|| PError::expected("closing quote in is required reason"))?;
                    let reason = double_quoted[..end].to_string();
                    let after = &double_quoted[end + 1..];
                    let (after, _) = ws(after)?;
                    let after = after
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is required"))?;
                    is_required = Some(Some(reason));
                    rest = after;
                    let (r2, _) = ws(rest)?;
                    rest = r2;
                    continue;
                } else if let Some(single_quoted) = inner.strip_prefix('\'') {
                    let end = single_quoted
                        .find('\'')
                        .ok_or_else(|| PError::expected("closing quote in is required reason"))?;
                    let reason = single_quoted[..end].to_string();
                    let after = &single_quoted[end + 1..];
                    let (after, _) = ws(after)?;
                    let after = after
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is required"))?;
                    is_required = Some(Some(reason));
                    rest = after;
                    let (r2, _) = ws(rest)?;
                    rest = r2;
                    continue;
                }
            }
            is_required = Some(None);
        } else if trait_name == "DEPRECATED" {
            // `is DEPRECATED` or `is DEPRECATED("message")`
            let (r_ws, _) = ws(r)?;
            if let Some(inner) = r_ws.strip_prefix('(') {
                let (inner, _) = ws(inner)?;
                // Parse the message string
                if let Some(double_quoted) = inner.strip_prefix('"') {
                    let end = double_quoted.find('"').ok_or_else(|| {
                        PError::expected("closing quote in is DEPRECATED message")
                    })?;
                    let msg = double_quoted[..end].to_string();
                    let after = &double_quoted[end + 1..];
                    let (after, _) = ws(after)?;
                    let after = after
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is DEPRECATED"))?;
                    deprecated_message = Some(msg);
                    rest = after;
                    let (r2, _) = ws(rest)?;
                    rest = r2;
                    continue;
                } else if let Some(single_quoted) = inner.strip_prefix('\'') {
                    let end = single_quoted.find('\'').ok_or_else(|| {
                        PError::expected("closing quote in is DEPRECATED message")
                    })?;
                    let msg = single_quoted[..end].to_string();
                    let after = &single_quoted[end + 1..];
                    let (after, _) = ws(after)?;
                    let after = after
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is DEPRECATED"))?;
                    deprecated_message = Some(msg);
                    rest = after;
                    let (r2, _) = ws(rest)?;
                    rest = r2;
                    continue;
                }
                // Non-string expression in parens — find closing paren
                let mut depth = 1u32;
                let mut idx = 0;
                for (i, ch) in inner.char_indices() {
                    match ch {
                        '(' => depth += 1,
                        ')' => {
                            depth -= 1;
                            if depth == 0 {
                                idx = i;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                let after = &inner[idx + 1..];
                deprecated_message = Some(String::new());
                rest = after;
                let (r2, _) = ws(rest)?;
                rest = r2;
                continue;
            }
            deprecated_message = Some(String::new());
        } else if trait_name == "built" {
            // `is built` or `is built(False)`
            let (r_ws, _) = ws(r)?;
            if let Some(inner) = r_ws.strip_prefix('(') {
                let (inner, _) = ws(inner)?;
                if let Some(after) = inner.strip_prefix("False") {
                    let (after, _) = ws(after)?;
                    let after = after
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is built"))?;
                    is_built = Some(false);
                    let (r2, _) = ws(after)?;
                    rest = r2;
                    continue;
                } else if let Some(after) = inner.strip_prefix("True") {
                    let (after, _) = ws(after)?;
                    let after = after
                        .strip_prefix(')')
                        .ok_or_else(|| PError::expected("closing paren in is built"))?;
                    is_built = Some(true);
                    let (r2, _) = ws(after)?;
                    rest = r2;
                    continue;
                }
                // Skip unknown content in parens
                let mut depth = 1u32;
                let mut idx = 0;
                for (i, ch) in inner.char_indices() {
                    match ch {
                        '(' => depth += 1,
                        ')' => {
                            depth -= 1;
                            if depth == 0 {
                                idx = i;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                let after = &inner[idx + 1..];
                is_built = Some(true);
                let (r2, _) = ws(after)?;
                rest = r2;
                continue;
            }
            is_built = Some(true);
        } else if trait_name
            .chars()
            .next()
            .is_some_and(|c| c.is_ascii_uppercase())
        {
            // Uppercase-starting trait name: `is Buf`, `is BagHash`, etc.
            // This is a container type trait for `@`/`%` attributes.
            is_type = Some(trait_name.to_string());
        } else {
            // Unknown lowercase trait — dispatched to a custom `trait_mod:<is>`
            // at class registration, or X::Comp::Trait::Unknown if none exists.
            // Capture optional argument in parens: `is doc('barks')` -> `'barks'`.
            let (r_ws, _) = ws(r)?;
            if let Some(stripped) = r_ws.strip_prefix('(') {
                let mut depth = 1u32;
                let mut idx = 0;
                for (i, ch) in stripped.char_indices() {
                    match ch {
                        '(' => depth += 1,
                        ')' => {
                            depth -= 1;
                            if depth == 0 {
                                idx = i;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                let inner = &stripped[..idx];
                let mut trait_arg: Option<Expr> = None;
                if !inner.trim().is_empty()
                    && let Ok((leftover, arg_expr)) = expression(inner)
                    && leftover.trim().is_empty()
                {
                    trait_arg = Some(arg_expr);
                }
                unknown_traits.push(("is".to_string(), trait_name.to_string(), trait_arg));
                rest = &stripped[idx + 1..];
                let (r2, _) = ws(rest)?;
                rest = r2;
                continue;
            }
            unknown_traits.push(("is".to_string(), trait_name.to_string(), None));
        }
        let (r, _) = ws(r)?;
        rest = r;
    }

    // Parse `will` traits on has declarations
    while let Some(r) = keyword("will", rest) {
        let Ok((r, _)) = ws1(r) else { break };
        let Ok((r, trait_name)) = ident(r) else { break };
        // `will` traits are not supported on attributes, record for X::Comp::Trait::Unknown
        unknown_traits.push(("will".to_string(), trait_name.to_string(), None));
        // Skip optional block: `will bar { ... }` -> skip the block
        let (r_ws, _) = ws(r)?;
        if let Some(stripped_block) = r_ws.strip_prefix('{') {
            let mut depth = 1u32;
            let mut idx = 0;
            for (i, ch) in stripped_block.char_indices() {
                match ch {
                    '{' => depth += 1,
                    '}' => {
                        depth -= 1;
                        if depth == 0 {
                            idx = i + 1;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            rest = &stripped_block[idx..];
            let (r2, _) = ws(rest)?;
            rest = r2;
        } else {
            let (r2, _) = ws(r)?;
            rest = r2;
        }
    }

    // `does Role` trait on an attribute, e.g. `has $.x does Foo` — mixes the role
    // into the attribute's container. Parsed here as an attribute trait (recorded
    // in `unknown_traits` as `("does", role, None)`); without this it is left for
    // the statement parser, which mis-reads it as a class-level `does Foo`
    // composition. Applied to the attribute's value at construction.
    while let Some(r) = keyword("does", rest) {
        let Ok((r, _)) = ws1(r) else { break };
        let Ok((r, role_name)) = ident(r) else { break };
        unknown_traits.push(("does".to_string(), role_name.to_string(), None));
        let (r, _) = ws(r)?;
        rest = r;
    }

    // `handles` trait, e.g. `has $.x handles <a b>`
    let mut handles = Vec::new();
    while let Some(r) = keyword("handles", rest) {
        let (r, _) = ws1(r)?;
        parse_handle_specs(r, &mut handles, &mut rest)?;
        let (r, _) = ws(rest)?;
        rest = r;
    }

    // Postfix container typing: has $.a of Int; has @.a of Int; has %.h of Str;
    if type_constraint.is_none()
        && let Some(r) = keyword("of", rest)
    {
        let (r, _) = ws1(r)?;
        let (r, tc) = parse_type_constraint_expr(r).ok_or_else(|| PError::expected("type"))?;
        let (r, _) = ws(r)?;
        type_constraint = Some(tc);
        rest = r;
    }

    // Optional `where` constraint
    let (rest, where_constraint) = if let Some(r) = keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, pred) = expression(r)?;
        let (r, _) = ws(r)?;
        (r, Some(Box::new(pred)))
    } else {
        (rest, None)
    };

    // Default value
    let (rest, mut default) = if let Some(stripped) = rest.strip_prefix(".=") {
        let (rest, _) = ws(stripped)?;
        let (rest, method_name) =
            take_while1(rest, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let method_name = method_name.to_string();
        let (rest, args) = if rest.starts_with('(') {
            let (r, _) = parse_char(rest, '(')?;
            let (r, _) = ws(r)?;
            let (r, args) = super::super::super::primary::parse_call_arg_list(r)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ')')?;
            (r, args)
        } else if rest.starts_with(':') && !rest.starts_with("::") {
            let r = &rest[1..];
            let (r, _) = ws(r)?;
            let (r, first_arg) = parse_colon_method_arg(r)?;
            let mut args = vec![first_arg];
            let mut r_inner = r;
            loop {
                let (r2, _) = ws(r_inner)?;
                if r2.starts_with(':')
                    && !r2.starts_with("::")
                    && let Ok((r3, arg)) = crate::parser::primary::misc::colonpair_expr(r2)
                {
                    args.push(arg);
                    r_inner = r3;
                    continue;
                }
                if !r2.starts_with(',') {
                    break;
                }
                let r2 = &r2[1..];
                let (r2, _) = ws(r2)?;
                // Handle trailing comma before ';' or '}'
                if r2.starts_with(';') || r2.starts_with('}') || r2.is_empty() {
                    r_inner = r2;
                    break;
                }
                let (r2, next) = parse_colon_method_arg(r2)?;
                args.push(next);
                r_inner = r2;
            }
            (r_inner, args)
        } else {
            (rest, Vec::new())
        };
        let target_name = type_constraint.clone().unwrap_or_else(|| name.clone());
        let target_expr = if target_name == "::?CLASS" {
            Expr::Var("?CLASS".to_string())
        } else if target_name == "::?ROLE" {
            Expr::Var("?ROLE".to_string())
        } else {
            Expr::BareWord(target_name)
        };
        (
            rest,
            Some(Expr::MethodCall {
                target: Box::new(target_expr),
                name: Symbol::intern(&method_name),
                args,
                modifier: None,
                quoted: false,
            }),
        )
    } else if rest.starts_with('=') && !rest.starts_with("==") {
        let rest = &rest[1..];
        let (rest, _) = ws(rest)?;
        let (rest, expr) = expression(rest)?;
        // For @ and % sigils, parse comma-separated list of expressions
        if (sigil == b'@' || sigil == b'%') && rest.starts_with(',') {
            let mut items = vec![expr];
            let mut r = rest;
            while r.starts_with(',') {
                r = &r[1..];
                let (r2, _) = ws(r)?;
                let (r2, next_expr) = expression(r2)?;
                items.push(next_expr);
                r = r2;
            }
            (r, Some(Expr::ArrayLiteral(items)))
        } else {
            (rest, Some(expr))
        }
    } else if let Some(default_expr) = is_default_trait.clone() {
        // `is default(expr)` was used (no `=` initializer). For a `$`/`&` scalar
        // attribute the value doubles as the initializer (an unassigned scalar
        // reads back as its default). For an `@`/`%` container the value is the
        // *element* default, NOT the container itself — leave the initializer
        // empty so the container is built empty and gets its `.default` tagged
        // from the `is_default` trait at construction time.
        if sigil == b'@' || sigil == b'%' {
            (rest, None)
        } else {
            (rest, Some(default_expr))
        }
    } else {
        (rest, None)
    };

    // Track whether user provided an explicit default (before auto-default)
    let has_explicit_default = default.is_some();

    // A placeholder variable (`$^b`, `@_`, ...) used directly in an attribute
    // initializer cannot be captured by any signature -> X::Placeholder::Attribute.
    if let Some(def) = default.as_ref()
        && let Some(ph) = crate::ast::collect_unattached_placeholders(std::slice::from_ref(
            &Stmt::Expr(def.clone()),
        ))
        .into_iter()
        .next()
    {
        let message = format!(
            "Cannot use placeholder parameter {} in an attribute initializer",
            ph
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("placeholder".to_string(), Value::str(ph));
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(Symbol::intern("X::Placeholder::Attribute"), attrs);
        return Err(PError::fatal_with_exception(message, Box::new(ex)));
    }

    // A virtual accessor call (`$.y`) in an attribute initializer dereferences the
    // partially-constructed invocant -> X::Syntax::VirtualCall.
    if let Some(def) = default.as_ref()
        && let Some(call) = crate::ast::first_virtual_call_in_expr(def)
    {
        // suggest the $!attr direct-access form (e.g. `$.y` -> `$!y`)
        let direct = format!("{}!{}", &call[..1], &call[2..]);
        let message = format!(
            "Virtual method call {} may not be used on partially constructed object (maybe you mean {} for direct attribute access here?)",
            call, direct
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("call".to_string(), Value::str(call));
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(Symbol::intern("X::Syntax::VirtualCall"), attrs);
        return Err(PError::fatal_with_exception(message, Box::new(ex)));
    }

    // Apply `use attributes :D/:U/:_` pragma if no explicit smiley on the type
    let smiley_from_pragma = type_smiley.is_none() && type_constraint.is_some() && {
        let pragma = super::super::simple::current_attributes_pragma();
        matches!(pragma.as_str(), ":D" | ":U")
    };
    let type_smiley = if type_smiley.is_none() && type_constraint.is_some() {
        let pragma = super::super::simple::current_attributes_pragma();
        match pragma.as_str() {
            ":D" => Some("D".to_string()),
            ":U" => Some("U".to_string()),
            _ => type_smiley, // ":_" or empty - no change
        }
    } else {
        type_smiley
    };

    // Enforce type smiley constraints at parse time. A `:D` smiley on an array
    // (`@`) or hash (`%`) attribute constrains the ELEMENTS, not the container —
    // the container itself defaults to an empty (defined) Array/Hash, so it needs
    // no initializer. Only scalar-like attributes (`$`, `&`) must be initialized.
    let effective_smiley = type_smiley.as_deref().unwrap_or("_");
    if effective_smiley == "D"
        && sigil != b'@'
        && sigil != b'%'
        && !has_explicit_default
        && is_required.is_none()
    {
        // :D attribute without a default or `is required` → X::Syntax::Variable::MissingInitializer
        let twigil = if is_public { "." } else { "!" };
        let tc_display = if let Some(ref tc) = type_constraint {
            format!("{}:D", tc)
        } else {
            "Any:D".to_string()
        };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("type".to_string(), Value::str(tc_display));
        attrs.insert("name".to_string(), Value::str(format!("$!{}", name)));
        if smiley_from_pragma {
            attrs.insert(
                "implicit".to_string(),
                Value::str(":D by pragma".to_string()),
            );
        }
        let ex = Value::make_instance(
            Symbol::intern("X::Syntax::Variable::MissingInitializer"),
            attrs,
        );
        return Err(PError::fatal_with_exception(
            format!(
                "Variable '{}{}{}' of type '{}:D' must be initialized",
                sigil as char,
                twigil,
                name,
                type_constraint.as_deref().unwrap_or("Any")
            ),
            Box::new(ex),
        ));
    }

    // Auto-default: typed scalar attribute with no explicit default → use type object
    // Native types (int, atomicint, num, str, etc.) get zero/empty defaults.
    if !has_explicit_default
        && is_required.is_none()
        && sigil == b'$'
        && let Some(ref tc) = type_constraint
    {
        if tc == "::?CLASS" {
            default = Some(Expr::Var("?CLASS".to_string()));
        } else if tc == "::?ROLE" {
            default = Some(Expr::Var("?ROLE".to_string()));
        } else if let Some(target) = coercion_target_type(tc) {
            // A coercion type (`Int()`, `Int(Str)`): an uninitialized attribute
            // defaults to the *target* type object (`Int`), not a bareword of the
            // whole coercion spec. (The provided-value coercion happens at
            // construction time.)
            default = Some(Expr::BareWord(target.to_string()));
        } else {
            default = Some(match tc.as_str() {
                "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
                | "uint32" | "uint64" | "byte" | "atomicint" => Expr::Literal(Value::Int(0)),
                "num" | "num32" | "num64" => Expr::Literal(Value::Num(0.0)),
                "str" => Expr::Literal(Value::str("".to_string())),
                _ => Expr::BareWord(tc.clone()),
            });
        }
    }

    if sigil == b'@' {
        if let Some(dims) = shape_dims {
            // Shaped array attribute: has @.a[3, 3]
            if let Some(data_expr) = default.take() {
                default = Some(shaped_array_new_with_data_expr(dims, data_expr));
            } else {
                default = Some(shaped_array_new_expr(dims));
            }
        } else if let Some(expr) = default.take() {
            default = Some(match expr {
                Expr::ArrayLiteral(_)
                | Expr::BracketArray(..)
                | Expr::ArrayVar(_)
                | Expr::Var(_)
                | Expr::Index { .. } => expr,
                other => Expr::ArrayLiteral(vec![other]),
            });
        }
    }
    let (rest, _) = ws(rest)?;

    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::HasDecl {
            name: Symbol::intern(&name),
            is_public,
            default,
            handles,
            is_rw,
            is_readonly,
            type_constraint,
            type_smiley,
            is_required,
            sigil: sigil as char,
            where_constraint,
            is_alias,
            is_our: false,
            is_my: false,
            is_default: is_default_trait,
            is_type,
            deprecated_message,
            is_built,
            unknown_traits,
        },
    ))
}

fn has_type_method_decl<'a>(input: &'a str, has_type: &str) -> PResult<'a, Stmt> {
    let is_submethod_kw = input.starts_with("submethod");
    let kw = if is_submethod_kw {
        "submethod"
    } else {
        "method"
    };
    let rest = keyword(kw, input).ok_or_else(|| PError::expected("method or submethod"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, mut stmt) = method_decl_body(rest, false, false)?;
    if let Stmt::MethodDecl {
        ref name,
        ref mut return_type,
        ref mut is_submethod,
        ..
    } = stmt
    {
        if let Some(existing_rt) = return_type {
            let msg = format!(
                "X::Redeclaration: Redeclaration of return type for '{}' (previous return type was {}).",
                name, existing_rt
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("symbol".to_string(), Value::str(name.to_string()));
            attrs.insert("what".to_string(), Value::str("return type".to_string()));
            let ex = Value::make_instance(Symbol::intern("X::Redeclaration"), attrs);
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
        *return_type = Some(has_type.to_string());
        if is_submethod_kw {
            *is_submethod = true;
        }
    }
    Ok((rest, stmt))
}
