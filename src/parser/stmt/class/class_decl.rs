use super::*;

use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

use crate::parser::expr::expression;
use crate::parser::helpers::{skip_balanced_parens, ws, ws1};
use crate::parser::parse_result::{PError, PResult, opt_char, take_while1};
use crate::parser::stmt::sub::parse_indirect_decl_name;
use crate::parser::stmt::{block, ident, keyword, qualified_ident};

pub(crate) fn parse_declarator_traits(input: &str) -> PResult<'_, Vec<(String, Value)>> {
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
                value = Value::str(inner[..end].to_string());
                rest = &inner[end + 1..];
            } else {
                return Err(PError::expected("closing '>' in trait value"));
            }
        } else if rest.starts_with('(') {
            let after = skip_balanced_parens(rest);
            let body = &rest[1..rest.len() - after.len() - 1];
            value = Value::str(body.trim().to_string());
            rest = after;
        }
        traits.push((name.to_string(), value));
        let (r, _) = ws(rest)?;
        rest = r;
    }
    Ok((rest, traits))
}

pub(crate) fn parse_optional_bracket_suffix(input: &str) -> PResult<'_, String> {
    if !input.starts_with('[') {
        return Ok((input, String::new()));
    }
    let mut depth = 0u32;
    let mut end = None;
    for (i, ch) in input.char_indices() {
        if ch == '[' {
            depth += 1;
        } else if ch == ']' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                end = Some(i + 1);
                break;
            }
        }
    }
    let end = end.ok_or_else(|| PError::expected("closing ']'"))?;
    Ok((&input[end..], input[..end].to_string()))
}

pub(crate) fn meta_setter_stmt(type_name: &str, key: &str, value: Value) -> Stmt {
    Stmt::Expr(Expr::Call {
        name: Symbol::intern("__MUTSU_SET_META__"),
        args: vec![
            Expr::Literal(Value::str(type_name.to_string())),
            Expr::Literal(Value::str(key.to_string())),
            Expr::Literal(value),
        ],
    })
}

/// Parse `class` declaration.
pub(crate) fn class_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("class", input).ok_or_else(|| PError::expected("class declaration"))?;
    let (rest, _) = ws1(rest)?;
    class_decl_body(rest, false)
}

/// Parse `augment class ClassName { ... }` declaration (monkey-patching).
/// Also handles `augment role RoleName { ... }` (always illegal — roles are
/// closed) and the anonymous form `augment class { ... }` (X::Anon::Augment).
pub(crate) fn augment_class_decl(input: &str) -> PResult<'_, Stmt> {
    let rest =
        keyword("augment", input).ok_or_else(|| PError::expected("augment class declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, is_role) = if let Some(r) = keyword("class", rest) {
        (r, false)
    } else if let Some(r) = keyword("role", rest) {
        (r, true)
    } else {
        return Err(PError::expected("'class' or 'role' after 'augment'"));
    };
    let (rest, _) = ws1(rest)?;
    // Anonymous augment (`augment class { }` / `augment role { }`) — no name
    // follows the package kind. Raku rejects this with X::Anon::Augment.
    let package_kind = if is_role { "role" } else { "class" };
    if rest.starts_with('{') {
        let msg = format!("Cannot augment anonymous {}", package_kind);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert(
            "package-kind".to_string(),
            Value::str(package_kind.to_string()),
        );
        let ex = Value::make_instance(Symbol::intern("X::Anon::Augment"), attrs);
        return Err(PError::fatal_with_exception(msg, Box::new(ex)));
    }
    let (rest, name) = qualified_ident(rest)?;
    // Check for type adverbs (:D, :U, :auth, :ver, :api) which are not allowed on augment
    if rest.starts_with(':') && !rest.starts_with("::") {
        let adverb_rest = &rest[1..];
        // Check for known type adverbs
        let is_adverb = adverb_rest.starts_with('D')
            || adverb_rest.starts_with('U')
            || adverb_rest.starts_with("auth")
            || adverb_rest.starts_with("ver")
            || adverb_rest.starts_with("api");
        if is_adverb {
            let msg = "Cannot put adverbs on a typename when augmenting".to_string();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Syntax::Augment::Adverb"), attrs);
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
    }
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::AugmentClass {
            name: Symbol::intern(&name),
            body,
            is_role,
        },
    ))
}

/// Parse `anon class Name { ... }` declaration.
/// The class is created but not installed in the lexical scope.
/// Emits a ClassDecl (registered under the real name) followed by a call to
/// `__MUTSU_UNREGISTER_CLASS__` to remove the name from the scope.
pub(crate) fn anon_class_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("anon", input).ok_or_else(|| PError::expected("anon class declaration"))?;
    let (rest, _) = ws1(rest)?;
    let rest = keyword("class", rest).ok_or_else(|| PError::expected("class after anon"))?;
    let (rest, _) = ws1(rest)?;
    // Parse the class name
    let (rest, user_name) = qualified_ident(rest)?;
    let (rest, _) = ws(rest)?;
    // Parse parent classes
    let mut parents = Vec::new();
    let mut anon_repr: Option<String> = None;
    let mut r = rest;
    while let Some(r2) = keyword("is", r) {
        let (r2, _) = ws1(r2)?;
        let (r2, parent) = qualified_ident(r2)?;
        if parent == "repr" {
            if let Some(inner) = r2.strip_prefix('(') {
                let end = inner.find(')').unwrap_or(inner.len());
                let repr_val = inner[..end].trim().trim_matches('\'').trim_matches('"');
                anon_repr = Some(repr_val.to_string());
            }
            let r2 = skip_balanced_parens(r2);
            let (r2, _) = ws(r2)?;
            r = r2;
            continue;
        }
        parents.push(parent);
        let (r2, _) = ws(r2)?;
        r = r2;
    }
    let (rest, body) = block(r)?;
    reject_no_self_in_subs(&body)?;
    reject_no_self_in_attr_where(&body)?;
    reject_no_twigil_attr_at_body_level(&body)?;
    let class_decl = Stmt::ClassDecl {
        name: Symbol::intern(&user_name),
        name_expr: None,
        parents,
        class_is_rw: false,
        is_hidden: false,
        is_lexical: false,
        hidden_parents: Vec::new(),
        does_parents: Vec::new(),
        repr: anon_repr,
        body,
        language_version: super::super::simple::current_language_version(),
        custom_traits: Vec::new(),
        is_unit: false,
        decl_id: crate::ast::next_class_decl_id(),
    };
    // Emit the class registration followed by unregistering the name from the scope
    let unregister = Stmt::Expr(Expr::Call {
        name: Symbol::intern("__MUTSU_UNREGISTER_CLASS__"),
        args: vec![Expr::Literal(Value::str(user_name))],
    });
    Ok((rest, Stmt::Block(vec![class_decl, unregister])))
}

/// Parse the body of a class declaration (after `class` keyword and whitespace).
pub(crate) fn class_decl_body(input: &str, is_lexical: bool) -> PResult<'_, Stmt> {
    let (rest, name, name_expr) = if let Some(after_colons) = input.strip_prefix("::") {
        // Check if this is `::Ident` (forward stub) or `::(expr)` (indirect name)
        if after_colons.starts_with('(') {
            let (rest, (name, expr)) = parse_indirect_decl_name(input)?;
            (rest, name, Some(expr))
        } else {
            // `class ::F { ... }` — forward declaration stub, name is just the identifier
            let (rest, name) = qualified_ident(after_colons)?;
            (rest, name, None)
        }
    } else {
        let (rest, name) = qualified_ident(input)?;
        (rest, name, None)
    };
    check_pseudo_package_in_decl(&name)?;
    let (rest, traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;

    // Parent clauses in any order: `is Parent`, `does Role[...]`, `hides Parent`.
    let mut is_hidden = false;
    let mut class_is_rw = false;
    let mut hidden_parents = Vec::new();
    let mut parents = Vec::new();
    let mut does_parents = Vec::new();
    let mut is_repr: Option<String> = None;
    let mut custom_traits: Vec<(String, Option<Expr>)> = Vec::new();
    let mut r = rest;
    loop {
        if let Some(r2) = keyword("is", r) {
            let (r2, _) = ws1(r2)?;
            // Handle `is ::Foo` (indirect name lookup) — treat the
            // `::Ident` as a parent name so that validation later
            // produces X::Inheritance::UnknownParent.
            let (r2, parent) = if let Some(stripped) = r2.strip_prefix("::") {
                let (r3, ident_part) = qualified_ident(stripped)?;
                (r3, format!("::{}", ident_part))
            } else {
                qualified_ident(r2)?
            };
            if parent == "hidden" {
                is_hidden = true;
            } else if parent == "rw" {
                class_is_rw = true;
            } else if parent == "repr" {
                // Extract repr value from `is repr('CUnion')` etc.
                if let Some(inner) = r2.strip_prefix('(') {
                    // Find the content between parens, stripping quotes
                    let end = inner.find(')').unwrap_or(inner.len());
                    let repr_val = inner[..end].trim().trim_matches('\'').trim_matches('"');
                    is_repr = Some(repr_val.to_string());
                }
                let r2 = skip_balanced_parens(r2);
                let (r2, _) = ws(r2)?;
                r = r2;
                continue;
            } else if parent == "DEPRECATED" {
                // `is DEPRECATED` on a class — skip optional parenthesized arg
                let r2 = skip_balanced_parens(r2);
                let (r2, _) = ws(r2)?;
                r = r2;
                continue;
            } else if parent.starts_with(|c: char| c.is_ascii_uppercase())
                || parent.starts_with("::")
            {
                let (r2, bracket_suffix) = parse_optional_bracket_suffix(r2)?;
                parents.push(format!("{}{}", parent, bracket_suffix));
                r = r2;
                let (r2, _) = ws(r)?;
                r = r2;
                continue;
            }
            // Unrecognized `is` trait with lowercase name — check for custom
            // trait_mod:<is> dispatch with optional parenthesized argument, or
            // treat as a potential parent class.
            if !matches!(
                parent.as_str(),
                "rw" | "hidden"
                    | "export"
                    | "DEPRECATED"
                    | "default"
                    | "nodal"
                    | "raw"
                    | "required"
                    | "built"
                    | "cached"
                    | "repr"
                    | "open"
            ) {
                // Check for parenthesized argument: `is trait-name('arg')`
                // This indicates a custom trait_mod:<is> call.
                if let Some(inner_start) = r2.strip_prefix('(') {
                    if let Ok((after_expr, expr)) = expression(inner_start) {
                        let after_expr = after_expr.trim_start();
                        if let Some(after_paren) = after_expr.strip_prefix(')') {
                            custom_traits.push((parent.clone(), Some(expr)));
                            let (r3, _) = ws(after_paren)?;
                            r = r3;
                            continue;
                        }
                    }
                    // Fallback: skip balanced parens
                    let r3 = skip_balanced_parens(r2);
                    custom_traits.push((parent.clone(), None));
                    let (r3, _) = ws(r3)?;
                    r = r3;
                    continue;
                }
                let (r2, bracket_suffix) = parse_optional_bracket_suffix(r2)?;
                parents.push(format!("{}{}", parent, bracket_suffix));
                r = r2;
                let (r2, _) = ws(r)?;
                r = r2;
                continue;
            }
            let (r2, _) = ws(r2)?;
            r = r2;
            continue;
        }
        if let Some(r2) = keyword("does", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, role_name) = qualified_ident(r2)?;
            let (r2, _) = ws(r2)?;
            let (r2, bracket_suffix) = parse_optional_bracket_suffix(r2)?;
            let full_name = format!("{}{}", role_name, bracket_suffix);
            parents.push(full_name.clone());
            does_parents.push(full_name);
            let (r2, _) = ws(r2)?;
            r = r2;
            continue;
        }
        if let Some(r2) = keyword("hides", r) {
            let (r2, _) = ws1(r2)?;
            let (r2, parent) = qualified_ident(r2)?;
            let (r2, _) = ws(r2)?;
            let (r2, bracket_suffix) = parse_optional_bracket_suffix(r2)?;
            let hidden_parent = format!("{}{}", parent, bracket_suffix);
            parents.push(hidden_parent.clone());
            hidden_parents.push(hidden_parent);
            let (r2, _) = ws(r2)?;
            r = r2;
            continue;
        }
        break;
    }

    let (rest, mut body) = block(r)?;
    reject_no_self_in_subs(&body)?;
    reject_no_self_in_attr_where(&body)?;
    reject_no_twigil_attr_at_body_level(&body)?;
    body.retain(|stmt| {
        if stmt_is_also_is_rw(stmt) {
            class_is_rw = true;
            false
        } else if let Some(parent_name) = stmt_also_is_parent(stmt) {
            parents.push(parent_name);
            false
        } else {
            true
        }
    });
    // Extract repr from `is repr(...)` or from declarator traits
    let repr = is_repr.or_else(|| {
        traits.iter().find_map(|(k, v)| {
            if k == "repr" {
                if let Value::Str(s) = v {
                    Some(s.to_string())
                } else {
                    None
                }
            } else {
                None
            }
        })
    });
    // Register the class name so the parser can disambiguate identifiers
    // from regex/substitution operators (e.g. `S` vs `S///`).
    super::super::simple::register_user_type(&name);

    // Record `is export` operator methods so `import ClassName` teaches the
    // parser the new operator symbols (`method infix:<as> is export` makes `as`
    // a usable infix in code parsed after the `import`).
    let exported_ops = super::package_decl::extract_exported_operator_methods(&body);
    if !exported_ops.is_empty() {
        super::super::simple::register_inline_module_exports(&name, exported_ops);
    }

    // Validate declarator traits: only 'ver', 'auth', and 'api' are allowed on class names
    for (trait_name, _) in &traits {
        if trait_name != "ver" && trait_name != "auth" && trait_name != "api" {
            let msg = format!(
                "Cannot use adverb {} on a type name (only 'ver', 'auth' and 'api' are understood)",
                trait_name
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Syntax::Type::Adverb"), attrs);
            return Err(PError::fatal_with_exception(msg, Box::new(ex)));
        }
    }
    let class_stmt = Stmt::ClassDecl {
        name: Symbol::intern(&name),
        name_expr,
        parents,
        class_is_rw,
        is_hidden,
        is_lexical,
        hidden_parents,
        does_parents,
        repr,
        body,
        language_version: super::super::simple::current_language_version(),
        custom_traits,
        is_unit: false,
        decl_id: crate::ast::next_class_decl_id(),
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

/// Parse `also is <trait>;` statement.
///
/// This is primarily used inside class bodies (e.g. `also is rw;`) and is
/// represented as a small infix expression statement for later lowering.
pub(crate) fn also_trait_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("also", input).ok_or_else(|| PError::expected("also trait statement"))?;
    let (rest, _) = ws1(rest)?;
    // Handle `also does RoleName;`
    if let Some(r) = keyword("does", rest) {
        let (r, _) = ws1(r)?;
        let (r, name) = parse_token_like_name(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::DoesDecl {
                name: Symbol::intern(&name),
            },
        ));
    }
    let rest = keyword("is", rest).ok_or_else(|| PError::expected("is or does after also"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, trait_name) = ident(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::Expr(Expr::InfixFunc {
            name: "is".to_string(),
            left: Box::new(Expr::BareWord("also".to_string())),
            right: vec![Expr::BareWord(trait_name)],
            modifier: None,
        }),
    ))
}
