use super::*;

use crate::ast::Stmt;
use crate::symbol::Symbol;
use crate::value::Value;

use crate::parser::helpers::{ws, ws1};
use crate::parser::parse_result::{PError, PResult, opt_char, parse_char};
use crate::parser::primary::var::is_pseudo_package;
use crate::parser::stmt::sub::parse_sub_name;
use crate::parser::stmt::{block, keyword, parse_param_list, parse_sub_traits, qualified_ident};

/// Extract names of exported sub declarations from a statement list.
pub(crate) fn extract_exported_subs(
    stmts: &[Stmt],
) -> Vec<super::super::simple::InlineModuleExportSpec> {
    let mut names = Vec::new();
    for stmt in stmts {
        match stmt {
            Stmt::SubDecl {
                name,
                is_export,
                precedence_trait,
                associativity,
                ..
            } if *is_export => {
                names.push((
                    name.to_string(),
                    precedence_trait.clone(),
                    associativity.clone(),
                ));
            }
            Stmt::SyntheticBlock(inner) => {
                names.extend(extract_exported_subs(inner));
            }
            _ => {}
        }
    }
    names
}

/// Recursively collect exported sub names (descending into nested blocks),
/// returning the first symbol that is exported more than once. In Raku two
/// `is export` declarations of the same symbol within one package raise
/// X::Export::NameClash at compile time.
pub(crate) fn find_export_name_clash(stmts: &[Stmt]) -> Option<String> {
    let mut seen = std::collections::HashSet::new();
    fn walk(stmts: &[Stmt], seen: &mut std::collections::HashSet<String>) -> Option<String> {
        for stmt in stmts {
            match stmt {
                // `multi` candidates legitimately share a name, so only a
                // non-multi (`only`) exported sub can clash.
                Stmt::SubDecl {
                    name,
                    is_export,
                    multi,
                    ..
                } if *is_export && !*multi => {
                    if !seen.insert(name.to_string()) {
                        return Some(name.to_string());
                    }
                }
                Stmt::Block(inner) | Stmt::SyntheticBlock(inner) => {
                    if let Some(clash) = walk(inner, seen) {
                        return Some(clash);
                    }
                }
                _ => {}
            }
        }
        None
    }
    walk(stmts, &mut seen)
}

/// Build an X::Export::NameClash parse error for a symbol exported twice.
pub(crate) fn export_name_clash_error(name: &str) -> PError {
    let symbol = format!("&{}", name);
    let msg = format!("A symbol '{}' has already been exported", symbol);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("symbol".to_string(), Value::str(symbol));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Export::NameClash"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

/// Check if a declaration name is a pseudo-package and return an error if so.
/// In Raku, the action is always "package name" regardless of the specific declarator.
pub(crate) fn check_pseudo_package_in_decl(name: &str) -> Result<(), PError> {
    if is_pseudo_package(name) {
        let action = "package name";
        let msg = format!("Cannot use pseudo package {} in {}", name, action);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("pseudo-package".to_string(), Value::str(name.to_string()));
        attrs.insert("action".to_string(), Value::str(action.to_string()));
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let ex = Value::make_instance(Symbol::intern("X::PseudoPackage::InDeclaration"), attrs);
        return Err(PError::fatal_with_exception(msg, Box::new(ex)));
    }
    Ok(())
}

/// Parse `unit module` or `unit class` statement.
pub(crate) fn unit_module_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unit", input).ok_or_else(|| PError::expected("unit statement"))?;
    let (rest, _) = ws1(rest)?;
    // unit class Name;
    // unit class Name is Parent does Role;
    if let Some(r) = keyword("class", rest) {
        let (r, _) = ws1(r)?;
        let (r, name) = qualified_ident(r)?;
        check_pseudo_package_in_decl(&name)?;
        // Skip optional type adverbs (:ver<...>, :auth<...>, :api<...>)
        let (r, _traits) = parse_declarator_traits(r)?;
        let (r, _) = ws(r)?;
        // Parse `is Parent` and `does Role` clauses before the semicolon
        let mut parents = Vec::new();
        let mut does_parents = Vec::new();
        let mut class_is_rw = false;
        let mut is_hidden = false;
        let mut hidden_parents = Vec::new();
        let mut r = r;
        loop {
            if let Some(r2) = keyword("is", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, parent) = if let Some(stripped) = r2.strip_prefix("::") {
                    let (r3, ident_part) = qualified_ident(stripped)?;
                    (r3, format!("::{}", ident_part))
                } else {
                    qualified_ident(r2)?
                };
                if parent == "rw" {
                    class_is_rw = true;
                } else if parent == "hidden" {
                    is_hidden = true;
                } else {
                    parents.push(parent);
                }
                let (r2, _) = ws(r2)?;
                r = r2;
                continue;
            }
            if let Some(r2) = keyword("does", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, role_name) = qualified_ident(r2)?;
                parents.push(role_name.clone());
                does_parents.push(role_name);
                let (r2, _) = ws(r2)?;
                r = r2;
                continue;
            }
            if let Some(r2) = keyword("hides", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, parent) = qualified_ident(r2)?;
                parents.push(parent.clone());
                hidden_parents.push(parent);
                let (r2, _) = ws(r2)?;
                r = r2;
                continue;
            }
            break;
        }
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::ClassDecl {
                name: Symbol::intern(&name),
                name_expr: None,
                parents,
                class_is_rw,
                is_hidden,
                is_lexical: false,
                hidden_parents,
                does_parents,
                repr: None,
                body: Vec::new(),
                language_version: super::super::simple::current_language_version(),
                custom_traits: Vec::new(),
                is_unit: true,
                decl_id: crate::ast::next_class_decl_id(),
            },
        ));
    }
    // unit role Name;  — declare a role at the file scope.
    if let Some(r) = keyword("role", rest) {
        let (r, _) = ws1(r)?;
        let (r, name) = qualified_ident(r)?;
        check_pseudo_package_in_decl(&name)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::RoleDecl {
                name: Symbol::intern(&name),
                type_params: Vec::new(),
                type_param_defs: Vec::new(),
                is_export: false,
                export_tags: Vec::new(),
                body: Vec::new(),
                is_rw: false,
                language_version: super::super::simple::current_language_version(),
                custom_traits: Vec::new(),
            },
        ));
    }
    // unit grammar Name;  — declare a grammar at the file scope.
    // unit grammar Name is Parent;
    if let Some(r) = keyword("grammar", rest) {
        let (r, _) = ws1(r)?;
        let (r, name) = qualified_ident(r)?;
        check_pseudo_package_in_decl(&name)?;
        // Consume optional type adverbs (`:ver<...>`, `:auth<...>`, `:api<...>`)
        // on the grammar name before the `is`/`does` parent clauses, e.g.
        // `unit grammar Foo:ver<0.3.8> is Bar;`. Without this, the adverb blocks
        // the `is Bar` parent from being parsed (parent silently dropped).
        let (r, _traits) = parse_declarator_traits(r)?;
        let (r, _) = ws(r)?;
        let mut r = r;
        let mut parents = Vec::new();
        let mut does_parents = Vec::new();
        loop {
            if let Some(r2) = keyword("is", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, parent) = if let Some(stripped) = r2.strip_prefix("::") {
                    let (r3, ident_part) = qualified_ident(stripped)?;
                    (r3, format!("::{}", ident_part))
                } else {
                    qualified_ident(r2)?
                };
                parents.push(parent);
                let (r2, _) = ws(r2)?;
                r = r2;
                continue;
            }
            if let Some(r2) = keyword("does", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, role_name) = qualified_ident(r2)?;
                parents.push(role_name.clone());
                does_parents.push(role_name);
                let (r2, _) = ws(r2)?;
                r = r2;
                continue;
            }
            break;
        }
        // Default parent is Grammar if no `is` clause
        if parents.is_empty() && name != "Grammar" {
            parents.push("Grammar".to_string());
        }
        let (r, _) = opt_char(r, ';');
        super::super::simple::register_user_type(&name);
        return Ok((
            r,
            Stmt::ClassDecl {
                name: Symbol::intern(&name),
                name_expr: None,
                parents,
                class_is_rw: false,
                is_hidden: false,
                is_lexical: false,
                hidden_parents: vec![],
                does_parents,
                repr: None,
                body: Vec::new(),
                language_version: super::super::simple::current_language_version(),
                custom_traits: Vec::new(),
                is_unit: true,
                decl_id: crate::ast::next_class_decl_id(),
            },
        ));
    }
    // Accept both `unit module Foo;` and `unit package Foo;`
    let (rest, kind) = if let Some(r) = keyword("module", rest) {
        (r, crate::ast::PackageKind::Module)
    } else if let Some(r) = keyword("package", rest) {
        (r, crate::ast::PackageKind::Package)
    } else {
        return Err(PError::expected("'module' or 'package' after 'unit'"));
    };
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    check_pseudo_package_in_decl(&name)?;
    // Consume optional type adverbs (:ver<...>, :auth<...>, :api<...>) on the
    // unit package name, e.g. `unit module Foo:ver<0.0.12>:auth<zef:bar>;`.
    let (rest, _traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::Package {
            name: Symbol::intern(&name),
            body: Vec::new(),
            kind,
            is_unit: true,
            is_my: false,
        },
    ))
}

/// Parse `package` declaration.
pub(crate) fn package_decl(input: &str) -> PResult<'_, Stmt> {
    package_decl_with_scope(input, false)
}

/// Parse `my package` declaration (lexically scoped).
pub(crate) fn package_decl_my(input: &str) -> PResult<'_, Stmt> {
    package_decl_with_scope(input, true)
}

pub(crate) fn package_decl_with_scope(input: &str, is_my: bool) -> PResult<'_, Stmt> {
    let rest = keyword("package", input).ok_or_else(|| PError::expected("package declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    check_pseudo_package_in_decl(&name)?;
    let (rest, _traits) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    Ok((
        rest,
        Stmt::Package {
            name: Symbol::intern(&name),
            body,
            kind: crate::ast::PackageKind::Package,
            is_unit: false,
            is_my,
        },
    ))
}

/// Parse `proto` declaration.
pub(crate) fn proto_decl(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("proto", input).ok_or_else(|| PError::expected("proto declaration"))?;
    let (rest, _) = ws1(rest)?;
    // proto token | proto rule | proto sub | proto method
    let _is_token = keyword("token", rest).is_some() || keyword("rule", rest).is_some();
    let is_method = keyword("method", rest).is_some() || keyword("submethod", rest).is_some();
    let rest = if let Some(r) = keyword("token", rest)
        .or_else(|| keyword("rule", rest))
        .or_else(|| keyword("sub", rest))
        .or_else(|| keyword("submethod", rest))
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
                name: Symbol::intern(&name),
                params,
                param_defs,
                body,
                is_export: traits.is_export,
                custom_traits: traits
                    .custom_traits
                    .iter()
                    .map(|(n, _)| n.clone())
                    .collect(),
                is_method,
            },
        ));
    }
    let (rest, _) = opt_char(rest, ';');
    Ok((
        rest,
        Stmt::ProtoDecl {
            name: Symbol::intern(&name),
            params,
            param_defs,
            body,
            is_export: traits.is_export,
            custom_traits: traits
                .custom_traits
                .iter()
                .map(|(n, _)| n.clone())
                .collect(),
            is_method,
        },
    ))
}
