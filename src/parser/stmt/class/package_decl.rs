use super::*;

use crate::ast::Stmt;
use crate::symbol::Symbol;
use crate::value::Value;

use crate::parser::helpers::{parse_trait_angle_arg, skip_balanced_parens, ws, ws1};
use crate::parser::parse_result::{PError, PResult, opt_char, parse_char};
use crate::parser::primary::var::is_pseudo_package;
use crate::parser::stmt::sub::parse_sub_name;
use crate::parser::stmt::{
    block, keyword, package_body_block, parse_param_list_with_return_pub, parse_sub_traits,
    qualified_ident,
};

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

/// The `__MUTSU_SET_META__` calls for a declarator's `:ver`/`:auth`/`:api`
/// adverbs. Other adverbs are not metadata and are dropped here, as they are in
/// the block forms.
fn decl_adverb_meta_stmts(name: &str, adverbs: Vec<(String, crate::ast::Expr)>) -> Vec<Stmt> {
    adverbs
        .into_iter()
        .filter(|(k, _)| matches!(k.as_str(), "ver" | "auth" | "api"))
        .map(|(k, v)| super::class_decl::meta_setter_stmt(name, &k, v))
        .collect()
}

/// Pair a `unit class`/`unit role`/`unit grammar` declaration with the metadata
/// setters its declarator adverbs produced. A `SyntheticBlock` is a non-lexical
/// statement sequence, so the declaration keeps its compilation-unit scope; the
/// declaration is always its *last* element, which is what `stmt_list` relies on
/// when it absorbs the rest of the file into the declaration's body. Returns the
/// bare declaration when there are no adverbs, so the overwhelmingly common
/// shape is untouched.
fn with_meta_stmts(mut meta_stmts: Vec<Stmt>, decl: Stmt) -> Stmt {
    if meta_stmts.is_empty() {
        return decl;
    }
    meta_stmts.push(decl);
    Stmt::SyntheticBlock(meta_stmts)
}

/// Parse `unit module` or `unit class` statement.
pub(crate) fn unit_module_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unit", input).ok_or_else(|| PError::expected("unit statement"))?;
    let (rest, _) = ws1(rest)?;
    // unit class Name;
    // unit class Name is Parent does Role;
    //
    // A declarator registered through a `use`d module's `EXPORTHOW::DECLARE`
    // block (`monitor`, from the bundled `OO::Monitors`) is a package
    // declarator peer to `class` and accepts the same file-scope form —
    // `Terminal::ANSI::Virtual.rakumod` is written as
    // `unit monitor Terminal::ANSI::Virtual;`. It parses exactly like
    // `unit class` and only differs by the `__mutsu_declare_how` marker trait
    // that tells registration which HOW to attach, so both share this arm.
    let class_kw = keyword("class", rest).map(|r| (r, None));
    let declare_kw = || {
        super::super::simple::declare_keyword_names()
            .into_iter()
            // Role-kind slang declarators belong to the `unit role` arm below.
            .filter(|kw| !super::super::simple::declare_keyword_is_role(kw))
            .find_map(|kw| keyword(&kw, rest).map(|r| (r, Some(kw))))
    };
    if let Some((r, declare_how)) = class_kw.or_else(declare_kw) {
        let (r, _) = ws1(r)?;
        let (r, name) = qualified_ident(r)?;
        check_pseudo_package_in_decl(&name)?;
        // Optional type adverbs (:ver<...>, :auth<...>, :api<...>)
        let (r, traits) = parse_declarator_traits(r)?;
        let (r, _) = ws(r)?;
        let meta_stmts = decl_adverb_meta_stmts(&name, traits);
        // Parse `is Parent` and `does Role` clauses before the semicolon
        let mut parents = Vec::new();
        let mut does_parents = Vec::new();
        let mut class_is_rw = false;
        let mut is_hidden = false;
        let mut hidden_parents = Vec::new();
        let mut parent_args: Vec<(String, Vec<crate::ast::Expr>)> = Vec::new();
        let mut is_repr: Option<String> = None;
        let mut custom_traits: Vec<(String, Option<crate::ast::Expr>)> = Vec::new();
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
                    let (r2, _) = ws(r2)?;
                    r = r2;
                    continue;
                } else if parent == "hidden" {
                    is_hidden = true;
                    let (r2, _) = ws(r2)?;
                    r = r2;
                    continue;
                } else if parent == "repr" {
                    // `unit class Foo is repr('CStruct');` / `is repr<CStruct>;`
                    if r2.starts_with('<') {
                        let (r3, repr_val) = parse_trait_angle_arg(r2)?;
                        is_repr = Some(repr_val);
                        let (r3, _) = ws(r3)?;
                        r = r3;
                        continue;
                    }
                    if let Some(inner) = r2.strip_prefix('(') {
                        let end = inner.find(')').unwrap_or(inner.len());
                        let repr_val = inner[..end].trim().trim_matches('\'').trim_matches('"');
                        is_repr = Some(repr_val.to_string());
                    }
                    let r2 = skip_balanced_parens(r2);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                    continue;
                } else if parent.starts_with(|c: char| c.is_ascii_uppercase())
                    || parent.starts_with("::")
                {
                    // Uppercase / indirect name is a superclass (possibly
                    // parametric, e.g. `is Foo[Int]`).
                    let (r2, bracket_suffix) = parse_optional_bracket_suffix(r2)?;
                    let full_name = format!("{}{}", parent, bracket_suffix);
                    if let Some(exprs) = super::class_decl::parse_bracket_arg_exprs(bracket_suffix)
                    {
                        parent_args.push((full_name.clone(), exprs));
                    }
                    parents.push(full_name);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                    continue;
                }
                // A lowercase `is` name on a `unit class` is a trait
                // (`export`, `DEPRECATED`, `ctype<...>`, a custom trait_mod,
                // ...), NOT a parent class. Record name + angle/paren
                // argument (if any) as a custom trait rather than mis-recording
                // it as a superclass.
                if r2.starts_with('<') {
                    let (r3, arg) = parse_trait_angle_arg(r2)?;
                    custom_traits.push((
                        parent.clone(),
                        Some(crate::ast::Expr::Literal(Value::str(arg))),
                    ));
                    let (r3, _) = ws(r3)?;
                    r = r3;
                    continue;
                }
                // `unit class A::B::C is export;` publishes the short name `C`
                // to the importer; carry the marker the module loader reads.
                if parent == "export" && name.contains("::") {
                    let mut tags = Vec::new();
                    super::class_decl::push_export_tags(r2, &mut tags);
                    custom_traits.push(super::class_decl::export_type_marker(&tags));
                }
                let r2 = skip_balanced_parens(r2);
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
                if let Some(exprs) = super::class_decl::parse_bracket_arg_exprs(bracket_suffix) {
                    parent_args.push((full_name.clone(), exprs));
                }
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
                let full_name = format!("{}{}", parent, bracket_suffix);
                if let Some(exprs) = super::class_decl::parse_bracket_arg_exprs(bracket_suffix) {
                    parent_args.push((full_name.clone(), exprs));
                }
                parents.push(full_name.clone());
                hidden_parents.push(full_name);
                let (r2, _) = ws(r2)?;
                r = r2;
                continue;
            }
            break;
        }
        let (r, _) = opt_char(r, ';');
        if let Some(kw) = declare_how {
            custom_traits.push((
                "__mutsu_declare_how".to_string(),
                Some(crate::ast::Expr::Literal(Value::str(kw))),
            ));
        }
        // `stmt_list` parses the rest of a `unit class` as this declaration's
        // body. Register the type before returning so body methods can use the
        // fully-qualified name in a `when` matcher.
        super::super::simple::register_user_type(&name);
        return Ok((
            r,
            with_meta_stmts(
                meta_stmts,
                Stmt::ClassDecl {
                    name: Symbol::intern(&name),
                    name_expr: None,
                    parents,
                    class_is_rw,
                    is_hidden,
                    is_lexical: false,
                    hidden_parents,
                    does_parents,
                    repr: is_repr,
                    body: Vec::new(),
                    language_version: super::super::simple::current_language_version(),
                    custom_traits,
                    is_unit: true,
                    implicit_grammar_parent: false,
                    is_grammar: false,
                    decl_id: crate::ast::next_class_decl_id(),
                    parent_args,
                    body_parents: Vec::new(),
                },
            ),
        ));
    }
    // unit role Name;  — declare a role at the file scope.
    // unit role Name is export does OtherRole;  — traits and composition too.
    //
    // A slang declarator whose `$*PKGDECL` is `role` (ADR-0091) is a peer of
    // `role` and takes the same file-scope form: Test::Async's own bundles are
    // written `unit test-bundle Test::Async::Base;`. It differs only by the
    // `__mutsu_declare_how` marker trait naming the keyword, so both share
    // this arm.
    let role_kw = keyword("role", rest)
        .map(|r| (r, "role".to_string()))
        .or_else(|| {
            super::super::simple::declare_keyword_names()
                .into_iter()
                .filter(|kw| super::super::simple::declare_keyword_is_role(kw))
                .find_map(|kw| keyword(&kw, rest).map(|r| (r, kw)))
        });
    if let Some((r, role_kw)) = role_kw {
        let (r, _) = ws1(r)?;
        let (r, name) = qualified_ident(r)?;
        check_pseudo_package_in_decl(&name)?;
        let (mut r, (mut type_params, mut type_param_defs)) =
            super::role_decl::parse_optional_role_type_params(r)?;
        // Optional type adverbs (:ver<...>, :auth<...>, :api<...>).
        let (r2, adverbs) = parse_declarator_traits(r)?;
        let (r2, _) = ws(r2)?;
        let meta_stmts = decl_adverb_meta_stmts(&name, adverbs);
        r = r2;
        // The signature may also FOLLOW the adverbs — the order Raku itself uses:
        // `unit role Algorithm::Treap:ver<0.10.3>:auth<zef:titsuki>[::KeyT];`.
        if type_params.is_empty() && r.starts_with('[') {
            let (r2, (tp, tpd)) = super::role_decl::parse_optional_role_type_params(r)?;
            let (r2, _) = ws(r2)?;
            type_params = tp;
            type_param_defs = tpd;
            r = r2;
        }
        // Optional parent/trait clauses in any order (mirrors block-form roles).
        // (name, bracket args, came-from-`is`) — see the block-form role
        // parser for why the declarator has to survive into the body (#8100).
        let mut parent_roles: Vec<(String, Option<Vec<crate::ast::Expr>>, bool)> = Vec::new();
        let mut is_export = false;
        let mut export_tags: Vec<String> = Vec::new();
        let mut role_is_rw = false;
        let mut custom_traits: Vec<(String, Option<crate::ast::Expr>)> = Vec::new();
        loop {
            if let Some(r2) = keyword("does", r) {
                let (r2, _) = ws1(r2)?;
                let (r2, role_name) = qualified_ident(r2)?;
                let (r2, _) = ws(r2)?;
                let (r2, bracket_suffix) = parse_optional_bracket_suffix(r2)?;
                let (r2, _) = ws(r2)?;
                let args = super::class_decl::parse_bracket_arg_exprs(bracket_suffix);
                parent_roles.push((format!("{}{}", role_name, bracket_suffix), args, false));
                r = r2;
                continue;
            }
            if let Some(r2) = keyword("is", r) {
                let (r2, _) = ws1(r2)?;
                // A unit role can inherit from a qualified type, for example
                // Red's `unit role MetamodelX::Red::SubModelHOW is
                // Metamodel::SubsetHOW`.  Parse the complete parent name so
                // the `::SubsetHOW` suffix is not left as a stray term.
                let (r2, trait_name) = qualified_ident(r2)?;
                if trait_name == "rw" {
                    role_is_rw = true;
                    let r2 = skip_balanced_parens(r2);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                } else if trait_name == "export" {
                    is_export = true;
                    super::class_decl::push_export_tags(r2, &mut export_tags);
                    let r2 = skip_balanced_parens(r2);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                } else if r2.starts_with('<') {
                    // `is repr<CStruct>` / `is ctype<long>` — angle-bracket
                    // trait argument (no dedicated field on RoleDecl, so it
                    // is recorded via custom_traits like an unrecognized
                    // `is` trait with a parenthesized argument).
                    let (r3, arg) = parse_trait_angle_arg(r2)?;
                    custom_traits
                        .push((trait_name, Some(crate::ast::Expr::Literal(Value::str(arg)))));
                    let (r3, _) = ws(r3)?;
                    r = r3;
                } else {
                    // Unknown lowercase trait: skip any parenthesized argument.
                    // An uppercase bare name would be a parent role, but a
                    // `unit role` with parents almost always spells them with
                    // `does`, so treat everything else as a skipped trait_mod.
                    let has_parens = r2.starts_with('(');
                    let r2 = skip_balanced_parens(r2);
                    if !has_parens && trait_name.starts_with(|c: char| c.is_ascii_uppercase()) {
                        parent_roles.push((trait_name, None, true));
                    } else {
                        custom_traits.push((trait_name, None));
                    }
                    let (r2, _) = ws(r2)?;
                    r = r2;
                }
                continue;
            }
            break;
        }
        let (r, _) = opt_char(r, ';');
        // `stmt_list` parses the rest of a `unit role` as this declaration's
        // body, so make the declaration visible before that parsing starts.
        super::super::simple::register_user_type(&name);
        let mut body: Vec<Stmt> = Vec::new();
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
        if role_kw != "role" {
            custom_traits.push((
                "__mutsu_declare_how".to_string(),
                Some(crate::ast::Expr::Literal(Value::str(role_kw))),
            ));
        }
        return Ok((
            r,
            with_meta_stmts(
                meta_stmts,
                Stmt::RoleDecl {
                    name: Symbol::intern(&name),
                    type_params,
                    type_param_defs,
                    is_export,
                    export_tags,
                    body,
                    is_rw: role_is_rw,
                    language_version: super::super::simple::current_language_version(),
                    custom_traits,
                    decl_id: crate::ast::next_class_decl_id(),
                },
            ),
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
        let (r, traits) = parse_declarator_traits(r)?;
        let (r, _) = ws(r)?;
        let meta_stmts = decl_adverb_meta_stmts(&name, traits);
        let mut r = r;
        let mut parents = Vec::new();
        let mut does_parents = Vec::new();
        let mut is_parent_count = 0usize;
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
                is_parent_count += 1;
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
        // Default parent is Grammar if no `is` clause. A module-local
        // `grammar Grammar` (qualified to `Mod::Grammar`) must still inherit the
        // built-in Grammar; only a genuine top-level `grammar Grammar` that IS
        // the built-in would self-parent, and that is dropped at registration
        // (see the self-parent filter in `exec_register_class_op`). A composed
        // role is not an `is` parent, so `unit grammar G does R;` must still
        // inherit Grammar.
        let implicit_grammar_parent = is_parent_count == 0;
        if implicit_grammar_parent {
            parents.push("Grammar".to_string());
        }
        let (r, _) = opt_char(r, ';');
        // A `unit grammar` also captures the remainder of the compilation unit
        // as its body; register its name before that body is parsed.
        super::super::simple::register_user_type(&name);
        return Ok((
            r,
            with_meta_stmts(
                meta_stmts,
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
                    implicit_grammar_parent,
                    is_grammar: true,
                    decl_id: crate::ast::next_class_decl_id(),
                    parent_args: Vec::new(),
                    body_parents: Vec::new(),
                },
            ),
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
    // Consume bareword traits such as `is export` / `is rw` / custom
    // `is Foo(...)` before the terminating semicolon, e.g.
    // `unit module App::Racoco::ConfigFile is export;`.
    let (rest, export_tags) = parse_package_is_traits(rest)?;
    let (rest, _) = opt_char(rest, ';');
    let package = Stmt::Package {
        name: Symbol::intern(&name),
        body: Vec::new(),
        kind,
        is_unit: true,
        is_my: false,
    };
    // `unit module A::B::C is export;` publishes the short name `C` (the
    // package itself), as rakudo does for a qualified declarator name.
    if let Some(tags) = export_tags
        && name.contains("::")
    {
        return Ok((
            rest,
            Stmt::SyntheticBlock(vec![
                package,
                super::class_decl::export_type_stmt(&name, &tags),
            ]),
        ));
    }
    Ok((rest, package))
}

/// Consume a package declarator's bareword traits (`is export`, `is rw`, a
/// custom `is Foo(...)`), returning the `is export` tags when present.
pub(crate) fn parse_package_is_traits(input: &str) -> PResult<'_, Option<Vec<String>>> {
    let mut rest = input;
    let mut export_tags: Option<Vec<String>> = None;
    while let Some(r) = keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = crate::parser::stmt::ident(r)?;
        if trait_name == "export" {
            let mut tags = Vec::new();
            super::class_decl::push_export_tags(r, &mut tags);
            export_tags = Some(tags);
        }
        let r = skip_balanced_parens(r);
        let (r, _) = ws(r)?;
        rest = r;
    }
    Ok((rest, export_tags))
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
    let (rest, body) = {
        let _pkg = super::super::simple::push_package_path(&name);
        package_body_block(rest)?
    };
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
    proto_decl_scoped(input, false)
}

/// `proto` declaration, `is_our` set for the `our proto ...` spelling.
pub(crate) fn proto_decl_scoped(input: &str, is_our: bool) -> PResult<'_, Stmt> {
    let rest = keyword("proto", input).ok_or_else(|| PError::expected("proto declaration"))?;
    let (rest, _) = ws1(rest)?;
    // proto token | proto rule | proto regex | proto sub | proto method
    // `proto token` / `proto rule` / `proto regex` all declare a proto REGEX
    // (an LTM dispatcher over `name:sym<...>` candidates), never a proto sub.
    // `rule`/`regex` used to fall through to `ProtoDecl`, which registered a
    // package-level proto sub, so a second instantiation of a role carrying
    // one (a pun, then a composition) died with X::Redeclaration (#9337).
    let is_regex_proto = keyword("token", rest).is_some()
        || keyword("rule", rest).is_some()
        || keyword("regex", rest).is_some();
    let is_method = keyword("method", rest).is_some() || keyword("submethod", rest).is_some();
    let rest = if let Some(r) = keyword("token", rest)
        .or_else(|| keyword("rule", rest))
        .or_else(|| keyword("regex", rest))
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
    if !is_regex_proto && !is_method {
        // A lone `proto sub infix:<op>` (no `multi` candidates in this
        // file) still declares the operator for the rest of the scope.
        super::super::simple::register_user_sub(&name);
    }
    let (rest, _) = ws(rest)?;
    let (rest, (param_defs, return_type)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, (pd, return_type)) = parse_param_list_with_return_pub(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        (r, (pd, return_type))
    } else {
        (rest, (Vec::new(), None))
    };
    let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
    let (rest, _) = ws(rest)?;
    // Parse traits (is export, etc.)
    let (rest, traits) = parse_sub_traits(rest)?;
    // A `proto sub infix:<precedes>(...) {*}` declares the operator for the
    // rest of the scope, exactly as an `only`/`multi` sub would: `$a precedes
    // $b` must parse even before (or without) any candidate (#10516).
    if !is_method && !is_regex_proto {
        crate::parser::stmt::simple::register_user_sub(&name);
        crate::parser::stmt::simple::register_user_callable_term_symbol(&name);
        crate::parser::stmt::sub::register_parse_affecting_traits(&name, &traits);
    }
    let (rest, _) = ws(rest)?;
    // May have body or just semicolon
    let mut body = Vec::new();
    if rest.starts_with('{') {
        // `{*}` is the proto's dispatcher, not a statement list, to rakudo.
        let parsed = if crate::parser::stmt::trace::is_dispatcher_body(rest) {
            crate::parser::stmt::trace::unnumbered(|| block(rest))
        } else {
            block(rest)
        };
        let (rest, parsed_body) = match parsed {
            Ok(ok) => ok,
            Err(_) => consume_raw_braced_body(rest)?,
        };
        body = parsed_body;
        if is_regex_proto {
            return Ok((
                rest,
                Stmt::ProtoToken {
                    name: Symbol::intern(&name),
                    param_defs,
                },
            ));
        }
        return Ok((
            rest,
            Stmt::ProtoDecl {
                name: Symbol::intern(&name),
                params,
                param_defs,
                return_type,
                body,
                is_export: traits.is_export,
                export_tags: traits.export_tags.clone(),
                custom_traits: traits
                    .custom_traits
                    .iter()
                    .map(|(n, _)| n.clone())
                    .collect(),
                trait_args: traits.custom_traits.clone(),
                is_method,
                is_our,
            },
        ));
    }
    let (rest, _) = opt_char(rest, ';');
    if is_regex_proto {
        return Ok((
            rest,
            Stmt::ProtoToken {
                name: Symbol::intern(&name),
                param_defs,
            },
        ));
    }
    Ok((
        rest,
        Stmt::ProtoDecl {
            name: Symbol::intern(&name),
            params,
            param_defs,
            return_type,
            body,
            is_export: traits.is_export,
            export_tags: traits.export_tags.clone(),
            custom_traits: traits
                .custom_traits
                .iter()
                .map(|(n, _)| n.clone())
                .collect(),
            trait_args: traits.custom_traits.clone(),
            is_method,
            is_our,
        },
    ))
}

#[cfg(test)]
mod unit_repr_tests {
    use super::*;

    // `unit class`/`unit role` extend to the end of the file, so their
    // angle-bracket trait parsing (`is repr<...>`, `is ctype<...>`) is
    // exercised here directly rather than via a t/*.t script — see
    // t/is-repr-angle-bracket-trait.t for the block-form (`class`/`role`)
    // coverage of the same underlying `parse_trait_angle_arg` mechanism.

    #[test]
    fn unit_class_accepts_repr_angle_trait() {
        let (_, stmt) = unit_module_stmt("unit class Foo is repr<CStruct>;").unwrap();
        let Stmt::ClassDecl { repr, .. } = stmt else {
            panic!("expected ClassDecl, got {stmt:?}");
        };
        assert_eq!(repr.as_deref(), Some("CStruct"));
    }

    #[test]
    fn unit_class_accepts_ctype_angle_trait_as_custom_trait() {
        let (_, stmt) = unit_module_stmt("unit class Foo is ctype<long>;").unwrap();
        let Stmt::ClassDecl { custom_traits, .. } = stmt else {
            panic!("expected ClassDecl, got {stmt:?}");
        };
        assert!(
            custom_traits.iter().any(|(name, _)| name == "ctype"),
            "expected a 'ctype' custom trait, got {custom_traits:?}"
        );
    }

    #[test]
    fn unit_role_accepts_ctype_angle_trait_as_custom_trait() {
        let (_, stmt) = unit_module_stmt("unit role Foo is ctype<long>;").unwrap();
        let Stmt::RoleDecl { custom_traits, .. } = stmt else {
            panic!("expected RoleDecl, got {stmt:?}");
        };
        assert!(
            custom_traits.iter().any(|(name, _)| name == "ctype"),
            "expected a 'ctype' custom trait, got {custom_traits:?}"
        );
    }
}
