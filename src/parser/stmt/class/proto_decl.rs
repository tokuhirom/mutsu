use super::consume_raw_braced_body;
use crate::ast::Stmt;
use crate::parser::helpers::{ws, ws1};
use crate::parser::parse_result::{PError, PResult, opt_char, parse_char};
use crate::parser::stmt::sub::parse_sub_name;
use crate::parser::stmt::{block, keyword, parse_param_list_with_return_pub, parse_sub_traits};
use crate::symbol::Symbol;

pub(crate) fn proto_decl(input: &str) -> PResult<'_, Stmt> {
    proto_decl_scoped(input, false, false)
}

/// `proto` declaration, `is_my` / `is_our` set for the `my proto ...` /
/// `our proto ...` spellings.
pub(crate) fn proto_decl_scoped(input: &str, is_my: bool, is_our: bool) -> PResult<'_, Stmt> {
    let rest = keyword("proto", input).ok_or_else(|| PError::expected("proto declaration"))?;
    let (rest, _) = ws1(rest)?;
    // proto token | proto rule | proto regex | proto sub | proto method
    // `proto token` / `proto rule` / `proto regex` all declare a proto REGEX
    // (an LTM dispatcher over `name:sym<...>` candidates), never a proto sub.
    // `rule`/`regex` used to fall through to `ProtoDecl`, which registered a
    // package-level proto sub, so a second instantiation of a role carrying
    // one (a pun, then a composition) died with X::Redeclaration (#9337).
    let regex_kind = if keyword("rule", rest).is_some() {
        crate::regex_tree::RegexDeclKind::Rule
    } else if keyword("regex", rest).is_some() {
        crate::regex_tree::RegexDeclKind::Regex
    } else {
        crate::regex_tree::RegexDeclKind::Token
    };
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
    let return_type = return_type.or_else(|| traits.return_type.clone());
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
            Err(error) if error.is_fatal() => return Err(error),
            Err(_) => consume_raw_braced_body(rest)?,
        };
        crate::parser::stmt::statement_boundary::require_proto_body_boundary(rest)?;
        body = parsed_body;
        if is_regex_proto {
            return Ok((
                rest,
                Stmt::ProtoToken {
                    name: Symbol::intern(&name),
                    param_defs,
                    regex_kind,
                    is_my,
                    is_our,
                },
            ));
        }
        crate::ast::routine_trait::attach(&mut body, traits.source_traits);
        return Ok((
            rest,
            Stmt::ProtoDecl {
                is_hidden_from_backtrace: traits.is_hidden_from_backtrace,
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
                regex_kind,
                is_my,
                is_our,
            },
        ));
    }
    crate::ast::routine_trait::attach(&mut body, traits.source_traits);
    Ok((
        rest,
        Stmt::ProtoDecl {
            is_hidden_from_backtrace: traits.is_hidden_from_backtrace,
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
