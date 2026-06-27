use crate::ast::{ParamDef, Stmt, collect_placeholders_shallow};
use crate::parser::helpers::{ws, ws1};
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::symbol::Symbol;
use crate::value::Value;
use std::collections::HashMap;

pub(crate) fn mark_params_as_invocant(params: &mut [ParamDef]) {
    for param in params {
        if !param.traits.iter().any(|t| t == "invocant") {
            param.traits.push("invocant".to_string());
        }
    }
}

/// Parse `method` declaration.
pub(crate) fn method_decl(input: &str) -> PResult<'_, Stmt> {
    let (rest, multi) = if let Some(r) = super::super::keyword("multi", input) {
        let (r, _) = ws1(r)?;
        let r = super::super::keyword("method", r)
            .ok_or_else(|| PError::expected("method declaration"))?;
        let (r, _) = ws1(r)?;
        (r, true)
    } else {
        let r = super::super::keyword("method", input)
            .ok_or_else(|| PError::expected("method declaration"))?;
        let (r, _) = ws1(r)?;
        (r, false)
    };
    method_decl_body(rest, multi, false)
}

/// Parse `submethod` declaration (treated like method, not inherited by subclasses).
/// Accepts an optional `multi` declarator prefix (`multi submethod foo(...) {...}`).
pub(crate) fn submethod_decl(input: &str) -> PResult<'_, Stmt> {
    let (r, multi) = if let Some(r) = super::super::keyword("multi", input) {
        let (r, _) = ws1(r)?;
        let r = super::super::keyword("submethod", r)
            .ok_or_else(|| PError::expected("submethod declaration"))?;
        let (r, _) = ws1(r)?;
        (r, true)
    } else {
        let r = super::super::keyword("submethod", input)
            .ok_or_else(|| PError::expected("submethod declaration"))?;
        let (r, _) = ws1(r)?;
        (r, false)
    };
    method_decl_body_with_my(r, multi, false, true, true)
}

pub(crate) fn method_decl_body(input: &str, multi: bool, is_our: bool) -> PResult<'_, Stmt> {
    method_decl_body_with_my(input, multi, is_our, false, false)
}

pub(crate) fn method_decl_body_my(input: &str, multi: bool, is_our: bool) -> PResult<'_, Stmt> {
    method_decl_body_with_my(input, multi, is_our, true, false)
}

fn method_decl_body_with_my(
    input: &str,
    multi: bool,
    is_our: bool,
    is_my: bool,
    is_submethod: bool,
) -> PResult<'_, Stmt> {
    let (rest, is_private) = if let Some(rest) = input.strip_prefix('!') {
        (rest, true)
    } else {
        (input, false)
    };
    // Handle ^name metamethods (e.g., method ^foo(Mu) { ... })
    let (rest, is_meta) = if let Some(r) = rest.strip_prefix('^') {
        (r, true)
    } else {
        (rest, false)
    };
    let (rest, name, name_expr) = if rest.starts_with("::") {
        let (rest, (name, expr)) = super::super::sub::parse_indirect_decl_name(rest)?;
        let name = if is_meta { format!("^{}", name) } else { name };
        (rest, name, Some(expr))
    } else {
        let (rest, name) = super::super::sub::parse_sub_name(rest)?;
        let name = if is_meta { format!("^{}", name) } else { name };
        (rest, name, None)
    };
    let (rest, _) = ws(rest)?;

    let (rest, (params, param_defs, param_return_type)) = if rest.starts_with('(') {
        let (r, _) = parse_char(rest, '(')?;
        let (r, _) = ws(r)?;
        let (r, (pd, rt)) = super::super::sub::parse_param_list_with_return(r)?;
        super::super::sub::validate_signature_params(&pd)?;
        // In BUILD/TWEAK submethods, reject $.x (public accessor twigil) parameters
        // as X::Syntax::VirtualCall — the object is not fully constructed yet.
        if name == "BUILD" || name == "TWEAK" {
            for p in &pd {
                if p.name.starts_with('.') {
                    let call = format!("$.{}", &p.name[1..]);
                    let msg = format!(
                        "X::Syntax::VirtualCall: Virtual method call {} may not be used on partially constructed object",
                        call
                    );
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    attrs.insert("call".to_string(), Value::str(call));
                    let ex = Value::make_instance(Symbol::intern("X::Syntax::VirtualCall"), attrs);
                    return Err(PError::fatal_with_exception(msg, Box::new(ex)));
                }
            }
        }
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        let names: Vec<String> = pd.iter().map(|p| p.name.clone()).collect();
        (r, (names, pd, rt))
    } else {
        (rest, (Vec::new(), Vec::new(), None))
    };

    let (rest, traits) = super::super::sub::parse_sub_traits(rest)?;
    let return_type = traits.return_type.or(param_return_type);
    let (rest, body) = if param_defs.iter().any(|p| p.sigilless) {
        // When there are sigilless params, register them as term symbols in the
        // block scope so bare references resolve to the parameter rather than to
        // a builtin/keyword of the same name (e.g. `method m(\times) { times }`).
        let (r, _) = parse_char(rest, '{')?;
        super::super::simple::push_scope();
        for pd in &param_defs {
            if pd.sigilless {
                super::super::simple::register_user_term_symbol(&pd.name);
            }
        }
        let result = super::super::block_inner(r);
        super::super::simple::pop_scope();
        result?
    } else {
        super::super::block(rest)?
    };
    // When no explicit signature is given, collect placeholder variables
    // (@_, $^a, $^b, etc.) from the body as implicit parameters.
    let (params, param_defs) = if params.is_empty() && param_defs.is_empty() {
        let placeholders = collect_placeholders_shallow(&body);
        if placeholders.is_empty() {
            (params, param_defs)
        } else {
            (placeholders, Vec::new())
        }
    } else {
        (params, param_defs)
    };
    Ok((
        rest,
        Stmt::MethodDecl {
            name: Symbol::intern(&name),
            name_expr,
            params,
            param_defs,
            body,
            multi,
            is_rw: traits.is_rw,
            is_private,
            is_our,
            is_my,
            is_submethod,
            our_variable_form: false,
            return_type,
            is_default_candidate: traits.custom_traits.iter().any(|(t, _)| t == "default"),
            deprecated_message: traits.custom_traits.iter().find_map(|(t, _)| {
                if t == "DEPRECATED" {
                    Some(String::new())
                } else {
                    t.strip_prefix("DEPRECATED:").map(|msg| msg.to_string())
                }
            }),
            handles: traits.handles.clone(),
            custom_traits: traits
                .custom_traits
                .iter()
                .filter(|(t, _)| {
                    t != "default"
                        && t != "DEPRECATED"
                        && !t.starts_with("DEPRECATED:")
                        && !t.starts_with("__")
                })
                .cloned()
                .collect(),
            is_export: traits.is_export,
            export_tags: traits.export_tags.clone(),
        },
    ))
}
