use crate::ast::Expr;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PResult, parse_char};
use crate::parser::primary::misc::parse_block_body;

pub(crate) fn invocant_param_def() -> crate::ast::ParamDef {
    crate::ast::ParamDef {
        name: "self".to_string(),
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        slurpy: false,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: true,
        shape_constraints: None,
    }
}

pub(crate) fn make_anon_method(body: Vec<crate::ast::Stmt>) -> Expr {
    Expr::AnonSubParams {
        params: vec!["self".to_string()],
        param_defs: vec![invocant_param_def()],
        return_type: None,
        body,
        is_rw: false,
        is_whatever_code: false,
    }
}

pub(crate) fn parse_anon_method_with_params(input: &str) -> PResult<'_, Expr> {
    let (r, _) = parse_char(input, '(')?;
    let (r, _) = ws(r)?;
    let (r, (param_defs, return_type)) = crate::parser::stmt::parse_param_list_with_return_pub(r)?;
    let mut params = vec!["self".to_string()];
    params.extend(param_defs.iter().map(|p| p.name.clone()));
    let mut method_param_defs = vec![invocant_param_def()];
    method_param_defs.extend(param_defs);
    parse_anon_sub_rest(r, params, method_param_defs, return_type)
}

pub(crate) fn parse_anon_sub_rest(
    input: &str,
    params: Vec<String>,
    param_defs: Vec<crate::ast::ParamDef>,
    return_type: Option<String>,
) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    let (r, _) = parse_char(r, ')')?;
    let (r, _) = ws(r)?;
    let (r, traits) = crate::parser::stmt::parse_sub_traits_pub(r)?;
    let (r, body) = parse_block_body(r)?;
    Ok((
        r,
        Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            body,
            is_rw: traits.is_rw,
            is_whatever_code: false,
        },
    ))
}

/// Parse anonymous sub with params: sub ($x, $y) { ... }
pub(crate) fn parse_anon_sub_with_params(input: &str) -> PResult<'_, Expr> {
    let (r, _) = parse_char(input, '(')?;
    let (r, _) = ws(r)?;
    let (r, (param_defs, return_type)) = crate::parser::stmt::parse_param_list_with_return_pub(r)?;
    let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
    parse_anon_sub_rest(r, params, param_defs, return_type)
}

pub(crate) fn set_anon_sub_rw(expr: Expr, is_rw: bool) -> Expr {
    match expr {
        Expr::AnonSub { body, is_block, .. } => Expr::AnonSub {
            body,
            is_rw,
            is_block,
        },
        Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            body,
            is_whatever_code,
            ..
        } => Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            is_whatever_code,
        },
        other => other,
    }
}
