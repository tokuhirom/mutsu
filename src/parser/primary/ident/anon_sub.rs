use crate::ast::Expr;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PResult, parse_char};
use crate::parser::primary::misc::parse_block_body_routine;

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
        traits: vec![crate::ast::IMPLICIT_INVOCANT_TRAIT.to_string()],
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: true,
        shape_constraints: None,
        block_param: false,
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
        is_sub: false,
    }
}

pub(crate) fn parse_anon_method_with_params(input: &str) -> PResult<'_, Expr> {
    let (r, _) = parse_char(input, '(')?;
    let (r, _) = ws(r)?;
    let (r, (param_defs, return_type)) = crate::parser::stmt::parse_param_list_with_return_pub(r)?;
    // A method literal carries its receiver in a leading synthetic `self`
    // parameter, because the invocant reaches the closure binder as the first
    // positional argument. An *explicitly declared* invocant (`method ($x: $p)`,
    // `method (List:D:)`) names that same receiver -- it is NOT an extra
    // positional. Keeping both in the list made the signature one parameter too
    // long ("Too few positionals passed; expected 3 arguments but got 2"), so
    // fold the declaration into the single `self` parameter: its type/`where`
    // constraint moves onto `self` (so `method (List:D:)` still type-checks the
    // invocant), and a user-chosen name is bound to `self` in the body.
    let mut invocant = invocant_param_def();
    let mut invocant_aliases: Vec<String> = Vec::new();
    let mut rest_params: Vec<crate::ast::ParamDef> = Vec::new();
    let mut seen_positional = false;
    for pd in param_defs {
        let declares_invocant =
            pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") || pd.name == "self";
        let declares_self_lexical = pd.declares_self_lexical();
        if !seen_positional && declares_invocant {
            if pd.type_constraint.is_some() {
                invocant.type_constraint = pd.type_constraint;
            }
            if pd.where_constraint.is_some() {
                invocant.where_constraint = pd.where_constraint;
            }
            // A user-written `$self:` is aliased like any other named invocant:
            // it declares the `$self` *lexical*, which no longer shares the
            // invocant's env key (ADR-0061). A parser-synthesized anonymous
            // invocant (`method (Foo:D:)`) declares nothing and is skipped.
            if !pd.name.is_empty() && (pd.name != "self" || declares_self_lexical) {
                invocant_aliases.push(pd.name);
            }
            continue;
        }
        seen_positional = true;
        rest_params.push(pd);
    }
    let mut params = vec!["self".to_string()];
    params.extend(rest_params.iter().map(|p| p.name.clone()));
    let mut method_param_defs = vec![invocant];
    method_param_defs.extend(rest_params);
    let (r, expr) = parse_anon_sub_rest(r, params, method_param_defs, return_type, false)?;
    Ok((r, bind_invocant_aliases(expr, &invocant_aliases)))
}

/// Prepend `my $NAME := self;` for every user-named invocant of a method
/// literal, so `method ($x: $p) { ... }` can read the receiver as `$x` while
/// `self` keeps working (rakudo binds both).
fn bind_invocant_aliases(expr: Expr, aliases: &[String]) -> Expr {
    if aliases.is_empty() {
        return expr;
    }
    let Expr::AnonSubParams {
        params,
        param_defs,
        return_type,
        body,
        is_rw,
        is_whatever_code,
        is_sub,
    } = expr
    else {
        return expr;
    };
    let mut new_body: Vec<crate::ast::Stmt> = aliases
        .iter()
        .map(|name| crate::ast::Stmt::VarDecl {
            // `$self` binds the reserved lexical key, not the invocant's own
            // (ADR-0061); every other alias keeps its sigil-less name.
            name: if name == "self" {
                crate::env::LEX_SELF.to_string()
            } else {
                name.clone()
            },
            expr: Expr::BareWord("self".to_string()),
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: vec![("__scalar_bind".to_string(), None)],
            where_constraint: None,
        })
        .collect();
    new_body.extend(body);
    Expr::AnonSubParams {
        params,
        param_defs,
        return_type,
        body: new_body,
        is_rw,
        is_whatever_code,
        is_sub,
    }
}

/// The shared tail of a parenthesised anonymous routine literal: the closing
/// `)`, its traits, and its block body. `is_sub` records which declarator the
/// source wrote — `sub (...)` sets it, a `method (...)` literal does not — so
/// the RakuAST converter can tell `RakuAST::Sub` from `RakuAST::PointyBlock`
/// without guessing. It carries no execution meaning.
pub(crate) fn parse_anon_sub_rest(
    input: &str,
    params: Vec<String>,
    param_defs: Vec<crate::ast::ParamDef>,
    return_type: Option<String>,
    is_sub: bool,
) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    let (r, _) = parse_char(r, ')')?;
    let (r, _) = ws(r)?;
    let (r, traits) = crate::parser::stmt::parse_sub_traits_pub(r)?;
    let (r, body) = parse_block_body_routine(r)?;
    Ok((
        r,
        Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            body,
            is_rw: traits.is_rw,
            is_whatever_code: false,
            is_sub,
        },
    ))
}

/// Parse anonymous sub with params: sub ($x, $y) { ... }
pub(crate) fn parse_anon_sub_with_params(input: &str) -> PResult<'_, Expr> {
    let (r, _) = parse_char(input, '(')?;
    let (r, _) = ws(r)?;
    let (r, (param_defs, return_type)) = crate::parser::stmt::parse_param_list_with_return_pub(r)?;
    let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
    parse_anon_sub_rest(r, params, param_defs, return_type, true)
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
            is_sub,
            ..
        } => Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            is_whatever_code,
            is_sub,
        },
        other => other,
    }
}
