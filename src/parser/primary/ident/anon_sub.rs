use crate::ast::Expr;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PResult, parse_char};
use crate::parser::primary::misc::parse_block_body_routine_with_params;

pub(crate) fn invocant_param_def() -> crate::ast::ParamDef {
    crate::ast::ParamDef {
        type_capture: None,
        name: "self".to_string(),
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        named_alias: false,
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
        code: Default::default(),
        trait_args: Vec::new(),
    }
}

/// Build a bodied `method { ... }` / `submethod { ... }` literal — the form
/// with no parameter list, whose only parameter is the implicit invocant.
pub(crate) fn make_anon_method(
    body: Vec<crate::ast::Stmt>,
    declarator: crate::ast::RoutineDeclarator,
) -> Expr {
    anon_method_expr(Vec::new(), None, body, declarator)
}

/// What the parser's fold of a method literal's declared invocant left in
/// the receiver and the body: its type, the name it bound the receiver to
/// (`(name, sigilless)`) and the body without that binding.
pub(crate) struct FoldedInvocant<'a> {
    pub(crate) type_constraint: Option<&'a str>,
    pub(crate) alias: Option<(String, bool)>,
    pub(crate) body: &'a [crate::ast::Stmt],
}

/// The declared invocant of a method literal (`method (Foo:D $x: ...)`), read
/// back out of its receiver `receiver` and `body`: the inverse of the fold
/// `parse_anon_method_with_params` does, through [`anon_method_expr_declared`].
/// `None` for a receiver that is not the parser's synthetic one.
// Cost: O(1) (the body is borrowed).
pub(crate) fn folded_invocant<'a>(
    receiver: &'a crate::ast::ParamDef,
    body: &'a [crate::ast::Stmt],
) -> Option<FoldedInvocant<'a>> {
    use crate::ast::Stmt;
    if !receiver.is_invocant
        || receiver.name != "self"
        || receiver.traits.len() != 1
        || receiver.traits[0] != crate::ast::IMPLICIT_INVOCANT_TRAIT
        || receiver.where_constraint.is_some()
        || receiver.default.is_some()
        || receiver.type_capture.is_some()
        || receiver.sub_signature.is_some()
        || receiver.code_signature.is_some()
    {
        return None;
    }
    let is_self = |e: &Expr| matches!(e, Expr::BareWord(n) if n == "self");
    let (alias, body) = match body.first() {
        Some(Stmt::VarDecl {
            name,
            expr,
            type_constraint: None,
            custom_traits,
            ..
        }) if is_self(expr)
            && custom_traits.len() == 1
            && custom_traits[0].0 == "__scalar_bind" =>
        {
            let name = if name == crate::env::LEX_SELF {
                "self".to_string()
            } else {
                name.clone()
            };
            (Some((name, false)), &body[1..])
        }
        Some(Stmt::SyntheticBlock(inner)) => match inner.as_slice() {
            [
                Stmt::VarDecl {
                    name,
                    expr,
                    type_constraint: None,
                    custom_traits,
                    ..
                },
                Stmt::MarkSigillessReadonly(marked),
            ] if is_self(expr) && custom_traits.is_empty() && name == marked => {
                (Some((name.clone(), true)), &body[1..])
            }
            _ => (None, body),
        },
        _ => (None, body),
    };
    Some(FoldedInvocant {
        type_constraint: receiver.type_constraint.as_deref(),
        alias,
        body,
    })
}

/// A method literal whose invocant was declared: the type that moved onto the
/// receiver, and the name the receiver is bound to in the body. The parser's
/// own fold and the RakuAST lowering share this builder.
pub(crate) fn anon_method_expr_declared(
    type_constraint: Option<String>,
    alias: Option<(String, bool)>,
    rest: Vec<crate::ast::ParamDef>,
    return_type: Option<String>,
    body: Vec<crate::ast::Stmt>,
    declarator: crate::ast::RoutineDeclarator,
) -> Expr {
    let mut expr = anon_method_expr(rest, return_type, body, declarator);
    if let Expr::AnonSubParams { param_defs, .. } = &mut expr
        && let Some(receiver) = param_defs.first_mut()
    {
        receiver.type_constraint = type_constraint;
    }
    match alias {
        Some(alias) => bind_invocant_aliases(expr, &[alias]),
        None => expr,
    }
}

/// A method literal (`method ($a) { … }`) over its written parameters `rest`:
/// the synthetic receiver comes first, because the invocant reaches the
/// closure binder as the first positional argument.
pub(crate) fn anon_method_expr(
    rest: Vec<crate::ast::ParamDef>,
    return_type: Option<String>,
    body: Vec<crate::ast::Stmt>,
    declarator: crate::ast::RoutineDeclarator,
) -> Expr {
    let mut params = vec!["self".to_string()];
    params.extend(rest.iter().map(|p| p.name.clone()));
    let mut param_defs = vec![invocant_param_def()];
    param_defs.extend(rest);
    Expr::AnonSubParams {
        params,
        param_defs,
        return_type,
        body,
        is_rw: false,
        is_raw: false,
        custom_traits: Default::default(),
        is_whatever_code: false,
        declarator,
    }
}

pub(crate) fn parse_anon_method_with_params(
    input: &str,
    declarator: crate::ast::RoutineDeclarator,
) -> PResult<'_, Expr> {
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
    // `(name, sigilless)` for every user-named invocant.
    let mut invocant_aliases: Vec<(String, bool)> = Vec::new();
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
                invocant_aliases.push((pd.name, pd.sigilless));
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
    let (r, expr) = parse_anon_sub_rest(r, params, method_param_defs, return_type, declarator)?;
    Ok((r, bind_invocant_aliases(expr, &invocant_aliases)))
}

/// Prepend `my $NAME := self;` for every user-named invocant of a method
/// literal, so `method ($x: $p) { ... }` can read the receiver as `$x` while
/// `self` keeps working (rakudo binds both).
///
/// A **sigilless** invocant (`method (\SELF: |)`, OO::Monitors' wrapper
/// idiom) is declared like `my \SELF := self`: it carries the
/// `MarkSigillessReadonly` marker that tells the compiler the bare word reads
/// the local slot. Without it `SELF` compiled to a bare-word lookup, which
/// only found the binding when the dual env store happened to be synced, so
/// the invocant read as `(Any)` in a program that loaded no module.
pub(crate) fn bind_invocant_aliases(expr: Expr, aliases: &[(String, bool)]) -> Expr {
    if aliases.is_empty() {
        return expr;
    }
    let Expr::AnonSubParams {
        params,
        param_defs,
        return_type,
        body,
        is_rw,
        is_raw,
        custom_traits,
        is_whatever_code,
        declarator,
    } = expr
    else {
        return expr;
    };
    let mut new_body: Vec<crate::ast::Stmt> = aliases
        .iter()
        .map(|(name, sigilless)| {
            if *sigilless {
                return crate::parser::stmt::control::simple_pointy_bind(
                    name,
                    &Expr::BareWord("self".to_string()),
                    true,
                );
            }
            crate::ast::Stmt::VarDecl {
                // `$self` binds the reserved lexical key, not the invocant's
                // own (ADR-0061); every other alias keeps its sigil-less name.
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
            }
        })
        .collect();
    new_body.extend(body);
    Expr::AnonSubParams {
        params,
        param_defs,
        return_type,
        body: new_body,
        is_rw,
        is_raw,
        custom_traits,
        is_whatever_code,
        declarator,
    }
}

/// The shared tail of a parenthesised anonymous routine literal: the closing
/// `)`, its traits, and its block body. `declarator` records which declarator
/// the source wrote — `sub (...)`, `method (...)`, `submethod (...)`, or none
/// of them — which selects the compile path and the closure's runtime type as
/// well as the node the RakuAST converter emits.
pub(crate) fn parse_anon_sub_rest(
    input: &str,
    params: Vec<String>,
    param_defs: Vec<crate::ast::ParamDef>,
    return_type: Option<String>,
    declarator: crate::ast::RoutineDeclarator,
) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    let (r, _) = parse_char(r, ')')?;
    let (r, _) = ws(r)?;
    let (r, traits) = crate::parser::stmt::parse_sub_traits_pub(r)?;
    // An anonymous routine can declare its return constraint as a trait after
    // the signature (`sub (..) returns Str { ... }`), just like a named sub.
    // The signature parser only sees the `-->` form inside the parentheses,
    // so preserve the trait form on the closure node as well. Otherwise the
    // runtime signature reports the default `Mu` and drops return-type checks.
    let return_type = return_type.or(traits.return_type);
    // The literal's own parameters have to be in scope for its body parse — see
    // `parse_block_body_routine_with_params`.
    let (r, body) = parse_block_body_routine_with_params(r, &param_defs)?;
    Ok((
        r,
        Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            body,
            is_rw: traits.is_rw,
            is_raw: traits.is_raw,
            custom_traits: traits.custom_traits.into(),
            is_whatever_code: false,
            declarator,
        },
    ))
}

/// Parse anonymous sub with params: sub ($x, $y) { ... }
pub(crate) fn parse_anon_sub_with_params(input: &str) -> PResult<'_, Expr> {
    let (r, _) = parse_char(input, '(')?;
    let (r, _) = ws(r)?;
    let (r, (param_defs, return_type)) = crate::parser::stmt::parse_param_list_with_return_pub(r)?;
    let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
    parse_anon_sub_rest(
        r,
        params,
        param_defs,
        return_type,
        crate::ast::RoutineDeclarator::Sub,
    )
}

pub(crate) fn set_anon_sub_rw(expr: Expr, is_rw: bool) -> Expr {
    match expr {
        Expr::AnonSub {
            body,
            is_raw,
            is_block,
            ..
        } => Expr::AnonSub {
            body,
            is_rw,
            is_raw,
            is_block,
            doc: Default::default(),
        },
        Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            body,
            is_raw,
            custom_traits,
            is_whatever_code,
            declarator,
            ..
        } => Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            is_raw,
            custom_traits,
            is_whatever_code,
            declarator,
        },
        other => other,
    }
}
