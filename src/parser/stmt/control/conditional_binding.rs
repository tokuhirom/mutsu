use super::*;

pub(super) fn parse_if_binding_params(input: &str) -> PResult<'_, Option<Vec<ParamDef>>> {
    let Some(rest) = input.strip_prefix("->") else {
        return Ok((input, None));
    };
    let (rest, _) = ws(rest)?;
    // Zero-parameter pointy block: `if EXPR -> { ... }`
    if rest.starts_with('{') {
        return Ok((rest, Some(Vec::new())));
    }

    let (rest, params) = if let Some(rest) = rest.strip_prefix('(') {
        let (rest, _) = ws(rest)?;
        let (rest, params) = super::super::parse_param_list_pub(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        // A pointy block has no parenthesised parameter list: `-> (...)` is one
        // parameter with a destructuring sub-signature, the same shape `for` and
        // a bare `-> (...)` lambda record. Reading the parens away turned
        // `-> (:key($k))` into a top-level NAMED parameter and `-> ($a, $b)`
        // into two positionals.
        (
            rest,
            crate::parser::stmt::sub_param::fold_parenthesised_pointy_params(params),
        )
    } else {
        super::super::parse_param_list_pub(rest)?
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = if let Some(after_arrow) = rest.strip_prefix("-->") {
        let (rest, _) = super::super::parse_return_type_annotation_pub(after_arrow)?;
        let (rest, _) = ws(rest)?;
        (rest, ())
    } else {
        (rest, ())
    };
    Ok((rest, Some(params)))
}
