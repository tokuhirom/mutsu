use crate::ast::{Expr, ParamDef};
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::value::Value;
use std::collections::HashMap;

/// Helper to construct a default ParamDef with only required fields.
pub(crate) fn make_param(name: String) -> ParamDef {
    ParamDef {
        name,
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
        is_invocant: false,
        shape_constraints: None,
        block_param: false,
    }
}

/// Build an `X::Parameter::AfterDefault` error for a trait or post-constraint
/// that appears after the parameter's default value (e.g. `$x = 60 is rw` or
/// `$x = 60 where Int`). `kind` is `"trait"` or `"post constraint"`; `modifier`
/// is the offending source fragment (`is rw`, `where Int`); `default` is the
/// default value source (`60`).
pub(crate) fn after_default_error(kind: &str, modifier: &str, default: &str) -> PError {
    let msg = format!(
        "The {kind} '{modifier}' came after the default value.  Did you mean: ...{modifier} = {default} ?"
    );
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("type".to_string(), Value::str(kind.to_string()));
    attrs.insert("modifier".to_string(), Value::str(modifier.to_string()));
    attrs.insert("default".to_string(), Value::str(default.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Parameter::AfterDefault"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

pub(crate) fn parse_param_default_expr(input: &str) -> PResult<'_, Expr> {
    if let Ok((rest, expr)) = crate::parser::expr::expression(input) {
        return Ok((rest, expr));
    }
    if let Some(after_my) = input.strip_prefix("my ")
        && let Ok((rest, expr)) = crate::parser::expr::expression(after_my)
    {
        return Ok((rest, expr));
    }
    Err(PError::expected("parameter default expression"))
}

pub(crate) fn is_anonymous_sigil_param(param: &ParamDef) -> bool {
    matches!(
        param.name.as_str(),
        "__ANON_STATE__" | "__ANON_ARRAY__" | "__ANON_HASH__" | "__ANON_CODE__"
    )
}

pub(crate) fn starts_with_sigil_param(input: &str) -> bool {
    matches!(input.as_bytes().first(), Some(b'$' | b'@' | b'%' | b'&'))
}

/// The optional tail a parameter may carry after a destructuring sub-signature:
/// `is` traits, a `where` post-constraint, and a default value.
pub(crate) struct SubsigTail {
    pub(crate) traits: Vec<String>,
    pub(crate) where_constraint: Option<Box<Expr>>,
    pub(crate) default: Option<Expr>,
}

/// Parse whatever follows a parameter's sub-signature.
///
/// A parameter may carry a sub-signature *and* a post-constraint at once —
/// `sub f($x ($a, $b) where { ... })`, `:$x! (*@a) where { ... }` — with the
/// `where` testing the parameter's own value while the sub-signature unpacks it.
/// Each sub-signature branch in `param_inner` used to stop after its `is` trait
/// loop, so the `where` was left unconsumed and the signature failed to parse at
/// the closing paren. Sharing one tail parser keeps the four branches (bare
/// `(...)`, `$x (...)`, `&cb (...)`, `@a [...]`) in step.
pub(crate) fn parse_subsig_tail(input: &str) -> PResult<'_, SubsigTail> {
    use crate::parser::helpers::{ws, ws1};

    let (mut rest, _) = ws(input)?;
    let mut traits = Vec::new();
    while let Some(r) = super::super::keyword("is", rest) {
        let (r, _) = ws1(r)?;
        let (r, trait_name) = super::super::ident(r)?;
        let (r, _) = super::super::sub::validate_param_trait(&trait_name, &traits, r)?;
        traits.push(trait_name);
        let (r, _) = ws(r)?;
        rest = r;
    }
    let (rest, where_constraint) = if let Some(r) = super::super::keyword("where", rest) {
        let (r, _) = ws1(r)?;
        let (r, constraint) = super::where_constraint::parse_where_constraint_expr(r)?;
        (r, Some(Box::new(constraint)))
    } else {
        (rest, None)
    };
    let (rest, _) = ws(rest)?;
    let (rest, default) = if rest.starts_with('=') && !rest.starts_with("==") {
        let (r, _) = ws(&rest[1..])?;
        let (r, expr) = crate::parser::expr::expression(r)?;
        (r, Some(expr))
    } else {
        (rest, None)
    };
    Ok((
        rest,
        SubsigTail {
            traits,
            where_constraint,
            default,
        },
    ))
}

/// Returns (rest, required, optional_marker).
/// `!` → required=true, optional_marker=false
/// `?` → required=false, optional_marker=true
/// neither → required=false, optional_marker=false
pub(crate) fn parse_required_suffix(input: &str) -> (&str, bool, bool) {
    if let Some(rest) = input.strip_prefix('!') {
        (rest, true, false)
    } else if let Some(rest) = input.strip_prefix('?') {
        (rest, false, true)
    } else {
        (input, false, false)
    }
}
