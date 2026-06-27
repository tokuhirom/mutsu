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
