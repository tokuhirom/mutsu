//! Anonymous positional and slurpy destructuring parameters.

use super::convert::{
    build_type_node, leaf_field, node_field, signature, type_setting_any, unsupported,
};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::{ParamDef, ParamTarget};
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value};

/// An anonymous destructuring parameter, `[$a, $b]` or `($a, $b)`: rakudo's
/// `Parameter` with no target, holding the `sub-signature`; the bracket form
/// marks it `is-array`, and the parenthesised one carries the implicit `Any`
/// type where a sub signature does, and a written type (`Pair (…)`) is the
/// parameter's (measured on rakudo 2026.09). Defaults, where constraints and
/// traits remain fields on the outer parameter.
// Cost: O(s), s = size of the sub-signature.
pub(super) fn anonymous_destructuring(
    names: &NameScope,
    pd: &ParamDef,
    type_setting: bool,
) -> Result<Option<RakuAstNode>, RuntimeError> {
    let is_array = match pd.target {
        ParamTarget::Brackets => true,
        ParamTarget::Parens => false,
        _ => return Ok(None),
    };
    let Some(sub_params) = pd.sub_signature.as_deref() else {
        return Ok(None);
    };
    // A capture `| (…)` and the like are not this form.
    if pd.named || pd.sigilless {
        return Ok(None);
    }
    if pd.optional_marker || pd.type_capture.is_some() {
        return Err(unsupported(
            "anonymous sub-signature with an optional marker or type capture",
        ));
    }
    let mut sub = signature(names, sub_params, type_setting, None)?;
    if is_array && !pd.onearg && !pd.slurpy {
        sub.fields
            .push(leaf_field(Some("is-array"), Value::truth(true)));
    }
    let mut fields = Vec::with_capacity(6);
    // `Pair (…)` / `Positional […]` carry the written type.
    match pd.type_constraint.as_deref() {
        Some(t) => fields.push(node_field(Some("type"), build_type_node(names, t)?)),
        None if type_setting && !is_array => {
            fields.push(node_field(Some("type"), type_setting_any()));
        }
        None => {}
    }
    if let Some(default) = &pd.default {
        fields.push(node_field(
            Some("default"),
            super::convert::convert_expr(names, default)?,
        ));
    } else if pd.onearg || pd.slurpy {
        let marker = if pd.onearg {
            RakuAstClass::ParameterSlurpySingleArgument
        } else if pd.double_slurpy {
            RakuAstClass::ParameterSlurpyUnflattened
        } else {
            RakuAstClass::ParameterSlurpyFlattened
        };
        fields.push(leaf_field(
            Some("slurpy"),
            super::slurpy_marker_value(marker),
        ));
    } else {
        fields.push(leaf_field(Some("optional"), Value::truth(false)));
    }
    if let Some(where_constraint) = &pd.where_constraint {
        fields.push(node_field(
            Some("where"),
            super::convert::convert_expr(names, where_constraint)?,
        ));
    }
    if let Some(traits) = super::convert::parameter_traits(names, pd)? {
        fields.push(traits);
    }
    fields.push(node_field(Some("sub-signature"), sub));
    Ok(Some(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Parameter,
        fields,
    }))
}
