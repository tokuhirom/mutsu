//! Anonymous positional and slurpy destructuring parameters.

use super::convert::{
    ANONYMOUS_ARRAY_SUBSIGNATURE, ANONYMOUS_SUBSIGNATURE, build_type_node, leaf_field, node_field,
    signature, type_setting_any, unsupported,
};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::ParamDef;
use crate::value::{RuntimeError, Value};

/// Whether `name` is the parser's name for an anonymous destructuring
/// parameter, and if so whether it was the `[…]` form: `@` / `__subsig__` in
/// a signature, `__for_unpack[_array][_N]` in a `for` loop's.
fn anonymous_destructuring_form(name: &str) -> Option<bool> {
    use crate::parser::{FOR_UNPACK, FOR_UNPACK_ARRAY};
    let numbered = |base: &str| {
        name == base
            || name
                .strip_prefix(base)
                .and_then(|rest| rest.strip_prefix('_'))
                .is_some_and(|n| !n.is_empty() && n.bytes().all(|b| b.is_ascii_digit()))
    };
    match name {
        ANONYMOUS_ARRAY_SUBSIGNATURE => Some(true),
        ANONYMOUS_SUBSIGNATURE => Some(false),
        _ if numbered(FOR_UNPACK_ARRAY) => Some(true),
        _ if numbered(FOR_UNPACK) => Some(false),
        _ => None,
    }
}

/// An anonymous destructuring parameter, `[$a, $b]` or `($a, $b)`: rakudo's
/// `Parameter` with no target, holding the `sub-signature`; the bracket form
/// marks it `is-array`, and the parenthesised one carries the implicit `Any`
/// type where a sub signature does, and a written type (`Pair (…)`) is the
/// parameter's (measured on rakudo 2026.09). `None` for any other parameter;
/// one with a default, `where` or trait is refused.
// Cost: O(s), s = size of the sub-signature.
pub(super) fn anonymous_destructuring(
    pd: &ParamDef,
    type_setting: bool,
) -> Result<Option<RakuAstNode>, RuntimeError> {
    let Some(is_array) = anonymous_destructuring_form(&pd.name) else {
        return Ok(None);
    };
    let Some(sub_params) = pd.sub_signature.as_deref() else {
        return Ok(None);
    };
    // A capture `| (…)` and the like are not this form.
    if pd.named || pd.sigilless {
        return Ok(None);
    }
    if pd.optional_marker
        || pd.default.is_some()
        || pd.type_capture.is_some()
        || pd.where_constraint.is_some()
        || !pd.traits.is_empty()
    {
        return Err(unsupported(
            "anonymous sub-signature with a default, `where` or trait",
        ));
    }
    let mut sub = signature(sub_params, type_setting, None)?;
    if is_array && !pd.onearg && !pd.slurpy {
        sub.fields
            .push(leaf_field(Some("is-array"), Value::truth(true)));
    }
    let mut fields = Vec::with_capacity(3);
    // `Pair (…)` / `Positional […]` carry the written type.
    match pd.type_constraint.as_deref() {
        Some(t) => fields.push(node_field(Some("type"), build_type_node(t)?)),
        None if type_setting && !is_array => {
            fields.push(node_field(Some("type"), type_setting_any()));
        }
        None => {}
    }
    if pd.onearg || pd.slurpy {
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
    fields.push(node_field(Some("sub-signature"), sub));
    Ok(Some(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields,
    }))
}
