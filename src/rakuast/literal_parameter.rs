//! Literal-value parameters shared by routine, pointy and signature forms.

use super::convert::{build_type_node, leaf_field, node_field, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::ParamDef;
use crate::value::{RuntimeError, Value, ValueView};

/// The parser's name for a literal-value parameter.
pub(super) const NAME: &str = "__literal__";

/// The type Rakudo records beside a scalar literal parameter value. Mutsu
/// stores large rational literals in more than one representation, but all of
/// them are Raku `Rat` values at this boundary.
// Cost: O(1).
pub(super) fn value_type(value: &Value) -> Option<&'static str> {
    match value.view() {
        ValueView::Int(_) | ValueView::BigInt(_) => Some("Int"),
        ValueView::Num(_) => Some("Num"),
        ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => Some("Rat"),
        ValueView::Complex(..) => Some("Complex"),
        ValueView::Str(_) => Some("Str"),
        _ => None,
    }
}

/// `sub f(<1+2i>)` and other scalar literal parameters have no target, and
/// carry the literal's Raku type and value.
// Cost: O(1).
pub(super) fn convert(pd: &ParamDef, value: &Value) -> Result<RakuAstNode, RuntimeError> {
    if pd.name != NAME {
        return Err(unsupported("literal-value parameter of another kind"));
    }
    let inferred =
        value_type(value).ok_or_else(|| unsupported("literal-value parameter of another kind"))?;
    let type_name = pd.type_constraint.as_deref().unwrap_or(inferred);
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Parameter,
        fields: vec![
            node_field(Some("type"), build_type_node(type_name)?),
            RakuAstField {
                name: Some("optional"),
                value: RakuAstFieldValue::Node(Value::truth(false)),
            },
            leaf_field(Some("value"), value.clone()),
        ],
    })
}
