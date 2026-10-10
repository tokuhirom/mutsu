//! Literal postconstraints in a declaring signature. The generated `where`
//! refers to the parameter's value through Term::Declaration, as Rakudo does.

use super::convert::{build_type_node, leaf_field, name_from_identifier, node_field, unsupported};
use super::lower::named_child;
use super::{RakuAstClass as C, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, SignatureVar};
use crate::value::{RuntimeError, Value, ValueView};

fn node(class: C, fields: Vec<RakuAstField>) -> RakuAstNode {
    RakuAstNode { class, fields }
}

// Cost: O(1).
fn constraint() -> RakuAstNode {
    let accepts = node(
        C::ApplyPostfix,
        vec![
            node_field(Some("operand"), node(C::TermDeclaration, vec![])),
            node_field(
                Some("postfix"),
                node(
                    C::CallMethod,
                    vec![
                        node_field(Some("name"), name_from_identifier("ACCEPTS")),
                        node_field(
                            Some("args"),
                            node(
                                C::ArgList,
                                vec![node_field(
                                    None,
                                    node(
                                        C::VarLexical,
                                        vec![leaf_field(None, Value::str_from("$_"))],
                                    ),
                                )],
                            ),
                        ),
                    ],
                ),
            ),
        ],
    );
    let boolean = node(
        C::ApplyPostfix,
        vec![
            node_field(Some("operand"), accepts),
            node_field(
                Some("postfix"),
                node(
                    C::CallMethod,
                    vec![node_field(Some("name"), name_from_identifier("Bool"))],
                ),
            ),
        ],
    );
    node(
        C::Block,
        vec![node_field(
            Some("body"),
            node(
                C::Blockoid,
                vec![node_field(
                    None,
                    node(
                        C::StatementList,
                        vec![node_field(
                            None,
                            node(
                                C::StatementExpression,
                                vec![node_field(Some("expression"), boolean)],
                            ),
                        )],
                    ),
                )],
            ),
        )],
    )
}

// Cost: O(s), s = size of the literal value.
pub(super) fn convert(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let (Expr::Literal(value) | Expr::LiteralSrc(value, _)) = expr else {
        return Err(unsupported("nonconstant signature literal postconstraint"));
    };
    let type_name = match value.view() {
        ValueView::Str(_) => "Str",
        ValueView::Int(_) | ValueView::BigInt(_) => "Int",
        ValueView::Num(_) => "Num",
        ValueView::Rat(..) => "Rat",
        _ => return Err(unsupported("signature literal postconstraint value")),
    };
    Ok(node(
        C::Parameter,
        vec![
            node_field(Some("type"), build_type_node(type_name)?),
            node_field(
                Some("target"),
                node(
                    C::ParameterTargetVar,
                    vec![leaf_field(Some("name"), Value::str_from("$"))],
                ),
            ),
            leaf_field(Some("optional"), Value::truth(false)),
            node_field(Some("where"), constraint()),
            leaf_field(Some("value"), value.clone()),
        ],
    ))
}

// Cost: O(s), s = size of the parameter tree.
pub(super) fn lower(param: &RakuAstNode) -> Result<SignatureVar, RuntimeError> {
    if let Ok(target) = named_child(param, "target")
        && (target.class != C::ParameterTargetVar || super::lower::leaf_str(target, "name")? != "$")
    {
        return Err(super::lower::unsupported(param));
    }
    if param.fields.iter().any(|field| {
        !matches!(
            field.name,
            Some("type" | "target" | "optional" | "where" | "value")
        )
    }) || param.fields.iter().any(|f| f.name == Some("optional"))
        && super::lower::bool_field(param, "optional")?
    {
        return Err(super::lower::unsupported(param));
    }
    let value = param
        .fields
        .iter()
        .find(|f| f.name == Some("value"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => Some(v),
            _ => None,
        })
        .ok_or_else(|| super::lower::unsupported(param))?;
    // Only the generated value constraint can be replaced by the common
    // signature-declaration expansion. Reject other constraints explicitly.
    if let Ok(written) = named_child(param, "where")
        && !Value::rakuast(Box::new(written.clone())).eqv(&Value::rakuast(Box::new(constraint())))
    {
        return Err(super::lower::unsupported(param));
    }
    let mut var = SignatureVar::plain("$__literal_match");
    var.literal_value = Some(Expr::Literal(value.clone()));
    if let Ok(ty) = named_child(param, "type") {
        var.per_var_type_constraint = Some(super::type_lower::type_constraint(param, ty)?);
    }
    // Validate the value through the same conversion used by the read side.
    convert(var.literal_value.as_ref().unwrap())?;
    Ok(var)
}
