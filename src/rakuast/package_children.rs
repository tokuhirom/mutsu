//! Rakudo's `visit-children` shape of a package declaration (#15780,
//! measured on rakudo 2026.11): a package visits its name and its body, and
//! the body visits the package's traits before its `Blockoid`. A role's
//! `RoleBody` visits the role's name, its parameterization (an empty
//! `Signature` when none is written) and its traits first.
//!
//! mutsu keeps the traits (and a role's name and parameterization) on the
//! package node, as `.raku` shows them, so the body handed to the visitor is
//! a copy that carries them in front of its own fields.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// `node` as `visit-children` walks it, when it is a package declaration:
/// its traits (and a role's name and parameterization) moved into a copy of
/// its body. `None` for any other node, or a package without a body.
// Cost: O(f + t), f = fields of the package, t = its traits.
pub(crate) fn reshaped(node: &RakuAstNode) -> Option<RakuAstNode> {
    use RakuAstClass as C;
    let role = node.class == C::Role;
    if !matches!(
        node.class,
        C::Class | C::Grammar | C::Module | C::Package | C::Role
    ) {
        return None;
    }
    let body_at = node.fields.iter().position(|f| f.name == Some("body"))?;
    let RakuAstFieldValue::Node(body) = &node.fields[body_at].value else {
        return None;
    };
    let ValueView::RakuAst(body) = body.view() else {
        return None;
    };
    let mut front: Vec<RakuAstField> = Vec::new();
    // A body built with its own name and signature already visits them.
    let own = |name: &str| body.fields.iter().any(|f| f.name == Some(name));
    if role && !own("name") && !own("signature") {
        if let Some(name) = node.fields.iter().find(|f| f.name == Some("name")) {
            front.push(name.clone());
        }
        let signature = match node
            .fields
            .iter()
            .find(|f| f.name == Some("parameterization"))
        {
            Some(field) => field.value.clone(),
            None => RakuAstFieldValue::Node(super::empty_fields::empty_node(
                node,
                "parameterization",
                C::Signature,
            )),
        };
        front.push(RakuAstField {
            name: Some("parameterization"),
            value: signature,
        });
    }
    if let Some(traits) = node.fields.iter().find(|f| f.name == Some("traits")) {
        front.push(traits.clone());
    }
    if front.is_empty() {
        return None;
    }
    let mut body = RakuAstNode::clone(body);
    front.append(&mut body.fields);
    body.fields = front;
    let mut out = node.clone();
    out.fields[body_at].value = RakuAstFieldValue::Node(Value::rakuast(Box::new(body)));
    out.fields.retain(|f| {
        !matches!(f.name, Some("traits")) && !(role && f.name == Some("parameterization"))
    });
    Some(out)
}
