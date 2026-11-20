//! Nodes of a few `Spelling` markers whose view is a small wrapper.

use super::view::{leaf_field, view_expr};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value};

/// `Statement::Empty`, the node of a bare `;`.
// Cost: O(1).
pub(super) fn statement_empty() -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementEmpty,
        fields: Vec::new(),
    }
}

/// `@a».^name`: the metamethod call the parser kept undistributed, with its
/// postfix wrapped in `MetaPostfix::Hyper` as rakudo spells the written `».`.
// Cost: O(n), n = nodes of the call.
pub(super) fn hyper_meta_method(
    names: &NameScope,
    call: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let mut node = view_expr(names, call)?;
    for field in &mut node.fields {
        if field.name == Some("postfix")
            && let RakuAstFieldValue::Node(postfix) = &field.value
        {
            let hyper = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::MetaPostfixHyper,
                fields: vec![leaf_field(None, postfix.clone())],
            };
            field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(hyper)));
        }
    }
    Ok(node)
}
