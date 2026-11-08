//! `RakuAST::Statement::For.otherwise` / `replace-otherwise`, as rakudo 2026.09
//! keeps it: a private block that `.raku` leaves out, that `new(:otherwise)`
//! stores and `replace-otherwise($block)` swaps; the `RakuAST::Block` type
//! object answers when none was given. Like `WHY` (see [`super::why_doc`]) it
//! lives in a hidden field that no walk, render or lowering reads.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

/// The hidden field holding the block.
const FIELD: &str = "otherwise";

/// Whether `field` is the hidden `otherwise` of `node`.
// Cost: O(1).
pub(super) fn is_hidden(node: &RakuAstNode, field: &RakuAstField) -> bool {
    node.class == RakuAstClass::StatementFor && field.name == Some(FIELD)
}

fn block_type() -> Value {
    Value::package(crate::symbol::Symbol::intern("RakuAST::Block"))
}

/// `otherwise`: the stored block, else the `RakuAST::Block` type object.
// Cost: O(f), f = fields of the node.
pub(crate) fn otherwise(node: &RakuAstNode) -> Value {
    node.fields
        .iter()
        .find_map(|f| match (&f.name, &f.value) {
            (Some(FIELD), RakuAstFieldValue::Node(value)) => Some(value.clone()),
            _ => None,
        })
        .unwrap_or_else(block_type)
}

/// Check that `value` is a `Block` (or a subclass, or its type object), as the
/// `RakuAST::Block $otherwise` parameter does.
// Cost: O(1).
fn check(value: &Value) -> Result<(), RuntimeError> {
    let ok = match value.view() {
        ValueView::RakuAst(node) => matches!(
            node.class,
            RakuAstClass::Block | RakuAstClass::PointyBlock
        ),
        ValueView::Package(name) => name.as_str() == "RakuAST::Block",
        _ => false,
    };
    if ok {
        return Ok(());
    }
    Err(RuntimeError::new(format!(
        "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
         '$otherwise'; expected RakuAST::Block but got {}",
        crate::value::type_name::value_type_name(value)
    )))
}

/// `replace-otherwise($block)`: keep `$block` (a type object clears it) and
/// answer `Nil`.
// Cost: O(f), f = fields of the node.
pub(crate) fn replace_otherwise(
    node: &mut RakuAstNode,
    block: &Value,
) -> Result<Value, RuntimeError> {
    if node.class != RakuAstClass::StatementFor {
        return Err(RuntimeError::new(
            "replace-otherwise is only available on RakuAST::Statement::For",
        ));
    }
    check(block)?;
    node.fields.retain(|f| f.name != Some(FIELD));
    if matches!(block.view(), ValueView::RakuAst(_)) {
        node.fields.push(RakuAstField {
            name: Some(FIELD),
            value: RakuAstFieldValue::Node(block.clone()),
        });
    }
    Ok(Value::NIL)
}
