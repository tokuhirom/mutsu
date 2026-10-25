//! Written ordering of role header traits, separate from their execution fields.

use super::RakuAstField;
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

const DECLARATION_NAME: &str = "role-declaration-name";

/// `RakuAST::Statement::Also.new(traits => (...))`, or `None` for any other
/// class.
// Cost: O(a), a = number of arguments.
pub(super) fn construct(
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if (class_name, method) != ("RakuAST::Statement::Also", "new") {
        return None;
    }
    let mut traits = Vec::new();
    for arg in args {
        match arg.view() {
            ValueView::Pair(key, value) if key.as_str() == "traits" => {
                traits = match value.view() {
                    ValueView::RakuAst(_) => vec![value.clone()],
                    _ => match value.as_list_items() {
                        Some(items) => items.to_vec(),
                        None => {
                            return Some(Err(RuntimeError::new(
                                "RakuAST::Statement::Also.new expects `traits` to be a list",
                            )));
                        }
                    },
                };
            }
            _ => {
                return Some(Err(RuntimeError::new(
                    "RakuAST::Statement::Also.new takes only `traits`",
                )));
            }
        }
    }
    Some(Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementAlso,
        fields: vec![RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(traits),
        }],
    }))))
}

/// An anonymous source declaration already owns a globally unique name.
// Cost: O(k), k = name length.
pub(super) fn retain_name(fields: &mut Vec<super::RakuAstField>, name: crate::symbol::Symbol) {
    if crate::value::is_internal_anon_type_name(name.as_str()) {
        fields.push(super::convert::leaf_field(
            Some(DECLARATION_NAME),
            Value::str_from(name.as_str()),
        ));
    }
}

// Cost: O(1).
pub(super) fn is_source_field(field: &super::RakuAstField) -> bool {
    field.name == Some(DECLARATION_NAME)
}

/// Constructed nodes have no source name and still allocate a fresh one.
// Cost: O(f + k), f = field count, k = name length.
pub(super) fn retained_name(node: &RakuAstNode) -> Result<Option<String>, RuntimeError> {
    if !node.fields.iter().any(is_source_field) {
        return Ok(None);
    }
    let name = super::lower::leaf_str(node, DECLARATION_NAME)?;
    if !crate::value::is_internal_anon_type_name(&name) {
        return Err(super::lower::unsupported(node));
    }
    Ok(Some(name))
}
