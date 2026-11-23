//! The `RakuAST::Type::*` introspection methods raku-doc documents and
//! rakudo 2026.11 answers (#18978): `is-native`, `is-coercive`,
//! `Definedness.name` and `Capture.definite`.

use super::{RakuAstClass, RakuAstNode, node_accessor};
use crate::value::{Value, ValueView};

/// The identifier a `Type::Simple` / `Type::Setting` names, if it is one.
// Cost: O(f), f = fields of the node.
fn type_identifier(node: &RakuAstNode) -> Option<String> {
    super::resolve::type_node_name(node)
}

/// The answer to `method` on the type node `node`, `None` for any other node
/// or method.
// Cost: O(f), f = fields of the node and of its base type.
pub(crate) fn call(node: &RakuAstNode, method: &str) -> Option<Value> {
    use RakuAstClass::*;
    match (node.class, method) {
        // `int`, `uint8`, `num64`, `str`: a native type.
        (TypeSimple | TypeSetting, "is-native") => Some(Value::truth(
            type_identifier(node)
                .is_some_and(|name| crate::native_types::is_native_array_element_type(&name)),
        )),
        // Only a coercion type coerces.
        (
            TypeSimple | TypeSetting | TypeDefinedness | TypeParameterized | TypeSubset
            | TypeCoercion | TypeCapture,
            "is-coercive",
        ) => Some(Value::truth(node.class == TypeCoercion)),
        // `Int:D`: the base type's name with the smiley.
        (TypeDefinedness, "name") => {
            let base = super::infix_origins::child(node, "base-type")?;
            let name = type_identifier(base)?;
            let definite = node_accessor(node, "definite").is_some_and(|v| v.truthy());
            let smiley = if definite { "D" } else { "U" };
            Some(Value::rakuast(Box::new(super::view::name_from_identifier(
                &format!("{name}:{smiley}"),
            ))))
        }
        // `::T:D` is definite, `::T:U` is not, and a capture without a smiley
        // answers the `Bool` type object.
        (TypeCapture, "definite") => {
            let smiley = node_accessor(node, "smiley");
            Some(match smiley.as_ref().map(|v| v.view()) {
                Some(ValueView::Str(s)) if s.as_str() == "D" => Value::truth(true),
                Some(ValueView::Str(s)) if s.as_str() == "U" => Value::truth(false),
                _ => Value::package(crate::symbol::Symbol::intern("Bool")),
            })
        }
        _ => None,
    }
}
