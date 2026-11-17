//! `nqp::getattr` / `nqp::bindattr` on a RakuAST node's attribute (#17327).
//!
//! A CHECK-time pass that rewrites the unit's tree (Red's `red-compile`) binds
//! a node's attribute directly: `nqp::bindattr(nqp::decont($apply),
//! RakuAST::ApplyDottyInfix, '$!left', $new)`. A node keeps its attributes as
//! model fields, so the attribute `$!left` is the model field `left`: reading it
//! answers the field, binding it replaces the field in place, where every holder
//! of the node sees it (as a rakudo attribute bind does).
//!
//! Only an attribute that names a field of the node, a declared model field of
//! its class or a named parameter of its `new` is answered here
//! (`ApplyDottyInfix`'s `$!left`, `Literal`'s `$!value`). An attribute rakudo
//! keeps under another shape than mutsu's fields (`ApplyInfix`'s `$!args`,
//! `Var::Lexical`'s `$!desigilname`) is left to the generic attribute path.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, fields};
use crate::value::{Value, ValueView};

/// The model field name an attribute name spells: `$!left` is `left`.
// Cost: O(1).
fn attribute_key(attr: &str) -> Option<&str> {
    attr.strip_prefix(['$', '@', '%', '&'])?.strip_prefix('!')
}

/// The declared model field of `class` named `key`, with its rakudo order.
// Cost: O(f), f = model fields of the class.
fn model_field(class: RakuAstClass, key: &str) -> Option<(usize, &'static str, fields::Absent)> {
    fields::model_fields(class)
        .iter()
        .enumerate()
        .find(|(_, (name, _))| *name == key)
        .map(|(index, (name, absent))| (index, *name, *absent))
}

/// Whether `key` is the class's positional payload (`IntLiteral`'s `value`)
/// and `node` holds it as its first, unnamed field.
// Cost: O(1).
fn is_positional_payload(node: &RakuAstNode, key: &str) -> bool {
    fields::positional_accessor(node.class) == Some(key)
        && node.fields.first().is_some_and(|f| f.name.is_none())
        && !node.fields.iter().any(|f| f.name == Some(key))
}

/// `nqp::getattr` of the attribute `attr` on `node`: its model field, or `None`
/// when the attribute is not one of the class's model fields.
// Cost: O(f + c), f = fields of the node, c = child values copied.
pub(crate) fn get_attribute(node: &RakuAstNode, attr: &str) -> Option<Value> {
    let key = attribute_key(attr)?;
    let declared = node.fields.iter().any(|f| f.name == Some(key))
        || model_field(node.class, key).is_some()
        || constructor_name(node.class, key).is_some()
        || is_positional_payload(node, key);
    if !declared {
        return None;
    }
    super::node_accessor(node, key)
}

/// The named parameter `key` of the class's `new`, which initializes the
/// attribute of that name.
// Cost: O(n), n = named parameters of the class.
fn constructor_name(class: RakuAstClass, key: &str) -> Option<&'static str> {
    super::ctor_names::declared_name(class.printed_name(), key)
}

/// `nqp::bindattr` of the attribute `attr` on `node` to `value`, in place:
/// `false` when the attribute is not one of the class's model fields.
// Cost: O(f + e), f = fields of the node, e = elements of a list value.
pub(crate) fn bind_attribute(node: &mut RakuAstNode, attr: &str, value: Value) -> bool {
    let Some(key) = attribute_key(attr) else {
        return false;
    };
    if is_positional_payload(node, key) {
        node.fields[0].value = RakuAstFieldValue::Node(value);
        return true;
    }
    if let Some(slot) = node.fields.iter_mut().find(|f| f.name == Some(key)) {
        let as_list = matches!(slot.value, RakuAstFieldValue::List(_));
        slot.value = field_value(value, as_list);
        return true;
    }
    let Some((index, name, absent)) = model_field(node.class, key) else {
        // A named parameter of `new` the node does not hold yet.
        let Some(name) = constructor_name(node.class, key) else {
            return false;
        };
        node.fields.push(RakuAstField {
            name: Some(name),
            value: field_value(value, false),
        });
        return true;
    };
    let field = RakuAstField {
        name: Some(name),
        value: field_value(value, absent == fields::Absent::EmptyList),
    };
    // Keep rakudo's declaration order: before the first held field declared
    // after this one.
    let at = node
        .fields
        .iter()
        .position(|f| {
            f.name
                .and_then(|other| model_field(node.class, other))
                .is_some_and(|(other, _, _)| other > index)
        })
        .unwrap_or(node.fields.len());
    node.fields.insert(at, field);
    true
}

/// A bound value as a field: a list field holds the elements of a list value.
// Cost: O(e), e = elements of a list value.
fn field_value(value: Value, as_list: bool) -> RakuAstFieldValue {
    if as_list && let ValueView::Array(items, _) = value.view() {
        return RakuAstFieldValue::List(items.as_slice().to_vec());
    }
    RakuAstFieldValue::Node(value)
}
