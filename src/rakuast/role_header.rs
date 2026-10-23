//! Written ordering of role header traits, separate from their execution fields.

use super::RakuAstField;
use super::lower::{named_child, positional_leaf};
use super::role::RoleDecl;
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::RoleHeaderTrait;
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

/// Reassemble the trait families using the parser's explicit source order.
// Cost: O(t), t = number of header traits.
pub(super) fn ordered_traits(
    role: &RoleDecl<'_>,
    traits: Vec<Value>,
    parents: usize,
) -> Result<Vec<Value>, RuntimeError> {
    let hidden = traits[..parents].iter().position(|value| {
        let ValueView::RakuAst(node) = value.view() else {
            return false;
        };
        node.class == RakuAstClass::TraitIs
            && named_child(node, "name")
                .and_then(positional_leaf)
                .is_ok_and(|name| name.to_string_value() == "hidden")
    });
    let composition: Vec<_> = (0..parents)
        .filter(|index| Some(*index) != hidden)
        .collect();
    let custom_start = parents + usize::from(role.is_rw) + usize::from(role.is_export);
    let mut slots: Vec<_> = traits.into_iter().map(Some).collect();
    let mut ordered = Vec::with_capacity(slots.len());
    for entry in role.header_order {
        let index = match entry {
            RoleHeaderTrait::Parent(index) => *composition.get(*index).ok_or_else(|| {
                super::convert::unsupported("inconsistent role parent provenance")
            })?,
            RoleHeaderTrait::Hidden => hidden.ok_or_else(|| {
                super::convert::unsupported("inconsistent hidden role provenance")
            })?,
            RoleHeaderTrait::Rw => parents,
            RoleHeaderTrait::Export => parents + usize::from(role.is_rw),
            RoleHeaderTrait::Custom(index) => custom_start + index,
        };
        let value = slots
            .get_mut(index)
            .and_then(Option::take)
            .ok_or_else(|| super::convert::unsupported("inconsistent role header provenance"))?;
        ordered.push(value);
    }
    // Normalization may lift `also is` clauses from the body. Their location
    // is not a header spelling and remains the existing normalization boundary.
    if slots.iter().any(Option::is_some) {
        return Err(super::convert::unsupported(
            "role traits absent from header provenance",
        ));
    }
    Ok(ordered)
}

/// Record the actual order of a constructed public trait list when lowering.
// Cost: O(t), t = number of header traits.
pub(super) fn lower_order(node: &RakuAstNode) -> Result<Vec<RoleHeaderTrait>, RuntimeError> {
    let Some(field) = node.fields.iter().find(|f| f.name == Some("traits")) else {
        return Ok(Vec::new());
    };
    let RakuAstFieldValue::List(items) = &field.value else {
        return Err(super::lower::unsupported(node));
    };
    let mut order = Vec::with_capacity(items.len());
    let mut parents = 0;
    let mut custom = 0;
    for value in items {
        let ValueView::RakuAst(t) = value.view() else {
            return Err(super::lower::unsupported(node));
        };
        let entry = match t.class {
            RakuAstClass::TraitDoes | RakuAstClass::TraitHides => {
                let entry = RoleHeaderTrait::Parent(parents);
                parents += 1;
                entry
            }
            RakuAstClass::TraitIs if named_child(t, "type").is_ok() => {
                let entry = RoleHeaderTrait::Parent(parents);
                parents += 1;
                entry
            }
            RakuAstClass::TraitIs => {
                let name = positional_leaf(named_child(t, "name")?)?;
                match name.to_string_value().as_str() {
                    "rw" => RoleHeaderTrait::Rw,
                    "export" => RoleHeaderTrait::Export,
                    "hidden" => RoleHeaderTrait::Hidden,
                    _ => {
                        let entry = RoleHeaderTrait::Custom(custom);
                        custom += 1;
                        entry
                    }
                }
            }
            _ => return Err(super::lower::unsupported(node)),
        };
        order.push(entry);
    }
    Ok(order)
}
