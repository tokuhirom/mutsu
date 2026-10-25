//! Executable and written `is` traits of modules and packages.

use super::convert::unsupported;
use super::lower::{list_field, named_child};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(a + t), a = constructor arguments, t = traits.
pub(super) fn construct(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let class = match name {
        "RakuAST::Module" => RakuAstClass::Module,
        "RakuAST::Package" => RakuAstClass::Package,
        _ => return None,
    };
    Some((|| {
        let mut fields = Vec::new();
        if let Some(scope) = super::named_arg(args, "scope") {
            if !matches!(scope.view(), ValueView::Str(_)) {
                return Err(RuntimeError::new(format!("{name}.new expects a Str scope")));
            }
            if !scope.to_string_value().is_empty() {
                fields.push(RakuAstField {
                    name: Some("scope"),
                    value: RakuAstFieldValue::Node(scope),
                });
            }
        }
        if let Some(value) = super::named_arg(args, "name") {
            super::require_rakuast_class(&value, RakuAstClass::Name, name)?;
            fields.push(RakuAstField {
                name: Some("name"),
                value: RakuAstFieldValue::Node(value),
            });
        }
        if let Some(value) = super::named_arg(args, "traits") {
            let items = value
                .as_list_items()
                .ok_or_else(|| RuntimeError::new(format!("{name}.new expects a traits list")))?;
            let mut traits = Vec::new();
            traits
                .try_reserve(items.len())
                .map_err(|_| RuntimeError::new("RakuAST: package trait list is too large"))?;
            for item in items {
                let item = item.with_deref(|value| value.descalarize().clone());
                super::require_any_rakuast(&item, name, "traits")?;
                traits.push(item);
            }
            if !traits.is_empty() {
                fields.push(RakuAstField {
                    name: Some("traits"),
                    value: RakuAstFieldValue::List(traits),
                });
            }
        }
        let body = super::named_arg(args, "body").unwrap_or_else(|| {
            Value::rakuast(Box::new(
                super::convert::block_node(&[]).expect("empty block is supported"),
            ))
        });
        super::require_rakuast_class(&body, RakuAstClass::Block, name)?;
        fields.push(RakuAstField {
            name: Some("body"),
            value: RakuAstFieldValue::Node(body),
        });
        Ok(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class,
            fields,
        })))
    })())
}

pub(super) fn apply(
    node: &mut RakuAstNode,
    traits: &[(String, Option<Expr>)],
    body: &[Stmt],
) -> Result<(), RuntimeError> {
    if super::routine_source::apply(node, body)? || traits.is_empty() {
        return Ok(());
    }
    // The traits are the ones the source wrote, in written order; the custom
    // traits the parser folded them into are not read back.
    Err(unsupported("package traits without a written-trait record"))
}

pub(super) fn has_export(node: &RakuAstNode) -> bool {
    list_field(node, "traits").unwrap_or(&[]).iter().any(|item| {
        let ValueView::RakuAst(trait_node) = item.view() else { return false };
        if trait_node.class != RakuAstClass::TraitIs { return false; }
        named_child(trait_node, "name").ok().is_some_and(|name| {
            matches!(super::name_parts::name_shape(name), Some(super::name_parts::NameShape::Identifier(name)) if name == "export")
        })
    })
}

pub(super) fn lower(node: &RakuAstNode) -> Result<Vec<(String, Option<Expr>)>, RuntimeError> {
    if !node.fields.iter().any(|field| field.name == Some("traits")) {
        return Ok(Vec::new());
    }
    let mut result = Vec::new();
    for item in list_field(node, "traits")? {
        let ValueView::RakuAst(item) = item.view() else {
            return Err(super::lower::unsupported(node));
        };
        if item.class != RakuAstClass::TraitIs {
            return Err(unsupported("non-is package trait"));
        }
        let name = if let Ok(ty) = named_child(item, "type") {
            super::type_lower::type_constraint(node, ty)?
        } else {
            let name = named_child(item, "name")?;
            let Some(super::name_parts::NameShape::Identifier(name)) =
                super::name_parts::name_shape(name)
            else {
                return Err(super::lower::unsupported(node));
            };
            name.to_string()
        };
        let argument = named_child(item, "argument")
            .ok()
            .map(|argument| {
                if argument.class == RakuAstClass::QuotedString {
                    super::lower::lower_expr(argument)
                } else {
                    super::attribute::lower_paren_argument(node, argument)
                }
            })
            .transpose()?;
        if !matches!(name.as_str(), "export" | "DEPRECATED") {
            result.push((name, argument));
        }
    }
    Ok(result)
}
