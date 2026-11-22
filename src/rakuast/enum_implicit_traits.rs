//! The roles an enum composes, as Rakudo's `Type::Enum` records them
//! (#18611): two `Trait::Does` nodes, `Enumeration` and
//! `NumericEnumeration` or `StringyEnumeration`, after the declaration's own
//! traits.
//!
//! Measured on rakudo 2026.11: `.traits` lists them after the written traits,
//! `visit-children` walks them last, unplaced, and `.raku` leaves them out.
//! The second role follows the enum's base type: the `of` type when one is
//! written, else the type of the first value (`Int` for a word list or a
//! first element without one). mutsu keeps them in a hidden field, and only
//! when the base type can be told from the source: a computed body, or a
//! first value that is not a literal, gets none (exact or absent).

use super::view::node_field;
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{EnumVariantForm, Expr};
use crate::value::{Value, ValueView};

/// The hidden field holding the implicit traits.
const FIELD: &str = "implicit-traits";

/// The implicit-traits field of an enum with base type `base` (as written,
/// if any), `variants` and `form`, or `None` when its base type is not known
/// from the source.
// Cost: O(1).
pub(super) fn field(
    base: Option<&str>,
    variants: &[(String, Option<Expr>)],
    form: &EnumVariantForm,
) -> Option<RakuAstField> {
    let numeric = match base {
        Some(base) => numeric_type(base)?,
        None if matches!(form, EnumVariantForm::Computed) => return None,
        None => match variants.first() {
            None | Some((_, None)) => true,
            Some((_, Some(value))) => numeric_value(value)?,
        },
    };
    let second = if numeric {
        "NumericEnumeration"
    } else {
        "StringyEnumeration"
    };
    let does = |role: &str| {
        Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TraitDoes,
            fields: vec![node_field(None, super::bareword::simple_type_node(role))],
        }))
    };
    Some(RakuAstField {
        name: Some(FIELD),
        value: RakuAstFieldValue::List(vec![does("Enumeration"), does(second)]),
    })
}

/// Whether the type an enum is based on is numeric (`true`) or stringy
/// (`false`); `None` for a type that is neither, or not known here.
// Cost: O(1).
fn numeric_type(name: &str) -> Option<bool> {
    match name {
        "Str" | "str" => Some(false),
        "Int" | "UInt" | "Num" | "Rat" | "FatRat" | "Real" | "Numeric" | "Bool" | "int"
        | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16" | "uint32"
        | "uint64" | "num" | "num32" | "num64" => Some(true),
        _ => None,
    }
}

/// Whether a literal first value is numeric (`true`) or a string (`false`);
/// `None` for any other expression.
// Cost: O(1).
fn numeric_value(value: &Expr) -> Option<bool> {
    let Expr::Literal(value) = value.peel_parens() else {
        return None;
    };
    match value.view() {
        ValueView::Str(_) => Some(false),
        ValueView::Int(_) | ValueView::Num(_) => Some(true),
        _ if value.is_numeric() => Some(true),
        _ => None,
    }
}

/// Whether `field` of `node` is left out of `.raku`: an enum's implicit
/// traits, as in Rakudo.
// Cost: O(1).
pub(super) fn is_hidden(node: &RakuAstNode, field: &RakuAstField) -> bool {
    node.class == RakuAstClass::TypeEnum && field.name == Some(FIELD)
}

/// `.traits` of an enum with implicit traits: the written ones, then those.
/// `None` for any other node or method.
// Cost: O(t), t = traits.
pub(super) fn accessor(node: &RakuAstNode, method: &str) -> Option<Value> {
    if node.class != RakuAstClass::TypeEnum || method != "traits" {
        return None;
    }
    let list = |name: &str| {
        node.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::List(items) if f.name == Some(name) => Some(items.clone()),
            _ => None,
        })
    };
    let implicit = list(FIELD)?;
    let mut traits = list("traits").unwrap_or_default();
    traits.extend(implicit);
    Some(Value::array(traits))
}

/// `.clean-clone` of an enum: the declaration without its implicit traits,
/// as Rakudo's (its `RakuAST::Deparse` prints the clean clone's traits).
// Cost: O(f), f = fields of `node`.
pub(super) fn clean_clone(node: &RakuAstNode) -> RakuAstNode {
    let mut clean = node.clone();
    clean.fields.retain(|f| f.name != Some(FIELD));
    clean
}
