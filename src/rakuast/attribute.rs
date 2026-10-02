//! Attribute traits (`has $.x is rw = 5`) across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, an attribute is a
//! `VarDeclaration::Simple(scope => "has", …)` whose `traits` list holds one
//! `Trait::Is(name => Name.from-identifier("rw"))` per written trait, followed
//! by the implicit `Trait::WillBuild(EXPR)` an `= EXPR` default adds. The
//! default is also the `initializer`, and the gist omits the implicit
//! `WillBuild` (only `.traits` shows it).
//!
//! The parser records `is rw` / `is readonly` / `is required` as flags on
//! `Stmt::HasDecl`, not in source order, so an attribute with more than one of
//! them is refused rather than rendered in an invented order. `is default(…)`
//! (whose value the parser also copies into the initializer) and `is built`
//! (whose argument it does not keep) are refused by the caller.

use super::convert::{name_from_identifier, node_field, unsupported};
use super::lower::{named_child, positional_leaf};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

/// The flag-valued attribute traits, as `HasDecl` records them.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(super) struct AttributeTraits {
    pub(super) is_rw: bool,
    pub(super) is_readonly: bool,
    pub(super) is_required: bool,
}

impl AttributeTraits {
    fn names(self) -> impl Iterator<Item = &'static str> {
        [
            (self.is_rw, "rw"),
            (self.is_readonly, "readonly"),
            (self.is_required, "required"),
        ]
        .into_iter()
        .filter_map(|(on, name)| on.then_some(name))
    }
}

/// Put the attribute's written traits at the front of `decl`'s `traits` list,
/// ahead of an implicit `WillBuild`, creating the field (after `desigilname`)
/// when the declaration has no default.
// Cost: O(f), f = fields of `decl`.
pub(super) fn add_traits(
    decl: &mut RakuAstNode,
    traits: AttributeTraits,
) -> Result<(), RuntimeError> {
    let written: Vec<&str> = traits.names().collect();
    if written.len() > 1 {
        return Err(unsupported(
            "attribute with several traits (their source order is not kept)",
        ));
    }
    let mut items: Vec<Value> = written
        .into_iter()
        .map(|name| {
            Value::rakuast(Box::new(RakuAstNode {
                class: RakuAstClass::TraitIs,
                fields: vec![node_field(Some("name"), name_from_identifier(name))],
            }))
        })
        .collect();
    if items.is_empty() {
        return Ok(());
    }
    if let Some(field) = decl.fields.iter_mut().find(|f| f.name == Some("traits")) {
        let RakuAstFieldValue::List(existing) = &mut field.value else {
            return Err(unsupported("attribute traits field"));
        };
        items.append(existing);
        *existing = items;
        return Ok(());
    }
    let at = decl
        .fields
        .iter()
        .position(|f| f.name == Some("desigilname"))
        .map_or(decl.fields.len(), |i| i + 1);
    decl.fields.insert(
        at,
        RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(items),
        },
    );
    Ok(())
}

/// The traits of an attribute `VarDeclaration::Simple`, back as `HasDecl`
/// flags. An implicit `WillBuild` is accepted only alongside an
/// `initializer`, which carries the same default.
// Cost: O(t), t = traits of the declaration.
pub(super) fn lower_traits(node: &RakuAstNode) -> Result<AttributeTraits, RuntimeError> {
    let refuse = || super::lower::unsupported(node);
    let mut traits = AttributeTraits::default();
    let Some(field) = node.fields.iter().find(|f| f.name == Some("traits")) else {
        return Ok(traits);
    };
    let RakuAstFieldValue::List(items) = &field.value else {
        return Err(refuse());
    };
    let has_initializer = node.fields.iter().any(|f| f.name == Some("initializer"));
    for item in items {
        let ValueView::RakuAst(t) = item.view() else {
            return Err(refuse());
        };
        match t.class {
            RakuAstClass::TraitWillBuild if has_initializer => {}
            RakuAstClass::TraitIs if t.fields.len() == 1 => {
                let name = positional_leaf(named_child(t, "name")?)?;
                let flag = match name.view() {
                    ValueView::Str(s) if s.as_str() == "rw" => &mut traits.is_rw,
                    ValueView::Str(s) if s.as_str() == "readonly" => &mut traits.is_readonly,
                    ValueView::Str(s) if s.as_str() == "required" => &mut traits.is_required,
                    _ => return Err(refuse()),
                };
                *flag = true;
            }
            _ => return Err(refuse()),
        }
    }
    Ok(traits)
}

/// The gist of an attribute omits the implicit `WillBuild` its default adds
/// (`.traits` still answers it): `decl` without it, or `None` when there is
/// none to hide.
// Cost: O(f + t), f = fields of `decl`, t = its traits.
pub(super) fn without_implicit_traits(decl: &RakuAstNode) -> Option<RakuAstNode> {
    let is_will_build = |v: &Value| matches!(v.view(), ValueView::RakuAst(t) if t.class == RakuAstClass::TraitWillBuild);
    let traits = decl.fields.iter().find(|f| f.name == Some("traits"))?;
    let RakuAstFieldValue::List(items) = &traits.value else {
        return None;
    };
    if !items.iter().any(is_will_build) {
        return None;
    }
    let mut shown = decl.clone();
    shown.fields.retain_mut(|f| {
        if f.name != Some("traits") {
            return true;
        }
        if let RakuAstFieldValue::List(items) = &mut f.value {
            items.retain(|v| !is_will_build(v));
            !items.is_empty()
        } else {
            true
        }
    });
    Some(shown)
}
