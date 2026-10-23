//! `my $x will begin { ... }` across the RakuAST boundary: a
//! `VarDeclaration::Simple` with `Trait::Will(phase, block)` entries, which
//! the parser spells as the declaration followed by one phaser per trait
//! ([`crate::ast::var_will`]).

use super::convert::{
    convert_stmt, expression_of, leaf_field, node_field, signature_block_node,
    statement_expression, unsupported,
};
use super::lower::{leaf_str, lower_block, named_child, rakuast_node_of};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{PhaserKind, Stmt};
use crate::value::{RuntimeError, Value};

/// The declaration statement with its `will` phasers as its traits.
// Cost: O(n), n = size of the declaration and the phaser bodies.
pub(super) fn convert(stmt: &Stmt) -> Result<RakuAstNode, RuntimeError> {
    let (decl, phasers) =
        crate::ast::var_will::recognize(stmt).ok_or_else(|| unsupported("will trait"))?;
    let statement = convert_stmt(decl)?.ok_or_else(|| unsupported("will trait"))?;
    let mut declaration = expression_of(&statement)
        .filter(|d| d.class == RakuAstClass::VarDeclarationSimple)
        .ok_or_else(|| unsupported("will trait"))?;
    let mut traits = Vec::with_capacity(phasers.len());
    for (kind, body) in phasers {
        traits.push(Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TraitWill,
            fields: vec![
                leaf_field(
                    Some("phase"),
                    Value::str_from(crate::ast::var_will::phase_name(kind)),
                ),
                node_field(Some("block"), signature_block_node(body)?),
            ],
        })));
    }
    // The parser reads `is` traits both before and after the `will` ones and
    // keeps no order between the two kinds, so a mix stays the boundary.
    if declaration.fields.iter().any(|f| f.name == Some("traits")) {
        return Err(unsupported("`will` trait next to other traits"));
    }
    super::decl_traits::insert(&mut declaration, traits);
    Ok(statement_expression(declaration))
}

/// A lowered `will` phaser: its kind and body.
type LoweredPhaser = (PhaserKind, Vec<Stmt>);

/// A declaration node without its `Trait::Will` entries, and the phasers they
/// declare (kind and lowered body); `None` when it has none.
// Cost: O(n), n = size of the phaser bodies.
pub(super) fn split(
    node: &RakuAstNode,
) -> Result<Option<(RakuAstNode, Vec<LoweredPhaser>)>, RuntimeError> {
    let Some(RakuAstField {
        value: RakuAstFieldValue::List(items),
        ..
    }) = node.fields.iter().find(|f| f.name == Some("traits"))
    else {
        return Ok(None);
    };
    let mut phasers = Vec::new();
    let mut kept = Vec::new();
    for item in items {
        match rakuast_node_of(item) {
            Some(t) if t.class == RakuAstClass::TraitWill => {
                let kind = crate::ast::var_will::phase_kind(&leaf_str(t, "phase")?)
                    .ok_or_else(|| super::lower::unsupported(t))?;
                phasers.push((kind, lower_block(named_child(t, "block")?)?));
            }
            _ => kept.push(item.clone()),
        }
    }
    if phasers.is_empty() {
        return Ok(None);
    }
    let mut stripped = node.clone();
    if kept.is_empty() {
        stripped.fields.retain(|f| f.name != Some("traits"));
    } else if let Some(f) = stripped
        .fields
        .iter_mut()
        .find(|f| f.name == Some("traits"))
    {
        f.value = RakuAstFieldValue::List(kept);
    }
    Ok(Some((stripped, phasers)))
}
