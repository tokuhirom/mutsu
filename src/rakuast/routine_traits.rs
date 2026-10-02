//! A method's `multi`, `!private`, `is rw` and `is raw` across the RakuAST
//! boundary.
//!
//! Measured against rakudo 2026.09, `multi method !p() is rw { … }` is
//!
//! ```text
//! Method(multiness => "multi", private => True, name => …,
//!        traits => (Trait::Is(name => Name.from-identifier("rw")),), body => …)
//! ```
//!
//! `multiness` and `private` precede `name`; a trait sits in `traits` before
//! `body`. The parser keeps `is rw` / `is raw` as flags beside the return-type
//! trait, not in source order, so a method carrying more than one of
//! `is rw`, `is raw` and `returns`/`of` is refused rather than rendered in an
//! invented order.

use super::convert::{leaf_field, name_from_identifier, node_field, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{RuntimeError, Value};

/// The flag-valued `is` traits a method can carry.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(super) struct IsTraits {
    pub(super) is_rw: bool,
    pub(super) is_raw: bool,
}

impl IsTraits {
    /// The trait name of `Trait::Is(name => …)` this flag set is, if any.
    pub(super) fn set_name(&mut self, name: &str) -> bool {
        let flag = match name {
            "rw" => &mut self.is_rw,
            "raw" => &mut self.is_raw,
            _ => return false,
        };
        *flag = true;
        true
    }
}

/// Add a method's `multiness`, `private` and flag traits to the routine node
/// `routine_node` built.
// Cost: O(f), f = fields of `node`.
pub(super) fn add_method_flags(
    node: &mut RakuAstNode,
    multi: bool,
    private: bool,
    traits: IsTraits,
) -> Result<(), RuntimeError> {
    let written: Vec<&str> = [(traits.is_rw, "rw"), (traits.is_raw, "raw")]
        .into_iter()
        .filter_map(|(on, name)| on.then_some(name))
        .collect();
    if let Some(name) = written.first() {
        let has_traits = node.fields.iter().any(|f| f.name == Some("traits"));
        if written.len() > 1 || has_traits {
            return Err(unsupported(
                "method with several traits (their source order is not kept)",
            ));
        }
        let at = node
            .fields
            .iter()
            .position(|f| f.name == Some("body"))
            .unwrap_or(node.fields.len());
        let trait_node = RakuAstNode {
            class: RakuAstClass::TraitIs,
            fields: vec![node_field(Some("name"), name_from_identifier(name))],
        };
        node.fields.insert(
            at,
            RakuAstField {
                name: Some("traits"),
                value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(trait_node))]),
            },
        );
    }
    if private {
        node.fields
            .insert(0, leaf_field(Some("private"), Value::truth(true)));
    }
    if multi {
        node.fields
            .insert(0, leaf_field(Some("multiness"), Value::str_from("multi")));
    }
    Ok(())
}
