//! The RakuAST node of a value the parsed tree holds that no literal class
//! spells (#16888): an object a slang action embedded with
//! `RakuAST::Literal.from-value` (EEL 0.3's compiled `EEL::Definition`), which
//! reaches the tree where the parser lowered the slang's body itself (a
//! `proto` with a non-`{*}` body). Rakudo keeps such a value in a plain
//! `RakuAST::Literal`; evaluating the node answers it.

use super::view::{leaf_field, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::value::{RuntimeError, Value, ValueView};

/// The node of `v`, the fallback of `view::view_literal`: an object is a
/// `RakuAST::Literal` holding it, any other value is a conversion gap.
// Cost: O(1).
pub(super) fn view_node(v: &Value) -> Result<RakuAstNode, RuntimeError> {
    match v.view() {
        ValueView::Instance { .. } => Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::Literal,
            fields: vec![leaf_field(None, v.clone())],
        }),
        other => Err(unsupported(&format!("literal {other:?}"))),
    }
}
