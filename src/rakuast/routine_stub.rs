//! The `body` of a routine a lookup resolves to (#17501).
//!
//! A `Call::Name` or `&name` that names a sub of the unit resolves to that
//! sub's declaration. The resolution pass hands out a copy of the declaration
//! without its body: a routine's body holds the lookups of the routines it
//! calls, itself included, so a resolution carrying its body would hold a copy
//! of every routine it reaches -- without end for a recursive one. In rakudo
//! the resolution *is* the declaration node, so `.resolution.body` reads the
//! body (Red's compiler inlines a helper sub that way). The copy answers its
//! `body` from the declaration in the unit's tree, the node with the same
//! class and origin.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// Whether `node` is a resolution's body-less copy of a routine declaration.
// Cost: O(f), f = fields of the node.
pub(super) fn is_stub(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::Sub
        && node.origin.0.is_some()
        && !node.fields.iter().any(|f| f.name == Some("body"))
}

/// The `from`/`to` offsets of a node's origin.
// Cost: O(1).
fn span(node: &RakuAstNode) -> Option<(i64, i64)> {
    let origin = node.origin.0.as_ref()?;
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return None;
    };
    let map = attributes.as_map();
    let offset = |name: &str| match map.get(name)?.view() {
        ValueView::Int(n) => Some(n),
        _ => None,
    };
    Some((offset("from")?, offset("to")?))
}

/// The `field` of the declaration in `root` that `stub` copies.
// Cost: O(n), n = nodes of the tree under `root`.
pub(super) fn declaration_field(root: &Value, stub: &RakuAstNode, field: &str) -> Option<Value> {
    let want = span(stub)?;
    find(root, stub.class, want, field)
}

// Cost: O(n), n = nodes of the subtree; recursion depth = the tree's depth,
// which the parser that built the tree already bounded.
fn find(value: &Value, class: RakuAstClass, want: (i64, i64), field: &str) -> Option<Value> {
    let ValueView::RakuAst(node) = value.view() else {
        return None;
    };
    if node.class == class
        && span(node) == Some(want)
        && let Some(found) = node.fields.iter().find(|f| f.name == Some(field))
        && let RakuAstFieldValue::Node(held) = &found.value
    {
        return Some(held.clone());
    }
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(child) => find(child, class, want, field),
        RakuAstFieldValue::List(items) => {
            items.iter().find_map(|item| find(item, class, want, field))
        }
        RakuAstFieldValue::Adverb(_) => None,
    })
}
