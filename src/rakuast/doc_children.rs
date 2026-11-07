//! The `RakuAST::Doc::Declarator` child of a documented declaration.
//!
//! Rakudo's `visit-children` of a declaration visits its `WHY` among the other
//! children, at a place that depends on the class (measured on 2026.09):
//! a routine (`Sub`, `Method`, `token`, ...) visits it right after its name
//! (first when anonymous); an `enum` or `subset` before its traits; every
//! other declaration (`class`, `role`, `grammar`, `module`, `package`, a
//! variable, a parameter) last. The `Doc::Declarator` itself has no children.

use super::{RakuAstClass as C, RakuAstField, RakuAstNode, why_doc};
use crate::value::{Value, ValueView};

/// Whether `field` is the `WHEREFORE` of a `Doc::Declarator`, which
/// `visit-children` does not follow (it is the documented node itself).
// Cost: O(1).
pub(crate) fn is_wherefore(node: &RakuAstNode, field: &RakuAstField) -> bool {
    node.class == C::DocDeclarator && field.name == Some("WHEREFORE")
}

fn class_of(value: &Value) -> Option<C> {
    match value.view() {
        ValueView::RakuAst(node) => Some(node.class),
        _ => None,
    }
}

fn is_trait(value: &Value) -> bool {
    class_of(value).is_some_and(|c| c.printed_name().starts_with("RakuAST::Trait::"))
}

/// Put the documentation of `node`, if it has any, among its `children`.
// Cost: O(c + f + l), c = children, f = fields of the node, l = lines of docs.
pub(crate) fn insert(node: &RakuAstNode, children: &mut Vec<Value>) {
    if node.class == C::DocDeclarator {
        return;
    }
    let doc = why_doc::why(node);
    if class_of(&doc) != Some(C::DocDeclarator) {
        return;
    }
    let at = match node.class {
        C::Sub
        | C::Method
        | C::Submethod
        | C::RegexDeclaration
        | C::TokenDeclaration
        | C::RuleDeclaration => usize::from(children.first().and_then(class_of) == Some(C::Name)),
        C::TypeEnum | C::TypeSubset => children.iter().position(is_trait).unwrap_or(children.len()),
        _ => children.len(),
    };
    children.insert(at, doc);
}
