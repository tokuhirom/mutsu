//! Declarator documentation on named declarations (`#| doc` above
//! `sub foo`, `#= doc` after `has $.x`; #14880).
//!
//! The parser (ADR-0136) decides which declaration each `#|`/`#=` comment
//! documents and records the declaration's start offset. The converted tree
//! carries no comments, so this pass walks it after the conversion and gives
//! the declaration node that starts at a documented offset the same hidden
//! `declarator-docs` fields an anonymous sub carries
//! ([`super::declarator_docs`]); rakudo prints them as a trailing
//! `.declarator-docs(...)` call.

use super::{RakuAstClass as C, RakuAstFieldValue, RakuAstNode};
use crate::decl_doc::DocComment;
use crate::value::{Value, ValueView};
use std::collections::HashMap;

/// Whether rakudo's declarator docs can sit on a node of `class`.
// Cost: O(1).
fn documentable(class: C) -> bool {
    matches!(
        class,
        C::Sub
            | C::Method
            | C::Submethod
            | C::Class
            | C::Role
            | C::Grammar
            | C::Module
            | C::Package
            | C::VarDeclarationSimple
    )
}

/// Attach the docs of `docs` to the declaration nodes of `node`'s tree.
// Cost: O(n), n = nodes of the tree.
pub(super) fn attach(node: &mut RakuAstNode, docs: Vec<DocComment>) {
    let by_start: HashMap<usize, DocComment> = docs
        .into_iter()
        .filter(|d| !d.is_anonymous)
        .map(|d| (d.start, d))
        .collect();
    if by_start.is_empty() {
        return;
    }
    walk(node, &by_start);
}

// Cost: O(size of the subtree).
fn walk(node: &mut RakuAstNode, docs: &HashMap<usize, DocComment>) {
    if documentable(node.class)
        && let Some((from, _)) = super::infix_origins::byte_range(node)
        && let Some(doc) = docs.get(&from)
        && !node.fields.iter().any(super::declarator_docs::is_field)
    {
        super::declarator_docs::attach(node, Some(&doc.doc));
    }
    for field in &mut node.fields {
        match &mut field.value {
            RakuAstFieldValue::Node(child) => visit(child, docs),
            RakuAstFieldValue::List(items) => {
                for item in items {
                    visit(item, docs);
                }
            }
            _ => {}
        }
    }
}

// Cost: O(size of the subtree).
fn visit(child: &mut Value, docs: &HashMap<usize, DocComment>) {
    let ValueView::RakuAst(held) = child.view() else {
        return;
    };
    let mut node = RakuAstNode::clone(held);
    walk(&mut node, docs);
    *child = Value::rakuast(Box::new(node));
}
