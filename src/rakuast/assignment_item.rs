//! Whether an `Assignment` is an item assignment (`Assignment.new(:item)`).
//!
//! Rakudo 2026.09's grammar (`src/Raku/Grammar.nqp`) decides it while it
//! parses an expression, from `$*LEFTSIGIL`: each `EXPR` starts it empty, the
//! first variable term of the expression sets it to its sigil, every postfix
//! (a method call, a subscript, `++`) sets it to `@`, and `infix:<=>` is an
//! item assignment exactly when it reads `$` (or sits inside a metaoperator),
//! then empties it again. A parenthesized list, an argument list, a ternary's
//! middle, a block or a declarator is an `EXPR` of its own and leaves the
//! enclosing one's sigil alone.
//!
//! The converted tree holds those terms in source order, so the flag is
//! derived by walking it the way the grammar read it: [`derive`] runs once
//! over a whole `.AST` tree. An `Assignment` inside a metaoperator (`X=`,
//! `R=`, `»=«`) is always `:item` and is left as converted.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// Set the `:item` flag of every plain `Assignment` under `node`.
// Cost: O(n), n = size of the tree.
pub(super) fn derive(node: &mut RakuAstNode) {
    nested(node);
}

/// The sigil an expression's variable terms have left so far: `$*LEFTSIGIL`.
#[derive(Clone, Copy, PartialEq)]
enum LeftSigil {
    Unset,
    Scalar,
    Other,
}

/// Walk `node` as an expression of its own: the children of a node that is
/// not part of an operator expression each start a fresh `EXPR`.
// Cost: O(n), n = size of the subtree.
fn nested(node: &mut RakuAstNode) {
    let mut sigil = LeftSigil::Unset;
    expression(node, &mut sigil);
}

/// Walk `node` as part of the expression whose sigil is `sigil`.
// Cost: O(n), n = size of the subtree.
fn expression(node: &mut RakuAstNode, sigil: &mut LeftSigil) {
    match node.class {
        RakuAstClass::ApplyInfix => {
            with_field(node, "left", |left| expression(left, sigil));
            with_field(node, "infix", |infix| {
                if infix.class == RakuAstClass::Assignment {
                    set_item(infix, *sigil == LeftSigil::Scalar);
                    *sigil = LeftSigil::Unset;
                } else {
                    nested(infix);
                }
            });
            with_field(node, "right", |right| expression(right, sigil));
        }
        RakuAstClass::ApplyListInfix => {
            with_field(node, "infix", nested);
            with_list(node, "operands", |operand| expression(operand, sigil));
        }
        RakuAstClass::ApplyPrefix => {
            with_field(node, "prefix", nested);
            with_field(node, "operand", |operand| expression(operand, sigil));
        }
        RakuAstClass::ApplyPostfix => {
            with_field(node, "operand", |operand| expression(operand, sigil));
            with_field(node, "postfix", nested);
            *sigil = LeftSigil::Other;
        }
        RakuAstClass::ApplyDottyInfix => {
            with_field(node, "left", |left| expression(left, sigil));
            with_field(node, "infix", nested);
            with_field(node, "right", nested);
            *sigil = LeftSigil::Other;
        }
        RakuAstClass::Ternary => {
            with_field(node, "condition", |c| expression(c, sigil));
            // `?? EXPR('i=') !!`: the middle is an expression of its own.
            with_field(node, "then", nested);
            with_field(node, "else", |e| expression(e, sigil));
        }
        class if is_variable(class) => {
            if *sigil == LeftSigil::Unset {
                *sigil = match variable_sigil(node) {
                    Some('$') => LeftSigil::Scalar,
                    _ => LeftSigil::Other,
                };
            }
            children(node);
        }
        _ => children(node),
    }
}

/// Walk every child node of `node` as an expression of its own.
// Cost: O(n), n = size of the subtree.
fn children(node: &mut RakuAstNode) {
    for field in &mut node.fields {
        match &mut field.value {
            RakuAstFieldValue::Node(held) => rewrite(held, nested),
            RakuAstFieldValue::List(items) => {
                for item in items {
                    rewrite(item, nested);
                }
            }
            _ => {}
        }
    }
}

/// The variable terms the grammar's `variable` token reads: the
/// `RakuAST::Var::*` family.
// Cost: O(1).
fn is_variable(class: RakuAstClass) -> bool {
    class.printed_name().starts_with("RakuAST::Var::")
}

/// The sigil a variable node was written with: its `sigil` field, or the
/// first character of its name.
// Cost: O(1).
fn variable_sigil(node: &RakuAstNode) -> Option<char> {
    if node.class == RakuAstClass::VarPositionalCapture {
        return Some('$');
    }
    let text = |names: &[Option<&str>]| {
        node.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if names.contains(&f.name) => {
                v.as_str().and_then(|s| s.chars().next())
            }
            _ => None,
        })
    };
    text(&[Some("sigil")]).or_else(|| text(&[None, Some("name")]))
}

/// Give an `Assignment` node the `:item` flag, or take it away.
// Cost: O(1).
fn set_item(assignment: &mut RakuAstNode, item: bool) {
    assignment
        .fields
        .retain(|f| !matches!(f.value, RakuAstFieldValue::Adverb("item")));
    if item {
        assignment.fields.push(super::RakuAstField {
            name: None,
            value: RakuAstFieldValue::Adverb("item"),
        });
    }
}

/// Apply `f` to the node held by `node`'s field `name`.
// Cost: O(1) plus `f`.
fn with_field(node: &mut RakuAstNode, name: &str, f: impl FnOnce(&mut RakuAstNode)) {
    if let Some(field) = node.fields.iter_mut().find(|f| f.name == Some(name))
        && let RakuAstFieldValue::Node(held) = &mut field.value
    {
        rewrite(held, f);
    }
}

/// Apply `f` to each node of `node`'s list field `name`.
// Cost: O(k) plus `f`, k = list length.
fn with_list(node: &mut RakuAstNode, name: &str, mut f: impl FnMut(&mut RakuAstNode)) {
    if let Some(field) = node.fields.iter_mut().find(|f| f.name == Some(name))
        && let RakuAstFieldValue::List(items) = &mut field.value
    {
        for item in items {
            rewrite(item, &mut f);
        }
    }
}

/// Apply `f` to the RakuAST node `held` holds, if it holds one.
// Cost: O(s) plus `f`, s = size of the node's own fields (copied).
fn rewrite(held: &mut Value, f: impl FnOnce(&mut RakuAstNode)) {
    let ValueView::RakuAst(inner) = held.view() else {
        return;
    };
    let mut inner = RakuAstNode::clone(inner);
    f(&mut inner);
    *held = Value::rakuast(Box::new(inner));
}
