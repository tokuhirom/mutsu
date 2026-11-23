//! An operator written in brackets (`1 [+] 2`, `1 ![==] 2`, `1 [R-] 2`)
//! (#18504).
//!
//! The parser resolves `[op]` to the operator itself, which is all execution
//! needs; rakudo keeps a `BracketedInfix` around it (inside the `!` of a
//! negated one). When an infix application is placed in the source, the text
//! between its operands says whether the operator was written in brackets, and
//! the bracket node is put back around it.

use super::infix_origins::{child, with_child};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Wrap the `infix` of the application `node` in the `BracketedInfix` the
/// source between its operands (`between`) spells, if it does.
// Cost: O(n), n = length of the text between the operands.
pub(super) fn recover(node: &mut RakuAstNode, between: SrcSpan) {
    let Some(infix) = child(node, "infix") else {
        return;
    };
    // `[R-]`: brackets around the whole operator.
    if let Some(depth) = bracket_depth(infix, None, between) {
        with_child(node, "infix", |infix| wrap(infix, depth));
        return;
    }
    // `![==]` / `R[+]`: the brackets sit inside the `!` / `R`.
    let meta = match infix.class {
        RakuAstClass::MetaInfixNegate => '!',
        RakuAstClass::MetaInfixReverse => 'R',
        _ => return,
    };
    let Some(held) = super::infix_origins::child_at(infix, 0) else {
        return;
    };
    if let Some(depth) = bracket_depth(held, Some(meta), between) {
        with_child(node, "infix", |infix| {
            super::infix_origins::with_child_at(infix, 0, |held| wrap(held, depth));
        });
    }
}

/// How many brackets enclose `op` in the text `between` the operands (after
/// the `meta` character, if any): `[[+]]` is two; `None` when it is not
/// written in brackets.
// Cost: O(n), n = length of the text between the operands.
fn bracket_depth(op: &RakuAstNode, meta: Option<char>, between: SrcSpan) -> Option<usize> {
    let (op, _) = super::infix_spelling::written(op)?;
    super::unit_source::with_text(between, |text| {
        let after = text.trim_start();
        let after = match meta {
            Some(meta) => after.strip_prefix(meta)?,
            None => after,
        };
        let opens = after.len() - after.trim_start_matches('[').len();
        let rest = after[opens..].strip_prefix(op.as_str())?;
        let closes = rest.len() - rest.trim_start_matches(']').len();
        (opens > 0 && opens == closes && opens <= 128).then_some(opens)
    })
}

/// `node` inside `depth` `BracketedInfix` nodes.
// Cost: O(d), d = `depth`.
fn wrap(node: &mut RakuAstNode, depth: usize) {
    for _ in 0..depth {
        let held = std::mem::replace(
            node,
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::BracketedInfix,
                fields: Vec::new(),
            },
        );
        node.fields.push(super::view::node_field(None, held));
    }
}

/// The application `node` with the brackets around its operator taken off,
/// which is what it lowers as (`1 [+] 2` runs `1 + 2`); `None` when its
/// operator is not bracketed. A compound `[op]=` keeps its own lowering.
// Cost: O(f + d), f = fields of `node`, d = bracket depth.
pub(super) fn peeled(node: &RakuAstNode) -> Option<RakuAstNode> {
    if !matches!(
        node.class,
        RakuAstClass::ApplyInfix | RakuAstClass::ApplyListInfix
    ) {
        return None;
    }
    let infix = child(node, "infix")?;
    let bracketed = |n: &RakuAstNode| n.class == RakuAstClass::BracketedInfix;
    let inside_meta = matches!(
        infix.class,
        RakuAstClass::MetaInfixNegate | RakuAstClass::MetaInfixReverse
    );
    let target = if inside_meta {
        super::infix_origins::child_at(infix, 0)?
    } else {
        infix
    };
    if !bracketed(target) {
        return None;
    }
    let mut node = node.clone();
    let unwrap = |n: &mut RakuAstNode| {
        let mut depth = 0;
        while n.class == RakuAstClass::BracketedInfix && depth < 128 {
            let Some(held) = super::infix_origins::child_at(n, 0).cloned() else {
                return;
            };
            *n = held;
            depth += 1;
        }
    };
    with_child(&mut node, "infix", |infix| {
        if inside_meta {
            super::infix_origins::with_child_at(infix, 0, unwrap);
        } else {
            unwrap(infix);
        }
    });
    Some(node)
}
