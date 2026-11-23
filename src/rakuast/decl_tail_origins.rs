//! Origins of the head of a declaration or assignment with a loose tail
//! (#18578): `my $fh = open $f orelse die`, `my $x = 1 and 2`,
//! `$x = 1 and 2`.
//!
//! The view puts the parser's head (`my $fh = open $f`) back as the leading
//! operand of the tail ([`super::decl_tail`]); the head is converted on its
//! own, so nothing placed it. It starts where the whole application starts
//! and ends where its last part (the initializer's expression, the assigned
//! value) ends, which is checked against the source before it is placed.

use super::infix_origins::{byte_range, child, child_at, with_child};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;
use crate::value::ValueView;

/// Place the unplaced head that `node`, an infix or list-infix application
/// with a range, starts with, and what it holds.
// Cost: O(n), n = length of the head's source.
pub(super) fn leading_head(names: &NameScope, node: &mut RakuAstNode) {
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let place = |head: &mut RakuAstNode| {
        if head.origin.0.is_some() {
            return;
        }
        let Some(to) = head_end(head) else {
            return;
        };
        if to <= from || !starts_as_written(head, from) {
            return;
        }
        super::origin::set_span(head, SrcSpan::new(from, to));
        super::infix_origins::derive(names, head);
    };
    match node.class {
        RakuAstClass::ApplyInfix => with_child(node, "left", place),
        RakuAstClass::ApplyListInfix => {
            if node.fields.iter().any(|f| {
                f.name == Some("operands") && matches!(f.value, RakuAstFieldValue::List(_))
            }) {
                with_operand0(node, place);
            }
        }
        _ => {}
    }
}

/// Run `f` on the first operand of a list-infix application.
// Cost: O(f), f = fields of `node`.
fn with_operand0(node: &mut RakuAstNode, f: impl FnOnce(&mut RakuAstNode)) {
    let Some(RakuAstFieldValue::List(items)) = node
        .fields
        .iter_mut()
        .find(|f| f.name == Some("operands"))
        .map(|f| &mut f.value)
    else {
        return;
    };
    if let Some(first) = items.first_mut()
        && let Some(held) = super::lower::rakuast_node_of(first)
    {
        let mut held = held.clone();
        f(&mut held);
        *first = crate::value::Value::rakuast(Box::new(held));
    }
}

/// Where a head ends: the end of its initializer's expression (a
/// declaration), of its right operand (an infix application: the value an
/// assignment assigns, or the last of a chain `my $x = 1 and 2` that a looser
/// operator continues) or of its last operand (a list application). `None`
/// for any other operand, or one whose last part is unplaced.
// Cost: O(f), f = fields of `head`.
fn head_end(head: &RakuAstNode) -> Option<usize> {
    match head.class {
        RakuAstClass::VarDeclarationSimple | RakuAstClass::VarDeclarationSignature => {
            let init = child(head, "initializer")?;
            if !matches!(
                init.class,
                RakuAstClass::InitializerAssign | RakuAstClass::InitializerBind
            ) {
                return None;
            }
            child_at(init, 0).and_then(byte_range).map(|(_, to)| to)
        }
        RakuAstClass::ApplyInfix => child(head, "right").and_then(byte_range).map(|(_, to)| to),
        RakuAstClass::ApplyListInfix => {
            let operands = head.fields.iter().find_map(|f| match &f.value {
                RakuAstFieldValue::List(items) if f.name == Some("operands") => Some(items),
                _ => None,
            })?;
            let last = super::lower::rakuast_node_of(operands.last()?)?;
            byte_range(last).map(|(_, to)| to)
        }
        _ => None,
    }
}

/// How deep a chain of heads is followed (`my $x = 1 and 2 and 3`).
const MAX_CHAIN: usize = 64;

/// Whether the source at `from` reads as the start of `head`: a
/// declaration's scope keyword, an assignment's variable, or (for an
/// application a looser operator continues) the start of its own head.
// Cost: O(d), d = depth of the chain of heads.
fn starts_as_written(head: &RakuAstNode, from: usize) -> bool {
    let mut head = head;
    for _ in 0..MAX_CHAIN {
        match head.class {
            RakuAstClass::VarDeclarationSimple | RakuAstClass::VarDeclarationSignature => {
                let scope = head
                    .fields
                    .iter()
                    .find_map(|f| match &f.value {
                        RakuAstFieldValue::Node(v) if f.name == Some("scope") => match v.view() {
                            ValueView::Str(s) => Some(s.to_string()),
                            _ => None,
                        },
                        _ => None,
                    })
                    .unwrap_or_else(|| "my".into());
                return super::unit_source::with_text_from(from, |text| {
                    text.strip_prefix(scope.as_str())
                        .map(|rest| rest.starts_with(|c: char| c.is_whitespace() || c == '('))
                })
                .unwrap_or(false);
            }
            RakuAstClass::ApplyInfix
                if child(head, "infix").is_some_and(|i| i.class == RakuAstClass::Assignment) =>
            {
                return child(head, "left")
                    .filter(|target| target.class == RakuAstClass::VarLexical)
                    .is_some_and(|target| variable_at(target, from));
            }
            RakuAstClass::ApplyInfix => match child(head, "left") {
                Some(left) => head = left,
                None => return false,
            },
            RakuAstClass::ApplyListInfix => match first_operand(head) {
                Some(first) => head = first,
                None => return false,
            },
            _ => return false,
        }
    }
    false
}

/// The first operand of a list-infix application.
// Cost: O(f), f = fields of `node`.
fn first_operand(node: &RakuAstNode) -> Option<&RakuAstNode> {
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::List(items) if f.name == Some("operands") => {
            items.first().and_then(super::lower::rakuast_node_of)
        }
        _ => None,
    })
}

/// Whether the source at `from` starts with the variable `target` names.
// Cost: O(v), v = length of the name.
fn variable_at(target: &RakuAstNode, from: usize) -> bool {
    let Some(name) = super::infix_origins::leaf_text(target) else {
        return false;
    };
    super::unit_source::with_text_from(from, |text| {
        text.strip_prefix(name.as_str()).map(|rest| {
            !rest.starts_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '\''))
        })
    })
    .unwrap_or(false)
}
