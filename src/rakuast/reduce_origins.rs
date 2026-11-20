//! Origins of the parts of a reduction, `[op] args` (`RakuAST::Term::Reduce`,
//! #15780): the operator is the text between the brackets (after a `\` of a
//! triangular reduction), and the `ArgList` the text after the `]` (inside
//! its parentheses for `[op](args)`). Read off the source and checked: exact
//! or absent.

use super::infix_origins::{byte_range, child, with_child};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place the operator and the arguments of `node`, a `Term::Reduce`.
// Cost: O(n), n = length of the reduction's source.
pub(super) fn derive(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(parts) = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let inner = text.strip_prefix('[')?;
        let close = inner.find(']')?;
        let op_from = 1 + usize::from(inner.starts_with('\\'));
        let op_to = 1 + close;
        let rest = &text[op_to + 1..];
        let args = match rest.strip_prefix('(').and_then(|r| r.strip_suffix(')')) {
            Some(body) => {
                let lead = body.len() - body.trim_start().len();
                let at = op_to + 2 + lead;
                (at, at + body.trim().len())
            }
            None => {
                let lead = rest.len() - rest.trim_start().len();
                (op_to + 1 + lead, text.len())
            }
        };
        Some((
            (
                from + op_from,
                from + op_to,
                text[op_from..op_to].to_string(),
            ),
            (from + args.0, from + args.1),
        ))
    }) else {
        return;
    };
    let ((op_from, op_to, written), (args_from, args_to)) = parts;
    if let Some(infix) = child(node, "infix")
        && infix.class == RakuAstClass::Infix
        && infix.origin.0.is_none()
        && super::infix_origins::leaf_text(infix).is_some_and(|op| op == written)
    {
        with_child(node, "infix", |infix| {
            super::origin::set_span(infix, SrcSpan::new(op_from, op_to))
        });
    }
    if args_from < args_to
        && let Some(args) = child(node, "args")
        && args.class == RakuAstClass::ArgList
        && args.origin.0.is_none()
    {
        with_child(node, "args", |args| {
            super::origin::set_span(args, SrcSpan::new(args_from, args_to))
        });
    }
}
