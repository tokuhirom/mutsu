//! Origins of a call on a term, `$f(1, 2)` / `(* + 1)(2)`
//! (`ApplyPostfix(operand, Call::Term(args))`): the `Call::Term` runs from
//! the `(` right after the operand to the closing `)`, and its `ArgList`
//! from the first argument to the last. Read off the source and checked:
//! exact or absent.

use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place the `Call::Term` of `node`, an application ending at `span.to`
/// whose operand ends at byte `operand_end`, and its `ArgList`.
// Cost: O(n), n = length of the call's source.
pub(super) fn place(node: &mut RakuAstNode, operand_end: usize, span: SrcSpan) {
    let to = span.to as usize;
    // `$f.(1)`: the call starts at the `(` after the dot.
    let dot = super::unit_source::with_text(SrcSpan::new(operand_end, to), |text| {
        Some(usize::from(text.starts_with(".(")))
    })
    .unwrap_or(0);
    let operand_end = operand_end + dot;
    let Some(inner) = super::unit_source::with_text(SrcSpan::new(operand_end, to), |text| {
        let body = text.strip_prefix('(')?.strip_suffix(')')?;
        // The first `(` must close at the very end.
        let mut depth = 1usize;
        for c in body.chars() {
            match c {
                '(' => depth += 1,
                ')' => depth = depth.checked_sub(1).filter(|d| *d > 0)?,
                _ => {}
            }
        }
        let trimmed = body.trim();
        let start = operand_end + 1 + (body.len() - body.trim_start().len());
        Some((start, start + trimmed.len()))
    }) else {
        return;
    };
    super::infix_origins::with_child(node, "postfix", |call| {
        if call.class != RakuAstClass::CallTerm || call.origin.0.is_some() {
            return;
        }
        super::origin::set_span(call, SrcSpan::new(operand_end, to));
        if inner.0 < inner.1 {
            super::infix_origins::with_child(call, "args", |args| {
                super::origin::set_span(args, SrcSpan::new(inner.0, inner.1))
            });
        } else {
            // `$f()`: the (absent) empty `ArgList` sits just inside the parentheses.
            super::empty_fields::set_span(
                call,
                "args",
                SrcSpan::new(operand_end + 1, operand_end + 1),
            );
        }
    });
}
