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
    let Some((at, inner)) = measure(operand_end, span.to as usize) else {
        return;
    };
    super::infix_origins::with_child(node, "postfix", |call| {
        set(call, at, span.to as usize, inner)
    });
}

/// Place the `Call::Term` a hyper postfix holds (`@a».(3)`), written after
/// the `»` / `>>` that ends at byte `after_marker`, and its `ArgList`.
// Cost: O(n), n = length of the call's source.
pub(super) fn place_hyper(hyper: &mut RakuAstNode, after_marker: usize, span: SrcSpan) {
    let Some((at, inner)) = measure(after_marker, span.to as usize) else {
        return;
    };
    super::infix_origins::with_child_at(hyper, 0, |call| set(call, at, span.to as usize, inner));
}

/// Where a call written after `operand_end` and ending at `to` starts (at its
/// `(`, past a `.` before it), and the range of its arguments' text.
// Cost: O(n), n = length of the call's source.
fn measure(operand_end: usize, to: usize) -> Option<(usize, (usize, usize))> {
    // `$f.(1)`: the call starts at the `(` after the dot.
    // `(* + *)(3, 4)`: parentheses around the operand are not a node of
    // their own, so the call starts after their `)`.
    let paren = super::unit_source::with_text(SrcSpan::new(operand_end, to), |text| {
        let rest = text.trim_start_matches(|c: char| c == ')' || c.is_whitespace());
        (rest.len() < text.len() && text.starts_with(')')).then_some(text.len() - rest.len())
    })
    .unwrap_or(0);
    let operand_end = operand_end + paren;
    let dot = super::unit_source::with_text(SrcSpan::new(operand_end, to), |text| {
        Some(usize::from(text.starts_with(".(")))
    })
    .unwrap_or(0);
    let operand_end = operand_end + dot;
    let inner = super::unit_source::with_text(SrcSpan::new(operand_end, to), |text| {
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
    })?;
    Some((operand_end, inner))
}

/// Give `call`, a `Call::Term` written from `at` to `to`, and its arguments
/// (at `inner`) their ranges.
// Cost: O(1).
fn set(call: &mut RakuAstNode, at: usize, to: usize, inner: (usize, usize)) {
    if call.class != RakuAstClass::CallTerm || call.origin.0.is_some() {
        return;
    }
    super::origin::set_span(call, SrcSpan::new(at, to));
    if inner.0 < inner.1 {
        super::infix_origins::with_child(call, "args", |args| {
            super::origin::set_span(args, SrcSpan::new(inner.0, inner.1))
        });
    } else {
        // `$f()`: the (absent) empty `ArgList` sits just inside the parentheses.
        super::empty_fields::set_span(call, "args", SrcSpan::new(at + 1, at + 1));
    }
}
