//! Origins of a prefix operator's operand the parser kept no range for
//! (ADR-10723 Stage 2, #15780): `!not(not 0)`. The operand runs from after
//! the placed `Prefix` (and blanks) to the end of the application, and its
//! own parts are derived from that range.

use super::infix_origins::{byte_range, child};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;

/// Place the unplaced operand of `node`, a placed `ApplyPrefix` whose
/// `Prefix` is placed, when it is a call (`not(...)`).
// Cost: O(n), n = length of the operand's text, plus its derivation.
pub(super) fn place(names: &NameScope, node: &mut RakuAstNode) {
    if node.class != RakuAstClass::ApplyPrefix {
        return;
    }
    let (Some((_, to)), Some((_, op_end))) =
        (byte_range(node), child(node, "prefix").and_then(byte_range))
    else {
        return;
    };
    let Some(operand) = child(node, "operand") else {
        return;
    };
    if operand.origin.0.is_some()
        || !matches!(
            operand.class,
            RakuAstClass::CallName | RakuAstClass::CallNameWithoutParentheses
        )
        || op_end >= to
    {
        return;
    }
    let Some(from) = super::unit_source::with_text(SrcSpan::new(op_end, to), |text| {
        let written = text.trim_start();
        (!written.is_empty()).then(|| to - written.len())
    }) else {
        return;
    };
    super::infix_origins::with_child(node, "operand", |operand| {
        super::origin::set_span(operand, SrcSpan::new(from, to));
        super::infix_origins::derive(names, operand);
    });
}
