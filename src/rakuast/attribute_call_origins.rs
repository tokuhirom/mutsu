//! Origins of a call through a public attribute's accessor (ADR-10723 Stage
//! 2; #15780): the arguments of `$.foo(1)` / `$.foo: 5`, and the meta-method
//! call of `$.^name`, which rakudo builds as `(self.^name).item`.

use super::infix_origins::{byte_range, child, leaf_text, with_child};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;

/// Place the `ArgList` of `node`, a `Var::Attribute::Public` with a range
/// whose name is followed by `(ARGS)` or `: ARGS`.
// Cost: O(n), n = length of the call's source.
pub(super) fn public_args(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let (Some(name), Some(_)) = (leaf_text(node), child(node, "args")) else {
        return;
    };
    let after = from + name.len();
    let args = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let rest = text.strip_prefix(name.as_str())?;
        if rest.starts_with('(') && rest.ends_with(')') {
            return super::call_origins::parenthesized(
                child(node, "args"),
                SrcSpan::new(after + 1, to - 1),
            );
        }
        let args = rest.strip_prefix(':')?;
        let lead = args.len() - args.trim_start().len();
        (lead > 0 && !args.trim().is_empty()).then(|| (after + 1 + lead, to))
    });
    if let Some((from, to)) = args {
        with_child(node, "args", |list| {
            super::origin::set_span(list, SrcSpan::new(from, to))
        });
    }
}

/// `$.^name`: `node` is the `ApplyPostfix(ApplyPostfix(Term::Self,
/// Call::MetaMethod), Call::Method(item))` it is built as. The meta-method
/// call runs over the name after `$.^`, with its empty `ArgList` at the end;
/// the inner application has no range (measured on rakudo 2026.11).
// Cost: O(n), n = length of the call's source.
pub(super) fn meta_item(node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let is_meta = child(node, "operand").is_some_and(|operand| {
        operand.class == RakuAstClass::ApplyPostfix
            && child(operand, "operand").is_some_and(|o| o.class == RakuAstClass::TermSelf)
            && child(operand, "postfix").is_some_and(|p| p.class == RakuAstClass::CallMetaMethod)
    });
    if !is_meta {
        return;
    }
    let written = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let name = text.strip_prefix("$.^")?;
        (!name.is_empty() && name.chars().all(|c| c.is_alphanumeric() || matches!(c, '_' | '-')))
            .then_some(())
    });
    if written.is_none() {
        return;
    }
    with_child(node, "operand", |operand| {
        with_child(operand, "postfix", |call| {
            super::origin::set_span(call, SrcSpan::new(from + 3, to));
            super::empty_fields::set_span(call, "args", SrcSpan::new(to, to));
        })
    });
}
