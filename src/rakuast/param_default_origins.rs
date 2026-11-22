//! The parts of a routine parameter the parser left without a range
//! (#15780): its `where` block, and a default whose operands are all `*`
//! (`$x = * <=> *`), whose text after the `=` is parsed again as a fragment
//! and the ranges copied onto it.

use super::RakuAstNode;
use super::infix_origins::{child, with_child};
use crate::ast::SrcSpan;

/// Place the unplaced `where` block and `default` of `parameter`, written at
/// `span`: the default is the expression after its `=` to the parameter's end.
// Cost: O(n), n = length of the parameter's source, plus the parse of the default.
pub(super) fn place(parameter: &mut RakuAstNode, span: SrcSpan) {
    super::expr_block_origins::parameter_where(parameter, span);
    if !span.is_some() || child(parameter, "default").is_none_or(|d| d.origin.0.is_some()) {
        return;
    }
    let from = span.from as usize;
    let Some((at, text)) = super::unit_source::with_text(span, |text| {
        let (op_at, _) = text.match_indices('=').find(|(i, _)| {
            // `==`, `=>`, `!=`, `<=` and the like are other operators.
            !text[..*i].ends_with(['=', ':', '!', '<', '>', '~'])
                && !text[i + 1..].starts_with(['=', '>', '~'])
        })?;
        let rest = text[op_at + 1..].trim_start();
        (!rest.is_empty()).then(|| (from + text.len() - rest.len(), rest.to_string()))
    }) else {
        return;
    };
    with_child(parameter, "default", |default| {
        super::fragment_origins::expression(&crate::scope::NameScope::default(), default, &text, at)
    });
}
