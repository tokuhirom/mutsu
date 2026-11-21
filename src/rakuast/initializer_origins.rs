//! Origins of a declaration's initializer whose expression has no range of
//! its own (ADR-10723 Stage 2, #15780): `my $f = * + *`, where the parser
//! turns the expression into a closure and keeps no position for it.
//!
//! The text after the `=` / `:=` is parsed again on its own and its ranges
//! are copied onto the expression (exact or absent); the `Initializer` then
//! runs from the operator to the end of the expression.

use super::RakuAstNode;
use super::infix_origins::{byte_range, with_child, with_child_at};
use crate::ast::SrcSpan;
use crate::scope::NameScope;

/// Place the initializer of `node`, a declaration with a range whose
/// initializer (written with `op`) holds an unplaced expression.
// Cost: O(k * n), k = occurrences of `op` in the declaration's source,
// n = its length, plus a parse of the text after each.
pub(super) fn unplaced_expression(names: &NameScope, node: &mut RakuAstNode, op: &str) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    let Some(text) = super::unit_source::with_text(SrcSpan::new(from, to), |t| Some(t.to_string()))
    else {
        return;
    };
    for (op_at, _) in text.match_indices(op) {
        let after = op_at + op.len();
        // `==`, `=>`, `=~`, `!=`, `::=` and the like are other operators.
        if text[..op_at].ends_with(['=', ':', '!', '<', '>', '~'])
            || text[after..].starts_with(['=', '>', '~'])
        {
            continue;
        }
        let rest = text[after..].trim_start();
        if rest.is_empty() {
            return;
        }
        let at = from + (text.len() - rest.len());
        let mut end = None;
        with_child(node, "initializer", |init| {
            with_child_at(init, 0, |expr| {
                super::fragment_origins::expression(names, expr, rest, at);
                end = byte_range(expr).map(|(_, e)| e);
            })
        });
        if let Some(end) = end {
            with_child(node, "initializer", |init| {
                super::origin::set_span(init, SrcSpan::new(from + op_at, end))
            });
            return;
        }
    }
}

/// Whether Rakudo gives `init`, the initializer of the declaration `node`,
/// an origin: always, except on an attribute (`has`), whose default keeps
/// one only when it is a literal one ([`super::attribute_decl::literal_default`]).
// Cost: O(s), s = segments of a quoted default.
pub(super) fn placed_by_rakudo(node: &RakuAstNode, init: &RakuAstNode) -> bool {
    let scope = node.fields.iter().find_map(|f| match &f.value {
        super::RakuAstFieldValue::Node(v) if f.name == Some("scope") => {
            v.as_str().map(str::to_string)
        }
        _ => None,
    });
    if scope.as_deref() != Some("has") {
        return true;
    }
    super::infix_origins::child_at(init, 0).is_none_or(super::attribute_decl::literal_default)
}
