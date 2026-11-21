//! Origins of a C-style loop's setup (ADR-10723 Stage 2, #15780):
//! `loop (my $i = 0; $i < 3; $i++) { }`. The parser keeps the setup as a
//! statement of its own with no range; it is the text between the `(` and
//! the first `;`, parsed again on its own.

use super::infix_origins::child;
use super::{RakuAstClass, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;

/// Place the unplaced `setup` of `node`, a `Statement::Loop` written at
/// `span` (after its labels).
// Cost: O(n), n = length of the setup's text, plus the parse of it.
pub(super) fn place(names: &NameScope, node: &mut RakuAstNode, span: SrcSpan) {
    if node.class != RakuAstClass::StatementLoop
        || child(node, "setup").is_none_or(|setup| setup.origin.0.is_some())
    {
        return;
    }
    let from = span.from as usize;
    let Some((at, text)) = super::unit_source::with_text(span, |text| {
        let rest = text.strip_prefix("loop")?;
        let open = rest.len() - rest.trim_start().len();
        let inside = rest[open..].strip_prefix('(')?;
        let setup = &inside[..inside.find(';')?];
        // Exact or absent: a bracket, quote or comment could hold the `;`.
        if setup.contains(['(', '[', '{', '"', '\'', '#']) {
            return None;
        }
        let lead = setup.len() - setup.trim_start().len();
        let written = setup.trim();
        let at = "loop".len() + open + 1 + lead;
        (!written.is_empty()).then(|| (from + at, written.to_string()))
    }) else {
        return;
    };
    super::infix_origins::with_child(node, "setup", |setup| {
        super::fragment_origins::expression(names, setup, &text, at)
    });
}
