//! A slang's override of the core `statement` action (ADR-15762 slice 3, the
//! pre-finalization pass of `slang_bridge::apply_action_overrides`).
//!
//! rakudo runs `method statement(Mu $/)` of a slang's actions role for every
//! statement it parses, after the core action attached the statement's node:
//! the override `callsame`s, reads `$/.made`, and may attach a replacement
//! (Dawa wraps each statement in a call of its debugger's stop point). Its
//! `$/` is the statement within the unit's text, so `.orig`, `.from` and `.to`
//! place it, and `$*ORIGIN-SOURCE.original-file` names the unit's file.
//!
//! The Rust parser builds statements without running actions, so the pass
//! offers each statement after the `use` of the activating module here: its
//! node, and its span from the `SetLine` marker that precedes it. A node the
//! override hands back unchanged leaves the parsed statement as it is.

use super::RakuAstFieldValue;
use crate::ast::{SrcSpan, Stmt};
use crate::scope::NameScope;
use crate::value::RuntimeError;

/// Offer `stmt`, which occupies `span` of the unit's source, to the slang's
/// `statement` override: `Some` replacement when the override attached a node
/// other than the core one.
///
/// A statement whose node cannot be built (a shape the conversion does not
/// model yet), or with no span in the unit's text, is not offered.
// TODO: offer every statement once the conversion models them all; a
// declaration whose node cannot be built is skipped rather than failing the
// unit.
// Cost: O(n + s) to build the node and send the unit's text, n = size of the
// statement, s = length of the unit's source; plus the slang's own action.
pub(super) fn apply(stmt: &Stmt, span: SrcSpan) -> Result<Option<Stmt>, RuntimeError> {
    if !span.is_some() {
        return Ok(None);
    }
    let Some(source) = crate::unit_source_text::innermost_source() else {
        return Ok(None);
    };
    let (from, to) = (span.from as usize, span.to as usize);
    let (Some(before), Some(text)) = (source.get(..from), source.get(from..to)) else {
        return Ok(None);
    };
    let names = NameScope::default();
    let Ok(list) = super::statements::statement_list(&names, std::slice::from_ref(stmt)) else {
        return Ok(None);
    };
    let Some(RakuAstFieldValue::Node(core)) = list.fields.first().map(|f| &f.value) else {
        return Ok(None);
    };
    // The host indexes the text by characters.
    let start = before.chars().count();
    let span = crate::slang_link::ActionSpan {
        end: start + text.chars().count(),
        file: crate::unit_source_file::current().map(|file| file.resolve().to_string()),
    };
    let attached =
        crate::parser::run_slang_action_at("statement", core.clone(), &source, start, span)
            .map_err(|e| RuntimeError::new(format!("slang action statement: {e}")))?;
    if attached == *core {
        return Ok(None);
    }
    super::slang_bridge::lower_statement(&attached).map(Some)
}
