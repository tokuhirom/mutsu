//! The source text a recorded [`SrcSpan`] covers, read back from the buffer
//! being parsed: a diagnosis that names an expression by its spelling (the
//! sink worry's "Useless use of 1 + 2"), and the `------>` line it points at.

use super::{ORIGINAL_SOURCE, source_position};
use crate::ast::SrcSpan;

/// The text `span` covers in the buffer being parsed, when the buffer holds
/// it (`None` for an unrecorded span, or one outside the buffer).
// Cost: O(n), n = the span's length.
pub(in crate::parser) fn span_text(span: SrcSpan) -> Option<String> {
    if !span.is_some() {
        return None;
    }
    let from = source_position(span.from as usize)?;
    let to = source_position(span.to as usize)?;
    ORIGINAL_SOURCE.with(|s| {
        let origin = s.borrow();
        let text = origin.text.as_ref()?;
        text.get(from..to).map(str::to_string)
    })
}

/// Rakudo's worry pointer for a construct starting at unit offset `at`: the
/// line it is on, with `<HERE>` at the construct (`<BOL>` when nothing
/// precedes it on the line, `<EOL>` when nothing follows).
// Cost: O(l), l = the length of the line.
pub(in crate::parser) fn here_pointer(at: u32) -> Option<String> {
    if at == u32::MAX {
        return None;
    }
    let at = source_position(at as usize)?;
    ORIGINAL_SOURCE.with(|s| {
        let origin = s.borrow();
        let text = origin.text.as_ref()?;
        let before = text.get(..at)?;
        let after = text.get(at..)?;
        let line_start = before.rfind('\n').map_or(0, |i| i + 1);
        let prefix = &before[line_start..];
        let rest = after.split('\n').next().unwrap_or("");
        let prefix = if prefix.is_empty() { "<BOL>" } else { prefix };
        let rest = if rest.is_empty() { "<EOL>" } else { rest };
        Some(format!("{prefix}<HERE>{rest}"))
    })
}
