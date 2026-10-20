//! Validated slices of the source buffer currently returned by the parser.

use super::{LEAKED_REGIONS, LeakedRegion, newline_index};
use crate::parser::expr::consumed_span;

/// Register a permanently allocated heredoc buffer and its source-line jump.
/// The two source slices identify where each half was copied from.
pub(in crate::parser) fn register_leaked_region_with_jump(
    leaked: &'static str,
    jump_offset: usize,
    line_before_jump: i64,
    line_after_jump: i64,
    before_src: &str,
    after_src: &str,
) {
    let newlines = newline_index(leaked);
    LEAKED_REGIONS.with(|r| {
        r.borrow_mut().push(LeakedRegion {
            source: leaked,
            ptr: leaked.as_ptr() as usize,
            len: leaked.len(),
            jump_offset,
            line_before_jump,
            line_after_jump,
            newlines,
            before_src: before_src.as_ptr() as usize,
            after_src: after_src.as_ptr() as usize,
        });
    });
}

pub(in crate::parser) struct SourceTail<'a> {
    source: &'a str,
    offset: usize,
    end: usize,
}

impl<'a> SourceTail<'a> {
    pub(in crate::parser) fn before(&self) -> &'a str {
        &self.source[..self.offset]
    }

    pub(in crate::parser) fn restore_from(&self, start: usize) -> Option<&'a str> {
        (start <= self.offset)
            .then(|| self.source.get(start..self.end))
            .flatten()
    }
}

/// Find the actual buffer containing the returned remainder. Heredoc parsing
/// rewrites source, so subtracting lengths from the original input is invalid.
// Cost: O(h), h = registered heredoc buffers; O(1) for an unchanged buffer.
pub(in crate::parser) fn source_tail<'a>(input: &'a str, rest: &str) -> Option<SourceTail<'a>> {
    if let Some(consumed) = consumed_span(input, rest) {
        return Some(SourceTail {
            source: input,
            offset: consumed.len(),
            end: input.len(),
        });
    }
    LEAKED_REGIONS.with(|regions| {
        regions.borrow().iter().rev().find_map(|region| {
            let offset = (rest.as_ptr() as usize).checked_sub(region.ptr)?;
            let end = offset.checked_add(rest.len())?;
            region.source.get(..offset)?;
            region.source.get(offset..end)?;
            Some(SourceTail {
                source: region.source,
                offset,
                end,
            })
        })
    })
}
