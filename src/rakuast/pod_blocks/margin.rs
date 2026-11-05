//! A Pod block whose directive is indented (`    =begin pod` inside a block,
//! or at the unit's top level): a `Doc::Block` with `margin` set to that
//! indentation (#14880).
//!
//! Rakudo reads the block with the margin taken off every line of it, so the
//! block is read here from a view of its lines with the margin stripped
//! (offsets kept absolute) by the ordinary reader. A delimited block runs to
//! its `=end`; an extended or abbreviated one to the first blank, directive or
//! less-indented line. Measured on rakudo 2026.09; a margin of exactly one
//! blank loses the trailing blank line of an abbreviated paragraph there, a
//! quirk not read (the block is left out).

use super::{Block, Lines, Part};

/// The block that begins at the indented line `i`, and the line after it.
// Cost: O(b), b = lines of the block.
pub(super) fn block(lines: &Lines, i: usize) -> Option<(Block, usize)> {
    let indent = lines.indent(i);
    let margin = &lines.text[i][..indent];
    if margin.contains('\t') {
        return None;
    }
    let (name, rest) = lines.directive(i)?;
    let last = match name {
        "begin" => {
            let kind = rest.split_whitespace().next()?;
            let mut j = i + 1;
            loop {
                if j >= lines.text.len() {
                    return None;
                }
                if let Some(("end", tail)) = lines.directive(j)
                    && tail.trim() == kind
                    && lines.text[j].starts_with(margin)
                {
                    break j;
                }
                if !lines.blank(j) && !lines.text[j].starts_with(margin) {
                    return None;
                }
                j += 1;
            }
        }
        "end" | "config" | "table" | "formula" => return None,
        _ => {
            // A margin of one blank has a paragraph quirk of its own.
            if indent == 1 {
                return None;
            }
            let mut j = i + 1;
            while j < lines.text.len()
                && !lines.blank(j)
                && lines.directive(j).is_none()
                && lines.text[j].starts_with(margin)
            {
                j += 1;
            }
            while j < lines.text.len() && lines.blank(j) {
                j += 1;
            }
            j - 1
        }
    };
    // A line indented more than the margin keeps its extra blanks in some
    // shapes and loses them in others: not read.
    if (i + 1..=last).any(|j| {
        let line = lines.text[j];
        !lines.blank(j) && line[indent..].starts_with(char::is_whitespace)
    }) {
        return None;
    }
    // The same quirk drops the blank line after a paragraph of a delimited
    // block.
    if indent == 1 && (i..=last).any(|j| lines.blank(j)) {
        return None;
    }
    let view = Lines {
        names: lines.names,
        base: lines.base,
        text: (i..=last)
            .map(|j| lines.text[j].get(indent..).unwrap_or(""))
            .collect(),
        start: (i..=last).map(|j| lines.start[j] + indent).collect(),
    };
    let (mut block, next) = super::block_flat(&view, 0)?;
    if block.parts.iter().any(|p| matches!(p, Part::Block(_))) {
        return None;
    }
    block.margin = Some(margin.to_string());
    Some((block, i + next))
}
