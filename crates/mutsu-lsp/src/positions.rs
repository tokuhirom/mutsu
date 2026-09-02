//! Converting mutsu's positions into LSP's.
//!
//! This is the module ADR-0065 D5 warns about most. mutsu numbers lines and
//! columns from 1 and counts columns in **characters**; LSP numbers lines and
//! characters from 0 and counts characters in **UTF-16 code units**. Every
//! diagnostic the server emits passes through here, and the intended consumer —
//! an AI agent — will absorb an off-by-one silently and never complain. So the
//! conversion is pinned by tests rather than by looking at an editor.

use lsp_types::{Position, Range};

/// The line `line_1based` of `text` and its 0-based index, or the last line
/// when the position is past the end — a diagnostic must still land somewhere
/// in the document rather than be dropped.
fn line_text(text: &str, line_1based: u32) -> (&str, u32) {
    let wanted = line_1based.max(1) - 1;
    // `split` always yields at least one item, so `found` is always assigned.
    let mut found = ("", 0);
    for (index, line) in text.split('\n').enumerate() {
        found = (line, index as u32);
        if index as u32 == wanted {
            break;
        }
    }
    found
}

/// UTF-16 code units in the first `chars` characters of `line`.
fn utf16_offset(line: &str, chars: u32) -> u32 {
    line.chars()
        .take(chars as usize)
        .map(|c| c.len_utf16() as u32)
        .sum()
}

/// mutsu's 1-based (line, character column) to LSP's 0-based (line, UTF-16
/// offset). A column past the end of the line clamps to the line's end.
pub fn to_position(text: &str, line_1based: u32, column_1based: u32) -> Position {
    let (line, line_index) = line_text(text, line_1based);
    Position {
        line: line_index,
        character: utf16_offset(line, column_1based.saturating_sub(1)),
    }
}

/// The range a diagnostic covers, given the point mutsu reported.
///
/// mutsu reports a *point*, not a span: the AST has no positions at all
/// (ADR-0065 D6) and a parse failure carries only the offset it ejected at. The
/// choice made here is **from that point to the end of the line**, because for a
/// parse failure the remainder of the line is precisely the text mutsu could not
/// make sense of. A warning, which has no column, covers the whole line.
///
/// A zero-width range would also be defensible and is what a caret-accurate
/// server would emit; it is rejected because the consumer is an agent reading a
/// range out of JSON, not a human watching a squiggle, and "this much of the
/// line is the problem" carries more information than "the problem starts here".
pub fn diagnostic_range(text: &str, line_1based: u32, column_1based: Option<u32>) -> Range {
    let (line, line_index) = line_text(text, line_1based);
    let line_end = utf16_offset(line, line.chars().count() as u32);
    // A warning carries no column, and column 1 converts to offset 0 — the
    // start of the line — which is exactly the "whole line" answer it wants.
    let start = to_position(text, line_1based, column_1based.unwrap_or(1)).character;
    // A point at (or past) the end of the line would produce an empty range at
    // the far right, which renders as nothing and reads as nothing. Fall back to
    // the whole line, which is the honest answer: the problem is on this line
    // and mutsu cannot narrow it further.
    let (start, end) = if start >= line_end {
        (0, line_end)
    } else {
        (start, line_end)
    };
    Range {
        start: Position {
            line: line_index,
            character: start,
        },
        end: Position {
            line: line_index,
            character: end,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_column_of_the_first_line_is_the_origin() {
        let p = to_position("say 1;\n", 1, 1);
        assert_eq!(
            p,
            Position {
                line: 0,
                character: 0
            }
        );
    }

    #[test]
    fn lines_and_columns_both_lose_one() {
        let p = to_position("say 1;\nsay 2;\n", 2, 5);
        assert_eq!(
            p,
            Position {
                line: 1,
                character: 4
            }
        );
    }

    #[test]
    fn columns_are_utf16_code_units_not_characters() {
        // Four astral-plane characters, each two UTF-16 code units. mutsu's
        // column 5 is the 5th *character*, which LSP calls character 8.
        let text = "my $x = '🐪🐪🐪🐪';\n";
        assert_eq!(to_position(text, 1, 5).character, 4, "ASCII prefix is 1:1");
        let camels = "🐪🐪🐪🐪 rest";
        assert_eq!(utf16_offset(camels, 4), 8);
        assert_eq!(utf16_offset(camels, 5), 9, "the space after four camels");
    }

    #[test]
    fn a_column_past_the_end_of_the_line_clamps_to_the_end() {
        let p = to_position("ab\n", 1, 99);
        assert_eq!(
            p,
            Position {
                line: 0,
                character: 2
            }
        );
    }

    #[test]
    fn a_line_past_the_end_of_the_document_clamps_to_the_last_line() {
        // A trailing newline makes a final empty line, which is where this lands.
        let p = to_position("ab\n", 99, 1);
        assert_eq!(p.line, 1);
    }

    #[test]
    fn a_range_runs_from_the_point_to_the_end_of_its_line() {
        let r = diagnostic_range("my $x = 1;\nsay + ;\n", 2, Some(5));
        assert_eq!(
            r.start,
            Position {
                line: 1,
                character: 4
            }
        );
        assert_eq!(
            r.end,
            Position {
                line: 1,
                character: 7
            }
        );
    }

    #[test]
    fn a_range_without_a_column_covers_the_whole_line() {
        let r = diagnostic_range("my $x = 1;\n$x == 1;\n", 2, None);
        assert_eq!(
            r.start,
            Position {
                line: 1,
                character: 0
            }
        );
        assert_eq!(
            r.end,
            Position {
                line: 1,
                character: 8
            }
        );
    }

    #[test]
    fn a_point_at_the_end_of_a_line_falls_back_to_the_whole_line() {
        let r = diagnostic_range("say 1;\n", 1, Some(7));
        assert_eq!(r.start.character, 0);
        assert_eq!(r.end.character, 6);
    }

    #[test]
    fn an_empty_line_yields_an_empty_range_rather_than_a_panic() {
        let r = diagnostic_range("\n\n", 2, Some(1));
        assert_eq!(r.start, r.end);
        assert_eq!(r.start.line, 1);
    }
}
