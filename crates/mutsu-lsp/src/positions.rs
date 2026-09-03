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

/// The full range of one 1-based line.
pub fn line_range(text: &str, line_1based: u32) -> Range {
    let (line, line_index) = line_text(text, line_1based);
    Range {
        start: Position {
            line: line_index,
            character: 0,
        },
        end: Position {
            line: line_index,
            character: utf16_offset(line, line.chars().count() as u32),
        },
    }
}

/// The range from the start of `first_line` to the end of `last_line`.
pub fn span(text: &str, first_line: u32, last_line: u32) -> Range {
    Range {
        start: line_range(text, first_line).start,
        end: line_range(text, last_line.max(first_line)).end,
    }
}

/// Where `name` appears on `line_1based`, or the whole line when it does not.
///
/// LSP wants a declaration's `selectionRange` to cover the *name*, which is
/// where "go to symbol" puts the caret. mutsu's AST cannot say where the name
/// is — it has no positions (D6) — but the declaration line is short and the
/// name is a literal, so finding it in the text is both cheap and exact. The
/// match must be on an identifier boundary, or `has $.x` would select the `x`
/// inside `max`.
pub fn name_range(text: &str, line_1based: u32, name: &str) -> Range {
    let (line, line_index) = line_text(text, line_1based);
    if let Some(start_byte) = identifier_occurrence(line, name) {
        let start = utf16_offset(line, line[..start_byte].chars().count() as u32);
        let end = start + name.chars().map(|c| c.len_utf16() as u32).sum::<u32>();
        return Range {
            start: Position {
                line: line_index,
                character: start,
            },
            end: Position {
                line: line_index,
                character: end,
            },
        };
    }
    line_range(text, line_1based)
}

/// Byte offset of `name` in `line` as a whole identifier, if present.
fn identifier_occurrence(line: &str, name: &str) -> Option<usize> {
    if name.is_empty() {
        return None;
    }
    let mut from = 0;
    while let Some(found) = line[from..].find(name) {
        let start = from + found;
        let end = start + name.len();
        let before_ok = line[..start]
            .chars()
            .next_back()
            .is_none_or(|c| !is_identifier_char(c));
        let after_ok = line[end..]
            .chars()
            .next()
            .is_none_or(|c| !is_identifier_char(c));
        if before_ok && after_ok {
            return Some(start);
        }
        from = start + name.chars().next().map_or(1, char::len_utf8);
    }
    None
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// The Raku identifier under `position`, if there is one.
///
/// `definition` needs to know what the caret is on, and mutsu's AST cannot say:
/// it has no positions for *references*, only the `Stmt::SetLine` markers that
/// place declarations (D6). Reading the identifier straight out of the document
/// text sidesteps that entirely — the server has the text, and an identifier is
/// a lexical notion that needs no parse.
///
/// Hyphens and apostrophes are identifier characters in Raku (`is-prime`,
/// `don't`), but only between alphanumerics, so a trailing `-` in `$x-` is not
/// taken.
pub fn identifier_at(text: &str, position: Position) -> Option<String> {
    let line = text.split('\n').nth(position.line as usize)?;
    // LSP characters are UTF-16 code units; walk the line accumulating them to
    // find the byte offset the caret is at.
    let mut byte = line.len();
    let mut units = 0u32;
    for (offset, c) in line.char_indices() {
        if units >= position.character {
            byte = offset;
            break;
        }
        units += c.len_utf16() as u32;
    }
    if units < position.character {
        byte = line.len();
    }

    let bytes_start = line[..byte]
        .char_indices()
        .rev()
        .take_while(|(i, c)| is_identifier_char(*c) || is_infix_identifier_char(line, *i, *c))
        .map(|(i, _)| i)
        .last()
        .unwrap_or(byte);
    let mut end = byte;
    for (i, c) in line[byte..].char_indices() {
        let absolute = byte + i;
        if is_identifier_char(c) || is_infix_identifier_char(line, absolute, c) {
            end = absolute + c.len_utf8();
        } else {
            break;
        }
    }
    let word = &line[bytes_start..end];
    let word = word.trim_matches(|c| !is_identifier_char(c));
    (!word.is_empty()).then(|| word.to_string())
}

/// `-` and `'` join an identifier only when they sit between two alphanumerics.
fn is_infix_identifier_char(line: &str, offset: usize, c: char) -> bool {
    if c != '-' && c != '\'' {
        return false;
    }
    let before = line[..offset].chars().next_back();
    let after = line[offset + c.len_utf8()..].chars().next();
    matches!((before, after), (Some(b), Some(a)) if is_identifier_char(b) && is_identifier_char(a))
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

    #[test]
    fn a_name_range_covers_just_the_name() {
        let text = "class Foo {\n    method bar() { 1 }\n}\n";
        let r = name_range(text, 2, "bar");
        assert_eq!(
            r.start,
            Position {
                line: 1,
                character: 11
            }
        );
        assert_eq!(
            r.end,
            Position {
                line: 1,
                character: 14
            }
        );
    }

    #[test]
    fn a_name_range_does_not_match_inside_a_longer_word() {
        // `x` must not select the `x` inside `max`.
        let text = "my $max = 1;\nhas $.x;\n";
        let r = name_range(text, 2, "x");
        assert_eq!(r.start.character, 6, "the attribute's own x");
    }

    #[test]
    fn a_name_that_is_not_on_the_line_falls_back_to_the_whole_line() {
        let r = name_range("say 1;\n", 1, "elsewhere");
        assert_eq!(r, line_range("say 1;\n", 1));
    }

    #[test]
    fn the_identifier_under_the_caret_is_read_from_the_text() {
        let text = "say frobnicate(1);\n";
        let at = |ch| {
            identifier_at(
                text,
                Position {
                    line: 0,
                    character: ch,
                },
            )
        };
        assert_eq!(at(4).as_deref(), Some("frobnicate"));
        assert_eq!(at(9).as_deref(), Some("frobnicate"));
        assert_eq!(at(1).as_deref(), Some("say"));
    }

    #[test]
    fn a_hyphenated_identifier_is_one_word() {
        let text = "say is-prime(7);\n";
        assert_eq!(
            identifier_at(
                text,
                Position {
                    line: 0,
                    character: 6
                }
            )
            .as_deref(),
            Some("is-prime")
        );
    }

    #[test]
    fn a_caret_on_punctuation_yields_nothing() {
        assert_eq!(
            identifier_at(
                "  ;\n",
                Position {
                    line: 0,
                    character: 2
                }
            ),
            None
        );
        assert_eq!(
            identifier_at(
                "say 1;\n",
                Position {
                    line: 9,
                    character: 0
                }
            ),
            None
        );
    }

    #[test]
    fn the_caret_position_is_utf16_when_reading_an_identifier() {
        let text = "my $s = '🐪🐪'; say frobnicate(1);\n";
        // `frobnicate` starts after the camels: 4 UTF-16 units for two camels.
        let camel_units: u32 = "my $s = '🐪🐪'; say "
            .chars()
            .map(|c| c.len_utf16() as u32)
            .sum();
        assert_eq!(
            identifier_at(
                text,
                Position {
                    line: 0,
                    character: camel_units + 2
                }
            )
            .as_deref(),
            Some("frobnicate")
        );
    }
}
