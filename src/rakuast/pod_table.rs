//! The lines of a Pod `table` block as `RakuAST::Doc::LegacyRow` rows
//! (ADR-10723 Stage 4; #14880).
//!
//! A port of rakudo 2026.11's `RakuAST::Doc::Block!interpret-as-table`
//! (`src/core.c/RakuAST/Fixups.rakumod`): the common leading whitespace is
//! taken off, the lines are read assuming virtual (whitespace) column
//! dividers, and when a row holds a visible `|` / `+` divider they are read
//! again by their visible dividers. Rows between dividers are then merged
//! into multi-line rows, and the legacy header heuristic decides
//! `header-row`. Where rakudo dies ("Table has a mixture of visible and
//! invisible column-separator types"), or a cell would hold a formatting code
//! (a `Doc::Paragraph` cell, not read here), the table is left out: exact or
//! absent.

/// A row of the table.
pub(super) struct Row {
    pub(super) dividers: Option<String>,
    pub(super) offsets: Vec<i64>,
    pub(super) cells: Vec<String>,
}

pub(super) enum TablePart {
    Row(Row),
    Separator(String),
}

pub(super) struct Table {
    pub(super) parts: Vec<TablePart>,
    /// The legacy heuristic found a header row.
    pub(super) header_row: bool,
}

/// The characters a row divider line is made of.
// Cost: O(1).
fn is_row_divider(c: char) -> bool {
    matches!(c, ' ' | '_' | '-' | '+' | '|' | '=')
}

/// Rakudo's `is-ws`: a blank (the text read here is ASCII).
// Cost: O(1).
fn is_ws(c: char) -> bool {
    c == ' '
}

/// The table of `raw`, the lines of the block's body.
// Cost: O(n), n = size of the body.
pub(super) fn read(raw: &[&str]) -> Option<Table> {
    // `@matched.join.subst(/\n+ $/).lines`: trailing blank lines go.
    let to = raw.iter().rposition(|l| !l.is_empty())? + 1;
    let from = raw.iter().position(|l| !l.is_empty())?;
    // Offsets count code points, cells graphemes: only text where the two
    // agree is read.
    if raw[from..to]
        .iter()
        .any(|l| !l.is_ascii() || l.contains('\r'))
    {
        return None;
    }
    let mut lines: Vec<&str> = raw[from..to].to_vec();
    // Remove the leading whitespace all lines share, as measured from the
    // first line (a line of fewer blanks, or an empty one, lowers it).
    let mut leading = lines[0].len() - lines[0].trim_start().len();
    for line in &lines[1..] {
        leading = leading.min(line.len() - line.trim_start().len());
    }
    if leading > 0 {
        for line in &mut lines {
            *line = &line[leading..];
        }
    }
    let rows = match virtual_rows(&lines)? {
        Some(rows) => rows,
        None => lines
            .iter()
            .map(|l| visual_row(l))
            .collect::<Option<Vec<_>>>()?,
    };
    Some(post_process(rows))
}

/// The rows read assuming virtual dividers (two or more blanks between
/// columns); `Some(None)` when a row has a visible divider (read them
/// visually), `None` when the table mixes both kinds (rakudo dies).
// Cost: O(n), n = size of the lines.
fn virtual_rows(lines: &[&str]) -> Option<Option<Vec<TablePart>>> {
    let mut codes_per_row: Vec<Vec<char>> = Vec::new();
    let mut offsets_per_line: Vec<Option<Vec<usize>>> = Vec::new();
    for line in lines {
        let codes: Vec<char> = line.chars().collect();
        let elems = codes.len();
        let mut offsets = Vec::new();
        let mut is_row = false;
        let mut no_more_leading = false;
        let mut prev = '\0';
        let mut i = 0usize;
        while i < elems {
            let curr = codes[i];
            if curr == ' ' && prev == ' ' {
                if no_more_leading {
                    // In a column: eat the blanks; the next column starts
                    // after them, unless they end the line.
                    while i + 1 < elems && codes[i + 1] == ' ' {
                        i += 1;
                    }
                    if i + 1 < elems {
                        offsets.push(i + 1);
                    }
                }
            } else {
                if is_row_divider(curr) {
                    if is_row
                        && matches!(curr, '|' | '+')
                        && is_ws(prev)
                        && (i + 1 == elems || is_ws(codes[i + 1]))
                    {
                        // A visible divider: read visually, unless rows read
                        // virtually came before (a mixture).
                        return if codes_per_row.is_empty() {
                            Some(None)
                        } else {
                            None
                        };
                    }
                } else {
                    is_row = true;
                }
                if curr != ' ' {
                    no_more_leading = true;
                }
            }
            prev = curr;
            i += 1;
        }
        if is_row {
            codes_per_row.push(codes);
            offsets_per_line.push(Some(offsets));
        } else {
            offsets_per_line.push(None);
        }
    }
    // The column offsets: every offset seen, kept when two positions before
    // it no row has a character other than a blank.
    let mut offsets: Vec<usize> = Vec::new();
    for offset in offsets_per_line.iter().flatten().flatten() {
        if !offsets.contains(offset) {
            offsets.push(*offset);
        }
    }
    offsets.retain(|&offset| {
        !codes_per_row
            .iter()
            .any(|codes| offset >= 2 && offset - 2 < codes.len() && codes[offset - 2] != ' ')
    });
    offsets.sort_unstable();
    let column_offsets: Vec<i64> = std::iter::once(0)
        .chain(offsets.iter().map(|&o| o as i64))
        .collect();
    let mut parts = Vec::with_capacity(lines.len());
    for (line, per_line) in lines.iter().zip(&offsets_per_line) {
        if per_line.is_none() {
            parts.push(TablePart::Separator(line.to_string()));
            continue;
        }
        let chars = line.len();
        let mut cells = Vec::with_capacity(offsets.len() + 1);
        let mut start = 0usize;
        for &offset in &offsets {
            cells.push(if start > chars {
                String::new()
            } else {
                cell(substr(line, start, offset as i64 - start as i64 - 2))?
            });
            start = offset;
        }
        cells.push(if start > chars {
            String::new()
        } else {
            cell(&line[start..])?
        });
        parts.push(TablePart::Row(Row {
            dividers: None,
            offsets: column_offsets.clone(),
            cells,
        }));
    }
    Some(Some(parts))
}

/// One line read by its visible `|` / `+` dividers (rakudo's `columnify`);
/// `None` for a row without any divider (rakudo dies).
// Cost: O(l), l = length of the line.
fn visual_row(line: &str) -> Option<TablePart> {
    let mut codes: Vec<char> = line.chars().collect();
    let elems = codes.len();
    // A virtual blank at the end, for a trailing divider.
    codes.push(' ');
    let mut dividers = String::new();
    let mut offsets: Vec<usize> = Vec::new();
    let mut seen_divider = false;
    let mut prev = ' ';
    let mut is_row = false;
    let mut i = 0usize;
    while i < elems {
        let curr = codes[i];
        if curr == '|' || curr == '+' {
            seen_divider = true;
            if is_ws(prev) && is_ws(codes[i + 1]) {
                dividers.push(curr);
                i += 1;
                offsets.push(i + 1);
                prev = '\0';
            }
        } else {
            if !is_row_divider(curr) {
                is_row = true;
            }
            prev = curr;
        }
        i += 1;
    }
    if !is_row {
        return Some(TablePart::Separator(format!("{line}\n")));
    }
    if !seen_divider {
        return None;
    }
    let chars = line.len();
    let mut cells = Vec::with_capacity(offsets.len() + 1);
    let mut start = 0usize;
    for &offset in &offsets {
        // A first offset of 2 is a divider that starts the line: no cell
        // before it.
        if offset != 2 && start <= chars {
            cells.push(cell(substr(line, start, offset as i64 - start as i64 - 3))?);
        }
        start = offset;
    }
    if start <= chars {
        cells.push(cell(&line[start..])?);
    }
    Some(TablePart::Row(Row {
        dividers: Some(dividers),
        offsets: offsets.iter().map(|&o| o as i64).collect(),
        cells,
    }))
}

/// `nqp::substr($line, $start, $len)` for `start <= chars`: the text is cut
/// at the line's end, and a negative length takes the rest of the line.
// Cost: O(1).
fn substr(line: &str, start: usize, len: i64) -> &str {
    let end = if len < 0 {
        line.len()
    } else {
        (start + len as usize).min(line.len())
    };
    &line[start..end]
}

/// A cell's text; `None` when it holds a formatting code (rakudo's cell is
/// then a `Doc::Paragraph`, not read here).
// Cost: O(n), n = length of the cell.
fn cell(text: &str) -> Option<String> {
    let code = text
        .as_bytes()
        .windows(2)
        .any(|w| w[0].is_ascii_uppercase() && w[1] == b'<');
    (!code).then(|| text.to_string())
}

/// Merge the rows between dividers and decide the legacy header row.
// Cost: O(n), n = size of the rows.
fn post_process(mut rows: Vec<TablePart>) -> Table {
    let last_divider = match rows.last() {
        Some(TablePart::Separator(_)) => rows.pop(),
        _ => None,
    };
    let mut paragraphs: Vec<TablePart> = Vec::new();
    let mut sofar: Vec<Row> = Vec::new();
    let mut leading: Vec<TablePart> = Vec::new();
    let mut merge_multi_row = false;
    for part in rows {
        match part {
            TablePart::Separator(text) => {
                if !paragraphs.is_empty() {
                    if !sofar.is_empty() {
                        merge_multi_row = true;
                        add_rows(&mut paragraphs, &mut sofar, merge_multi_row);
                    }
                    paragraphs.push(TablePart::Separator(text));
                } else if !sofar.is_empty() {
                    // The first divider always merges the rows before it.
                    add_rows(&mut paragraphs, &mut sofar, true);
                    paragraphs.push(TablePart::Separator(text));
                } else {
                    leading.push(TablePart::Separator(text));
                }
            }
            TablePart::Row(row) => sofar.push(row),
        }
    }
    add_rows(&mut paragraphs, &mut sofar, merge_multi_row);
    // The legacy heuristic: a second divider unlike the first, or exactly
    // one divider after the first row, makes the first row a header.
    let mut seen_row = false;
    let mut first_divider: Option<&str> = None;
    let mut other_dividers = 0;
    let mut header_row = false;
    for part in &paragraphs {
        match part {
            TablePart::Separator(text) => {
                if let Some(first) = first_divider {
                    if text != first {
                        header_row = true;
                        break;
                    }
                    other_dividers += 1;
                } else if seen_row {
                    first_divider = Some(text);
                }
            }
            TablePart::Row(_) => seen_row = true,
        }
    }
    if !header_row && first_divider.is_some() && other_dividers == 0 {
        header_row = true;
    }
    let mut parts = leading;
    parts.extend(paragraphs);
    parts.extend(last_divider);
    Table { parts, header_row }
}

/// Put the rows collected so far into `paragraphs`: merged into the first
/// one when `merge` and there are several.
// Cost: O(n), n = size of the rows.
fn add_rows(paragraphs: &mut Vec<TablePart>, sofar: &mut Vec<Row>, merge: bool) {
    if merge && sofar.len() > 1 {
        let mut rest = std::mem::take(sofar).into_iter();
        if let Some(mut first) = rest.next() {
            for row in rest {
                merge_row(&mut first, row);
            }
            paragraphs.push(TablePart::Row(first));
        }
    } else {
        paragraphs.extend(sofar.drain(..).map(TablePart::Row));
    }
}

/// Rakudo's `LegacyRow.merge-rows` for one row: each of its cells is joined
/// to `first`'s with a newline, `first` widened to the wider of the two; a
/// shorter row stops joining at its last cell.
// Cost: O(n), n = size of the rows.
fn merge_row(first: &mut Row, row: Row) {
    if row.cells.len() > first.cells.len() {
        first.cells.resize(row.cells.len(), String::new());
    }
    for (merged, other) in first.cells.iter_mut().zip(&row.cells) {
        merged.push('\n');
        merged.push_str(other);
    }
}
