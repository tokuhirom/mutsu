//! The lines of a Pod `table` block as `RakuAST::Doc::LegacyRow` rows
//! (ADR-10723 Stage 4; #14880).
//!
//! Rakudo splits a table's lines into rows with a column heuristic whose
//! source is not on the box, so only the shapes measured and checked against
//! `raku` are read; anything else (tabs, a stray `|` or `+`, rows whose
//! columns do not line up, a header block of several lines next to other
//! separators, ...) returns `None` and leaves the block out of the tree:
//! exact or absent.
//!
//! - Whitespace tables: a row's cells start after each run of two or more
//!   spaces; every row must start its cells at the same offsets, and a cell
//!   runs up to two characters before the next offset (a wider gap stays in
//!   the cell). A table with no gap anywhere is one cell per row.
//! - Pipe tables: a `|` with a blank or the line's edge on both sides is a
//!   divider, each row has its own offsets (two past each divider) and its
//!   cells are the text between the dividers minus one blank on each side.
//! - A blank line or a rule line (`--`, `==`, `-- --`, `--+--`) is a
//!   separator, kept as a string. Exactly one separator with rows on both
//!   sides makes the rows before it one multi-line row and adds
//!   `header-row => 0` to the block.

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
    /// The rows before the one separator were a header.
    pub(super) header_row: bool,
}

enum Line<'a> {
    Row(&'a str),
    Separator(&'a str),
}

/// The table of `raw`, the lines of the block's body.
// Cost: O(n), n = size of the body.
pub(super) fn read(raw: &[&str]) -> Option<Table> {
    let blank = |l: &&str| l.is_empty();
    let from = raw.iter().position(|l| !blank(l))?;
    let to = raw.iter().rposition(|l| !blank(l))? + 1;
    let raw = &raw[from..to];
    if raw
        .iter()
        .any(|l| !l.is_ascii() || l.contains(['\t', '\r']))
        || raw.windows(2).any(|w| blank(&w[0]) && blank(&w[1]))
    {
        return None;
    }
    // An indented table drops the indentation of a table of columns and keeps
    // it in a single column: only unindented tables are read.
    if raw.iter().any(|l| l.starts_with(' ') && !blank(l)) {
        return None;
    }
    let mut lines = Vec::with_capacity(raw.len());
    for l in raw {
        lines.push(if l.is_empty() {
            Line::Separator("")
        } else if l.trim().is_empty() {
            // A line of blanks is not a blank line to rakudo.
            return None;
        } else if is_rule(l)? {
            Line::Separator(l)
        } else if l.ends_with(' ') || l.contains(['-', '=', '_', '+']) {
            // Rule characters inside a row change how rakudo reads the table.
            return None;
        } else {
            Line::Row(l)
        });
    }
    let rows: Vec<&str> = lines
        .iter()
        .filter_map(|l| match l {
            Line::Row(r) => Some(*r),
            Line::Separator(_) => None,
        })
        .collect();
    if rows.is_empty() {
        return None;
    }
    let piped = rows.iter().any(|r| r.contains('|'));
    let mut built = if piped {
        rows.iter()
            .map(|r| pipe_row(r))
            .collect::<Option<Vec<_>>>()?
    } else {
        space_rows(&rows)?
    }
    .into_iter();
    let mut parts: Vec<TablePart> = Vec::new();
    let mut separators = 0;
    for line in &lines {
        parts.push(match line {
            Line::Row(_) => TablePart::Row(built.next()?),
            Line::Separator(text) => {
                separators += 1;
                // A pipe table's separator keeps the line break.
                let text = if piped {
                    format!("{text}\n")
                } else {
                    text.to_string()
                };
                TablePart::Separator(text)
            }
        });
    }
    // A separator with rows on both sides is an interior one.
    let first_row = parts.iter().position(|p| matches!(p, TablePart::Row(_)))?;
    let last_row = parts.iter().rposition(|p| matches!(p, TablePart::Row(_)))?;
    let interior: Vec<usize> = (first_row..last_row)
        .filter(|&n| matches!(parts[n], TablePart::Separator(_)))
        .collect();
    // Rakudo rejects two separators in a row.
    if parts.windows(2).any(|w| {
        matches!(
            (&w[0], &w[1]),
            (TablePart::Separator(_), TablePart::Separator(_))
        )
    }) {
        return None;
    }
    match interior[..] {
        [at] => header(parts, at, separators == 1, piped),
        // Two different separators make the first a header's; two equal ones
        // rule every row off alike.
        [one, two] if separators == 2 && single_rows(&parts) => {
            let same = match (&parts[one], &parts[two]) {
                (TablePart::Separator(a), TablePart::Separator(b)) => a == b,
                _ => return None,
            };
            Some(Table {
                parts,
                header_row: !same,
            })
        }
        [] if separators > 0 && single_rows(&parts) => Some(Table {
            parts,
            header_row: false,
        }),
        [] if separators == 0 => Some(Table {
            parts,
            header_row: false,
        }),
        _ => None,
    }
}

/// Whether no two rows are adjacent: the only shapes measured around several
/// separators.
// Cost: O(n), n = parts.
fn single_rows(parts: &[TablePart]) -> bool {
    parts
        .windows(2)
        .all(|w| !(matches!(w[0], TablePart::Row(_)) && matches!(w[1], TablePart::Row(_))))
}

/// Whether the line is a rule (`None` for a line without letters or digits
/// that is not one of the measured rules).
// Cost: O(l).
fn is_rule(line: &str) -> Option<bool> {
    if line.chars().any(char::is_alphanumeric) {
        return Some(false);
    }
    let rule = line
        .chars()
        .all(|c| matches!(c, '-' | '=' | '_' | '+' | ' '))
        && line.contains(['-', '=', '_']);
    rule.then_some(true)
}

/// The rows before the separator at `at`, the table's one interior separator,
/// joined into one multi-line row (the header) when no other separator exists.
// Cost: O(n), n = size of the rows.
fn header(parts: Vec<TablePart>, at: usize, alone: bool, piped: bool) -> Option<Table> {
    let before = parts[..at]
        .iter()
        .rev()
        .take_while(|p| matches!(p, TablePart::Row(_)))
        .count();
    if before == 1 {
        return Some(Table {
            parts,
            header_row: true,
        });
    }
    // Only whitespace tables with a lone separator were measured with a
    // multi-line header.
    if piped || !alone {
        return None;
    }
    let mut parts = parts.into_iter();
    let mut head: Vec<Row> = Vec::new();
    for part in parts.by_ref().take(at) {
        if let TablePart::Row(row) = part {
            head.push(row);
        }
    }
    let Row {
        dividers,
        offsets,
        mut cells,
    } = head.remove(0);
    for row in head {
        for (cell, more) in cells.iter_mut().zip(row.cells) {
            cell.push('\n');
            cell.push_str(&more);
        }
    }
    let mut out = vec![TablePart::Row(Row {
        dividers,
        offsets,
        cells,
    })];
    out.extend(parts);
    Some(Table {
        parts: out,
        header_row: true,
    })
}

/// The rows of a whitespace table: every row's cells start at the same
/// offsets.
// Cost: O(n), n = size of the rows.
fn space_rows(rows: &[&str]) -> Option<Vec<Row>> {
    let starts = |row: &str| -> Vec<usize> {
        let bytes = row.as_bytes();
        let mut starts = vec![0];
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b' ' {
                let mut j = i;
                while j < bytes.len() && bytes[j] == b' ' {
                    j += 1;
                }
                if j - i >= 2 && j < bytes.len() {
                    starts.push(j);
                }
                i = j;
            } else {
                i += 1;
            }
        }
        starts
    };
    let first = starts(rows[0]);
    if rows.iter().any(|r| starts(r) != first) {
        return None;
    }
    Some(
        rows.iter()
            .map(|row| {
                let mut cells = Vec::with_capacity(first.len());
                for (n, &s) in first.iter().enumerate() {
                    cells.push(match first.get(n + 1) {
                        Some(&next) => row[s..next - 2].to_string(),
                        None => row[s..].to_string(),
                    });
                }
                Row {
                    dividers: None,
                    offsets: first.iter().map(|&s| s as i64).collect(),
                    cells,
                }
            })
            .collect(),
    )
}

/// A row of a pipe table.
// Cost: O(l), l = length of the row.
fn pipe_row(row: &str) -> Option<Row> {
    let bytes = row.as_bytes();
    let mut dividers = Vec::new();
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'|' {
            let before = i == 0 || bytes[i - 1] == b' ';
            let after = i + 1 == bytes.len() || bytes[i + 1] == b' ';
            if !(before && after) {
                return None;
            }
            dividers.push(i);
        }
    }
    let mut segments: Vec<&str> = Vec::new();
    let mut from = 0;
    for &d in &dividers {
        segments.push(&row[from..d]);
        from = d + 1;
    }
    segments.push(&row[from..]);
    if segments.first().is_some_and(|s| s.is_empty()) {
        segments.remove(0);
    }
    if segments.last().is_some_and(|s| s.is_empty()) {
        segments.pop();
    }
    let mut cells = Vec::with_capacity(segments.len());
    for seg in segments {
        let cell = seg.strip_prefix(' ').unwrap_or(seg);
        let cell = cell.strip_suffix(' ').unwrap_or(cell);
        if cell.trim().is_empty() {
            return None;
        }
        cells.push(cell.to_string());
    }
    // A lone divider (`| a`) is read as part of a cell by rakudo.
    if cells.len() < 2 {
        return None;
    }
    Some(Row {
        dividers: Some("|".repeat(dividers.len())),
        offsets: dividers.iter().map(|&d| d as i64 + 2).collect(),
        cells,
    })
}
