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
//!   cells are the text between the dividers minus the blank next to each
//!   divider.
//! - An indented table without a blank line is read without the indentation
//!   its lines share; an indented one with a blank line is not read.
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
    // An indented table of consecutive lines is read without the indentation
    // all its lines share (rule lines included): a line indented further keeps
    // the rest. One with a blank line inside keeps its indentation, in its
    // first cells.
    let margin = raw
        .iter()
        .filter(|l| !blank(l))
        .map(|l| l.len() - l.trim_start_matches(' ').len())
        .min()?;
    let margin = if raw.iter().any(blank) { 0 } else { margin };
    let dedented: Vec<&str> = raw
        .iter()
        .map(|l| if blank(l) { *l } else { &l[margin..] })
        .collect();
    let raw = &dedented[..];
    // A table with a `|` divider also divides at a `+` with a blank on both
    // sides (`Zebra  +   4  + Cookies` under an `Animal | Legs | Eats`).
    let piped = raw
        .iter()
        .any(|l| l.contains('|') && !is_rule(l).unwrap_or(true));
    let mut lines = Vec::with_capacity(raw.len());
    for l in raw {
        lines.push(if l.is_empty() {
            Line::Separator("")
        } else if l.trim().is_empty() {
            // A line of blanks is not a blank line to rakudo.
            return None;
        } else if is_rule(l)? {
            Line::Separator(l)
        } else if l.ends_with(' ')
            || l.contains(['-', '=', '_'])
            || (l.contains('+') && !(piped && plus_dividers_only(l)))
        {
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
        // Between several separators each run of rows is one multi-line row.
        // A first separator unlike the second makes the first row a header's;
        // equal ones rule every row off alike.
        [one, two, ..] if separators == interior.len() && (!piped || single_rows(&parts)) => {
            let same = match (&parts[one], &parts[two]) {
                (TablePart::Separator(a), TablePart::Separator(b)) => a == b,
                _ => return None,
            };
            Some(Table {
                parts: merge_groups(parts),
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

/// `parts` with each run of adjacent rows joined into one multi-line row.
// Cost: O(n), n = size of the rows.
fn merge_groups(parts: Vec<TablePart>) -> Vec<TablePart> {
    let mut out: Vec<TablePart> = Vec::with_capacity(parts.len());
    for part in parts {
        match (part, out.last_mut()) {
            (TablePart::Row(row), Some(TablePart::Row(above))) => {
                for (n, cell) in above.cells.iter_mut().enumerate() {
                    cell.push('\n');
                    cell.push_str(row.cells.get(n).map_or("", String::as_str));
                }
            }
            (part, _) => out.push(part),
        }
    }
    out
}

/// Whether no two rows are adjacent: the only shapes of a pipe table
/// measured around several separators.
// Cost: O(n), n = parts.
fn single_rows(parts: &[TablePart]) -> bool {
    parts
        .windows(2)
        .all(|w| !(matches!(w[0], TablePart::Row(_)) && matches!(w[1], TablePart::Row(_))))
}

/// Whether every `+` of `line` is a divider: a blank or the line's edge on
/// both sides.
// Cost: O(l), l = length of the line.
fn plus_dividers_only(line: &str) -> bool {
    let bytes = line.as_bytes();
    bytes.iter().enumerate().all(|(i, &b)| {
        b != b'+'
            || ((i == 0 || bytes[i - 1] == b' ') && (i + 1 == bytes.len() || bytes[i + 1] == b' '))
    })
}

/// Whether the line is a rule (`None` for a line without letters or digits
/// that is not one of the measured rules). A rule crosses a pipe table's
/// dividers with `+` or `|` (`--+--`, `==|==`).
// Cost: O(l).
fn is_rule(line: &str) -> Option<bool> {
    if line.chars().any(char::is_alphanumeric) {
        return Some(false);
    }
    let rule = line
        .chars()
        .all(|c| matches!(c, '-' | '=' | '_' | '+' | '|' | ' '))
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
    // Only a lone separator was measured with a multi-line header.
    if !alone {
        return None;
    }
    let mut parts = parts.into_iter();
    let mut head: Vec<Row> = Vec::new();
    for part in parts.by_ref().take(at) {
        if let TablePart::Row(row) = part {
            head.push(row);
        }
    }
    // The lines of a pipe header share their dividers (`Superhero | Secret |`
    // over `          | Identity | Superpower`).
    if piped
        && head
            .iter()
            .any(|row| row.dividers != head[0].dividers || row.offsets != head[0].offsets)
    {
        return None;
    }
    let Row {
        dividers,
        offsets,
        mut cells,
    } = head.remove(0);
    for (line, row) in head.into_iter().enumerate() {
        // A line with more cells starts the extra ones empty on the lines above.
        while cells.len() < row.cells.len() {
            cells.push("\n".repeat(line));
        }
        for (n, cell) in cells.iter_mut().enumerate() {
            cell.push('\n');
            cell.push_str(row.cells.get(n).map_or("", String::as_str));
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

/// The rows of a whitespace table. A column starts after a run of two or
/// more blanks in any row (not the run a line is indented by); of two starts
/// one column apart the later one is the column's. Every row is cut at the
/// same offsets, a cell running up to two characters before the next one.
// Cost: O(n log n), n = size of the rows.
fn space_rows(rows: &[&str]) -> Option<Vec<Row>> {
    let starts = |row: &str| -> Vec<usize> {
        let bytes = row.as_bytes();
        let mut starts = Vec::new();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b' ' {
                let mut j = i;
                while j < bytes.len() && bytes[j] == b' ' {
                    j += 1;
                }
                if i > 0 && j - i >= 2 && j < bytes.len() {
                    starts.push(j);
                }
                i = j;
            } else {
                i += 1;
            }
        }
        starts
    };
    let mut all: Vec<usize> = rows.iter().flat_map(|r| starts(r)).collect();
    all.sort_unstable();
    all.dedup();
    // `aa  bb` over `ddd  ee`: of two starts one column apart the later one
    // is the column's.
    let kept: Vec<usize> = all
        .iter()
        .copied()
        .filter(|s| all.binary_search(&(s + 1)).is_err())
        .collect();
    let mut offsets: Vec<usize> = vec![0];
    for start in kept {
        // Starts two apart were not measured.
        if start - offsets.last().copied().unwrap_or(0) <= 2 {
            return None;
        }
        offsets.push(start);
    }
    let cell = |row: &str, from: usize, to: Option<usize>| -> String {
        let to = to.map_or(row.len(), |t| t.min(row.len()));
        if from >= to {
            String::new()
        } else {
            row[from..to].to_string()
        }
    };
    Some(
        rows.iter()
            .map(|row| {
                let cells = offsets
                    .iter()
                    .enumerate()
                    .map(|(n, &s)| cell(row, s, offsets.get(n + 1).map(|&next| next - 2)))
                    .collect();
                Row {
                    dividers: None,
                    offsets: offsets.iter().map(|&s| s as i64).collect(),
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
        if b == b'|' || b == b'+' {
            let before = i == 0 || bytes[i - 1] == b' ';
            let after = i + 1 == bytes.len() || bytes[i + 1] == b' ';
            if !(before && after) {
                return None;
            }
            dividers.push(i);
        }
    }
    // Each segment, and whether a divider precedes it.
    let mut segments: Vec<(&str, bool)> = Vec::new();
    let mut from = 0;
    for &d in &dividers {
        segments.push((&row[from..d], from > 0));
        from = d + 1;
    }
    segments.push((&row[from..], from > 0));
    if segments.first().is_some_and(|(s, _)| s.is_empty()) {
        segments.remove(0);
    }
    if segments.last().is_some_and(|(s, _)| s.is_empty()) {
        segments.pop();
    }
    let mut cells = Vec::with_capacity(segments.len());
    for (seg, after_divider) in segments {
        // The blank next to a divider is the divider's; a row indented past
        // the table's margin keeps its leading blanks.
        let cell = if after_divider {
            seg.strip_prefix(' ').unwrap_or(seg)
        } else {
            seg
        };
        let cell = cell.strip_suffix(' ').unwrap_or(cell);
        cells.push(cell.to_string());
    }
    // A lone divider (`| a`) is read as part of a cell by rakudo; one that
    // ends the line (`a |`, `| a |`) divides a row of one cell.
    if cells.len() < 2 && dividers.last() != Some(&(bytes.len() - 1)) {
        return None;
    }
    Some(Row {
        dividers: Some(dividers.iter().map(|&d| char::from(bytes[d])).collect()),
        offsets: dividers.iter().map(|&d| d as i64 + 2).collect(),
        cells,
    })
}
