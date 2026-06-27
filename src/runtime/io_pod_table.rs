use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn make_pod_table_full(
        headers: Vec<Vec<String>>,
        rows: Vec<Vec<String>>,
        config: HashMap<String, Value>,
    ) -> Value {
        let mut attrs = HashMap::new();
        let contents = rows
            .into_iter()
            .map(|row| Value::array(row.into_iter().map(Value::str).collect::<Vec<_>>()))
            .collect::<Vec<_>>();
        attrs.insert("contents".to_string(), Value::array(contents));
        let header_vals = if headers.len() == 1 {
            headers[0].iter().map(|s| Value::str(s.clone())).collect()
        } else {
            Vec::new()
        };
        attrs.insert("headers".to_string(), Value::array(header_vals));
        // Set caption from config if available, otherwise empty string
        let caption = config
            .get("caption")
            .cloned()
            .unwrap_or_else(|| Value::str(String::new()));
        attrs.insert("caption".to_string(), caption);
        attrs.insert("config".to_string(), Value::hash(config));
        Value::make_instance(Symbol::intern("Pod::Block::Table"), attrs)
    }

    /// Strip Z<...> formatting codes from a line.
    fn strip_pod_z_codes(line: &str) -> String {
        let mut result = String::new();
        let mut chars = line.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == 'Z' && chars.peek() == Some(&'<') {
                chars.next();
                let mut depth = 1;
                for c in chars.by_ref() {
                    if c == '<' {
                        depth += 1;
                    } else if c == '>' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                }
            } else {
                result.push(ch);
            }
        }
        result
    }

    /// Check if a line is a Pod table separator (made of `-`, `=`, `+`, `|`, `:`, space, tab).
    pub(crate) fn is_pod_table_separator(line: &str) -> bool {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            return false;
        }
        let has_dash_or_eq = trimmed.chars().any(|c| matches!(c, '-' | '='));
        has_dash_or_eq
            && trimmed
                .chars()
                .all(|c| matches!(c, '-' | '=' | '+' | '|' | ':' | ' ' | '\t'))
    }

    /// Check if a separator line is a header separator (contains `=` but no `-`).
    fn is_header_separator(line: &str) -> bool {
        let trimmed = line.trim();
        trimmed.contains('=')
            && trimmed
                .chars()
                .all(|c| matches!(c, '=' | '+' | '|' | ':' | ' ' | '\t'))
    }

    /// Check if a line is a grid border like `+-----+----+---+`.
    fn is_grid_border(line: &str) -> bool {
        let trimmed = line.trim();
        !trimmed.is_empty()
            && trimmed.starts_with('+')
            && trimmed.ends_with('+')
            && trimmed.chars().all(|c| matches!(c, '+' | '-' | '=' | ' '))
    }

    /// Return true if a character is a breaking horizontal whitespace that
    /// should be collapsed during cell normalization.  Non-breaking spaces
    /// (U+00A0, U+202F, U+2060, U+FEFF) are intentionally excluded so that
    /// they survive pod table processing unchanged (GH #1852).
    fn is_breaking_hs(ch: char) -> bool {
        matches!(
            ch,
            '\t'          // U+0009 CHARACTER TABULATION
            | ' '         // U+0020 SPACE
            | '\u{1680}'  // OGHAM SPACE MARK
            | '\u{180E}'  // MONGOLIAN VOWEL SEPARATOR
            | '\u{2000}'  // EN QUAD
            | '\u{2001}'  // EM QUAD
            | '\u{2002}'  // EN SPACE
            | '\u{2003}'  // EM SPACE
            | '\u{2004}'  // THREE-PER-EM SPACE
            | '\u{2005}'  // FOUR-PER-EM SPACE
            | '\u{2006}'  // SIX-PER-EM SPACE
            | '\u{2007}'  // FIGURE SPACE
            | '\u{2008}'  // PUNCTUATION SPACE
            | '\u{2009}'  // THIN SPACE
            | '\u{200A}'  // HAIR SPACE
            | '\u{205F}'  // MEDIUM MATHEMATICAL SPACE
            | '\u{3000}' // IDEOGRAPHIC SPACE
        )
    }

    /// Normalize whitespace in a cell: collapse runs of breaking horizontal
    /// whitespace to a single ASCII space.  Non-breaking spaces within the
    /// cell content are preserved, but edge whitespace (including NBSP used
    /// as padding) is trimmed using Rust's standard `trim()`.
    fn normalize_cell(s: &str) -> String {
        // Trim all Unicode whitespace (including NBSP) from edges -- NBSP at
        // edges acts as padding and should be stripped, just like regular
        // spaces.  Interior NBSP is preserved by `is_breaking_hs` below.
        let trimmed = s.trim();
        let mut result = String::new();
        let mut prev_space = false;
        for ch in trimmed.chars() {
            if Self::is_breaking_hs(ch) {
                if !prev_space && !result.is_empty() {
                    result.push(' ');
                }
                prev_space = true;
            } else {
                prev_space = false;
                result.push(ch);
            }
        }
        if result.ends_with(' ') {
            result.pop();
        }
        result
    }

    /// Split a pipe-separated table line into cells, respecting `\|` and `\+` escapes.
    fn split_pipe_cells(line: &str) -> Vec<String> {
        let mut cells = Vec::new();
        let mut current = String::new();
        let mut chars = line.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == '\\' {
                if let Some(&next) = chars.peek()
                    && (next == '|' || next == '+')
                {
                    current.push(next);
                    chars.next();
                    continue;
                }
                current.push(ch);
            } else if ch == '|' || ch == '+' {
                cells.push(Self::normalize_cell(&current));
                current = String::new();
            } else {
                current.push(ch);
            }
        }
        cells.push(Self::normalize_cell(&current));
        cells
    }

    /// Determine if table lines use visible `|` or `+` column separators.
    fn table_uses_pipe_separator(lines: &[&str]) -> bool {
        for line in lines {
            let trimmed = line.trim();
            if trimmed.is_empty() || Self::is_pod_table_separator(trimmed) {
                continue;
            }
            let mut chars = trimmed.chars().peekable();
            while let Some(ch) = chars.next() {
                if ch == '\\' {
                    chars.next();
                } else if ch == '|' {
                    return true;
                }
            }
        }
        false
    }

    /// Detect column positions from whitespace-aligned data lines.
    fn detect_ws_columns(data_lines: &[&str], min_indent: usize) -> Vec<usize> {
        let mut all_col_starts: std::collections::BTreeSet<usize> =
            std::collections::BTreeSet::new();
        for line in data_lines {
            if line.trim().is_empty() {
                continue;
            }
            let adjusted = if line.len() > min_indent {
                &line[min_indent..]
            } else {
                line.trim_start()
            };
            let mut in_space = true;
            let mut space_count = 0;
            for (i, ch) in adjusted.chars().enumerate() {
                if ch == ' ' || ch == '\t' {
                    space_count += 1;
                    in_space = true;
                } else {
                    if in_space && (i == 0 || space_count >= 2) {
                        all_col_starts.insert(i);
                    }
                    in_space = false;
                    space_count = 0;
                }
            }
        }
        all_col_starts.into_iter().collect()
    }

    /// Extract cells from lines using pre-determined column positions.
    fn extract_ws_cells(
        data_lines: &[&str],
        col_positions: &[usize],
        min_indent: usize,
    ) -> Vec<Vec<String>> {
        let mut rows = Vec::new();
        for line in data_lines {
            if line.trim().is_empty() {
                continue;
            }
            let adjusted = if line.len() > min_indent {
                &line[min_indent..]
            } else {
                line.trim_start()
            };
            let mut cells = Vec::new();
            for (ci, &col_start) in col_positions.iter().enumerate() {
                let end = if ci + 1 < col_positions.len() {
                    col_positions[ci + 1]
                } else {
                    adjusted.len()
                };
                let cell = if col_start < adjusted.len() {
                    let actual_end = end.min(adjusted.len());
                    adjusted[col_start..actual_end].trim().to_string()
                } else {
                    String::new()
                };
                cells.push(cell);
            }
            rows.push(cells);
        }
        rows
    }

    /// Merge multiple rows within a separator-delimited group into one row.
    fn merge_table_group(group: &[Vec<String>]) -> Vec<String> {
        if group.len() == 1 {
            return group[0].clone();
        }
        let max_cols = group.iter().map(|r| r.len()).max().unwrap_or(0);
        let mut merged = vec![String::new(); max_cols];
        for row in group {
            for (j, cell) in row.iter().enumerate() {
                if j < max_cols {
                    if !merged[j].is_empty() && !cell.is_empty() {
                        merged[j].push(' ');
                    }
                    merged[j].push_str(cell);
                }
            }
        }
        merged
    }

    /// Pad all rows (headers and content) to have the same number of columns.
    fn pad_table_rows(headers: &mut [Vec<String>], rows: &mut [Vec<String>]) {
        let max_cols = headers
            .iter()
            .chain(rows.iter())
            .map(|r| r.len())
            .max()
            .unwrap_or(0);
        for row in headers.iter_mut() {
            while row.len() < max_cols {
                row.push(String::new());
            }
        }
        for row in rows.iter_mut() {
            while row.len() < max_cols {
                row.push(String::new());
            }
        }
    }

    /// Parse Pod table lines into (headers, content_rows).
    pub(crate) fn parse_pod_table_lines(
        raw_lines: &[&str],
    ) -> (Vec<Vec<String>>, Vec<Vec<String>>) {
        // Strip Z<> codes
        let stripped: Vec<String> = raw_lines
            .iter()
            .map(|l| Self::strip_pod_z_codes(l))
            .collect();
        let line_refs: Vec<&str> = stripped.iter().map(|s| s.as_str()).collect();

        // Trim leading/trailing blank lines
        let mut start = 0;
        let mut end = line_refs.len();
        while start < end && line_refs[start].trim().is_empty() {
            start += 1;
        }
        while end > start && line_refs[end - 1].trim().is_empty() {
            end -= 1;
        }
        let lines = &line_refs[start..end];
        if lines.is_empty() {
            return (Vec::new(), Vec::new());
        }

        // Detect grid table
        if Self::is_grid_border(lines[0].trim()) {
            return Self::parse_grid_table(lines);
        }

        // Detect column separator type
        let use_pipe = Self::table_uses_pipe_separator(lines);

        // Find all separator lines
        let mut all_sep_indices = Vec::new();
        let mut eq_sep_indices = Vec::new();
        let mut dash_sep_indices = Vec::new();

        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            if Self::is_pod_table_separator(trimmed) {
                all_sep_indices.push(i);
                if Self::is_header_separator(trimmed) {
                    eq_sep_indices.push(i);
                } else {
                    dash_sep_indices.push(i);
                }
            }
        }

        // Determine header separator:
        // - One `=` sep with `-` seps: `=` is header sep
        // - Exactly one separator total: it is the header separator
        // - Multiple separators of same type: no header
        let header_sep_idx: Option<usize> =
            if eq_sep_indices.len() == 1 && !dash_sep_indices.is_empty() {
                Some(eq_sep_indices[0])
            } else if all_sep_indices.len() == 1 && all_sep_indices[0] > 0 {
                Some(all_sep_indices[0])
            } else {
                None
            };

        // For whitespace-separated tables, use column-position alignment
        if !use_pipe {
            return Self::parse_ws_table(lines, &all_sep_indices, header_sep_idx);
        }

        // For pipe-separated tables, collect data grouped by separators
        let mut groups: Vec<Vec<Vec<String>>> = Vec::new();
        let mut current_group: Vec<Vec<String>> = Vec::new();
        let mut header_groups: Vec<Vec<Vec<String>>> = Vec::new();
        let mut past_header = header_sep_idx.is_none();

        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            if Self::is_pod_table_separator(trimmed) {
                if !current_group.is_empty() {
                    if past_header {
                        groups.push(current_group.clone());
                    } else {
                        header_groups.push(current_group.clone());
                    }
                    current_group.clear();
                }
                if Some(i) == header_sep_idx {
                    past_header = true;
                }
                continue;
            }
            let cells = Self::split_pipe_cells(trimmed);
            current_group.push(cells);
        }
        if !current_group.is_empty() {
            if past_header {
                groups.push(current_group);
            } else {
                header_groups.push(current_group);
            }
        }

        // Merge rows within groups
        let mut headers: Vec<Vec<String>> = Vec::new();
        let mut all_rows: Vec<Vec<String>> = Vec::new();

        for group in &header_groups {
            headers.push(Self::merge_table_group(group));
        }
        // Count separators that act as row separators (not the header separator)
        let content_sep_count = all_sep_indices
            .iter()
            .filter(|&&idx| Some(idx) != header_sep_idx)
            .count();
        if content_sep_count == 0 {
            // No row separators - each data line is its own row
            for group in &groups {
                for row in group {
                    all_rows.push(row.clone());
                }
            }
        } else {
            for group in &groups {
                all_rows.push(Self::merge_table_group(group));
            }
        }

        Self::pad_table_rows(&mut headers, &mut all_rows);
        (headers, all_rows)
    }

    /// Parse a whitespace-separated table using column-position alignment.
    fn parse_ws_table(
        lines: &[&str],
        all_sep_indices: &[usize],
        header_sep_idx: Option<usize>,
    ) -> (Vec<Vec<String>>, Vec<Vec<String>>) {
        // Collect raw data line groups separated by separator lines
        let mut header_raw: Vec<&str> = Vec::new();
        let mut content_groups: Vec<Vec<&str>> = Vec::new();
        let mut current_group: Vec<&str> = Vec::new();
        let mut past_header = header_sep_idx.is_none();

        // Track blank line indices for grouping
        let mut blank_indices = Vec::new();
        for (i, line) in lines.iter().enumerate() {
            if line.trim().is_empty() {
                blank_indices.push(i);
            }
        }

        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                // Blank lines act as group separators
                if !current_group.is_empty() {
                    if past_header {
                        content_groups.push(current_group.clone());
                    } else {
                        header_raw = current_group.clone();
                    }
                    current_group.clear();
                }
                continue;
            }
            if all_sep_indices.contains(&i) {
                if !current_group.is_empty() {
                    if past_header {
                        content_groups.push(current_group.clone());
                    } else {
                        header_raw = current_group.clone();
                    }
                    current_group.clear();
                }
                if Some(i) == header_sep_idx {
                    past_header = true;
                }
                continue;
            }
            current_group.push(line);
        }
        if !current_group.is_empty() {
            if past_header {
                content_groups.push(current_group);
            } else {
                header_raw = current_group;
            }
        }

        // Collect ALL data lines for global column position detection
        let mut all_data_lines: Vec<&str> = Vec::new();
        all_data_lines.extend_from_slice(&header_raw);
        for group in &content_groups {
            all_data_lines.extend_from_slice(group);
        }

        let min_indent = all_data_lines
            .iter()
            .filter(|l| !l.trim().is_empty())
            .map(|l| l.len() - l.trim_start().len())
            .min()
            .unwrap_or(0);
        let col_positions = Self::detect_ws_columns(&all_data_lines, min_indent);

        if col_positions.is_empty() {
            return (Vec::new(), Vec::new());
        }

        // Parse headers with global column positions
        let mut headers = if !header_raw.is_empty() && header_sep_idx.is_some() {
            let parsed = Self::extract_ws_cells(&header_raw, &col_positions, min_indent);
            if parsed.len() > 1 {
                vec![Self::merge_table_group(&parsed)]
            } else {
                parsed
            }
        } else {
            Vec::new()
        };

        // Parse content groups with global column positions, merge within each
        let content_sep_count = all_sep_indices
            .iter()
            .filter(|&&idx| Some(idx) != header_sep_idx)
            .count();
        // Groups should be merged if there are separator or blank-line boundaries
        let has_group_separators =
            (content_sep_count > 0 || !blank_indices.is_empty()) && content_groups.len() > 1;
        let mut all_rows = Vec::new();
        if has_group_separators {
            for group in &content_groups {
                let parsed = Self::extract_ws_cells(group, &col_positions, min_indent);
                all_rows.push(Self::merge_table_group(&parsed));
            }
        } else {
            // No row separators - each line is its own row
            for group in &content_groups {
                let parsed = Self::extract_ws_cells(group, &col_positions, min_indent);
                for row in parsed {
                    all_rows.push(row);
                }
            }
        }

        Self::pad_table_rows(&mut headers, &mut all_rows);
        (headers, all_rows)
    }

    /// Parse a grid-style table (with `+---+` borders and `|` cell delimiters).
    fn parse_grid_table(lines: &[&str]) -> (Vec<Vec<String>>, Vec<Vec<String>>) {
        let mut header_rows: Vec<Vec<String>> = Vec::new();
        let mut content_rows: Vec<Vec<String>> = Vec::new();
        let mut found_header_sep = false;

        for line in lines {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            if Self::is_grid_border(trimmed) {
                if trimmed.contains('=') {
                    found_header_sep = true;
                }
                continue;
            }
            let inner = if trimmed.starts_with('|') && trimmed.ends_with('|') {
                &trimmed[1..trimmed.len() - 1]
            } else {
                trimmed
            };
            let cells: Vec<String> = inner.split('|').map(Self::normalize_cell).collect();
            if found_header_sep {
                content_rows.push(cells);
            } else {
                header_rows.push(cells);
            }
        }

        if !found_header_sep && !header_rows.is_empty() {
            content_rows = header_rows;
            header_rows = Vec::new();
            if !content_rows.is_empty() {
                header_rows = vec![content_rows.remove(0)];
            }
        }

        Self::pad_table_rows(&mut header_rows, &mut content_rows);
        (header_rows, content_rows)
    }

    /// Collect table rows for abbreviated `=table` directive with headers.
    pub(crate) fn collect_table_rows_with_headers(
        lines: &[&str],
        mut idx: usize,
    ) -> (Vec<Vec<String>>, Vec<Vec<String>>, usize) {
        let mut table_lines: Vec<&str> = Vec::new();
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() {
                break;
            }
            // Stop at Pod directives but NOT at table separator lines (=====, -----)
            if trimmed.starts_with('=') && !Self::is_pod_table_separator(trimmed) {
                break;
            }
            table_lines.push(lines[idx]);
            idx += 1;
        }
        let (headers, rows) = Self::parse_pod_table_lines(&table_lines);
        (headers, rows, idx)
    }
}
