use super::*;

/// Where in a source the Pod blocks live, as byte ranges, recorded with a
/// precompiled module so a hit rebuilds `$=pod` from those ranges alone
/// instead of scanning every line (ADR-12026 §2.4).
pub(crate) type PodRanges = Vec<(usize, usize)>;

impl Interpreter {
    /// Build `$=pod` from `input`'s Pod blocks.
    pub(super) fn collect_pod_blocks(&mut self, input: &str) -> Result<(), RuntimeError> {
        Self::clear_pod_config_error();
        let (entries, _) = Self::scan_pod_source(input);
        self.finish_pod_blocks(entries)
    }

    /// Build `$=pod` from just the Pod `ranges` of a source (see
    /// [`Interpreter::pod_ranges_of`]); the rest of the source holds no Pod.
    // Cost: O(r), r = total size of the ranges.
    pub(super) fn collect_pod_blocks_in_ranges(
        &mut self,
        input: &str,
        ranges: &PodRanges,
    ) -> Result<(), RuntimeError> {
        Self::clear_pod_config_error();
        let mut text = String::new();
        for &(start, end) in ranges {
            match input.get(start..end) {
                Some(piece) => text.push_str(piece),
                // A stale range: scan the whole source instead.
                None => return self.collect_pod_blocks(input),
            }
            // A blank line keeps two separate ranges from reading as one block.
            text.push_str("\n\n");
        }
        let lines: Vec<&str> = text.lines().collect();
        let (entries, _) = Self::scan_pod_lines(&lines);
        self.finish_pod_blocks(entries)
    }

    /// The byte ranges of `input` that hold its Pod blocks, or none when
    /// they cannot be isolated (a heredoc body inside one) or the Pod is
    /// malformed.
    // Cost: O(n), n = size of the source.
    pub(crate) fn pod_ranges_of(input: &str) -> Option<PodRanges> {
        Self::clear_pod_config_error();
        let (_, ranges) = Self::scan_pod_source(input);
        Self::take_pod_config_error().is_none().then_some(ranges?)
    }

    fn finish_pod_blocks(&mut self, entries: Vec<Value>) -> Result<(), RuntimeError> {
        self.env
            .insert("=pod".to_string(), Value::real_array(entries));
        if let Some(err) = Self::take_pod_config_error() {
            return Err(err);
        }
        Ok(())
    }

    /// The Pod entries of a whole source, and the byte ranges they came from.
    fn scan_pod_source(input: &str) -> (Vec<Value>, Option<PodRanges>) {
        let raw_lines: Vec<&str> = input.lines().collect();
        let in_heredoc = Self::heredoc_body_lines(&raw_lines);
        let lines: Vec<&str> = raw_lines
            .iter()
            .zip(&in_heredoc)
            .map(|(line, masked)| if *masked { "" } else { *line })
            .collect();
        let (entries, spans) = Self::scan_pod_lines(&lines);
        // Byte offset of each line's start, as `str::lines` splits them.
        let mut starts = Vec::with_capacity(raw_lines.len() + 1);
        let mut offset = 0usize;
        for piece in input.split_inclusive('\n') {
            starts.push(offset);
            offset += piece.len();
        }
        starts.push(offset);
        let mut ranges: PodRanges = Vec::new();
        for (first, past) in spans {
            // A masked line cannot be rebuilt from the ranges alone.
            if in_heredoc[first..past].iter().any(|masked| *masked) {
                return (entries, None);
            }
            let (start, end) = (starts[first], starts[past]);
            match ranges.last_mut() {
                Some(last) if last.1 >= start => last.1 = last.1.max(end),
                _ => ranges.push((start, end)),
            }
        }
        (entries, Some(ranges))
    }

    /// The Pod entries of `lines`, and the line spans `[first, past)` each
    /// entry was read from (plus the one line after it, which the reader
    /// may have looked at to find the end).
    fn scan_pod_lines(lines: &[&str]) -> (Vec<Value>, Vec<(usize, usize)>) {
        let mut entries = Vec::new();
        let mut spans: Vec<(usize, usize)> = Vec::new();
        let (mut mark_len, mut mark_start) = (0usize, 0usize);
        let mut idx = 0usize;
        while idx < lines.len() {
            if entries.len() > mark_len {
                spans.push((mark_start, (idx + 1).min(lines.len())));
            }
            (mark_len, mark_start) = (entries.len(), idx);
            let trimmed = lines[idx].trim_start();
            if let Some((directive, rest)) = Self::active_pod_directive(lines[idx], None) {
                if directive == "end" {
                    idx += 1;
                    continue;
                }
                if directive == "comment" {
                    let (comment, next_idx) =
                        Self::collect_pod_comment_paragraph(&lines, idx, rest);
                    entries.push(comment);
                    idx = next_idx;
                    continue;
                }
                if directive == "config" {
                    let type_name = rest.split_whitespace().next().unwrap_or_default();
                    let after_type = rest
                        .strip_prefix(type_name)
                        .map(str::trim_start)
                        .unwrap_or_default();
                    let (cfg, _) = Self::parse_pod_config(after_type);
                    entries.push(Self::make_pod_config(type_name, cfg));
                    idx += 1;
                    continue;
                }
                if directive == "table" {
                    let (numbered, rest_after) = Self::extract_numbered_alias(rest);
                    let (mut config, _) = Self::parse_pod_config(rest_after);
                    if numbered {
                        config.insert("numbered".to_string(), Value::TRUE);
                    }
                    let (headers, rows, next_idx) =
                        Self::collect_table_rows_with_headers(&lines, idx + 1);
                    if !rows.is_empty() || !headers.is_empty() || numbered || !config.is_empty() {
                        entries.push(Self::make_pod_table_full(headers, rows, config));
                    }
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "for" {
                    let target = rest.split_whitespace().next().unwrap_or_default();
                    if target.is_empty() {
                        idx += 1;
                        continue;
                    }
                    let inline = rest
                        .strip_prefix(target)
                        .map(str::trim_start)
                        .unwrap_or_default();
                    if target == "comment" {
                        let (comment, next_idx) =
                            Self::collect_pod_comment_paragraph(&lines, idx, inline);
                        entries.push(comment);
                        idx = next_idx;
                        continue;
                    }
                    if target == "defn" {
                        let (config, leftover) = Self::parse_pod_config(inline);
                        let (defn, next_idx) =
                            Self::build_pod_defn_paragraph(&lines, idx + 1, leftover, config, None);
                        entries.push(defn);
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "table" {
                        let (numbered, inline_after) = Self::extract_numbered_alias(inline);
                        let (mut config, _) = Self::parse_pod_config(inline_after);
                        if numbered {
                            config.insert("numbered".to_string(), Value::TRUE);
                        }
                        let (headers, rows, next_idx) =
                            Self::collect_table_rows_with_headers(&lines, idx + 1);
                        entries.push(Self::make_pod_table_full(headers, rows, config));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "code" {
                        // `=for code` is a verbatim code paragraph, not a named
                        // block: its lines must not be re-wrapped into a Para.
                        let (numbered, inline_after) = Self::extract_numbered_alias(inline);
                        let (mut config, leftover) = Self::parse_pod_config(inline_after);
                        if numbered {
                            config.insert("numbered".to_string(), Value::TRUE);
                        }
                        let (code_lines, next_idx) =
                            Self::collect_pod_code_paragraph(&lines, idx + 1, leftover, None);
                        entries.push(Self::make_pod_code_block(code_lines, config));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    let (numbered, inline_after) = Self::extract_numbered_alias(inline);
                    let (mut config, leftover) = Self::parse_pod_config(inline_after);
                    if numbered {
                        config.insert("numbered".to_string(), Value::TRUE);
                    }
                    let mut cont_idx = idx + 1;
                    while cont_idx < lines.len() {
                        let cont = lines[cont_idx].trim_start();
                        if cont.starts_with("= ") || cont.starts_with("=\t") {
                            let cont_str = cont[1..].trim_start();
                            let (more, _) = Self::parse_pod_config(cont_str);
                            config.extend(more);
                            cont_idx += 1;
                        } else {
                            break;
                        }
                    }
                    let (para, next_idx) =
                        Self::collect_pod_para_with_inline(&lines, cont_idx, leftover, None);
                    let mut contents = Vec::new();
                    if let Some(para) = para {
                        contents.push(para);
                    }
                    entries.push(Self::make_pod_block_for_target(target, contents, config));
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "begin" {
                    let target = rest.split_whitespace().next().unwrap_or_default();
                    if target.is_empty() {
                        idx += 1;
                        continue;
                    }
                    if target == "comment" {
                        idx += 1;
                        let mut raw = String::new();
                        while idx < lines.len() {
                            if let Some((end_directive, end_rest)) =
                                Self::active_pod_directive(lines[idx], Some("comment"))
                                && end_directive == "end"
                                && end_rest.split_whitespace().next().unwrap_or_default()
                                    == "comment"
                            {
                                idx += 1;
                                break;
                            }
                            raw.push_str(lines[idx]);
                            raw.push('\n');
                            idx += 1;
                        }
                        entries.push(Self::make_pod_comment(raw));
                        continue;
                    }
                    if target == "defn" {
                        let after_target = rest.strip_prefix(target).unwrap_or("");
                        let (config, _) = Self::parse_pod_config(after_target);
                        let (defn, next_idx) =
                            Self::build_pod_defn_delimited(&lines, idx + 1, config);
                        entries.push(defn);
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "code" {
                        let after_target = rest.strip_prefix(target).unwrap_or("");
                        let (code_config, _) = Self::parse_pod_config(after_target);
                        idx += 1;
                        let mut code_lines: Vec<&str> = Vec::new();
                        while idx < lines.len() {
                            if let Some((ed, er)) =
                                Self::active_pod_directive(lines[idx], Some("code"))
                                && ed == "end"
                                && er.split_whitespace().next().unwrap_or_default() == "code"
                            {
                                idx += 1;
                                break;
                            }
                            code_lines.push(lines[idx]);
                            idx += 1;
                        }
                        entries.push(Self::make_pod_code_block(
                            Self::dedent_pod_code_lines(&code_lines),
                            code_config,
                        ));
                        continue;
                    }
                    if target == "table" {
                        let after_target = rest.strip_prefix(target).unwrap_or("");
                        let (mut tbl_config, _) = Self::parse_pod_config(after_target);
                        idx += 1;
                        // Handle config continuation lines (= :key(value))
                        while idx < lines.len() {
                            let cont = lines[idx].trim_start();
                            if (cont.starts_with("= ") || cont.starts_with("=\t"))
                                && !Self::is_pod_table_separator(cont)
                            {
                                let cont_str = cont[1..].trim_start();
                                let (more, _) = Self::parse_pod_config(cont_str);
                                tbl_config.extend(more);
                                idx += 1;
                            } else {
                                break;
                            }
                        }
                        let mut table_lines: Vec<&str> = Vec::new();
                        while idx < lines.len() {
                            if let Some((ed, er)) =
                                Self::active_pod_directive(lines[idx], Some("table"))
                                && ed == "end"
                                && er.split_whitespace().next().unwrap_or_default() == "table"
                            {
                                idx += 1;
                                break;
                            }
                            table_lines.push(lines[idx]);
                            idx += 1;
                        }
                        let (headers, rows) = Self::parse_pod_table_lines(&table_lines);
                        entries.push(Self::make_pod_table_full(headers, rows, tbl_config));
                        continue;
                    }
                    let (contents, next_idx) = Self::collect_pod_entries(
                        &lines,
                        idx + 1,
                        Some(target),
                        Self::pod_line_indent(lines[idx]),
                    );
                    let after_target = rest.strip_prefix(target).unwrap_or("");
                    let (config, _) = Self::parse_pod_config(after_target);
                    entries.push(Self::make_pod_block_for_target(target, contents, config));
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if let Some((level, inline)) = Self::parse_item_directive(trimmed) {
                    let (para, next_idx) =
                        Self::collect_pod_para_with_inline(&lines, idx + 1, inline, None);
                    let mut item_contents = Vec::new();
                    if let Some(para) = para {
                        item_contents.push(para);
                    }
                    entries.push(Self::make_pod_item(level, item_contents));
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "defn" {
                    let (config, leftover) = Self::parse_pod_config(rest);
                    let (defn, next_idx) =
                        Self::build_pod_defn_paragraph(&lines, idx + 1, leftover, config, None);
                    entries.push(defn);
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if directive == "code" {
                    // Abbreviated `=code`: a verbatim code paragraph.
                    let (numbered, rest_after) = Self::extract_numbered_alias(rest);
                    let (mut config, leftover) = Self::parse_pod_config(rest_after);
                    if numbered {
                        config.insert("numbered".to_string(), Value::TRUE);
                    }
                    let (code_lines, next_idx) =
                        Self::collect_pod_code_paragraph(&lines, idx + 1, leftover, None);
                    entries.push(Self::make_pod_code_block(code_lines, config));
                    idx = next_idx.max(idx + 1);
                    continue;
                }

                let (numbered, rest_after) = Self::extract_numbered_alias(rest);
                let mut config = ValueMap::default();
                if numbered {
                    config.insert("numbered".to_string(), Value::TRUE);
                }
                let (para, next_idx) =
                    Self::collect_pod_para_with_inline(&lines, idx + 1, rest_after, None);
                let mut contents = Vec::new();
                if let Some(para) = para {
                    contents.push(para);
                }
                if let Some(level) = Self::parse_heading_level(directive) {
                    entries.push(Self::make_pod_heading_with_config(level, contents, config));
                } else {
                    entries.push(Self::make_pod_named_with_config(
                        directive, contents, config,
                    ));
                }
                idx = next_idx.max(idx + 1);
                continue;
            }
            idx += 1;
        }
        if entries.len() > mark_len {
            spans.push((mark_start, lines.len()));
        }
        (entries, spans)
    }
}
