use super::*;

impl Interpreter {
    pub(crate) fn collect_paragraph(lines: &[&str], mut idx: usize) -> (String, usize) {
        let mut text = String::new();
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || trimmed.starts_with('=') {
                break;
            }
            text.push_str(lines[idx]);
            text.push('\n');
            idx += 1;
        }
        (text, idx)
    }

    pub(crate) fn normalize_pod_text(parts: &[String]) -> String {
        // Join all parts, then collapse runs of breaking whitespace into single spaces.
        // Non-breaking whitespace (U+00A0, U+202F, U+2060, U+FEFF) is preserved as-is.
        let joined = parts.join(" ");
        let mut result = String::new();
        let mut in_breaking_ws = false;
        for ch in joined.chars() {
            if Self::is_breaking_whitespace(ch) {
                if !in_breaking_ws && !result.is_empty() {
                    result.push(' ');
                }
                in_breaking_ws = true;
            } else {
                in_breaking_ws = false;
                result.push(ch);
            }
        }
        // Trim trailing space
        if result.ends_with(' ') {
            result.pop();
        }
        result
    }

    /// Returns true for whitespace characters that should be normalized (collapsed)
    /// in Pod text. Non-breaking spaces (U+00A0, U+202F, U+2060, U+FEFF) are NOT
    /// considered breaking and are preserved as-is.
    fn is_breaking_whitespace(ch: char) -> bool {
        matches!(
            ch,
            ' ' | '\t' | '\n' | '\r' | '\x0B' | '\x0C' | '\u{1680}' | '\u{180E}' | '\u{2000}'
                ..='\u{200A}' | '\u{2028}' | '\u{2029}' | '\u{205F}' | '\u{3000}'
        )
    }

    /// Collect a paragraph and parse formatting codes in the text.
    fn collect_pod_para_formatted(
        lines: &[&str],
        mut idx: usize,
        end_target: Option<&str>,
    ) -> (Value, usize) {
        let allows_code_blocks = matches!(end_target, Some("pod"))
            || end_target.is_some_and(|t| Self::parse_item_level(t).is_some());
        let mut para_lines = Vec::new();
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || Self::active_pod_directive(lines[idx], end_target).is_some() {
                break;
            }
            // Stop if line is indented (code block follows) — only in pod/item blocks
            if allows_code_blocks {
                let indent = lines[idx].len() - trimmed.len();
                if indent > 0 {
                    break;
                }
            }
            para_lines.push(lines[idx].trim().to_string());
            idx += 1;
        }
        let text = Self::normalize_pod_text(&para_lines);
        if text.is_empty() {
            (Self::make_pod_para(Vec::new()), idx)
        } else {
            // Check if text contains formatting codes
            let has_formatting = text
                .as_bytes()
                .windows(2)
                .any(|w| w[0].is_ascii_uppercase() && w[1] == b'<');
            if has_formatting {
                (Self::make_pod_para_with_formatting(&text), idx)
            } else {
                (Self::make_pod_para(vec![text]), idx)
            }
        }
    }

    /// Collect consecutive indented lines as a Pod::Block::Code.
    /// Groups lines with the same base indentation level.
    /// When indentation changes, this returns and lets the caller create a new block.
    fn collect_pod_code_block(
        lines: &[&str],
        mut idx: usize,
        end_target: Option<&str>,
    ) -> (Value, usize) {
        // Determine base indentation from first line
        let base_indent = lines[idx].len() - lines[idx].trim_start().len();
        let mut code_lines: Vec<&str> = Vec::new();

        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if Self::active_pod_directive(lines[idx], end_target).is_some() {
                break;
            }
            if trimmed.is_empty() {
                // Blank line: include it if the next non-blank line has the same
                // base indentation. Peek ahead.
                let mut peek = idx + 1;
                while peek < lines.len() && lines[peek].trim().is_empty() {
                    peek += 1;
                }
                if peek < lines.len() {
                    let next_trimmed = lines[peek].trim_start();
                    if !next_trimmed.is_empty() {
                        let next_indent = lines[peek].len() - next_trimmed.len();
                        if next_indent == base_indent {
                            // Include the blank line(s) and continue
                            code_lines.push("");
                            idx += 1;
                            continue;
                        }
                    }
                }
                // End of this code block
                break;
            }
            let indent = lines[idx].len() - trimmed.len();
            if indent == 0 {
                // Not indented → not a code block line
                break;
            }
            if indent < base_indent {
                // Different base indentation → different code block
                break;
            }
            // Strip the base indentation
            if lines[idx].len() >= base_indent {
                code_lines.push(&lines[idx][base_indent..]);
            } else {
                code_lines.push(trimmed);
            }
            idx += 1;
        }

        // Remove trailing empty lines
        while code_lines.last().is_some_and(|l| l.is_empty()) {
            code_lines.pop();
        }

        let text = code_lines.join("\n");
        (Self::make_pod_code(text), idx)
    }

    pub(crate) fn collect_pod_para_with_inline(
        lines: &[&str],
        mut idx: usize,
        inline: &str,
        end_target: Option<&str>,
    ) -> (Option<Value>, usize) {
        let mut para_lines = Vec::new();
        if !inline.trim().is_empty() {
            para_lines.push(inline.trim().to_string());
        }
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || Self::active_pod_directive(lines[idx], end_target).is_some() {
                break;
            }
            para_lines.push(lines[idx].trim().to_string());
            idx += 1;
        }
        if para_lines.is_empty() {
            return (None, idx);
        }
        let text = Self::normalize_pod_text(&para_lines);
        if text.is_empty() {
            (Some(Self::make_pod_para(Vec::new())), idx)
        } else {
            let has_formatting = text
                .as_bytes()
                .windows(2)
                .any(|w| w[0].is_ascii_uppercase() && w[1] == b'<');
            if has_formatting {
                (Some(Self::make_pod_para_with_formatting(&text)), idx)
            } else {
                (Some(Self::make_pod_para(vec![text])), idx)
            }
        }
    }

    fn pod_block_allows_flush_directives(end_target: Option<&str>) -> bool {
        match end_target {
            None => true,
            Some("pod") => true,
            Some(target) => Self::parse_item_level(target).is_some(),
        }
    }

    pub(crate) fn active_pod_directive<'a>(
        line: &'a str,
        end_target: Option<&str>,
    ) -> Option<(&'a str, &'a str)> {
        let trimmed = line.trim_start();
        let (directive, rest) = Self::parse_pod_directive_line(trimmed)?;
        let has_indent = trimmed.len() != line.len();

        if directive == "end" {
            let target = rest.split_whitespace().next().unwrap_or_default();
            if end_target.is_some_and(|expected| expected == target) {
                return Some((directive, rest));
            }
        }

        if Self::pod_block_allows_flush_directives(end_target) || has_indent {
            Some((directive, rest))
        } else {
            None
        }
    }

    fn parse_pod_directive_line(line: &str) -> Option<(&str, &str)> {
        let trimmed = line.trim_start();
        let token = trimmed.split_whitespace().next()?;
        let directive = token.strip_prefix('=')?;
        let first = directive.as_bytes().first().copied()?;
        if !first.is_ascii_alphabetic() {
            return None;
        }
        if !directive
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
        {
            return None;
        }
        let rest = trimmed[token.len()..].trim_start();
        Some((directive, rest))
    }

    pub(crate) fn parse_heading_level(directive: &str) -> Option<&str> {
        let level = directive.strip_prefix("head")?;
        if level.is_empty() || !level.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        Some(level)
    }

    pub(crate) fn collect_pod_entries(
        lines: &[&str],
        mut idx: usize,
        end_target: Option<&str>,
    ) -> (Vec<Value>, usize) {
        let mut entries = Vec::new();
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() {
                idx += 1;
                continue;
            }
            if let Some((directive, rest)) = Self::active_pod_directive(lines[idx], end_target) {
                if directive == "end" {
                    let target = rest.split_whitespace().next().unwrap_or_default();
                    if end_target.is_some_and(|expected| expected == target) {
                        return (entries, idx + 1);
                    }
                    idx += 1;
                    continue;
                }
                if directive == "comment" {
                    let (text, next_idx) = Self::collect_paragraph(lines, idx + 1);
                    entries.push(Self::make_pod_comment(text));
                    idx = next_idx.max(idx + 1);
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
                        config.insert("numbered".to_string(), Value::Bool(true));
                    }
                    let (headers, rows, next_idx) =
                        Self::collect_table_rows_with_headers(lines, idx + 1);
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
                        let mut text = String::new();
                        if !inline.is_empty() {
                            text.push_str(inline);
                            text.push('\n');
                        }
                        let (tail, next_idx) = Self::collect_paragraph(lines, idx + 1);
                        text.push_str(&tail);
                        entries.push(Self::make_pod_comment(text));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "defn" {
                        let (config, leftover) = Self::parse_pod_config(inline);
                        let (defn, next_idx) = Self::build_pod_defn_paragraph(
                            lines,
                            idx + 1,
                            leftover,
                            config,
                            end_target,
                        );
                        entries.push(defn);
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "table" {
                        let (numbered, inline_after) = Self::extract_numbered_alias(inline);
                        let (mut config, _) = Self::parse_pod_config(inline_after);
                        if numbered {
                            config.insert("numbered".to_string(), Value::Bool(true));
                        }
                        let (headers, rows, next_idx) =
                            Self::collect_table_rows_with_headers(lines, idx + 1);
                        entries.push(Self::make_pod_table_full(headers, rows, config));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    let (numbered, inline_after) = Self::extract_numbered_alias(inline);
                    let (mut config, leftover) = Self::parse_pod_config(inline_after);
                    if numbered {
                        config.insert("numbered".to_string(), Value::Bool(true));
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
                        Self::collect_pod_para_with_inline(lines, cont_idx, leftover, end_target);
                    let mut contents = Vec::new();
                    if let Some(para) = para {
                        contents.push(para);
                    }
                    if let Some(level) = Self::parse_heading_level(target) {
                        entries.push(Self::make_pod_heading_with_config(level, contents, config));
                    } else {
                        entries.push(Self::make_pod_named_with_config(target, contents, config));
                    }
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
                        entries.push(Self::make_pod_block(vec![Value::str(raw)]));
                        continue;
                    }
                    if target == "defn" {
                        let after_target = rest.strip_prefix(target).unwrap_or("");
                        let (config, _) = Self::parse_pod_config(after_target);
                        let (defn, next_idx) =
                            Self::build_pod_defn_delimited(lines, idx + 1, config);
                        entries.push(defn);
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    if target == "code" {
                        // =begin code ... =end code → Pod::Block::Code
                        // Collect raw text (no inner directive parsing)
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
                        // Strip common leading indentation
                        let min_indent = code_lines
                            .iter()
                            .filter(|l| !l.trim().is_empty())
                            .map(|l| l.len() - l.trim_start().len())
                            .min()
                            .unwrap_or(0);
                        let text: String = code_lines
                            .iter()
                            .map(|l| {
                                if l.len() >= min_indent {
                                    &l[min_indent..]
                                } else {
                                    l.trim_start()
                                }
                            })
                            .collect::<Vec<_>>()
                            .join("\n");
                        // Trim trailing newlines
                        let text = text.trim_end_matches('\n').to_string();
                        entries.push(Self::make_pod_code_with_config(text, code_config));
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
                    if let Some(level) = Self::parse_item_level(target) {
                        let (item_contents, next_idx) =
                            Self::collect_pod_entries(lines, idx + 1, Some(target));
                        entries.push(Self::make_pod_item(level, item_contents));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    let (contents, next_idx) =
                        Self::collect_pod_entries(lines, idx + 1, Some(target));
                    if target == "pod" {
                        entries.push(Self::make_pod_block(contents));
                    } else if let Some(level) = Self::parse_heading_level(target) {
                        entries.push(Self::make_pod_heading(level, contents));
                    } else {
                        entries.push(Self::make_pod_named(target, contents));
                    }
                    idx = next_idx.max(idx + 1);
                    continue;
                }
                if let Some((level, inline)) = Self::parse_item_directive(trimmed) {
                    let (para, next_idx) =
                        Self::collect_pod_para_with_inline(lines, idx + 1, inline, end_target);
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
                    let (defn, next_idx) = Self::build_pod_defn_paragraph(
                        lines,
                        idx + 1,
                        leftover,
                        config,
                        end_target,
                    );
                    entries.push(defn);
                    idx = next_idx.max(idx + 1);
                    continue;
                }

                let (numbered, rest_after) = Self::extract_numbered_alias(rest);
                let mut config = HashMap::new();
                if numbered {
                    config.insert("numbered".to_string(), Value::Bool(true));
                }
                let (para, next_idx) =
                    Self::collect_pod_para_with_inline(lines, idx + 1, rest_after, end_target);
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

            // Check if this line is indented → code block (only in pod/item blocks)
            let indent = lines[idx].len() - lines[idx].trim_start().len();
            let allows_code_blocks = matches!(end_target, Some("pod"))
                || end_target.is_some_and(|t| Self::parse_item_level(t).is_some());
            if indent > 0 && allows_code_blocks {
                let (code, next_idx) = Self::collect_pod_code_block(lines, idx, end_target);
                entries.push(code);
                idx = next_idx.max(idx + 1);
            } else {
                let (para, next_idx) = Self::collect_pod_para_formatted(lines, idx, end_target);
                entries.push(para);
                idx = next_idx.max(idx + 1);
            }
        }
        (entries, idx)
    }

    pub(crate) fn parse_item_level(token: &str) -> Option<i64> {
        let suffix = token
            .strip_prefix("=item")
            .or_else(|| token.strip_prefix("item"))?;
        if suffix.is_empty() {
            return Some(1);
        }
        suffix.parse::<i64>().ok().filter(|n| *n > 0)
    }

    pub(crate) fn parse_item_directive(line: &str) -> Option<(i64, &str)> {
        let trimmed = line.trim_start();
        let rest = trimmed.strip_prefix("=item")?;
        let digit_len = rest.bytes().take_while(|b| b.is_ascii_digit()).count();
        let level = if digit_len == 0 {
            1
        } else {
            rest[..digit_len].parse::<i64>().ok().filter(|n| *n > 0)?
        };
        let after = &rest[digit_len..];
        if let Some(ch) = after.chars().next()
            && !ch.is_whitespace()
        {
            return None;
        }
        Some((level, after.trim_start()))
    }
}
