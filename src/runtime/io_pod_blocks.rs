use super::*;

impl Interpreter {
    pub(super) fn collect_pod_blocks(&mut self, input: &str) {
        let lines: Vec<&str> = input.lines().collect();
        let mut entries = Vec::new();
        let mut idx = 0usize;
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if let Some((directive, rest)) = Self::active_pod_directive(lines[idx], None) {
                if directive == "end" {
                    idx += 1;
                    continue;
                }
                if directive == "comment" {
                    let (text, next_idx) = Self::collect_paragraph(&lines, idx + 1);
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
                        let mut text = String::new();
                        if !inline.is_empty() {
                            text.push_str(inline);
                            text.push('\n');
                        }
                        let (tail, next_idx) = Self::collect_paragraph(&lines, idx + 1);
                        text.push_str(&tail);
                        entries.push(Self::make_pod_comment(text));
                        idx = next_idx.max(idx + 1);
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
                            config.insert("numbered".to_string(), Value::Bool(true));
                        }
                        let (headers, rows, next_idx) =
                            Self::collect_table_rows_with_headers(&lines, idx + 1);
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
                        Self::collect_pod_para_with_inline(&lines, cont_idx, leftover, None);
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
                            Self::collect_pod_entries(&lines, idx + 1, Some(target));
                        entries.push(Self::make_pod_item(level, item_contents));
                        idx = next_idx.max(idx + 1);
                        continue;
                    }
                    let (contents, next_idx) =
                        Self::collect_pod_entries(&lines, idx + 1, Some(target));
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

                let (numbered, rest_after) = Self::extract_numbered_alias(rest);
                let mut config = HashMap::new();
                if numbered {
                    config.insert("numbered".to_string(), Value::Bool(true));
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
        self.env.insert("=pod".to_string(), Value::array(entries));
    }
}
