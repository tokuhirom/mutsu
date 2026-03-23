use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    fn dynamic_name_alias(name: &str) -> Option<String> {
        if let Some(rest) = name.strip_prefix("$*") {
            return Some(format!("*{}", rest));
        }
        if let Some(rest) = name.strip_prefix('*') {
            return Some(format!("$*{}", rest));
        }
        None
    }

    fn make_pod_block(contents: Vec<Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block"), attrs)
    }

    fn make_pod_named(name: &str, contents: Vec<Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(name.to_string()));
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Named"), attrs)
    }

    fn make_pod_heading(level: &str, contents: Vec<Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("level".to_string(), Value::str(level.to_string()));
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Heading"), attrs)
    }

    fn make_pod_comment(content: String) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "contents".to_string(),
            Value::array(vec![Value::str(content)]),
        );
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Comment"), attrs)
    }

    fn make_pod_para(lines: Vec<String>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "contents".to_string(),
            Value::array(lines.into_iter().map(Value::str).collect::<Vec<_>>()),
        );
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Para"), attrs)
    }

    fn make_pod_item(level: i64, contents: Vec<Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        attrs.insert("level".to_string(), Value::Int(level));
        Value::make_instance(Symbol::intern("Pod::Item"), attrs)
    }

    fn make_pod_table(rows: Vec<Vec<String>>) -> Value {
        let mut attrs = HashMap::new();
        let contents = rows
            .into_iter()
            .map(|row| Value::array(row.into_iter().map(Value::str).collect::<Vec<_>>()))
            .collect::<Vec<_>>();
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("headers".to_string(), Value::array(Vec::new()));
        attrs.insert("caption".to_string(), Value::str(String::new()));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Table"), attrs)
    }

    fn is_pod_table_separator(line: &str) -> bool {
        let trimmed = line.trim();
        !trimmed.is_empty()
            && trimmed.contains('-')
            && trimmed
                .chars()
                .all(|c| matches!(c, '-' | '+' | '|' | ':' | ' ' | '\t'))
    }

    fn collect_table_rows(lines: &[&str], mut idx: usize) -> (Vec<Vec<String>>, usize) {
        let mut rows = Vec::new();
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || trimmed.starts_with('=') {
                break;
            }
            if Self::is_pod_table_separator(trimmed) {
                idx += 1;
                continue;
            }
            if !trimmed.contains('|') {
                break;
            }
            let row = trimmed
                .split('|')
                .map(|cell| cell.trim().to_string())
                .collect::<Vec<_>>();
            rows.push(row);
            idx += 1;
        }
        (rows, idx)
    }

    fn collect_paragraph(lines: &[&str], mut idx: usize) -> (String, usize) {
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

    fn normalize_pod_text(parts: &[String]) -> String {
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

    fn collect_pod_para(lines: &[&str], mut idx: usize) -> (Value, usize) {
        let mut para_lines = Vec::new();
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || trimmed.starts_with('=') {
                break;
            }
            para_lines.push(lines[idx].trim().to_string());
            idx += 1;
        }
        let text = Self::normalize_pod_text(&para_lines);
        let payload = if text.is_empty() {
            Vec::new()
        } else {
            vec![text]
        };
        (Self::make_pod_para(payload), idx)
    }

    fn collect_pod_para_with_inline(
        lines: &[&str],
        mut idx: usize,
        inline: &str,
    ) -> (Option<Value>, usize) {
        let mut para_lines = Vec::new();
        if !inline.trim().is_empty() {
            para_lines.push(inline.trim().to_string());
        }
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if trimmed.is_empty() || trimmed.starts_with('=') {
                break;
            }
            para_lines.push(lines[idx].trim().to_string());
            idx += 1;
        }
        if para_lines.is_empty() {
            return (None, idx);
        }
        let text = Self::normalize_pod_text(&para_lines);
        let payload = if text.is_empty() {
            Vec::new()
        } else {
            vec![text]
        };
        (Some(Self::make_pod_para(payload)), idx)
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

    fn parse_heading_level(directive: &str) -> Option<&str> {
        let level = directive.strip_prefix("head")?;
        if level.is_empty() || !level.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        Some(level)
    }

    fn collect_pod_entries(
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
            if let Some((directive, rest)) = Self::parse_pod_directive_line(trimmed) {
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
                if directive == "table" {
                    let (rows, next_idx) = Self::collect_table_rows(lines, idx + 1);
                    if !rows.is_empty() {
                        entries.push(Self::make_pod_table(rows));
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
                    let (para, next_idx) =
                        Self::collect_pod_para_with_inline(lines, idx + 1, inline);
                    let mut contents = Vec::new();
                    if let Some(para) = para {
                        contents.push(para);
                    }
                    if let Some(level) = Self::parse_heading_level(target) {
                        entries.push(Self::make_pod_heading(level, contents));
                    } else {
                        entries.push(Self::make_pod_named(target, contents));
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
                                Self::parse_pod_directive_line(lines[idx].trim_start())
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
                        Self::collect_pod_para_with_inline(lines, idx + 1, inline);
                    let mut item_contents = Vec::new();
                    if let Some(para) = para {
                        item_contents.push(para);
                    }
                    entries.push(Self::make_pod_item(level, item_contents));
                    idx = next_idx.max(idx + 1);
                    continue;
                }

                let (para, next_idx) = Self::collect_pod_para_with_inline(lines, idx + 1, rest);
                let mut contents = Vec::new();
                if let Some(para) = para {
                    contents.push(para);
                }
                if let Some(level) = Self::parse_heading_level(directive) {
                    entries.push(Self::make_pod_heading(level, contents));
                } else {
                    entries.push(Self::make_pod_named(directive, contents));
                }
                idx = next_idx.max(idx + 1);
                continue;
            }

            let (para, next_idx) = Self::collect_pod_para(lines, idx);
            entries.push(para);
            idx = next_idx.max(idx + 1);
        }
        (entries, idx)
    }

    fn parse_item_level(token: &str) -> Option<i64> {
        let suffix = token
            .strip_prefix("=item")
            .or_else(|| token.strip_prefix("item"))?;
        if suffix.is_empty() {
            return Some(1);
        }
        suffix.parse::<i64>().ok().filter(|n| *n > 0)
    }

    fn parse_item_directive(line: &str) -> Option<(i64, &str)> {
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

    pub(super) fn collect_pod_blocks(&mut self, input: &str) {
        let lines: Vec<&str> = input.lines().collect();
        let mut entries = Vec::new();
        let mut idx = 0usize;
        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            if let Some((directive, rest)) = Self::parse_pod_directive_line(trimmed) {
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
                if directive == "table" {
                    let (rows, next_idx) = Self::collect_table_rows(&lines, idx + 1);
                    if !rows.is_empty() {
                        entries.push(Self::make_pod_table(rows));
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
                    let (para, next_idx) =
                        Self::collect_pod_para_with_inline(&lines, idx + 1, inline);
                    let mut contents = Vec::new();
                    if let Some(para) = para {
                        contents.push(para);
                    }
                    if let Some(level) = Self::parse_heading_level(target) {
                        entries.push(Self::make_pod_heading(level, contents));
                    } else {
                        entries.push(Self::make_pod_named(target, contents));
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
                                Self::parse_pod_directive_line(lines[idx].trim_start())
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
                        Self::collect_pod_para_with_inline(&lines, idx + 1, inline);
                    let mut item_contents = Vec::new();
                    if let Some(para) = para {
                        item_contents.push(para);
                    }
                    entries.push(Self::make_pod_item(level, item_contents));
                    idx = next_idx.max(idx + 1);
                    continue;
                }

                let (para, next_idx) = Self::collect_pod_para_with_inline(&lines, idx + 1, rest);
                let mut contents = Vec::new();
                if let Some(para) = para {
                    contents.push(para);
                }
                if let Some(level) = Self::parse_heading_level(directive) {
                    entries.push(Self::make_pod_heading(level, contents));
                } else {
                    entries.push(Self::make_pod_named(directive, contents));
                }
                idx = next_idx.max(idx + 1);
                continue;
            }
            idx += 1;
        }
        self.env.insert("=pod".to_string(), Value::array(entries));
    }

    pub(super) fn init_io_environment(&mut self) {
        let stdout = self.create_handle(
            IoHandleTarget::Stdout,
            IoHandleMode::Write,
            Some("STDOUT".to_string()),
        );
        self.env.insert("$*OUT".to_string(), stdout.clone());
        self.env.insert("*OUT".to_string(), stdout);
        let stderr = self.create_handle(
            IoHandleTarget::Stderr,
            IoHandleMode::Write,
            Some("STDERR".to_string()),
        );
        self.env.insert("$*ERR".to_string(), stderr.clone());
        self.env.insert("*ERR".to_string(), stderr);
        let stdin = self.create_handle(
            IoHandleTarget::Stdin,
            IoHandleMode::Read,
            Some("STDIN".to_string()),
        );
        self.env.insert("$*IN".to_string(), stdin.clone());
        self.env.insert("*IN".to_string(), stdin);
        let argfiles = self.create_handle(
            IoHandleTarget::ArgFiles,
            IoHandleMode::Read,
            Some("$*ARGFILES".to_string()),
        );
        self.env.insert("$*ARGFILES".to_string(), argfiles.clone());
        self.env.insert("*ARGFILES".to_string(), argfiles);
        let spec = self.make_io_spec_instance();
        self.env.insert("$*SPEC".to_string(), spec.clone());
        self.env.insert("*SPEC".to_string(), spec);
        #[cfg(not(target_arch = "wasm32"))]
        let cwd_str = env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .to_string_lossy()
            .to_string();
        #[cfg(target_arch = "wasm32")]
        let cwd_str = "/".to_string();
        let cwd_val = self.make_io_path_instance(&cwd_str);
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val);
        #[cfg(not(target_arch = "wasm32"))]
        let tmpdir_str = env::temp_dir().to_string_lossy().to_string();
        #[cfg(target_arch = "wasm32")]
        let tmpdir_str = "/tmp".to_string();
        let tmpdir_val = self.make_io_path_instance(&tmpdir_str);
        self.env.insert("$*TMPDIR".to_string(), tmpdir_val.clone());
        self.env.insert("*TMPDIR".to_string(), tmpdir_val);
        #[cfg(not(target_arch = "wasm32"))]
        let home_val = if let Ok(home) = env::var("HOME") {
            self.make_io_path_instance(&home)
        } else {
            Value::Nil
        };
        #[cfg(target_arch = "wasm32")]
        let home_val = Value::Nil;
        self.env.insert("$*HOME".to_string(), home_val.clone());
        self.env.insert("*HOME".to_string(), home_val);
        // $*EXECUTABLE - path to the interpreter binary
        #[cfg(not(target_arch = "wasm32"))]
        let exe_path = std::env::current_exe()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|_| "mutsu".to_string());
        #[cfg(target_arch = "wasm32")]
        let exe_path = "mutsu".to_string();
        let exe_io = self.make_io_path_instance(&exe_path);
        self.env.insert("$*EXECUTABLE".to_string(), exe_io.clone());
        self.env.insert("*EXECUTABLE".to_string(), exe_io);
        self.env.insert(
            "$*EXECUTABLE-NAME".to_string(),
            Value::str(
                std::path::Path::new(&exe_path)
                    .file_name()
                    .map(|f| f.to_string_lossy().to_string())
                    .unwrap_or_else(|| exe_path.clone()),
            ),
        );
        let exec_name = self.env.get("$*EXECUTABLE-NAME").cloned().unwrap();
        self.env.insert("*EXECUTABLE-NAME".to_string(), exec_name);
        let distro = Self::make_distro_instance();
        self.env.insert("*DISTRO".to_string(), distro.clone());
        self.env.insert("?DISTRO".to_string(), distro);
        let perl = Self::make_perl_instance();
        self.env.insert("*PERL".to_string(), perl.clone());
        self.env.insert("?PERL".to_string(), perl);
        let raku = Self::make_perl_instance();
        self.env.insert("*RAKU".to_string(), raku.clone());
        self.env.insert("?RAKU".to_string(), raku);
        let vm = Self::make_vm_instance();
        self.env.insert("$*VM".to_string(), vm.clone());
        self.env.insert("*VM".to_string(), vm);
        let kernel = Self::make_kernel_instance();
        self.env.insert("*KERNEL".to_string(), kernel.clone());
        self.env.insert("?KERNEL".to_string(), kernel);
    }

    pub(super) fn create_handle(
        &mut self,
        target: IoHandleTarget,
        mode: IoHandleMode,
        path: Option<String>,
    ) -> Value {
        let id = self.next_handle_id;
        self.next_handle_id += 1;
        let state = IoHandleState {
            target,
            mode,
            path: path.clone(),
            line_separators: self.default_line_separators(),
            line_chomp: true,
            encoding: "utf-8".to_string(),
            file: None,
            socket: None,
            listener: None,
            closed: false,
            out_buffer_capacity: None,
            out_buffer_pending: Vec::new(),
            bin: false,
            nl_out: "\n".to_string(),
            bytes_written: 0,
            read_attempted: false,
            argfiles_index: 0,
            argfiles_reader: None,
        };
        self.handles.insert(id, state);
        self.make_handle_instance(id)
    }

    pub(super) fn make_handle_instance(&self, handle_id: usize) -> Value {
        self.make_handle_instance_with_bin(handle_id, false)
    }

    pub(super) fn make_handle_instance_with_bin(&self, handle_id: usize, bin: bool) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("handle".to_string(), Value::Int(handle_id as i64));
        if let Some(state) = self.handles.get(&handle_id) {
            if let Some(path) = &state.path {
                attrs.insert("path".to_string(), Value::str(path.clone()));
            }
            attrs.insert(
                "mode".to_string(),
                Value::str(Self::mode_name(state.mode).to_string()),
            );
            // Store handle state attributes so IO::Handle.open can inherit them
            attrs.insert("chomp".to_string(), Value::Bool(state.line_chomp));
            attrs.insert("nl-out".to_string(), Value::str(state.nl_out.clone()));
            // Store nl-in
            if state.line_separators.len() == 1 {
                attrs.insert(
                    "nl-in".to_string(),
                    Value::str(String::from_utf8_lossy(&state.line_separators[0]).to_string()),
                );
            } else {
                let items: Vec<Value> = state
                    .line_separators
                    .iter()
                    .map(|s| Value::str(String::from_utf8_lossy(s).to_string()))
                    .collect();
                attrs.insert("nl-in".to_string(), Value::real_array(items));
            }
        }
        if bin {
            attrs.insert("bin".to_string(), Value::Bool(true));
        }
        Value::make_instance(Symbol::intern("IO::Handle"), attrs)
    }

    pub(super) fn mode_name(mode: IoHandleMode) -> &'static str {
        match mode {
            IoHandleMode::Read => "r",
            IoHandleMode::Write => "w",
            IoHandleMode::Append => "a",
            IoHandleMode::ReadWrite => "rw",
        }
    }

    pub(super) fn make_io_spec_instance(&self) -> Value {
        let attrs = HashMap::new();
        Value::make_instance(Symbol::intern("IO::Spec"), attrs)
    }

    pub(super) fn make_distro_instance() -> Value {
        let os = std::env::consts::OS;
        let (name, auth, version_str, desc, release) = match os {
            "macos" => {
                let product_version = Command::new("sw_vers")
                    .arg("-productVersion")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                let build_version = Command::new("sw_vers")
                    .arg("-buildVersion")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                let product_name = Command::new("sw_vers")
                    .arg("-productName")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                let major = product_version.split('.').next().unwrap_or("").to_string();
                let desc_str = if product_name.is_empty() {
                    format!("macOS {}", major)
                } else {
                    // sw_vers returns "macOS" as the product name
                    format!("{} {}", product_name, major)
                };
                (
                    "macos".to_string(),
                    "Apple Inc.".to_string(),
                    product_version,
                    desc_str,
                    build_version,
                )
            }
            "linux" => {
                use std::sync::OnceLock;
                static DISTRO_UNAME_R: OnceLock<String> = OnceLock::new();
                let kernel_release = DISTRO_UNAME_R
                    .get_or_init(|| {
                        Command::new("uname")
                            .arg("-r")
                            .output()
                            .ok()
                            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                            .unwrap_or_default()
                    })
                    .clone();
                let mut distro_desc = String::new();
                if let Ok(content) = std::fs::read_to_string("/etc/os-release") {
                    for line in content.lines() {
                        if let Some(val) = line.strip_prefix("PRETTY_NAME=") {
                            distro_desc = val.trim_matches('"').to_string();
                            break;
                        }
                    }
                }
                if distro_desc.is_empty() {
                    distro_desc = "Linux".to_string();
                }
                (
                    "linux".to_string(),
                    "unknown".to_string(),
                    kernel_release.clone(),
                    distro_desc,
                    kernel_release,
                )
            }
            "windows" => {
                let ver = Command::new("cmd")
                    .args(["/C", "ver"])
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                (
                    "mswin32".to_string(),
                    "Microsoft".to_string(),
                    String::new(),
                    ver.clone(),
                    ver,
                )
            }
            _ => {
                use std::sync::OnceLock;
                static FALLBACK_UNAME_R: OnceLock<String> = OnceLock::new();
                let kernel_release = FALLBACK_UNAME_R
                    .get_or_init(|| {
                        Command::new("uname")
                            .arg("-r")
                            .output()
                            .ok()
                            .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                            .unwrap_or_default()
                    })
                    .clone();
                (
                    os.to_string(),
                    "unknown".to_string(),
                    kernel_release.clone(),
                    os.to_string(),
                    kernel_release,
                )
            }
        };

        // Parse version string into Value::Version
        let version = Self::parse_version_string(&version_str);

        let path_sep = if cfg!(windows) {
            ";".to_string()
        } else {
            ":".to_string()
        };

        let is_win = cfg!(windows);

        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(name));
        attrs.insert("auth".to_string(), Value::str(auth));
        attrs.insert("version".to_string(), version);
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Blob"), HashMap::new()),
        );
        attrs.insert("desc".to_string(), Value::str(desc));
        attrs.insert("release".to_string(), Value::str(release));
        attrs.insert("path-sep".to_string(), Value::str(path_sep));
        attrs.insert("is-win".to_string(), Value::Bool(is_win));

        Value::make_instance(Symbol::intern("Distro"), attrs)
    }

    pub(super) fn parse_version_string(s: &str) -> Value {
        use crate::value::VersionPart;
        let parts: Vec<VersionPart> = s
            .split('.')
            .filter_map(|p| p.parse::<i64>().ok().map(VersionPart::Num))
            .collect();
        if parts.is_empty() {
            Value::Version {
                parts: vec![VersionPart::Num(0)],
                plus: false,
                minus: false,
            }
        } else {
            Value::Version {
                parts,
                plus: false,
                minus: false,
            }
        }
    }

    pub(super) fn make_perl_instance() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str_from("Raku"));
        attrs.insert("auth".to_string(), Value::str_from("The Perl Foundation"));
        attrs.insert(
            "version".to_string(),
            Value::Version {
                parts: vec![crate::value::VersionPart::Num(6)],
                plus: false,
                minus: false,
            },
        );
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Blob"), {
                let mut a = HashMap::new();
                a.insert("values".to_string(), Value::array(vec![Value::Int(0)]));
                a
            }),
        );
        attrs.insert(
            "desc".to_string(),
            Value::str_from("Raku Programming Language"),
        );
        attrs.insert(
            "DISTROnames".to_string(),
            Value::array(vec![
                Value::str_from("macos"),
                Value::str_from("linux"),
                Value::str_from("freebsd"),
                Value::str_from("mswin32"),
                Value::str_from("openbsd"),
                Value::str_from("dragonfly"),
                Value::str_from("netbsd"),
                Value::str_from("browser"),
            ]),
        );
        attrs.insert(
            "KERNELnames".to_string(),
            Value::array(vec![
                Value::str_from("darwin"),
                Value::str_from("linux"),
                Value::str_from("freebsd"),
                Value::str_from("openbsd"),
                Value::str_from("netbsd"),
                Value::str_from("dragonfly"),
                Value::str_from("sunos"),
                Value::str_from("win32"),
                Value::str_from("browser"),
            ]),
        );
        Value::make_instance(Symbol::intern("Perl"), attrs)
    }

    pub(super) fn make_vm_instance() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str_from("mutsu"));
        attrs.insert("auth".to_string(), Value::str_from("github.com/tokuhirom"));
        attrs.insert(
            "version".to_string(),
            Value::Version {
                parts: vec![
                    crate::value::VersionPart::Num(0),
                    crate::value::VersionPart::Num(1),
                    crate::value::VersionPart::Num(0),
                ],
                plus: false,
                minus: false,
            },
        );
        attrs.insert("precomp-ext".to_string(), Value::str_from("mutsu"));
        attrs.insert("precomp-target".to_string(), Value::str_from("mutsu"));
        Value::make_instance(Symbol::intern("VM"), attrs)
    }

    pub(super) fn make_kernel_instance() -> Value {
        use std::sync::OnceLock;
        static UNAME_R: OnceLock<String> = OnceLock::new();
        static UNAME_M: OnceLock<String> = OnceLock::new();

        let os = std::env::consts::OS;
        let arch = std::env::consts::ARCH;

        // Kernel name (e.g., "linux", "darwin", "win32")
        let name = match os {
            "macos" => "darwin".to_string(),
            "windows" => "win32".to_string(),
            _ => os.to_string(),
        };

        // Kernel release (e.g., "6.18.7-76061807-generic") — cached
        let release = UNAME_R
            .get_or_init(|| {
                Command::new("uname")
                    .arg("-r")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default()
            })
            .clone();

        // Hardware (e.g., "x86_64") — cached
        let hardware = UNAME_M
            .get_or_init(|| {
                Command::new("uname")
                    .arg("-m")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_else(|| arch.to_string())
            })
            .clone();

        // Architecture (mapped from Rust's ARCH constant)
        let arch_str = match arch {
            "x86_64" => "x86_64",
            "x86" => "i386",
            "aarch64" => "aarch64",
            "arm" => "arm",
            _ => arch,
        }
        .to_string();

        // Bits
        let bits: i64 = if arch == "x86_64" || arch == "aarch64" || arch == "powerpc64" {
            64
        } else {
            32
        };

        // Hostname — cached
        static HOSTNAME: OnceLock<String> = OnceLock::new();
        let hostname = HOSTNAME
            .get_or_init(|| {
                Command::new("hostname")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default()
            })
            .clone();

        // Version from release string
        let version = Self::parse_version_string(&release);

        // Build signals list (first 32 standard POSIX signals)
        let signal_names = [
            "", "HUP", "INT", "QUIT", "ILL", "TRAP", "ABRT", "BUS", "FPE", "KILL", "USR1", "SEGV",
            "USR2", "PIPE", "ALRM", "TERM", "STKFLT", "CHLD", "CONT", "STOP", "TSTP", "TTIN",
            "TTOU", "URG", "XCPU", "XFSZ", "VTALRM", "PROF", "WINCH", "IO", "PWR", "SYS",
        ];
        let signals: Vec<Value> = (0..32)
            .map(|i| {
                if i < signal_names.len() && !signal_names[i].is_empty() {
                    Value::str(format!("SIG{}", signal_names[i]))
                } else {
                    Value::Nil
                }
            })
            .collect();

        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(name));
        attrs.insert("auth".to_string(), Value::str_from("unknown"));
        attrs.insert("version".to_string(), version);
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Blob"), HashMap::new()),
        );
        attrs.insert("desc".to_string(), Value::Str(String::new().into()));
        attrs.insert("release".to_string(), Value::str(release));
        attrs.insert("hardware".to_string(), Value::str(hardware));
        attrs.insert("arch".to_string(), Value::str(arch_str));
        attrs.insert("bits".to_string(), Value::Int(bits));
        attrs.insert("hostname".to_string(), Value::str(hostname));
        attrs.insert("signals".to_string(), Value::array(signals));

        Value::make_instance(Symbol::intern("Kernel"), attrs)
    }

    pub(super) fn get_dynamic_handle(&self, name: &str) -> Option<Value> {
        self.env.get(name).cloned().or_else(|| {
            Self::dynamic_name_alias(name).and_then(|alias| self.env.get(&alias).cloned())
        })
    }

    pub(super) fn default_input_handle(&self) -> Option<Value> {
        self.get_dynamic_handle("$*ARGFILES")
            .or_else(|| self.get_dynamic_handle("$*IN"))
    }

    pub(crate) fn write_to_named_handle(
        &mut self,
        name: &str,
        text: &str,
        newline: bool,
    ) -> Result<(), RuntimeError> {
        if let Some(handle) = self.get_dynamic_handle(name) {
            if Self::handle_id_from_value(&handle).is_some() {
                return self.write_to_handle_value(&handle, text, newline);
            }
            let payload = if newline {
                format!("{}\n", text)
            } else {
                text.to_string()
            };
            if self
                .call_method_with_values(handle, "print", vec![Value::str(payload.clone())])
                .is_ok()
            {
                return Ok(());
            }
            if name == "$*ERR" {
                self.stderr_output.push_str(&payload);
            }
            self.emit_output(&payload);
            return Ok(());
        }
        let payload = if newline {
            format!("{}\n", text)
        } else {
            text.to_string()
        };
        if name == "$*ERR" {
            self.stderr_output.push_str(&payload);
        }
        self.emit_output(&payload);
        Ok(())
    }

    pub(crate) fn render_gist_value(&mut self, value: &Value) -> String {
        self.call_method_with_values(value.clone(), "gist", vec![])
            .map(|result| result.to_string_value())
            .unwrap_or_else(|_| crate::runtime::gist_value(value))
    }

    /// Stringify a value by calling .Str method (used by put/print).
    /// Falls back to to_string_value() if .Str method dispatch fails.
    pub(crate) fn render_str_value(&mut self, value: &Value) -> String {
        self.call_method_with_values(value.clone(), "Str", vec![])
            .map(|result| result.to_string_value())
            .unwrap_or_else(|_| value.to_string_value())
    }

    pub(super) fn get_dynamic_string(&self, name: &str) -> Option<String> {
        self.get_dynamic_handle(name).and_then(|value| match value {
            Value::Str(s) => Some(s.to_string()),
            Value::Instance { attributes, .. } => {
                // Support IO::Path instances (e.g., $*CWD)
                attributes.get("path").map(|v| v.to_string_value())
            }
            _ => None,
        })
    }

    pub(super) fn get_cwd_path(&self) -> PathBuf {
        if let Some(cwd) = self.get_dynamic_string("$*CWD") {
            return PathBuf::from(cwd);
        }
        env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
    }

    pub(super) fn resolve_path(&self, path: &str) -> PathBuf {
        let pb = PathBuf::from(path);
        if pb.is_absolute() {
            self.apply_chroot(pb)
        } else {
            let cwd = self.get_cwd_path();
            self.apply_chroot(cwd.join(pb))
        }
    }

    pub(super) fn apply_chroot(&self, path: PathBuf) -> PathBuf {
        if let Some(root) = &self.chroot_root {
            if path.starts_with(root) {
                return path;
            }
            if path.is_absolute() {
                if let Ok(stripped_root) = path.strip_prefix(root) {
                    return root.join(stripped_root);
                }
                if let Ok(stripped_slash) = path.strip_prefix("/") {
                    return root.join(stripped_slash);
                }
                return root.join(path);
            }
        }
        path
    }

    pub(super) fn stringify_path(path: &Path) -> String {
        path.to_string_lossy().to_string()
    }

    pub(crate) fn make_io_path_instance(&self, path: &str) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("path".to_string(), Value::str(path.to_string()));
        Value::make_instance(Symbol::intern("IO::Path"), attrs)
    }

    pub(super) fn collect_doc_comments(&mut self, input: &str) {
        use super::DocComment;

        self.doc_comments.clear();
        self.doc_comment_list.clear();
        let mut pending_leading: Option<String> = None;
        // The last declaration that can receive trailing #= comments
        let mut last_declarant: Option<(String, super::DocDeclKind)> = None;
        // Track the current class scope for method doc keys
        let mut current_class: Option<String> = None;
        // Stack of class scopes for nested classes
        let mut class_stack: Vec<Option<String>> = Vec::new();

        fn extract_ident(s: &str) -> String {
            s.trim_start()
                .split(|c: char| !c.is_alphanumeric() && c != '_' && c != '-' && c != ':')
                .next()
                .unwrap_or("")
                .to_string()
        }

        fn append_doc_text(existing: Option<String>, text: &str) -> Option<String> {
            if text.is_empty() {
                return existing;
            }
            Some(match existing {
                Some(mut prev) => {
                    prev.push(' ');
                    prev.push_str(text);
                    prev
                }
                None => text.to_string(),
            })
        }

        fn matching_bracket(c: char) -> Option<char> {
            match c {
                '(' => Some(')'),
                '[' => Some(']'),
                '{' => Some('}'),
                '<' => Some('>'),
                '\u{00AB}' => Some('\u{00BB}'), // << >>
                '\u{2018}' => Some('\u{2019}'),
                '\u{201C}' => Some('\u{201D}'),
                '\u{300C}' => Some('\u{300D}'),
                '\u{300E}' => Some('\u{300F}'),
                '\u{FF08}' => Some('\u{FF09}'),
                '\u{300A}' => Some('\u{300B}'),
                '\u{3008}' => Some('\u{3009}'),
                '\u{169B}' => Some('\u{169C}'),
                '\u{2045}' => Some('\u{2046}'),
                '\u{207D}' => Some('\u{207E}'),
                '\u{2768}' => Some('\u{2769}'),
                '\u{276E}' => Some('\u{276F}'),
                '\u{2770}' => Some('\u{2771}'),
                '\u{2772}' => Some('\u{2773}'),
                '\u{27E6}' => Some('\u{27E7}'),
                '\u{2985}' => Some('\u{2986}'),
                '\u{2993}' => Some('\u{2994}'),
                '\u{2995}' => Some('\u{2996}'),
                _ => None,
            }
        }

        fn parse_doc_comment(
            lines: &[&str],
            start: usize,
            prefix: &str,
        ) -> Option<(String, usize, bool)> {
            let trimmed = lines.get(start)?.trim_start();
            let rest = trimmed.strip_prefix(prefix)?;
            let rest = rest.trim_start();
            if rest.is_empty() {
                return Some((String::new(), start + 1, false));
            }

            let mut chars = rest.chars();
            let open = chars.next()?;
            let Some(close) = matching_bracket(open) else {
                return Some((rest.trim().to_string(), start + 1, false));
            };

            let mut count = 1usize;
            let mut scan = chars.as_str();
            while scan.starts_with(open) {
                count += 1;
                scan = &scan[open.len_utf8()..];
            }

            let open_seq: String = std::iter::repeat_n(open, count).collect();
            let close_seq: String = std::iter::repeat_n(close, count).collect();
            let mut depth = 1i32;
            let mut idx = start;
            let mut current = scan;
            let mut payload = String::new();

            loop {
                if count == 1 {
                    while !current.is_empty() {
                        let ch = current.chars().next().unwrap();
                        if ch == open {
                            depth += 1;
                            payload.push(ch);
                            current = &current[ch.len_utf8()..];
                            continue;
                        }
                        if ch == close {
                            depth -= 1;
                            if depth == 0 {
                                let normalized =
                                    payload.split_whitespace().collect::<Vec<_>>().join(" ");
                                return Some((normalized, idx + 1, true));
                            }
                            payload.push(ch);
                            current = &current[ch.len_utf8()..];
                            continue;
                        }
                        payload.push(ch);
                        current = &current[ch.len_utf8()..];
                    }
                } else {
                    while !current.is_empty() {
                        if current.starts_with(&close_seq[..]) {
                            depth -= 1;
                            if depth == 0 {
                                let normalized =
                                    payload.split_whitespace().collect::<Vec<_>>().join(" ");
                                return Some((normalized, idx + 1, true));
                            }
                            payload.push_str(&close_seq);
                            current = &current[close_seq.len()..];
                            continue;
                        }
                        if current.starts_with(&open_seq[..]) {
                            depth += 1;
                            payload.push_str(&open_seq);
                            current = &current[open_seq.len()..];
                            continue;
                        }
                        let ch = current.chars().next().unwrap();
                        payload.push(ch);
                        current = &current[ch.len_utf8()..];
                    }
                }

                idx += 1;
                if idx >= lines.len() {
                    let normalized = payload.split_whitespace().collect::<Vec<_>>().join(" ");
                    return Some((normalized, idx, true));
                }
                payload.push('\n');
                current = lines[idx];
            }
        }

        /// Check if a line (after stripping optional prefix keywords) contains
        /// a trailing #= comment on the same line. Returns (line_without_trailing, trailing_text).
        fn extract_inline_trailing(line: &str) -> (String, Option<String>) {
            // Look for #= not inside strings
            let mut in_sq = false;
            let mut in_dq = false;
            let bytes = line.as_bytes();
            let mut i = 0;
            while i < bytes.len() {
                let b = bytes[i];
                if b == b'\'' && !in_dq {
                    in_sq = !in_sq;
                } else if b == b'"' && !in_sq {
                    in_dq = !in_dq;
                } else if b == b'#'
                    && !in_sq
                    && !in_dq
                    && i + 1 < bytes.len()
                    && bytes[i + 1] == b'='
                {
                    // Check it's not #== (comparison)
                    if i + 2 < bytes.len() && bytes[i + 2] == b'=' {
                        i += 1;
                        continue;
                    }
                    let before = line[..i].to_string();
                    let raw = line[i + 2..].trim();
                    // Handle block form #={...} / #=(...)  / #=[...] / #=<...>
                    let after = if let Some(inner) = raw.strip_prefix('{') {
                        inner.strip_suffix('}').unwrap_or(inner).trim().to_string()
                    } else if let Some(inner) = raw.strip_prefix('(') {
                        inner.strip_suffix(')').unwrap_or(inner).trim().to_string()
                    } else if let Some(inner) = raw.strip_prefix('[') {
                        inner.strip_suffix(']').unwrap_or(inner).trim().to_string()
                    } else if let Some(inner) = raw.strip_prefix('<') {
                        inner.strip_suffix('>').unwrap_or(inner).trim().to_string()
                    } else {
                        raw.to_string()
                    };
                    return (before, Some(after));
                }
                i += 1;
            }
            (line.to_string(), None)
        }

        /// Try to extract a declarant name from a line, handling various declaration patterns.
        /// Returns (name, is_class_like, kind)
        fn try_extract_declarant(
            trimmed: &str,
            current_class: &Option<String>,
        ) -> Option<(String, bool, super::DocDeclKind)> {
            use super::DocDeclKind;
            // Strip optional scope declarators
            let s = trimmed
                .strip_prefix("my ")
                .or_else(|| trimmed.strip_prefix("our "))
                .unwrap_or(trimmed);

            // class/module/package/role/grammar declarations
            for kw in &["class ", "module ", "package ", "grammar "] {
                if let Some(rest) = s.strip_prefix(kw) {
                    let name = extract_ident(rest);
                    if !name.is_empty() {
                        let full_name = if name.contains("::") {
                            name
                        } else if let Some(class) = current_class {
                            format!("{}::{}", class, name)
                        } else {
                            name
                        };
                        return Some((full_name, true, DocDeclKind::Package));
                    }
                }
            }

            // role (may have parametric [...])
            if let Some(rest) = s.strip_prefix("role ") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    let full_name = if name.contains("::") {
                        name
                    } else if let Some(class) = current_class {
                        format!("{}::{}", class, name)
                    } else {
                        name
                    };
                    return Some((full_name, true, DocDeclKind::Package));
                }
            }

            // sub/method/submethod/token/rule/regex declarations
            let s2 = s
                .strip_prefix("multi ")
                .or_else(|| s.strip_prefix("proto "))
                .or_else(|| s.strip_prefix("only "))
                .unwrap_or(s);

            let had_dispatch_prefix =
                s.starts_with("multi ") || s.starts_with("proto ") || s.starts_with("only ");

            for kw in &["sub ", "method ", "submethod ", "token ", "rule ", "regex "] {
                if let Some(rest) = s2.strip_prefix(kw) {
                    let name = extract_ident(rest);
                    if !name.is_empty() {
                        let full_name = if kw.starts_with("method")
                            || kw.starts_with("submethod")
                            || kw.starts_with("token")
                            || kw.starts_with("rule")
                            || kw.starts_with("regex")
                        {
                            if let Some(class) = current_class {
                                format!("{}::{}", class, name)
                            } else {
                                name
                            }
                        } else {
                            // Prefix sub names with & to avoid collision with package names
                            format!("&{}", name)
                        };
                        return Some((full_name, false, DocDeclKind::Sub));
                    }
                }
            }

            // multi/proto/only without explicit sub/method keyword: treat as sub
            if had_dispatch_prefix {
                let name = extract_ident(s2);
                if !name.is_empty() {
                    return Some((format!("&{}", name), false, DocDeclKind::Sub));
                }
            }

            // has $.attr declarations
            if let Some(rest) = s.strip_prefix("has ") {
                let rest = rest.trim_start();
                let attr_rest =
                    if rest.starts_with('$') || rest.starts_with('@') || rest.starts_with('%') {
                        rest
                    } else {
                        rest.find(['$', '@', '%'])
                            .map(|i| &rest[i..])
                            .unwrap_or(rest)
                    };
                if let Some(after_sigil) = attr_rest
                    .strip_prefix("$.")
                    .or_else(|| attr_rest.strip_prefix("$!"))
                    .or_else(|| attr_rest.strip_prefix("@."))
                    .or_else(|| attr_rest.strip_prefix("@!"))
                    .or_else(|| attr_rest.strip_prefix("%."))
                    .or_else(|| attr_rest.strip_prefix("%!"))
                {
                    let name = extract_ident(after_sigil);
                    if !name.is_empty() {
                        let full_name = if let Some(class) = current_class {
                            format!("{}::$!{}", class, name)
                        } else {
                            format!("$!{}", name)
                        };
                        return Some((full_name, false, DocDeclKind::Attr));
                    }
                }
            }

            // enum declaration
            if let Some(rest) = s.strip_prefix("enum ") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    return Some((name, false, DocDeclKind::Package));
                }
            }

            // subset declaration
            if let Some(rest) = s.strip_prefix("subset ") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    return Some((name, false, DocDeclKind::Package));
                }
            }

            // unit module
            if let Some(rest) = s.strip_prefix("unit module") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    return Some((name, false, DocDeclKind::Package));
                }
            }

            None
        }

        let lines: Vec<&str> = input.lines().collect();
        let mut idx = 0usize;
        while idx < lines.len() {
            let line = lines[idx];
            let trimmed = line.trim_start();

            // Leading doc comment (#|)
            if let Some((text, next_idx, _is_block)) = parse_doc_comment(&lines, idx, "#|") {
                pending_leading = append_doc_text(pending_leading.take(), &text);
                last_declarant = None;
                idx = next_idx;
                continue;
            }

            // Trailing doc comment (#=) on its own line
            if let Some((text, next_idx, _is_block)) = parse_doc_comment(&lines, idx, "#=") {
                if !text.is_empty()
                    && let Some((ref name, ref kind)) = last_declarant
                {
                    let entry =
                        self.doc_comments
                            .entry(name.clone())
                            .or_insert_with(|| DocComment {
                                wherefore_name: name.clone(),
                                kind: kind.clone(),
                                ..Default::default()
                            });
                    entry.trailing = append_doc_text(entry.trailing.take(), &text);
                }
                idx = next_idx;
                continue;
            }

            // Not a doc comment line — check for declarations
            // Skip empty lines and plain comments (but not #| or #=)
            if trimmed.is_empty()
                || (trimmed.starts_with('#')
                    && !trimmed.starts_with("#|")
                    && !trimmed.starts_with("#="))
            {
                // Don't reset last_declarant for empty/comment lines within a class body
                // but do reset pending_leading if we see a regular comment
                if trimmed.starts_with('#')
                    && !trimmed.starts_with("#|")
                    && !trimmed.starts_with("#=")
                {
                    // Regular comment — don't affect doc comments
                }
                idx += 1;
                continue;
            }

            // Opening brace tracking for class scope
            if trimmed == "}" {
                if let Some(prev) = class_stack.pop() {
                    current_class = prev;
                } else {
                    current_class = None;
                }
                last_declarant = None;
                idx += 1;
                continue;
            }

            // Check for inline trailing #= on the same line as a declaration
            let (line_without_trailing, inline_trailing) = extract_inline_trailing(trimmed);
            let check_line = line_without_trailing.trim();

            if let Some((name, is_class_like, kind)) =
                try_extract_declarant(check_line, &current_class)
            {
                let leading = pending_leading.take();
                let has_leading = leading.is_some();
                let has_inline_trailing = inline_trailing.is_some();
                // Only create doc_comments entry if there's actual doc content
                if has_leading || has_inline_trailing {
                    let entry =
                        self.doc_comments
                            .entry(name.clone())
                            .or_insert_with(|| DocComment {
                                wherefore_name: name.clone(),
                                kind: kind.clone(),
                                ..Default::default()
                            });
                    entry.kind = kind.clone();
                    if has_leading {
                        entry.leading = leading;
                    }
                    if let Some(ref trail) = inline_trailing {
                        entry.trailing = append_doc_text(entry.trailing.take(), trail);
                    }
                }
                // Always set last_declarant so trailing #= on the next line can attach
                last_declarant = Some((name.clone(), kind.clone()));
                if is_class_like {
                    class_stack.push(current_class.take());
                    current_class = Some(name);
                }
            } else {
                // Not a recognized declaration — discard pending leading
                pending_leading = None;
                last_declarant = None;
            }

            idx += 1;
        }

        // Build ordered doc_comment_list: collect names in order during a simple re-scan
        self.doc_comment_list.clear();
        let mut seen_names = std::collections::HashSet::new();
        let _ = &self.doc_comments; // used below
        // Collect keys that have actual content, in source order
        // Since HashMap doesn't preserve order, re-scan declarations
        {
            let mut cur_class2: Option<String> = None;
            let mut cls_stack2: Vec<Option<String>> = Vec::new();
            let mut idx2 = 0usize;
            while idx2 < lines.len() {
                let trimmed2 = lines[idx2].trim_start();
                if parse_doc_comment(&lines, idx2, "#|").is_some()
                    || parse_doc_comment(&lines, idx2, "#=").is_some()
                {
                    // Skip doc comment lines — they were handled in first pass
                    let (_, next_idx, _) = parse_doc_comment(&lines, idx2, "#|")
                        .or_else(|| parse_doc_comment(&lines, idx2, "#="))
                        .unwrap();
                    idx2 = next_idx;
                    continue;
                }
                if trimmed2.is_empty() || (trimmed2.starts_with('#')) {
                    idx2 += 1;
                    continue;
                }
                if trimmed2 == "}" {
                    if let Some(prev) = cls_stack2.pop() {
                        cur_class2 = prev;
                    } else {
                        cur_class2 = None;
                    }
                    idx2 += 1;
                    continue;
                }
                let (lwt, _) = extract_inline_trailing(trimmed2);
                let ck = lwt.trim();
                if let Some((name, is_cls, _kind)) = try_extract_declarant(ck, &cur_class2) {
                    if !seen_names.contains(&name)
                        && let Some(dc) = self.doc_comments.get(&name)
                    {
                        seen_names.insert(name.clone());
                        self.doc_comment_list.push(dc.clone());
                    }
                    if is_cls {
                        cls_stack2.push(cur_class2.take());
                        cur_class2 = Some(name);
                    }
                }
                idx2 += 1;
            }
        }
    }

    /// Add Pod::Block::Declarator entries to $=pod from doc_comment_list.
    pub(super) fn add_declarator_pod_entries(&mut self) {
        use super::DocDeclKind;
        // Get existing $=pod entries
        let mut pod_entries: Vec<Value> = if let Some(Value::Array(arr, _)) = self.env.get("=pod") {
            arr.iter().cloned().collect()
        } else {
            Vec::new()
        };
        // Add declarator doc entries
        // TODO: For Sub/Method/Attr, the WHEREFORE should ideally be the actual
        // runtime object, not a Package placeholder. Currently we use the type
        // name as a Package value so .^name returns the correct type.
        for dc in &self.doc_comment_list {
            let wherefore = match dc.kind {
                DocDeclKind::Package => {
                    Value::Package(crate::symbol::Symbol::intern(&dc.wherefore_name))
                }
                DocDeclKind::Sub => {
                    // Use "Sub" as the type name — in mutsu, ^name returns "Sub"
                    // for both subs and methods.
                    Value::Package(crate::symbol::Symbol::intern("Sub"))
                }
                DocDeclKind::Attr => Value::Package(crate::symbol::Symbol::intern("Attribute")),
            };
            let pod_entry = Interpreter::make_pod_declarator(dc, wherefore);
            pod_entries.push(pod_entry);
        }
        self.env
            .insert("=pod".to_string(), Value::array(pod_entries));
    }
}
