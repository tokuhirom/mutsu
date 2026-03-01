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
        attrs.insert("name".to_string(), Value::Str(name.to_string()));
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Named"), attrs)
    }

    fn make_pod_heading(level: &str, contents: Vec<Value>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("level".to_string(), Value::Str(level.to_string()));
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Heading"), attrs)
    }

    fn make_pod_comment(content: String) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "contents".to_string(),
            Value::array(vec![Value::Str(content)]),
        );
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance(Symbol::intern("Pod::Block::Comment"), attrs)
    }

    fn make_pod_para(lines: Vec<String>) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "contents".to_string(),
            Value::array(lines.into_iter().map(Value::Str).collect::<Vec<_>>()),
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
            .map(|row| Value::array(row.into_iter().map(Value::Str).collect::<Vec<_>>()))
            .collect::<Vec<_>>();
        attrs.insert("contents".to_string(), Value::array(contents));
        attrs.insert("headers".to_string(), Value::array(Vec::new()));
        attrs.insert("caption".to_string(), Value::Str(String::new()));
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
        parts
            .iter()
            .flat_map(|part| part.split_whitespace())
            .collect::<Vec<_>>()
            .join(" ")
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
                        entries.push(Self::make_pod_block(vec![Value::Str(raw)]));
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
                        entries.push(Self::make_pod_block(vec![Value::Str(raw)]));
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
            Value::Str(
                std::path::Path::new(&exe_path)
                    .file_name()
                    .map(|f| f.to_string_lossy().to_string())
                    .unwrap_or_else(|| exe_path.clone()),
            ),
        );
        self.env.insert(
            "*EXECUTABLE-NAME".to_string(),
            self.env.get("$*EXECUTABLE-NAME").cloned().unwrap(),
        );
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
            closed: false,
            bin: false,
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
                attrs.insert("path".to_string(), Value::Str(path.clone()));
            }
            attrs.insert(
                "mode".to_string(),
                Value::Str(Self::mode_name(state.mode).to_string()),
            );
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
                let kernel_release = Command::new("uname")
                    .arg("-r")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
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
                let kernel_release = Command::new("uname")
                    .arg("-r")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
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
        attrs.insert("name".to_string(), Value::Str(name));
        attrs.insert("auth".to_string(), Value::Str(auth));
        attrs.insert("version".to_string(), version);
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Blob"), HashMap::new()),
        );
        attrs.insert("desc".to_string(), Value::Str(desc));
        attrs.insert("release".to_string(), Value::Str(release));
        attrs.insert("path-sep".to_string(), Value::Str(path_sep));
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
        attrs.insert("name".to_string(), Value::Str("Raku".to_string()));
        attrs.insert(
            "auth".to_string(),
            Value::Str("The Perl Foundation".to_string()),
        );
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
            Value::Str("Raku Programming Language".to_string()),
        );
        attrs.insert(
            "DISTROnames".to_string(),
            Value::array(vec![
                Value::Str("macos".to_string()),
                Value::Str("linux".to_string()),
                Value::Str("freebsd".to_string()),
                Value::Str("mswin32".to_string()),
                Value::Str("openbsd".to_string()),
                Value::Str("dragonfly".to_string()),
                Value::Str("netbsd".to_string()),
                Value::Str("browser".to_string()),
            ]),
        );
        Value::make_instance(Symbol::intern("Perl"), attrs)
    }

    pub(super) fn make_vm_instance() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::Str("mutsu".to_string()));
        attrs.insert(
            "auth".to_string(),
            Value::Str("github.com/tokuhirom".to_string()),
        );
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
        attrs.insert("precomp-ext".to_string(), Value::Str("mutsu".to_string()));
        attrs.insert(
            "precomp-target".to_string(),
            Value::Str("mutsu".to_string()),
        );
        Value::make_instance(Symbol::intern("VM"), attrs)
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
                .call_method_with_values(handle, "print", vec![Value::Str(payload.clone())])
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

    pub(super) fn get_dynamic_string(&self, name: &str) -> Option<String> {
        self.get_dynamic_handle(name).and_then(|value| match value {
            Value::Str(s) => Some(s.clone()),
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
        attrs.insert("path".to_string(), Value::Str(path.to_string()));
        Value::make_instance(Symbol::intern("IO::Path"), attrs)
    }

    pub(super) fn collect_doc_comments(&mut self, input: &str) {
        self.doc_comments.clear();
        let mut pending_before: Option<String> = None;
        // The last declaration that can receive trailing #= comments
        let mut last_declarant: Option<String> = None;
        // Whether trailing #= has already been started for last_declarant
        let mut trailing_started = false;
        // Track the current class scope for method doc keys
        let mut current_class: Option<String> = None;

        fn extract_ident(s: &str) -> String {
            s.trim_start()
                .split(|c: char| !c.is_alphanumeric() && c != '_' && c != '-' && c != ':')
                .next()
                .unwrap_or("")
                .to_string()
        }

        for line in input.lines() {
            let trimmed = line.trim_start();

            // Leading doc comment (#|)
            if let Some(rest) = trimmed.strip_prefix("#|") {
                let text = rest.trim();
                pending_before = Some(match pending_before.take() {
                    Some(mut prev) => {
                        prev.push(' ');
                        prev.push_str(text);
                        prev
                    }
                    None => text.to_string(),
                });
                last_declarant = None;
                continue;
            }

            // Trailing doc comment (#=)
            if let Some(rest) = trimmed.strip_prefix("#=") {
                let after = rest.trim();
                if !after.is_empty()
                    && let Some(ref name) = last_declarant
                {
                    let entry = self.doc_comments.entry(name.clone()).or_default();
                    if entry.is_empty() {
                        entry.push_str(after);
                    } else if trailing_started {
                        // Consecutive #= lines: join with space
                        entry.push(' ');
                        entry.push_str(after);
                    } else {
                        // First #= after leading #|: join with newline
                        entry.push('\n');
                        entry.push_str(after);
                    }
                    trailing_started = true;
                }
                continue;
            }

            // Not a doc comment line — check for declarations
            last_declarant = None;
            trailing_started = false;

            // Skip empty lines and plain comments
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }

            // unit module
            if let Some(rest) = trimmed.strip_prefix("unit module") {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    if let Some(before) = pending_before.take() {
                        self.doc_comments.insert(name.clone(), before);
                    }
                    last_declarant = Some(name);
                }
                continue;
            }

            // class declaration
            let class_rest = trimmed
                .strip_prefix("my ")
                .or(Some(trimmed))
                .and_then(|s| s.strip_prefix("class "));
            if let Some(rest) = class_rest {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    if let Some(before) = pending_before.take() {
                        self.doc_comments.insert(name.clone(), before);
                    }
                    last_declarant = Some(name.clone());
                    current_class = Some(name);
                }
                continue;
            }

            // method/submethod declaration
            let method_rest = trimmed
                .strip_prefix("multi method ")
                .or_else(|| trimmed.strip_prefix("method "))
                .or_else(|| trimmed.strip_prefix("submethod "));
            if let Some(rest) = method_rest {
                let name = extract_ident(rest);
                if !name.is_empty() {
                    let full_name = if let Some(ref class) = current_class {
                        format!("{}::{}", class, name)
                    } else {
                        name
                    };
                    if let Some(before) = pending_before.take() {
                        self.doc_comments.insert(full_name.clone(), before);
                    }
                    last_declarant = Some(full_name);
                }
                continue;
            }

            // Closing brace exits class scope
            if trimmed == "}" {
                current_class = None;
            }

            pending_before = None;
        }
    }
}
