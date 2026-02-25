use super::*;

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
        Value::make_instance("Pod::Block".to_string(), attrs)
    }

    fn make_pod_comment(content: String) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "contents".to_string(),
            Value::array(vec![Value::Str(content)]),
        );
        attrs.insert("config".to_string(), Value::hash(HashMap::new()));
        Value::make_instance("Pod::Block::Comment".to_string(), attrs)
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
        Value::make_instance("Pod::Block::Table".to_string(), attrs)
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

    pub(super) fn collect_pod_blocks(&mut self, input: &str) {
        let lines: Vec<&str> = input.lines().collect();
        let mut entries = Vec::new();
        let mut idx = 0usize;

        while idx < lines.len() {
            let trimmed = lines[idx].trim_start();
            let mut words = trimmed.split_whitespace();
            let first = words.next();
            let second = words.next();

            if first == Some("=comment") {
                let (text, next_idx) = Self::collect_paragraph(&lines, idx + 1);
                entries.push(Self::make_pod_comment(text));
                idx = next_idx;
                continue;
            }

            if first == Some("=table") {
                let (rows, next_idx) = Self::collect_table_rows(&lines, idx + 1);
                if !rows.is_empty() {
                    entries.push(Self::make_pod_table(rows));
                }
                idx = next_idx;
                continue;
            }

            if first == Some("=for") && second == Some("comment") {
                let inline = trimmed
                    .strip_prefix("=for")
                    .map(str::trim_start)
                    .and_then(|rest| rest.strip_prefix("comment"))
                    .map(str::trim_start)
                    .unwrap_or_default();
                let mut text = String::new();
                if !inline.is_empty() {
                    text.push_str(inline);
                    text.push('\n');
                }
                let (tail, next_idx) = Self::collect_paragraph(&lines, idx + 1);
                text.push_str(&tail);
                entries.push(Self::make_pod_comment(text));
                idx = next_idx;
                continue;
            }

            if first == Some("=begin") && second == Some("comment") {
                idx += 1;
                let mut raw = String::new();
                while idx < lines.len() {
                    let end_trimmed = lines[idx].trim_start();
                    let mut end_words = end_trimmed.split_whitespace();
                    if end_words.next() == Some("=end") && end_words.next() == Some("comment") {
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

            if first == Some("=begin") && second == Some("pod") {
                idx += 1;
                let mut contents = Vec::new();
                while idx < lines.len() {
                    let inner = lines[idx].trim_start();
                    let mut inner_words = inner.split_whitespace();
                    let inner_first = inner_words.next();
                    let inner_second = inner_words.next();
                    if inner_first == Some("=end") && inner_second == Some("pod") {
                        idx += 1;
                        break;
                    }
                    if inner_first == Some("=comment") {
                        let (text, next_idx) = Self::collect_paragraph(&lines, idx + 1);
                        contents.push(Self::make_pod_comment(text));
                        idx = next_idx;
                        continue;
                    }
                    if inner_first == Some("=table") {
                        let (rows, next_idx) = Self::collect_table_rows(&lines, idx + 1);
                        if !rows.is_empty() {
                            contents.push(Self::make_pod_table(rows));
                        }
                        idx = next_idx;
                        continue;
                    }
                    if inner_first == Some("=for") && inner_second == Some("comment") {
                        let inline = inner
                            .strip_prefix("=for")
                            .map(str::trim_start)
                            .and_then(|rest| rest.strip_prefix("comment"))
                            .map(str::trim_start)
                            .unwrap_or_default();
                        let mut text = String::new();
                        if !inline.is_empty() {
                            text.push_str(inline);
                            text.push('\n');
                        }
                        let (tail, next_idx) = Self::collect_paragraph(&lines, idx + 1);
                        text.push_str(&tail);
                        contents.push(Self::make_pod_comment(text));
                        idx = next_idx;
                        continue;
                    }
                    idx += 1;
                }
                entries.push(Self::make_pod_block(contents));
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
        Value::make_instance("IO::Handle".to_string(), attrs)
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
        Value::make_instance("IO::Spec".to_string(), attrs)
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
            Value::make_instance("Blob".to_string(), HashMap::new()),
        );
        attrs.insert("desc".to_string(), Value::Str(desc));
        attrs.insert("release".to_string(), Value::Str(release));
        attrs.insert("path-sep".to_string(), Value::Str(path_sep));
        attrs.insert("is-win".to_string(), Value::Bool(is_win));

        Value::make_instance("Distro".to_string(), attrs)
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
            Value::make_instance("Blob".to_string(), {
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
        Value::make_instance("Perl".to_string(), attrs)
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
        Value::make_instance("VM".to_string(), attrs)
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
            self.output.push_str(&payload);
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
        self.output.push_str(&payload);
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
        Value::make_instance("IO::Path".to_string(), attrs)
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
