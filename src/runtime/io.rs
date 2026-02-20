use super::*;

impl Interpreter {
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
        let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        let cwd_str = cwd.to_string_lossy().to_string();
        self.env
            .insert("$*CWD".to_string(), Value::Str(cwd_str.clone()));
        self.env
            .insert("*CWD".to_string(), Value::Str(cwd_str.clone()));
        let tmpdir = env::temp_dir();
        let tmpdir_str = tmpdir.to_string_lossy().to_string();
        self.env
            .insert("$*TMPDIR".to_string(), Value::Str(tmpdir_str.clone()));
        self.env
            .insert("*TMPDIR".to_string(), Value::Str(tmpdir_str));
        if let Ok(home) = env::var("HOME") {
            self.env
                .insert("$*HOME".to_string(), Value::Str(home.clone()));
            self.env.insert("*HOME".to_string(), Value::Str(home));
        } else {
            self.env
                .insert("$*HOME".to_string(), Value::Str(cwd_str.clone()));
            self.env.insert("*HOME".to_string(), Value::Str(cwd_str));
        }
        // $*EXECUTABLE - path to the interpreter binary
        let exe_path = std::env::current_exe()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|_| "mutsu".to_string());
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
        attrs.insert("version".to_string(), Value::Str("v6.d".to_string()));
        attrs.insert(
            "signature".to_string(),
            Value::Array(vec![Value::Int(0)]), // Blob placeholder (non-empty)
        );
        attrs.insert(
            "desc".to_string(),
            Value::Str("Raku Programming Language".to_string()),
        );
        attrs.insert(
            "DISTROnames".to_string(),
            Value::Array(vec![
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

    pub(super) fn get_dynamic_handle(&self, name: &str) -> Option<Value> {
        self.env.get(name).cloned()
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
            return self.write_to_handle_value(&handle, text, newline);
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

    pub(super) fn get_dynamic_string(&self, name: &str) -> Option<String> {
        self.env.get(name).and_then(|value| match value {
            Value::Str(s) => Some(s.clone()),
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

    pub(super) fn make_io_path_instance(&self, path: &str) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("path".to_string(), Value::Str(path.to_string()));
        Value::make_instance("IO::Path".to_string(), attrs)
    }

    pub(super) fn collect_doc_comments(&mut self, input: &str) {
        self.doc_comments.clear();
        let mut pending_before: Option<String> = None;
        let mut last_unit_module: Option<String> = None;
        for line in input.lines() {
            let trimmed = line.trim_start();
            if let Some(rest) = trimmed.strip_prefix("#|") {
                pending_before = Some(rest.trim().to_string());
                continue;
            }
            if let Some(rest) = trimmed.strip_prefix("unit module") {
                let name = rest
                    .trim_start()
                    .split(|c: char| c.is_whitespace() || c == ';')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    if let Some(before) = pending_before.take()
                        && !before.is_empty()
                    {
                        self.doc_comments.insert(name.clone(), before);
                    }
                    last_unit_module = Some(name);
                }
                continue;
            }
            if let Some(rest) = trimmed.strip_prefix("#=")
                && let Some(name) = last_unit_module.take()
            {
                let after = rest.trim();
                if !after.is_empty() {
                    let entry = self.doc_comments.entry(name).or_default();
                    if entry.is_empty() {
                        entry.push_str(after);
                    } else {
                        entry.push('\n');
                        entry.push_str(after);
                    }
                }
            }
        }
    }
}
