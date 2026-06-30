use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn create_handle(
        &mut self,
        target: IoHandleTarget,
        mode: IoHandleMode,
        path: Option<String>,
    ) -> Value {
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
            utf16_bom_written: false,
            utf16_detected_be: None,
            argfiles_index: 0,
            argfiles_reader: None,
            argfiles_paths: None,
            pending_words: std::collections::VecDeque::new(),
            close_on_word_exhaust: false,
        };
        let id = self.insert_handle_state(state);
        self.make_handle_instance(id)
    }

    pub(super) fn make_handle_instance(&self, handle_id: usize) -> Value {
        self.make_handle_instance_with_bin(handle_id, false)
    }

    pub(super) fn make_handle_instance_with_bin(&self, handle_id: usize, bin: bool) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("handle".to_string(), Value::Int(handle_id as i64));
        if let Some(state) = self.io_handles().map.get(&handle_id) {
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
            // A text handle exposes its decoder name via `.encoding`; a binary
            // handle reports no encoding (handled by the `bin` branch below).
            // Raku reports the canonical name (`utf8`), not the dashed form.
            if !bin {
                let enc = match state.encoding.as_str() {
                    "utf-8" => "utf8",
                    "utf-16" => "utf16",
                    other => other,
                };
                attrs.insert("encoding".to_string(), Value::str(enc.to_string()));
            }
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
        // `$*SPEC` is the platform IO::Spec *type object* (Raku: `$*SPEC.DEFINITE`
        // is False), not an instance. IO::Spec methods are class methods, and the
        // value must be `===` / `is-deeply` identical to `IO::Path.SPEC` (which
        // returns the same type object), so represent it as a Package.
        let class_name = if cfg!(target_os = "windows") {
            "IO::Spec::Win32"
        } else {
            "IO::Spec::Unix"
        };
        Value::Package(Symbol::intern(class_name))
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
            "VMnames".to_string(),
            Value::array(vec![
                Value::str_from("mutsu"),
                Value::str_from("moar"),
                Value::str_from("jvm"),
                Value::str_from("js"),
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

    /// Update `$*RAKU.version` to reflect `use v6.x` after parsing.
    pub(super) fn update_raku_version_from_parser(&mut self) {
        let lang_version = crate::parser::current_language_version();
        if lang_version.is_empty() || lang_version == "6" {
            return;
        }
        let parts: Vec<crate::value::VersionPart> = lang_version
            .split('.')
            .map(|s| {
                if let Ok(n) = s.parse::<i64>() {
                    crate::value::VersionPart::Num(n)
                } else {
                    crate::value::VersionPart::Str(s.to_string())
                }
            })
            .collect();
        let new_version = Value::Version {
            parts,
            plus: false,
            minus: false,
        };
        for key in ["*RAKU", "?RAKU", "*PERL", "?PERL"] {
            if let Some(Value::Instance {
                class_name,
                attributes,
                id,
            }) = self.env.get(key).cloned()
            {
                let mut new_attrs = attributes.to_map();
                new_attrs.insert("version".to_string(), new_version.clone());
                let new_val = Value::write_back_sharing(&attributes, class_name, new_attrs, id);
                self.env.insert(key.to_string(), new_val);
            }
        }
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
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Blob"), {
                let mut a = HashMap::new();
                a.insert("values".to_string(), Value::array(vec![Value::Int(0)]));
                a
            }),
        );
        attrs.insert("desc".to_string(), Value::str_from("mutsu virtual machine"));
        attrs.insert("precomp-ext".to_string(), Value::str_from("mutsu"));
        attrs.insert("precomp-target".to_string(), Value::str_from("mutsu"));
        attrs.insert("prefix".to_string(), Value::str_from("mutsu"));
        // properties: a non-empty hash so the value is truthy.
        let mut props = HashMap::new();
        props.insert("name".to_string(), Value::str_from("mutsu"));
        attrs.insert("properties".to_string(), Value::hash(props));
        // config: a non-empty hash so the value is truthy.
        let mut config = HashMap::new();
        config.insert("name".to_string(), Value::str_from("mutsu"));
        // be: 0 for little-endian, 1 for big-endian (matches Rakudo's $*VM.config<be>)
        let be_val = if cfg!(target_endian = "big") {
            "1"
        } else {
            "0"
        };
        config.insert("be".to_string(), Value::str_from(be_val));
        attrs.insert("config".to_string(), Value::hash(config));
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

        // endian: Endian enum value matching the host system
        let endian_val = if cfg!(target_endian = "little") {
            Value::Enum {
                enum_type: crate::symbol::Symbol::intern("Endian"),
                key: crate::symbol::Symbol::intern("LittleEndian"),
                value: crate::value::EnumValue::Int(1),
                index: 1,
            }
        } else {
            Value::Enum {
                enum_type: crate::symbol::Symbol::intern("Endian"),
                key: crate::symbol::Symbol::intern("BigEndian"),
                value: crate::value::EnumValue::Int(2),
                index: 2,
            }
        };
        attrs.insert("endian".to_string(), endian_val);

        Value::make_instance(Symbol::intern("Kernel"), attrs)
    }
}
