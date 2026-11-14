use super::*;
use crate::symbol::Symbol;

/// The `Distro` fields rakudo derives before building the object.
struct DistroFields {
    name: String,
    auth: String,
    version: String,
    desc: String,
    release: String,
}

/// Rakudo's `kv2Map` over `/etc/os-release`: every `"` is dropped, a line is
/// split at `=` (the value keeps any further `=`), both sides are trimmed, and
/// a line without `=` is skipped.
// Cost: O(n), n = bytes of the file.
fn os_release_map(content: &str) -> std::collections::HashMap<String, String> {
    let mut map = std::collections::HashMap::new();
    for line in content.replace('"', "").lines() {
        if let Some((key, value)) = line.split_once('=') {
            map.insert(key.to_string(), value.trim().to_string());
        }
    }
    map
}

/// Overlay the `/etc/os-release` identity onto `fields`, as rakudo's
/// `$*DISTRO` does on Linux: `ID` is the name, `HOME_URL` the auth,
/// `VERSION` the version, `VERSION_ID` the release and `PRETTY_NAME` the
/// description. A key the file lacks leaves the field as it was.
// Cost: O(n), n = bytes of the file.
fn distro_from_os_release(content: &str, fields: &mut DistroFields) {
    let map = os_release_map(content);
    if let Some(v) = map.get("ID") {
        fields.name = v.clone();
    }
    if let Some(v) = map.get("HOME_URL") {
        fields.auth = v.clone();
    }
    if let Some(v) = map.get("VERSION") {
        fields.version = v.clone();
    }
    if let Some(v) = map.get("VERSION_ID") {
        fields.release = v.clone();
    }
    if let Some(v) = map.get("PRETTY_NAME") {
        fields.desc = v.clone();
    }
}

#[cfg(test)]
mod distro_os_release_tests {
    use super::*;

    fn fields() -> DistroFields {
        DistroFields {
            name: "linux".into(),
            auth: "unknown".into(),
            version: "6.1".into(),
            desc: "unknown".into(),
            release: "unknown".into(),
        }
    }

    #[test]
    fn ubuntu_style_file_sets_every_field() {
        let mut f = fields();
        distro_from_os_release(
            "PRETTY_NAME=\"Ubuntu 24.04.5 LTS\"\nNAME=\"Ubuntu\"\nVERSION_ID=\"24.04\"\n\
             VERSION=\"24.04.5 LTS (Noble Numbat)\"\nID=ubuntu\nHOME_URL=\"https://www.ubuntu.com/\"\n",
            &mut f,
        );
        assert_eq!(f.name, "ubuntu");
        assert_eq!(f.auth, "https://www.ubuntu.com/");
        assert_eq!(f.version, "24.04.5 LTS (Noble Numbat)");
        assert_eq!(f.release, "24.04");
        assert_eq!(f.desc, "Ubuntu 24.04.5 LTS");
    }

    #[test]
    fn missing_keys_keep_the_kernel_defaults() {
        let mut f = fields();
        distro_from_os_release("ID=arch\n# comment\nBUILD_ID=rolling\n", &mut f);
        assert_eq!(f.name, "arch");
        assert_eq!(f.version, "6.1");
        assert_eq!(f.release, "unknown");
        assert_eq!(f.auth, "unknown");
    }

    #[test]
    fn a_value_keeps_further_equals_signs() {
        let mut f = fields();
        distro_from_os_release("HOME_URL=https://x.org/?a=b\n", &mut f);
        assert_eq!(f.auth, "https://x.org/?a=b");
    }
}

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
            path,
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
            stream_hit_eof: false,
            utf16_bom_written: false,
            utf16_detected_be: None,
            pending_words: std::collections::VecDeque::new(),
            close_on_exhaust: false,
            seq_reader: None,
        };
        let id = self.insert_handle_state(state);
        self.make_handle_instance(id)
    }

    pub(super) fn make_handle_instance(&self, handle_id: usize) -> Value {
        self.make_handle_instance_with_bin(handle_id, false)
    }

    pub(super) fn make_handle_instance_with_bin(&self, handle_id: usize, bin: bool) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("handle".to_string(), Value::int(handle_id as i64));
        if let Some(state) = self.io_handles().map.get(&handle_id) {
            if let Some(path) = &state.path {
                attrs.insert("path".to_string(), Value::str(path.clone()));
            }
            attrs.insert(
                "mode".to_string(),
                Value::str(Self::mode_name(state.mode).to_string()),
            );
            // Store handle state attributes so IO::Handle.open can inherit them
            attrs.insert("chomp".to_string(), Value::truth(state.line_chomp));
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
            attrs.insert("bin".to_string(), Value::TRUE);
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
        Value::package(Symbol::intern(class_name))
    }

    /// The attributes of a `Distro` constructed with arguments and nothing
    /// else: Rakudo declares them unset, except `name` and `auth`, which are
    /// "unknown".
    // Cost: O(1).
    pub(super) fn blank_distro_attributes() -> ValueMap {
        let mut attrs = ValueMap::default();
        for (key, type_name) in [
            ("release", "Str"),
            ("path-sep", "Str"),
            ("version", "Version"),
            ("signature", "Blob"),
            ("desc", "Str"),
        ] {
            attrs.insert(key.to_string(), Value::package(Symbol::intern(type_name)));
        }
        attrs.insert("name".to_string(), Value::str_from("unknown"));
        attrs.insert("auth".to_string(), Value::str_from("unknown"));
        attrs.insert("is-win".to_string(), Value::FALSE);
        attrs
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
                // Rakudo's `$*DISTRO` starts from the kernel (`name` "linux",
                // `version` = the kernel release, the rest "unknown") and, when
                // `/etc/os-release` exists, takes the distribution's identity
                // from it (see `distro_from_os_release`).
                let kernel_release = super::io_sysinfo_host::host_info().release.clone();
                let mut fields = DistroFields {
                    name: "linux".to_string(),
                    auth: "unknown".to_string(),
                    version: kernel_release.clone(),
                    desc: "unknown".to_string(),
                    release: "unknown".to_string(),
                };
                if let Ok(content) = std::fs::read_to_string("/etc/os-release") {
                    distro_from_os_release(&content, &mut fields);
                } else {
                    // Without os-release the old reading of the release stays
                    // the best available description.
                    fields.release = kernel_release;
                }
                (
                    fields.name,
                    fields.auth,
                    fields.version,
                    fields.desc,
                    fields.release,
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
                let kernel_release = super::io_sysinfo_host::host_info().release.clone();
                (
                    os.to_string(),
                    "unknown".to_string(),
                    kernel_release.clone(),
                    os.to_string(),
                    kernel_release,
                )
            }
        };

        // `$version.Version`: rakudo converts the string with `Str.Version`, so
        // `24.04.5 LTS (Noble Numbat)` keeps its words as parts.
        let version = Value::version_from_str(&version_str);

        let path_sep = if cfg!(windows) {
            ";".to_string()
        } else {
            ":".to_string()
        };

        let is_win = cfg!(windows);

        let mut attrs = ValueMap::default();
        attrs.insert("name".to_string(), Value::str(name));
        attrs.insert("auth".to_string(), Value::str(auth));
        attrs.insert("version".to_string(), version);
        attrs.insert(
            "signature".to_string(),
            // Rakudo leaves every `.signature` of the system objects unset:
            // the `Blob` type object, which stringifies to "" with a warning.
            Value::package(Symbol::intern("Blob")),
        );
        attrs.insert("desc".to_string(), Value::str(desc));
        attrs.insert("release".to_string(), Value::str(release));
        attrs.insert("path-sep".to_string(), Value::str(path_sep));
        attrs.insert("is-win".to_string(), Value::truth(is_win));

        Value::make_instance(Symbol::intern("Distro"), attrs)
    }

    pub(super) fn parse_version_string(s: &str) -> Value {
        use crate::value::VersionPart;
        let parts: Vec<VersionPart> = s
            .split('.')
            .filter_map(|p| p.parse::<i64>().ok().map(VersionPart::Num))
            .collect();
        if parts.is_empty() {
            Value::version(vec![VersionPart::Num(0)], false, false)
        } else {
            Value::version(parts, false, false)
        }
    }

    /// Parse the currently-in-effect language version (`use v6.x`, default
    /// `"6.d"`) into the `Version` `Value` `$*RAKU.version`/`$*PERL.version`
    /// expose. Shared by [`Self::make_perl_instance`] — so a lazily-built
    /// instance already reflects the compile unit's `use v6.x` at
    /// construction time instead of the old hardcoded `Version.new(6)`
    /// default (todo/tickets/magic-vars-should-be-built-lazily.md Slice 2:
    /// under eager construction this default was always immediately
    /// overwritten by [`Self::update_raku_version_from_parser`] before
    /// execution began, so the two must agree) — and by
    /// `update_raku_version_from_parser` (which mutates an
    /// already-materialized instance in place for a later parse in the same
    /// process, e.g. a nested `EVAL` with a different `use v6.x`).
    pub(crate) fn language_version_value() -> Value {
        let lang_version = crate::parser::current_language_version();
        let parts: Vec<crate::value::VersionPart> = lang_version
            .split('.')
            .filter(|s| !s.is_empty())
            .map(|s| {
                if let Ok(n) = s.parse::<i64>() {
                    crate::value::VersionPart::Num(n)
                } else {
                    crate::value::VersionPart::Str(s.to_string())
                }
            })
            .collect();
        if parts.is_empty() {
            Value::version(vec![crate::value::VersionPart::Num(6)], false, false)
        } else {
            Value::version(parts, false, false)
        }
    }

    pub(super) fn make_perl_instance() -> Value {
        let mut attrs = ValueMap::default();
        attrs.insert("name".to_string(), Value::str_from("Raku"));
        attrs.insert("auth".to_string(), Value::str_from("The Perl Foundation"));
        attrs.insert("version".to_string(), Self::language_version_value());
        attrs.insert(
            "signature".to_string(),
            // The `Blob` type object, as in Rakudo.
            Value::package(Symbol::intern("Blob")),
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
        // The compiler-identity object's type is `Raku`, not the pre-rename
        // `Perl` (rakudo: `$*RAKU.^name` and `$*PERL.^name` are both `Raku`).
        Value::make_instance(Symbol::intern("Raku"), attrs)
    }

    /// Update `$*RAKU.version` to reflect `use v6.x` after parsing.
    ///
    /// Only mutates a `*RAKU`/`?RAKU`/`*PERL`/`?PERL` entry already present
    /// in THIS interpreter's `env` overlay — i.e. only when a previous read
    /// (in this same process) already materialized it via
    /// [`Self::lazy_magic_dynamic_var`]. A not-yet-materialized instance
    /// needs no fixup here: [`Self::make_perl_instance`] reads the current
    /// language version directly at construction time, so its first build
    /// (whenever that happens) already reflects whatever `use v6.x` was in
    /// effect for the most recent completed parse.
    pub(super) fn update_raku_version_from_parser(&mut self) {
        let lang_version = crate::parser::current_language_version();
        if lang_version.is_empty() || lang_version == "6" {
            return;
        }
        let new_version = Self::language_version_value();
        for key in ["*RAKU", "?RAKU", "*PERL", "?PERL"] {
            if let Some(v) = self.env.get(key).cloned()
                && let ValueView::Instance {
                    class_name,
                    attributes,
                    id,
                } = v.view()
            {
                let mut new_attrs = attributes.to_map();
                new_attrs.insert("version".to_string(), new_version.clone());
                let new_val = Value::write_back_sharing(&attributes, class_name, new_attrs, id);
                self.env.insert(key.to_string(), new_val);
            }
        }
    }

    pub(super) fn make_vm_instance() -> Value {
        let mut attrs = ValueMap::default();
        attrs.insert("name".to_string(), Value::str_from("mutsu"));
        attrs.insert("auth".to_string(), Value::str_from("github.com/tokuhirom"));
        attrs.insert(
            "version".to_string(),
            Value::version(
                vec![
                    crate::value::VersionPart::Num(0),
                    crate::value::VersionPart::Num(1),
                    crate::value::VersionPart::Num(0),
                ],
                false,
                false,
            ),
        );
        attrs.insert(
            "signature".to_string(),
            // The `Blob` type object, as in Rakudo.
            Value::package(Symbol::intern("Blob")),
        );
        attrs.insert("desc".to_string(), Value::str_from("mutsu virtual machine"));
        // osname mirrors the build-time OS name MoarVM exposes via `$*VM.osname`
        // (e.g. "linux", "darwin", "mswin32"); zef branches on it (tar/CLI).
        attrs.insert(
            "osname".to_string(),
            Value::str_from(super::io_sysinfo_vm_config::osname()),
        );
        attrs.insert("precomp-ext".to_string(), Value::str_from("mutsu"));
        attrs.insert("precomp-target".to_string(), Value::str_from("mutsu"));
        attrs.insert("prefix".to_string(), Value::str_from("mutsu"));
        // properties: a non-empty hash so the value is truthy.
        let mut props = ValueMap::default();
        props.insert("name".to_string(), Value::str_from("mutsu"));
        attrs.insert("properties".to_string(), Value::hash(props));
        attrs.insert(
            "config".to_string(),
            Value::hash(super::io_sysinfo_vm_config::vm_config()),
        );
        Value::make_instance(Symbol::intern("VM"), attrs)
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;
    use crate::parser::{current_language_version, set_current_language_version};

    /// `make_perl_instance`'s "version" attribute must reflect whatever
    /// `use v6.x` is in effect at construction time (Slice 2 of
    /// todo/tickets/magic-vars-should-be-built-lazily.md switched this from
    /// a hardcoded `Version.new(6)` — see `language_version_value`'s doc
    /// comment for why the two must agree). This is the Rust-level
    /// equivalent of the roast-test-suite-unfriendly `t/` pin: an
    /// `is_run`-based `t/` regression for the same property trips an
    /// unrelated, pre-existing module-resolution/dispatch fragility (see
    /// todo/deep/is-run-after-raku-read-swallows-child-spawn.md), so this
    /// property is pinned here instead, directly against the function that
    /// matters, with no subprocess involved.
    #[test]
    fn make_perl_instance_version_reflects_current_language_version() {
        let saved = current_language_version();
        for v in ["6.c", "6.d", "6.e"] {
            set_current_language_version(v);
            let perl = Interpreter::make_perl_instance();
            let crate::value::ValueView::Instance { attributes, .. } = perl.view() else {
                panic!("make_perl_instance() did not return an Instance");
            };
            let version = attributes.as_map().get("version").cloned().unwrap();
            assert_eq!(
                version.to_string_value(),
                v,
                "make_perl_instance()'s version attribute should reflect \
                 current_language_version() = {v:?} at construction time"
            );
        }
        set_current_language_version(&saved);
    }
}
