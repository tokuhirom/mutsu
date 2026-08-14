use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Process-constant `Kernel` instance (built lazily since Slice 2 of
    /// todo/tickets/magic-vars-should-be-built-lazily.md — see
    /// `Interpreter::lazy_magic_dynamic_var` in `io_env.rs`). Split out of
    /// `io_sysinfo.rs` (docs/conventions: keep source files under 500 lines).
    pub(super) fn make_kernel_instance() -> Value {
        // One cached `uname(2)` covers release, hardware and hostname; see
        // `io_sysinfo_host`. This used to be three `fork`/`exec`s per startup.
        let host = super::io_sysinfo_host::host_info();

        let os = std::env::consts::OS;
        let arch = std::env::consts::ARCH;

        // Kernel name (e.g., "linux", "darwin", "win32")
        let name = match os {
            "macos" => "darwin".to_string(),
            "windows" => "win32".to_string(),
            _ => os.to_string(),
        };

        // Kernel release (e.g., "6.18.7-76061807-generic")
        let release = host.release.clone();

        // Hardware (e.g., "x86_64")
        let hardware = if host.machine.is_empty() {
            arch.to_string()
        } else {
            host.machine.clone()
        };

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

        // Hostname (`uname -n`, the same string the `hostname` command prints)
        let hostname = host.hostname.clone();

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
                    Value::NIL
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
        attrs.insert("desc".to_string(), Value::str_arc(String::new().into()));
        attrs.insert("release".to_string(), Value::str(release));
        attrs.insert("hardware".to_string(), Value::str(hardware));
        attrs.insert("arch".to_string(), Value::str(arch_str));
        attrs.insert("bits".to_string(), Value::int(bits));
        attrs.insert("hostname".to_string(), Value::str(hostname));
        attrs.insert("signals".to_string(), Value::array(signals));

        // endian: Endian enum value matching the host system
        let endian_val = if cfg!(target_endian = "little") {
            Value::enum_parts(
                crate::symbol::Symbol::intern("Endian"),
                crate::symbol::Symbol::intern("LittleEndian"),
                crate::value::EnumValue::Int(1),
                1,
            )
        } else {
            Value::enum_parts(
                crate::symbol::Symbol::intern("Endian"),
                crate::symbol::Symbol::intern("BigEndian"),
                crate::value::EnumValue::Int(2),
                2,
            )
        };
        attrs.insert("endian".to_string(), endian_val);

        Value::make_instance(Symbol::intern("Kernel"), attrs)
    }
}
