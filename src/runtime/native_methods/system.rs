use crate::runtime::*;
use crate::symbol::Symbol;

impl Interpreter {
    // --- Distro ---

    pub(in crate::runtime) fn native_distro(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "name" | "auth" | "desc" | "release" | "path-sep" | "is-win" | "version"
            | "signature" => Ok(attributes.get(method).cloned().unwrap_or(Value::Nil)),
            "gist" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let v = attributes
                    .get("version")
                    .map(|v| {
                        if let Value::Version { parts, .. } = v {
                            Value::version_parts_to_string(parts)
                        } else {
                            v.to_string_value()
                        }
                    })
                    .unwrap_or_default();
                Ok(Value::str(format!("{} ({})", n, v)))
            }
            "Str" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(n))
            }
            "raku" | "perl" => {
                let release = attributes
                    .get("release")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let path_sep = attributes
                    .get("path-sep")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let auth = attributes
                    .get("auth")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let ver = attributes
                    .get("version")
                    .map(|v| {
                        if let Value::Version { parts, .. } = v {
                            format!("v{}", Value::version_parts_to_string(parts))
                        } else {
                            v.to_string_value()
                        }
                    })
                    .unwrap_or_default();
                let desc = attributes
                    .get("desc")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(format!(
                    "Distro.new(release => \"{}\", path-sep => \"{}\", name => \"{}\", auth => \"{}\", version => {}, signature => Blob, desc => \"{}\")",
                    release, path_sep, n, auth, ver, desc
                )))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Distro",
                method
            ))),
        }
    }

    // --- Kernel ---

    pub(in crate::runtime) fn native_kernel(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "name" | "auth" | "desc" | "release" | "hardware" | "arch" | "bits" | "hostname"
            | "version" | "signature" | "signals" => {
                Ok(attributes.get(method).cloned().unwrap_or(Value::Nil))
            }
            "signal" => {
                // .signal(SIGHUP), .signal("SIGHUP"), .signal("HUP"), .signal(Int)
                let arg = args.first().cloned().unwrap_or(Value::Nil);
                let signal_names = [
                    "", "HUP", "INT", "QUIT", "ILL", "TRAP", "ABRT", "BUS", "FPE", "KILL", "USR1",
                    "SEGV", "USR2", "PIPE", "ALRM", "TERM", "STKFLT", "CHLD", "CONT", "STOP",
                    "TSTP", "TTIN", "TTOU", "URG", "XCPU", "XFSZ", "VTALRM", "PROF", "WINCH", "IO",
                    "PWR", "SYS",
                ];
                match &arg {
                    Value::Int(n) => Ok(Value::Int(*n)),
                    Value::Str(s) => {
                        let name = s
                            .strip_prefix("SIG")
                            .map(|s| s.to_string())
                            .unwrap_or_else(|| s.to_string());
                        for (i, sn) in signal_names.iter().enumerate() {
                            if *sn == name {
                                return Ok(Value::Int(i as i64));
                            }
                        }
                        Ok(Value::Int(0))
                    }
                    Value::Enum { key, .. } => {
                        let key_str = key.resolve();
                        let name = key_str.strip_prefix("SIG").unwrap_or(&key_str);
                        for (i, sn) in signal_names.iter().enumerate() {
                            if *sn == name {
                                return Ok(Value::Int(i as i64));
                            }
                        }
                        Ok(Value::Int(0))
                    }
                    _ => Ok(Value::Int(0)),
                }
            }
            "gist" | "Str" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(n))
            }
            "raku" | "perl" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let auth = attributes
                    .get("auth")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(format!(
                    "Kernel.new(release => Str, hardware => Str, arch => Str, bits => Int, name => \"{}\", auth => \"{}\", version => Version, signature => Blob, desc => Str)",
                    n, auth
                )))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Kernel",
                method
            ))),
        }
    }

    // --- VM ---

    pub(in crate::runtime) fn native_vm(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "name" | "auth" | "version" | "precomp-ext" | "precomp-target" | "prefix" | "desc"
            | "signature" | "config" | "properties" => {
                Ok(attributes.get(method).cloned().unwrap_or(Value::Nil))
            }
            "raku" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(format!("{}.new(...)", name)))
            }
            "request-garbage-collection" => {
                // Clear persisted closure environments to release stale Instance
                // references that would otherwise prevent DESTROY from firing.
                // This mimics a real GC that would trace live references and
                // collect unreachable objects.
                self.closure_env_overrides.clear();
                // Process pending DESTROY submethods for objects whose refcount
                // dropped to 0 (possibly including items freed by the clear above).
                self.run_pending_instance_destroys()?;
                Ok(Value::Nil)
            }
            "gist" | "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(name))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on VM",
                method
            ))),
        }
    }

    // --- Perl ---

    pub(in crate::runtime) fn native_perl(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Value {
        match method {
            "compiler" => {
                let mut compiler_attrs = HashMap::new();
                compiler_attrs.insert("name".to_string(), Value::str_from("mutsu"));
                compiler_attrs.insert("auth".to_string(), Value::str_from("github.com/tokuhirom"));
                compiler_attrs.insert(
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
                compiler_attrs.insert(
                    "signature".to_string(),
                    Value::make_instance(Symbol::intern("Blob"), {
                        let mut a = HashMap::new();
                        a.insert("values".to_string(), Value::array(vec![Value::Int(0)]));
                        a
                    }),
                );
                compiler_attrs.insert(
                    "desc".to_string(),
                    Value::str_from("mutsu Raku interpreter"),
                );
                compiler_attrs.insert("release".to_string(), Value::str_from("0.1.0"));
                compiler_attrs.insert("codename".to_string(), Value::str_from("mutsu"));
                compiler_attrs.insert("id".to_string(), Value::str(String::new()));
                Value::make_instance(Symbol::intern("Compiler"), compiler_attrs)
            }
            "backend" => Value::str_from("mutsu"),
            "gist" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let version = attributes
                    .get("version")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(format!("{} ({})", name, version))
            }
            "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(name)
            }
            "raku" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(format!("{}.new(...)", name))
            }
            _ => attributes.get(method).cloned().unwrap_or(Value::Nil),
        }
    }
}
