use super::*;

impl Interpreter {
    /// Build a `Failure` value wrapping an `X::IO::*` exception with the given
    /// type name and offending path. The Failure throws the exception when sunk.
    fn make_io_failure(&self, ex_type: &str, path: &str) -> Value {
        let mut ex_attrs = HashMap::new();
        ex_attrs.insert("path".to_string(), Value::str(path.to_string()));
        ex_attrs.insert(
            "message".to_string(),
            Value::str(format!("{}: {}", ex_type, path)),
        );
        let exception = Value::make_instance(Symbol::intern(ex_type), ex_attrs);
        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::Bool(false));
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    pub(crate) fn native_io_path(
        &mut self,
        attributes: &HashMap<String, Value>,
        class_name: &str,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Pure lexical path methods (`.parent`/`.add`/`.basename`/`.sibling`/…)
        // are handled by the single shared `try_io_path_lexical` (the same impl
        // the bytecode VM dispatches natively). Only the filesystem / cwd-relative
        // forms below need `&self`.
        if let Some(result) = Self::try_io_path_lexical(class_name, attributes, method, &args) {
            return result;
        }
        // `.absolute` / `.relative` derive a string from the path + cwd (lexical,
        // no filesystem) — handled by the shared `try_io_path_cwd_method` the VM
        // also dispatches natively.
        if let Some(result) = self.try_io_path_cwd_method(attributes, method, &args) {
            return result;
        }
        // Filesystem `stat`-only predicates / accessors (`e`/`f`/`d`/…/`s`/
        // `modified`) — `stat` reads only, no `io_handles`, shared with the VM's
        // native dispatch via `try_io_path_fs_stat`.
        if let Some(result) = self.try_io_path_fs_stat(attributes, method) {
            return result;
        }
        // Whole-file content reads (`slurp`/`lines`/`words`) — read the file,
        // split/decode the bytes; no `io_handles`, shared with the VM's native
        // dispatch via `try_io_path_content_read`.
        if let Some(result) = self.try_io_path_content_read(attributes, method, &args) {
            return result;
        }
        // Single-path filesystem mutations (`spurt`/`mkdir`/`rmdir`/`unlink`/
        // `chmod`) — one-shot syscall, no `io_handles`, shared with the VM's
        // native dispatch via `try_io_path_fs_mutate`.
        if let Some(result) = self.try_io_path_fs_mutate(attributes, class_name, method, &args) {
            return result;
        }
        // `open` allocates an `io_handles` entry and returns an `IO::Handle`. The
        // VM owns `io_handles`, so it dispatches `open` natively via the shared
        // `try_io_path_open` (ledger §D ③).
        if let Some(result) = self.try_io_path_open(attributes, method, &args) {
            return result;
        }
        // Two-path FS ops (`copy`/`rename`/`move`/`symlink`/`link`) — resolve both
        // paths against the VM-owned cwd, one-shot syscall, no `io_handles`, shared
        // with the VM's native dispatch via `try_io_path_two_path_op`.
        if let Some(result) = self.try_io_path_two_path_op(attributes, method, &args) {
            return result;
        }
        // `comb` reads the whole file then combs the content (matcher dispatch is
        // `&mut self` but touches no `io_handles`) — shared with the VM's native
        // dispatch via `try_io_path_comb`.
        if let Some(result) = self.try_io_path_comb(attributes, method, &args) {
            return result;
        }
        // The concrete class of the receiver (`IO::Path` or a SPEC-variant
        // subclass `IO::Path::Unix`/`::Win32`/`::Cygwin`/`::QNX`). Path-deriving
        // methods (`.child :secure`, ...) must round-trip this class so e.g.
        // `IO::Path::Win32.new("x").parent(0)` stays an `IO::Path::Win32`
        // (Rakudo preserves the subclass; `is-deeply`/`eqv` compares it).
        let io_path_class = Symbol::intern(class_name);
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let instance_cwd = attributes.get("cwd").map(|v| v.to_string_value());
        let path_buf = if Path::new(&p).is_absolute() {
            self.resolve_path(&p)
        } else if let Some(cwd) = &instance_cwd {
            self.apply_chroot(PathBuf::from(cwd).join(Path::new(&p)))
        } else {
            self.resolve_path(&p)
        };
        let cwd_path = self.get_cwd_path();
        let original = Path::new(&p);
        match method {
            "raku" | "perl" => {
                let escape = |s: &str| {
                    s.replace('\\', "\\\\")
                        .replace('"', "\\\"")
                        .replace('\n', "\\n")
                        .replace('\t', "\\t")
                        .replace('\r', "\\r")
                        .replace('\0', "\\0")
                };
                let cwd = instance_cwd.unwrap_or_else(|| Self::stringify_path(&cwd_path));
                // Derive class name from SPEC attribute
                let class_name = attributes
                    .get("SPEC")
                    .and_then(|s| {
                        let name = match s {
                            Value::Package(n) => n.resolve().to_string(),
                            Value::Instance { class_name, .. } => class_name.resolve().to_string(),
                            _ => return None,
                        };
                        // IO::Spec::Win32 -> IO::Path::Win32
                        name.strip_prefix("IO::Spec::")
                            .map(|rest| format!("IO::Path::{}", rest))
                    })
                    .unwrap_or_else(|| "IO::Path".to_string());
                Ok(Value::str(format!(
                    "{}.new(\"{}\", :CWD(\"{}\"))",
                    class_name,
                    escape(&p),
                    escape(&cwd)
                )))
            }
            "CWD" => {
                let cwd = instance_cwd.unwrap_or_else(|| Self::stringify_path(&cwd_path));
                Ok(Value::str(cwd))
            }
            // IO::Path is Cool: `.Numeric`/`.Int`/`.Rat`/`.Num`/`.FatRat`/`.Real`
            // coerce the *basename* to a number, failing with an X::Str::Numeric
            // Failure (a soft fail, what `fails-like` expects) when the basename
            // is not numerical (raku-doc Type/IO/Path: method Numeric / method Int).
            "Numeric" | "Real" | "Int" | "Rat" | "Num" | "FatRat" => {
                let (_, _, bname) = Self::io_path_parts_spec(&p, attributes);
                let bname_val = Value::str(bname.clone());
                if crate::runtime::str_numeric::parse_raku_str_to_numeric(&bname).is_some() {
                    // Per the spec, IO::Path's `.Int`/`.Rat`/`.Num`/`.FatRat` fall
                    // out of `.Numeric` (Cool): coerce the basename to its natural
                    // numeric first (so "3.5" -> Rat, "1+1i" -> Complex), then to
                    // the requested type. Going straight to `Str.Num` would choke on
                    // a complex-valued basename like "3+0i".
                    let numeric = self.call_method_with_values(bname_val, "Numeric", vec![])?;
                    match method {
                        "Numeric" | "Real" => Ok(numeric),
                        _ => self.call_method_with_values(numeric, method, args),
                    }
                } else {
                    // Non-numerical basename: fail with an X::Str::Numeric Failure
                    // (a soft fail, what `fails-like` expects), not a thrown error.
                    let err = crate::runtime::utils::check_str_numeric(&bname_val)
                        .err()
                        .unwrap_or_else(|| {
                            crate::runtime::utils::str_numeric_error(&bname, 0, "malformed number")
                        });
                    Ok(self.fail_error_to_failure_value(&err))
                }
            }
            // `.child($name, :secure)` resolves the path against the filesystem.
            // (Plain `.child`/`.add` are pure lexical joins handled by
            // `try_io_path_lexical` before this match.)
            "child" => {
                let child_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let joined = Self::io_path_join_child(attributes, &p, &child_name)?;
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(joined.clone()));
                let child = Value::make_instance(io_path_class, new_attrs);
                // `.child($name, :secure)` verifies that the resulting path is a
                // real child of the (completely resolved) parent. It fails with
                // X::IO::Resolve when the parent or the child path cannot be
                // completely resolved, and with X::IO::NotAChild when the
                // resolved child escapes the parent directory.
                if Self::named_bool(&args, "secure") {
                    let parent_abs = if original.is_absolute() {
                        path_buf.clone()
                    } else if let Some(cwd) = &instance_cwd {
                        PathBuf::from(cwd).join(original)
                    } else {
                        cwd_path.join(original)
                    };
                    let child_path = Path::new(&joined);
                    let child_abs = if child_path.is_absolute() {
                        child_path.to_path_buf()
                    } else if let Some(cwd) = &instance_cwd {
                        PathBuf::from(cwd).join(child_path)
                    } else {
                        cwd_path.join(child_path)
                    };
                    let res_parent = Self::resolve_io_path(&parent_abs, true, &p);
                    let res_child = Self::resolve_io_path(&child_abs, true, &joined);
                    match (res_parent, res_child) {
                        (Err(_), _) | (_, Err(_)) => {
                            return Ok(self.make_io_failure("X::IO::Resolve", &p));
                        }
                        (Ok(rp), Ok(rc)) => {
                            let sep = Self::io_path_sep(attributes);
                            let prefix = format!("{}{}", rp, sep);
                            if !rc.starts_with(&prefix) || rc == rp {
                                return Ok(self.make_io_failure("X::IO::NotAChild", &joined));
                            }
                        }
                    }
                }
                Ok(child)
            }
            "resolve" => {
                let completely = Self::named_bool(&args, "completely");
                let resolved = match Self::resolve_io_path(&path_buf, completely, &p) {
                    Ok(r) => r,
                    // `.resolve(:completely)` fails (returns a Failure) rather
                    // than throwing when the path cannot be fully resolved.
                    Err(_) => return Ok(self.make_io_failure("X::IO::Resolve", &p)),
                };
                // A resolved path is absolute, so its CWD becomes the volume root
                // (the SPEC's dir separator on POSIX).
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(resolved));
                let sep = Self::io_path_sep(attributes).to_string();
                new_attrs.insert("cwd".to_string(), Value::str(sep));
                Ok(Value::make_instance(io_path_class, new_attrs))
            }
            // `e`/`f`/`d`/`l`/`r`/`w`/`x`/`rw`/`rwx`/`z`/`mode`/`s`/`created`/
            // `modified`/`accessed`/`changed` (stat-only) are handled above by the
            // shared `try_io_path_fs_stat`, which the VM also dispatches natively.
            // `lines`/`words` (whole-file content reads) are handled above by the
            // shared `try_io_path_content_read`, which the VM also dispatches
            // natively. `comb` is handled above by the shared `try_io_path_comb`,
            // which the VM also dispatches natively.
            // `slurp` (whole-file content read) is handled above by the shared
            // `try_io_path_content_read`, which the VM also dispatches natively.
            // `open` (allocates an `io_handles` entry) is handled above by the
            // shared `try_io_path_open`, which the VM also dispatches natively.
            // `copy`/`rename`/`move` (two-path FS ops) are handled above by the
            // shared `try_io_path_two_path_op`, which the VM also dispatches natively.
            // `chmod`/`mkdir`/`rmdir` (single-path FS mutations) are handled above
            // by the shared `try_io_path_fs_mutate`, which the VM also dispatches
            // natively.
            "dir" => {
                let mut entries = Vec::new();
                let requested = PathBuf::from(&p);
                let requested_is_absolute = requested.is_absolute();
                let make_entry = |out_path: PathBuf| {
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "path".to_string(),
                        Value::str(Self::stringify_path(&out_path)),
                    );
                    if let Some(cwd) = &instance_cwd
                        && !out_path.is_absolute()
                    {
                        attrs.insert("cwd".to_string(), Value::str(cwd.clone()));
                    }
                    Value::make_instance(io_path_class, attrs)
                };
                for entry in fs::read_dir(&path_buf).map_err(|err| {
                    RuntimeError::new(format!("Failed to read dir '{}': {}", p, err))
                })? {
                    let entry = entry.map_err(|err| {
                        RuntimeError::new(format!("Failed to read dir entry '{}': {}", p, err))
                    })?;
                    let file_name = entry.file_name();
                    let out_path = if requested_is_absolute {
                        path_buf.join(&file_name)
                    } else if p == "." {
                        PathBuf::from(&file_name)
                    } else {
                        requested.join(&file_name)
                    };
                    entries.push(make_entry(out_path));
                }
                Ok(Value::array(entries))
            }
            // `spurt`/`unlink` (single-path FS mutations) are handled above by the
            // shared `try_io_path_fs_mutate`, which the VM also dispatches natively.
            // `symlink`/`link` (two-path FS ops) are handled above by the shared
            // `try_io_path_two_path_op`, which the VM also dispatches natively.
            "watch" => {
                let supply_id = super::native_methods::next_supply_id();
                let (tx, rx) = std::sync::mpsc::channel();
                if let Ok(mut map) = super::native_methods::supply_channel_map_pub().lock() {
                    map.insert(supply_id, rx);
                }

                let watched_path = path_buf.clone();
                std::thread::spawn(move || {
                    let poll_interval = std::time::Duration::from_millis(10);
                    let mut last_state = fs::metadata(&watched_path).ok().map(|meta| {
                        let modified = meta
                            .modified()
                            .ok()
                            .and_then(|ts| ts.duration_since(std::time::UNIX_EPOCH).ok())
                            .map(|dur| dur.as_nanos())
                            .unwrap_or(0);
                        (meta.len(), modified)
                    });

                    loop {
                        let state = fs::metadata(&watched_path).ok().map(|meta| {
                            let modified = meta
                                .modified()
                                .ok()
                                .and_then(|ts| ts.duration_since(std::time::UNIX_EPOCH).ok())
                                .map(|dur| dur.as_nanos())
                                .unwrap_or(0);
                            (meta.len(), modified)
                        });

                        if state != last_state {
                            // Emit the watched path on each observable filesystem change.
                            if tx
                                .send(super::native_methods::SupplyEvent::Emit(Value::str(
                                    Self::stringify_path(&watched_path),
                                )))
                                .is_err()
                            {
                                break;
                            }
                            last_state = state;
                        }

                        std::thread::sleep(poll_interval);
                    }
                });

                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(Vec::new()));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("supply_id".to_string(), Value::Int(supply_id as i64));
                Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Path",
                method
            ))),
        }
    }
}
