use super::*;
use crate::symbol::Symbol;
use num_traits::ToPrimitive;
use std::path::Component;
use unicode_segmentation::UnicodeSegmentation;

/// Coerce a positional limit argument (the `$limit` of `.lines`/`.words`/`.get`
/// reads) to a row count. Accepts any non-negative numeric, including an
/// allomorph (`<3>`, `<3e0>`, `<3+0i>`) by unwrapping the `Mixin` to its inner
/// numeric. Returns `None` for non-numeric args, `*`/`Whatever`, and `+Inf`
/// (all meaning "no limit").
fn numeric_limit_arg(arg: &Value) -> Option<usize> {
    match arg {
        Value::Int(i) => Some((*i).max(0) as usize),
        Value::BigInt(bi) => Some(bi.to_usize().unwrap_or(usize::MAX)),
        Value::Num(f) if f.is_infinite() => None,
        Value::Num(f) if *f >= 0.0 => Some(*f as usize),
        Value::Rat(n, d) if *d != 0 => Some(((*n as f64 / *d as f64) as i64).max(0) as usize),
        Value::Complex(re, im) if *im == 0.0 && *re >= 0.0 => Some(*re as usize),
        Value::Mixin(inner, _) => numeric_limit_arg(inner),
        _ => None,
    }
}

fn io_exception(class_name: &str, message: String) -> RuntimeError {
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern(class_name),
        HashMap::new(),
    )));
    err
}

fn io_exception_failure(class_name: &str, message: String) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message));
    let ex = Value::make_instance(Symbol::intern(class_name), attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

enum IoPathExtensionPartsSpec {
    Exact(i64),
    Range { low: i64, high: i64 },
}

fn io_path_missing_failure(path: &str, method: &str) -> Value {
    let message = format!("Failed to find '{}' while trying to do '.{}'", path, method);
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message));
    attrs.insert("path".to_string(), Value::str(path.to_string()));
    attrs.insert("trying".to_string(), Value::str(method.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::IO::DoesNotExist"), attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

fn io_path_missing_error(path: &str, method: &str) -> RuntimeError {
    let message = format!("Failed to find '{}' while trying to do '.{}'", path, method);
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("path".to_string(), Value::str(path.to_string()));
    attrs.insert("trying".to_string(), Value::str(method.to_string()));
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::IO::DoesNotExist"),
        attrs,
    )));
    err
}

fn io_path_metadata(
    path: &Path,
    display_path: &str,
    method: &str,
) -> Result<fs::Metadata, RuntimeError> {
    fs::metadata(path).map_err(|_| io_path_missing_error(display_path, method))
}

#[cfg(unix)]
fn path_access(path: &Path, mode: libc::c_int) -> bool {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;
    let Ok(cpath) = CString::new(path.as_os_str().as_bytes()) else {
        return false;
    };
    unsafe { libc::access(cpath.as_ptr(), mode) == 0 }
}

#[cfg(unix)]
fn path_is_readable(path: &Path) -> bool {
    path_access(path, libc::R_OK)
}

#[cfg(unix)]
fn path_is_writable(path: &Path) -> bool {
    path_access(path, libc::W_OK)
}

#[cfg(unix)]
fn path_is_executable(path: &Path) -> bool {
    path_access(path, libc::X_OK)
}

#[cfg(not(unix))]
fn path_is_readable(path: &Path) -> bool {
    fs::metadata(path).is_ok()
}

#[cfg(not(unix))]
fn path_is_writable(path: &Path) -> bool {
    fs::metadata(path)
        .map(|m| !m.permissions().readonly())
        .unwrap_or(false)
}

#[cfg(not(unix))]
fn path_is_executable(path: &Path) -> bool {
    fs::metadata(path).map(|m| m.is_file()).unwrap_or(false)
}

impl IoPathExtensionPartsSpec {
    fn select(&self, available: i64) -> Option<i64> {
        match self {
            Self::Exact(n) => {
                if *n <= available {
                    Some(*n)
                } else {
                    None
                }
            }
            Self::Range { low, high } => {
                if low > high {
                    return None;
                }
                let best = available.min(*high);
                if best < *low { None } else { Some(best) }
            }
        }
    }
}

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

    /// Join a `child`/`add` name onto a path lexically (no filesystem access),
    /// honoring the receiver's SPEC (Win32 vs POSIX separators). Raises
    /// `X::IO::Null` for an embedded null byte. Shared by `try_io_path_lexical`
    /// (the pure fast path) and `native_io_path`'s `child :secure` arm so the
    /// join is implemented once.
    fn io_path_join_child(
        attributes: &HashMap<String, Value>,
        p: &str,
        child_name: &str,
    ) -> Result<String, RuntimeError> {
        if child_name.contains('\0') || p.contains('\0') {
            return Err(RuntimeError::new(
                "X::IO::Null: Found null byte in pathname",
            ));
        }
        let joined = if Self::is_win32_spec(attributes) {
            if p == "." {
                child_name.to_string()
            } else if p.ends_with('\\') || p.ends_with('/') {
                format!("{}{}", p, child_name)
            } else {
                format!("{}\\{}", p, child_name)
            }
        } else if p == "." {
            child_name.to_string()
        } else if p.ends_with('/') {
            format!("{}{}", p, child_name)
        } else {
            Self::stringify_path(&Path::new(p).join(child_name))
        };
        Ok(joined)
    }

    /// Whether `class_name` is one of the built-in `IO::Path` family classes
    /// (`IO::Path` plus the SPEC-variant subclasses) whose pure lexical methods
    /// the VM may dispatch natively via [`try_io_path_lexical`](Self::try_io_path_lexical).
    /// A user subclass that overrides a method is dispatched as compiled bytecode
    /// before the catch-all is reached, so restricting the native fast path to the
    /// built-in family keeps any user override authoritative.
    pub(crate) fn is_io_path_lexical_class(class_name: &str) -> bool {
        matches!(
            class_name,
            "IO::Path"
                | "IO::Path::Unix"
                | "IO::Path::Win32"
                | "IO::Path::Cygwin"
                | "IO::Path::QNX"
        )
    }

    /// Pure, lexical `IO::Path` methods: those that derive a new path (or a
    /// string/bool) purely from the receiver's `path`/`SPEC` *attributes* and the
    /// arguments, with no filesystem access, no cwd resolution (`&self`), and no
    /// env. This is the single authoritative implementation shared by the
    /// bytecode VM's catch-all native dispatch (`try_compiled_method_*`) and the
    /// interpreter's [`native_io_path`](Self::native_io_path), which delegates to
    /// it (`1 operation = 1 implementation`).
    ///
    /// Returns `None` (fall through to `native_io_path`'s richer handling) for any
    /// method that needs interpreter-owned state: filesystem stat/read/write
    /// (`e`/`f`/`d`/`slurp`/`spurt`/`open`/`resolve`/…), cwd-relative forms
    /// (`absolute`/`relative`/`CWD`/`raku`), numeric coercion that re-dispatches
    /// through `self` (`Numeric`/`Int`/…), and `child :secure` (which resolves the
    /// path against the filesystem). Path-deriving methods round-trip the concrete
    /// receiver class (`IO::Path::Win32.parent` stays `IO::Path::Win32`).
    pub(crate) fn try_io_path_lexical(
        class_name: &str,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let io_path_class = Symbol::intern(class_name);
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let original = Path::new(&p);
        let result = match method {
            "Str" => Ok(Value::str(p.clone())),
            "gist" => {
                let shown = if Self::io_path_is_absolute_spec(attributes, &p, original) {
                    if Self::is_win32_spec(attributes) {
                        Self::canonpath_win32(&p, false)
                    } else if Self::is_cygwin_spec(attributes) {
                        Self::canonpath_cygwin(&p.replace('\\', "/"), false)
                    } else {
                        p.clone()
                    }
                } else {
                    p.clone()
                };
                Ok(Value::str(format!("\"{}\".IO", shown.replace('"', "\\\""))))
            }
            "IO" => Ok(Value::make_instance(io_path_class, attributes.clone())),
            "SPEC" => {
                let spec_name = attributes
                    .get("SPEC")
                    .and_then(|s| match s {
                        Value::Package(n) => Some(n.resolve().to_string()),
                        Value::Instance { class_name, .. } => {
                            Some(class_name.resolve().to_string())
                        }
                        _ => None,
                    })
                    .unwrap_or_else(|| "IO::Spec::Unix".to_string());
                Ok(Value::Package(Symbol::intern(&spec_name)))
            }
            "basename" => {
                let (_, _, bname) = Self::io_path_parts_spec(&p, attributes);
                Ok(Value::str(bname))
            }
            "dirname" => {
                let (_, dname, _) = Self::io_path_parts_spec(&p, attributes);
                Ok(Value::str(dname))
            }
            "volume" => {
                let (volume, _, _) = Self::io_path_parts_spec(&p, attributes);
                Ok(Value::str(volume))
            }
            "cleanup" => {
                let normalized = if Self::is_win32_spec(attributes) {
                    Self::cleanup_io_path_lexical_win32(&p)
                } else {
                    Self::cleanup_io_path_lexical(&p)
                };
                Ok(Self::clone_io_path_with_path(
                    attributes,
                    io_path_class,
                    normalized,
                ))
            }
            "parts" => {
                let (volume, dirname, basename) = Self::io_path_parts_spec(&p, attributes);
                let mut parts = HashMap::new();
                parts.insert("volume".to_string(), Value::str(volume));
                parts.insert("dirname".to_string(), Value::str(dirname));
                parts.insert("basename".to_string(), Value::str(basename));
                Ok(Value::hash(parts))
            }
            "parent" => {
                let mut levels = 1i64;
                if let Some(Value::Int(i)) = args.first() {
                    if *i < 0 {
                        return Some(Err(RuntimeError::new(
                            "X::IO::ParentOutOfRange: Cannot go to a negative parent",
                        )));
                    }
                    levels = *i;
                }
                if levels == 0 {
                    return Some(Ok(Value::make_instance(io_path_class, attributes.clone())));
                }
                let sep = Self::io_path_sep(attributes);
                let mut path = if Self::is_cygwin_spec(attributes) {
                    p.replace('\\', "/")
                } else {
                    p.clone()
                };
                for _ in 0..levels {
                    if path == "." {
                        path = "..".to_string();
                        continue;
                    }
                    if path == ".." {
                        path = format!("..{}{}", sep, "..");
                        continue;
                    }
                    let (volume, dirname, _basename) = Self::io_path_parts(&path);
                    let full_vol_dir = format!("{}{}", volume, dirname);
                    if dirname == "/" || dirname == "\\" {
                        let new_path = format!("{}{}", volume, dirname);
                        if new_path == path {
                            break;
                        }
                        path = new_path;
                    } else if dirname == "." && volume.is_empty() {
                        path = ".".to_string();
                    } else {
                        path = full_vol_dir;
                    }
                }
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(path));
                Ok(Value::make_instance(io_path_class, new_attrs))
            }
            "sibling" => {
                let sibling_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let (volume, dirname, _) = Self::io_path_parts(&p);
                let dir_with_volume = format!("{}{}", volume, dirname);
                let sibling_path = if dir_with_volume == "/" || dir_with_volume == "\\" {
                    format!("{}{}", dir_with_volume, sibling_name)
                } else if dir_with_volume == "." {
                    sibling_name
                } else if dir_with_volume.ends_with('/') || dir_with_volume.ends_with('\\') {
                    format!("{}{}", dir_with_volume, sibling_name)
                } else {
                    format!("{}/{}", dir_with_volume, sibling_name)
                };
                Ok(Self::clone_io_path_with_path(
                    attributes,
                    io_path_class,
                    sibling_path,
                ))
            }
            // `.add($name)` and `.child($name)` without `:secure` are pure lexical
            // joins. `.child(:secure)` resolves against the filesystem and stays in
            // `native_io_path`.
            "add" | "child" => {
                if method == "child" && Self::named_bool(args, "secure") {
                    return None;
                }
                let child_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                match Self::io_path_join_child(attributes, &p, &child_name) {
                    Ok(joined) => {
                        let mut new_attrs = attributes.clone();
                        new_attrs.insert("path".to_string(), Value::str(joined));
                        Ok(Value::make_instance(io_path_class, new_attrs))
                    }
                    Err(e) => Err(e),
                }
            }
            "extension" => (|| -> Result<Value, RuntimeError> {
                let subst = Self::positional_value(args, 0).map(|v| v.to_string_value());
                let parts_spec = Self::io_path_extension_parts_spec(args)?;
                let selected_parts = parts_spec.select(Self::io_path_extension_part_count(&p));

                if let Some(subst) = subst {
                    let Some(parts_to_replace) = selected_parts else {
                        return Ok(Value::make_instance(io_path_class, attributes.clone()));
                    };
                    let joiner = Self::named_value(args, "joiner")
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| {
                            if subst.is_empty() {
                                "".to_string()
                            } else {
                                ".".to_string()
                            }
                        });
                    let (dir_prefix, basename) = Self::split_path_for_extension(&p);
                    let Some(base_without_ext) =
                        Self::io_path_extension_strip_n_parts(basename, parts_to_replace)
                    else {
                        return Ok(Value::make_instance(io_path_class, attributes.clone()));
                    };
                    let mut new_basename = format!("{base_without_ext}{joiner}{subst}");
                    if new_basename.is_empty() {
                        new_basename = ".".to_string();
                    }
                    let mut new_attrs = attributes.clone();
                    new_attrs.insert(
                        "path".to_string(),
                        Value::str(format!("{dir_prefix}{new_basename}")),
                    );
                    Ok(Value::make_instance(io_path_class, new_attrs))
                } else {
                    let Some(parts) = selected_parts else {
                        return Ok(Value::str(String::new()));
                    };
                    Ok(Value::str(
                        Self::io_path_extension_with_n_parts(&p, parts).unwrap_or_default(),
                    ))
                }
            })(),
            "starts-with" => {
                let prefix = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::Bool(p.starts_with(&prefix)))
            }
            "is-absolute" => Ok(Value::Bool(Self::io_path_is_absolute_spec(
                attributes, &p, original,
            ))),
            "is-relative" => Ok(Value::Bool(!Self::io_path_is_absolute_spec(
                attributes, &p, original,
            ))),
            "succ" => {
                let (volume, dirname, basename) = Self::io_path_parts(&p);
                let new_basename = crate::builtins::str_increment::string_succ(&basename);
                let sep = Self::io_path_sep(attributes);
                let new_path = Self::join_io_path_parts(&volume, &dirname, &new_basename, sep);
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(new_path));
                Ok(Value::make_instance(io_path_class, new_attrs))
            }
            "pred" => {
                let (volume, dirname, basename) = Self::io_path_parts(&p);
                let new_basename = crate::builtins::str_increment::string_pred(&basename);
                let sep = Self::io_path_sep(attributes);
                let new_path = Self::join_io_path_parts(&volume, &dirname, &new_basename, sep);
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(new_path));
                Ok(Value::make_instance(io_path_class, new_attrs))
            }
            _ => return None,
        };
        Some(result)
    }

    /// `.absolute` / `.relative` on an `IO::Path`: like [`try_io_path_lexical`],
    /// these derive a string from the path, but additionally depend on the
    /// **cwd** (`$*CWD` / the instance `cwd` attribute / the process cwd) — read
    /// through `&self` (`resolve_path`/`get_cwd_path`/`apply_chroot`), which are
    /// purely lexical (no filesystem access). The VM owns env/cwd, so this is a
    /// native dispatch; the single shared impl that `native_io_path` delegates to.
    /// Returns `None` for any other method (`.resolve` canonicalizes against the
    /// real filesystem and stays in `native_io_path`).
    pub(crate) fn try_io_path_cwd_method(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "absolute" | "relative") {
            return None;
        }
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
        Some(match method {
            "absolute" => {
                if Self::is_win32_spec(attributes) {
                    let base = args
                        .first()
                        .map(|v| v.to_string_value())
                        .or_else(|| instance_cwd.clone())
                        .unwrap_or_else(|| Self::stringify_path(&cwd_path));
                    let abs = if Self::io_path_is_absolute_win32(&p) {
                        p.clone()
                    } else {
                        let sep = '\\';
                        if base.ends_with('\\') || base.ends_with('/') {
                            format!("{}{}", base, p)
                        } else {
                            format!("{}{}{}", base, sep, p)
                        }
                    };
                    let cleaned = Self::canonpath_win32(&abs, false);
                    Ok(Value::str(cleaned))
                } else if Self::is_cygwin_spec(attributes) {
                    let base = args
                        .first()
                        .map(|v| v.to_string_value())
                        .or_else(|| instance_cwd.clone())
                        .unwrap_or_else(|| Self::stringify_path(&cwd_path));
                    let pn = p.replace('\\', "/");
                    let abs = if Self::io_path_is_absolute_win32(&pn) {
                        pn
                    } else {
                        let bn = base.replace('\\', "/");
                        if bn.ends_with('/') {
                            format!("{}{}", bn, pn)
                        } else {
                            format!("{}/{}", bn, pn)
                        }
                    };
                    Ok(Value::str(Self::canonpath_cygwin(&abs, false)))
                } else {
                    let base = args.first().map(|v| v.to_string_value());
                    if let Some(base) = base {
                        if original.is_absolute() {
                            Ok(Value::str(p.clone()))
                        } else {
                            let joined = PathBuf::from(&base).join(&p);
                            Ok(Value::str(Self::stringify_path(&joined)))
                        }
                    } else {
                        let absolute = Self::stringify_path(&path_buf);
                        Ok(Value::str(absolute))
                    }
                }
            }
            "relative" => {
                if Self::is_win32_spec(attributes) {
                    let base = args
                        .first()
                        .map(|v| v.to_string_value())
                        .or_else(|| instance_cwd.clone())
                        .unwrap_or_else(|| Self::stringify_path(&cwd_path));
                    let norm_p = p.replace('/', "\\");
                    let norm_base = base.replace('/', "\\");
                    let rel = norm_p
                        .strip_prefix(&norm_base)
                        .and_then(|r| r.strip_prefix('\\'))
                        .unwrap_or(&norm_p);
                    Ok(Value::str(rel.to_string()))
                } else if Self::is_cygwin_spec(attributes) {
                    let base = args
                        .first()
                        .map(|v| v.to_string_value())
                        .or_else(|| instance_cwd.clone())
                        .unwrap_or_else(|| Self::stringify_path(&cwd_path));
                    let norm_p = p.replace('\\', "/");
                    let norm_base = base.replace('\\', "/");
                    let rel = norm_p
                        .strip_prefix(&norm_base)
                        .and_then(|r| r.strip_prefix('/'))
                        .unwrap_or(&norm_p);
                    Ok(Value::str(rel.to_string()))
                } else {
                    let rel_base_arg = args.first().map(|v| v.to_string_value());
                    let rel_base = rel_base_arg
                        .as_ref()
                        .map(PathBuf::from)
                        .or_else(|| instance_cwd.as_ref().map(PathBuf::from))
                        .unwrap_or_else(|| cwd_path.clone());
                    let rel = path_buf
                        .strip_prefix(&rel_base)
                        .map(Self::stringify_path)
                        .unwrap_or_else(|_| Self::stringify_path(&path_buf));
                    Ok(Value::str(rel))
                }
            }
            _ => unreachable!(),
        })
    }

    /// Filesystem `stat`-only predicates / accessors on an `IO::Path`
    /// (`e`/`f`/`d`/`l`/`r`/`w`/`x`/`rw`/`rwx`/`z` file tests and the
    /// `mode`/`s`/`created`/`modified`/`accessed`/`changed` stat readers). They
    /// resolve the receiver's path against the cwd (`&self`, VM-owned env) and
    /// then read the filesystem via `stat` only — no `io_handles` allocation, no
    /// `emit_output`, no encoding/content read. So the VM can dispatch them
    /// natively (ledger §D): the single shared impl that `native_io_path`
    /// delegates to. Returns `None` for any other method (content reads
    /// `slurp`/`lines`/handle-opening `open`/`spurt`, which need flags/encoding/
    /// `io_handles` and stay in `native_io_path`).
    pub(crate) fn try_io_path_fs_stat(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "e" | "f"
                | "d"
                | "l"
                | "r"
                | "w"
                | "x"
                | "rw"
                | "rwx"
                | "z"
                | "mode"
                | "s"
                | "created"
                | "modified"
                | "accessed"
                | "changed"
        ) {
            return None;
        }
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        Some(Self::io_path_stat_result(&path_buf, &p, method))
    }

    /// Resolve an `IO::Path`'s `path` attribute to an absolute filesystem
    /// `PathBuf` against the VM-owned cwd (`$*CWD` / the instance `cwd` attribute /
    /// the process cwd), applying any chroot. Purely lexical (no filesystem
    /// access) — shared by the `&self` native IO::Path methods
    /// (`try_io_path_fs_stat` / `try_io_path_content_read`) and `native_io_path`.
    fn resolve_io_path_buf(&self, attributes: &HashMap<String, Value>, p: &str) -> PathBuf {
        let instance_cwd = attributes.get("cwd").map(|v| v.to_string_value());
        if Path::new(p).is_absolute() {
            self.resolve_path(p)
        } else if let Some(cwd) = &instance_cwd {
            self.apply_chroot(PathBuf::from(cwd).join(Path::new(p)))
        } else {
            self.resolve_path(p)
        }
    }

    /// Pure `stat`-based result for the [`try_io_path_fs_stat`] methods given an
    /// already-resolved `path_buf` (and the original `p` for error/Failure
    /// messages). Factored out so both the VM-native path and `native_io_path`
    /// run the exact same filesystem queries and Failure shaping.
    fn io_path_stat_result(path_buf: &Path, p: &str, method: &str) -> Result<Value, RuntimeError> {
        match method {
            "e" => Ok(Value::Bool(path_buf.exists())),
            "f" => match fs::metadata(path_buf) {
                Ok(meta) => Ok(Value::Bool(meta.is_file())),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "d" => match fs::metadata(path_buf) {
                Ok(meta) => Ok(Value::Bool(meta.is_dir())),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "l" => match fs::symlink_metadata(path_buf) {
                Ok(meta) => Ok(Value::Bool(meta.file_type().is_symlink())),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "r" => match fs::metadata(path_buf) {
                Ok(_) => Ok(Value::Bool(path_is_readable(path_buf))),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "w" => match fs::metadata(path_buf) {
                Ok(_) => Ok(Value::Bool(path_is_writable(path_buf))),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "x" => match fs::metadata(path_buf) {
                Ok(_) => Ok(Value::Bool(path_is_executable(path_buf))),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "rw" => match fs::metadata(path_buf) {
                Ok(_) => Ok(Value::Bool(
                    path_is_readable(path_buf) && path_is_writable(path_buf),
                )),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "rwx" => match fs::metadata(path_buf) {
                Ok(_) => Ok(Value::Bool(
                    path_is_readable(path_buf)
                        && path_is_writable(path_buf)
                        && path_is_executable(path_buf),
                )),
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "z" => match fs::metadata(path_buf) {
                Ok(meta) => Ok(Value::Bool(meta.len() == 0)),
                Err(_) => {
                    let message = format!("Failed to find '{}' while trying to do '.z'", p);
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(message));
                    attrs.insert("path".to_string(), Value::str(p.to_string()));
                    attrs.insert("trying".to_string(), Value::str_from("z"));
                    let ex = Value::make_instance(Symbol::intern("X::IO::DoesNotExist"), attrs);
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ))
                }
            },
            "mode" => {
                let metadata = fs::metadata(path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to stat '{}': {}", p, err)))?;
                #[cfg(unix)]
                {
                    let mode = metadata.permissions().mode() & 0o777;
                    Ok(Value::str(format!("{:04o}", mode)))
                }
                #[cfg(not(unix))]
                {
                    let mode = if metadata.permissions().readonly() {
                        "0444"
                    } else {
                        "0666"
                    };
                    Ok(Value::str(mode.to_string()))
                }
            }
            "s" => {
                let size = io_path_metadata(path_buf, p, method)?.len();
                Ok(Value::Int(size as i64))
            }
            "created" => {
                let ts = fs::metadata(path_buf)
                    .and_then(|meta| meta.created())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get created time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "modified" => {
                let ts = fs::metadata(path_buf)
                    .and_then(|meta| meta.modified())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get modified time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "accessed" => {
                let ts = fs::metadata(path_buf)
                    .and_then(|meta| meta.accessed())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get accessed time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "changed" => {
                #[cfg(unix)]
                {
                    use std::os::unix::fs::MetadataExt;
                    let meta = fs::metadata(path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to get changed time '{}': {}", p, err))
                    })?;
                    Ok(Value::Int(meta.ctime()))
                }
                #[cfg(not(unix))]
                {
                    // On non-Unix platforms, fall back to modified time
                    let ts = fs::metadata(path_buf)
                        .and_then(|meta| meta.modified())
                        .map(Self::system_time_to_int)
                        .map_err(|err| {
                            RuntimeError::new(format!(
                                "Failed to get changed time '{}': {}",
                                p, err
                            ))
                        })?;
                    Ok(Value::Int(ts))
                }
            }
            _ => unreachable!("io_path_stat_result called with non-stat method"),
        }
    }

    /// Filesystem *whole-file content reads* on an `IO::Path`
    /// (`slurp`/`lines`/`words`): resolve the path against the VM-owned cwd, read
    /// the entire file (`fs::read[_to_string]`), then split / decode the bytes.
    /// These allocate **no `io_handles`** and emit nothing; flag parsing
    /// (`parse_io_flags_values`) and encoding lookup (`decode_with_encoding`, which
    /// reads the VM-owned encoding registry) are `&self` reads, so the VM
    /// dispatches them natively (ledger §D) via the single impl `native_io_path`
    /// also delegates to. `comb` (its regex/closure dispatch needs `&mut self`)
    /// and `open`/`spurt` (io_handles / FS writes) return `None` and stay in
    /// `native_io_path`. Behavior-invariant (same read + split/decode logic).
    pub(crate) fn try_io_path_content_read(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "slurp" | "lines" | "words") {
            return None;
        }
        Some(self.io_path_content_read(attributes, method, args))
    }

    /// The fallible body of [`try_io_path_content_read`] (the gate returns
    /// `Option` so it cannot use `?`). Resolves the path then reads + splits /
    /// decodes the whole file. Behavior-invariant with the arms `native_io_path`
    /// previously held.
    fn io_path_content_read(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        match method {
            "slurp" => {
                let (_, _, _, bin, _, _, _, _, enc, _, _) = self.parse_io_flags_values(args);
                if bin {
                    let bytes = fs::read(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to slurp '{}': {}", p, err))
                    })?;
                    let byte_vals: Vec<Value> = bytes
                        .into_iter()
                        .map(|b| Value::Int(i64::from(b)))
                        .collect();
                    let mut attrs = HashMap::new();
                    attrs.insert("bytes".to_string(), Value::array(byte_vals));
                    return Ok(Value::make_instance(Symbol::intern("Buf[uint8]"), attrs));
                }
                // A non-utf-8 encoding reads raw bytes and decodes; utf-8 reads
                // the string directly (stripping a leading BOM).
                let needs_non_utf8 = enc.as_ref().is_some_and(|e| {
                    let lower = e.to_lowercase();
                    lower != "utf-8" && lower != "utf8"
                });
                if needs_non_utf8 {
                    let bytes = fs::read(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to slurp '{}': {}", p, err))
                    })?;
                    let decoded = self.decode_with_encoding(&bytes, enc.as_ref().unwrap())?;
                    Ok(Value::str(decoded))
                } else {
                    let content = fs::read_to_string(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to slurp '{}': {}", p, err))
                    })?;
                    Ok(Value::str(super::utils::strip_utf8_bom(content)))
                }
            }
            "lines" => {
                let content = fs::read_to_string(&path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?;
                let content = super::utils::strip_utf8_bom(content);
                let (_, _, _, _, chomp, nl_in, _, _, _, _, _) = self.parse_io_flags_values(args);
                let mut parts = Self::split_content_by_separators(&content, &nl_in, chomp);
                if let Some(n) = args.iter().find_map(numeric_limit_arg) {
                    parts.truncate(n);
                }
                Ok(Value::Seq(std::sync::Arc::new(parts)))
            }
            "words" => {
                let content = fs::read_to_string(&path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?;
                let content = super::utils::strip_utf8_bom(content);
                let mut parts: Vec<Value> = content
                    .split_whitespace()
                    .map(|token| Value::str(token.to_string()))
                    .collect();
                if let Some(n) = args.iter().find_map(numeric_limit_arg) {
                    parts.truncate(n);
                }
                Ok(Value::Seq(std::sync::Arc::new(parts)))
            }
            _ => unreachable!("io_path_content_read called with non-content method"),
        }
    }

    /// Single-path filesystem *mutations* on an `IO::Path`
    /// (`spurt`/`mkdir`/`rmdir`/`unlink`/`chmod`): resolve the path against the
    /// VM-owned cwd, then perform a one-shot syscall (`fs::write`/`create_dir_all`/
    /// `remove_dir`/`remove_file`/`set_permissions`). They allocate **no
    /// `io_handles`** — `spurt` opens, writes, and immediately drops its file
    /// handle. Encoding lookup (`encode_with_encoding`, reading the VM-owned
    /// encoding registry) is a `&self` read, so the VM dispatches these natively
    /// (ledger §D): the single impl `native_io_path` also delegates to. Two-path
    /// ops (`copy`/`rename`/`move`/`symlink`/`link`, which resolve a destination)
    /// and handle-opening `open` return `None` and stay in `native_io_path`.
    pub(crate) fn try_io_path_fs_mutate(
        &self,
        attributes: &HashMap<String, Value>,
        class_name: &str,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "spurt" | "mkdir" | "rmdir" | "unlink" | "chmod") {
            return None;
        }
        Some(self.io_path_fs_mutate(attributes, class_name, method, args))
    }

    /// The fallible body of [`try_io_path_fs_mutate`] (the gate returns `Option`
    /// so it cannot use `?`). Behavior-invariant with the arms `native_io_path`
    /// previously held.
    fn io_path_fs_mutate(
        &self,
        attributes: &HashMap<String, Value>,
        class_name: &str,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        match method {
            "spurt" => {
                let content_value = args
                    .first()
                    .cloned()
                    .unwrap_or(Value::Str(String::new().into()));
                let mut append = false;
                let mut createonly = false;
                let mut enc: Option<String> = None;
                for arg in args.iter().skip(1) {
                    if let Value::Pair(key, val) = arg {
                        match key.as_str() {
                            "append" => append = val.truthy(),
                            "createonly" => createonly = val.truthy(),
                            "enc" => enc = Some(val.to_string_value()),
                            _ => {}
                        }
                    }
                }
                if createonly && path_buf.exists() {
                    return Ok(io_exception_failure(
                        "X::IO::Spurt",
                        format!("Failed to spurt '{}': file already exists", p),
                    ));
                }
                let is_buf = crate::runtime::Interpreter::is_buf_value(&content_value);
                let write_result = if is_buf {
                    let bytes = crate::runtime::Interpreter::extract_buf_bytes(&content_value);
                    if append {
                        use std::io::Write;
                        fs::OpenOptions::new()
                            .append(true)
                            .create(true)
                            .open(&path_buf)
                            .and_then(|mut file| file.write_all(&bytes))
                    } else {
                        fs::write(&path_buf, &bytes)
                    }
                } else {
                    let content = content_value.to_string_value();
                    let bytes = if let Some(ref enc_name) = enc {
                        match self.encode_with_encoding(&content, enc_name) {
                            Ok(mut b) => {
                                // For utf16 (auto-endian), prepend BOM
                                let enc_lower = enc_name.to_lowercase();
                                if enc_lower == "utf-16" || enc_lower == "utf16" {
                                    let bom: &[u8] = if cfg!(target_endian = "little") {
                                        &[0xFF, 0xFE]
                                    } else {
                                        &[0xFE, 0xFF]
                                    };
                                    let mut with_bom = Vec::with_capacity(bom.len() + b.len());
                                    with_bom.extend_from_slice(bom);
                                    with_bom.append(&mut b);
                                    with_bom
                                } else {
                                    b
                                }
                            }
                            Err(e) => {
                                return Ok(io_exception_failure("X::IO::Spurt", e.message));
                            }
                        }
                    } else {
                        content.into_bytes()
                    };
                    if append {
                        use std::io::Write;
                        fs::OpenOptions::new()
                            .append(true)
                            .create(true)
                            .open(&path_buf)
                            .and_then(|mut file| file.write_all(&bytes))
                    } else {
                        fs::write(&path_buf, &bytes)
                    }
                };
                match write_result {
                    Ok(()) => Ok(Value::Bool(true)),
                    Err(err) => Ok(io_exception_failure(
                        "X::IO::Spurt",
                        format!("Failed to spurt '{}': {}", p, err),
                    )),
                }
            }
            "mkdir" => match fs::create_dir_all(&path_buf) {
                Ok(()) => Ok(Value::make_instance(
                    Symbol::intern(class_name),
                    attributes.clone(),
                )),
                Err(err) => {
                    let msg = format!(
                        "Failed to create directory '{}' with mode '0o777': Failed to mkdir: {}",
                        p, err
                    );
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("message".to_string(), Value::str_from(&msg));
                    ex_attrs.insert("path".to_string(), Value::str_from(&p));
                    let ex = Value::make_instance(Symbol::intern("X::IO::Mkdir"), ex_attrs);
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ))
                }
            },
            "rmdir" => match fs::remove_dir(&path_buf) {
                Ok(()) => Ok(Value::Bool(true)),
                Err(err) => {
                    let msg = format!("Failed to remove the directory '{}': {}", p, err);
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("message".to_string(), Value::str_from(&msg));
                    ex_attrs.insert("path".to_string(), Value::str_from(&p));
                    let ex = Value::make_instance(Symbol::intern("X::IO::Rmdir"), ex_attrs);
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ))
                }
            },
            "unlink" => match fs::remove_file(&path_buf) {
                Ok(()) => Ok(Value::Bool(true)),
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(Value::Bool(false)),
                Err(err) => Err(RuntimeError::new(format!(
                    "Failed to unlink '{}': {}",
                    p, err
                ))),
            },
            "chmod" => {
                let mode_value = args
                    .first()
                    .cloned()
                    .ok_or_else(|| RuntimeError::new("chmod requires mode"))?;
                let mode_int = match mode_value {
                    Value::Int(i) => i as u32,
                    Value::Str(s) => u32::from_str_radix(&s, 8).unwrap_or(0),
                    other => {
                        return Err(RuntimeError::new(format!(
                            "Invalid mode: {}",
                            other.to_string_value()
                        )));
                    }
                };
                #[cfg(unix)]
                {
                    let perms = PermissionsExt::from_mode(mode_int);
                    fs::set_permissions(&path_buf, perms).map_err(|err| {
                        RuntimeError::new(format!("Failed to chmod '{}': {}", p, err))
                    })?;
                }
                #[cfg(not(unix))]
                {
                    let _ = mode_int;
                    return Err(RuntimeError::new("chmod not supported on this platform"));
                }
                Ok(Value::Bool(true))
            }
            _ => unreachable!("io_path_fs_mutate called with non-mutation method"),
        }
    }

    /// Open a file handle for an `IO::Path` (`open`): allocate an `io_handles`
    /// entry and return the `IO::Handle`. This is the one IO::Path FS method that
    /// mutates VM-owned `io_handles` state (`&mut self`) — but the VM *owns* that
    /// table (a shared `Arc<RwLock>`), so it dispatches `open` natively (ledger §D
    /// ③) via the single shared `open_file_handle` the interpreter also uses, with
    /// the same `:r`/`:w`/`:a`/`:rw`/`:bin`/`:enc`/`:create`/`:exclusive` flag
    /// handling and the same Failure-on-error shaping. Path resolution
    /// (`resolve_io_path_buf`) and flag parsing (`parse_io_flags_values`) are
    /// `&self` reads returning owned values, so there is no borrow conflict with
    /// the subsequent `&mut self` `open_file_handle`. Returns `None` for any other
    /// method.
    pub(crate) fn try_io_path_open(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "open" {
            return None;
        }
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        let (
            read,
            write,
            append,
            bin,
            line_chomp,
            line_separators,
            out_buffer_capacity,
            nl_out,
            enc,
            create,
            exclusive,
        ) = self.parse_io_flags_values(args);
        Some(
            match self.open_file_handle(
                &path_buf,
                read,
                write,
                append,
                bin,
                line_chomp,
                line_separators,
                out_buffer_capacity,
                nl_out,
                enc,
                create,
                exclusive,
            ) {
                Ok(handle) => Ok(handle),
                // Like the `open` sub, `IO::Path.open` returns a Failure (wrapping
                // the exception) on error rather than throwing.
                Err(err) => {
                    let class_name = err
                        .exception
                        .as_deref()
                        .and_then(|ex| match ex {
                            Value::Instance { class_name, .. } => Some(class_name.to_string()),
                            _ => None,
                        })
                        .unwrap_or_else(|| "X::AdHoc".to_string());
                    Ok(io_exception_failure(&class_name, err.message))
                }
            },
        )
    }

    /// Two-path filesystem operations on an `IO::Path`
    /// (`copy`/`rename`/`move`/`symlink`/`link`): resolve both the receiver path
    /// and the destination/link path against the VM-owned cwd, then perform a
    /// one-shot syscall (`fs::copy`/`fs::rename`/`unix_fs::symlink`/
    /// `fs::hard_link`). They allocate **no `io_handles`** and only read the
    /// VM-owned cwd (`resolve_path`, `&self`), so the VM dispatches them natively
    /// (ledger §D): the single impl `native_io_path` also delegates to. Returns
    /// `None` for any other method.
    pub(crate) fn try_io_path_two_path_op(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "copy" | "rename" | "move" | "symlink" | "link") {
            return None;
        }
        Some(self.io_path_two_path_op(attributes, method, args))
    }

    /// The fallible body of [`try_io_path_two_path_op`] (the gate returns `Option`
    /// so it cannot use `?`). Behavior-invariant with the arms `native_io_path`
    /// previously held.
    fn io_path_two_path_op(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        match method {
            "copy" => {
                let dest = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("copy requires destination"))?;
                let createonly = Self::named_bool(args, "createonly");
                let dest_buf = self.resolve_path(&dest);
                // Check if source and destination are the same file
                if path_buf == dest_buf
                    || (path_buf.exists()
                        && dest_buf.exists()
                        && fs::canonicalize(&path_buf).ok() == fs::canonicalize(&dest_buf).ok())
                {
                    return Ok(io_exception_failure(
                        "X::IO::Copy",
                        format!(
                            "Failed to copy '{}': source and destination are the same file",
                            p
                        ),
                    ));
                }
                if createonly && dest_buf.exists() {
                    return Ok(io_exception_failure(
                        "X::IO::Copy",
                        format!("Failed to copy '{}': destination already exists", p),
                    ));
                }
                fs::copy(&path_buf, &dest_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to copy '{}': {}", p, err)))?;
                Ok(Value::Bool(true))
            }
            "rename" | "move" => {
                let ex_type = if method == "rename" {
                    "X::IO::Rename"
                } else {
                    "X::IO::Move"
                };
                let verb = if method == "rename" { "rename" } else { "move" };
                let dest = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("rename/move requires destination"))?;
                let createonly = Self::named_bool(args, "createonly");
                let dest_buf = self.resolve_path(&dest);
                // Check if source and destination are the same file
                if path_buf == dest_buf
                    || (path_buf.exists()
                        && dest_buf.exists()
                        && fs::canonicalize(&path_buf).ok() == fs::canonicalize(&dest_buf).ok())
                {
                    let ex = Value::make_instance(Symbol::intern(ex_type), HashMap::new());
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    failure_attrs.insert("handled".to_string(), Value::Bool(false));
                    failure_attrs.insert(
                        "message".to_string(),
                        Value::Str(
                            format!(
                                "Failed to {} '{}': source and destination are the same file",
                                verb, p
                            )
                            .into(),
                        ),
                    );
                    return Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ));
                }
                if createonly && dest_buf.exists() {
                    return Err(io_exception(
                        ex_type,
                        format!("Failed to {} '{}': destination already exists", verb, p),
                    ));
                }
                fs::rename(&path_buf, &dest_buf).map_err(|err| {
                    io_exception(ex_type, format!("Failed to {} '{}': {}", verb, p, err))
                })?;
                Ok(Value::Bool(true))
            }
            "symlink" => {
                // IO::Path.symlink($name, :$absolute = True)
                // Creates a symlink named $name pointing to self (the target).
                let link_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("symlink requires a link name"))?;
                // :absolute defaults to True; :!absolute uses the original path string.
                let absolute = Self::named_value(args, "absolute")
                    .map(|v| v.truthy())
                    .unwrap_or(true);
                let link_buf = self.resolve_path(&link_name);
                let target_for_symlink = if absolute {
                    path_buf.clone()
                } else {
                    std::path::PathBuf::from(&p)
                };
                #[cfg(unix)]
                {
                    match unix_fs::symlink(&target_for_symlink, &link_buf) {
                        Ok(()) => Ok(Value::Bool(true)),
                        Err(err) => Ok(Self::make_symlink_failure(&p, &link_name, &err)),
                    }
                }
                #[cfg(windows)]
                {
                    let metadata = fs::metadata(&target_for_symlink);
                    let result = if metadata.map(|meta| meta.is_dir()).unwrap_or(false) {
                        windows_fs::symlink_dir(&target_for_symlink, &link_buf)
                    } else {
                        windows_fs::symlink_file(&target_for_symlink, &link_buf)
                    };
                    match result {
                        Ok(()) => Ok(Value::Bool(true)),
                        Err(err) => Ok(Self::make_symlink_failure(&p, &link_name, &err)),
                    }
                }
                #[cfg(not(any(unix, windows)))]
                {
                    Err(RuntimeError::new("symlink not supported on this platform"))
                }
            }
            "link" => {
                // IO::Path.link($name): creates a new hard link named $name
                // pointing to self (the target). Fails with X::IO::Link.
                let link_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("link requires a link name"))?;
                let link_buf = self.resolve_path(&link_name);
                match fs::hard_link(&path_buf, &link_buf) {
                    Ok(()) => Ok(Value::Bool(true)),
                    Err(err) => Ok(Self::make_link_failure(&p, &link_name, &err)),
                }
            }
            _ => unreachable!("io_path_two_path_op called with non-two-path method"),
        }
    }

    /// `IO::Path.comb`: read the whole file, then comb the content. The matcher
    /// dispatch (`dispatch_comb_with_args`) is `&mut self` because a regex/closure
    /// matcher runs the match engine — but it reads no `io_handles`, so the VM
    /// dispatches `comb` natively (ledger §D): the single impl `native_io_path`
    /// also delegates to. **Also fixes a pre-existing bug**: the no-matcher form
    /// (`$path.IO.comb` with no positional) used to return an empty Seq here (the
    /// old arm mapped `dispatch_comb_with_args`'s `None` to empty); it now splits
    /// the content into graphemes, matching `Str.comb` and Rakudo. Returns `None`
    /// for any other method.
    pub(crate) fn try_io_path_comb(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "comb" {
            return None;
        }
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        let content = match fs::read_to_string(&path_buf) {
            Ok(c) => super::utils::strip_utf8_bom(c),
            Err(err) => {
                return Some(Err(RuntimeError::new(format!(
                    "Failed to read '{}': {}",
                    p, err
                ))));
            }
        };
        // Filter out :close (irrelevant for IO::Path) before delegating.
        let comb_args: Vec<Value> = args
            .iter()
            .filter(|a| !matches!(a, Value::Pair(k, _) if k == "close"))
            .cloned()
            .collect();
        Some(
            match self.dispatch_comb_with_args(Value::str(content.clone()), &comb_args) {
                Some(res) => res,
                // No matcher: comb into graphemes (same as `Str.comb` with no
                // args / Rakudo). The old arm wrongly returned an empty Seq.
                None => {
                    let parts: Vec<Value> = content
                        .graphemes(true)
                        .map(|g| Value::str(g.to_string()))
                        .collect();
                    Ok(Value::Seq(std::sync::Arc::new(parts)))
                }
            },
        )
    }

    pub(super) fn native_io_path(
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

    fn split_path_for_extension(path: &str) -> (&str, &str) {
        if let Some(idx) = path.rfind(['/', '\\']) {
            (&path[..=idx], &path[idx + 1..])
        } else {
            ("", path)
        }
    }

    fn clone_io_path_with_path(
        attributes: &HashMap<String, Value>,
        class: Symbol,
        path: String,
    ) -> Value {
        let mut new_attrs = attributes.clone();
        new_attrs.insert("path".to_string(), Value::str(path));
        Value::make_instance(class, new_attrs)
    }

    /// Check if the IO::Path instance has a Win32 SPEC attribute.
    fn is_win32_spec(attributes: &HashMap<String, Value>) -> bool {
        attributes
            .get("SPEC")
            .map(|s| {
                let name = match s {
                    Value::Package(n) => n.resolve().to_string(),
                    Value::Instance { class_name, .. } => class_name.resolve().to_string(),
                    _ => String::new(),
                };
                name == "IO::Spec::Win32" || name.ends_with("Win32")
            })
            .unwrap_or(false)
    }

    /// Whether the path's SPEC is `IO::Spec::Cygwin`. Cygwin uses Win32-style
    /// volume/absoluteness semantics (UNC, drive letters) but normalizes all
    /// backslashes to forward slashes in its output.
    fn is_cygwin_spec(attributes: &HashMap<String, Value>) -> bool {
        attributes
            .get("SPEC")
            .map(|s| {
                let name = match s {
                    Value::Package(n) => n.resolve().to_string(),
                    Value::Instance { class_name, .. } => class_name.resolve().to_string(),
                    _ => String::new(),
                };
                name == "IO::Spec::Cygwin" || name.ends_with("Cygwin")
            })
            .unwrap_or(false)
    }

    /// Whether `p` is absolute under the receiver's SPEC. Win32/Cygwin use
    /// drive-letter / UNC / leading-separator rules; other SPECs defer to the
    /// platform's `Path::is_absolute`.
    fn io_path_is_absolute_spec(
        attributes: &HashMap<String, Value>,
        p: &str,
        original: &Path,
    ) -> bool {
        if Self::is_win32_spec(attributes) {
            Self::io_path_is_absolute_win32(p)
        } else if Self::is_cygwin_spec(attributes) {
            Self::io_path_is_absolute_win32(&p.replace('\\', "/"))
        } else {
            original.is_absolute()
        }
    }

    /// Split a path into (volume, dirname, basename), honoring the Cygwin SPEC
    /// by first converting backslashes to forward slashes (Cygwin treats `\`
    /// and `/` interchangeably and emits forward slashes).
    fn io_path_parts_spec(
        path: &str,
        attributes: &HashMap<String, Value>,
    ) -> (String, String, String) {
        if Self::is_cygwin_spec(attributes) {
            Self::io_path_parts(&path.replace('\\', "/"))
        } else {
            Self::io_path_parts(path)
        }
    }

    /// Get the directory separator based on the SPEC attribute.
    fn io_path_sep(attributes: &HashMap<String, Value>) -> char {
        if Self::is_win32_spec(attributes) {
            '\\'
        } else {
            '/'
        }
    }

    /// Check if a path is absolute in the context of a Win32 SPEC.
    fn io_path_is_absolute_win32(path: &str) -> bool {
        // On Win32, absolute paths start with: \, /, drive letter + :\, drive letter + :/
        // or are UNC paths (\\server\share, //server/share)
        if path.is_empty() {
            return false;
        }
        let bytes = path.as_bytes();
        // Starts with / or \
        if bytes[0] == b'/' || bytes[0] == b'\\' {
            return true;
        }
        // Drive letter followed by :\ or :/
        if path.len() >= 3
            && bytes[0].is_ascii_alphabetic()
            && bytes[1] == b':'
            && (bytes[2] == b'\\' || bytes[2] == b'/')
        {
            return true;
        }
        false
    }

    fn io_path_parts(path: &str) -> (String, String, String) {
        let mut volume = String::new();
        let mut rest = path;

        // Check for UNC paths: \\server\share or //server/share
        if path.len() >= 2 {
            let bytes = path.as_bytes();
            if (bytes[0] == b'\\' && bytes[1] == b'\\') || (bytes[0] == b'/' && bytes[1] == b'/') {
                // UNC path: volume is //server/share or \\server\share
                let after_prefix = &path[2..];
                // Find the server part (up to next separator)
                if let Some(sep1) = after_prefix.find(['/', '\\']) {
                    let server = &after_prefix[..sep1];
                    let after_server = &after_prefix[sep1 + 1..];
                    // Find the share part (up to next separator or end)
                    let share_end = after_server.find(['/', '\\']).unwrap_or(after_server.len());
                    let share = &after_server[..share_end];
                    // Volume is prefix + server + sep + share
                    volume = format!(
                        "{}{}{}{}",
                        &path[..2],
                        server,
                        &path[2..][sep1..sep1 + 1],
                        share
                    );
                    rest = &after_server[share_end..];
                    if rest.is_empty() {
                        // For UNC paths with no trailing content, dirname and basename are both the separator
                        let sep = &path[0..1];
                        return (volume, sep.to_string(), sep.to_string());
                    }
                } else {
                    // Just \\something with no separator after
                    volume = path.to_string();
                    return (volume, ".".to_string(), ".".to_string());
                }
            } else if bytes[1] == b':' && bytes[0].is_ascii_alphabetic() {
                volume.push_str(&path[..2]);
                rest = &path[2..];
            }
        }

        if rest == "/" || rest == "\\" {
            return (volume, rest.to_string(), rest.to_string());
        }

        let trimmed = rest.trim_end_matches(['/', '\\']);
        if trimmed.is_empty() {
            let root = if rest.contains('\\') && !rest.contains('/') {
                "\\".to_string()
            } else {
                "/".to_string()
            };
            return (volume, root.clone(), root);
        }

        let (dirname, basename) = if let Some(idx) = trimmed.rfind(['/', '\\']) {
            let dirname = &trimmed[..idx];
            let basename = &trimmed[idx + 1..];
            let dirname = if dirname.is_empty() {
                trimmed[..=idx].to_string()
            } else {
                // Strip trailing separators from dirname (handles "foo//bar" -> "foo")
                let d = dirname.trim_end_matches(['/', '\\']);
                if d.is_empty() {
                    dirname[..1].to_string() // Keep single separator for root paths
                } else {
                    d.to_string()
                }
            };
            (dirname, basename.to_string())
        } else {
            (".".to_string(), trimmed.to_string())
        };

        (volume, dirname, basename)
    }

    /// Join volume, dirname, and basename into a path string.
    fn join_io_path_parts(volume: &str, dirname: &str, basename: &str, sep: char) -> String {
        let mut result = String::new();
        result.push_str(volume);
        if dirname == "." && volume.is_empty() {
            // Relative path with no directory
            result.push_str(basename);
        } else if dirname == "/" || dirname == "\\" {
            result.push_str(dirname);
            result.push_str(basename);
        } else {
            result.push_str(dirname);
            result.push(sep);
            result.push_str(basename);
        }
        result
    }

    fn io_path_extension_part_count(path: &str) -> i64 {
        let (_, basename) = Self::split_path_for_extension(path);
        basename.chars().filter(|&c| c == '.').count() as i64
    }

    /// Strict Unix `IO::Spec::Unix.canonpath` implementation used by
    /// `IO::Spec::Unix` / `IO::Spec::QNX` etc. When `parent` is true, `..`
    /// segments are resolved lexically. Returns `''` for empty input.
    /// When `preserve_double_slash` is true, a leading `//` is kept
    /// (POSIX implementation-defined, used by QNX).
    pub fn canonpath_unix(path: &str, parent: bool) -> String {
        Self::canonpath_unix_inner(path, parent, false)
    }

    /// QNX variant that preserves a leading `//`.
    pub fn canonpath_qnx(path: &str, parent: bool) -> String {
        Self::canonpath_unix_inner(path, parent, true)
    }

    fn canonpath_unix_inner(path: &str, parent: bool, preserve_double_slash: bool) -> String {
        if path.is_empty() {
            return String::new();
        }
        let bytes = path.as_bytes();
        let mut leading = 0usize;
        while leading < bytes.len() && bytes[leading] == b'/' {
            leading += 1;
        }
        let prefix = if leading == 0 {
            ""
        } else if preserve_double_slash && leading == 2 {
            "//"
        } else {
            "/"
        };
        let is_absolute = leading > 0;
        let rest = &path[leading..];
        let segments: Vec<&str> = rest
            .split('/')
            .filter(|s| !s.is_empty() && *s != ".")
            .collect();
        // For "//"-prefixed paths (QNX), do not collapse leading `..` past the prefix.
        let collapse_leading_dotdot = is_absolute && prefix == "/";
        let mut stack: Vec<&str> = Vec::new();
        if parent {
            for seg in segments {
                if seg == ".." {
                    if let Some(top) = stack.last() {
                        if *top == ".." {
                            stack.push(seg);
                        } else {
                            stack.pop();
                        }
                    } else if collapse_leading_dotdot {
                        // can't go above root, drop
                    } else {
                        stack.push(seg);
                    }
                } else {
                    stack.push(seg);
                }
            }
        } else {
            for seg in segments {
                if seg == ".." && collapse_leading_dotdot && stack.is_empty() {
                    continue;
                }
                stack.push(seg);
            }
        }
        let joined = stack.join("/");
        let mut out = String::new();
        out.push_str(prefix);
        if !joined.is_empty() {
            out.push_str(&joined);
        }
        if out.is_empty() {
            // Non-empty input that reduced to nothing (e.g. "foo/.." with :parent)
            return ".".to_string();
        }
        out
    }

    /// Cygwin `IO::Spec::Cygwin.canonpath`: converts backslashes to forward
    /// slashes, strips drive-letter prefix, then delegates to Unix canonpath
    /// (preserving leading `//` like QNX).
    pub fn canonpath_cygwin(path: &str, parent: bool) -> String {
        if path.is_empty() {
            return String::new();
        }
        // Convert backslashes to forward slashes
        let converted = path.replace('\\', "/");
        // Extract drive letter prefix if present (e.g. "c:" or "C:")
        let (prefix, rest) = if converted.len() >= 2
            && converted.as_bytes()[0].is_ascii_alphabetic()
            && converted.as_bytes()[1] == b':'
        {
            (&converted[..2], &converted[2..])
        } else {
            ("", converted.as_str())
        };
        // Cygwin preserves leading // (like QNX)
        let canon = Self::canonpath_unix_inner(rest, parent, true);
        if prefix.is_empty() {
            canon
        } else {
            format!("{}{}", prefix, canon)
        }
    }

    /// Split a Cygwin path into (volume, rest).
    /// Handles UNC paths (//server/share -> volume="//server/share", rest="/")
    /// and drive letters (c:/foo -> volume="c:", rest="/foo").
    pub fn split_cygwin_volume(path: &str) -> (String, String) {
        let bytes = path.as_bytes();
        // UNC path: //server/share
        if bytes.len() >= 2 && bytes[0] == b'/' && bytes[1] == b'/' {
            // Find end of server name
            if let Some(server_end) = path[2..].find('/') {
                let share_start = 2 + server_end + 1;
                // Find end of share name
                let share_end = path[share_start..]
                    .find('/')
                    .map(|p| share_start + p)
                    .unwrap_or(path.len());
                let volume = path[..share_end].to_string();
                let rest = if share_end >= path.len() {
                    "/".to_string()
                } else {
                    path[share_end..].to_string()
                };
                return (volume, rest);
            }
        }
        // Drive letter: X:
        if bytes.len() >= 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':' {
            let vol = path[..2].to_string();
            let rest = path[2..].to_string();
            return (vol, rest);
        }
        // No volume
        (String::new(), path.to_string())
    }

    /// Extract Win32 volume (drive letter or UNC) from a path.
    /// Returns (volume, rest) where rest is everything after the volume.
    /// The volume preserves original slash style; callers should normalize if needed.
    pub fn split_win32_volume(path: &str) -> (String, String) {
        let bytes = path.as_bytes();
        // UNC path: \\server\share or //server/share
        if bytes.len() >= 2
            && ((bytes[0] == b'\\' && bytes[1] == b'\\') || (bytes[0] == b'/' && bytes[1] == b'/'))
        {
            let after = &path[2..];
            // Server name must be non-empty and not start with a separator
            if !after.is_empty() && !after.starts_with('/') && !after.starts_with('\\') {
                if let Some(server_end) = after.find(['/', '\\']) {
                    let share_start = server_end + 1;
                    let share_end = after[share_start..]
                        .find(['/', '\\'])
                        .map(|p| share_start + p)
                        .unwrap_or(after.len());
                    let volume = path[..2 + share_end].to_string();
                    let rest = path[2 + share_end..].to_string();
                    return (volume, rest);
                }
                // Only server, no share -- whole thing is volume
                return (path.to_string(), String::new());
            }
        }
        // Drive letter: X:
        if bytes.len() >= 2 && bytes[0].is_ascii_alphabetic() && bytes[1] == b':' {
            let vol = path[..2].to_string();
            let rest = path[2..].to_string();
            return (vol, rest);
        }
        // No volume
        (String::new(), path.to_string())
    }

    /// Like `split_win32_volume` but normalizes the volume (/ -> \).
    pub fn split_win32_volume_normalized(path: &str) -> (String, String) {
        let (vol, rest) = Self::split_win32_volume(path);
        (vol.replace('/', "\\"), rest)
    }

    /// Win32 `.path` method: reads PATH (or Path), splits on `;`,
    /// strips `"` characters from each entry, always prepends ".".
    pub fn win32_path_from_env() -> Vec<Value> {
        // PATH overrides Path
        let path_env = if let Ok(v) = std::env::var("PATH") {
            Some(v)
        } else {
            std::env::var("Path").ok()
        };
        let mut result = vec![Value::str_from(".")];
        let raw = match path_env {
            None => return result, // env unset -> (".",)
            Some(v) => v,
        };
        if raw.is_empty() {
            return result; // empty -> (".",)
        }
        // Win32 PATH parsing: split on `;`, strip `"` chars, skip empties.
        for entry in raw.split(';') {
            let cleaned: String = entry.chars().filter(|&c| c != '"').collect();
            if !cleaned.is_empty() {
                result.push(Value::str(cleaned));
            }
        }
        result
    }

    pub fn cleanup_io_path_lexical(path: &str) -> String {
        if path.is_empty() {
            return ".".to_string();
        }
        let mut prefix = "";
        let mut rest = path;
        if path.len() >= 2 {
            let bytes = path.as_bytes();
            if bytes[1] == b':' && bytes[0].is_ascii_alphabetic() {
                prefix = &path[..2];
                rest = &path[2..];
            }
        }
        let is_absolute = rest.starts_with('/') || rest.starts_with('\\');
        let sep = if path.contains('\\') && !path.contains('/') {
            '\\'
        } else {
            '/'
        };
        let mut stack: Vec<&str> = Vec::new();
        for seg in rest.split(['/', '\\']) {
            if seg.is_empty() || seg == "." {
                continue;
            }
            if seg == ".." {
                if !is_absolute || !stack.is_empty() {
                    stack.push(seg);
                }
                continue;
            }
            stack.push(seg);
        }
        let joined = stack.join(&sep.to_string());
        let mut out = String::new();
        out.push_str(prefix);
        if is_absolute {
            out.push(sep);
        }
        if !joined.is_empty() {
            out.push_str(&joined);
        }
        if out.is_empty() { ".".to_string() } else { out }
    }

    /// Win32-specific canonpath with optional `:parent` resolution.
    pub fn canonpath_win32(path: &str, parent: bool) -> String {
        if path.is_empty() {
            return String::new();
        }
        let mut prefix = String::new();
        let mut rest = path;
        let bytes = path.as_bytes();
        // Check for UNC paths (\\server\share or //server/share)
        // A valid UNC needs a non-empty server name that is not "." or "..".
        if path.len() >= 2
            && ((bytes[0] == b'\\' && bytes[1] == b'\\') || (bytes[0] == b'/' && bytes[1] == b'/'))
        {
            let after = &path[2..];
            let server_name = after.split(['/', '\\']).next().unwrap_or("");
            let has_server = !server_name.is_empty() && server_name != "." && server_name != "..";
            if has_server {
                if let Some(sep1) = after.find(['/', '\\']) {
                    let after_server = &after[sep1 + 1..];
                    let share_end = after_server.find(['/', '\\']).unwrap_or(after_server.len());
                    let unc_end = 2 + sep1 + 1 + share_end;
                    prefix = path[..unc_end].replace('/', "\\");
                    rest = &path[unc_end..];
                } else {
                    return path.replace('/', "\\");
                }
            }
            // else: not a valid UNC (e.g. "//" or "////"), treat as absolute path
        } else if path.len() >= 2 && bytes[1] == b':' && bytes[0].is_ascii_alphabetic() {
            // Drive letter -- uppercase it
            let mut p = String::new();
            p.push(bytes[0].to_ascii_uppercase() as char);
            p.push(':');
            prefix = p;
            rest = &path[2..];
        }

        let is_unc = prefix.starts_with("\\\\");
        let is_absolute = rest.starts_with('/') || rest.starts_with('\\');

        let mut stack: Vec<String> = Vec::new();
        for seg in rest.split(['/', '\\']) {
            if seg.is_empty() || seg == "." {
                continue;
            }
            if seg == ".." {
                if is_unc {
                    // UNC paths: never go above \\server\share
                    continue;
                }
                if parent {
                    if stack.last().is_some_and(|last| *last != "..") {
                        stack.pop();
                        continue;
                    }
                    if is_absolute {
                        continue;
                    }
                    stack.push("..".to_string());
                } else {
                    if is_absolute && stack.is_empty() {
                        continue;
                    }
                    stack.push("..".to_string());
                }
                continue;
            }
            stack.push(seg.to_string());
        }

        let joined = stack.join("\\");
        let mut out = prefix;
        if is_absolute {
            // For UNC paths, only add \ if there are segments to append
            if !is_unc || !joined.is_empty() {
                out.push('\\');
            }
        }
        if !joined.is_empty() {
            out.push_str(&joined);
        }

        if out.is_empty() { ".".to_string() } else { out }
    }

    // Legacy wrapper
    pub fn cleanup_io_path_lexical_win32(path: &str) -> String {
        Self::canonpath_win32(path, false)
    }

    /// Win32 catdir: join directory parts and canonicalize.
    pub fn win32_catdir(parts: &[String]) -> String {
        if parts.is_empty() {
            return String::new();
        }
        let first_nonempty_idx = parts.iter().position(|p| !p.is_empty());
        let had_leading_empties = first_nonempty_idx.is_some_and(|i| i > 0);

        let (volume, first_rest) = if let Some(idx) = first_nonempty_idx {
            if had_leading_empties {
                (String::new(), (idx, parts[idx].clone()))
            } else {
                let (v, r) = Self::split_win32_volume_normalized(&parts[idx]);
                (v, (idx, r))
            }
        } else {
            (String::new(), (0, String::new()))
        };

        let starts_absolute = if first_nonempty_idx.is_some() {
            let rest_starts_sep = first_rest.1.starts_with('/') || first_rest.1.starts_with('\\');
            if !volume.is_empty() {
                rest_starts_sep
            } else {
                had_leading_empties || rest_starts_sep
            }
        } else {
            false
        };

        let mut all_segs: Vec<&str> = Vec::new();
        for (i, part) in parts.iter().enumerate() {
            let p = if Some(i) == first_nonempty_idx && !volume.is_empty() {
                first_rest.1.as_str()
            } else {
                part.as_str()
            };
            for seg in p.split(['/', '\\']) {
                all_segs.push(seg);
            }
        }

        let mut stack: Vec<&str> = Vec::new();
        let is_unc = volume.starts_with("\\\\");
        for seg in &all_segs {
            if seg.is_empty() || *seg == "." {
                continue;
            }
            if *seg == ".." {
                if is_unc || (starts_absolute && stack.is_empty()) {
                    continue;
                }
                stack.push(seg);
                continue;
            }
            stack.push(seg);
        }

        let joined = stack.join("\\");
        let mut out = volume;
        if (starts_absolute && !is_unc) || (is_unc && !joined.is_empty()) {
            out.push('\\');
        }
        if !joined.is_empty() {
            out.push_str(&joined);
        }

        if out.is_empty() { ".".to_string() } else { out }
    }

    /// Win32 catfile: like catdir but without trailing separator treatment.
    pub fn win32_catfile(parts: &[String]) -> String {
        if parts.is_empty() {
            return String::new();
        }
        let first_nonempty_idx = parts.iter().position(|p| !p.is_empty());
        let had_leading_empties = first_nonempty_idx.is_some_and(|i| i > 0);

        let (volume, first_rest) = if let Some(idx) = first_nonempty_idx {
            if had_leading_empties {
                (String::new(), (idx, parts[idx].clone()))
            } else {
                let (v, r) = Self::split_win32_volume_normalized(&parts[idx]);
                (v, (idx, r))
            }
        } else {
            (String::new(), (0, String::new()))
        };

        let starts_absolute = if first_nonempty_idx.is_some() {
            let rest_starts_sep = first_rest.1.starts_with('/') || first_rest.1.starts_with('\\');
            if !volume.is_empty() {
                rest_starts_sep
            } else {
                had_leading_empties || rest_starts_sep
            }
        } else {
            false
        };

        let mut all_segs: Vec<&str> = Vec::new();
        for (i, part) in parts.iter().enumerate() {
            let p = if Some(i) == first_nonempty_idx && !volume.is_empty() {
                first_rest.1.as_str()
            } else {
                part.as_str()
            };
            for seg in p.split(['/', '\\']) {
                all_segs.push(seg);
            }
        }

        let mut stack: Vec<&str> = Vec::new();
        let is_unc = volume.starts_with("\\\\");
        for seg in &all_segs {
            if seg.is_empty() || *seg == "." {
                continue;
            }
            if *seg == ".." {
                if is_unc || (starts_absolute && stack.is_empty()) {
                    continue;
                }
                stack.push(seg);
                continue;
            }
            stack.push(seg);
        }

        let joined = stack.join("\\");
        let mut out = volume;
        if (starts_absolute && !is_unc) || (is_unc && !joined.is_empty()) {
            out.push('\\');
        }
        if !joined.is_empty() {
            out.push_str(&joined);
        }

        if out.is_empty() { ".".to_string() } else { out }
    }

    fn resolve_io_path(
        path: &Path,
        completely: bool,
        display_path: &str,
    ) -> Result<String, RuntimeError> {
        let mut resolved = PathBuf::new();
        let mut unresolved = PathBuf::new();
        let mut unresolved_started = false;
        let component_count = path.components().count();

        for (idx, component) in path.components().enumerate() {
            match component {
                Component::Prefix(prefix) => {
                    resolved.push(prefix.as_os_str());
                    if unresolved_started {
                        unresolved.push(prefix.as_os_str());
                    }
                }
                Component::RootDir => {
                    resolved.push(component.as_os_str());
                    if unresolved_started {
                        unresolved.push(component.as_os_str());
                    }
                }
                Component::CurDir => {}
                Component::ParentDir => {
                    if unresolved_started {
                        unresolved.push("..");
                    } else if !resolved.pop() && completely {
                        return Err(RuntimeError::new(format!(
                            "X::IO::Resolve: Failed to completely resolve {}",
                            display_path
                        )));
                    }
                }
                Component::Normal(part) => {
                    if unresolved_started {
                        unresolved.push(part);
                        continue;
                    }

                    let candidate = resolved.join(part);
                    match fs::symlink_metadata(&candidate) {
                        Ok(_) => {
                            resolved = fs::canonicalize(&candidate).unwrap_or(candidate);
                        }
                        Err(_) => {
                            let is_last = idx + 1 == component_count;
                            if completely && !is_last {
                                return Err(RuntimeError::new(format!(
                                    "X::IO::Resolve: Failed to completely resolve {}",
                                    display_path
                                )));
                            }
                            unresolved_started = true;
                            unresolved.push(part);
                        }
                    }
                }
            }
        }

        let combined = if unresolved_started {
            resolved.join(unresolved)
        } else {
            resolved
        };
        Ok(Self::cleanup_io_path_lexical(&Self::stringify_path(
            &combined,
        )))
    }

    fn io_path_extension_dot_index_for_n_parts(basename: &str, parts: i64) -> Option<usize> {
        if parts <= 0 {
            return None;
        }
        let mut seen = 0i64;
        for (idx, ch) in basename.char_indices().rev() {
            if ch == '.' {
                seen += 1;
                if seen == parts {
                    return Some(idx);
                }
            }
        }
        None
    }

    fn io_path_extension_with_n_parts(path: &str, parts: i64) -> Option<String> {
        if parts == 0 {
            return Some(String::new());
        }
        let (_, basename) = Self::split_path_for_extension(path);
        let dot_idx = Self::io_path_extension_dot_index_for_n_parts(basename, parts)?;
        Some(basename[dot_idx + 1..].to_string())
    }

    fn io_path_extension_strip_n_parts(basename: &str, parts: i64) -> Option<String> {
        if parts == 0 {
            return Some(basename.to_string());
        }
        let dot_idx = Self::io_path_extension_dot_index_for_n_parts(basename, parts)?;
        Some(basename[..dot_idx].to_string())
    }

    fn io_path_extension_normalize_count(v: &Value) -> Result<i64, RuntimeError> {
        let n = match v {
            Value::Int(i) => *i,
            Value::BigInt(i) => i.to_i64().unwrap_or_else(|| {
                if i.sign() == num_bigint::Sign::Minus {
                    i64::MIN
                } else {
                    i64::MAX
                }
            }),
            Value::Num(f) => {
                if f.is_nan() {
                    return Err(RuntimeError::new(
                        "Exception: IO::Path.extension: :parts endpoint must be numeric",
                    ));
                }
                if f.is_infinite() {
                    if f.is_sign_positive() {
                        i64::MAX
                    } else {
                        i64::MIN
                    }
                } else {
                    *f as i64
                }
            }
            Value::Whatever | Value::HyperWhatever => i64::MAX,
            Value::Str(_) => {
                return Err(RuntimeError::new(
                    "Exception: IO::Path.extension: :parts endpoint must be numeric",
                ));
            }
            other => {
                let f = other.to_f64();
                if f.is_nan() {
                    return Err(RuntimeError::new(
                        "Exception: IO::Path.extension: :parts endpoint must be numeric",
                    ));
                }
                if f.is_infinite() {
                    if f.is_sign_positive() {
                        i64::MAX
                    } else {
                        i64::MIN
                    }
                } else {
                    f as i64
                }
            }
        };
        Ok(n.max(0))
    }

    fn io_path_extension_endpoint(v: &Value) -> Result<(Option<i64>, bool), RuntimeError> {
        match v {
            Value::Whatever | Value::HyperWhatever => Ok((None, false)),
            Value::Num(f) if f.is_infinite() => Ok((None, false)),
            Value::Num(f) if f.is_nan() => Err(RuntimeError::new(
                "Exception: IO::Path.extension: :parts endpoint must be numeric",
            )),
            Value::Str(_) => Err(RuntimeError::new(
                "Exception: IO::Path.extension: :parts endpoint must be numeric",
            )),
            _ => Ok((Some(Self::io_path_extension_normalize_count(v)?), true)),
        }
    }

    fn io_path_extension_range_bounds(
        start: &Value,
        end: &Value,
        excl_start: bool,
        excl_end: bool,
    ) -> Result<(i64, i64), RuntimeError> {
        let (mut low, low_finite) = Self::io_path_extension_endpoint(start)?;
        let (mut high, high_finite) = Self::io_path_extension_endpoint(end)?;

        if low_finite && excl_start {
            low = low.map(|v| v.saturating_add(1));
        }
        if high_finite && excl_end {
            high = high.map(|v| v.saturating_sub(1));
        }

        let low = low.unwrap_or(0).max(0);
        let high = high.unwrap_or(i64::MAX).max(0);
        Ok((low, high))
    }

    fn io_path_extension_parts_spec(
        args: &[Value],
    ) -> Result<IoPathExtensionPartsSpec, RuntimeError> {
        let Some(parts_val) = Self::named_value(args, "parts") else {
            return Ok(IoPathExtensionPartsSpec::Exact(1));
        };

        match parts_val {
            Value::Range(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::Int(start),
                    &Value::Int(end),
                    false,
                    false,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            Value::RangeExcl(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::Int(start),
                    &Value::Int(end),
                    false,
                    true,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            Value::RangeExclStart(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::Int(start),
                    &Value::Int(end),
                    true,
                    false,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            Value::RangeExclBoth(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::Int(start),
                    &Value::Int(end),
                    true,
                    true,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    start.as_ref(),
                    end.as_ref(),
                    excl_start,
                    excl_end,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            other => Ok(IoPathExtensionPartsSpec::Exact(
                Self::io_path_extension_normalize_count(&other)?,
            )),
        }
    }

    pub(super) fn native_io_handle(
        &mut self,
        target: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let target_val = Value::make_instance(Symbol::intern("IO::Handle"), target.clone());
        match method {
            "DESTROY" => {
                // Standard handles ($*IN, $*OUT, $*ERR) must not be closed by DESTROY
                let is_std = if let Some(id) = Self::handle_id_from_value(&target_val) {
                    self.io_handles().map.get(&id).is_some_and(|s| {
                        matches!(
                            s.target,
                            IoHandleTarget::Stdin | IoHandleTarget::Stdout | IoHandleTarget::Stderr
                        )
                    })
                } else {
                    false
                };
                if !is_std {
                    let _ = self.close_handle_value(&target_val)?;
                }
                Ok(Value::Bool(true))
            }
            "path" | "IO" => {
                // For standard handles ($*IN, $*OUT, $*ERR), return IO::Special
                if let Some(id) = Self::handle_id_from_value(&target_val)
                    && let Some(state) = self.io_handles().map.get(&id)
                {
                    let special_name = match state.target {
                        IoHandleTarget::Stdout => Some("STDOUT"),
                        IoHandleTarget::Stderr => Some("STDERR"),
                        IoHandleTarget::Stdin => Some("STDIN"),
                        _ => None,
                    };
                    if let Some(name) = special_name {
                        return Ok(Self::make_io_special_instance(name));
                    }
                }
                if let Some(path_val) = target.get("path") {
                    let io_path = match path_val {
                        Value::Instance { class_name, .. } if class_name == "IO::Path" => {
                            path_val.clone()
                        }
                        _ => self.make_io_path_instance(&path_val.to_string_value()),
                    };
                    return Ok(io_path);
                }
                Ok(Value::Nil)
            }
            "Str" | "gist" => {
                if let Some(path_val) = target.get("path") {
                    let path = match path_val {
                        Value::Instance {
                            class_name,
                            attributes,
                            ..
                        } if class_name == "IO::Path" => attributes
                            .as_map()
                            .get("path")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default(),
                        _ => path_val.to_string_value(),
                    };
                    return Ok(Value::str(path));
                }
                Ok(Value::str_from("IO::Handle()"))
            }
            "open" => {
                // IO::Handle.new(:path(...)).open(:w, :nl-out(...))
                // Merge instance attributes with open args (args override instance attrs)
                let path_str = target
                    .get("path")
                    .map(|v| match v {
                        Value::Instance {
                            class_name,
                            attributes,
                            ..
                        } if class_name == "IO::Path" => attributes
                            .as_map()
                            .get("path")
                            .map(|p| p.to_string_value())
                            .unwrap_or_default(),
                        _ => v.to_string_value(),
                    })
                    .unwrap_or_default();
                let path_buf = self.resolve_path(&path_str);

                // Build merged args: instance attributes as defaults, open args override
                let mut merged_args = Vec::new();
                // Collect which keys the open() args explicitly specify
                let mut explicit_keys: std::collections::HashSet<String> =
                    std::collections::HashSet::new();
                for arg in &args {
                    if let Value::Pair(name, _) = arg {
                        explicit_keys.insert(name.clone());
                    }
                }
                // Add instance attributes as pairs (only if not overridden by open args)
                for (key, value) in target.iter() {
                    if key == "handle" || key == "path" || key == "mode" {
                        continue;
                    }
                    if !explicit_keys.contains(key) {
                        merged_args.push(Value::Pair(key.clone(), Box::new(value.clone())));
                    }
                }
                merged_args.extend(args.iter().cloned());

                let (
                    read,
                    write,
                    append,
                    bin,
                    line_chomp,
                    line_separators,
                    out_buffer_capacity,
                    nl_out,
                    enc,
                    create,
                    exclusive,
                ) = self.parse_io_flags_values(&merged_args);
                match self.open_file_handle(
                    &path_buf,
                    read,
                    write,
                    append,
                    bin,
                    line_chomp,
                    line_separators,
                    out_buffer_capacity,
                    nl_out,
                    enc,
                    create,
                    exclusive,
                ) {
                    Ok(handle) => Ok(handle),
                    // Like the `open` sub, `IO::Handle.open` returns a Failure
                    // (wrapping the exception) on error rather than throwing.
                    Err(err) => {
                        let class_name = err
                            .exception
                            .as_deref()
                            .and_then(|ex| match ex {
                                Value::Instance { class_name, .. } => Some(class_name.to_string()),
                                _ => None,
                            })
                            .unwrap_or_else(|| "X::AdHoc".to_string());
                        Ok(io_exception_failure(&class_name, err.message))
                    }
                }
            }
            "nl-out" => {
                let set = args.first().map(|a| a.to_string_value());
                let nl =
                    self.with_handle_mut(&target_val, |state| Ok(state.nl_out_setting(set)))?;
                Ok(Value::str(nl))
            }
            "nl-in" => {
                if let Some(arg) = args.first() {
                    let _ = self.with_handle_mut_opt(&target_val, |state| {
                        match arg {
                            Value::Array(items, ..) => {
                                let seps: Vec<Vec<u8>> = items
                                    .iter()
                                    .map(|v: &Value| v.to_string_value().into_bytes())
                                    .collect();
                                state.line_separators = seps;
                            }
                            _ => {
                                let s = arg.to_string_value();
                                state.line_separators = vec![s.clone().into_bytes()];
                            }
                        }
                        Ok(())
                    })?;
                    return Ok(arg.clone());
                }
                let got = self.with_handle_mut_opt(&target_val, |state| {
                    if state.line_separators.len() == 1 {
                        Ok(Value::str(
                            String::from_utf8_lossy(&state.line_separators[0]).to_string(),
                        ))
                    } else {
                        let items: Vec<Value> = state
                            .line_separators
                            .iter()
                            .map(|s| Value::str(String::from_utf8_lossy(s).to_string()))
                            .collect();
                        Ok(Value::real_array(items))
                    }
                })?;
                match got {
                    Some(v) => Ok(v),
                    None => {
                        // Default nl-in for unopened handles
                        let items: Vec<Value> = self
                            .default_line_separators()
                            .iter()
                            .map(|s| Value::str(String::from_utf8_lossy(s).to_string()))
                            .collect();
                        if items.len() == 1 {
                            Ok(items.into_iter().next().unwrap())
                        } else {
                            Ok(Value::real_array(items))
                        }
                    }
                }
            }
            "chomp" => {
                let set = args.first().map(|a| a.truthy());
                let chomp =
                    self.with_handle_mut(&target_val, |state| Ok(state.chomp_setting(set)))?;
                Ok(Value::Bool(chomp))
            }
            "print-nl" => {
                let nl = self.with_handle_mut(&target_val, |state| Ok(state.nl_out.clone()))?;
                self.write_to_handle_value(&target_val, &nl, false)?;
                Ok(Value::Bool(true))
            }
            "close" => Ok(Value::Bool(self.close_handle_value(&target_val)?)),
            "get" => Ok(self
                .read_line_from_handle_value(&target_val)?
                .map(Value::str)
                .unwrap_or(Value::Nil)),
            "getc" => {
                let encoding =
                    self.with_handle_mut(&target_val, |state| Ok(state.encoding.clone()))?;
                let needs_decode = !encoding.is_empty()
                    && encoding != "utf-8"
                    && encoding != "utf8"
                    && encoding != "bin";
                if needs_decode {
                    // For single-byte encodings, read 1 byte and decode
                    let bytes = self.read_bytes_from_handle_value(&target_val, 1)?;
                    if bytes.is_empty() {
                        Ok(Value::Nil)
                    } else {
                        let decoded = self.decode_with_encoding(&bytes, &encoding)?;
                        Ok(Value::str(decoded))
                    }
                } else {
                    // UTF-8: read one character, possibly multi-byte.
                    let s = self.read_chars_from_handle_value(&target_val, Some(1))?;
                    if s.is_empty() {
                        Ok(Value::Nil)
                    } else {
                        Ok(Value::str(s))
                    }
                }
            }
            "readchars" => {
                let count = if let Some(arg) = args.first() {
                    match Self::parse_out_buffer_size(arg) {
                        Some(n) => Some(n),
                        None => {
                            return Err(RuntimeError::new(
                                "readchars count must be a non-negative integer",
                            ));
                        }
                    }
                } else {
                    None
                };
                Ok(Value::str(
                    self.read_chars_from_handle_value(&target_val, count)?,
                ))
            }
            "lines" => {
                let mut limit: Option<usize> = None;
                let mut close_after = false;
                for arg in &args {
                    match arg {
                        Value::Pair(k, v) if k == "close" => {
                            close_after = v.truthy();
                        }
                        // Any numeric (incl. allomorphs) is a row limit; named
                        // args and Whatever/+Inf mean "no limit".
                        _ => {
                            if let Some(n) = numeric_limit_arg(arg) {
                                limit = Some(n);
                            }
                        }
                    }
                }
                if limit.is_some() {
                    // Bounded: read eagerly, return Seq
                    let mut lines = Vec::new();
                    while let Some(line) = self.read_line_from_handle_value(&target_val)? {
                        lines.push(Value::str(line));
                        if let Some(n) = limit
                            && lines.len() >= n
                        {
                            break;
                        }
                    }
                    if close_after {
                        self.close_handle_value(&target_val)?;
                    }
                    Ok(Value::Seq(std::sync::Arc::new(lines)))
                } else {
                    // No limit: return a lazy IO lines iterator so that
                    // consumers (e.g. for-loop) can read on demand and
                    // $fh.tell reflects the current position.
                    Ok(Value::LazyIoLines {
                        handle: Box::new(target_val.clone()),
                        kv: false,
                        words: false,
                    })
                }
            }
            "words" => {
                let mut limit: Option<usize> = None;
                let mut close_after = false;
                for arg in &args {
                    match arg {
                        Value::Pair(k, v) if k == "close" => {
                            close_after = v.truthy();
                        }
                        // Any numeric (incl. allomorphs) is a word limit; named
                        // args and Whatever/+Inf mean "no limit".
                        _ => {
                            if let Some(n) = numeric_limit_arg(arg) {
                                limit = Some(n);
                            }
                        }
                    }
                }
                if limit.is_none() {
                    // No limit: return a lazy word iterator so a partial consumer
                    // (e.g. `$fh.words[1,2]`) leaves the handle open, while a full
                    // consumer triggers close-on-exhaust when `:close` was given.
                    if close_after {
                        self.with_handle_mut(&target_val, |state| {
                            state.close_on_word_exhaust = true;
                            Ok(())
                        })?;
                    }
                    return Ok(Value::LazyIoLines {
                        handle: Box::new(target_val.clone()),
                        kv: false,
                        words: true,
                    });
                }
                let mut words = Vec::new();
                'outer: while let Some(word) = self.read_word_from_handle_value(&target_val)? {
                    words.push(Value::str(word));
                    if let Some(n) = limit
                        && words.len() >= n
                    {
                        break 'outer;
                    }
                }
                if close_after {
                    self.close_handle_value(&target_val)?;
                }
                Ok(Value::Seq(std::sync::Arc::new(words)))
            }
            "read" => {
                // .read() always returns a Buf (Buf[uint8]) in Raku
                let count = args
                    .first()
                    .and_then(|v| match v {
                        Value::Int(i) if *i > 0 => Some(*i as usize),
                        _ => None,
                    })
                    .unwrap_or(0);
                if count > 0 {
                    let bytes = self.read_bytes_from_handle_value(&target_val, count)?;
                    return Ok(Self::make_buf(bytes));
                }
                let mut all_bytes = Vec::new();
                loop {
                    let chunk = self.read_bytes_from_handle_value(&target_val, 8192)?;
                    if chunk.is_empty() {
                        break;
                    }
                    all_bytes.extend(chunk);
                }
                Ok(Self::make_buf(all_bytes))
            }
            "write" => {
                let mut bytes = Vec::new();
                for arg in &args {
                    match arg {
                        Value::Instance { class_name, .. }
                            if {
                                let cn = class_name.resolve();
                                cn == "Buf"
                                    || cn == "Blob"
                                    || cn == "utf8"
                                    || cn == "utf16"
                                    || cn.starts_with("buf")
                                    || cn.starts_with("blob")
                                    || cn.starts_with("Buf[")
                                    || cn.starts_with("Blob[")
                            } =>
                        {
                            bytes.extend(self.supply_chunk_to_bytes(arg, "utf-8"));
                        }
                        _ => bytes.extend(self.render_str_value(arg).into_bytes()),
                    }
                }
                // Write raw bytes directly to avoid UTF-8 lossy conversion
                // which corrupts non-UTF-8 binary data (e.g., ISO-8859-1 encoded bytes)
                self.write_bytes_to_handle_value(&target_val, &bytes)?;
                Ok(Value::Bool(true))
            }
            "print" => {
                let mut content = String::new();
                for arg in &args {
                    content.push_str(&self.render_str_value(arg));
                }
                self.write_to_handle_value_trying(&target_val, &content, false, "print")?;
                Ok(Value::Bool(true))
            }
            "printf" => {
                // If the first arg is a Junction, thread through it
                if let Some(Value::Junction { kind: _, values }) = args.first() {
                    let mut content = String::new();
                    for v in values.iter() {
                        content.push_str(&self.render_str_value(v));
                    }
                    self.write_to_handle_value_trying(&target_val, &content, false, "printf")?;
                    Ok(Value::Bool(true))
                } else {
                    let fmt = args
                        .first()
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let rest = &args[1..];
                    super::sprintf::validate_sprintf_directives(&fmt, rest.len())?;
                    let content = super::sprintf::format_sprintf_args(&fmt, rest);
                    self.write_to_handle_value_trying(&target_val, &content, false, "printf")?;
                    Ok(Value::Bool(true))
                }
            }
            "say" => {
                let mut content = String::new();
                for arg in &args {
                    content.push_str(&self.render_gist_value(arg));
                }
                self.write_to_handle_value_trying(&target_val, &content, true, "say")?;
                Ok(Value::Bool(true))
            }
            "put" => {
                let mut content = String::new();
                for arg in &args {
                    content.push_str(&self.render_str_value(arg));
                }
                self.write_to_handle_value_trying(&target_val, &content, true, "put")?;
                Ok(Value::Bool(true))
            }
            "spurt" => {
                // IO::Handle.spurt($data) -- write data to the handle
                let content_value = args
                    .first()
                    .cloned()
                    .unwrap_or(Value::Str(String::new().into()));
                let is_buf = crate::runtime::Interpreter::is_buf_value(&content_value);
                if is_buf {
                    let bytes = crate::runtime::Interpreter::extract_buf_bytes(&content_value);
                    self.write_bytes_to_handle_value(&target_val, &bytes)?;
                } else {
                    let content = content_value.to_string_value();
                    // Determine encoding from the handle's state
                    let enc = self
                        .with_handle_mut_opt(&target_val, |state| Ok(state.encoding.clone()))?
                        .unwrap_or_else(|| "utf-8".to_string());
                    let bytes = if enc == "utf-8" || enc == "utf8" {
                        content.into_bytes()
                    } else {
                        self.encode_with_encoding(&content, &enc)?
                    };
                    self.write_bytes_to_handle_value(&target_val, &bytes)?;
                }
                Ok(Value::Bool(true))
            }
            "flush" => {
                let flushed =
                    self.with_handle_mut_opt(&target_val, |state| state.flush_for_method())?;
                if flushed.is_some() {
                    Ok(Value::Bool(true))
                } else {
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str_from("Failed to flush handle"),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::IO::Flush"), ex_attrs);
                    let mut failure_attrs = HashMap::new();
                    failure_attrs.insert("exception".to_string(), ex);
                    Ok(Value::make_instance(
                        Symbol::intern("Failure"),
                        failure_attrs,
                    ))
                }
            }
            "out-buffer" => {
                let set = args.first().map(Self::parse_out_buffer_size);
                let size =
                    self.with_handle_mut(&target_val, |state| state.out_buffer_setting(set))?;
                Ok(Value::Int(size as i64))
            }
            "seek" => {
                let pos = args
                    .first()
                    .and_then(|v| match v {
                        Value::Int(i) => Some(*i),
                        _ => None,
                    })
                    .unwrap_or(0);
                // Second argument: seek mode
                let mode_str = args
                    .get(1)
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "SeekFromBeginning".to_string());
                let seek_mode = match mode_str.as_str() {
                    "SeekFromBeginning" => 0,
                    "SeekFromCurrent" => 1,
                    "SeekFromEnd" => 2,
                    _ => 0,
                };
                let offset = self.seek_handle_value(&target_val, pos, seek_mode)?;
                Ok(Value::Int(offset))
            }
            "tell" => {
                let position = self.tell_handle_value(&target_val)?;
                Ok(Value::Int(position))
            }
            "eof" => {
                let at_end = self.handle_eof_value(&target_val)?;
                Ok(Value::Bool(at_end))
            }
            "t" => {
                let is_tty = self.with_handle_mut(&target_val, |state| Ok(state.is_tty()))?;
                Ok(Value::Bool(is_tty))
            }
            "encoding" => {
                if let Some(arg) = args.first() {
                    // Nil means switch to binary mode (no encoding)
                    if matches!(arg, Value::Nil) {
                        self.set_handle_encoding(&target_val, Some("bin".to_string()))?;
                        return Ok(Value::Nil);
                    }
                    let encoding = arg.to_string_value();
                    if encoding == "bin" {
                        self.set_handle_encoding(&target_val, Some("bin".to_string()))?;
                        return Ok(Value::Nil);
                    }
                    self.set_handle_encoding(&target_val, Some(encoding.clone()))?;
                    return Ok(Value::str(encoding));
                }
                let current = self.set_handle_encoding(&target_val, None)?;
                if current == "bin" {
                    return Ok(Value::Nil);
                }
                Ok(Value::str(current))
            }
            "opened" => {
                let opened = self.with_handle_mut(&target_val, |state| Ok(state.is_opened()))?;
                Ok(Value::Bool(opened))
            }
            "slurp" => {
                let has_bin_arg = Self::named_bool(&args, "bin");
                let (is_bin, handle_encoding) = self.with_handle_mut(&target_val, |state| {
                    let bin = has_bin_arg || state.bin || state.encoding == "bin";
                    let enc = state.encoding.clone();
                    Ok((bin, enc))
                })?;
                let mut all_bytes = Vec::new();
                loop {
                    let chunk = self.read_bytes_from_handle_value(&target_val, 8192)?;
                    if chunk.is_empty() {
                        break;
                    }
                    all_bytes.extend(chunk);
                }
                if is_bin {
                    return Ok(Self::make_buf(all_bytes));
                }
                // Decode using the handle's encoding if it's not UTF-8
                let needs_decode = !handle_encoding.is_empty()
                    && handle_encoding != "utf-8"
                    && handle_encoding != "utf8"
                    && handle_encoding != "bin";
                if needs_decode {
                    let decoded = self.decode_with_encoding(&all_bytes, &handle_encoding)?;
                    Ok(Value::str(decoded))
                } else {
                    Ok(Value::str(String::from_utf8_lossy(&all_bytes).to_string()))
                }
            }
            "split" => {
                // Slurp the handle, optionally close it, then delegate to the
                // generic Str.split implementation.
                let close = args
                    .iter()
                    .any(|a| matches!(a, Value::Pair(k, v) if k == "close" && v.truthy()));
                // Filter out :close from args before delegating to split.
                let split_args: Vec<Value> = args
                    .iter()
                    .filter(|a| !matches!(a, Value::Pair(k, _) if k == "close"))
                    .cloned()
                    .collect();
                let mut all_bytes = Vec::new();
                loop {
                    let chunk = self.read_bytes_from_handle_value(&target_val, 8192)?;
                    if chunk.is_empty() {
                        break;
                    }
                    all_bytes.extend(chunk);
                }
                let text = String::from_utf8_lossy(&all_bytes).to_string();
                if close {
                    let _ = self.close_handle_value(&target_val)?;
                }
                self.handle_split_method(Value::str(text), split_args)
            }
            "comb" => {
                // Slurp the handle, optionally close it, then delegate to the
                // generic Str.comb implementation.
                let close = args
                    .iter()
                    .any(|a| matches!(a, Value::Pair(k, v) if k == "close" && v.truthy()));
                // Filter out :close from args before delegating to comb.
                let comb_args: Vec<Value> = args
                    .iter()
                    .filter(|a| !matches!(a, Value::Pair(k, _) if k == "close"))
                    .cloned()
                    .collect();
                let mut all_bytes = Vec::new();
                loop {
                    let chunk = self.read_bytes_from_handle_value(&target_val, 8192)?;
                    if chunk.is_empty() {
                        break;
                    }
                    all_bytes.extend(chunk);
                }
                let text = String::from_utf8_lossy(&all_bytes).to_string();
                if close {
                    let _ = self.close_handle_value(&target_val)?;
                }
                match self.dispatch_comb_with_args(Value::str(text), &comb_args) {
                    Some(res) => res,
                    None => Ok(Value::Seq(std::sync::Arc::new(Vec::new()))),
                }
            }
            "Supply" => self.handle_supply(target, &args),
            "native-descriptor" => {
                let fd = self.with_handle_mut(&target_val, |state| state.native_descriptor())?;
                Ok(Value::Int(fd))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Handle",
                method
            ))),
        }
    }

    fn handle_supply(
        &mut self,
        target: &HashMap<String, Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Extract :size named parameter (default 65536)
        let size = args
            .iter()
            .find_map(|a| {
                if let Value::Pair(name, val) = a
                    && name == "size"
                {
                    match val.as_ref() {
                        Value::Int(i) => Some(*i as usize),
                        _ => None,
                    }
                } else {
                    None
                }
            })
            .unwrap_or(65536);

        let is_bin = target.get("bin").is_some_and(|v| v.truthy());

        let target_val = Value::make_instance(Symbol::intern("IO::Handle"), target.clone());

        let mut values = Vec::new();
        if is_bin {
            // Binary mode: read bytes in chunks, produce Buf instances
            loop {
                let bytes = self.read_bytes_from_handle_value(&target_val, size)?;
                if bytes.is_empty() {
                    break;
                }
                for chunk in bytes.chunks(size) {
                    let byte_vals: Vec<Value> =
                        chunk.iter().map(|b| Value::Int(*b as i64)).collect();
                    let mut buf_attrs = HashMap::new();
                    buf_attrs.insert("bytes".to_string(), Value::array(byte_vals));
                    values.push(Value::make_instance(
                        Symbol::intern("Buf[uint8]"),
                        buf_attrs,
                    ));
                }
                if bytes.len() < size {
                    break;
                }
            }
        } else {
            // Text mode: read all content as string, split into chunks of `size` chars
            let path = target.get("path").map(|v| v.to_string_value());
            let content = if let Some(ref p) = path {
                fs::read_to_string(p)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?
            } else {
                // Read from handle
                let mut all = String::new();
                while let Some(line) = self.read_line_from_handle_value(&target_val)? {
                    all.push_str(&line);
                    all.push('\n');
                }
                all
            };
            let chars: Vec<char> = content.chars().collect();
            for chunk in chars.chunks(size) {
                let s: String = chunk.iter().collect();
                values.push(Value::str(s));
            }
        }

        let mut supply_attrs = HashMap::new();
        supply_attrs.insert("live".to_string(), Value::Bool(false));
        supply_attrs.insert("values".to_string(), Value::array(values));
        Ok(Value::make_instance(Symbol::intern("Supply"), supply_attrs))
    }

    pub(super) fn native_io_pipe(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Check if this is a writable stdin pipe (from run :in)
        if let Some(Value::Int(pid)) = attributes.get("proc-pid") {
            let pid_u32 = *pid as u32;
            match method {
                "print" => {
                    // Write to the child's stdin
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                        && let Some(ref mut stdin) = *guard
                    {
                        use std::io::Write;
                        for arg in args {
                            let s = arg.to_string_value();
                            let _ = stdin.write_all(s.as_bytes());
                        }
                        let _ = stdin.flush();
                    }
                    return Ok(Value::Bool(true));
                }
                "say" => {
                    // Write to the child's stdin with a trailing newline
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                        && let Some(ref mut stdin) = *guard
                    {
                        use std::io::Write;
                        for arg in args {
                            let s = arg.to_string_value();
                            let _ = stdin.write_all(s.as_bytes());
                        }
                        let _ = stdin.write_all(b"\n");
                        let _ = stdin.flush();
                    }
                    return Ok(Value::Bool(true));
                }
                "put" => {
                    // Same as say for IO handles
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                        && let Some(ref mut stdin) = *guard
                    {
                        use std::io::Write;
                        for arg in args {
                            let s = arg.to_string_value();
                            let _ = stdin.write_all(s.as_bytes());
                        }
                        let _ = stdin.write_all(b"\n");
                        let _ = stdin.flush();
                    }
                    return Ok(Value::Bool(true));
                }
                "flush" => {
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                        && let Some(ref mut stdin) = *guard
                    {
                        use std::io::Write;
                        let _ = stdin.flush();
                    }
                    return Ok(Value::Bool(true));
                }
                "write" => {
                    // Write binary data (Buf/Blob/utf8) to stdin
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                        && let Some(ref mut stdin) = *guard
                    {
                        use std::io::Write;
                        for arg in args {
                            match arg {
                                Value::Instance {
                                    class_name,
                                    attributes,
                                    ..
                                } if crate::runtime::utils::is_buf_or_blob_class(
                                    &class_name.resolve(),
                                ) =>
                                {
                                    // Try "bytes" first (make_buf), then "data"
                                    let map = attributes.as_map();
                                    let bytes_arr = map.get("bytes").or_else(|| map.get("data"));
                                    if let Some(Value::Array(bytes, _)) = bytes_arr {
                                        let data: Vec<u8> =
                                            bytes.iter().map(|v| v.to_f64() as u8).collect();
                                        let _ = stdin.write_all(&data);
                                    }
                                }
                                _ => {
                                    let s = arg.to_string_value();
                                    let _ = stdin.write_all(s.as_bytes());
                                }
                            }
                        }
                    }
                    return Ok(Value::Bool(true));
                }
                "close" => {
                    // Close the child's stdin (drop it)
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                    {
                        *guard = None; // Drop the ChildStdin
                    }
                    // `.close` on a Proc pipe returns the owning Proc.
                    if let Ok(map) = super::builtins_system::proc_by_pid_map().lock()
                        && let Some(proc) = map.get(pid)
                    {
                        return Ok(proc.clone());
                    }
                    return Ok(Value::Bool(true));
                }
                _ => {}
            }
        }
        // Handle "live" IO::Pipe from a live Proc (has `live-pid`).
        if let Some(Value::Int(live_pid)) = attributes.get("live-pid") {
            let pipe_type = attributes
                .get("pipe-type")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| "out".to_string());
            let finalize_and_get_content = || -> String {
                let pid = *live_pid;
                if let Ok(mut map) = super::builtins_system::live_proc_map().lock()
                    && let Some(mut state) = map.remove(&pid)
                {
                    // Do NOT close stdin here -- let the explicit .close handle it.
                    // Closing stdin prematurely breaks concurrent writes from `start` blocks.
                    let captured_out: Option<String> = if state.capture_out {
                        state.child.stdout.take().map(|mut s| {
                            let mut buf = String::new();
                            use std::io::Read;
                            let _ = s.read_to_string(&mut buf);
                            buf
                        })
                    } else {
                        None
                    };
                    let captured_err: Option<String> = if state.capture_err {
                        state.child.stderr.take().map(|mut s| {
                            let mut buf = String::new();
                            use std::io::Read;
                            let _ = s.read_to_string(&mut buf);
                            buf
                        })
                    } else {
                        None
                    };
                    let (exitcode, signal): (i64, i64) = match state.child.wait() {
                        Ok(status) => {
                            let ec = status.code().unwrap_or(-1) as i64;
                            #[cfg(unix)]
                            let sig = {
                                use std::os::unix::process::ExitStatusExt;
                                status.signal().unwrap_or(0) as i64
                            };
                            #[cfg(not(unix))]
                            let sig = 0i64;
                            (ec, sig)
                        }
                        Err(_) => (-1i64, 0i64),
                    };
                    if let Ok(mut fmap) = super::builtins_system::finalized_proc_map().lock() {
                        fmap.insert(
                            pid,
                            super::builtins_system::FinalizedProc {
                                exitcode,
                                signal,
                                captured_out,
                                captured_err,
                            },
                        );
                    }
                }
                if let Ok(fmap) = super::builtins_system::finalized_proc_map().lock()
                    && let Some(finalized) = fmap.get(&pid)
                {
                    match pipe_type.as_str() {
                        "out" => finalized.captured_out.clone().unwrap_or_default(),
                        "err" => finalized.captured_err.clone().unwrap_or_default(),
                        _ => String::new(),
                    }
                } else {
                    String::new()
                }
            };
            match method {
                "slurp" | "Str" | "gist" => {
                    let has_bin = Self::named_bool(args, "bin");
                    let pipe_bin = matches!(attributes.get("bin"), Some(Value::Bool(true)));
                    let content = finalize_and_get_content();
                    if has_bin || pipe_bin {
                        return Ok(Self::make_buf(content.into_bytes()));
                    }
                    return Ok(Value::str(content));
                }
                "lines" => {
                    let content = finalize_and_get_content();
                    let trimmed = content.strip_suffix('\n').unwrap_or(&content);
                    let lines: Vec<Value> = if trimmed.is_empty() {
                        Vec::new()
                    } else {
                        trimmed
                            .split('\n')
                            .map(|s| Value::str(s.trim_end_matches('\r').to_string()))
                            .collect()
                    };
                    return Ok(Value::array(lines));
                }
                "get" => {
                    let content = finalize_and_get_content();
                    let line = content.lines().next().unwrap_or("");
                    return Ok(Value::str(line.to_string()));
                }
                "close" => {
                    let _ = finalize_and_get_content();
                    // `.close` on a Proc pipe returns the owning Proc.
                    if let Ok(map) = super::builtins_system::proc_by_pid_map().lock()
                        && let Some(proc) = map.get(live_pid)
                    {
                        return Ok(proc.clone());
                    }
                    return Ok(Value::Bool(true));
                }
                "encoding" => return Ok(Value::str("utf8".to_string())),
                "IO" | "path" => {
                    return Ok(Value::Package(crate::symbol::Symbol::intern("IO::Path")));
                }
                _ => {}
            }
        }

        // Look up persistent per-pipe cursor state (if any). Repeated calls
        // on the same logical IO::Pipe share this so `.get` / `.lines` can
        // advance through buffered content line by line.
        let pipe_id = attributes.get("pipe-id").and_then(|v| {
            if let Value::Int(i) = v {
                Some(*i)
            } else {
                None
            }
        });
        if let Some(id) = pipe_id {
            match method {
                "get" => {
                    if let Ok(mut map) = super::builtins_system::io_pipe_state_map().lock()
                        && let Some(state) = map.get_mut(&id)
                    {
                        if state.closed {
                            return Err(RuntimeError::io_closed("get"));
                        }
                        if state.cursor >= state.content.len() {
                            return Ok(Value::Nil);
                        }
                        let rest = &state.content[state.cursor..];
                        let (line, advance) = if let Some(pos) = rest.find('\n') {
                            let line = rest[..pos].trim_end_matches('\r').to_string();
                            (line, pos + 1)
                        } else {
                            (rest.to_string(), rest.len())
                        };
                        state.cursor += advance;
                        return Ok(Value::str(line));
                    }
                    return Ok(Value::Nil);
                }
                "lines" => {
                    if let Ok(mut map) = super::builtins_system::io_pipe_state_map().lock()
                        && let Some(state) = map.get_mut(&id)
                    {
                        if state.closed {
                            return Err(RuntimeError::io_closed("lines"));
                        }
                        let rest = state.content[state.cursor..].to_string();
                        state.cursor = state.content.len();
                        let trimmed = rest.strip_suffix('\n').unwrap_or(&rest);
                        let lines: Vec<Value> = if trimmed.is_empty() {
                            Vec::new()
                        } else {
                            trimmed
                                .split('\n')
                                .map(|s| Value::str(s.trim_end_matches('\r').to_string()))
                                .collect()
                        };
                        return Ok(Value::array(lines));
                    }
                    return Ok(Value::array(vec![]));
                }
                "eof" => {
                    if let Ok(map) = super::builtins_system::io_pipe_state_map().lock()
                        && let Some(state) = map.get(&id)
                    {
                        return Ok(Value::Bool(state.cursor >= state.content.len()));
                    }
                    return Ok(Value::Bool(true));
                }
                _ => {}
            }
        }
        let content = attributes
            .get("content")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        match method {
            "slurp" | "Str" | "gist" => {
                let has_bin = Self::named_bool(args, "bin");
                let pipe_bin = matches!(attributes.get("bin"), Some(Value::Bool(true)));
                if has_bin || pipe_bin {
                    return Ok(Self::make_buf(content.into_bytes()));
                }
                Ok(Value::str(content))
            }
            "encoding" => Ok(Value::str("utf8".to_string())),
            "close" => {
                if let Some(id) = pipe_id
                    && let Ok(mut map) = super::builtins_system::io_pipe_state_map().lock()
                    && let Some(state) = map.get_mut(&id)
                {
                    state.closed = true;
                }
                // Return the parent Proc object so that `$pipe.close === $proc`
                if let Some(id) = pipe_id
                    && let Ok(map) = super::builtins_system::pipe_proc_map().lock()
                    && let Some(proc_val) = map.get(&id)
                {
                    return Ok(proc_val.clone());
                }
                Ok(Value::Bool(true))
            }
            "proc" => {
                // Return the parent Proc object
                if let Some(id) = pipe_id
                    && let Ok(map) = super::builtins_system::pipe_proc_map().lock()
                    && let Some(proc_val) = map.get(&id)
                {
                    return Ok(proc_val.clone());
                }
                Ok(Value::Nil)
            }
            "IO" | "path" => {
                // Returns the IO::Path type object (not an instance)
                Ok(Value::Package(crate::symbol::Symbol::intern("IO::Path")))
            }
            "split" => {
                // Basic split for IO::Pipe
                let separator = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let skip_empty = args
                    .iter()
                    .any(|a| matches!(a, Value::Pair(k, v) if k == "skip-empty" && v.truthy()));
                let parts: Vec<Value> = content
                    .split(&separator)
                    .filter(|s| !skip_empty || !s.is_empty())
                    .map(|s| Value::str(s.to_string()))
                    .collect();
                Ok(Value::array(parts))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Pipe",
                method
            ))),
        }
    }
}
