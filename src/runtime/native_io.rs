use super::*;
use crate::symbol::Symbol;
use num_traits::ToPrimitive;
use std::path::Component;

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
fn metadata_is_readable(metadata: &fs::Metadata) -> bool {
    metadata.permissions().mode() & 0o444 != 0
}

#[cfg(not(unix))]
fn metadata_is_readable(_metadata: &fs::Metadata) -> bool {
    true
}

#[cfg(unix)]
fn metadata_is_writable(metadata: &fs::Metadata) -> bool {
    metadata.permissions().mode() & 0o222 != 0
}

#[cfg(not(unix))]
fn metadata_is_writable(metadata: &fs::Metadata) -> bool {
    !metadata.permissions().readonly()
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
    pub(super) fn native_io_path(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
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
            "Str" => Ok(Value::str(p.clone())),
            "gist" => {
                // IO::Path.gist returns "path".IO with the path quoted
                // Only escape double quotes, not backslashes
                Ok(Value::str(format!("\"{}\".IO", p.replace('"', "\\\""))))
            }
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
            "IO" => Ok(Value::make_instance(
                Symbol::intern("IO::Path"),
                attributes.clone(),
            )),
            "basename" => {
                let (_, _, bname) = Self::io_path_parts(&p);
                Ok(Value::str(bname))
            }
            "dirname" => {
                let (_, dname, _) = Self::io_path_parts(&p);
                Ok(Value::str(dname))
            }
            "cleanup" => {
                let normalized = if Self::is_win32_spec(attributes) {
                    Self::cleanup_io_path_lexical_win32(&p)
                } else {
                    Self::cleanup_io_path_lexical(&p)
                };
                Ok(Self::clone_io_path_with_path(attributes, normalized))
            }
            "parts" => {
                let (volume, dirname, basename) = Self::io_path_parts(&p);
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
                        return Err(RuntimeError::new(
                            "X::IO::ParentOutOfRange: Cannot go to a negative parent",
                        ));
                    }
                    levels = *i;
                }
                if levels == 0 {
                    return Ok(Value::make_instance(
                        Symbol::intern("IO::Path"),
                        attributes.clone(),
                    ));
                }
                let sep = Self::io_path_sep(attributes);
                let mut path = p.clone();
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
                            // Already at root, stop
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
                Ok(Value::make_instance(Symbol::intern("IO::Path"), new_attrs))
            }
            "sibling" => {
                let sibling_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                // For sibling, get the dirname and join with the new name
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
                Ok(Self::clone_io_path_with_path(attributes, sibling_path))
            }
            "child" | "add" => {
                let child_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                if child_name.contains('\0') || p.contains('\0') {
                    return Err(RuntimeError::new(
                        "X::IO::Null: Found null byte in pathname",
                    ));
                }
                let joined = if Self::is_win32_spec(attributes) {
                    if p == "." {
                        child_name.clone()
                    } else if p.ends_with('\\') || p.ends_with('/') {
                        format!("{}{}", p, child_name)
                    } else {
                        format!("{}\\{}", p, child_name)
                    }
                } else if p == "." {
                    child_name.clone()
                } else if p.ends_with('/') {
                    format!("{}{}", p, child_name)
                } else {
                    Self::stringify_path(&original.join(&child_name))
                };
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(joined));
                Ok(Value::make_instance(Symbol::intern("IO::Path"), new_attrs))
            }
            "extension" => {
                let subst = Self::positional_value(&args, 0).map(|v| v.to_string_value());
                let parts_spec = Self::io_path_extension_parts_spec(&args)?;
                let selected_parts = parts_spec.select(Self::io_path_extension_part_count(&p));

                if let Some(subst) = subst {
                    let Some(parts_to_replace) = selected_parts else {
                        return Ok(Value::make_instance(
                            Symbol::intern("IO::Path"),
                            attributes.clone(),
                        ));
                    };
                    let joiner = Self::named_value(&args, "joiner")
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
                        return Ok(Value::make_instance(
                            Symbol::intern("IO::Path"),
                            attributes.clone(),
                        ));
                    };
                    let mut new_basename = format!("{base_without_ext}{joiner}{subst}");
                    // Raku: empty basename after replacement becomes "."
                    if new_basename.is_empty() {
                        new_basename = ".".to_string();
                    }
                    let mut new_attrs = attributes.clone();
                    new_attrs.insert(
                        "path".to_string(),
                        Value::str(format!("{dir_prefix}{new_basename}")),
                    );
                    Ok(Value::make_instance(Symbol::intern("IO::Path"), new_attrs))
                } else {
                    let Some(parts) = selected_parts else {
                        return Ok(Value::str(String::new()));
                    };
                    Ok(Value::str(
                        Self::io_path_extension_with_n_parts(&p, parts).unwrap_or_default(),
                    ))
                }
            }
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
                    Ok(Value::str(abs))
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
                    // Normalize separators for comparison
                    let norm_p = p.replace('/', "\\");
                    let norm_base = base.replace('/', "\\");
                    let rel = norm_p
                        .strip_prefix(&norm_base)
                        .and_then(|r| r.strip_prefix('\\'))
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
            "starts-with" => {
                let prefix = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::Bool(p.starts_with(&prefix)))
            }
            "resolve" => {
                let completely = Self::named_bool(&args, "completely");
                let resolved = Self::resolve_io_path(&path_buf, completely, &p)?;
                Ok(Self::clone_io_path_with_path(attributes, resolved))
            }
            "volume" => {
                let (volume, _, _) = Self::io_path_parts(&p);
                Ok(Value::str(volume))
            }
            "is-absolute" => {
                let abs = if Self::is_win32_spec(attributes) {
                    Self::io_path_is_absolute_win32(&p)
                } else {
                    original.is_absolute()
                };
                Ok(Value::Bool(abs))
            }
            "is-relative" => {
                let abs = if Self::is_win32_spec(attributes) {
                    Self::io_path_is_absolute_win32(&p)
                } else {
                    original.is_absolute()
                };
                Ok(Value::Bool(!abs))
            }
            "succ" => {
                let (volume, dirname, basename) = Self::io_path_parts(&p);
                let new_basename = crate::builtins::str_increment::string_succ(&basename);
                let sep = Self::io_path_sep(attributes);
                let new_path = Self::join_io_path_parts(&volume, &dirname, &new_basename, sep);
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(new_path));
                Ok(Value::make_instance(Symbol::intern("IO::Path"), new_attrs))
            }
            "pred" => {
                let (volume, dirname, basename) = Self::io_path_parts(&p);
                let new_basename = crate::builtins::str_increment::string_pred(&basename);
                let sep = Self::io_path_sep(attributes);
                let new_path = Self::join_io_path_parts(&volume, &dirname, &new_basename, sep);
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(new_path));
                Ok(Value::make_instance(Symbol::intern("IO::Path"), new_attrs))
            }
            "e" => Ok(Value::Bool(path_buf.exists())),
            "f" => Ok(Value::Bool(
                io_path_metadata(&path_buf, &p, method)?.is_file(),
            )),
            "d" => Ok(Value::Bool(
                io_path_metadata(&path_buf, &p, method)?.is_dir(),
            )),
            "l" => {
                let linked = fs::symlink_metadata(&path_buf)
                    .map(|meta| meta.file_type().is_symlink())
                    .map_err(|_| io_path_missing_error(&p, method))?;
                Ok(Value::Bool(linked))
            }
            "r" => Ok(Value::Bool(metadata_is_readable(&io_path_metadata(
                &path_buf, &p, method,
            )?))),
            "w" => Ok(Value::Bool(metadata_is_writable(&io_path_metadata(
                &path_buf, &p, method,
            )?))),
            "x" => {
                let executable =
                    Self::metadata_is_executable(&io_path_metadata(&path_buf, &p, method)?);
                Ok(Value::Bool(executable))
            }
            "rw" => {
                let meta = io_path_metadata(&path_buf, &p, method)?;
                Ok(Value::Bool(
                    metadata_is_readable(&meta) && metadata_is_writable(&meta),
                ))
            }
            "rwx" => {
                let meta = io_path_metadata(&path_buf, &p, method)?;
                Ok(Value::Bool(
                    metadata_is_readable(&meta)
                        && metadata_is_writable(&meta)
                        && Self::metadata_is_executable(&meta),
                ))
            }
            "mode" => {
                let metadata = fs::metadata(&path_buf)
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
                let size = io_path_metadata(&path_buf, &p, method)?.len();
                Ok(Value::Int(size as i64))
            }
            "z" => match fs::metadata(&path_buf) {
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
            "created" => {
                let ts = fs::metadata(&path_buf)
                    .and_then(|meta| meta.created())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get created time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "modified" => {
                let ts = fs::metadata(&path_buf)
                    .and_then(|meta| meta.modified())
                    .map(Self::system_time_to_int)
                    .map_err(|err| {
                        RuntimeError::new(format!("Failed to get modified time '{}': {}", p, err))
                    })?;
                Ok(Value::Int(ts))
            }
            "accessed" => {
                let ts = fs::metadata(&path_buf)
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
                    let meta = fs::metadata(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to get changed time '{}': {}", p, err))
                    })?;
                    Ok(Value::Int(meta.ctime()))
                }
                #[cfg(not(unix))]
                {
                    // On non-Unix platforms, fall back to modified time
                    let ts = fs::metadata(&path_buf)
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
            "lines" => {
                let content = fs::read_to_string(&path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?;
                let content = super::utils::strip_utf8_bom(content);
                let parts = content
                    .lines()
                    .map(|line| Value::str(line.to_string()))
                    .collect();
                Ok(Value::array(parts))
            }
            "words" => {
                let content = fs::read_to_string(&path_buf)
                    .map_err(|err| RuntimeError::new(format!("Failed to read '{}': {}", p, err)))?;
                let content = super::utils::strip_utf8_bom(content);
                let parts = content
                    .split_whitespace()
                    .map(|token| Value::str(token.to_string()))
                    .collect();
                Ok(Value::array(parts))
            }
            "slurp" => {
                let (_, _, _, bin, _, _, _, _, enc, _) = self.parse_io_flags_values(&args);
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
                // If an encoding is specified and it's not utf-8, read raw bytes
                // and decode with the specified encoding
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
                    let content = super::utils::strip_utf8_bom(content);
                    Ok(Value::str(content))
                }
            }
            "open" => {
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
                ) = self.parse_io_flags_values(&args);
                self.open_file_handle(
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
                )
            }
            "copy" => {
                let dest = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("copy requires destination"))?;
                let createonly = Self::named_bool(&args, "createonly");
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
                let createonly = Self::named_bool(&args, "createonly");
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
            "mkdir" => match fs::create_dir_all(&path_buf) {
                Ok(()) => Ok(Value::make_instance(
                    Symbol::intern("IO::Path"),
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
                    Value::make_instance(Symbol::intern("IO::Path"), attrs)
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
            "spurt" => {
                let content_value = args
                    .first()
                    .cloned()
                    .unwrap_or(Value::Str(String::new().into()));
                let mut append = false;
                let mut createonly = false;
                for arg in args.iter().skip(1) {
                    if let Value::Pair(key, val) = arg {
                        match key.as_str() {
                            "append" => append = val.truthy(),
                            "createonly" => createonly = val.truthy(),
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
                let is_buf = crate::vm::VM::is_buf_value(&content_value);
                let write_result = if is_buf {
                    let bytes = crate::vm::VM::extract_buf_bytes(&content_value);
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
                    if append {
                        use std::io::Write;
                        fs::OpenOptions::new()
                            .append(true)
                            .create(true)
                            .open(&path_buf)
                            .and_then(|mut file| file.write_all(content.as_bytes()))
                    } else {
                        fs::write(&path_buf, &content)
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
            "unlink" => match fs::remove_file(&path_buf) {
                Ok(()) => Ok(Value::Bool(true)),
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(Value::Bool(false)),
                Err(err) => Err(RuntimeError::new(format!(
                    "Failed to unlink '{}': {}",
                    p, err
                ))),
            },
            "symlink" => {
                // IO::Path.symlink($name, :$absolute = True)
                // Creates a symlink named $name pointing to self (the target).
                let link_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .ok_or_else(|| RuntimeError::new("symlink requires a link name"))?;
                // Check :absolute named arg (defaults to True)
                // named_bool returns false if not present, so we invert the logic:
                // :!absolute → Pair("absolute", false) → named_bool returns false → absolute=false
                // :absolute  → Pair("absolute", true)  → named_bool returns true  → absolute=true
                // absent     → named_bool returns false → but default should be true
                let absolute = Self::named_value(&args, "absolute")
                    .map(|v| v.truthy())
                    .unwrap_or(true);
                let link_buf = self.resolve_path(&link_name);
                // Determine the target path: if :absolute (default), use the resolved
                // absolute path; if :!absolute, use the original path string as-is.
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

    fn clone_io_path_with_path(attributes: &HashMap<String, Value>, path: String) -> Value {
        let mut new_attrs = attributes.clone();
        new_attrs.insert("path".to_string(), Value::str(path));
        Value::make_instance(Symbol::intern("IO::Path"), new_attrs)
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
    /// Preserves a leading `//` (POSIX implementation-defined) but collapses
    /// three or more leading slashes to a single `/`.
    pub fn canonpath_unix(path: &str, parent: bool) -> String {
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
        } else if leading == 2 {
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
        // For "//"-prefixed paths, do not collapse leading `..` — POSIX leaves
        // `//` implementation-defined and Rakudo's IO::Spec::Unix preserves it.
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

    /// Win32-specific cleanup: always uses `\` as separator.
    pub fn cleanup_io_path_lexical_win32(path: &str) -> String {
        if path.is_empty() {
            return ".".to_string();
        }
        let mut prefix = String::new();
        let mut rest = path;
        // Check for UNC paths
        let bytes = path.as_bytes();
        if path.len() >= 2
            && ((bytes[0] == b'\\' && bytes[1] == b'\\') || (bytes[0] == b'/' && bytes[1] == b'/'))
        {
            // Extract UNC volume
            let after = &path[2..];
            if let Some(sep1) = after.find(['/', '\\']) {
                let after_server = &after[sep1 + 1..];
                let share_end = after_server.find(['/', '\\']).unwrap_or(after_server.len());
                let unc_end = 2 + sep1 + 1 + share_end;
                prefix = path[..unc_end].replace('/', "\\");
                rest = &path[unc_end..];
            } else {
                return path.replace('/', "\\");
            }
        } else if path.len() >= 2 && bytes[1] == b':' && bytes[0].is_ascii_alphabetic() {
            prefix = path[..2].to_string();
            rest = &path[2..];
        }
        let is_absolute = rest.starts_with('/') || rest.starts_with('\\');
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
        let joined = stack.join("\\");
        let mut out = prefix;
        if is_absolute {
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
                    self.handles.get(&id).is_some_and(|s| {
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
                    && let Some(state) = self.handles.get(&id)
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
                ) = self.parse_io_flags_values(&merged_args);
                self.open_file_handle(
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
                )
            }
            "nl-out" => {
                if let Some(arg) = args.first() {
                    let val = arg.to_string_value();
                    let state = self.handle_state_mut(&target_val)?;
                    state.nl_out = val.clone();
                    return Ok(Value::str(val));
                }
                let state = self.handle_state_mut(&target_val)?;
                Ok(Value::str(state.nl_out.clone()))
            }
            "nl-in" => {
                if let Some(arg) = args.first() {
                    if let Ok(state) = self.handle_state_mut(&target_val) {
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
                    }
                    return Ok(arg.clone());
                }
                if let Ok(state) = self.handle_state_mut(&target_val) {
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
                } else {
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
            "chomp" => {
                if let Some(arg) = args.first() {
                    let val = arg.truthy();
                    let state = self.handle_state_mut(&target_val)?;
                    state.line_chomp = val;
                    return Ok(Value::Bool(val));
                }
                let state = self.handle_state_mut(&target_val)?;
                Ok(Value::Bool(state.line_chomp))
            }
            "print-nl" => {
                let nl = {
                    let state = self.handle_state_mut(&target_val)?;
                    state.nl_out.clone()
                };
                self.write_to_handle_value(&target_val, &nl, false)?;
                Ok(Value::Bool(true))
            }
            "close" => Ok(Value::Bool(self.close_handle_value(&target_val)?)),
            "get" => Ok(self
                .read_line_from_handle_value(&target_val)?
                .map(Value::str)
                .unwrap_or(Value::Nil)),
            "getc" => {
                let encoding = {
                    let state = self.handle_state_mut(&target_val)?;
                    state.encoding.clone()
                };
                let needs_decode = !encoding.is_empty()
                    && encoding != "utf-8"
                    && encoding != "utf8"
                    && encoding != "bin";
                if needs_decode {
                    // For single-byte encodings, read 1 byte and decode
                    let bytes = self.read_bytes_from_handle_value(&target_val, 1)?;
                    if bytes.is_empty() {
                        Ok(Value::str(String::new()))
                    } else {
                        let decoded = self.decode_with_encoding(&bytes, &encoding)?;
                        Ok(Value::str(decoded))
                    }
                } else {
                    // UTF-8: may need multiple bytes for one character
                    let bytes = self.read_bytes_from_handle_value(&target_val, 1)?;
                    Ok(Value::str(String::from_utf8_lossy(&bytes).to_string()))
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
                let limit = args.first().and_then(|arg| match arg {
                    Value::Int(i) => Some((*i).max(0) as usize),
                    Value::BigInt(bi) => {
                        use num_traits::ToPrimitive;
                        Some(bi.to_usize().unwrap_or(usize::MAX))
                    }
                    Value::Whatever => None,
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                    Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                    Value::Rat(n, d) if *d == 0 && *n > 0 => None,
                    Value::Rat(n, d) if *d != 0 => {
                        Some(((*n as f64 / *d as f64) as i64).max(0) as usize)
                    }
                    Value::Complex(re, im) if *im == 0.0 && *re >= 0.0 => Some(*re as usize),
                    _ => None,
                });
                if limit.is_some() {
                    // Bounded: read eagerly
                    let mut lines = Vec::new();
                    while let Some(line) = self.read_line_from_handle_value(&target_val)? {
                        lines.push(Value::str(line));
                        if let Some(n) = limit
                            && lines.len() >= n
                        {
                            break;
                        }
                    }
                    Ok(Value::array(lines))
                } else {
                    // No limit: return a lazy IO lines iterator so that
                    // consumers (e.g. for-loop) can read on demand and
                    // $fh.tell reflects the current position.
                    Ok(Value::LazyIoLines {
                        handle: Box::new(target_val.clone()),
                        kv: false,
                    })
                }
            }
            "words" => {
                let mut words = Vec::new();
                while let Some(line) = self.read_line_from_handle_value(&target_val)? {
                    for token in line.split_whitespace() {
                        words.push(Value::str(token.to_string()));
                    }
                }
                Ok(Value::array(words))
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
            "flush" => {
                if let Ok(state) = self.handle_state_mut(&target_val) {
                    Self::flush_file_handle_buffer(state)?;
                    if let Some(file) = state.file.as_mut() {
                        file.flush().map_err(|err| {
                            RuntimeError::new(format!("Failed to flush handle: {}", err))
                        })?;
                    }
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
                let state = self.handle_state_mut(&target_val)?;
                if let Some(arg) = args.first() {
                    Self::flush_file_handle_buffer(state)?;
                    state.out_buffer_capacity = Self::parse_out_buffer_size(arg);
                }
                let size = state.out_buffer_capacity.unwrap_or(0);
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
                // Check if the handle is a TTY
                use std::io::IsTerminal;
                let state = self.handle_state_mut(&target_val)?;
                let is_tty = match state.target {
                    IoHandleTarget::Stdin => std::io::stdin().is_terminal(),
                    IoHandleTarget::Stdout => std::io::stdout().is_terminal(),
                    IoHandleTarget::Stderr => std::io::stderr().is_terminal(),
                    IoHandleTarget::File => {
                        if let Some(file) = state.file.as_ref() {
                            file.is_terminal()
                        } else {
                            false
                        }
                    }
                    _ => false,
                };
                Ok(Value::Bool(is_tty))
            }
            "encoding" => {
                if let Some(arg) = args.first() {
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
                let state = self.handle_state_mut(&target_val)?;
                Ok(Value::Bool(!state.closed))
            }
            "slurp" => {
                let has_bin_arg = Self::named_bool(&args, "bin");
                let (is_bin, handle_encoding) = {
                    let state = self.handle_state_mut(&target_val)?;
                    let bin = has_bin_arg || state.bin || state.encoding == "bin";
                    let enc = state.encoding.clone();
                    (bin, enc)
                };
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
            "Supply" => self.handle_supply(target, &args),
            "native-descriptor" => {
                let state = self.handle_state_mut(&target_val)?;
                let fd = match state.target {
                    IoHandleTarget::Stdin => 0i64,
                    IoHandleTarget::Stdout => 1i64,
                    IoHandleTarget::Stderr => 2i64,
                    _ => {
                        #[cfg(unix)]
                        {
                            if let Some(ref file) = state.file {
                                use std::os::unix::io::AsRawFd;
                                file.as_raw_fd() as i64
                            } else {
                                return Err(RuntimeError::new(
                                    "native-descriptor: handle has no file descriptor",
                                ));
                            }
                        }
                        #[cfg(not(unix))]
                        {
                            let _ = state;
                            return Err(RuntimeError::new(
                                "native-descriptor: not supported on this platform",
                            ));
                        }
                    }
                };
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
                "close" => {
                    // Close the child's stdin (drop it)
                    if let Ok(map) = super::native_methods::proc_stdin_map().lock()
                        && let Some(stdin_arc) = map.get(&pid_u32)
                        && let Ok(mut guard) = stdin_arc.lock()
                    {
                        *guard = None; // Drop the ChildStdin
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
            "slurp" | "Str" | "gist" => Ok(Value::str(content)),
            "encoding" => Ok(Value::str("utf8".to_string())),
            "close" => {
                // TODO: should return the parent Proc object for identity
                Ok(Value::Bool(true))
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
