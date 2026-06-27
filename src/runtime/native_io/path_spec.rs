use super::*;

impl Interpreter {
    /// Join a `child`/`add` name onto a path lexically (no filesystem access),
    /// honoring the receiver's SPEC (Win32 vs POSIX separators). Raises
    /// `X::IO::Null` for an embedded null byte. Shared by `try_io_path_lexical`
    /// (the pure fast path) and `native_io_path`'s `child :secure` arm so the
    /// join is implemented once.
    pub(crate) fn io_path_join_child(
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

    pub(crate) fn split_path_for_extension(path: &str) -> (&str, &str) {
        if let Some(idx) = path.rfind(['/', '\\']) {
            (&path[..=idx], &path[idx + 1..])
        } else {
            ("", path)
        }
    }

    pub(crate) fn clone_io_path_with_path(
        attributes: &HashMap<String, Value>,
        class: Symbol,
        path: String,
    ) -> Value {
        let mut new_attrs = attributes.clone();
        new_attrs.insert("path".to_string(), Value::str(path));
        Value::make_instance(class, new_attrs)
    }

    /// Check if the IO::Path instance has a Win32 SPEC attribute.
    pub(crate) fn is_win32_spec(attributes: &HashMap<String, Value>) -> bool {
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
    pub(crate) fn is_cygwin_spec(attributes: &HashMap<String, Value>) -> bool {
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
    pub(crate) fn io_path_is_absolute_spec(
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
    pub(crate) fn io_path_parts_spec(
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
    pub(crate) fn io_path_sep(attributes: &HashMap<String, Value>) -> char {
        if Self::is_win32_spec(attributes) {
            '\\'
        } else {
            '/'
        }
    }

    /// Check if a path is absolute in the context of a Win32 SPEC.
    pub(crate) fn io_path_is_absolute_win32(path: &str) -> bool {
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

    pub(crate) fn io_path_parts(path: &str) -> (String, String, String) {
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
    pub(crate) fn join_io_path_parts(
        volume: &str,
        dirname: &str,
        basename: &str,
        sep: char,
    ) -> String {
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

    pub(crate) fn io_path_extension_part_count(path: &str) -> i64 {
        let (_, basename) = Self::split_path_for_extension(path);
        basename.chars().filter(|&c| c == '.').count() as i64
    }
}
