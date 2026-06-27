use super::*;

impl Interpreter {
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
}
