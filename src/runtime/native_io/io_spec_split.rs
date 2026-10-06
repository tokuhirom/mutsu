//! `IO::Spec::*`'s path primitives (splitting and joining), one function per method, shared by the
//! method table's rows (ADR-11276 §9.19). A function reads its receiver's class as
//! a [`SpecKind`], so `IO::Spec::Win32.join` and `IO::Spec::Unix.join` are one
//! implementation.

use super::io_spec_kind::SpecKind;
use super::*;

impl Interpreter {

    /// `IO::Spec::*.splitpath($path, :nofile)`: the volume, directory and file as a list.
    // Cost: O(n), n = path length.
    pub(crate) fn io_spec_splitpath(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        let mut positional: Vec<&Value> = Vec::new();
        let mut nofile = false;
        for a in args {
            if let ValueView::Pair(k, v) = a.view() {
                if k == "nofile" {
                    nofile = v.truthy();
                }
            } else {
                positional.push(a);
            }
        }
        let path = positional
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if is_win32 {
            let (volume, after_vol) = Self::split_win32_volume_normalized(&path);
            let (dir, file) = if nofile
                || after_vol.ends_with('/')
                || after_vol.ends_with('\\')
            {
                (after_vol, String::new())
            } else if after_vol == "." || after_vol == ".." {
                (String::new(), after_vol)
            } else {
                let last_sep = after_vol.rfind(['/', '\\']);
                let basename = last_sep
                    .map(|pos| &after_vol[pos + 1..])
                    .unwrap_or(&after_vol);
                if basename == "." || basename == ".." {
                    (after_vol.to_string(), String::new())
                } else if let Some(pos) = last_sep {
                    (
                        after_vol[..=pos].to_string(),
                        after_vol[pos + 1..].to_string(),
                    )
                } else {
                    (String::new(), after_vol.to_string())
                }
            };
            return Ok(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                    Value::str(volume),
                    Value::str(dir),
                    Value::str(file),
                ])),
                crate::value::ArrayKind::List,
            ));
        }
        if path == "." {
            return Ok(Value::array(vec![
                Value::str_from(""),
                Value::str_from(""),
                Value::str_from("."),
            ]));
        }
        let basename = path
            .rfind('/')
            .map(|pos| &path[pos + 1..])
            .unwrap_or(path.as_str());
        let (dir, file) =
            if nofile || path.ends_with('/') || basename == "." || basename == ".."
            {
                (path.as_str(), "")
            } else if let Some(pos) = path.rfind('/') {
                (&path[..=pos], &path[pos + 1..])
            } else {
                ("", path.as_str())
            };
        Ok(Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                Value::str_from(""),
                Value::str(dir.to_string()),
                Value::str(file.to_string()),
            ])),
            crate::value::ArrayKind::List,
        ))
    }

    /// `IO::Spec::*.split($path)`: the volume, dirname and basename as an `IO::Path::Parts`.
    // Cost: O(n), n = path length.
    pub(crate) fn io_spec_split(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        let is_cygwin = kind == SpecKind::Cygwin;
        let raw_path = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if is_win32 {
            let (volume, after_vol) = Self::split_win32_volume(&raw_path);
            let rest = after_vol;
            let is_sep = |c: char| c == '/' || c == '\\';
            let only_seps = !rest.is_empty() && rest.chars().all(is_sep);
            let (dirname, basename) = if only_seps {
                let sep = rest.chars().next().unwrap().to_string();
                (sep.clone(), sep)
            } else if rest.ends_with('/') || rest.ends_with('\\') {
                let trimmed = rest.trim_end_matches(['/', '\\']);
                if let Some(pos) = trimmed.rfind(['/', '\\']) {
                    let dir = if pos == 0 {
                        trimmed[..=pos].to_string()
                    } else {
                        trimmed[..pos].to_string()
                    };
                    (dir, trimmed[pos + 1..].to_string())
                } else {
                    (".".to_string(), trimmed.to_string())
                }
            } else if let Some(pos) = rest.rfind(['/', '\\']) {
                let dir = if pos == 0 {
                    "\\".to_string()
                } else {
                    rest[..pos].to_string()
                };
                (dir, rest[pos + 1..].to_string())
            } else if rest == "." {
                (".".to_string(), ".".to_string())
            } else if rest.is_empty() {
                if volume.starts_with("//") || volume.starts_with("\\\\") {
                    ("\\".to_string(), "\\".to_string())
                } else {
                    (String::new(), String::new())
                }
            } else {
                (".".to_string(), rest)
            };
            let mut hash = std::collections::HashMap::new();
            hash.insert("volume".to_string(), Value::str(volume));
            hash.insert("dirname".to_string(), Value::str(dirname));
            hash.insert("basename".to_string(), Value::str(basename));
            return Ok(Value::make_instance(
                crate::symbol::Symbol::intern("IO::Path::Parts"),
                hash,
            ));
        }
        let path = if is_cygwin {
            raw_path.replace('\\', "/")
        } else {
            raw_path
        };
        let (volume, rest) = if is_cygwin {
            Self::split_cygwin_volume(&path)
        } else {
            ("".to_string(), path)
        };
        let only_seps = !rest.is_empty() && rest.chars().all(|c| c == '/');
        let (dirname, basename) = if only_seps {
            ("/", "/")
        } else if rest.is_empty() {
            ("", "")
        } else if rest.ends_with('/') {
            let trimmed = rest.trim_end_matches('/');
            if let Some(pos) = trimmed.rfind('/') {
                let dir = if pos == 0 { "/" } else { &trimmed[..pos] };
                (dir, &trimmed[pos + 1..])
            } else {
                (".", trimmed)
            }
        } else if let Some(pos) = rest.rfind('/') {
            let dir = if pos == 0 { "/" } else { &rest[..pos] };
            (dir, &rest[pos + 1..])
        } else if rest == "." {
            (".", ".")
        } else {
            (".", rest.as_str())
        };
        let mut hash = std::collections::HashMap::new();
        hash.insert("volume".to_string(), Value::str(volume));
        hash.insert("dirname".to_string(), Value::str(dirname.to_string()));
        hash.insert("basename".to_string(), Value::str(basename.to_string()));
        Ok(Value::make_instance(
            crate::symbol::Symbol::intern("IO::Path::Parts"),
            hash,
        ))
    }

    /// `IO::Spec::*.join($volume, $dirname, $basename)`: the path they make.
    // Cost: O(n), n = total input length.
    pub(crate) fn io_spec_join(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        let is_cygwin = kind == SpecKind::Cygwin;
        let vol = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let dir = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
        let file = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
        let dir_nonempty = !dir.is_empty();
        if is_win32 {
            let path_part = if file.is_empty() {
                if dir.is_empty() { String::new() } else { dir }
            } else if dir.is_empty() || dir == "." {
                file
            } else {
                let dir_is_sep = dir.chars().all(|c| c == '/' || c == '\\');
                let file_is_sep = file.chars().all(|c| c == '/' || c == '\\');
                if dir_is_sep && file_is_sep {
                    dir
                } else if dir.ends_with('/') || dir.ends_with('\\') {
                    format!("{}{}", dir, file)
                } else {
                    format!("{}\\{}", dir, file)
                }
            };
            let result = if !vol.is_empty() {
                if vol.starts_with("\\\\") || vol.starts_with("//") {
                    let path_only_seps = !path_part.is_empty()
                        && path_part.chars().all(|c| c == '/' || c == '\\');
                    if path_only_seps || path_part.is_empty() {
                        vol
                    } else {
                        format!("{}{}", vol, path_part)
                    }
                } else {
                    // Insert a separator between a bare volume and
                    // the directory when the directory is non-empty
                    // and neither side already carries a boundary
                    // separator. A drive volume (`C:`) joins
                    // directly (`C:` + `bar` => `C:bar`), but a bare
                    // volume gets a separator (`foo` + `bar` =>
                    // `foo\bar`).
                    let vol_has_boundary = vol.ends_with(':')
                        || vol.ends_with('/')
                        || vol.ends_with('\\');
                    let path_has_boundary =
                        path_part.starts_with('/') || path_part.starts_with('\\');
                    if dir_nonempty && !vol_has_boundary && !path_has_boundary {
                        format!("{}\\{}", vol, path_part)
                    } else {
                        format!("{}{}", vol, path_part)
                    }
                }
            } else {
                path_part
            };
            return Ok(Value::str(result));
        }
        let path_part = if file.is_empty() {
            if dir.is_empty() { String::new() } else { dir }
        } else if dir.is_empty() || dir == "." {
            file
        } else if dir == "/" && file == "/" {
            "/".to_string()
        } else if dir.ends_with('/') {
            format!("{}{}", dir, file)
        } else {
            format!("{}/{}", dir, file)
        };
        let result = if is_cygwin && !vol.is_empty() {
            format!("{}{}", vol, path_part)
        } else {
            path_part
        };
        Ok(Value::str(result))
    }

    /// `IO::Spec::*.splitdir($path)`: the directory's segments as a list.
    // Cost: O(n), n = path length.
    pub(crate) fn io_spec_splitdir(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if path.is_empty() {
            return Ok(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                    Value::str_from(""),
                ])),
                crate::value::ArrayKind::List,
            ));
        }
        let parts: Vec<Value> = if is_win32 {
            path.split(['/', '\\'])
                .map(|s| Value::str(s.to_string()))
                .collect()
        } else {
            path.split('/').map(|s| Value::str(s.to_string())).collect()
        };
        Ok(Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(parts)),
            crate::value::ArrayKind::List,
        ))
    }

    /// `IO::Spec::*.catpath($volume, $dir, $file)`: the path they make.
    // Cost: O(n), n = total input length.
    pub(crate) fn io_spec_catpath(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        let is_cygwin = kind == SpecKind::Cygwin;
        let vol = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let dir = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
        let file = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
        let sep = if is_win32 { '\\' } else { '/' };
        let mut result = dir;
        if !file.is_empty() {
            if !result.is_empty()
                && !result.ends_with('/')
                && !result.ends_with('\\')
            {
                result.push(sep);
            }
            result.push_str(&file);
        }
        if (is_cygwin || is_win32) && !vol.is_empty() {
            result = format!("{}{}", vol, result);
        }
        Ok(Value::str(result))
    }
}
