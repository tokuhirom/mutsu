//! `IO::Spec::*`'s path primitives (predicates, constants, concatenation and relativizing), one function per method, shared by the
//! method table's rows (ADR-11276 §9.19). A function reads its receiver's class as
//! a [`SpecKind`], so `IO::Spec::Win32.join` and `IO::Spec::Unix.join` are one
//! implementation.

use super::io_spec_kind::SpecKind;
use super::*;

impl Interpreter {

    /// `IO::Spec::*.is-absolute($path)`: whether the path is absolute under the class's rules.
    // Cost: O(p), p = chars of the path.
    pub(crate) fn io_spec_is_absolute(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        let is_cygwin = kind == SpecKind::Cygwin;
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let abs = if is_win32 {
            let bytes = path.as_bytes();
            let drive_abs = bytes.len() >= 3
                && bytes[0].is_ascii_alphabetic()
                && bytes[1] == b':'
                && (bytes[2] == b'\\' || bytes[2] == b'/');
            let unc = bytes.len() >= 2
                && ((bytes[0] == b'\\' && bytes[1] == b'\\')
                    || (bytes[0] == b'/' && bytes[1] == b'/'));
            let leading = path.starts_with('/') || path.starts_with('\\');
            drive_abs || unc || leading
        } else if is_cygwin {
            let bytes = path.as_bytes();
            path.starts_with('/')
                || (bytes.len() >= 3
                    && bytes[0].is_ascii_alphabetic()
                    && bytes[1] == b':'
                    && (bytes[2] == b'\\' || bytes[2] == b'/'))
        } else {
            path.starts_with('/')
        };
        Ok(Value::truth(abs))
    }

    /// `IO::Spec::*.dir-sep`: the directory separator.
    // Cost: O(1).
    pub(crate) fn io_spec_dir_sep(kind: SpecKind) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        Ok(Value::str_from(if is_win32 { "\\" } else { "/" }))
    }

    /// `IO::Spec::*.devnull`: the null device.
    // Cost: O(1).
    pub(crate) fn io_spec_devnull(kind: SpecKind) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        Ok(Value::str_from(if is_win32 { "nul" } else { "/dev/null" }))
    }

    /// `IO::Spec::*.rootdir`: the root directory.
    // Cost: O(1).
    pub(crate) fn io_spec_rootdir(kind: SpecKind) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        Ok(Value::str_from(if is_win32 { "\\" } else { "/" }))
    }

    /// `IO::Spec::*.catdir(*@parts)`: the directory path made of the parts.
    // Cost: O(n), n = total chars of the parts.
    pub(crate) fn io_spec_catdir(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        // catdir/catfile are slurpy (`*@parts`): a passed list
        // (`$*SPEC.catdir(<a b>)`) flattens into its elements.
        let mut flat = Vec::new();
        crate::runtime::types::flatten_into_slurpy(args, &mut flat);
        let parts: Vec<String> = flat.iter().map(|a| a.to_string_value()).collect();
        if parts.is_empty() {
            return Ok(Value::str_from(""));
        }
        if is_win32 {
            return Ok(Value::str(Self::win32_catdir(&parts)));
        }
        let mut joined = parts.join("/");
        joined.push('/');
        let result = Self::canonpath_unix(&joined, false);
        Ok(Value::str(result))
    }

    /// `IO::Spec::*.catfile(*@parts)`: the file path made of the parts.
    // Cost: O(n), n = total chars of the parts.
    pub(crate) fn io_spec_catfile(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        let mut flat = Vec::new();
        crate::runtime::types::flatten_into_slurpy(args, &mut flat);
        let parts: Vec<String> = flat.iter().map(|a| a.to_string_value()).collect();
        if is_win32 {
            return Ok(Value::str(Self::win32_catfile(&parts)));
        }
        let joined = parts.join("/");
        let result = Self::canonpath_unix(&joined, false);
        Ok(Value::str(result))
    }

    /// `IO::Spec::*.curupdir`: the `IO::Spec::CurUpDir` test object that matches `.` and `..`.
    // Cost: O(1).
    pub(crate) fn io_spec_curupdir() -> Result<Value, RuntimeError> {
        Ok(Value::make_instance(
            crate::symbol::Symbol::intern("IO::Spec::CurUpDir"),
            std::collections::HashMap::new(),
        ))
    }

    /// `IO::Spec::*.abs2rel($path, $base)`: the path relative to the base (the current directory by default).
    // Cost: O(n), n = path and base length.
    pub(crate) fn io_spec_abs2rel(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        let path_str = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let base_str =
            args.get(1).map(|v| v.to_string_value()).unwrap_or_else(|| {
                std::env::current_dir()
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or_else(|_| ".".to_string())
            });
        if is_win32 {
            let path_canon = Self::canonpath_win32(&path_str, false);
            let base_canon = Self::canonpath_win32(&base_str, false);
            let (path_vol, path_rest) =
                Self::split_win32_volume_normalized(&path_canon);
            let (base_vol, base_rest) =
                Self::split_win32_volume_normalized(&base_canon);
            if path_vol != base_vol
                && ((!path_vol.is_empty()
                    && !base_vol.is_empty()
                    && path_vol.to_uppercase() != base_vol.to_uppercase())
                    || (path_vol.is_empty() != base_vol.is_empty())
                    || (path_vol.starts_with("\\\\")
                        != base_vol.starts_with("\\\\")))
            {
                return Ok(Value::str(path_canon));
            }
            let path_parts: Vec<&str> = path_rest
                .split(['/', '\\'])
                .filter(|s| !s.is_empty())
                .collect();
            let base_parts: Vec<&str> = base_rest
                .split(['/', '\\'])
                .filter(|s| !s.is_empty())
                .collect();
            let mut common = 0;
            while common < path_parts.len()
                && common < base_parts.len()
                && path_parts[common].eq_ignore_ascii_case(base_parts[common])
            {
                common += 1;
            }
            let ups = base_parts.len() - common;
            let mut result_parts: Vec<&str> = vec![".."; ups];
            result_parts.extend_from_slice(&path_parts[common..]);
            let result = if result_parts.is_empty() {
                ".".to_string()
            } else {
                result_parts.join("\\")
            };
            return Ok(Value::str(result));
        }
        let path = Self::canonpath_unix(&path_str, false);
        let base = Self::canonpath_unix(&base_str, false);
        let path_parts: Vec<&str> =
            path.split('/').filter(|s| !s.is_empty()).collect();
        let base_parts: Vec<&str> =
            base.split('/').filter(|s| !s.is_empty()).collect();
        let mut common = 0;
        while common < path_parts.len()
            && common < base_parts.len()
            && path_parts[common] == base_parts[common]
        {
            common += 1;
        }
        let ups = base_parts.len() - common;
        let mut result_parts: Vec<&str> = vec![".."; ups];
        result_parts.extend_from_slice(&path_parts[common..]);
        let result = if result_parts.is_empty() {
            ".".to_string()
        } else {
            result_parts.join("/")
        };
        Ok(Value::str(result))
    }

    /// `IO::Spec::*.rel2abs($path, $base)`: the path made absolute against the base (the current directory by default).
    // Cost: O(n), n = path, base and cwd length.
    pub(crate) fn io_spec_rel2abs(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        let is_cygwin = kind == SpecKind::Cygwin;
        let path_str = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let cwd = std::env::current_dir()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|_| ".".to_string());
        let base = args
            .get(1)
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| cwd.clone());
        Ok(Value::str(Self::io_spec_make_absolute(
            &path_str, &base, &cwd, is_win32, is_cygwin,
        )))
    }

    /// `IO::Spec::*.basename($path)`: the last segment of the path.
    // Cost: O(n), n = path length.
    pub(crate) fn io_spec_basename(kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let is_win32 = kind == SpecKind::Win32;
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let result = if is_win32 {
            if let Some(pos) = path.rfind(['/', '\\']) {
                &path[pos + 1..]
            } else {
                path.as_str()
            }
        } else if let Some(pos) = path.rfind('/') {
            &path[pos + 1..]
        } else {
            path.as_str()
        };
        Ok(Value::str(result.to_string()))
    }

    /// `IO::Spec::*.extension($path)`: everything after the last `.` of the path.
    // Cost: O(n), n = path length.
    pub(crate) fn io_spec_extension(_kind: SpecKind, args: &[Value]) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        // Extension is everything after the last '.' in the full path
        let result = if let Some(pos) = path.rfind('.') {
            &path[pos + 1..]
        } else {
            ""
        };
        Ok(Value::str(result.to_string()))
    }

    /// `IO::Spec::*.path`: the directories of `%*ENV<PATH>` (`Path` as well on
    /// Win32, whose entries are split on `;` and unquoted, with `.` first).
    // Cost: O(n), n = chars of the variable.
    pub(crate) fn io_spec_path(&self, kind: SpecKind) -> Value {
        if kind == SpecKind::Win32 {
            let path_env = self
                .env_hash_var("PATH")
                .or_else(|| self.env_hash_var("Path"));
            return Value::seq(Self::win32_path_from_env(path_env));
        }
        let path_env = self.env_hash_var("PATH").unwrap_or_default();
        if path_env.is_empty() {
            return Value::seq(Vec::new());
        }
        let parts: Vec<Value> = path_env
            .split(':')
            .map(|p| {
                if p.is_empty() {
                    Value::str_from(".")
                } else {
                    Value::str(p.to_string())
                }
            })
            .collect();
        Value::seq(parts)
    }

    /// `IO::Spec::*.tmpdir`: the temporary directory, as an `IO::Path`.
    // Cost: O(1).
    pub(crate) fn io_spec_tmpdir(&self) -> Value {
        #[cfg(not(target_arch = "wasm32"))]
        let tmpdir_str = std::env::temp_dir().to_string_lossy().to_string();
        #[cfg(target_arch = "wasm32")]
        let tmpdir_str = "/tmp".to_string();
        self.make_io_path_instance(&tmpdir_str)
    }
}
