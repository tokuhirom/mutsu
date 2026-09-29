use super::Interpreter;

impl Interpreter {
    /// Resolve a relative IO::Spec path against an absolute base. Both spec
    /// variants use the process cwd when their caller supplies a relative base.
    // Cost: O(p + b + c), p = path length, b = base length, c = cwd length.
    pub(super) fn io_spec_rel2abs(
        path: &str,
        base: &str,
        cwd: &str,
        win32: bool,
        cygwin: bool,
    ) -> String {
        if win32 {
            let absolute_base = if Self::win32_path_is_absolute(base) {
                Self::canonpath_win32(base, false)
            } else {
                Self::canonpath_win32(&format!("{cwd}\\{base}"), false)
            };
            let (path_vol, path_rest) = Self::split_win32_volume_normalized(path);
            if !path_vol.is_empty() && (path_rest.starts_with('/') || path_rest.starts_with('\\')) {
                return Self::canonpath_win32(path, false);
            }
            if path.starts_with('/') || path.starts_with('\\') {
                let (base_vol, _) = Self::split_win32_volume_normalized(&absolute_base);
                return Self::canonpath_win32(&format!("{base_vol}{path}"), false);
            }
            Self::canonpath_win32(&format!("{absolute_base}\\{path}"), false)
        } else if path.starts_with('/') {
            if cygwin {
                Self::canonpath_cygwin(path, false)
            } else {
                Self::canonpath_unix(path, false)
            }
        } else {
            let joined_base = if base.starts_with('/') {
                base.to_string()
            } else {
                format!("{cwd}/{base}")
            };
            if cygwin {
                let absolute_base = Self::canonpath_cygwin(&joined_base, false);
                Self::canonpath_cygwin(&format!("{absolute_base}/{path}"), false)
            } else {
                let absolute_base = Self::canonpath_unix(&joined_base, false);
                Self::canonpath_unix(&format!("{absolute_base}/{path}"), false)
            }
        }
    }

    // Cost: O(n), n = path length.
    fn win32_path_is_absolute(path: &str) -> bool {
        let bytes = path.as_bytes();
        path.starts_with('/')
            || path.starts_with('\\')
            || (bytes.len() >= 3
                && bytes[0].is_ascii_alphabetic()
                && bytes[1] == b':'
                && (bytes[2] == b'/' || bytes[2] == b'\\'))
    }
}
