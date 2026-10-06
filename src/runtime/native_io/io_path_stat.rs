use super::*;
use crate::value::AttrMap;

impl Interpreter {
    /// The pieces `.absolute` and `.relative` read of an `IO::Path`: its path,
    /// its own `cwd` attribute, the path resolved against the cwd (the
    /// instance's, `$*CWD`, or the process's, with any chroot applied) and the
    /// current working directory. Purely lexical, no filesystem access.
    fn io_path_cwd_frame(&self, attributes: &AttrMap) -> (String, Option<String>, PathBuf, PathBuf) {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let instance_cwd = attributes.get("cwd").map(|v| v.to_string_value());
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        (p, instance_cwd, path_buf, self.get_cwd_path())
    }

    /// `IO::Path.absolute`: the path made absolute against `$base` (the
    /// instance's `CWD` by default, which is `$*CWD` unless it was given one),
    /// as the receiver's SPEC writes it. Depends on the cwd, which the
    /// interpreter owns, and not on the filesystem.
    // Cost: O(p), p = chars of the path.
    pub(crate) fn io_path_absolute(
        &self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let (p, instance_cwd, path_buf, cwd_path) = self.io_path_cwd_frame(attributes);
        let original = Path::new(&p);
        if Self::is_win32_spec(attributes) {
            let base = Self::positional_value(args, 0)
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
            let base = Self::positional_value(args, 0)
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
            let base = Self::positional_value(args, 0).map(|v| v.to_string_value());
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

    /// `IO::Path.relative`: the path relative to `$base` (`$*CWD` by default),
    /// as the receiver's SPEC writes it. Depends on the cwd, which the
    /// interpreter owns, and not on the filesystem.
    // Cost: O(p + b), p = chars of the path, b = chars of the base.
    pub(crate) fn io_path_relative(
        &self,
        attributes: &AttrMap,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let (p, instance_cwd, path_buf, cwd_path) = self.io_path_cwd_frame(attributes);
        if Self::is_win32_spec(attributes) {
            let base = Self::positional_value(args, 0)
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
            let base = Self::positional_value(args, 0)
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
            // Compute a path relative to `base` (default `$*CWD`),
            // matching raku's `$*SPEC.abs2rel`: make both the receiver
            // and the base absolute, drop the common leading prefix, and
            // prepend a `..` for each remaining base component. A plain
            // `strip_prefix` only works when the base is a literal
            // ancestor of the path — for a sibling/relative base it must
            // walk up with `..` (raku returns e.g. `../A/x`), and the old
            // fall-through to the absolute path corrupted zef's extract
            // paths (it uses `$archive.relative($tmp)` to build `-C`).
            // `.relative`'s default base is `$*CWD` (`cwd_path`), NOT the
            // receiver's own `.CWD` attribute — unlike `.absolute`, which
            // defaults to `$.CWD`. So `.resolve` (which stamps `:CWD("/")`)
            // followed by no-arg `.relative` still relativizes against the
            // process cwd: `"foo/bar".IO.resolve.relative` is `foo/bar`, and
            // `IO::Path.new("b/c", :CWD("/a")).relative` is `../../..a/b/c`
            // relative to `$*CWD`, not `b/c`. The receiver's `.CWD` is only
            // used to make the *target* path absolute (`path_buf`, above).
            let base_buf = match Self::positional_value(args, 0).map(|v| v.to_string_value()) {
                Some(base) => self.resolve_path(&base),
                None => cwd_path.clone(),
            };
            let rel = Self::lexical_abs2rel(&path_buf, &base_buf);
            Ok(Value::str(rel))
        }
    }

    /// `IO::Path.CWD`: the directory the path was made relative to, the
    /// instance's own `cwd` attribute or else the current working directory.
    // Cost: O(1).
    pub(crate) fn io_path_cwd_of(&self, attributes: &AttrMap) -> String {
        attributes
            .get("cwd")
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| Self::stringify_path(&self.get_cwd_path()))
    }

    /// `IO::Path.raku`: the `.new` call that rebuilds the path, with its `:CWD`.
    /// A plain `IO::Path` renders its `:SPEC` explicitly (`IO::Spec::Unix` on
    /// POSIX); a SPEC-variant subclass (`IO::Path::Win32`) omits it, as its
    /// class already implies the spec, matching Rakudo. The instance's actual
    /// class names the call, not one derived from the `SPEC` attribute, so
    /// `is-deeply $p.raku.EVAL, $p` round-trips (the class is part of an
    /// instance's equality).
    // Cost: O(p), p = chars of the path.
    pub(crate) fn io_path_raku(&self, class_name: &str, attributes: &AttrMap) -> String {
        let escape = |s: &str| {
            s.replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace('\n', "\\n")
                .replace('\t', "\\t")
                .replace('\r', "\\r")
                .replace('\0', "\\0")
        };
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let spec = if class_name == "IO::Path" {
            ":SPEC(IO::Spec::Unix), "
        } else {
            ""
        };
        format!(
            "{}.new(\"{}\", {}:CWD(\"{}\"))",
            class_name,
            escape(&p),
            spec,
            escape(&self.io_path_cwd_of(attributes))
        )
    }

    /// Lexically compute the path of `target` relative to `base`, the core of
    /// `IO::Path.relative` (raku's `$*SPEC.abs2rel`). Both are assumed absolute.
    /// Purely lexical and the filesystem is never consulted (matching raku, which
    /// does not resolve symlinks here). `.` segments are dropped but `..` is kept
    /// as a literal component and compared verbatim — exactly what raku's
    /// `abs2rel` does (it splits the two absolute strings and never collapses
    /// `..`). Returns `.` when the two paths are equal.
    pub(crate) fn lexical_abs2rel(target: &Path, base: &Path) -> String {
        use std::path::Component;
        // Component-name vector. The leading `/` (RootDir) is kept as a sentinel
        // "" so two absolute paths always share it as a common prefix element.
        // `Path::components()` already drops `.`; `..` stays as a `ParentDir`.
        fn norm(p: &Path) -> Vec<String> {
            let mut out: Vec<String> = Vec::new();
            for c in p.components() {
                match c {
                    Component::RootDir => out.push(String::new()),
                    Component::CurDir => {}
                    Component::ParentDir => out.push("..".to_string()),
                    Component::Normal(s) => out.push(s.to_string_lossy().to_string()),
                    Component::Prefix(_) => {}
                }
            }
            out
        }
        let t = norm(target);
        let b = norm(base);
        let mut i = 0;
        while i < t.len() && i < b.len() && t[i] == b[i] {
            i += 1;
        }
        let mut parts: Vec<String> = Vec::new();
        parts.extend((i..b.len()).map(|_| "..".to_string()));
        parts.extend(t[i..].iter().cloned());
        if parts.is_empty() {
            ".".to_string()
        } else {
            parts.join("/")
        }
    }

    /// The answer of one `stat`-only `IO::Path` method: the `e`/`f`/`d`/`l`/
    /// `r`/`w`/`x`/`rw`/`rwx`/`z` file tests and the `mode`/`inode`/`dev`/
    /// `devtype`/`s`/`created`/`modified`/`accessed`/`changed` readers, named by
    /// `kind` (the method's own name; the `-e $path` file-test operators share
    /// [`io_file_test`]). The receiver's path is
    /// resolved against the cwd and the filesystem is read via `stat` only: no
    /// `io_handles` allocation, no output, no content read. A missing path is a
    /// `Failure`, as in Rakudo.
    // Cost: O(p) plus one filesystem query, p = path length.
    pub(crate) fn io_path_stat(&self, attributes: &AttrMap, kind: &str) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        Self::io_path_stat_result(&path_buf, kind)
    }

    /// Resolve an `IO::Path`'s `path` attribute to an absolute filesystem
    /// `PathBuf` against the VM-owned cwd (`$*CWD` / the instance `cwd` attribute /
    /// the process cwd), applying any chroot. Purely lexical (no filesystem
    /// access) — shared by the `&self` native IO::Path methods
    /// (`io_path_stat` / `io_path_content_read`) and `native_io_path`.
    pub(crate) fn resolve_io_path_buf(&self, attributes: &AttrMap, p: &str) -> PathBuf {
        let instance_cwd = attributes.get("cwd").map(|v| v.to_string_value());
        if Path::new(p).is_absolute() {
            self.resolve_path(p)
        } else if let Some(cwd) = &instance_cwd {
            self.apply_chroot(PathBuf::from(cwd).join(Path::new(p)))
        } else {
            self.resolve_path(p)
        }
    }

    /// Pure `stat`-based result for the [`Self::io_path_stat`] methods given an
    /// already-resolved `path_buf`, which also names the path in a Failure's
    /// message. Factored out so both the VM-native path and `native_io_path`
    /// run the exact same filesystem queries and Failure shaping.
    fn io_path_stat_result(path_buf: &Path, method: &str) -> Result<Value, RuntimeError> {
        // A missing path is reported by its absolute form, as Rakudo's
        // `X::IO::DoesNotExist` does (`fs_errors` module docs).
        let abs = path_buf.to_string_lossy();
        let p = abs.as_ref();
        match method {
            "e" => Ok(Value::truth(path_buf.exists())),
            "f" | "d" | "l" | "r" | "w" | "x" | "rw" | "rwx" => {
                match super::helpers::io_file_test(path_buf, method) {
                    Some(answer) => Ok(Value::truth(answer)),
                    None => Ok(io_path_missing_failure(p, method)),
                }
            }
            "z" => match fs::metadata(path_buf) {
                Ok(meta) => Ok(Value::truth(meta.len() == 0)),
                Err(_) => Ok(io_path_missing_failure(p, "z")),
            },
            // `.mode` returns an `IntStr` allomorph (`.Int` = the octal mode value,
            // `.Str` = the zero-padded octal string) and fails with
            // `X::IO::DoesNotExist` on a missing path (roast S32-io/file-tests.t).
            "mode" => match fs::metadata(path_buf) {
                Ok(metadata) => {
                    #[cfg(unix)]
                    let (mode, s) = {
                        let m = metadata.permissions().mode() & 0o777;
                        (m as i64, format!("{:04o}", m))
                    };
                    #[cfg(not(unix))]
                    let (mode, s) = if metadata.permissions().readonly() {
                        (0o444, "0444".to_string())
                    } else {
                        (0o666, "0666".to_string())
                    };
                    Self::build_native_allomorph_value("IntStr", &[Value::int(mode), Value::str(s)])
                }
                Err(_) => Ok(io_path_missing_failure(p, "mode")),
            },
            "inode" | "dev" | "devtype" => match fs::metadata(path_buf) {
                Ok(meta) => {
                    #[cfg(unix)]
                    {
                        use std::os::unix::fs::MetadataExt;
                        let number = match method {
                            "inode" => meta.ino(),
                            "dev" => meta.dev(),
                            _ => meta.rdev(),
                        };
                        Ok(i64::try_from(number)
                            .map(Value::int)
                            .unwrap_or_else(|_| Value::bigint(number.into())))
                    }
                    #[cfg(not(unix))]
                    {
                        let _ = meta;
                        Ok(Value::NIL)
                    }
                }
                Err(_) => Ok(io_path_missing_failure(p, method)),
            },
            "s" => match fs::metadata(path_buf) {
                Ok(meta) => Ok(Value::int(meta.len() as i64)),
                // `.s` on a missing path fails with `X::IO::DoesNotExist` (a Failure),
                // not a thrown generic error (roast S32-io/file-tests.t).
                Err(_) => Ok(io_path_missing_failure(p, "s")),
            },
            // `.created`/`.modified`/`.accessed`/`.changed` return an `Instant` in
            // Raku, and fail with `X::IO::DoesNotExist` (a Failure) on a missing
            // path — not a plain Int / generic error (roast S32-io/file-tests.t).
            "created" | "accessed" | "modified" | "changed" => {
                let code = match method {
                    "created" => 5,
                    "accessed" => 6,
                    "modified" => 7,
                    _ => 8,
                };
                match crate::runtime::nqp_stat::stat_time(path_buf, code, false) {
                    Ok(t) => Ok(Value::make_instant_from_posix(t)),
                    Err(_) => Ok(io_path_missing_failure(p, method)),
                }
            }
            _ => unreachable!("io_path_stat_result called with non-stat method"),
        }
    }
}
