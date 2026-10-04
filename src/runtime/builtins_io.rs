use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

/// Check for NUL bytes in a path and return X::IO::Null error if found.
pub(super) fn check_null_in_path(path: &str) -> Result<(), RuntimeError> {
    if path.contains('\0') {
        Err(RuntimeError::new(
            "X::IO::Null: Found null byte in pathname",
        ))
    } else {
        Ok(())
    }
}

pub(super) fn io_exception_error(class_name: &str, message: String) -> RuntimeError {
    // The exception object carries the text too, as `io_exception_failure`'s
    // does: without it `$!.message` on a caught error was empty.
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern(class_name),
        attrs,
    )));
    err
}

/// Create a Failure value wrapping an IO exception.
pub(super) fn io_exception_failure(class_name: &str, message: String) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message));
    let ex = Value::make_instance(Symbol::intern(class_name), attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

#[cfg(unix)]
pub(super) fn has_required_mode_bits(path: &Path, read: bool, write: bool, execute: bool) -> bool {
    use std::os::unix::fs::PermissionsExt;
    // With no mode bits requested the conjunction below is vacuously true, so
    // do not stat at all: a caller that also skipped the existence test
    // (`indir :!d, $nonexistent`) would otherwise be rejected as "permission
    // denied" purely because `metadata` could not find the path.
    if !read && !write && !execute {
        return true;
    }
    let mode = match fs::metadata(path) {
        Ok(meta) => meta.permissions().mode() & 0o777,
        Err(_) => return false,
    };
    (!read || (mode & 0o444) != 0)
        && (!write || (mode & 0o222) != 0)
        && (!execute || (mode & 0o111) != 0)
}

#[cfg(not(unix))]
pub(super) fn has_required_mode_bits(
    _path: &Path,
    _read: bool,
    _write: bool,
    _execute: bool,
) -> bool {
    true
}

pub(super) fn parse_io_requirements(args: &[Value]) -> (bool, bool, bool, bool) {
    let mut require_dir = true;
    let mut require_read = false;
    let mut require_write = false;
    let mut require_exec = false;
    for arg in args {
        if let ValueView::Pair(key, val) = arg.view() {
            match key.as_str() {
                "d" => require_dir = val.truthy(),
                "r" => require_read = val.truthy(),
                "w" => require_write = val.truthy(),
                "x" => require_exec = val.truthy(),
                _ => {}
            }
        }
    }
    (require_dir, require_read, require_write, require_exec)
}

impl Interpreter {
    /// Resolve a `slurp`/`spurt`-family positional path argument to an
    /// absolute filesystem `PathBuf`, plus the display string used in error
    /// messages. An already-constructed `IO::Path` (sub)class instance
    /// carries its own `cwd` attribute, captured from `$*CWD` at `.IO`/
    /// `.new` time (`build_io_path_instance`/`make_io_path_instance`) — it
    /// must be resolved against *that* captured directory, exactly as the
    /// `.slurp`/`.spurt` methods on the instance do via
    /// [`Self::resolve_io_path_buf`]. A plain `Str` (or other stringifiable
    /// scalar) has no such capture, so it resolves against the live virtual
    /// `$*CWD` instead, matching `chdir`/`indir`.
    pub(super) fn resolve_io_arg_path(&self, arg: &Value) -> (PathBuf, String) {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = arg.view()
            && (class_name == "IO::Path"
                || self
                    .class_mro(&class_name.resolve())
                    .iter()
                    .any(|n| n == "IO::Path"))
        {
            let attrs = attributes.as_map();
            let p = attrs
                .get("path")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let path_buf = self.resolve_io_path_buf(&attrs, &p);
            return (path_buf, p);
        }
        let p = arg.to_string_value();
        (self.resolve_path(&p), p)
    }

    pub(super) fn dir_test_matches(
        &mut self,
        test: &Value,
        entry_name: &str,
        dir_path: &Path,
    ) -> bool {
        if let ValueView::Bool(b) = test.view() {
            return b;
        }

        let saved_cwd = self.env.get("$*CWD").cloned();
        let saved_cwd_star = self.env.get("*CWD").cloned();
        let saved_topic = self.env.get("_").cloned();
        let saved_dollar_topic = self.env.get("$_").cloned();

        let cwd_val = self.make_io_path_instance(&Self::stringify_path(dir_path));
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val);
        self.env
            .insert("_".to_string(), Value::str(entry_name.to_string()));
        self.env
            .insert("$_".to_string(), Value::str(entry_name.to_string()));

        let matched = match test.view() {
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => self
                .call_sub_value(test.clone(), vec![Value::str(entry_name.to_string())], true)
                .map(|v| {
                    // A predicate block whose sole/final statement is a bare
                    // regex (`{ /\.html$/ }`, the common `dir(:test)` shape)
                    // returns the `Regex` object itself, not a `Match` --
                    // Raku defers the match to `Regex.Bool`, evaluated
                    // against the topic in effect when the value is
                    // boolified. `Value::truthy()` has no such context and
                    // treats a `Regex` as unconditionally true, so it never
                    // rejected any entry here; smart-match it against the
                    // entry name explicitly instead of boolifying blindly.
                    match v.view() {
                        ValueView::Regex(_) | ValueView::RegexWithAdverbs { .. } => {
                            self.smart_match(&Value::str(entry_name.to_string()), &v)
                        }
                        _ => v.truthy(),
                    }
                })
                .unwrap_or(false),
            _ => self.smart_match(&Value::str(entry_name.to_string()), test),
        };

        if let Some(v) = saved_cwd {
            self.env.insert("$*CWD".to_string(), v);
        } else {
            self.env.remove("$*CWD");
        }
        if let Some(v) = saved_cwd_star {
            self.env.insert("*CWD".to_string(), v);
        } else {
            self.env.remove("*CWD");
        }
        if let Some(v) = saved_topic {
            self.env.insert("_".to_string(), v);
        } else {
            self.env.remove("_");
        }
        if let Some(v) = saved_dollar_topic {
            self.env.insert("$_".to_string(), v);
        } else {
            self.env.remove("$_");
        }

        matched
    }

    /// VM-native dispatch for the file/FS builtin *functions* (`slurp`/`open`/
    /// `unlink`/…). These read or mutate the filesystem and the VM-owned `io_handles`
    /// store; the `builtin_*` impls already own that state, but the only path reaching
    /// them was the generic `call_function` name-match fallback (§D state ownership ③
    /// — IO native methods were already drained; this drains the function forms).
    /// Dispatched after all user-sub resolution (so a user `sub slurp` still wins),
    /// mirroring the `call_function` IO arms 1:1 — same args, same `self`, byte-identical.
    ///
    /// Deliberately excludes `indir` (runs a callback block), `chdir`/`tmpdir`/
    /// `homedir` (process-cwd/env side state), and the output routines
    /// (`print`/`say`/`note`/`warn`/`sink`) — those keep their existing dispatch.
    pub(crate) fn try_native_io_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let r = match name {
            "slurp" => self.builtin_slurp(args),
            "spurt" => self.builtin_spurt(args),
            "unlink" => self.builtin_unlink(args),
            "open" => self.builtin_open(args),
            "close" => self.builtin_close(args),
            "dir" => self.builtin_dir(args),
            "copy" => self.builtin_copy(args),
            "rename" | "move" => self.builtin_rename(name, args),
            "chmod" => self.builtin_chmod(args),
            "mkdir" => self.builtin_mkdir(args),
            "rmdir" => self.builtin_rmdir(args),
            "link" => self.builtin_link(args),
            "symlink" => self.builtin_symlink(args),
            _ => return None,
        };
        Some(r)
    }

    pub(super) fn builtin_slurp(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Check if first arg is a named pair (not a positional path)
        let first_is_pair = args.first().is_none_or(|v| v.is_string_pair_value());
        // If no positional path argument, slurp from $*ARGFILES
        if first_is_pair {
            let argfiles = self.env.get("$*ARGFILES").cloned().unwrap_or(Value::NIL);
            return self.call_method_with_values(argfiles, "slurp", args.to_vec());
        }
        // If first arg is an IO::Handle, delegate to .slurp() method on it
        if let Some(handle) = args.first()
            && let ValueView::Instance { class_name, .. } = handle.view()
            && class_name == "IO::Handle"
        {
            let has_close = args[1..].iter().any(|arg| {
                matches!(arg.view(), ValueView::Pair(name, value) if name == "close" && value.truthy())
            });
            // Filter out :close from args passed to the slurp method
            let remaining: Vec<Value> = args[1..]
                .iter()
                .filter(|arg| !matches!(arg.view(), ValueView::Pair(name, _) if name == "close"))
                .cloned()
                .collect();
            let result = self.call_method_with_values(handle.clone(), "slurp", remaining)?;
            if has_close {
                self.call_method_with_values(handle.clone(), "close", vec![])?;
            }
            return Ok(result);
        }
        let (path_buf, path) = self.resolve_io_arg_path(args.first().unwrap());
        check_null_in_path(&path)?;
        let bin = args
            .iter()
            .skip(1)
            .any(|arg| matches!(arg.view(), ValueView::Pair(name, value) if name == "bin" && value.truthy()));
        let enc = args.iter().skip(1).find_map(|arg| {
            if let ValueView::Pair(name, value) = arg.view()
                && name == "enc"
            {
                return Some(value.to_string_value());
            }
            None
        });
        self.slurp_file(&path_buf, bin, enc.as_deref())
    }

    pub(super) fn builtin_spurt(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // If the first argument is an IO::Handle, delegate to IO::Handle.spurt
        if let Some(inst) = args.first()
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = inst.view()
            && class_name == "IO::Handle"
        {
            let content_value = args.get(1).cloned().unwrap_or(Value::str(String::new()));
            let method_args = std::iter::once(content_value)
                .chain(args.iter().skip(2).cloned())
                .collect();
            return self.native_io_handle(&(attributes).as_map(), "spurt", method_args);
        }
        let Some(path_arg) = args.first() else {
            return Err(RuntimeError::new("spurt requires a path argument"));
        };
        let (resolved, path) = self.resolve_io_arg_path(path_arg);
        check_null_in_path(&path)?;
        // Since Rakudo 2020.12, `spurt $path` with no content creates an
        // empty file (or truncates an existing one) rather than erroring.
        let content_value = args.get(1).cloned().unwrap_or(Value::str(String::new()));
        let content_value = &content_value;
        let mut append = false;
        let mut createonly = false;
        let mut enc: Option<String> = None;
        for arg in args.iter().skip(2) {
            if let ValueView::Pair(key, val) = arg.view() {
                match key.as_str() {
                    "append" => append = val.truthy(),
                    "createonly" => createonly = val.truthy(),
                    "enc" => enc = Some(val.to_string_value()),
                    _ => {}
                }
            }
        }
        Ok(self.spurt_file(&resolved, content_value, append, createonly, enc.as_deref()))
    }

    pub(super) fn builtin_unlink(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Raku's `unlink` sub returns an Array of the paths it successfully
        // removed. A path that did not exist is deemed a success (included); a
        // path whose removal failed for another reason (e.g. it is a directory)
        // is silently dropped from the result, and the sub never throws. (The
        // `.unlink` method form fails softly instead — handled separately.)
        let mut names = Vec::new();
        // `unlink <a b c>` passes a single list argument; flatten so each path
        // is removed individually rather than stringifying the whole list.
        let paths: Vec<Value> = args
            .iter()
            .flat_map(crate::runtime::utils::value_to_list)
            .collect();
        for arg in &paths {
            let path = arg.to_string_value();
            let resolved = self.resolve_path(&path);
            if native_io::fs_syscalls::unlink_file(&resolved).is_ok() {
                names.push(Value::str(path));
            }
        }
        // `unlink` returns an `Array` (not a List), so `say unlink <...>` gists
        // as `[...]` and `.WHAT` is `Array`.
        Ok(Value::real_array(names))
    }

    pub(super) fn builtin_open(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Raku lets a named argument precede a positional one at the call site,
        // so `open :w, $path` and `open $path, :w` produce the same call — but
        // the first spelling puts the `:w` Pair in `args[0]`. The path is
        // therefore the first *non-Pair* argument, not literally `args[0]`, and
        // every Pair is a flag regardless of where it appears.
        let path_arg = args
            .iter()
            .find(|a| !matches!(a.view(), ValueView::Pair(..)));
        let flag_args: Vec<Value> = args
            .iter()
            .filter(|a| !path_arg.is_some_and(|p| std::ptr::eq(*a, p)))
            .cloned()
            .collect();
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
            _create,
            _exclusive,
        ) = self.parse_io_flags_values(&flag_args);

        // IO::Special is a sentinel for an already-open standard stream, not
        // a path named "IO::Special()". Reopen it as a fresh IO::Handle so
        // per-handle options such as :nl-out do not mutate $*OUT/$*ERR/$*IN.
        if let Some(ValueView::Instance {
            class_name,
            attributes,
            ..
        }) = path_arg.map(|v| v.view())
            && class_name == "IO::Special"
            && let Some(what) = attributes.as_map().get("what")
        {
            let what = what.to_string_value();
            let (target, default_mode, target_name) = match what.as_str() {
                "<STDOUT>" | "STDOUT" => (IoHandleTarget::Stdout, IoHandleMode::Write, "STDOUT"),
                "<STDERR>" | "STDERR" => (IoHandleTarget::Stderr, IoHandleMode::Write, "STDERR"),
                "<STDIN>" | "STDIN" => (IoHandleTarget::Stdin, IoHandleMode::Read, "STDIN"),
                _ => (IoHandleTarget::File, IoHandleMode::Read, ""),
            };
            if target != IoHandleTarget::File {
                let mode = if append {
                    IoHandleMode::Append
                } else if read && write {
                    IoHandleMode::ReadWrite
                } else if write {
                    IoHandleMode::Write
                } else if read {
                    // With no explicit mode, retain the stream's natural
                    // direction (stdout/stderr are writable by default).
                    if flag_args.iter().any(|arg| {
                        matches!(arg.view(), ValueView::Pair(name, value) if name == "r" && value.truthy())
                    }) {
                        IoHandleMode::Read
                    } else {
                        default_mode
                    }
                } else {
                    default_mode
                };
                let handle = self.create_handle(target, mode, Some(target_name.to_string()));
                self.with_handle_mut(&handle, |state| {
                    state.line_chomp = line_chomp;
                    state.line_separators = line_separators;
                    state.out_buffer_capacity = out_buffer_capacity;
                    state.nl_out = nl_out.unwrap_or_else(|| "\n".to_string());
                    state.bin = bin;
                    state.encoding = if bin {
                        "bin".to_string()
                    } else {
                        enc.unwrap_or_else(|| "utf-8".to_string())
                    };
                    Ok(())
                })?;
                return Ok(handle);
            }
        }

        // `open` takes an `IO()`-coercible path. When handed an IO::Handle
        // (e.g. `open(IO::Handle.new(:path($p)))`), coerce it to its `.path`
        // so the underlying file is opened, matching rakudo.
        let path = match path_arg {
            Some(v) => match v.view() {
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name.resolve() == "IO::Handle" => attributes
                    .as_map()
                    .get("path")
                    .map(|p| p.to_string_value())
                    .unwrap_or_default(),
                _ => v.to_string_value(),
            },
            None => return Err(RuntimeError::new("open requires a path argument")),
        };
        check_null_in_path(&path)?;
        let create = _create;
        let exclusive = _exclusive;
        let path_buf = self.resolve_path(&path);
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
            // The handle's `.path` is the path as written, as in Rakudo.
            Some(Path::new(&path)),
        ) {
            Ok(handle) => Ok(handle),
            // Raku returns a Failure (wrapping the exception) when open() fails,
            // rather than dying immediately. Sinking/using the Failure later
            // throws the exception. Preserve any specific exception type the
            // error already carries; otherwise default to X::AdHoc.
            Err(err) => Ok(native_io::fs_errors::open_error_failure(err)),
        }
    }

    pub(super) fn builtin_close(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let handle = args
            .first()
            .ok_or_else(|| RuntimeError::new("close requires a handle"))?;
        Ok(Value::truth(self.close_handle_value(handle)?))
    }
}
