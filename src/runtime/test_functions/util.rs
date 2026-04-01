use super::super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// `doesn't-hang` -- run code in a subprocess and verify it completes
    /// within a timeout. Checks stdout and stderr if expected values given.
    pub(crate) fn test_fn_doesnt_hang(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        use std::process::{Command, Stdio};

        let first_arg = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);

        // Determine executable and arguments.
        // Two calling conventions:
        //   1. doesn't-hang("code", $desc, ...)    -- Str first arg
        //   2. doesn't-hang(\($exe, '-e', $code), $desc, ...)  -- Capture first arg
        let (exe, cmd_args) = if let Value::Capture { positional, .. } = &first_arg {
            // Capture form: first element is exe, rest are args
            let exe_val = positional.first().cloned().unwrap_or(Value::Nil);
            let exe_str = exe_val.to_string_value();
            let rest: Vec<String> = positional[1..]
                .iter()
                .map(|v| v.to_string_value())
                .collect();
            (exe_str, rest)
        } else {
            // String form: run as `$exe -e $code`
            let code = first_arg.to_string_value();
            let exe = self
                .env
                .get("*EXECUTABLE")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    Self::resolved_current_executable_path()
                        .to_string_lossy()
                        .to_string()
                });
            (exe, vec!["-e".to_string(), code])
        };

        let desc = Self::positional_string_opt(args, 1)
            .unwrap_or_else(|| "code does not hang".to_string());
        let expected_out = Self::named_value(args, "out");
        let expected_err = Self::named_value(args, "err");
        let wait_secs = Self::named_value(args, "wait")
            .and_then(|v| match v {
                Value::Int(i) => Some(i as u64),
                Value::Num(f) => Some(f as u64),
                _ => None,
            })
            .unwrap_or(15);

        let mut child = Command::new(&exe)
            .args(&cmd_args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| RuntimeError::new(format!("doesn't-hang: spawn failed: {}", e)))?;

        #[cfg(unix)]
        let pid = child.id();
        let timeout = std::time::Duration::from_secs(wait_secs);
        let start = std::time::Instant::now();

        // Wait for the child with timeout
        let did_not_hang = loop {
            match child.try_wait() {
                Ok(Some(_)) => break true,
                Ok(None) => {
                    if start.elapsed() >= timeout {
                        // Kill the hung process
                        #[cfg(unix)]
                        unsafe {
                            libc::kill(pid as i32, libc::SIGKILL);
                        }
                        #[cfg(not(unix))]
                        let _ = child.kill();
                        let _ = child.wait();
                        break false;
                    }
                    std::thread::sleep(std::time::Duration::from_millis(50));
                }
                Err(_) => break false,
            }
        };

        // Collect output
        let stdout_str = child
            .stdout
            .take()
            .map(|out| {
                use std::io::Read;
                let mut s = String::new();
                let mut reader = std::io::BufReader::new(out);
                let _ = reader.read_to_string(&mut s);
                s
            })
            .unwrap_or_default();
        let stderr_str = child
            .stderr
            .take()
            .map(|err| {
                use std::io::Read;
                let mut s = String::new();
                let mut reader = std::io::BufReader::new(err);
                let _ = reader.read_to_string(&mut s);
                s
            })
            .unwrap_or_default();

        // Run as subtest
        let ctx = self.begin_subtest();
        let mut plan_count = 1;
        if did_not_hang {
            if expected_out.is_some() {
                plan_count += 1;
            }
            if expected_err.is_some() {
                plan_count += 1;
            }
        }
        self.test_fn_plan(&[Value::Int(plan_count)])?;
        self.test_ok(did_not_hang, "program did not hang", false)?;
        if did_not_hang {
            if let Some(expected) = expected_out {
                let ok = self.smart_match(&Value::str(stdout_str), &expected);
                self.test_ok(ok, "STDOUT", false)?;
            }
            if let Some(expected) = expected_err {
                let ok = self.smart_match(&Value::str(stderr_str), &expected);
                self.test_ok(ok, "STDERR", false)?;
            }
        }
        self.finish_subtest(ctx, &desc, Ok(()))?;
        Ok(Value::Bool(did_not_hang))
    }

    /// `make-temp-file` -- create a temporary file, optionally with content.
    pub(crate) fn test_fn_make_temp_file(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let content = Self::named_value(args, "content");
        let chmod_val = Self::named_value(args, "chmod");

        let tmp_dir = std::env::temp_dir();
        let rand_name = format!(
            "mutsu_roast_{}_{}",
            std::process::id(),
            crate::runtime::native_methods::next_supply_id()
        );
        let path = tmp_dir.join(rand_name);

        if let Some(c) = content {
            // If content is a Buf/Blob, write raw bytes instead of string representation
            if crate::vm::VM::is_buf_value(&c) {
                let raw_bytes = crate::vm::VM::extract_buf_bytes(&c);
                std::fs::write(&path, &raw_bytes).map_err(|e| {
                    RuntimeError::new(format!("make-temp-file: cannot write: {}", e))
                })?;
            } else {
                let text = c.to_string_value();
                std::fs::write(&path, &text).map_err(|e| {
                    RuntimeError::new(format!("make-temp-file: cannot write: {}", e))
                })?;
            }
        } else if chmod_val.is_some() {
            // :chmod without :content -- create empty file
            std::fs::write(&path, "")
                .map_err(|e| RuntimeError::new(format!("make-temp-file: cannot create: {}", e)))?;
        }
        // When neither :content nor :chmod is given, don't create the file

        if let Some(Value::Int(_mode)) = chmod_val {
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let perms = std::fs::Permissions::from_mode(_mode as u32);
                let _ = std::fs::set_permissions(&path, perms);
            }
        }

        let path_str = path.to_string_lossy().to_string();
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("path".to_string(), Value::str(path_str));
        Ok(Value::make_instance(Symbol::intern("IO::Path"), attrs))
    }

    /// `make-temp-dir` -- create a temporary directory.
    /// Accepts an optional positional `Int $chmod?` or named `:chmod`.
    pub(crate) fn test_fn_make_temp_dir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let chmod_val =
            Self::named_value(args, "chmod").or_else(|| Self::positional_value(args, 0).cloned());

        let tmp_dir = std::env::temp_dir();
        let rand_name = format!(
            "mutsu_roast_dir_{}_{}",
            std::process::id(),
            crate::runtime::native_methods::next_supply_id()
        );
        let path = tmp_dir.join(rand_name);
        std::fs::create_dir_all(&path)
            .map_err(|e| RuntimeError::new(format!("make-temp-dir: cannot create: {}", e)))?;

        if let Some(Value::Int(_mode)) = chmod_val {
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let perms = std::fs::Permissions::from_mode(_mode as u32);
                let _ = std::fs::set_permissions(&path, perms);
            }
        }

        let path_str = path.to_string_lossy().to_string();
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("path".to_string(), Value::str(path_str));
        Ok(Value::make_instance(Symbol::intern("IO::Path"), attrs))
    }

    pub(crate) fn positional_string_opt(args: &[Value], idx: usize) -> Option<String> {
        let mut pos_idx = 0;
        for arg in args {
            match arg {
                Value::Pair(..) | Value::ValuePair(..) => continue,
                _ => {
                    if pos_idx == idx {
                        return Some(arg.to_string_value());
                    }
                    pos_idx += 1;
                }
            }
        }
        None
    }

    /// `is-path` -- compare two IO::Path values after resolving.
    /// Equivalent to `cmp-ok $got.resolve, '~~', $exp.resolve, $desc`.
    pub(crate) fn test_fn_is_path(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let got = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let exp = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);

        // Resolve both paths: call .resolve method if available, otherwise
        // fall back to the string representation.
        let got_resolved = self
            .call_method_with_values(got.clone(), "resolve", vec![])
            .map(|v| v.to_string_value())
            .unwrap_or_else(|_| got.to_string_value());
        let exp_resolved = self
            .call_method_with_values(exp.clone(), "resolve", vec![])
            .map(|v| v.to_string_value())
            .unwrap_or_else(|_| exp.to_string_value());

        let ok = self.smart_match(
            &Value::str(got_resolved.clone()),
            &Value::str(exp_resolved.clone()),
        );
        self.test_ok(ok, &desc, false)?;
        if !ok {
            let diag = format!(
                "# Failed test '{}'\n# expected: {}\n#      got: {}\n",
                desc, exp_resolved, got_resolved
            );
            self.stderr_output.push_str(&diag);
        }
        Ok(Value::Bool(ok))
    }
}
