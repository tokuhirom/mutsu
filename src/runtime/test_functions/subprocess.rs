use super::super::*;

impl Interpreter {
    pub(crate) fn test_fn_is_run(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "is_run expects code")?;
        let program = match program_val {
            Value::Str(s) => s.to_string(),
            // Str type object = no code (e.g., is_run Str, :args['--help'])
            Value::Package(name) if name == "Str" => String::new(),
            Value::Nil => String::new(),
            _ => return Err(RuntimeError::new("is_run expects string code")),
        };
        let (input, expectations_idx, desc_idx) =
            if matches!(Self::positional_value(args, 1), Some(Value::Hash(_))) {
                (String::new(), 1usize, 2usize)
            } else {
                (
                    Self::positional_value(args, 1)
                        .map(|v| v.to_string_value())
                        .unwrap_or_default(),
                    2usize,
                    3usize,
                )
            };
        let expectations =
            Self::positional_value_required(args, expectations_idx, "is_run expects expectations")?
                .clone();
        let desc = Self::positional_string(args, desc_idx);
        let mut expected_out: Option<Value> = None;
        let mut expected_err: Option<Value> = None;
        let mut expected_status: Option<Value> = None;
        let mut run_args: Option<Vec<Value>> = None;
        if let Value::Hash(expected_hash) = &expectations {
            for (name, value) in expected_hash.iter() {
                match name.as_str() {
                    "out" => expected_out = Some(value.clone()),
                    "err" => expected_err = Some(value.clone()),
                    "status" => expected_status = Some(value.clone()),
                    _ => {}
                }
            }
        }
        if let Some(Value::Array(items, ..)) = Self::named_value(args, "args") {
            run_args = Some(items.to_vec());
        }
        // Check for :compiler-args
        let compiler_args: Vec<String> =
            if let Some(Value::Array(items, ..)) = Self::named_value(args, "compiler-args") {
                items.iter().map(|v| v.to_string_value()).collect()
            } else {
                Vec::new()
            };
        let is_doc_mode = compiler_args.iter().any(|a| a == "--doc");
        let mut has_unsupported_compiler_args = false;
        let mut ci = 0usize;
        while ci < compiler_args.len() {
            let arg = &compiler_args[ci];
            if arg == "--doc" {
                ci += 1;
                continue;
            }
            if arg == "-I" {
                if ci + 1 >= compiler_args.len() {
                    has_unsupported_compiler_args = true;
                    break;
                }
                ci += 2;
                continue;
            }
            if arg.starts_with("-I") {
                if arg.len() == 2 {
                    has_unsupported_compiler_args = true;
                    break;
                }
                ci += 1;
                continue;
            }
            has_unsupported_compiler_args = true;
            break;
        }

        // Determine if we need to spawn a real subprocess
        // (e.g., for --help, when program is empty with CLI args,
        //  or when the code uses Supply.interval/threads that could call
        //  std::process::exit on die)
        let code_needs_subprocess = program.contains("Supply.interval")
            || program.contains("Supply.interval:")
            || (expected_err.is_some() && Self::program_mentions_qx(&program))
            || (program.contains("start") && program.contains("exit"));
        let needs_subprocess = has_unsupported_compiler_args
            || (program.is_empty() && run_args.is_some())
            || code_needs_subprocess;

        let (out, err, status) = if is_doc_mode {
            match crate::doc_mode::run_doc_mode(&program) {
                Ok(result) => (result.output, String::new(), result.status),
                Err(err) => (String::new(), err.message, 1i64),
            }
        } else if !input.is_empty() {
            let run_args = run_args
                .unwrap_or_default()
                .into_iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>();
            Self::run_test_code_subprocess(&program, &input, &run_args, &compiler_args)
        } else if needs_subprocess {
            // Spawn actual mutsu binary for CLI flag tests
            self.is_run_subprocess(&program, &run_args, &compiler_args)
        } else {
            let mut nested = Interpreter::new();
            nested.nested_mode = true;
            if let Some(Value::Int(pid)) = self.env.get("*PID") {
                nested.set_pid(pid.saturating_add(1));
            }
            // Apply supported compiler args in in-process mode.
            // `-I <path>` must behave like CLI include paths for module loading.
            let mut i = 0usize;
            while i < compiler_args.len() {
                if compiler_args[i] == "-I" {
                    if let Some(path) = compiler_args.get(i + 1)
                        && !path.is_empty()
                    {
                        nested.add_lib_path(path.clone());
                    }
                    i += 2;
                    continue;
                }
                if let Some(path) = compiler_args[i].strip_prefix("-I")
                    && !path.is_empty()
                {
                    nested.add_lib_path(path.to_string());
                }
                i += 1;
            }
            if let Some(items) = run_args {
                nested.set_args(items);
            }
            nested.program_path = self.program_path.clone();
            let result = nested.run(&program);
            nested.flush_all_handles();
            Self::extract_run_output(&nested, result)
        };
        let mut ok = true;
        if let Some(expected) = expected_out {
            ok &= self.smart_match(&Value::str(out), &expected);
        }
        if let Some(expected) = expected_err {
            ok &= self.smart_match(&Value::str(err), &expected);
        }
        if let Some(expected) = expected_status {
            ok &= self.smart_match(&Value::Int(status), &expected);
        }
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    pub(crate) fn test_fn_get_out(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "get_out expects code")?;
        let program = match program_val {
            Value::Str(s) => s.to_string(),
            _ => return Err(RuntimeError::new("get_out expects string code")),
        };
        let input = Self::positional_value(args, 1)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let run_args: Vec<String> = match Self::named_value(args, "args") {
            Some(Value::Array(items, ..)) | Some(Value::Seq(items)) | Some(Value::Slip(items)) => {
                items.iter().map(|v| v.to_string_value()).collect()
            }
            _ => Vec::new(),
        };
        let compiler_args: Vec<String> = match Self::named_value(args, "compiler-args") {
            Some(Value::Array(items, ..)) | Some(Value::Seq(items)) | Some(Value::Slip(items)) => {
                items.iter().map(|v| v.to_string_value()).collect()
            }
            _ => Vec::new(),
        };
        let (out, err, status) =
            Self::run_test_code_subprocess(&program, &input, &run_args, &compiler_args);
        let mut hash = std::collections::HashMap::new();
        hash.insert("out".to_string(), Value::str(out));
        hash.insert("err".to_string(), Value::str(err));
        hash.insert("status".to_string(), Value::Int(status));
        Ok(Value::Hash(std::sync::Arc::new(hash)))
    }

    pub(crate) fn test_fn_run(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let got = self.test_fn_get_out(args)?;
        let Value::Hash(map) = got else {
            return Ok(Value::str(String::new()));
        };
        if let Some(Value::Str(err)) = map.get("err")
            && !err.is_empty()
        {
            self.emit_output(&format!("# error: {}\n", err.trim_end()));
        }
        if let Some(Value::Str(out)) = map.get("out") {
            return Ok(Value::Str(out.clone()));
        }
        Ok(Value::str(String::new()))
    }

    pub(crate) fn run_test_code_subprocess(
        program: &str,
        input: &str,
        run_args: &[String],
        compiler_args: &[String],
    ) -> (String, String, i64) {
        use std::io::Write;
        use std::process::{Command, Stdio};
        use std::time::{SystemTime, UNIX_EPOCH};

        let exe = std::env::current_exe()
            .unwrap_or_else(|_| std::path::PathBuf::from("target/debug/mutsu"));
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let temp_code_path = std::env::temp_dir().join(format!(
            "mutsu-test-util-{}-{}.raku",
            std::process::id(),
            stamp
        ));
        if std::fs::write(&temp_code_path, program).is_err() {
            return (
                String::new(),
                "Failed to create temporary source file".to_string(),
                1,
            );
        }
        let mut cmd = Command::new(&exe);
        // Split compiler args by whitespace to match raku's Test::Util behavior
        // which joins them with spaces in a shell command.
        for arg in compiler_args {
            for part in arg.split_whitespace() {
                cmd.arg(part);
            }
        }
        cmd.arg(temp_code_path.as_os_str());
        for arg in run_args {
            cmd.arg(arg);
        }
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        if !input.is_empty() {
            cmd.stdin(Stdio::piped());
        }
        let mut child = match cmd.spawn() {
            Ok(child) => child,
            Err(e) => return (String::new(), format!("Failed to run subprocess: {}", e), 1),
        };
        if !input.is_empty()
            && let Some(mut stdin) = child.stdin.take()
        {
            let _ = stdin.write_all(input.as_bytes());
            let _ = stdin.flush();
            drop(stdin);
        }
        let output = child.wait_with_output();
        let _ = std::fs::remove_file(&temp_code_path);
        match output {
            Ok(output) => {
                let out = String::from_utf8_lossy(&output.stdout).to_string();
                let err = String::from_utf8_lossy(&output.stderr).to_string();
                let status = output.status.code().unwrap_or(1) as i64;
                (out, err, status)
            }
            Err(e) => (
                String::new(),
                format!("Failed to read subprocess output: {}", e),
                1,
            ),
        }
    }

    pub(crate) fn is_run_subprocess(
        &self,
        program: &str,
        run_args: &Option<Vec<Value>>,
        compiler_args: &[String],
    ) -> (String, String, i64) {
        use std::time::{SystemTime, UNIX_EPOCH};

        let exe = std::env::current_exe()
            .unwrap_or_else(|_| std::path::PathBuf::from("target/debug/mutsu"));
        let mut cmd = std::process::Command::new(&exe);
        // Split compiler args by whitespace to match raku's Test::Util behavior
        // which joins them with spaces in a shell command.
        let mut compiler_has_e = false;
        for arg in compiler_args {
            for part in arg.split_whitespace() {
                if part == "-e" || part == "-ne" || part == "-pe" {
                    compiler_has_e = true;
                }
                cmd.arg(part);
            }
        }
        if let Some(items) = run_args {
            for item in items {
                cmd.arg(item.to_string_value());
            }
        }
        // When compiler-args contain -e (e.g. '-n -e .say'), write the program
        // to a temp file and pass it as a positional arg (matching raku's
        // Test::Util behavior). Otherwise pass it with -e.
        let temp_code_path = if compiler_has_e && !program.is_empty() {
            let stamp = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0);
            let path = std::env::temp_dir().join(format!(
                "mutsu-isrun-{}-{}.raku",
                std::process::id(),
                stamp
            ));
            if std::fs::write(&path, program).is_err() {
                return (
                    String::new(),
                    "Failed to create temporary source file".to_string(),
                    1,
                );
            }
            cmd.arg(path.as_os_str());
            Some(path)
        } else {
            if !program.is_empty() {
                cmd.arg("-e").arg(program);
            }
            None
        };
        let result = match cmd.output() {
            Ok(output) => {
                let out = String::from_utf8_lossy(&output.stdout).to_string();
                let err = String::from_utf8_lossy(&output.stderr).to_string();
                let status = output.status.code().unwrap_or(1) as i64;
                (out, err, status)
            }
            Err(e) => (String::new(), format!("Failed to run subprocess: {}", e), 1),
        };
        if let Some(path) = temp_code_path {
            let _ = std::fs::remove_file(&path);
        }
        result
    }

    pub(crate) fn extract_run_output(
        nested: &Interpreter,
        result: Result<String, RuntimeError>,
    ) -> (String, String, i64) {
        let stderr_content = nested.stderr_output.clone();
        match result {
            Ok(output) => {
                let s = if nested.bailed_out {
                    255i64
                } else {
                    nested.exit_code
                };
                let (stdout_tap, tap_err) = Self::split_tap_output_streams(&output);
                let mut err = stderr_content;
                err.push_str(&tap_err);
                (stdout_tap, err, s)
            }
            Err(e) => {
                let combined = nested.output.clone();
                let (stdout_only, tap_err) = Self::split_tap_output_streams(&combined);
                let mut err = stderr_content;
                err.push_str(&tap_err);
                if !e.message.is_empty() {
                    if !err.is_empty() && !err.ends_with('\n') {
                        err.push('\n');
                    }
                    err.push_str(&e.message);
                    err.push('\n');
                }
                let s = if nested.exit_code != 0 {
                    nested.exit_code
                } else {
                    1i64
                };
                (stdout_only, err, s)
            }
        }
    }

    pub(crate) fn split_tap_output_streams(output: &str) -> (String, String) {
        let mut stdout = String::new();
        let mut stderr = String::new();
        let mut last_not_ok_todo: Option<bool> = None;

        for line in output.split_inclusive('\n') {
            let trimmed = line.trim_start();
            if trimmed.starts_with("not ok ") {
                last_not_ok_todo = Some(trimmed.contains("# TODO"));
                stdout.push_str(line);
                continue;
            }
            if trimmed.starts_with("ok ") {
                last_not_ok_todo = None;
                stdout.push_str(line);
                continue;
            }
            let is_failure_diag = trimmed.starts_with("# Failed test")
                || trimmed.starts_with("# at ")
                || trimmed.starts_with("# You failed");
            if is_failure_diag && matches!(last_not_ok_todo, Some(false)) {
                stderr.push_str(line);
                continue;
            }
            stdout.push_str(line);
        }

        (stdout, stderr)
    }
}
