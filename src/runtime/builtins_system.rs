use super::*;

/// Options extracted from named arguments for run/shell.
struct ProcOptions {
    cwd: Option<String>,
    env: HashMap<String, String>,
    capture_err: bool,
    capture_out: bool,
    win_verbatim_args: bool,
}

impl Interpreter {
    fn missing_symbol_name_from_failure(value: &Value) -> Option<String> {
        let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
        else {
            return None;
        };
        if class_name != "Failure" {
            return None;
        }
        let exception = attributes.get("exception")?;
        let Value::Instance {
            attributes: ex_attrs,
            ..
        } = exception
        else {
            return None;
        };
        let message = ex_attrs.get("message")?.to_string_value();
        let prefix = "No such symbol '";
        let rest = message.strip_prefix(prefix)?;
        let symbol = rest.strip_suffix('\'')?;
        if symbol.is_empty() {
            None
        } else {
            Some(symbol.to_string())
        }
    }

    fn require_target_is_path_like(target: &str) -> bool {
        target.ends_with(".rakumod")
            || target.ends_with(".pm6")
            || target.contains('/')
            || target.contains('\\')
    }

    fn require_guess_module_name_from_path(path: &str) -> Option<String> {
        let p = Path::new(path);
        let stem = p.file_stem()?.to_string_lossy().to_string();
        if stem.is_empty() { None } else { Some(stem) }
    }

    fn resolve_require_file_path(&self, file: &str) -> Option<PathBuf> {
        let direct = PathBuf::from(file);
        if direct.exists() {
            return Some(direct);
        }
        for base in &self.lib_paths {
            let candidate = Path::new(base).join(file);
            if candidate.exists() {
                return Some(candidate);
            }
            let candidate = Path::new(base).join("lib").join(file);
            if candidate.exists() {
                return Some(candidate);
            }
        }
        if let Some(cwd) = self.get_dynamic_string("$*CWD") {
            let candidate = Path::new(&cwd).join(file);
            if candidate.exists() {
                return Some(candidate);
            }
        }
        None
    }

    fn require_load_from_file(
        &mut self,
        file: &str,
        package_hint: Option<&str>,
    ) -> Result<(), RuntimeError> {
        let path = self
            .resolve_require_file_path(file)
            .ok_or_else(|| RuntimeError::new(format!("Module not found: {}", file)))?;
        let code = std::fs::read_to_string(&path)
            .map_err(|e| RuntimeError::new(format!("Failed to read module {}: {}", file, e)))?;
        let preprocessed = Self::preprocess_roast_directives(&code);
        crate::parser::set_parser_lib_paths(self.lib_paths.clone());
        crate::parser::set_parser_program_path(self.program_path.clone());
        let result = parse_dispatch::parse_source(&preprocessed);
        crate::parser::clear_parser_lib_paths();
        for warning in crate::parser::take_parse_warnings() {
            self.write_warn_to_stderr(&warning);
        }
        let stmts = result.map(|(stmts, _)| stmts).map_err(|mut err| {
            err.message = format!("Failed to parse module '{}': {}", file, err.message);
            err
        })?;
        let stmts = Self::merge_unit_class(stmts);
        let saved_package = self.current_package.clone();
        let before_function_keys: std::collections::HashSet<String> =
            self.functions.keys().cloned().collect();
        let before_env_keys: std::collections::HashSet<String> = self.env.keys().cloned().collect();
        let before_class_keys: std::collections::HashSet<String> =
            self.classes.keys().cloned().collect();
        if let Some(pkg) = package_hint
            && !pkg.is_empty()
        {
            self.current_package = pkg.to_string();
        }
        let run_result = self.run_block(&stmts);
        self.current_package = saved_package;
        run_result?;

        if let Some(pkg) = package_hint
            && !pkg.is_empty()
        {
            let mut fn_aliases: Vec<(String, FunctionDef)> = Vec::new();
            for (name, def) in &self.functions {
                if before_function_keys.contains(name) {
                    continue;
                }
                let tail = name.rsplit_once("::").map(|(_, t)| t).unwrap_or(name);
                if tail.contains('/') || tail.contains(':') {
                    continue;
                }
                let alias = format!("{pkg}::{tail}");
                if !self.functions.contains_key(&alias) {
                    fn_aliases.push((alias, def.clone()));
                }
            }
            for (alias, def) in fn_aliases {
                self.functions.insert(alias, def);
            }

            let mut env_aliases: Vec<(String, Value)> = Vec::new();
            for (name, value) in &self.env {
                if before_env_keys.contains(name) {
                    continue;
                }
                if name.starts_with('$') || name.starts_with('@') || name.starts_with('%') {
                    continue;
                }
                let tail = name.rsplit_once("::").map(|(_, t)| t).unwrap_or(name);
                let alias = format!("{pkg}::{tail}");
                if !self.env.contains_key(&alias) {
                    env_aliases.push((alias, value.clone()));
                }
            }
            for (alias, value) in env_aliases {
                self.env.insert(alias, value);
            }

            let mut class_aliases: Vec<(String, ClassDef)> = Vec::new();
            for (name, class_def) in &self.classes {
                if before_class_keys.contains(name) {
                    continue;
                }
                let tail = name.rsplit_once("::").map(|(_, t)| t).unwrap_or(name);
                let alias = format!("{pkg}::{tail}");
                if !self.classes.contains_key(&alias) {
                    class_aliases.push((alias, class_def.clone()));
                }
            }
            for (alias, class_def) in class_aliases {
                self.classes.insert(alias, class_def);
            }
        }

        Ok(())
    }

    fn import_single_require_symbol(&mut self, module: &str, symbol: &str) -> bool {
        if symbol.is_empty() {
            return true;
        }
        if module == "Test" {
            return true;
        }
        if let Some(name) = symbol.strip_prefix('&') {
            let source_single = format!("{module}::{name}");
            let source_prefix = format!("{module}::{name}/");
            let target_single = format!("GLOBAL::{name}");
            let target_prefix = format!("GLOBAL::{name}/");
            let mut found = false;
            let function_entries: Vec<(String, FunctionDef)> = self
                .functions
                .iter()
                .filter_map(|(k, v)| {
                    if k == &source_single {
                        found = true;
                        Some((target_single.clone(), v.clone()))
                    } else if k.starts_with(&source_prefix) {
                        found = true;
                        Some((k.replacen(&source_prefix, &target_prefix, 1), v.clone()))
                    } else {
                        None
                    }
                })
                .collect();
            for (k, v) in function_entries {
                self.functions.insert(k, v);
            }
            return found || self.functions.contains_key(&format!("GLOBAL::{name}"));
        }

        if let Some(sigil) = symbol.chars().next()
            && matches!(sigil, '$' | '@' | '%' | '&')
        {
            let bare = &symbol[1..];
            let candidates = [
                format!("{sigil}{module}::{bare}"),
                format!("{sigil}{bare}"),
                format!("{module}::{bare}"),
                bare.to_string(),
            ];
            for source in candidates {
                if let Some(value) = self.env.get(&source).cloned() {
                    self.env.insert(symbol.to_string(), value);
                    return true;
                }
            }
            return false;
        }

        let source_single = format!("{module}::{symbol}");
        if self.has_class(&source_single) || self.is_role(&source_single) {
            self.env
                .insert(symbol.to_string(), Value::Package(source_single.clone()));
            self.env.insert(
                format!("__mutsu_sigilless_readonly::{symbol}"),
                Value::Bool(true),
            );
            return true;
        }
        if self.has_class(symbol) || self.is_role(symbol) {
            self.env
                .insert(symbol.to_string(), Value::Package(symbol.to_string()));
            self.env.insert(
                format!("__mutsu_sigilless_readonly::{symbol}"),
                Value::Bool(true),
            );
            return true;
        }
        if let Some(value) = self.env.get(&source_single).cloned() {
            if self.functions.contains_key(&source_single) && matches!(value, Value::Int(_)) {
                return false;
            }
            self.env.insert(symbol.to_string(), value);
            self.env.insert(
                format!("__mutsu_sigilless_readonly::{symbol}"),
                Value::Bool(true),
            );
            return true;
        }
        if let Some(value) = self.env.get(symbol).cloned() {
            let is_nil = matches!(value, Value::Nil);
            self.env.insert(symbol.to_string(), value);
            self.env.insert(
                format!("__mutsu_sigilless_readonly::{symbol}"),
                Value::Bool(true),
            );
            return !is_nil;
        }
        false
    }

    fn require_import_symbols(
        &mut self,
        module: &str,
        imports: &[String],
    ) -> Result<(), RuntimeError> {
        let mut missing = Vec::new();
        for symbol in imports {
            if !self.import_single_require_symbol(module, symbol) {
                missing.push(symbol.clone());
            }
        }
        if missing.is_empty() {
            Ok(())
        } else {
            Err(RuntimeError::new(format!(
                "X::Import::MissingSymbols: {}",
                missing.join(" ")
            )))
        }
    }

    pub(super) fn builtin_require(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut module_value: Option<Value> = None;
        let mut file_target: Option<String> = None;
        let mut imports: Vec<String> = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "__mutsu_require_file" => {
                    file_target = Some(value.to_string_value());
                }
                Value::Array(items, ..) => {
                    imports.extend(items.iter().map(|v| v.to_string_value()));
                }
                other => {
                    if module_value.is_none() {
                        module_value = Some(other.clone());
                    } else {
                        imports.push(other.to_string_value());
                    }
                }
            }
        }
        let module_value =
            module_value.ok_or_else(|| RuntimeError::new("require expects a module"))?;
        let module_string = match &module_value {
            Value::Package(name) => name.clone(),
            Value::Str(name) => name.clone(),
            value @ Value::Instance { .. } => Self::missing_symbol_name_from_failure(value)
                .unwrap_or_else(|| value.to_string_value()),
            other => other.to_string_value(),
        };
        let return_is_str = matches!(module_value, Value::Str(_));
        let mut module_name = if module_string.is_empty() {
            None
        } else {
            Some(module_string.clone())
        };

        let load_from_file = file_target.is_some()
            || module_name
                .as_ref()
                .is_some_and(|m| Self::require_target_is_path_like(m));
        if load_from_file {
            let file = file_target
                .clone()
                .or_else(|| module_name.clone())
                .ok_or_else(|| RuntimeError::new("require expects a file"))?;
            if file_target.is_none() {
                module_name = Self::require_guess_module_name_from_path(&file);
            }
            self.require_load_from_file(&file, module_name.as_deref())?;
            if let Some(module) = module_name.as_ref() {
                self.loaded_modules.insert(module.clone());
            }
        } else if let Some(module) = module_name.as_ref() {
            self.use_module(module)?;
        } else {
            return Err(RuntimeError::new("require expects a module name"));
        }

        let in_method_context = !self.method_class_stack.is_empty();
        let should_install_stub = !return_is_str || !in_method_context;
        if let Some(module) = module_name.as_ref()
            && should_install_stub
        {
            self.env
                .insert(module.clone(), Value::Package(module.clone()));
            if !imports.is_empty() {
                self.require_import_symbols(module, &imports)?;
            }
        } else if let Some(module) = module_name.as_ref()
            && !imports.is_empty()
        {
            self.require_import_symbols(module, &imports)?;
        }

        if return_is_str {
            Ok(Value::Str(module_string))
        } else {
            let name = module_name.unwrap_or(module_string);
            Ok(Value::Package(name))
        }
    }

    pub(super) fn builtin_gethost(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let host_str = args.first().map(|v| v.to_string_value());
        let hostname = host_str.unwrap_or_else(Self::hostname);
        let addrs = Self::resolve_host(&hostname);
        Ok(Self::make_os_name_value(hostname, addrs))
    }

    pub(super) fn builtin_chroot(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let path_str = args
            .first()
            .map(|v| v.to_string_value())
            .or_else(|| {
                self.get_dynamic_string("$*CWD")
                    .or_else(|| self.get_dynamic_string("$*CHROOT"))
            })
            .unwrap_or_else(|| ".".to_string());
        let path_buf = PathBuf::from(path_str.clone());
        if !path_buf.is_dir() {
            return Ok(Value::Bool(false));
        }
        let canonical = path_buf.canonicalize().unwrap_or_else(|_| path_buf.clone());
        if std::env::set_current_dir(&canonical).is_err() {
            return Ok(Value::Bool(false));
        }
        self.chroot_root = Some(canonical.clone());
        let repr = Self::stringify_path(&canonical);
        self.env
            .insert("$*CHROOT".to_string(), Value::Str(repr.clone()));
        self.env
            .insert("*CHROOT".to_string(), Value::Str(repr.clone()));
        let cwd_val = self.make_io_path_instance(&repr);
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val);
        Ok(Value::Bool(true))
    }

    /// Helper to create a Proc instance from process execution results.
    fn make_proc_instance(
        exitcode: i64,
        signal: i64,
        pid: i64,
        command: Value,
        captured_err: Option<String>,
        captured_out: Option<String>,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("exitcode".to_string(), Value::Int(exitcode));
        attrs.insert("signal".to_string(), Value::Int(signal));
        attrs.insert("pid".to_string(), Value::Int(pid));
        attrs.insert("command".to_string(), command);
        if let Some(err_content) = captured_err {
            attrs.insert("err".to_string(), Self::make_io_pipe(err_content));
        }
        if let Some(out_content) = captured_out {
            attrs.insert("out".to_string(), Self::make_io_pipe(out_content));
        }
        Value::make_instance("Proc".to_string(), attrs)
    }

    /// Create an IO::Pipe instance wrapping captured content.
    fn make_io_pipe(content: String) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("content".to_string(), Value::Str(content));
        Value::make_instance("IO::Pipe".to_string(), attrs)
    }

    /// Extract named options (cwd, env, :err, :out) from arguments.
    fn extract_proc_options(args: &[Value], skip: usize) -> ProcOptions {
        let mut opts = ProcOptions {
            cwd: None,
            env: HashMap::new(),
            capture_err: false,
            capture_out: false,
            win_verbatim_args: false,
        };
        for value in args.iter().skip(skip) {
            if let Value::Hash(map) = value {
                for (key, inner) in map.iter() {
                    match key.as_str() {
                        "cwd" => {
                            opts.cwd = Some(inner.to_string_value());
                        }
                        "env" => {
                            if let Value::Hash(env_map) = inner {
                                for (ek, ev) in env_map.iter() {
                                    opts.env.insert(ek.clone(), ev.to_string_value());
                                }
                            }
                        }
                        "err" => {
                            opts.capture_err = inner.truthy();
                        }
                        "out" => {
                            opts.capture_out = inner.truthy();
                        }
                        "win-verbatim-args" => {
                            opts.win_verbatim_args = inner.truthy();
                        }
                        _ => {}
                    }
                }
            }
            if let Value::Pair(key, inner) = value {
                match key.as_str() {
                    "cwd" => opts.cwd = Some(inner.to_string_value()),
                    "err" => opts.capture_err = inner.truthy(),
                    "out" => opts.capture_out = inner.truthy(),
                    "win-verbatim-args" => opts.win_verbatim_args = inner.truthy(),
                    _ => {}
                }
            }
        }
        opts
    }

    #[cfg(windows)]
    fn apply_run_args(cmd: &mut Command, args: &[String], win_verbatim_args: bool) {
        use std::os::windows::process::CommandExt;

        if win_verbatim_args {
            if !args.is_empty() {
                // Raku's :win-verbatim-args forwards argv payload without Win32 quoting.
                cmd.raw_arg(args.join(" "));
            }
        } else {
            cmd.args(args);
        }
    }

    #[cfg(not(windows))]
    fn apply_run_args(cmd: &mut Command, args: &[String], _win_verbatim_args: bool) {
        cmd.args(args);
    }

    pub(super) fn builtin_run(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Self::make_proc_instance(
                -1,
                0,
                0,
                Value::Str(String::new()),
                None,
                None,
            ));
        }

        // Collect positional string args and named options
        let mut positional: Vec<String> = Vec::new();
        for arg in args.iter() {
            match arg {
                Value::Hash(_) | Value::Pair(_, _) => {}
                _ => {
                    positional.push(arg.to_string_value());
                }
            }
        }

        let opts = Self::extract_proc_options(args, 0);

        if positional.is_empty() {
            return Ok(Self::make_proc_instance(
                -1,
                0,
                0,
                Value::Str(String::new()),
                None,
                None,
            ));
        }

        let program = &positional[0];
        let rest_args = &positional[1..];

        // Build the command tuple for .command attribute
        let command_val = Value::array(positional.iter().map(|s| Value::Str(s.clone())).collect());

        let mut cmd = Command::new(program);
        Self::apply_run_args(&mut cmd, rest_args, opts.win_verbatim_args);

        if opts.capture_out {
            cmd.stdout(std::process::Stdio::piped());
        } else {
            cmd.stdout(std::process::Stdio::null());
        }
        if opts.capture_err {
            cmd.stderr(std::process::Stdio::piped());
        } else {
            cmd.stderr(std::process::Stdio::null());
        }

        if let Some(cwd) = opts.cwd {
            cmd.current_dir(cwd);
        }
        for (k, v) in opts.env {
            cmd.env(k, v);
        }

        match cmd.spawn() {
            Ok(mut child) => {
                let pid = child.id() as i64;
                let captured_out = if opts.capture_out {
                    child.stdout.take().map(|mut s| {
                        let mut buf = String::new();
                        use std::io::Read;
                        let _ = s.read_to_string(&mut buf);
                        buf
                    })
                } else {
                    None
                };
                let captured_err = if opts.capture_err {
                    child.stderr.take().map(|mut s| {
                        let mut buf = String::new();
                        use std::io::Read;
                        let _ = s.read_to_string(&mut buf);
                        buf
                    })
                } else {
                    None
                };
                match child.wait() {
                    Ok(status) => {
                        let exitcode = status.code().unwrap_or(-1) as i64;
                        #[cfg(unix)]
                        let signal = {
                            use std::os::unix::process::ExitStatusExt;
                            status.signal().unwrap_or(0) as i64
                        };
                        #[cfg(not(unix))]
                        let signal = 0i64;
                        Ok(Self::make_proc_instance(
                            exitcode,
                            signal,
                            pid,
                            command_val,
                            captured_err,
                            captured_out,
                        ))
                    }
                    Err(_) => Ok(Self::make_proc_instance(
                        -1,
                        0,
                        pid,
                        command_val,
                        captured_err,
                        captured_out,
                    )),
                }
            }
            Err(_) => Ok(Self::make_proc_instance(-1, 0, 0, command_val, None, None)),
        }
    }

    pub(super) fn builtin_shell(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let command_str = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if command_str.is_empty() {
            return Ok(Self::make_proc_instance(
                -1,
                0,
                0,
                Value::Str(String::new()),
                None,
                None,
            ));
        }

        let opts = Self::extract_proc_options(args, 1);

        let command_val = Value::Str(command_str.clone());

        let mut command = if cfg!(windows) {
            let mut cmd = Command::new("cmd");
            cmd.arg("/C").arg(&command_str);
            cmd
        } else {
            let mut cmd = Command::new("sh");
            cmd.arg("-c").arg(&command_str);
            cmd
        };

        if opts.capture_out {
            command.stdout(std::process::Stdio::piped());
        } else {
            command.stdout(std::process::Stdio::null());
        }
        if opts.capture_err {
            command.stderr(std::process::Stdio::piped());
        } else {
            command.stderr(std::process::Stdio::null());
        }

        if let Some(cwd) = opts.cwd {
            command.current_dir(cwd);
        }
        for (k, v) in opts.env {
            command.env(k, v);
        }

        match command.spawn() {
            Ok(mut child) => {
                let pid = child.id() as i64;
                let captured_out = if opts.capture_out {
                    child.stdout.take().map(|mut s| {
                        let mut buf = String::new();
                        use std::io::Read;
                        let _ = s.read_to_string(&mut buf);
                        buf
                    })
                } else {
                    None
                };
                let captured_err = if opts.capture_err {
                    child.stderr.take().map(|mut s| {
                        let mut buf = String::new();
                        use std::io::Read;
                        let _ = s.read_to_string(&mut buf);
                        buf
                    })
                } else {
                    None
                };
                match child.wait() {
                    Ok(status) => {
                        let exitcode = status.code().unwrap_or(-1) as i64;
                        #[cfg(unix)]
                        let signal = {
                            use std::os::unix::process::ExitStatusExt;
                            status.signal().unwrap_or(0) as i64
                        };
                        #[cfg(not(unix))]
                        let signal = 0i64;
                        Ok(Self::make_proc_instance(
                            exitcode,
                            signal,
                            pid,
                            command_val,
                            captured_err,
                            captured_out,
                        ))
                    }
                    Err(_) => Ok(Self::make_proc_instance(
                        -1,
                        0,
                        pid,
                        command_val,
                        captured_err,
                        captured_out,
                    )),
                }
            }
            Err(_) => Ok(Self::make_proc_instance(-1, 0, 0, command_val, None, None)),
        }
    }

    pub(super) fn builtin_qx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let command = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();

        let shell_args = vec![
            Value::Str(command),
            Value::Pair("out".to_string(), Box::new(Value::Bool(true))),
        ];
        let proc_value = self.builtin_shell(&shell_args)?;

        if let Value::Instance { attributes, .. } = proc_value
            && let Some(out_pipe) = attributes.get("out")
        {
            return self.call_method_with_values(out_pipe.clone(), "slurp", vec![]);
        }

        Ok(Value::Str(String::new()))
    }

    pub(super) fn builtin_kill(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let signal = args.first().map(super::to_int).unwrap_or(15);
        let mut success = true;
        let mut had_pid = false;
        for val in args.iter().skip(1) {
            had_pid = true;
            let pid = super::to_int(val);
            success &= Self::send_signal(pid, signal);
        }
        if !had_pid {
            success = false;
        }
        Ok(Value::Bool(success))
    }

    pub(super) fn builtin_syscall(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let Some(val) = args.first() else {
            return Ok(Value::Nil);
        };
        let num = super::to_int(val);
        if num == 0 {
            let pid = self
                .env
                .get("*PID")
                .and_then(|v| match v {
                    Value::Int(i) => Some(*i),
                    _ => None,
                })
                .unwrap_or_else(|| {
                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        std::process::id() as i64
                    }
                    #[cfg(target_arch = "wasm32")]
                    {
                        0
                    }
                });
            return Ok(Value::Int(pid));
        }
        Ok(Value::Int(-1))
    }

    pub(super) fn builtin_sleep(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let duration = Self::duration_from_seconds(Self::seconds_from_value(args.first().cloned()));
        thread::sleep(duration);
        self.sync_shared_vars_to_env();
        Ok(Value::Nil)
    }

    pub(super) fn builtin_sleep_timer(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let duration = Self::duration_from_seconds(Self::seconds_from_value(args.first().cloned()));
        let start = Instant::now();
        thread::sleep(duration);
        let elapsed = start.elapsed();
        let remaining = duration.checked_sub(elapsed).unwrap_or_default();
        Ok(Value::Num(remaining.as_secs_f64()))
    }

    pub(super) fn builtin_sleep_till(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(target_time) = Self::system_time_from_value(args.first().cloned()) {
            let now = std::time::SystemTime::UNIX_EPOCH
                + std::time::Duration::from_secs_f64(crate::value::current_time_secs_f64());
            if target_time <= now {
                return Ok(Value::Bool(false));
            }
            if let Ok(diff) = target_time.duration_since(now) {
                thread::sleep(diff);
                return Ok(Value::Bool(true));
            }
        }
        Ok(Value::Bool(false))
    }

    /// `start { ... }` — spawn a thread to execute the block and return a Promise.
    pub(super) fn builtin_start(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        use crate::value::SharedPromise;

        let block = args.into_iter().next().unwrap_or(Value::Nil);
        let promise = SharedPromise::new();
        let ret = Value::Promise(promise.clone());
        let mut thread_interp = self.clone_for_thread();

        std::thread::spawn(
            move || match thread_interp.call_sub_value(block, vec![], false) {
                Ok(result) => {
                    let output = std::mem::take(&mut thread_interp.output);
                    let stderr = std::mem::take(&mut thread_interp.stderr_output);
                    promise.keep(result, output, stderr);
                }
                Err(e) => {
                    let output = std::mem::take(&mut thread_interp.output);
                    let stderr = std::mem::take(&mut thread_interp.stderr_output);
                    let error_val = if let Some(ex) = e.exception {
                        *ex
                    } else {
                        Value::Str(e.message)
                    };
                    promise.break_with(error_val, output, stderr);
                }
            },
        );

        Ok(ret)
    }

    /// `await` — block until Promise(s) resolve, then return their results.
    pub(super) fn builtin_await(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new(
                "Must specify a Promise or list of Promises to await",
            ));
        }
        let mut await_targets: Vec<Value> = Vec::new();
        for arg in args {
            match arg {
                Value::Array(items, _) | Value::Seq(items) | Value::Slip(items) => {
                    await_targets.extend(items.iter().cloned());
                }
                other => await_targets.push(other.clone()),
            }
        }
        let mut results = Vec::new();
        for arg in &await_targets {
            match arg {
                Value::Promise(shared) => {
                    let (result, output, stderr) = shared.wait();
                    self.emit_output(&output);
                    self.stderr_output.push_str(&stderr);
                    if shared.status() == "Broken" {
                        self.sync_shared_vars_to_env();
                        let msg = result.to_string_value();
                        let mut err = RuntimeError::new(msg);
                        // Preserve exception object if it's already an exception instance
                        match &result {
                            Value::Instance { class_name, .. }
                                if class_name.starts_with("X::")
                                    || class_name == "Exception"
                                    || class_name.ends_with("Exception") =>
                            {
                                err.exception = Some(Box::new(result));
                            }
                            _ => {
                                // Wrap in X::AdHoc
                                let mut attrs = std::collections::HashMap::new();
                                attrs.insert(
                                    "payload".to_string(),
                                    Value::Str(result.to_string_value()),
                                );
                                attrs.insert(
                                    "message".to_string(),
                                    Value::Str(result.to_string_value()),
                                );
                                let ex = Value::make_instance("X::AdHoc".to_string(), attrs);
                                err.exception = Some(Box::new(ex));
                            }
                        }
                        return Err(err);
                    }
                    // Replay deferred Proc::Async taps
                    if let Value::Instance {
                        ref class_name,
                        ref attributes,
                        ..
                    } = result
                        && class_name == "Proc"
                    {
                        self.replay_proc_taps(attributes);
                    }
                    results.push(result);
                }
                // Backward compat: Instance-based Promise
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Promise" => {
                    let result = attributes.get("result").cloned().unwrap_or(Value::Nil);
                    results.push(result);
                }
                Value::Array(elems, ..) => {
                    for elem in elems.iter() {
                        match elem {
                            Value::Promise(shared) => {
                                let (result, output, stderr) = shared.wait();
                                self.emit_output(&output);
                                self.stderr_output.push_str(&stderr);
                                if shared.status() == "Broken" {
                                    self.sync_shared_vars_to_env();
                                    let msg = result.to_string_value();
                                    return Err(RuntimeError::new(msg));
                                }
                                results.push(result);
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if class_name == "Promise" => {
                                let result =
                                    attributes.get("result").cloned().unwrap_or(Value::Nil);
                                results.push(result);
                            }
                            _ => results.push(elem.clone()),
                        }
                    }
                }
                _ => results.push(arg.clone()),
            }
        }
        // Sync shared variables back from child threads
        self.sync_shared_vars_to_env();

        if results.len() == 1 {
            Ok(results.into_iter().next().unwrap())
        } else {
            Ok(Value::array(results))
        }
    }
    /// `signal(SIGINT, ...)` — returns a Supply that emits Signal enum values
    /// when the process receives the specified OS signals.
    pub(super) fn builtin_signal(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        use std::sync::mpsc;

        // Extract signal numbers and their enum representations
        let signals: Vec<(i64, Value)> = args
            .iter()
            .filter_map(|v| match v {
                Value::Enum { value, .. } => Some((*value, v.clone())),
                Value::Int(i) => Some((*i, v.clone())),
                _ => None,
            })
            .collect();

        let supply_id = super::native_methods::next_supply_id();

        // Create channel for the Supply
        let (tx, rx) = mpsc::channel();

        // Register the channel in the supply channel map
        if let Ok(mut map) = super::native_methods::supply_channel_map_pub().lock() {
            map.insert(supply_id, rx);
        }

        // Set up real signal handling using pipe + sigaction
        for (signum, sig_val) in &signals {
            signal_watcher::register_signal(*signum as i32, supply_id, tx.clone(), sig_val.clone());
        }

        let mut attrs = std::collections::HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("supply_id".to_string(), Value::Int(supply_id as i64));
        Ok(Value::make_instance("Supply".to_string(), attrs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_proc_options_reads_win_verbatim_pair() {
        let args = vec![Value::Pair(
            "win-verbatim-args".to_string(),
            Box::new(Value::Bool(true)),
        )];
        let opts = Interpreter::extract_proc_options(&args, 0);
        assert!(opts.win_verbatim_args);
    }

    #[test]
    fn extract_proc_options_reads_win_verbatim_hash() {
        let mut map = HashMap::new();
        map.insert("win-verbatim-args".to_string(), Value::Bool(true));
        let args = vec![Value::Hash(map.into())];
        let opts = Interpreter::extract_proc_options(&args, 0);
        assert!(opts.win_verbatim_args);
    }
}
