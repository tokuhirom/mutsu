use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    pub(super) fn init_io_environment(&mut self) {
        let stdout = self.create_handle(
            IoHandleTarget::Stdout,
            IoHandleMode::Write,
            Some("STDOUT".to_string()),
        );
        self.env.insert("$*OUT".to_string(), stdout.clone());
        self.env.insert("*OUT".to_string(), stdout);
        let stderr = self.create_handle(
            IoHandleTarget::Stderr,
            IoHandleMode::Write,
            Some("STDERR".to_string()),
        );
        self.env.insert("$*ERR".to_string(), stderr.clone());
        self.env.insert("*ERR".to_string(), stderr);
        let stdin = self.create_handle(
            IoHandleTarget::Stdin,
            IoHandleMode::Read,
            Some("STDIN".to_string()),
        );
        self.env.insert("$*IN".to_string(), stdin.clone());
        self.env.insert("*IN".to_string(), stdin);
        let argfiles = self.create_handle(
            IoHandleTarget::ArgFiles,
            IoHandleMode::Read,
            Some("$*ARGFILES".to_string()),
        );
        self.env.insert("$*ARGFILES".to_string(), argfiles.clone());
        self.env.insert("*ARGFILES".to_string(), argfiles);
        let spec = self.make_io_spec_instance();
        self.env.insert("$*SPEC".to_string(), spec.clone());
        self.env.insert("*SPEC".to_string(), spec);
        #[cfg(not(target_arch = "wasm32"))]
        let cwd_str = env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .to_string_lossy()
            .to_string();
        #[cfg(target_arch = "wasm32")]
        let cwd_str = "/".to_string();
        let cwd_val = self.make_io_path_instance(&cwd_str);
        self.env.insert("$*CWD".to_string(), cwd_val.clone());
        self.env.insert("*CWD".to_string(), cwd_val);
        #[cfg(not(target_arch = "wasm32"))]
        let tmpdir_str = env::temp_dir().to_string_lossy().to_string();
        #[cfg(target_arch = "wasm32")]
        let tmpdir_str = "/tmp".to_string();
        let tmpdir_val = self.make_io_path_instance(&tmpdir_str);
        self.env.insert("$*TMPDIR".to_string(), tmpdir_val.clone());
        self.env.insert("*TMPDIR".to_string(), tmpdir_val);
        #[cfg(not(target_arch = "wasm32"))]
        let home_val = if let Ok(home) = env::var("HOME") {
            self.make_io_path_instance(&home)
        } else {
            Value::NIL
        };
        #[cfg(target_arch = "wasm32")]
        let home_val = Value::NIL;
        self.env.insert("$*HOME".to_string(), home_val.clone());
        self.env.insert("*HOME".to_string(), home_val);
        // $*EXECUTABLE - path to the interpreter binary
        #[cfg(not(target_arch = "wasm32"))]
        let exe_path = Self::resolved_current_executable_path()
            .to_string_lossy()
            .to_string();
        #[cfg(target_arch = "wasm32")]
        let exe_path = "mutsu".to_string();
        let exe_io = self.make_io_path_instance(&exe_path);
        self.env.insert("$*EXECUTABLE".to_string(), exe_io.clone());
        self.env.insert("*EXECUTABLE".to_string(), exe_io);
        self.env.insert(
            "$*EXECUTABLE-NAME".to_string(),
            Value::str(
                std::path::Path::new(&exe_path)
                    .file_name()
                    .map(|f| f.to_string_lossy().to_string())
                    .unwrap_or_else(|| exe_path.clone()),
            ),
        );
        let exec_name = self.env.get("$*EXECUTABLE-NAME").cloned().unwrap();
        self.env.insert("*EXECUTABLE-NAME".to_string(), exec_name);
        let distro = Self::make_distro_instance();
        self.env.insert("*DISTRO".to_string(), distro.clone());
        self.env.insert("?DISTRO".to_string(), distro);
        let perl = Self::make_perl_instance();
        self.env.insert("*PERL".to_string(), perl.clone());
        self.env.insert("?PERL".to_string(), perl);
        let raku = Self::make_perl_instance();
        self.env.insert("*RAKU".to_string(), raku.clone());
        self.env.insert("?RAKU".to_string(), raku);
        let vm = Self::make_vm_instance();
        self.env.insert("$*VM".to_string(), vm.clone());
        self.env.insert("*VM".to_string(), vm.clone());
        self.env.insert("?VM".to_string(), vm);
        let kernel = Self::make_kernel_instance();
        self.env.insert("*KERNEL".to_string(), kernel.clone());
        self.env.insert("?KERNEL".to_string(), kernel);
    }

    pub(super) fn get_dynamic_handle(&self, name: &str) -> Option<Value> {
        self.env.get(name).cloned().or_else(|| {
            Self::dynamic_name_alias(name).and_then(|alias| self.env.get(&alias).cloned())
        })
    }

    pub(super) fn default_input_handle(&self) -> Option<Value> {
        self.get_dynamic_handle("$*ARGFILES")
            .or_else(|| self.get_dynamic_handle("$*IN"))
    }

    pub(crate) fn write_to_named_handle(
        &mut self,
        name: &str,
        text: &str,
        newline: bool,
    ) -> Result<(), RuntimeError> {
        if let Some(handle) = self.get_dynamic_handle(name) {
            if Self::handle_id_from_value(&handle).is_some() {
                return self.write_to_handle_value(&handle, text, newline);
            }
            let payload = if newline {
                format!("{}\n", text)
            } else {
                text.to_string()
            };
            // A user `$*OUT`/`$*ERR` handle's `print` method can mutate a
            // captured-outer caller lexical (the classic output-capture idiom:
            // `my $out; my $*OUT = class { method print(*@a) { $out ~= @a.join } }`).
            // This internal dispatch has no surrounding `CallMethod` op to drain
            // the writeback, so across successive `say`/`print` calls the earlier
            // mutations were lost (only the last write survived). Reconcile the
            // caller frame afterwards so the accumulation persists (Slice F).
            let caller_code = self.current_code;
            if self
                .call_method_with_values(handle, "print", vec![Value::str(payload.clone())])
                .is_ok()
            {
                self.reconcile_caller_after_internal_dispatch(caller_code);
                return Ok(());
            }
            self.reconcile_caller_after_internal_dispatch(caller_code);
            if name == "$*ERR" {
                self.output_sink_mut().stderr_output.push_str(&payload);
            }
            self.emit_output(&payload);
            return Ok(());
        }
        let payload = if newline {
            format!("{}\n", text)
        } else {
            text.to_string()
        };
        if name == "$*ERR" {
            self.output_sink_mut().stderr_output.push_str(&payload);
        }
        self.emit_output(&payload);
        Ok(())
    }

    /// Stringify a value by calling .gist. A dispatch failure falls back to
    /// the native gist — EXCEPT a `return` control signal (or its typed
    /// X::ControlFlow::Return form) raised while the gist forces a lazy Seq:
    /// `say foo` where foo returned a lazy map whose block does `return` must
    /// die like rakudo, not silently print the fallback gist
    /// (integration/error-reporting.t test 21).
    /// TODO: `render_str_value` (put/print) still swallows these signals.
    pub(crate) fn render_gist_value(&mut self, value: &Value) -> Result<String, RuntimeError> {
        match self.call_method_with_values(value.clone(), "gist", vec![]) {
            Ok(result) => Ok(result.to_string_value()),
            Err(e) if e.return_value.is_some() => Err(RuntimeError::controlflow_return(true)),
            Err(e)
                if e.exception.as_ref().is_some_and(|ex| {
                    matches!(ex.view(), ValueView::Instance { class_name, .. }
                        if class_name.resolve() == "X::ControlFlow::Return")
                }) =>
            {
                Err(e)
            }
            Err(_) => Ok(crate::runtime::gist_value(value)),
        }
    }

    /// Stringify a value by calling .Str method (used by put/print).
    /// Falls back to to_string_value() if .Str method dispatch fails.
    pub(crate) fn render_str_value(&mut self, value: &Value) -> String {
        // Printing a type object stringifies to "" with rakudo's
        // uninitialized-value warning suggesting .^name/.raku/.gist/.say.
        if let ValueView::Package(name) = value.view() {
            let n = name.resolve();
            let msg = format!(
                "Use of uninitialized value of type {} in string context.\nMethods .^name, .raku, .gist, or .say can be used to stringify it to something meaningful.",
                n
            );
            let resumed = self
                .raise_resumable_warning(&msg, Value::str(String::new()))
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            return resumed;
        }
        self.call_method_with_values(value.clone(), "Str", vec![])
            .map(|result| result.to_string_value())
            .unwrap_or_else(|_| value.to_string_value())
    }

    pub(super) fn get_dynamic_string(&self, name: &str) -> Option<String> {
        self.get_dynamic_handle(name)
            .and_then(|value| match value.view() {
                ValueView::Str(s) => Some(s.to_string()),
                ValueView::Instance { attributes, .. } => {
                    // Support IO::Path instances (e.g., $*CWD)
                    attributes.as_map().get("path").map(|v| v.to_string_value())
                }
                _ => None,
            })
    }

    pub(super) fn get_cwd_path(&self) -> PathBuf {
        if let Some(cwd) = self.get_dynamic_string("$*CWD") {
            return PathBuf::from(cwd);
        }
        env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
    }

    pub(super) fn resolve_path(&self, path: &str) -> PathBuf {
        let pb = PathBuf::from(path);
        if pb.is_absolute() {
            self.apply_chroot(pb)
        } else {
            let cwd = self.get_cwd_path();
            self.apply_chroot(cwd.join(pb))
        }
    }

    pub(super) fn apply_chroot(&self, path: PathBuf) -> PathBuf {
        if let Some(root) = &self.chroot_root {
            if path.starts_with(root) {
                return path;
            }
            if path.is_absolute() {
                if let Ok(stripped_root) = path.strip_prefix(root) {
                    return root.join(stripped_root);
                }
                if let Ok(stripped_slash) = path.strip_prefix("/") {
                    return root.join(stripped_slash);
                }
                return root.join(path);
            }
        }
        path
    }

    pub(super) fn stringify_path(path: &Path) -> String {
        path.to_string_lossy().to_string()
    }

    pub(crate) fn make_io_path_instance(&self, path: &str) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("path".to_string(), Value::str(path.to_string()));
        // Inherit $*SPEC if set (check both env lookup styles)
        let spec = self
            .env
            .get("$*SPEC")
            .or_else(|| self.env.get("*SPEC"))
            .cloned()
            .or_else(|| self.get_dynamic_var("*SPEC").ok());
        if let Some(spec) = spec
            && !spec.is_nil()
        {
            attrs.insert("SPEC".to_string(), spec);
        }
        // Set CWD from $*CWD if available
        if let Some(cwd) = self.get_dynamic_string("$*CWD") {
            attrs.insert("cwd".to_string(), Value::str(cwd));
        }
        Value::make_instance(Symbol::intern("IO::Path"), attrs)
    }
}
