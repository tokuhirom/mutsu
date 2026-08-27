use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;
use std::sync::OnceLock;

impl Interpreter {
    /// Rebuild the per-interpreter IO/dynamic-var environment. Called from
    /// `Interpreter::new()`; every `clone_for_thread` spawn uses the
    /// [`Self::init_io_environment_for_thread_clone`] variant, which inherits
    /// already-present dynamic IO handle vars instead of clobbering them.
    ///
    /// `$*CWD` is deliberately rebuilt via a real `current_dir()` syscall on
    /// EVERY call, including thread-clone spawns, even though `cloned.env`
    /// already carries the parent's current `$*CWD` value verbatim (from the
    /// struct-construction-time env clone in `clone_for_thread_excluding`) and
    /// a skip-and-reuse optimization looks safe at first glance. It is NOT
    /// safe: a `start` block that both READS and WRITES a dynamic var inside
    /// itself (e.g. `start indir $dir, { $*CWD.basename; $*CWD = ...; }`) can
    /// have that var boxed into a captured `ContainerRef` cell at
    /// closure-creation time (`box_captured_lexicals`), snapshotting whatever
    /// value is CURRENTLY in `env` at that point — and a later `indir`/dynamic
    /// rebind that only does `self.env.insert(...)` does not reach a cell the
    /// closure already captured. Before this rebuild ran unconditionally, that
    /// snapshot was always a fresh `IO::Path` Instance (built moments earlier
    /// by this very function); skipping the rebuild let the snapshot instead be
    /// whatever non-Path value an outer `my $*CWD = ...` lexical override had
    /// left in env, breaking `t/start-dynamic-var-indir.t`. Rebuilding
    /// unconditionally keeps that pre-closure-creation snapshot correct. See
    /// docs/per-task-clone-slimming.md slice 3 for the caching this function
    /// does apply (the other dynamic vars, which no closure boxing issue
    /// touches because their identity is process-constant, not reassigned by
    /// ordinary programs).
    pub(super) fn init_io_environment(&mut self) {
        self.init_io_environment_impl(false)
    }

    /// [`Self::init_io_environment`] for a `clone_for_thread` spawn
    /// (docs/per-task-clone-slimming.md slice 6): dynamic IO handle vars
    /// (`$*OUT`/`$*ERR`/`$*IN`/`$*ARGFILES`) the cloned env already carries are
    /// INHERITED instead of clobbered with fresh default handles — in Raku a
    /// `start` block sees the spawning scope's `my $*OUT = ...` redirection
    /// (pin: `t/start-inherits-dynamic-out.t`). The non-handle dynamic vars
    /// below the handle block are still rebuilt exactly as before (see the
    /// `$*CWD` comment above for why that rebuild must stay unconditional).
    pub(super) fn init_io_environment_for_thread_clone(&mut self) {
        self.init_io_environment_impl(true)
    }

    /// Whether the cloned env carries a usable inherited entry for the dynamic
    /// IO var `name` (or its bare `alias`): a user object (the output-capture
    /// redirection idiom), or a default handle whose id survived the referenced-
    /// handle clone in `clone_for_thread_excluding`. A handle id that did NOT
    /// survive (e.g. the parent closed it) reports unusable so the caller
    /// rebuilds the default, preserving the pre-slice-6 behavior for that edge.
    fn inherited_io_entry_usable(&self, name: &str, alias: &str) -> bool {
        let Some(v) = self.env.get(name).or_else(|| self.env.get(alias)) else {
            return false;
        };
        match Self::handle_id_from_value(v) {
            Some(id) => self.io_handles().map.contains_key(&id),
            None => !v.is_nil(),
        }
    }

    fn init_io_environment_impl(&mut self, for_thread_clone: bool) {
        if !(for_thread_clone && self.inherited_io_entry_usable("$*OUT", "*OUT")) {
            let stdout = self.create_handle(
                IoHandleTarget::Stdout,
                IoHandleMode::Write,
                Some("STDOUT".to_string()),
            );
            self.env.insert("$*OUT".to_string(), stdout.clone());
            self.env.insert("*OUT".to_string(), stdout);
        }
        if !(for_thread_clone && self.inherited_io_entry_usable("$*ERR", "*ERR")) {
            let stderr = self.create_handle(
                IoHandleTarget::Stderr,
                IoHandleMode::Write,
                Some("STDERR".to_string()),
            );
            self.env.insert("$*ERR".to_string(), stderr.clone());
            self.env.insert("*ERR".to_string(), stderr);
        }
        if !(for_thread_clone && self.inherited_io_entry_usable("$*IN", "*IN")) {
            let stdin = self.create_handle(
                IoHandleTarget::Stdin,
                IoHandleMode::Read,
                Some("STDIN".to_string()),
            );
            self.env.insert("$*IN".to_string(), stdin.clone());
            self.env.insert("*IN".to_string(), stdin);
        }
        if !(for_thread_clone && self.inherited_io_entry_usable("$*ARGFILES", "*ARGFILES")) {
            let argfiles = self.create_handle(
                IoHandleTarget::ArgFiles,
                IoHandleMode::Read,
                Some("$*ARGFILES".to_string()),
            );
            self.env.insert("$*ARGFILES".to_string(), argfiles.clone());
            self.env.insert("*ARGFILES".to_string(), argfiles);
        }
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
        let tmpdir_val = self.make_io_path_instance(Self::cached_tmpdir_string());
        self.env.insert("$*TMPDIR".to_string(), tmpdir_val.clone());
        self.env.insert("*TMPDIR".to_string(), tmpdir_val);
        let home_val = match Self::cached_home_string() {
            Some(home) => self.make_io_path_instance(home),
            None => Value::NIL,
        };
        self.env.insert("$*HOME".to_string(), home_val.clone());
        self.env.insert("*HOME".to_string(), home_val);
        // $*EXECUTABLE - path to the interpreter binary
        let exe_path = Self::cached_executable_path_string();
        let exe_io = self.make_io_path_instance(exe_path);
        self.env.insert("$*EXECUTABLE".to_string(), exe_io.clone());
        self.env.insert("*EXECUTABLE".to_string(), exe_io);
        self.env.insert(
            "$*EXECUTABLE-NAME".to_string(),
            Value::str(
                std::path::Path::new(exe_path)
                    .file_name()
                    .map(|f| f.to_string_lossy().to_string())
                    .unwrap_or_else(|| exe_path.to_string()),
            ),
        );
        let exec_name = self.env.get("$*EXECUTABLE-NAME").cloned().unwrap();
        self.env.insert("*EXECUTABLE-NAME".to_string(), exec_name);
        // $*DISTRO/$*PERL/$*RAKU/$*VM/$*KERNEL are intentionally NOT built or
        // inserted here (todo/tickets/magic-vars-should-be-built-lazily.md
        // Slice 2). They materialize on first read via
        // `lazy_magic_dynamic_var`, called from the general dynamic-var read
        // miss path (`Interpreter::get_env_with_main_alias_inner`), and are
        // cached process-wide the same way as before (see the
        // `cached_*_instance` OnceLocks below).
    }

    /// Construct-on-first-read for the five process-constant magic vars whose
    /// `Instance` building (Version parses, a 32-element signal array, the
    /// `vm_config` hash) is real CPU work: `$*DISTRO`/`$*PERL`/`$*RAKU`/
    /// `$*VM`/`$*KERNEL` (todo/tickets/magic-vars-should-be-built-lazily.md
    /// Slice 2). `name` is the env key exactly as compiled (sigil-and-twigil
    /// forms: bare `*NAME` for a `$*NAME` read, `?NAME` for the rarer `$?NAME`
    /// compile-time-twigil spelling, plus the literal `$*VM` key that a couple
    /// of call sites still probe directly).
    ///
    /// Called from `Interpreter::get_env_with_main_alias_inner`'s final
    /// fallback (`src/vm/vm_env_helpers.rs`) — the one chokepoint every other
    /// per-instance/dynamic-var read path (the VM's `GetGlobal` fast path,
    /// `get_dynamic_handle`, ...) already falls through to on a genuine miss,
    /// so a single check here covers every reader without touching each of
    /// them individually. Each underlying `Value` is still built at most once
    /// per process (the `cached_*_instance` `OnceLock`s below do that part);
    /// this only decides *when* that first build happens.
    pub(crate) fn lazy_magic_dynamic_var(name: &str) -> Option<Value> {
        Some(match name {
            "*DISTRO" | "?DISTRO" => Self::cached_distro_instance(),
            "*PERL" | "?PERL" => Self::cached_perl_instance(),
            "*RAKU" | "?RAKU" => Self::cached_raku_instance(),
            "$*VM" | "*VM" | "?VM" => Self::cached_vm_instance(),
            "*KERNEL" | "?KERNEL" => Self::cached_kernel_instance(),
            "$*COLLATION" | "*COLLATION" => Self::cached_collation_instance(),
            "*USER" => Self::cached_user_instance(),
            "*GROUP" => Self::cached_group_instance(),
            _ => return None,
        })
    }

    /// The process-wide `$*COLLATION` singleton, materialized on first read.
    ///
    /// Rakudo declares `$*COLLATION` in `PROCESS::` as one mutable `Collation`
    /// object with every level enabled (`collation-level => 85`), and
    /// `Collation.set` mutates *that* object and returns it — so a `.set` made
    /// anywhere is observed everywhere afterwards, including inside a called sub
    /// and by the `coll`/`unicmp` operators. Handing out clones of one cached
    /// `Value` reproduces exactly that: an instance's attributes live in a shared
    /// `Gc<InstanceAttrs>` cell that `Value::write_back_sharing` (the `set` arm in
    /// `collation_temporal.rs`) commits into in place, so every holder — and every
    /// later read of the magic var — sees the update. A `my $*COLLATION =
    /// Collation.new` still shadows it lexically, exactly as in rakudo.
    fn cached_collation_instance() -> Value {
        static CACHE: OnceLock<Value> = OnceLock::new();
        CACHE
            .get_or_init(|| Self::make_collation_instance(1, 1, 1, 1))
            .clone()
    }

    /// Process-constant `Distro` instance (docs/per-task-clone-slimming.md
    /// slice 3; built lazily since Slice 2 of
    /// todo/tickets/magic-vars-should-be-built-lazily.md — see
    /// `lazy_magic_dynamic_var`): built once, on first read, via
    /// `make_distro_instance` (which shells out to `sw_vers` on macOS and
    /// reads `/etc/os-release` on Linux) and shared by `Value` clone (a cheap
    /// handle copy, see `Value`'s internal `Arc`/`Gc` reprs) into every
    /// interpreter/thread that reads it thereafter.
    fn cached_distro_instance() -> Value {
        static CACHE: OnceLock<Value> = OnceLock::new();
        CACHE.get_or_init(Self::make_distro_instance).clone()
    }

    /// Process-constant `$*PERL` instance. Cached separately from
    /// [`Self::cached_raku_instance`] even though both build from the same
    /// `make_perl_instance()` body: `$*PERL` and `$*RAKU` are historically
    /// distinct objects (`$*PERL !=== $*RAKU`), so they get their own cache
    /// slot rather than aliasing one shared instance.
    fn cached_perl_instance() -> Value {
        static CACHE: OnceLock<Value> = OnceLock::new();
        CACHE.get_or_init(Self::make_perl_instance).clone()
    }

    /// Process-constant `$*RAKU` instance (see [`Self::cached_perl_instance`]).
    /// `make_perl_instance`'s "version" attribute is read from
    /// `current_language_version()` at construction time, so the first read
    /// (whenever it happens, always after its compile unit's own parse)
    /// already reflects that unit's `use v6.x`. `update_raku_version_from_parser`
    /// additionally mutates an ALREADY-materialized instance in place via
    /// `Value::write_back_sharing` (commits into the same shared attrs cell,
    /// does not rebind to a new object) for a later parse in the same process
    /// (e.g. a nested `EVAL` with a different version) — matching the
    /// long-standing assumption that `$*RAKU`/`$*PERL` hold one process-wide
    /// value.
    fn cached_raku_instance() -> Value {
        static CACHE: OnceLock<Value> = OnceLock::new();
        CACHE.get_or_init(Self::make_perl_instance).clone()
    }

    /// Process-constant `$*VM` instance.
    fn cached_vm_instance() -> Value {
        static CACHE: OnceLock<Value> = OnceLock::new();
        CACHE.get_or_init(Self::make_vm_instance).clone()
    }

    /// Process-constant `$*KERNEL` instance.
    fn cached_kernel_instance() -> Value {
        static CACHE: OnceLock<Value> = OnceLock::new();
        CACHE.get_or_init(Self::make_kernel_instance).clone()
    }

    /// Process-constant `$*USER` instance (see
    /// `Interpreter::make_user_instance` in `io_sysinfo_user.rs`).
    fn cached_user_instance() -> Value {
        static CACHE: OnceLock<Value> = OnceLock::new();
        CACHE.get_or_init(Self::make_user_instance).clone()
    }

    /// Process-constant `$*GROUP` instance (see
    /// `Interpreter::make_group_instance` in `io_sysinfo_user.rs`).
    fn cached_group_instance() -> Value {
        static CACHE: OnceLock<Value> = OnceLock::new();
        CACHE.get_or_init(Self::make_group_instance).clone()
    }

    /// Process-constant interpreter-executable path string, computed once via
    /// `resolved_current_executable_path()` (a `current_exe()` syscall). The
    /// `IO::Path` `Value` itself is still built fresh per call
    /// (`make_io_path_instance` embeds the CURRENT `$*SPEC`/`$*CWD`), only the
    /// expensive path string is cached.
    fn cached_executable_path_string() -> &'static str {
        static CACHE: OnceLock<String> = OnceLock::new();
        CACHE.get_or_init(|| {
            #[cfg(not(target_arch = "wasm32"))]
            {
                Self::resolved_current_executable_path()
                    .to_string_lossy()
                    .to_string()
            }
            #[cfg(target_arch = "wasm32")]
            {
                "mutsu".to_string()
            }
        })
    }

    /// Process-constant temp-dir path string, computed once via
    /// `env::temp_dir()`. See [`Self::cached_executable_path_string`] for why
    /// only the string (not the `IO::Path` `Value`) is cached.
    fn cached_tmpdir_string() -> &'static str {
        static CACHE: OnceLock<String> = OnceLock::new();
        CACHE.get_or_init(|| {
            #[cfg(not(target_arch = "wasm32"))]
            {
                env::temp_dir().to_string_lossy().to_string()
            }
            #[cfg(target_arch = "wasm32")]
            {
                "/tmp".to_string()
            }
        })
    }

    /// Process-constant `$HOME` path string, computed once via `env::var`.
    /// `None` when the process has no `HOME` (matches the prior per-call
    /// behavior of falling back to `Value::NIL`).
    fn cached_home_string() -> Option<&'static str> {
        static CACHE: OnceLock<Option<String>> = OnceLock::new();
        CACHE
            .get_or_init(|| {
                #[cfg(not(target_arch = "wasm32"))]
                {
                    env::var("HOME").ok()
                }
                #[cfg(target_arch = "wasm32")]
                {
                    None
                }
            })
            .as_deref()
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

    /// Stringify a value by calling .gist. Only a *dispatch* failure (no
    /// `.gist` candidate — X::Method::NotFound / X::Multi::NoMatch) falls
    /// back to the native gist; any other error is the user's code throwing
    /// from inside `.gist` (typically while it forces a lazy Seq: `say f()`
    /// where `f`'s gather dies mid-force must die like rakudo, not silently
    /// print the fallback gist). A `return` control signal keeps its
    /// dedicated re-wrap (integration/error-reporting.t test 21).
    /// TODO: `render_str_value` (put/print) still swallows these signals.
    pub(crate) fn render_gist_value(&mut self, value: &Value) -> Result<String, RuntimeError> {
        // The pure native `.gist` fast path cannot reproduce the base method's
        // virtual `.Str` call on a role Mixin. Enter mixin dispatch directly so
        // a role-provided `gist`, or its inherited-gist/provided-Str fallback,
        // runs before ordinary native dispatch.
        let result = if value.is_mixin_value() {
            self.dispatch_mixin_method_call(value, "gist", vec![])
                .unwrap_or_else(|| self.call_method_with_values(value.clone(), "gist", vec![]))
        } else {
            self.call_method_with_values(value.clone(), "gist", vec![])
        };
        match result {
            Ok(result) => Ok(result.to_string_value()),
            Err(e) if e.return_value.is_some() => Err(RuntimeError::controlflow_return(true)),
            Err(e) if e.is_method_not_found() || e.is_multi_no_match() => {
                Ok(crate::runtime::gist_value(value))
            }
            Err(e) => Err(e),
        }
    }

    /// Emit rakudo's "Use of uninitialized value[ element] of type X in string
    /// context." warning for a bare type object used in string context, and
    /// resume with the empty string. `element` selects the interpolation
    /// wording (`... value element of type ...`) that Rakudo uses inside `"$x"`;
    /// prefix/infix `~` and the string comparators use the non-`element` form.
    ///
    /// Callers must first rule out a user-defined `.Str`/`.Stringy` (a type
    /// object whose class defines one dispatches it and is NOT warned) and any
    /// operator-specific hard error (e.g. `prefix:<~>(Mu:U)`). The warning
    /// handler can run user code that mutates a captured-outer caller lexical,
    /// so its writeback is reconciled here (Slice 1b render pattern).
    pub(crate) fn warn_type_object_string_context(
        &mut self,
        type_name: &str,
        element: bool,
    ) -> Result<Value, RuntimeError> {
        // A lexically-scoped or role-candidate type carries a mangled storage
        // name (ADR-0047 P1: `Foo\u{0}<decl-id>`) — show the user-facing bare
        // name in the message.
        let msg = format!(
            "Use of uninitialized value{} of type {} in string context.\nMethods .^name, .raku, .gist, or .say can be used to stringify it to something meaningful.",
            if element { " element" } else { "" },
            crate::value::user_facing_type_name(type_name),
        );
        let caller_code = self.current_code;
        let resumed = self.raise_resumable_warning(&msg, Value::str(String::new()))?;
        self.reconcile_caller_after_internal_dispatch(caller_code);
        Ok(resumed)
    }

    /// Emit rakudo's "Use of uninitialized value of type X in numeric context"
    /// warning for a bare type object used in numeric context, and resume with
    /// integer `0` (Rakudo's `Mu.Numeric` coercion). The numeric wording has no
    /// `Methods .^name...` suffix (that is string-context only). Like its string
    /// sibling, the warning handler can run user code that mutates a
    /// captured-outer caller lexical, so its writeback is reconciled here.
    pub(crate) fn warn_type_object_numeric_context(
        &mut self,
        type_name: &str,
    ) -> Result<Value, RuntimeError> {
        self.warn_type_object_numeric_context_resume(type_name, Value::int(0))
    }

    /// Like [`Self::warn_type_object_numeric_context`], but resumes with an
    /// explicit value instead of the default integer `0`. Prefix `+`/`-` on a
    /// bare numeric type object resumes with the type's own numeric *zero*
    /// (`+Num` → `0e0`, `+Rat` → `0.0`, `+Complex` → `0+0i`), not a bare `Int 0`.
    pub(crate) fn warn_type_object_numeric_context_resume(
        &mut self,
        type_name: &str,
        resume: Value,
    ) -> Result<Value, RuntimeError> {
        // Demangled for the same reason as the string-context sibling above.
        let type_name = crate::value::user_facing_type_name(type_name);
        let msg = format!("Use of uninitialized value of type {type_name} in numeric context");
        let caller_code = self.current_code;
        let resumed = self.raise_resumable_warning(&msg, resume)?;
        self.reconcile_caller_after_internal_dispatch(caller_code);
        Ok(resumed)
    }

    /// The numeric *zero* a bare type object of `type_name` resumes with when
    /// used in numeric context (prefix `+`/`-`, `.Numeric` on the type object):
    /// `Num` → `0e0`, `Rat` → `0.0`, `FatRat` → `FatRat.new(0, 1)`, `Complex` →
    /// `0+0i`, and every other type (`Int`, `Real`, `Cool`, `Str`, user classes,
    /// ...) → `Int 0`. Mirrors rakudo's `Mu.Numeric` per-type coercion.
    pub(crate) fn type_object_numeric_zero(type_name: &str) -> Value {
        match type_name {
            "Num" => Value::num(0.0),
            "Rat" => crate::value::make_rat(0, 1),
            "FatRat" => crate::value::make_big_fat_rat(0.into(), 1.into()),
            "Complex" => Value::complex(0.0, 0.0),
            _ => Value::int(0),
        }
    }

    /// Coerce a value used as a plain (`Str`-keyed) hash subscript key into its
    /// string key, matching Rakudo. A bare type object stringifies to the empty
    /// string with the "uninitialized value of type X in string context"
    /// warning — unless its class defines a user `.Str`/`.Stringy`, which
    /// dispatches (`%h{Foo}` where `Foo` defines `method Str` keys as its
    /// result, no warning). All other values use the ordinary
    /// `to_string_value` encoding. Object hashes (typed keys) do NOT reach here
    /// — they key by `.WHICH` and keep the type object.
    pub(crate) fn coerce_type_object_hash_key(
        &mut self,
        val: &Value,
    ) -> Result<String, RuntimeError> {
        if let ValueView::Package(name) = val.view() {
            let cn = name.resolve().to_string();
            if self.has_user_method(&cn, "Stringy")
                && let Ok(r) = self.call_method_with_values(val.clone(), "Stringy", vec![])
            {
                return Ok(r.to_string_value());
            }
            if self.has_user_method(&cn, "Str")
                && let Ok(r) = self.call_method_with_values(val.clone(), "Str", vec![])
            {
                return Ok(r.to_string_value());
            }
            return Ok(self
                .warn_type_object_string_context(&cn, false)?
                .to_string_value());
        }
        Ok(val.to_string_value())
    }

    /// Build a `Hash` from a flat item list (`my %h = (...)`, `%(...)`, `Hash(...)`),
    /// coercing a bare type-object key to `""` with the Rakudo
    /// "uninitialized value of type X in string context" warning (or dispatching
    /// a user `.Str`/`.Stringy`). This is the interpreter-aware counterpart of the
    /// silent `build_hash_from_items`; the warning requires `&mut self`, so the
    /// no-interpreter `.hash`/`.Hash` list path only coerces silently.
    pub(crate) fn build_hash_from_items_warning(
        &mut self,
        items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        crate::runtime::utils::build_hash_from_items_with_key_coercion(items, |kk| {
            if matches!(kk.view(), ValueView::Package(_)) {
                Ok((self.coerce_type_object_hash_key(kk)?, false))
            } else {
                Ok((
                    Value::hash_key_encode(kk),
                    !matches!(kk.view(), ValueView::Str(_)),
                ))
            }
        })
    }

    /// Stringify a value by calling .Str method (used by put/print).
    /// Falls back to to_string_value() if .Str method dispatch fails.
    pub(crate) fn render_str_value(&mut self, value: &Value) -> String {
        // Printing a type object stringifies to "" with rakudo's
        // uninitialized-value warning suggesting .^name/.raku/.gist/.say —
        // unless its class defines a user `.Stringy`/`.Str`, which dispatches
        // instead (`class A { method Str {"foo"} }` then `print A` renders
        // "foo", matching Rakudo).
        if let ValueView::Package(name) = value.view() {
            let n = name.resolve().to_string();
            if self.has_user_method(&n, "Stringy")
                && let Ok(r) = self.call_method_with_values(value.clone(), "Stringy", vec![])
            {
                return r.to_string_value();
            }
            if self.has_user_method(&n, "Str")
                && let Ok(r) = self.call_method_with_values(value.clone(), "Str", vec![])
            {
                return r.to_string_value();
            }
            return self
                .warn_type_object_string_context(&n, false)
                .map(|v| v.to_string_value())
                .unwrap_or_default();
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
