use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn try_native_io_path_construct(
        &mut self,
        class_name: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn_resolved = class_name.resolve();
        if !Self::is_io_path_lexical_class(&cn_resolved) {
            return None;
        }
        // SPEC-variant subclasses are registered (parent IO::Path) the first time
        // they are constructed, mirroring the interpreter's `dispatch_new`.
        if cn_resolved.starts_with("IO::Path::")
            && !self.registry().classes.contains_key(cn_resolved.as_str())
        {
            self.registry_mut().classes.insert(
                cn_resolved.to_string(),
                ClassDef {
                    parents: vec!["IO::Path".to_string()],
                    attributes: Vec::new(),
                    methods: HashMap::new(),
                    native_methods: std::collections::HashSet::new(),
                    mro: Vec::new(),
                    attribute_types: HashMap::new(),
                    attribute_smileys: HashMap::new(),
                    attribute_built: HashMap::new(),
                    wildcard_handles: Vec::new(),
                    alias_attributes: HashSet::new(),
                    class_level_attrs: HashMap::new(),
                },
            );
        }
        Some(self.build_io_path_instance(class_name, &cn_resolved, args))
    }

    /// Pure path-string assembly for an `IO::Path` family `.new(...)`: a
    /// positional path (or an `IO::Path` instance whose `path` is reused), or a
    /// `basename`/`dirname`/`volume` triple, joined with the SPEC-derived
    /// separator, plus optional `CWD`/`SPEC` attributes. Reads only the registry
    /// (`class_mro`, for the IO::Path-instance argument case) — no FS, no cwd, no
    /// env, no user code. The single authoritative impl shared by the interpreter's
    /// `dispatch_new` arm and the VM's `try_native_io_path_construct`.
    pub(crate) fn build_io_path_instance(
        &mut self,
        class_name: Symbol,
        cn_resolved: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut positional_path: Option<String> = None;
        let mut basename_part: Option<String> = None;
        let mut dirname_part: Option<String> = None;
        let mut volume_part: Option<String> = None;
        let mut cwd_attr: Option<String> = None;
        let mut spec_attr: Option<Value> = None;
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "CWD" => {
                    cwd_attr = Some(value.to_string_value());
                }
                Value::Pair(key, value) if key == "SPEC" => {
                    spec_attr = Some((**value).clone());
                }
                Value::Pair(key, value) if key == "basename" => {
                    basename_part = Some(value.to_string_value());
                }
                Value::Pair(key, value) if key == "dirname" => {
                    dirname_part = Some(value.to_string_value());
                }
                Value::Pair(key, value) if key == "volume" => {
                    volume_part = Some(value.to_string_value());
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if positional_path.is_none()
                    && self
                        .class_mro(&class_name.resolve())
                        .iter()
                        .any(|n| n == "IO::Path") =>
                {
                    positional_path = Some(
                        attributes
                            .as_map()
                            .get("path")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default(),
                    );
                    if cwd_attr.is_none() {
                        cwd_attr = attributes.as_map().get("cwd").map(|v| v.to_string_value());
                    }
                }
                Value::Pair(_, _) => {}
                _ if positional_path.is_none() => {
                    positional_path = Some(arg.to_string_value());
                }
                _ => {}
            }
        }
        // Determine dir separator from SPEC (Win32 uses '\', others use '/')
        let is_win32_spec = spec_attr
            .as_ref()
            .map(|s| {
                let name = match s {
                    Value::Package(n) => n.resolve().to_string(),
                    Value::Instance { class_name, .. } => class_name.resolve().to_string(),
                    _ => String::new(),
                };
                name == "IO::Spec::Win32" || name.ends_with("Win32")
            })
            .unwrap_or(false);
        let dir_sep = if is_win32_spec { '\\' } else { '/' };
        let path = if let Some(positional) = positional_path {
            positional
        } else if let Some(basename) = basename_part {
            let mut built = match dirname_part {
                Some(dirname) if !dirname.is_empty() => {
                    if dirname.ends_with('/') || dirname.ends_with('\\') {
                        format!("{dirname}{basename}")
                    } else {
                        format!("{dirname}{dir_sep}{basename}")
                    }
                }
                _ => basename,
            };
            if let Some(volume) = volume_part
                && !volume.is_empty()
            {
                if volume.ends_with('/') || volume.ends_with('\\') {
                    built = format!("{volume}{built}");
                } else {
                    built = format!("{volume}{dir_sep}{built}");
                }
            }
            built
        } else {
            String::new()
        };
        if path.is_empty() {
            return Err(RuntimeError::new(
                "Must specify a non-empty string as a path",
            ));
        }
        if path.contains('\0') {
            return Err(RuntimeError::new(
                "X::IO::Null: Found null byte in pathname",
            ));
        }
        let mut attrs = HashMap::new();
        attrs.insert("path".to_string(), Value::str(path));
        if let Some(cwd) = cwd_attr {
            attrs.insert("cwd".to_string(), Value::str(cwd));
        }
        if cn_resolved.starts_with("IO::Path::") {
            let spec_name = format!("IO::Spec::{}", cn_resolved.trim_start_matches("IO::Path::"));
            attrs.insert(
                "SPEC".to_string(),
                Value::Package(Symbol::intern(&spec_name)),
            );
        } else if let Some(spec) = spec_attr {
            attrs.insert("SPEC".to_string(), spec);
        } else if let Some(spec) = self
            .env
            .get("$*SPEC")
            .or_else(|| self.env.get("*SPEC"))
            .cloned()
            .or_else(|| self.get_dynamic_var("*SPEC").ok())
        {
            // No explicit `:SPEC` — default to the dynamic `$*SPEC` (Raku:
            // `IO::Path.new('.').SPEC eqv $*SPEC`), so a `temp $*SPEC = ...`
            // override is honored by the constructor too.
            attrs.insert("SPEC".to_string(), spec);
        }
        Ok(Value::make_instance(class_name, attrs))
    }

    /// VM-native construction for a built-in type whose `.new(...)` is pure data
    /// assembly (no env / registry / user code): `Buf`/`Blob` (byte overlay),
    /// `utf8`/`utf16` (code units), `Uni` (codepoints), `Version`, `Duration`
    /// (the one fallible builder — a bad string is `X::Str::Numeric`),
    /// `StrDistance`, `Stash` and the empty-instance schedulers/handles. Returns
    /// `Some` with the constructed value (or its error) when `class_name` is one
    /// of these, else `None` so the caller falls through to the generic
    /// constructor. The interpreter's `dispatch_new` arms call the same per-type
    /// helpers, so the native path is byte-identical.
    /// VM-native construction for `Proc::Async.new(@cmd, :w, :enc)`. Pure data
    /// assembly: parse the positional command + `:w`/`:enc` flags, allocate three
    /// fresh process-global supply ids (`next_supply_id`, a bare global counter),
    /// and build empty stdout/stderr/merged `Supply` instances plus the
    /// not-yet-started `Proc::Async` instance. No `&self` — the process is only
    /// spawned later, by `.start`. The interpreter's `dispatch_new` arm delegates
    /// here so the native VM fast path is byte-identical.
    pub(crate) fn build_native_proc_async_value(class_name: Symbol, args: &[Value]) -> Value {
        let mut positional = Vec::new();
        let mut w_flag = false;
        let mut enc = Value::str_from("utf-8");
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "w" => {
                    w_flag = value.truthy();
                }
                Value::Pair(key, _value) if key == "out" => {}
                Value::Pair(key, value) if key == "enc" => {
                    enc = Value::str(value.to_string_value());
                }
                _ => positional.push(arg.clone()),
            }
        }
        let stdout_id = super::native_methods::next_supply_id();
        let stderr_id = super::native_methods::next_supply_id();
        let supply_id = super::native_methods::next_supply_id();
        let stdout_descriptor = SharedPromise::new();
        let stderr_descriptor = SharedPromise::new();
        let mut stdout_supply_attrs = HashMap::new();
        stdout_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
        stdout_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        stdout_supply_attrs.insert("supply_id".to_string(), Value::Int(stdout_id as i64));
        stdout_supply_attrs.insert("enc".to_string(), enc.clone());
        stdout_supply_attrs.insert(
            "native_descriptor_promise".to_string(),
            Value::Promise(stdout_descriptor),
        );
        let mut stderr_supply_attrs = HashMap::new();
        stderr_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
        stderr_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        stderr_supply_attrs.insert("supply_id".to_string(), Value::Int(stderr_id as i64));
        stderr_supply_attrs.insert("enc".to_string(), enc.clone());
        stderr_supply_attrs.insert(
            "native_descriptor_promise".to_string(),
            Value::Promise(stderr_descriptor),
        );
        let mut merged_supply_attrs = HashMap::new();
        merged_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
        merged_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        merged_supply_attrs.insert("supply_id".to_string(), Value::Int(supply_id as i64));

        let mut attrs = HashMap::new();
        attrs.insert("cmd".to_string(), Value::array(positional));
        attrs.insert("started".to_string(), Value::Bool(false));
        attrs.insert("enc".to_string(), enc);
        attrs.insert(
            "stdout".to_string(),
            Value::make_instance(Symbol::intern("Supply"), stdout_supply_attrs),
        );
        attrs.insert(
            "stderr".to_string(),
            Value::make_instance(Symbol::intern("Supply"), stderr_supply_attrs),
        );
        attrs.insert(
            "supply".to_string(),
            Value::make_instance(Symbol::intern("Supply"), merged_supply_attrs),
        );
        if w_flag {
            attrs.insert("w".to_string(), Value::Bool(true));
        }
        Value::make_instance(class_name, attrs)
    }

    pub(crate) fn try_native_builtin_construct(
        class_name: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn = class_name.resolve();
        if Self::is_native_buf_constructible(&cn) {
            Some(Ok(Self::build_native_buf_value(class_name, args)))
        } else if cn == "utf8" || cn == "utf16" {
            Some(Ok(Self::build_native_utf_value(class_name, args)))
        } else if cn == "Uni" {
            Some(Ok(Self::build_native_uni_value(args)))
        } else if cn == "Version" {
            Some(Ok(Self::version_from_value(
                args.first().cloned().unwrap_or(Value::Nil),
            )))
        } else if cn == "Complex" {
            Some(Ok(Self::build_native_complex_value(args)))
        } else if cn == "Int" {
            Some(Self::build_native_int_value(args))
        } else if cn == "Num" {
            Some(Self::build_native_num_value(args))
        } else if cn == "Str" {
            // The default Str constructor ignores positional args and yields the
            // empty string (mutsu is lenient where raku rejects a positional).
            // (`Bool` is intentionally NOT native-ized: it is an enum, so
            // `Bool.new` errors in `dispatch_new` before the basic-type arm —
            // the native path must preserve that, so it falls through.)
            Some(Ok(Value::str(String::new())))
        } else if cn == "Slip" {
            Some(Ok(Value::slip(args.to_vec())))
        } else if cn == "IterationBuffer" {
            Some(Ok(Self::build_native_iterationbuffer_value(
                class_name, args,
            )))
        } else if cn == "Rat" {
            Some(Ok(Self::build_native_rat_value(args)))
        } else if cn == "FatRat" {
            Some(Ok(Self::build_native_fatrat_value(args)))
        } else if cn == "Pair" {
            Some(Ok(Self::build_native_pair_value(args)))
        } else if cn == "Date" {
            // A `:formatter` renders a user Callable (`render_date_formatter`,
            // self-dependent) — fall through to the interpreter for that case;
            // the common no-formatter date is built natively.
            match Self::build_native_date(args) {
                Ok((date, None)) => Some(Ok(date)),
                Ok((_, Some(_))) => None,
                Err(e) => Some(Err(e)),
            }
        } else if cn == "DateTime" {
            // Same `:formatter` caveat as `Date` (the formatter renders a user
            // Callable via `eval_call_on_value`).
            match Self::build_native_datetime(args) {
                Ok((dt, None)) => Some(Ok(dt)),
                Ok((_, Some(_))) => None,
                Err(e) => Some(Err(e)),
            }
        } else if cn == "Duration" {
            Some(Self::build_native_duration_value(args))
        } else if cn == "StrDistance" {
            Some(Ok(Self::build_native_strdistance_value(args)))
        } else if cn == "Stash" {
            // A `Stash` is an empty Hash-typed instance.
            Some(Ok(Value::make_instance(class_name, HashMap::new())))
        } else if matches!(
            cn.as_str(),
            "ThreadPoolScheduler" | "CurrentThreadScheduler" | "Tap" | "Cancellation"
        ) {
            // These take no construction args — just an empty instance.
            Some(Ok(Value::make_instance(class_name, HashMap::new())))
        } else if matches!(cn.as_str(), "Lock" | "Lock::Async" | "Lock::Soft") {
            // A lock is pure data: a fresh global lock id (and an `async` flag for
            // `Lock::Async`). `next_lock_id` only bumps a process-global counter —
            // no env / registry / user code — so the VM builds it directly,
            // byte-identical to the interpreter's `dispatch_new` arm.
            let mut attrs = HashMap::new();
            attrs.insert(
                "lock-id".to_string(),
                Value::Int(super::native_methods::next_lock_id() as i64),
            );
            if cn == "Lock::Async" {
                attrs.insert("async".to_string(), Value::Bool(true));
            }
            Some(Ok(Value::make_instance(class_name, attrs)))
        } else if cn == "Promise" {
            // A bare `Promise.new` is an empty planned promise — pure shared
            // state, no env / registry / user code. Shared with the interpreter's
            // `dispatch_new` arm.
            Some(Ok(Value::Promise(crate::value::SharedPromise::new())))
        } else if cn == "Channel" {
            // Likewise an empty channel.
            Some(Ok(Value::Channel(crate::value::SharedChannel::new())))
        } else if matches!(cn.as_str(), "Supplier" | "Supplier::Preserving") {
            // A supplier is pure data: an empty emission log, a not-done flag, and
            // a fresh process-global supplier id. Shared with the interpreter's
            // `dispatch_new` arm.
            let mut attrs = HashMap::new();
            attrs.insert("emitted".to_string(), Value::array(Vec::new()));
            attrs.insert("done".to_string(), Value::Bool(false));
            attrs.insert(
                "supplier_id".to_string(),
                Value::Int(super::native_methods::next_supplier_id() as i64),
            );
            Some(Ok(Value::make_instance(class_name, attrs)))
        } else if cn == "Proc::Async" {
            // `Proc::Async.new(@cmd, :w, :enc)` is pure data: arg parsing +
            // process-global supply ids + empty Supply attributes. The process is
            // only spawned later by `.start`. Shared with the interpreter's
            // `dispatch_new` arm via the single `build_native_proc_async_value`.
            Some(Ok(Self::build_native_proc_async_value(class_name, args)))
        } else if cn == "Capture" {
            // The default `Capture.new` produces an *empty* Capture: named args
            // are dropped (Capture has no buildable public attributes — `bless`
            // ignores them) and positional args are rejected (`Mu.new` is
            // named-only). A *populated* Capture is built with the `\(...)`
            // literal, not `.new`. A named arg reaches here as `Value::Pair`;
            // anything else (a literal, a positional `"a" => 1` `ValuePair`) is
            // positional and dies, exactly as raku does.
            if args.iter().any(|a| !matches!(a, Value::Pair(..))) {
                Some(Err(RuntimeError::new(
                    "Default constructor for 'Capture' only takes named arguments",
                )))
            } else {
                Some(Ok(Value::Capture {
                    positional: Box::new(Vec::new()),
                    named: Box::new(HashMap::new()),
                }))
            }
        } else if cn == "FakeScheduler" {
            Some(Ok(Self::build_native_fakescheduler_value()))
        } else if cn == "Proxy" {
            Some(Ok(Self::build_native_proxy_value(args)))
        } else if cn == "Match" {
            Some(Ok(Self::build_native_match_value(args)))
        } else if matches!(cn.as_str(), "IntStr" | "NumStr" | "RatStr" | "ComplexStr") {
            // Allomorph `.new(numeric, string)` is pure data assembly (a numeric
            // value mixed with a `Str` override) — shared with the interpreter's
            // `dispatch_new_and_constructors` arm.
            Some(Self::build_native_allomorph_value(&cn, args))
        } else if matches!(cn.as_str(), "ObjAt" | "ValueObjAt") {
            // `ObjAt`/`ValueObjAt` `.new(which)` stores the stringified first
            // positional as the `WHICH` attribute — pure data assembly.
            Some(Self::build_native_objat_value(class_name, args))
        } else {
            None
        }
    }

    /// VM-native dispatch for a built-in *class* method (a method on a type
    /// object other than `.new`) whose result is pure data assembly — no env /
    /// registry / user code. Currently `Instant.from-posix(secs)`, which maps a
    /// POSIX timestamp to TAI seconds and wraps it in an `Instant`. Returns
    /// `Some` when handled, else `None` so the caller falls through to the
    /// interpreter's class-method dispatch. The interpreter calls the same arm,
    /// so the two stay byte-identical. The method name is matched dash-
    /// insensitively (`from-posix` == `from_posix`), as the interpreter does.
    pub(crate) fn try_native_builtin_class_method(
        class_name: Symbol,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn = class_name.resolve();
        if cn == "Instant" && method.replace('-', "_") == "from_posix" {
            let secs = args.first().and_then(to_float_value).unwrap_or(0.0);
            let tai = crate::builtins::methods_0arg::temporal::posix_to_instant(secs);
            let mut attrs = HashMap::new();
            attrs.insert("value".to_string(), Value::Num(tai));
            return Some(Ok(Value::make_instance(Symbol::intern("Instant"), attrs)));
        }
        None
    }

    /// Drop *named* arguments from a Hash/QuantHash constructor's argument list.
    ///
    /// A bareword `key => value` argument is a named argument that `.new`
    /// silently eats — it does NOT become an element (`Bag.new(a => 1).elems`
    /// is 0, while `Bag.new("a" => 1).elems` is 1). mutsu keeps the
    /// named/positional distinction in the value form: a named argument reaches
    /// here un-containerized as a `Value::Pair`, whereas a positional pair
    /// literal (`(a => 1)`, `"a" => 1`) is a `Value::ValuePair`, and pairs
    /// flattened out of an array/variable remain inside their `Array`. So
    /// dropping top-level `Value::Pair`s strips exactly the named arguments
    /// while preserving every positional pair.
    pub(super) fn strip_named_pair_args(args: Vec<Value>) -> Vec<Value> {
        if args.iter().any(|a| matches!(a, Value::Pair(..))) {
            args.into_iter()
                .filter(|a| !matches!(a, Value::Pair(..)))
                .collect()
        } else {
            args
        }
    }
}
