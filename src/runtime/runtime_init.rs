use super::*;
use crate::value::ValueMap;

/// The OS environment, as the backing map of `%*ENV`.
///
/// wasm32 has no process environment to sweep, so the browser build starts
/// with an empty `%*ENV` that scripts can still write to.
#[cfg(not(target_family = "wasm"))]
pub(crate) fn os_env_hash() -> ValueMap {
    std::env::vars()
        .map(|(key, value)| (key, builtins_collection::builtin_val(&[Value::str(value)])))
        .collect()
}

#[cfg(target_family = "wasm")]
pub(crate) fn os_env_hash() -> ValueMap {
    ValueMap::default()
}

impl Interpreter {
    /// Take any pending regex security error from the thread-local store.
    pub(crate) fn take_pending_regex_error() -> Option<RuntimeError> {
        // Delegate to the regex_parse module's thread-local error store
        regex_parse::PENDING_REGEX_ERROR.with(|e| e.borrow_mut().take())
    }

    /// Build the built-in declaration registry: every built-in `ClassDef`,
    /// the exception-class hierarchy, the composed-role seeds and the seeded
    /// method table.
    ///
    /// The built-in declaration registry, built **once per process** and shared
    /// by every top-level interpreter.
    ///
    /// [`Self::build_builtin_registry`] is the single most expensive thing
    /// `Interpreter::new` does — ~70% of a construct-and-drop cycle's
    /// instructions (#7572), split between building ~450 `ClassDef`s plus
    /// their seeded method entries and then dropping them again. The result is
    /// the same for every interpreter in the process, so it is built once and
    /// handed out as a shared `Arc`.
    ///
    /// Safe because the registry is already copy-on-write: every mutation goes
    /// through [`RegistryWriteGuard`], whose `deref_mut` calls `Arc::make_mut`,
    /// so the first write an interpreter performs forks it a private copy. This
    /// is exactly the mechanism `clone_for_thread` has always used to share one
    /// registry snapshot between threads — a shared process-wide template is
    /// the same share, one level up.
    ///
    /// [`RegistryWriteGuard`]: crate::runtime::registry::RegistryWriteGuard
    pub(super) fn shared_builtin_registry() -> Arc<Registry> {
        static TEMPLATE: std::sync::OnceLock<Arc<Registry>> = std::sync::OnceLock::new();
        Arc::clone(TEMPLATE.get_or_init(|| Arc::new(Self::build_builtin_registry())))
    }

    fn build_builtin_registry() -> Registry {
        let mut classes = rustc_hash::FxHashMap::default();
        classes.insert(
            "Mu".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Mu"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Any".to_string(),
            ClassDef {
                parents: vec!["Mu".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Any", "Mu"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "IterationBuffer".to_string(),
            ClassDef {
                parents: vec!["Any".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["IterationBuffer", "Any", "Mu"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Promise".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["keep", "result", "status", "then"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Promise"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Promise::Vow".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["keep", "break"].iter().map(|s| s.to_string()).collect(),
                mro: sym_mro(&["Promise::Vow"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Channel".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["send", "receive", "close", "closed"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Channel"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Collation".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Collation"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Thread".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["finish", "id"].iter().map(|s| s.to_string()).collect(),
                mro: sym_mro(&["Thread"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Supply".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: [
                    "emit",
                    "share",
                    "tap",
                    "act",
                    "encode",
                    "decode",
                    "repeated",
                    "do",
                    "reverse",
                    "split",
                    "interval",
                    "tail",
                    "delayed",
                    "min",
                    "minmax",
                    "collate",
                    "lines",
                    "merge",
                    "unique",
                    "on-close",
                    "classify",
                    "categorize",
                    "Channel",
                    "Supply",
                    "Promise",
                    "schedule-on",
                    "native-descriptor",
                    "migrate",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Supply"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "utf8".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["utf8"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "utf16".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["utf16"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Blob".to_string(),
            ClassDef {
                parents: vec!["Any".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Blob", "Any", "Mu"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Buf".to_string(),
            ClassDef {
                parents: vec!["Blob".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Buf", "Blob", "Any", "Mu"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Supplier".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: [
                    "emit",
                    "done",
                    "quit",
                    "Supply",
                    "__mutsu_register_close_phaser",
                    "__mutsu_interval_tick",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Supplier"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Supplier::Preserving".to_string(),
            ClassDef {
                parents: vec!["Supplier".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Supplier::Preserving", "Supplier"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Proc::Async".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                // `new` is Proc::Async's own constructor (`new(*@args)`), so it
                // is a local method in rakudo's `.^method_table` -- Test::Mock
                // walks that table to decide what it may override (#9340).
                native_methods: [
                    "new",
                    "start",
                    "command",
                    "started",
                    "w",
                    "pid",
                    "stdout",
                    "stderr",
                    "Supply",
                    "bind-stdin",
                    "bind-stdout",
                    "bind-stderr",
                    "kill",
                    "write",
                    "close-stdin",
                    "ready",
                    "print",
                    "put",
                    "say",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Proc::Async"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert("Proc".to_string(), {
            // Proc's public attributes, registered so `.raku`/`.gist` renders
            // the full Rakudo form. An un-run `Proc.new` seeds each with its
            // type-object / value default: `in`/`out`/`err` => IO::Pipe,
            // os-error => Str, signal => Any, exitcode/pid => Nil, command => [].
            let proc_attr = |name: &str, sigil: char, default: Option<Expr>| -> ClassAttributeDef {
                ClassAttributeDef {
                    name: name.to_string(),
                    is_public: true,
                    default: default.map(|e| crate::opcode::DeclTraitArg::Ast(Box::new(e))),
                    captured_env: None,
                    captured_unit: None,
                    declaring_package: None,
                    is_rw: false,
                    is_required: None,
                    sigil,
                    type_constraint: None,
                    where_constraint: None,
                    declared_shape: None,
                    source_line: None,
                    source_file: None,
                    default_is_seed: false,
                }
            };
            let nil_default = || Some(Expr::Literal(Value::NIL));
            let mut attribute_types = HashMap::new();
            attribute_types.insert("in".to_string(), "IO::Pipe".to_string());
            attribute_types.insert("out".to_string(), "IO::Pipe".to_string());
            attribute_types.insert("err".to_string(), "IO::Pipe".to_string());
            attribute_types.insert("os-error".to_string(), "Str".to_string());
            ClassDef {
                parents: Vec::new(),
                attributes: vec![
                    proc_attr("in", '$', None),
                    proc_attr("out", '$', None),
                    proc_attr("err", '$', None),
                    proc_attr("os-error", '$', None),
                    proc_attr("exitcode", '$', nil_default()),
                    proc_attr("signal", '$', None),
                    proc_attr("pid", '$', nil_default()),
                    proc_attr("command", '@', None),
                ],
                // `Str`/`gist` are NOT native: Rakudo's Proc has no stringifier
                // of its own, so the default instance repr applies (`Proc.new`),
                // not the exitcode number the old native arm rendered.
                native_methods: [
                    "exitcode", "signal", "command", "pid", "err", "out", "in", "Numeric", "Int",
                    "Bool", "spawn", "shell", "run",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Proc"]),
                attribute_types,
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            }
        });
        classes.insert(
            "Tap".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["cancel", "close", "socket-port", "socket-host"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Tap"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "__ScheduledTapPump".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                // ADR-0028 Slice 1: the callback-shim target `Supply.
                // schedule-on()` taps invoke instead of the real tap/done/
                // quit callback — see `native_methods::scheduled_tap_pump`.
                native_methods: [
                    "__mutsu_scheduled_emit",
                    "__mutsu_scheduled_done",
                    "__mutsu_scheduled_quit",
                    "__mutsu_scheduled_run_cue",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["__ScheduledTapPump"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "__SupplyCollector".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                // ADR-0031 Decision B (Slice 2): the emit/done/quit shim
                // `Interpreter::supply_collect_values` taps a Supply with,
                // instead of the old synchronous-replay helpers — see
                // `native_methods::supply_collector`.
                native_methods: [
                    "__mutsu_collector_emit",
                    "__mutsu_collector_done",
                    "__mutsu_collector_quit",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["__SupplyCollector"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "__SupplyQuitForwarder".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                // The `quit =>` callback a `whenever` on a chained on-demand
                // source hands to the inner tap, so an unhandled source quit
                // terminates the enclosing supply block (ADR-0031 Decision A)
                // — see `native_methods::supply_quit_forwarder`.
                native_methods: ["__mutsu_supply_forward_quit"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["__SupplyQuitForwarder"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "__ChannelSupply".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                // The per-tap producer of a `Channel.Supply`, and the closer
                // that detaches the tap -- see `native_methods::channel_supply`.
                native_methods: [
                    "__mutsu_channel_supply_start",
                    "__mutsu_channel_supply_close",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["__ChannelSupply"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "__SupplyDerive".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                // The producer and forwarders of a `Supply.grep`/`.map` over an
                // on-demand source — see `native_methods::supply_derive`.
                native_methods: [
                    "__mutsu_derive_start",
                    "__mutsu_derive_emit",
                    "__mutsu_derive_done",
                    "__mutsu_derive_quit",
                    "__mutsu_derive_close",
                    "__mutsu_promise_emit",
                    "__mutsu_promise_done",
                    "__mutsu_promise_quit",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["__SupplyDerive"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Scheduler".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                // `Scheduler` is a composable role (`class Test::Scheduler does
                // Scheduler {...}`) with NO native implementation of its own --
                // only the concrete schedulers below have one, and each lists
                // `cue` itself. Claiming a native `cue` here made every user
                // class that composes the role look native-backed, so its own
                // `method cue` was bypassed and dispatch died with
                // "No native method 'cue' on 'MyScheduler'".
                native_methods: HashSet::new(),
                mro: sym_mro(&["Scheduler"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "ThreadPoolScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                native_methods: ["cue", "uncaught_handler", "loads", "max_threads", "usage"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["ThreadPoolScheduler"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "CurrentThreadScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                native_methods: ["cue", "uncaught_handler", "loads"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["CurrentThreadScheduler"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "FakeScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                native_methods: ["cue", "progress-by", "time"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["FakeScheduler", "Scheduler"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Cancellation".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["cancel", "cancelled"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Cancellation"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Lock".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                // `new` is Lock's own constructor in rakudo (#9340).
                native_methods: ["new", "protect", "lock", "unlock", "condition"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Lock"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Lock::Async".to_string(),
            ClassDef {
                parents: vec!["Lock".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Lock::Async", "Lock"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Lock::Soft".to_string(),
            ClassDef {
                parents: vec!["Lock".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Lock::Soft", "Lock"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Lock::ConditionVariable".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["wait", "signal", "signal_all"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Lock::ConditionVariable"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Semaphore".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["acquire", "try_acquire", "release"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Semaphore"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "IO::Path".to_string(),
            ClassDef {
                // ADR-0051 P1: `IO::Path` genuinely IS `Cool` in raku
                // (`IO::Path.^mro` is `IO::Path, Cool, Any, Mu`) -- the previous
                // `parents: vec![]` / `mro: sym_mro(&["IO::Path"])` made `.^mro`
                // dead-end at itself (worse, `class_mro_readonly` returns the
                // cached `mro` field directly for a registered class, so leaving
                // it non-empty here permanently short-circuited recomputation).
                // Declaring the real parent and leaving `mro` empty lets
                // `Registry::compute_class_mro`'s ordinary linearization compute
                // and cache the correct `[IO::Path, Cool, Any, Mu]` chain the
                // same way every other class gets its MRO.
                parents: vec!["Cool".to_string()],
                attributes: Vec::new(),
                // `new` is IO::Path's own constructor in rakudo (#9340).
                native_methods: [
                    "new",
                    "Str",
                    "gist",
                    "raku",
                    "perl",
                    "IO",
                    "Numeric",
                    "Real",
                    "Int",
                    "Rat",
                    "Num",
                    "FatRat",
                    "basename",
                    "dirname",
                    "cleanup",
                    "parts",
                    "parent",
                    "sibling",
                    "child",
                    "add",
                    "extension",
                    "absolute",
                    "relative",
                    "resolve",
                    "volume",
                    "is-absolute",
                    "is-relative",
                    "e",
                    "f",
                    "d",
                    "l",
                    "r",
                    "w",
                    "x",
                    "rw",
                    "rwx",
                    "mode",
                    "s",
                    "z",
                    "created",
                    "modified",
                    "accessed",
                    "changed",
                    "lines",
                    "words",
                    "slurp",
                    "open",
                    "copy",
                    "rename",
                    "move",
                    "chmod",
                    "mkdir",
                    "rmdir",
                    "dir",
                    "spurt",
                    "unlink",
                    "symlink",
                    "starts-with",
                    "watch",
                    "succ",
                    "pred",
                    "CWD",
                    "SPEC",
                    "link",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                // Left empty deliberately (ADR-0051 P1) -- see the `parents`
                // comment above. `Registry::compute_class_mro` computes and
                // caches `[IO::Path, Cool, Any, Mu]` here on first use.
                mro: [].into(),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "IO::Handle".to_string(),
            ClassDef {
                parents: Vec::new(),
                // Rakudo seeds $!path with the IO type object. Keeping this as
                // an attribute default also covers IO::Handle.bless and derived
                // classes, instead of only patching the rendered gist.
                attributes: vec![ClassAttributeDef {
                    name: "path".to_string(),
                    is_public: true,
                    default: Some(crate::opcode::DeclTraitArg::Ast(Box::new(Expr::Literal(
                        Value::package(Symbol::intern("IO")),
                    )))),
                    captured_env: None,
                    captured_unit: None,
                    declaring_package: None,
                    is_rw: false,
                    is_required: None,
                    sigil: '$',
                    type_constraint: None,
                    where_constraint: None,
                    declared_shape: None,
                    source_line: None,
                    source_file: None,
                    default_is_seed: false,
                }],
                native_methods: [
                    "path",
                    "IO",
                    "Str",
                    "gist",
                    "DESTROY",
                    "close",
                    "get",
                    "getc",
                    "readchars",
                    "lines",
                    "words",
                    "read",
                    "write",
                    "print",
                    "say",
                    "put",
                    "flush",
                    "seek",
                    "tell",
                    "eof",
                    "encoding",
                    "opened",
                    "slurp",
                    // Deprecated Rakudo alias for `.slurp` from the current
                    // position (META6's `multi method new(IO::Handle :$file!)`).
                    "slurp-rest",
                    "out-buffer",
                    "Supply",
                    "open",
                    "nl-out",
                    "nl-in",
                    "print-nl",
                    "native-descriptor",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["IO::Handle"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "IO::CatHandle".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: [
                    "get",
                    "getc",
                    "lines",
                    "words",
                    "comb",
                    "split",
                    "slurp",
                    "readchars",
                    "read",
                    "next-handle",
                    "open",
                    "close",
                    "DESTROY",
                    "eof",
                    "opened",
                    "chomp",
                    "nl-in",
                    "encoding",
                    "on-switch",
                    "path",
                    "IO",
                    "handles",
                    "Str",
                    "gist",
                    "Supply",
                    "native-descriptor",
                    "seek",
                    "tell",
                    "t",
                    "lock",
                    "unlock",
                    "raku",
                    "perl",
                    // Write/low-level methods are NYI on a read-only cat; the
                    // native handler raises X::NYI for them (rakudo does too).
                    "flush",
                    "out-buffer",
                    "print",
                    "printf",
                    "print-nl",
                    "put",
                    "say",
                    "write",
                    "WRITE",
                    "READ",
                    "EOF",
                    // Obsolete: `.slurp-rest` -> `.slurp`.
                    "slurp-rest",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["IO::CatHandle"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Backtrace".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Backtrace"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "CompUnit::Repository::FileSystem".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["CompUnit::Repository::FileSystem"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "IO::Pipe".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["slurp", "slurp-rest", "Str", "gist", "print", "close"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["IO::Pipe"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "IO::Socket::INET".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: [
                    "close",
                    "getpeername",
                    "accept",
                    "localport",
                    "print",
                    "say",
                    "put",
                    "write",
                    "recv",
                    "read",
                    "get",
                    "lines",
                    "nl-in",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["IO::Socket::INET"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "IO::Socket::Async".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: [
                    "close",
                    "write",
                    "print",
                    "Supply",
                    "socket-port",
                    "peer-port",
                    "socket-host",
                    "peer-host",
                    "print-to",
                    "write-to",
                    "native-descriptor",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["IO::Socket::Async"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "IO::Socket::Async::Listener".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["tap", "act"].iter().map(|s| s.to_string()).collect(),
                // Real raku's `IO::Socket::Async.listen(...)` literally IS a
                // `Supply` (built from a `supply { ... }` block in CORE.setting) --
                // there is no separate "Listener" type. mutsu's listener is a
                // bespoke native object with its own `tap`/`act` handler
                // (`native_socket_async_listener`, dispatched by exact class name
                // in `native_methods/mod.rs`'s IMMUTABLE table, which already
                // special-cases this class name so it is unaffected by the MRO).
                // Adding `Supply` here makes `$listener ~~ Supply` true, matching
                // raku -- needed by consumers that type-check the return value of
                // `.listen()` before tapping it (e.g. IO::Socket::Async::SSL's
                // `!server-setup`, which fell through to treating the listener
                // itself as a single accepted connection when the smartmatch
                // failed). See `dispatch_native_instance_method_mut`'s hardcoded
                // class list in `native_methods/mod.rs`, which must also list this
                // class name explicitly -- its MRO-walk fallback would otherwise
                // now match `Supply` and route `tap`/`act` to the wrong (generic,
                // non-functional) mutable-Supply handler instead of falling
                // through to this class's real immutable one.
                mro: sym_mro(&["IO::Socket::Async::Listener", "Supply"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Distro".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: [
                    "name",
                    "auth",
                    "desc",
                    "release",
                    "path-sep",
                    "is-win",
                    "version",
                    "signature",
                    "gist",
                    "Str",
                    "raku",
                    "perl",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Distro"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        // The compiler-identity object is `Raku` (`$*RAKU.^name` is `Raku`, and
        // `Raku.^name` is `Raku` in rakudo). `Perl` is the pre-rename spelling,
        // which rakudo still keeps as a distinct type name (`Perl.^name` is
        // `Perl`) and still answers `$*PERL` with; both share one native method
        // table, and `make_perl_instance` builds the `Raku` one.
        for identity_name in ["Raku", "Perl"] {
            classes.insert(
                identity_name.to_string(),
                ClassDef {
                    parents: Vec::new(),
                    attributes: Vec::new(),
                    native_methods: [
                        "DISTROnames",
                        "KERNELnames",
                        "compiler",
                        "backend",
                        "name",
                        "auth",
                        "version",
                        "signature",
                        "desc",
                        "gist",
                        "raku",
                        "Str",
                        "release",
                        "codename",
                        "id",
                    ]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                    mro: sym_mro(&[identity_name]),
                    attribute_types: HashMap::new(),
                    attribute_smileys: HashMap::new(),
                    attribute_built: HashMap::new(),
                    embedded_attributes: HashSet::new(),
                    wildcard_handles: Vec::new(),
                    alias_attributes: HashSet::new(),
                    class_level_attrs: ValueMap::default(),
                },
            );
        }
        classes.insert(
            "Kernel".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: [
                    "name",
                    "auth",
                    "version",
                    "signature",
                    "desc",
                    "release",
                    "hardware",
                    "arch",
                    "bits",
                    "hostname",
                    "signals",
                    "signal",
                    "cpu-cores",
                    "free-memory",
                    "total-memory",
                    "endian",
                    "gist",
                    "raku",
                    "Str",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Kernel"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "VM".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: [
                    "name",
                    "auth",
                    "version",
                    "osname",
                    "precomp-ext",
                    "precomp-target",
                    "request-garbage-collection",
                    "gist",
                    "Str",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["VM"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Compiler".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: [
                    "name",
                    "auth",
                    "version",
                    "signature",
                    "desc",
                    "gist",
                    "raku",
                    "Str",
                    "release",
                    "codename",
                    "id",
                    "backend",
                    "verbose-config",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Compiler"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Encoding::Builtin".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["name", "alternative-names", "encoder", "decoder"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: sym_mro(&["Encoding::Builtin"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Encoding::Encoder".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["encode-chars"].iter().map(|s| s.to_string()).collect(),
                mro: sym_mro(&["Encoding::Encoder"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Encoding::Decoder::Builtin".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: [
                    "add-bytes",
                    "consume-all-chars",
                    "consume-available-chars",
                    "consume-exactly-chars",
                    "consume-line-chars",
                    "consume-exactly-bytes",
                    "bytes-available",
                    "is-empty",
                    "set-line-separators",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: sym_mro(&["Encoding::Decoder::Builtin"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "Encoding::Registry".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                native_methods: ["find", "register"].iter().map(|s| s.to_string()).collect(),
                mro: sym_mro(&["Encoding::Registry"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        // Pod object model (rakudo `src/core.c/Pod.pm6`). The declared
        // attribute list is what `.raku`/`.gist` enumerate, so it must carry
        // the same names, sigils and ORDER rakudo emits -- e.g.
        // `Pod::Heading.new(level => 1, config => {}, contents => [...])`.
        // `collect_class_attributes_display_order` walks the MRO forwards
        // (most-derived first) and keeps a redeclared name at its most-derived
        // position, so each subclass listing its own attributes before
        // `config`/`contents` reproduces rakudo's order exactly.
        // A `%`/`@` sigil also keeps the rendered value un-itemized
        // (`config => {}`, not `config => ${}`).
        /// `(class name, parent names, declared `(attribute, sigil)` pairs)`.
        type PodClassDecl = (
            &'static str,
            &'static [&'static str],
            &'static [(&'static str, char)],
        );
        const POD_CLASSES: &[PodClassDecl] = &[
            ("Pod::Block", &[], &[("config", '%'), ("contents", '@')]),
            (
                "Pod::Block::Code",
                &["Pod::Block"],
                &[("config", '%'), ("contents", '@')],
            ),
            (
                "Pod::Block::Comment",
                &["Pod::Block"],
                &[("config", '%'), ("contents", '@')],
            ),
            // `leading`/`trailing` are `$`-sigil here because that is what the
            // accessors return in rakudo too (`.leading` is the joined text,
            // not the private `@!leading` buffer), and `contents` likewise
            // holds the newline-joined doc text the `.WHY` consumers compare
            // against (roast S26 `is $thing.WHY.contents, "$leading\n$trailing"`).
            (
                "Pod::Block::Declarator",
                &["Pod::Block"],
                &[
                    ("WHEREFORE", '$'),
                    ("leading", '$'),
                    ("trailing", '$'),
                    ("config", '%'),
                    ("contents", '$'),
                ],
            ),
            (
                "Pod::Block::Named",
                &["Pod::Block"],
                &[("name", '$'), ("config", '%'), ("contents", '@')],
            ),
            (
                "Pod::Block::Para",
                &["Pod::Block"],
                &[("config", '%'), ("contents", '@')],
            ),
            (
                "Pod::Block::Table",
                &["Pod::Block"],
                &[
                    ("caption", '$'),
                    ("headers", '@'),
                    ("config", '%'),
                    ("contents", '@'),
                ],
            ),
            ("Pod::Config", &[], &[("type", '$'), ("config", '%')]),
            (
                "Pod::Defn",
                &["Pod::Block"],
                &[("term", '$'), ("config", '%'), ("contents", '@')],
            ),
            (
                "Pod::FormattingCode",
                &["Pod::Block"],
                &[
                    ("type", '$'),
                    ("meta", '@'),
                    ("config", '%'),
                    ("contents", '@'),
                ],
            ),
            (
                "Pod::Heading",
                &["Pod::Block"],
                &[("level", '$'), ("config", '%'), ("contents", '@')],
            ),
            (
                "Pod::Item",
                &["Pod::Block"],
                &[("level", '$'), ("config", '%'), ("contents", '@')],
            ),
        ];
        for (name, parents, attrs) in POD_CLASSES {
            let mut mro_names: Vec<&str> = vec![name];
            mro_names.extend_from_slice(parents);
            classes.insert(
                (*name).to_string(),
                ClassDef {
                    parents: parents.iter().map(|p| (*p).to_string()).collect(),
                    attributes: attrs
                        .iter()
                        .map(|(attr, sigil)| ClassAttributeDef {
                            name: (*attr).to_string(),
                            is_public: true,
                            default: None,
                            captured_env: None,
                            captured_unit: None,
                            declaring_package: None,
                            is_rw: false,
                            is_required: None,
                            sigil: *sigil,
                            type_constraint: None,
                            where_constraint: None,
                            declared_shape: None,
                            source_line: None,
                            source_file: None,
                            default_is_seed: false,
                        })
                        .collect(),
                    native_methods: HashSet::new(),
                    mro: sym_mro(&mro_names),
                    attribute_types: HashMap::new(),
                    attribute_smileys: HashMap::new(),
                    attribute_built: HashMap::new(),
                    embedded_attributes: HashSet::new(),
                    wildcard_handles: Vec::new(),
                    alias_attributes: HashSet::new(),
                    class_level_attrs: ValueMap::default(),
                },
            );
        }
        classes.insert(
            "Exception".to_string(),
            ClassDef {
                parents: vec![],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::AdHoc".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::AdHoc", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::TypeCheck".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::TypeCheck", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::TypeCheck::Binding".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::TypeCheck::Binding", "X::TypeCheck", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::TypeCheck::Binding::Parameter".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck::Binding".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&[
                    "X::TypeCheck::Binding::Parameter",
                    "X::TypeCheck::Binding",
                    "X::TypeCheck",
                    "Exception",
                ]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::Parameter".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::Parameter", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::Parameter::InvalidConcreteness".to_string(),
            ClassDef {
                // Despite the name, this does NOT inherit `X::Parameter` in
                // rakudo (`.^parents(:local)` is `Exception` directly;
                // `X::Parameter` is a bare package, not a class -- ADR-0029).
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::Parameter::InvalidConcreteness", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::Supply::Combinator".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::Supply::Combinator", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::TypeCheck::Argument".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::TypeCheck::Argument", "X::TypeCheck", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::TypeCheck::Assignment".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::TypeCheck::Assignment", "X::TypeCheck", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::Numeric::Real".to_string(),
            ClassDef {
                // Real superclass is `X::Numeric::CannotConvert`, not
                // `Exception` directly (ADR-0029, verified against raku).
                parents: vec!["X::Numeric::CannotConvert".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::Numeric::Real", "X::Numeric::CannotConvert", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::TypeCheck::Return".to_string(),
            ClassDef {
                // Real superclass is `X::TypeCheck` (ADR-0029, verified
                // against raku) -- unlike `X::TypeCheck::Argument` /
                // `::Assignment` / `::Binding` above, this one was missing
                // the intermediate ancestor.
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::TypeCheck::Return", "X::TypeCheck", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );
        classes.insert(
            "X::Coerce::Impossible".to_string(),
            ClassDef {
                // Real superclass is `X::Coerce` (ADR-0029, verified against
                // raku; `X::Coerce` itself is registered below via
                // `register_x`).
                parents: vec!["X::Coerce".to_string()],
                attributes: Vec::new(),
                native_methods: HashSet::new(),
                mro: sym_mro(&["X::Coerce::Impossible", "X::Coerce", "Exception"]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            },
        );

        // Register additional X:: exception classes using a helper closure
        // to reduce boilerplate. `parent` is a real superclass and drives
        // `parents`/`mro`, exactly as before. `does` (ADR-0029) is role
        // membership, NEVER folded into `parents`/`mro` -- rakudo's `X::`
        // role vocabulary (X::Comp, X::Syntax, ...) is composed, not
        // inherited. Recorded here and written into the composed-role
        // registries below, once `registry` (and its `role_parents`, for
        // transitive flattening) exists.
        let mut register_x_does: Vec<(String, Vec<String>)> = Vec::new();
        let mut register_x = |name: &str, parent: &str, does: &[&str]| {
            // Walk up through existing classes to build full MRO, borrowing
            // each name from the class table rather than cloning it per step
            // (this closure runs for every built-in `X::` class at startup).
            let mut mro: Vec<&str> = vec![name];
            let mut cur = parent;
            loop {
                mro.push(cur);
                if let Some(cls) = classes.get(cur)
                    && let Some(p) = cls.parents.first()
                    && p != cur
                {
                    cur = p;
                    continue;
                }
                break;
            }
            if !mro.contains(&"Exception") {
                mro.push("Exception");
            }
            let mro: std::sync::Arc<[Symbol]> = mro.iter().map(|s| Symbol::intern(s)).collect();
            classes.insert(
                name.to_string(),
                ClassDef {
                    parents: vec![parent.to_string()],
                    attributes: Vec::new(),
                    native_methods: HashSet::new(),
                    mro,
                    attribute_types: HashMap::new(),
                    attribute_smileys: HashMap::new(),
                    attribute_built: HashMap::new(),
                    embedded_attributes: HashSet::new(),
                    wildcard_handles: Vec::new(),
                    alias_attributes: HashSet::new(),
                    class_level_attrs: ValueMap::default(),
                },
            );
            if !does.is_empty() {
                register_x_does.push((
                    name.to_string(),
                    does.iter().map(|s| s.to_string()).collect(),
                ));
            }
        };

        // Names raku itself does not recognise as real Exception
        // subtypes (kept for now, unchanged -- out of ADR-0029's scope,
        // see TODO_roast/x-exception-role-membership.tsv's generation
        // script for how this was determined).
        //
        // ADR-0029 residue R4 (dual registration, deliberately kept): `X::Comp`
        // and `X::Syntax` are ALSO seeded as `RoleDef`s below (see the 16-name
        // marker-role loop), so each of these two names is both a `ClassDef`
        // here and a `RoleDef` there. In real rakudo neither is a class
        // (`X::Comp.HOW` is `ParametricRoleGroupHOW`), and mutsu already
        // answers that correctly -- `.HOW.^name` resolves through the role
        // registration, so the `ClassDef` shadow here does not leak into
        // metaobject introspection. It is kept, not deleted, because it is
        // load-bearing for two things: (1) `register_x`'s parent-walk MRO
        // synthesis below resolves `parent` names through `classes`, so
        // `X::TooLateForREPR`'s `parent = "X::Comp"` (R2, immediately below)
        // and `X::Syntax::Signature`'s `parent = "X::Syntax"` both depend on
        // a `ClassDef` existing for the name; (2) removing it without
        // rerouting both dependants would need a second, non-`classes`-backed
        // parent-resolution path in `register_x`, which is more machinery for
        // no observable gain. See
        // exception-class-hierarchy-is-mostly-unregistered (#7545) R4.
        register_x("X::Comp", "Exception", &[]);
        register_x("X::Value", "Exception", &[]);
        register_x("X::Syntax", "X::Comp", &[]);
        register_x("X::Syntax::Signature", "X::Syntax", &[]);
        register_x("X::Role::Composition::Conflict", "Exception", &[]);

        // ADR-0029 residue R2: `X::TooLateForREPR` is rakudo's one
        // "role-as-superclass pun" in this vocabulary -- `X::Comp` is
        // simultaneously a real MRO entry AND a composed role for this single
        // class. Verified against real raku (2026-08-19):
        //   X::TooLateForREPR.^mro             -> (X::TooLateForREPR X::Comp Exception Any Mu)
        //   X::TooLateForREPR.^parents(:local) -> (X::Comp)
        //   X::TooLateForREPR.^roles           -> (X::Comp)
        // This is the one documented, data-verified exception to "a marker
        // role name never appears in a class's `.^mro`" -- see
        // t/exception-role-membership.t and
        // exception-class-hierarchy-is-mostly-unregistered (#7545) R2.
        // It is expressible here (and not elsewhere) only because `X::Comp`
        // is dual-registered as a `ClassDef` immediately above (R4), which
        // `register_x`'s parent walk resolves.
        register_x("X::TooLateForREPR", "X::Comp", &["X::Comp"]);

        // ADR-0029 Slice 3: every row below is mechanically generated from
        // TODO_roast/x-exception-role-membership.tsv (real raku .^mro /
        // .^roles(:!transitive) output, captured by
        // scripts/adr0029-capture-x-exception-data.py) and sorted by real
        // mro depth so a parent always registers before its child. Do not
        // hand-edit individual rows -- regenerate from the TSV instead.
        register_x("X::Adverb", "Exception", &[]);
        register_x("X::Anon::Augment", "Exception", &["X::Comp"]);
        register_x("X::Anon::Multi", "Exception", &["X::Comp"]);
        register_x("X::ArrayShapeMismatch", "Exception", &[]);
        register_x("X::Assignment::RO", "Exception", &[]);
        register_x("X::Assignment::RO::Comp", "Exception", &["X::Comp"]);
        register_x("X::Assignment::ToShaped", "Exception", &[]);
        register_x("X::Attribute::NoPackage", "Exception", &["X::Comp"]);
        register_x("X::Attribute::Package", "Exception", &["X::Comp"]);
        register_x("X::Attribute::Required", "Exception", &["X::MOP"]);
        register_x("X::Attribute::Scope::Package", "Exception", &["X::Comp"]);
        register_x("X::Augment::NoSuchType", "Exception", &["X::Comp"]);
        register_x(
            "X::Backslash::NonVariableDollar",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Backslash::UnrecognizedSequence",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Bind", "Exception", &[]);
        register_x("X::Bind::NativeType", "Exception", &["X::Comp"]);
        register_x("X::Bind::Slice", "Exception", &[]);
        register_x("X::Buf::AsStr", "Exception", &[]);
        register_x("X::Buf::Pack", "Exception", &[]);
        register_x("X::Buf::Pack::NonASCII", "Exception", &[]);
        register_x("X::Cannot::Capture", "Exception", &[]);
        register_x("X::Cannot::Empty", "Exception", &[]);
        register_x("X::Cannot::Lazy", "Exception", &[]);
        register_x("X::Cannot::Map", "Exception", &[]);
        register_x("X::Cannot::New", "Exception", &[]);
        register_x("X::Channel::ReceiveOnClosed", "Exception", &[]);
        register_x("X::Channel::SendOnClosed", "Exception", &[]);
        register_x("X::Coerce", "Exception", &[]);
        register_x("X::Comp::BeginTime", "Exception", &["X::Comp"]);
        register_x("X::Comp::FailGoal", "Exception", &["X::Comp"]);
        register_x("X::Comp::Group", "Exception", &[]);
        register_x("X::Comp::WheneverOutOfScope", "Exception", &["X::Comp"]);
        register_x("X::CompUnit::UnsatisfiedDependency", "Exception", &[]);
        register_x("X::Composition::NotComposable", "Exception", &["X::Comp"]);
        register_x("X::Constructor::BadType", "Exception", &["X::BadType"]);
        register_x("X::Constructor::Positional", "Exception", &[]);
        register_x("X::ControlFlow", "Exception", &[]);
        register_x(
            "X::DateTime::InvalidDeltaUnit",
            "Exception",
            &["X::Temporal"],
        );
        register_x("X::DateTime::TimezoneClash", "Exception", &["X::Temporal"]);
        register_x("X::Declaration::OurScopeInRole", "Exception", &["X::Comp"]);
        register_x("X::Declaration::Scope", "Exception", &["X::Comp"]);
        register_x("X::Delete", "Exception", &[]);
        register_x("X::Does::TypeObject", "Exception", &[]);
        register_x("X::Dynamic::NotFound", "Exception", &[]);
        register_x("X::Dynamic::Package", "Exception", &["X::Comp"]);
        register_x("X::Dynamic::Postdeclaration", "Exception", &["X::Comp"]);
        register_x("X::EXPORTHOW::Conflict", "Exception", &["X::Comp"]);
        register_x("X::EXPORTHOW::InvalidDirective", "Exception", &["X::Comp"]);
        register_x(
            "X::EXPORTHOW::NothingToSupersede",
            "Exception",
            &["X::Comp"],
        );
        register_x(
            "X::Encoding::AlreadyRegistered",
            "Exception",
            &["X::Encoding"],
        );
        register_x("X::Encoding::Unknown", "Exception", &["X::Encoding"]);
        register_x("X::Enum::NoValue", "Exception", &[]);
        register_x("X::Eval::NoSuchLang", "Exception", &[]);
        register_x("X::Exhausted", "Exception", &[]);
        register_x("X::Experimental", "Exception", &["X::Comp"]);
        register_x("X::Export::NameClash", "Exception", &["X::Comp"]);
        register_x("X::Hash::Store::OddNumber", "Exception", &[]);
        register_x("X::HyperOp::Infinite", "Exception", &[]);
        register_x("X::HyperOp::NonDWIM", "Exception", &[]);
        register_x("X::HyperWhatever::Multiple", "Exception", &[]);
        register_x("X::IO::BinaryAndEncoding", "Exception", &["X::IO"]);
        register_x("X::IO::BinaryMode", "Exception", &["X::IO"]);
        register_x("X::IO::Chdir", "Exception", &["X::IO"]);
        register_x("X::IO::Chmod", "Exception", &["X::IO"]);
        register_x("X::IO::Chown", "Exception", &["X::IO"]);
        register_x("X::IO::Closed", "Exception", &["X::IO"]);
        register_x("X::IO::Copy", "Exception", &["X::IO"]);
        register_x("X::IO::Cwd", "Exception", &["X::IO"]);
        register_x("X::IO::Dir", "Exception", &["X::IO"]);
        register_x("X::IO::Directory", "Exception", &["X::IO"]);
        register_x("X::IO::DoesNotExist", "Exception", &["X::IO"]);
        register_x("X::IO::Flush", "Exception", &["X::IO"]);
        register_x("X::IO::Link", "Exception", &["X::IO"]);
        register_x("X::IO::Lock", "Exception", &["X::IO"]);
        register_x("X::IO::Mkdir", "Exception", &["X::IO"]);
        register_x("X::IO::Move", "Exception", &["X::IO"]);
        register_x("X::IO::NotAChild", "Exception", &["X::IO"]);
        register_x("X::IO::NotAFile", "Exception", &["X::IO"]);
        register_x("X::IO::Null", "Exception", &["X::IO"]);
        register_x("X::IO::Rename", "Exception", &["X::IO"]);
        register_x("X::IO::Resolve", "Exception", &["X::IO"]);
        register_x("X::IO::Rmdir", "Exception", &["X::IO"]);
        register_x("X::IO::Symlink", "Exception", &["X::IO"]);
        register_x("X::IO::Unknown", "Exception", &["X::IO"]);
        register_x("X::IO::Unlink", "Exception", &["X::IO"]);
        register_x("X::IllegalDimensionInShape", "Exception", &[]);
        register_x("X::IllegalOnFixedDimensionArray", "Exception", &[]);
        register_x("X::Immutable", "Exception", &[]);
        register_x("X::Import::MissingSymbols", "Exception", &[]);
        register_x("X::Import::NoSuchTag", "Exception", &[]);
        register_x("X::Import::OnlystarProto", "Exception", &["X::Comp"]);
        register_x("X::Import::Positional", "Exception", &[]);
        register_x("X::Import::Redeclaration", "Exception", &["X::Comp"]);
        register_x("X::Inheritance::NotComposed", "Exception", &["X::MOP"]);
        register_x("X::Inheritance::SelfInherit", "Exception", &[]);
        register_x("X::Inheritance::UnknownParent", "Exception", &[]);
        register_x("X::Inheritance::Unsupported", "Exception", &["X::Comp"]);
        register_x("X::Invalid::ComputedValue", "Exception", &[]);
        register_x("X::Invalid::Value", "Exception", &[]);
        register_x("X::InvalidCodepoint", "Exception", &[]);
        register_x("X::InvalidType", "Exception", &["X::Comp"]);
        register_x("X::InvalidTypeSmiley", "Exception", &["X::Comp"]);
        register_x("X::Ism::Unknown", "Exception", &[]);
        register_x("X::Item", "Exception", &[]);
        register_x("X::Language::IncompatRevisions", "Exception", &[]);
        register_x("X::Language::ModRequired", "Exception", &[]);
        register_x("X::Language::TooLate", "Exception", &[]);
        register_x("X::Language::Unsupported", "Exception", &[]);
        register_x("X::LibEmpty", "Exception", &["X::Comp"]);
        register_x("X::LibNone", "Exception", &["X::Comp"]);
        register_x("X::Localizer::NoContainer", "Exception", &[]);
        register_x("X::Lock::Async::NotLocked", "Exception", &[]);
        register_x("X::Lock::ConditionVariable::Duplicate", "Exception", &[]);
        register_x("X::Lock::ConditionVariable::New", "Exception", &[]);
        register_x("X::Lock::ConditionVariable::NoMutex", "Exception", &[]);
        register_x("X::Lock::ConditionVariable::WrongThread", "Exception", &[]);
        register_x("X::Lock::Unlock::NoMutex", "Exception", &[]);
        register_x("X::Lock::Unlock::WrongThread", "Exception", &[]);
        register_x("X::Make::MatchRequired", "Exception", &[]);
        register_x("X::Match::Bool", "Exception", &[]);
        register_x("X::Method::Duplicate", "Exception", &[]);
        register_x("X::Method::InvalidQualifier", "Exception", &[]);
        register_x("X::Method::NotFound", "Exception", &[]);
        register_x("X::Method::Private::Permission", "Exception", &["X::Comp"]);
        register_x("X::Method::Private::Unqualified", "Exception", &["X::Comp"]);
        register_x("X::Mixin::NotComposable", "Exception", &[]);
        register_x("X::Multi::Ambiguous", "Exception", &[]);
        register_x("X::Multi::NoMatch", "Exception", &[]);
        register_x("X::MultipleTypeSmiley", "Exception", &["X::Comp"]);
        register_x("X::MustBeParametric", "Exception", &[]);
        register_x("X::NQP::NotFound", "Exception", &[]);
        register_x("X::NYI", "Exception", &[]);
        register_x("X::NYI::BigInt", "Exception", &[]);
        register_x("X::NoCoreRevision", "Exception", &[]);
        register_x("X::NoDispatcher", "Exception", &[]);
        register_x("X::NoSuchSymbol", "Exception", &[]);
        register_x("X::NoZeroArgMeaning", "Exception", &[]);
        register_x(
            "X::Nominalizable::NoKind",
            "Exception",
            &["X::Nominalizable"],
        );
        register_x(
            "X::Nominalizable::NoWrappee",
            "Exception",
            &["X::Nominalizable"],
        );
        register_x("X::NotEnoughDimensions", "Exception", &[]);
        register_x("X::NotFoundInRepository", "Exception", &[]);
        register_x("X::NotParametric", "Exception", &[]);
        register_x("X::NotSingleGrapheme", "Exception", &[]);
        register_x("X::Numeric::CannotConvert", "Exception", &[]);
        register_x("X::Numeric::Confused", "Exception", &[]);
        register_x("X::Numeric::DivideByZero", "Exception", &[]);
        register_x("X::Numeric::Overflow", "Exception", &[]);
        register_x("X::Numeric::Underflow", "Exception", &[]);
        register_x("X::Numeric::Uninitialized", "Exception", &[]);
        register_x("X::Obsolete", "Exception", &["X::Comp"]);
        register_x("X::OutOfRange", "Exception", &[]);
        register_x("X::Package::SameNameAsEnclosing", "Exception", &["X::Comp"]);
        register_x("X::Package::Stubbed", "Exception", &["X::Comp"]);
        register_x("X::Package::UseLib", "Exception", &["X::Comp"]);
        register_x("X::Pairup::OddNumber", "Exception", &[]);
        register_x("X::Parameter::AfterDefault", "Exception", &["X::Syntax"]);
        register_x(
            "X::Parameter::BadType",
            "Exception",
            &["X::BadType", "X::Comp"],
        );
        register_x("X::Parameter::Default", "Exception", &["X::Comp"]);
        register_x(
            "X::Parameter::Default::TypeCheck",
            "Exception",
            &["X::Comp"],
        );
        register_x("X::Parameter::InvalidType", "Exception", &["X::Comp"]);
        register_x(
            "X::Parameter::MultipleTypeConstraints",
            "Exception",
            &["X::Comp"],
        );
        register_x(
            "X::Parameter::Named::SubsetTypeWithoutDefault",
            "Exception",
            &["X::Comp"],
        );
        register_x("X::Parameter::Placeholder", "Exception", &["X::Comp"]);
        register_x("X::Parameter::RW", "Exception", &[]);
        register_x("X::Parameter::Twigil", "Exception", &["X::Comp"]);
        register_x("X::Parameter::TypedSlurpy", "Exception", &["X::Comp"]);
        register_x("X::Parameter::WrongOrder", "Exception", &["X::Comp"]);
        register_x("X::ParametricConstant", "Exception", &[]);
        register_x("X::Phaser::Multiple", "Exception", &["X::Comp"]);
        register_x("X::Phaser::PrePost", "Exception", &[]);
        register_x("X::PhaserExceptions", "Exception", &[]);
        register_x("X::Placeholder::Block", "Exception", &["X::Comp"]);
        register_x("X::Placeholder::NonPlaceholder", "Exception", &["X::Comp"]);
        register_x("X::PoisonedAlias", "Exception", &["X::Comp"]);
        register_x("X::Pragma::CannotPrecomp", "Exception", &[]);
        register_x("X::Pragma::CannotWhat", "Exception", &[]);
        register_x("X::Pragma::MustOneOf", "Exception", &[]);
        register_x("X::Pragma::NoArgs", "Exception", &[]);
        register_x("X::Pragma::OnlyOne", "Exception", &[]);
        register_x("X::Pragma::Unknown", "Exception", &[]);
        register_x("X::Pragma::UnknownArg", "Exception", &[]);
        register_x(
            "X::Proc::Async::AlreadyStarted",
            "Exception",
            &["X::Proc::Async"],
        );
        register_x(
            "X::Proc::Async::BindOrUse",
            "Exception",
            &["X::Proc::Async"],
        );
        register_x(
            "X::Proc::Async::CharsOrBytes",
            "Exception",
            &["X::Proc::Async"],
        );
        register_x(
            "X::Proc::Async::MissingColsRows",
            "Exception",
            &["X::Proc::Async"],
        );
        register_x(
            "X::Proc::Async::MustBeStarted",
            "Exception",
            &["X::Proc::Async"],
        );
        register_x(
            "X::Proc::Async::OpenForWriting",
            "Exception",
            &["X::Proc::Async"],
        );
        register_x(
            "X::Proc::Async::SupplyOrStd",
            "Exception",
            &["X::Proc::Async"],
        );
        register_x(
            "X::Proc::Async::TapBeforeSpawn",
            "Exception",
            &["X::Proc::Async"],
        );
        register_x("X::Proc::Unsuccessful", "Exception", &[]);
        register_x("X::Promise::CauseOnlyValidOnBroken", "Exception", &[]);
        register_x("X::Promise::Combinator", "Exception", &[]);
        register_x("X::Promise::Resolved", "Exception", &[]);
        register_x("X::Promise::Vowed", "Exception", &[]);
        register_x("X::PseudoPackage::InDeclaration", "Exception", &["X::Comp"]);
        register_x("X::QuoteWords::Missing::Closer", "Exception", &["X::Comp"]);
        register_x("X::REPL::InvalidEnvironment", "Exception", &[]);
        register_x("X::Range::CannotIterate", "Exception", &[]);
        register_x("X::Range::Incomparable", "Exception", &[]);
        register_x("X::Range::InvalidArg", "Exception", &[]);
        register_x("X::Range::Rand::InvalidEndpoints", "Exception", &[]);
        register_x("X::Redeclaration", "Exception", &["X::Comp"]);
        register_x("X::Redeclaration::Multi", "Exception", &["X::Comp"]);
        register_x("X::Redeclaration::Outer", "Exception", &["X::Comp"]);
        register_x(
            "X::Role::Attribute::Conflicts",
            "Exception",
            &["X::Role::Attribute"],
        );
        register_x(
            "X::Role::Attribute::Exists",
            "Exception",
            &["X::Role::Attribute"],
        );
        register_x("X::Role::BodyReturn", "Exception", &[]);
        register_x("X::Role::Group::Documenting", "Exception", &[]);
        register_x("X::Role::Initialization", "Exception", &[]);
        register_x("X::Role::Instantiation", "Exception", &["X::Wrapper"]);
        register_x("X::Role::Parametric::NoSuchCandidate", "Exception", &[]);
        register_x(
            "X::Role::Unimplemented::Multi",
            "Exception",
            &["X::RoleApplier::Method", "X::RoleApplier"],
        );
        register_x(
            "X::Role::Unresolved",
            "Exception",
            &["X::RoleApplier::Method", "X::RoleApplier"],
        );
        register_x("X::Routine::Unwrap", "Exception", &[]);
        register_x("X::Scheduler::CueInNaNSeconds", "Exception", &[]);
        register_x("X::SecurityPolicy", "Exception", &[]);
        register_x("X::Seq::Consumed", "Exception", &[]);
        register_x("X::Seq::NotIndexable", "Exception", &[]);
        register_x("X::Sequence::Deduction", "Exception", &[]);
        register_x("X::Sequence::Endpoint", "Exception", &[]);
        register_x("X::Set::Coerce", "Exception", &[]);
        register_x("X::Signature::NameClash", "Exception", &["X::Comp"]);
        register_x("X::Signature::Placeholder", "Exception", &["X::Comp"]);
        register_x("X::Str::InvalidCharName", "Exception", &[]);
        register_x("X::Str::Match::x", "Exception", &[]);
        register_x("X::Str::Numeric", "Exception", &[]);
        register_x("X::Str::Sprintf::Directives::BadType", "Exception", &[]);
        register_x("X::Str::Sprintf::Directives::Count", "Exception", &[]);
        register_x("X::Str::Sprintf::Directives::Unsupported", "Exception", &[]);
        register_x("X::Str::Subst::Adverb", "Exception", &[]);
        register_x("X::Str::Trans::IllegalKey", "Exception", &[]);
        register_x("X::Str::Trans::InvalidArg", "Exception", &[]);
        register_x("X::StubCode", "Exception", &[]);
        register_x("X::Subscript::Negative", "Exception", &[]);
        register_x("X::Supply::Migrate::Needs", "Exception", &[]);
        register_x("X::Supply::New", "Exception", &[]);
        register_x("X::Symbol::Kind", "Exception", &[]);
        register_x(
            "X::Syntax::AddCategorical::TooFewParts",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::AddCategorical::TooManyParts",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Adverb", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::AmbiguousAdverb", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::Argument::MOPMacro", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::Augment::Adverb", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::Augment::Illegal", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Augment::WithoutMonkeyTyping",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::BlockGobbled", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::CannotMeta", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Coercer::TooComplex",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Comment::Embedded", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::ConditionalOperator::PrecedenceTooLoose",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::ConditionalOperator::SecondPartGobbled",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::ConditionalOperator::SecondPartInvalid",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Confused", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Doc::Declarator::MissingDeclarand",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::DuplicatedPrefix", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Extension::Category",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Extension::Null", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Extension::SpecialForm",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Extension::TooComplex",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::InfixInTermPosition",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::KeywordAsFunction", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::Malformed", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::Malformed::Elsif", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::Missing", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::Name::Null", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::NegatedPair", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::NoSelf", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::NonAssociative", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Number::IllegalDecimal",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Number::RadixOutOfRange",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::P5", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::ParentAsHash", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::Perl5Var", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Pod::BeginWithDirective",
            "Exception",
            &["X::Pod", "X::Syntax"],
        );
        register_x(
            "X::Syntax::Pod::BeginWithoutEnd",
            "Exception",
            &["X::Pod", "X::Syntax"],
        );
        register_x(
            "X::Syntax::Pod::BeginWithoutIdentifier",
            "Exception",
            &["X::Pod", "X::Syntax"],
        );
        register_x(
            "X::Syntax::Pod::DeclaratorLeading",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Pod::DeclaratorTrailing",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Regex::Adverb", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Regex::Alias::LongName",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Regex::InsignificantWhitespace",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Regex::MalformedRange",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Regex::NonQuantifiable",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Regex::NullRegex", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Regex::QuantifierValue",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Regex::SolitaryBacktrackControl",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Regex::SolitaryQuantifier",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Regex::SpacesInBareRange",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Regex::UnrecognizedMetachar",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Regex::UnrecognizedModifier",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Regex::Unspace", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Regex::Unterminated",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Reserved", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Self::WithoutObject",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Signature::InvocantMarker",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Signature::InvocantNotAllowed",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Term::MissingInitializer",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Type::Adverb", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::UnlessElse", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Variable::BadType",
            "Exception",
            &["X::BadType", "X::Comp"],
        );
        register_x(
            "X::Syntax::Variable::ConflictingTypes",
            "Exception",
            &["X::Comp"],
        );
        register_x(
            "X::Syntax::Variable::IndirectDeclaration",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Variable::Initializer",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Variable::Match", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Variable::MissingInitializer",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Variable::Numeric", "Exception", &["X::Syntax"]);
        register_x(
            "X::Syntax::Variable::SignatureAssignment",
            "Exception",
            &["X::Syntax"],
        );
        register_x(
            "X::Syntax::Variable::SignatureWithoutInitializer",
            "Exception",
            &["X::Syntax"],
        );
        register_x("X::Syntax::Variable::Twigil", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::VirtualCall", "Exception", &["X::Syntax"]);
        register_x("X::Syntax::WithoutElse", "Exception", &["X::Syntax"]);
        register_x("X::Temporal::InvalidFormat", "Exception", &["X::Temporal"]);
        register_x("X::TooManyDimensions", "Exception", &[]);
        register_x("X::Trait::Invalid", "Exception", &["X::Trait"]);
        register_x("X::Trait::NotOnNative", "Exception", &["X::Trait"]);
        register_x("X::Trait::Scope", "Exception", &["X::Trait"]);
        register_x("X::Trait::Unknown", "Exception", &["X::Trait"]);
        register_x("X::Undeclared", "Exception", &["X::Comp"]);
        register_x("X::Undeclared::Symbols", "Exception", &["X::Comp"]);
        register_x("X::UnitScope::Invalid", "Exception", &["X::Syntax"]);
        register_x("X::UnitScope::MustHaveUnit", "Exception", &["X::Syntax"]);
        register_x("X::UnitScope::TooLate", "Exception", &["X::Syntax"]);
        register_x("X::Useless::Declaration", "Exception", &["X::Comp"]);
        register_x("X::Value::Dynamic", "Exception", &["X::Comp"]);
        register_x("X::WheneverOutOfScope", "Exception", &[]);
        register_x("X::Worry", "Exception", &[]);
        register_x(
            "X::Assignment::ArrayShapeMismatch",
            "X::ArrayShapeMismatch",
            &[],
        );
        register_x("X::Attribute::Regex", "X::Undeclared", &["X::Comp"]);
        register_x("X::Attribute::Undeclared", "X::Undeclared", &["X::Comp"]);
        register_x("X::Bind::Rebind", "X::Bind", &[]);
        register_x("X::Bind::ZenSlice", "X::Bind::Slice", &[]);
        register_x("X::Caller::NotDynamic", "X::Symbol::Kind", &[]);
        register_x("X::Coerce::Role", "X::Coerce", &["X::Wrapper"]);
        register_x("X::Comp::AdHoc", "X::AdHoc", &["X::Comp"]);
        register_x("X::Comp::NYI", "X::NYI", &["X::Comp"]);
        register_x(
            "X::Comp::Trait::Invalid",
            "X::Trait::Invalid",
            &["X::Comp", "X::Trait"],
        );
        register_x(
            "X::Comp::Trait::NotOnNative",
            "X::Trait::NotOnNative",
            &["X::Comp", "X::Trait"],
        );
        register_x(
            "X::Comp::Trait::Scope",
            "X::Trait::Scope",
            &["X::Comp", "X::Trait"],
        );
        register_x(
            "X::Comp::Trait::Unknown",
            "X::Trait::Unknown",
            &["X::Comp", "X::Trait"],
        );
        register_x("X::Comp::TypeCheck", "X::TypeCheck", &["X::Comp"]);
        register_x("X::ControlFlow::Return", "X::ControlFlow", &[]);
        register_x(
            "X::Declaration::Scope::Multi",
            "X::Declaration::Scope",
            &["X::Comp"],
        );
        register_x("X::NYI::Available", "X::NYI", &[]);
        register_x(
            "X::Placeholder::Attribute",
            "X::Placeholder::Block",
            &["X::Comp"],
        );
        register_x(
            "X::Placeholder::Mainline",
            "X::Placeholder::Block",
            &["X::Comp"],
        );
        register_x(
            "X::Role::Unresolved::Method",
            "X::Role::Unresolved",
            &["X::RoleApplier::Method", "X::RoleApplier"],
        );
        register_x(
            "X::Role::Unresolved::Multi",
            "X::Role::Unresolved",
            &["X::RoleApplier::Method", "X::RoleApplier"],
        );
        register_x(
            "X::Role::Unresolved::Private",
            "X::Role::Unresolved",
            &["X::RoleApplier::Method", "X::RoleApplier"],
        );
        register_x("X::SecurityPolicy::Eval", "X::SecurityPolicy", &[]);
        register_x("X::Symbol::NotDynamic", "X::Symbol::Kind", &[]);
        register_x("X::Symbol::NotLexical", "X::Symbol::Kind", &[]);
        register_x(
            "X::Syntax::NonListAssociative",
            "X::Syntax::NonAssociative",
            &["X::Syntax"],
        );
        register_x("X::Temporal::OutOfRange", "X::OutOfRange", &["X::Temporal"]);
        register_x(
            "X::TypeCheck::Attribute::Default",
            "X::TypeCheck",
            &["X::Comp"],
        );
        register_x("X::TypeCheck::Splice", "X::TypeCheck", &["X::Comp"]);
        register_x("X::Worry::P5", "X::Worry", &[]);
        register_x("X::Worry::Precedence::Range", "X::Worry", &[]);
        register_x(
            "X::Syntax::Number::LiteralType",
            "X::TypeCheck::Assignment",
            &["X::Syntax"],
        );
        register_x("X::Worry::P5::BackReference", "X::Worry::P5", &[]);
        register_x("X::Worry::P5::LeadingZero", "X::Worry::P5", &[]);
        register_x("X::Worry::P5::Reference", "X::Worry::P5", &[]);
        // Built-in class definitions (PR-A slice 3: `classes` now lives in the
        // shared Registry instead of an Interpreter field).
        // Field-by-field init rather than `Registry { .. }` struct
        // update: `proto_subs`/`proto_gen` are private (their
        // mutations must flow through the gen-bumping accessors).
        for (name, class_def) in Self::io_notification_class_defs() {
            classes.insert(name, class_def);
        }
        let mut registry = Registry::default();
        registry.classes = classes;
        registry.seed_builtin_method_entries();
        // Built-in class -> composed-role seeds (PR-A slice 2: class metadata
        // now lives in the shared Registry instead of an Interpreter field).
        let ccr = &mut registry.class_composed_roles;
        ccr.insert(
            "CompUnit::Repository::FileSystem".to_string(),
            vec!["CompUnit::Repository".to_string()],
        );
        ccr.insert(
            "Encoding::Decoder::Builtin".to_string(),
            vec!["Encoding::Decoder".to_string()],
        );
        // Built-in type role composition
        ccr.insert(
            "Int".to_string(),
            vec!["Real".to_string(), "Numeric".to_string()],
        );
        ccr.insert(
            "Num".to_string(),
            vec!["Real".to_string(), "Numeric".to_string()],
        );
        ccr.insert(
            "Rat".to_string(),
            vec![
                "Rational[Int,Int]".to_string(),
                "Real".to_string(),
                "Numeric".to_string(),
            ],
        );
        ccr.insert(
            "FatRat".to_string(),
            vec![
                "Rational[Int,Int]".to_string(),
                "Real".to_string(),
                "Numeric".to_string(),
            ],
        );
        ccr.insert("Complex".to_string(), vec!["Numeric".to_string()]);
        ccr.insert("Str".to_string(), vec!["Stringy".to_string()]);
        // The Positional/Associative side of the built-in vocabulary, measured
        // against raku v2026.07 (`T.^roles`). Without these `.^roles` answered
        // an EMPTY list for every one of them. Like the numeric seeds above,
        // each list is the FLATTENED closure `class_composed_roles` is
        // documented to hold; `role_parents` below records which of them are
        // reached through another, so `.^roles(:!transitive)` can filter.
        // The DIRECT compositions of the built-ins whose flattened closure is
        // wider, so `.^roles(:!transitive)` can answer from a record rather
        // than re-derive directness from `role_parents` (which cannot see
        // through a parametric role like `Rational[Int,Int]` unless the
        // prelude declaring it was injected).
        for (class_name, direct) in [
            ("Int", &["Real"][..]),
            ("Num", &["Real"][..]),
            ("Rat", &["Rational[Int,Int]"][..]),
            ("FatRat", &["Rational[Int,Int]"][..]),
            ("Seq", &["Sequence", "Iterable"][..]),
            ("Buf", &["Blob[T]"][..]),
            ("Set", &["Setty"][..]),
            ("Bag", &["Baggy"][..]),
            ("Mix", &["Mixy"][..]),
        ] {
            registry.class_direct_composed_roles.insert(
                class_name.to_string(),
                direct.iter().map(|r| (*r).to_string()).collect(),
            );
        }
        let ccr = &mut registry.class_composed_roles;
        for (class_name, roles) in [
            ("Array", &["Positional", "Iterable"][..]),
            ("List", &["Positional", "Iterable"][..]),
            ("Slip", &["Positional", "Iterable"][..]),
            ("Range", &["Positional", "Iterable"][..]),
            ("Hash", &["Associative", "Iterable"][..]),
            ("Map", &["Associative", "Iterable"][..]),
            ("Pair", &["Associative"][..]),
            (
                "Seq",
                &["Sequence", "PositionalBindFailover", "Iterable"][..],
            ),
            ("Buf", &["Blob[T]", "Positional[T]", "Stringy"][..]),
            ("Blob", &["Positional[T]", "Stringy"][..]),
            ("Set", &["Setty", "QuantHash", "Associative"][..]),
            ("Bag", &["Baggy", "QuantHash", "Associative"][..]),
            ("Mix", &["Mixy", "Baggy", "QuantHash", "Associative"][..]),
        ] {
            ccr.insert(
                class_name.to_string(),
                roles.iter().map(|r| (*r).to_string()).collect(),
            );
        }
        for class_name in ["Compiler", "Distro", "Kernel", "Raku", "VM"] {
            ccr.insert(class_name.to_string(), vec!["Systemic".to_string()]);
        }
        // Built-in role definitions (PR-A slice 4: roles now live in the
        // shared Registry instead of an Interpreter field).
        registry.roles = {
            let mut roles = rustc_hash::FxHashMap::default();
            roles.insert(
                "Encoding".to_string(),
                RoleDef {
                    attributes: Vec::new(),
                    attribute_built: HashMap::new(),
                    methods: HashMap::new(),
                    is_stub_role: false,
                    is_hidden: false,
                    captured_env: None,
                    wildcard_handles: Vec::new(),
                    role_id: 0,
                    attribute_conflicts: Vec::new(),
                    own_attribute_names: std::collections::HashSet::new(),
                    deferred_body: Vec::new(),
                    body_bind_cells: Vec::new(),
                    decl_file: None,
                    deferred_custom_traits: Vec::new(),
                    pending_param_type_checks: Vec::new(),
                },
            );
            // Rakudo exposes the streaming encoding interfaces as roles.  The
            // native dispatch table is still registered as a class-shaped
            // entry above, but user implementations must be able to compose
            // these role names (for example Encoding::Emacs::Registry's
            // SingleByteDecoder/SingleByteEncoder).
            for role_name in ["Encoding::Decoder", "Encoding::Encoder"] {
                roles.insert(
                    role_name.to_string(),
                    RoleDef {
                        attributes: Vec::new(),
                        attribute_built: HashMap::new(),
                        methods: HashMap::new(),
                        is_stub_role: false,
                        is_hidden: false,
                        captured_env: None,
                        wildcard_handles: Vec::new(),
                        role_id: 0,
                        attribute_conflicts: Vec::new(),
                        own_attribute_names: std::collections::HashSet::new(),
                        deferred_body: Vec::new(),
                        body_bind_cells: Vec::new(),
                        decl_file: None,
                        deferred_custom_traits: Vec::new(),
                        pending_param_type_checks: Vec::new(),
                    },
                );
            }
            roles.insert(
                "Iterator".to_string(),
                RoleDef {
                    attributes: Vec::new(),
                    attribute_built: HashMap::new(),
                    methods: HashMap::new(),
                    is_stub_role: false,
                    is_hidden: false,
                    captured_env: None,
                    wildcard_handles: Vec::new(),
                    role_id: 0,
                    attribute_conflicts: Vec::new(),
                    own_attribute_names: std::collections::HashSet::new(),
                    deferred_body: Vec::new(),
                    body_bind_cells: Vec::new(),
                    decl_file: None,
                    deferred_custom_traits: Vec::new(),
                    pending_param_type_checks: Vec::new(),
                },
            );
            roles.insert(
                "PredictiveIterator".to_string(),
                RoleDef {
                    attributes: Vec::new(),
                    attribute_built: HashMap::new(),
                    methods: HashMap::new(),
                    is_stub_role: false,
                    is_hidden: false,
                    captured_env: None,
                    wildcard_handles: Vec::new(),
                    role_id: 0,
                    attribute_conflicts: Vec::new(),
                    own_attribute_names: std::collections::HashSet::new(),
                    deferred_body: Vec::new(),
                    body_bind_cells: Vec::new(),
                    decl_file: None,
                    deferred_custom_traits: Vec::new(),
                    pending_param_type_checks: Vec::new(),
                },
            );
            roles.insert(
                "Iterable".to_string(),
                RoleDef {
                    attributes: Vec::new(),
                    attribute_built: HashMap::new(),
                    methods: HashMap::new(),
                    is_stub_role: false,
                    is_hidden: false,
                    captured_env: None,
                    wildcard_handles: Vec::new(),
                    role_id: 0,
                    attribute_conflicts: Vec::new(),
                    own_attribute_names: std::collections::HashSet::new(),
                    deferred_body: Vec::new(),
                    body_bind_cells: Vec::new(),
                    decl_file: None,
                    deferred_custom_traits: Vec::new(),
                    pending_param_type_checks: Vec::new(),
                },
            );
            roles.insert(
                "X::Control".to_string(),
                RoleDef {
                    attributes: Vec::new(),
                    attribute_built: HashMap::new(),
                    methods: HashMap::new(),
                    is_stub_role: false,
                    is_hidden: false,
                    captured_env: None,
                    wildcard_handles: Vec::new(),
                    role_id: 0,
                    attribute_conflicts: Vec::new(),
                    own_attribute_names: std::collections::HashSet::new(),
                    deferred_body: Vec::new(),
                    body_bind_cells: Vec::new(),
                    decl_file: None,
                    deferred_custom_traits: Vec::new(),
                    pending_param_type_checks: Vec::new(),
                },
            );
            // ADR-0029: role-shaped `X::` exception "namespaces".
            // Measured against real rakudo (2026-08-17), 59% of the
            // `X::` classes mutsu raises or tests against compose one
            // or more of these marker roles rather than inheriting
            // from a same-named class -- `X::Comp`, `X::Syntax`,
            // `X::IO`, and `X::OS` are the heavily-used ones (135/69/
            // 22/22 classes respectively), the rest are 1-7 each. All
            // are empty-bodied here, as they are in rakudo too: mutsu's
            // exception machinery supplies the behaviour, these exist
            // purely so `.^roles`, `.^does`, and `~~` agree with
            // rakudo about which classes compose them. Registering
            // them as roles (not classes) also satisfies
            // `type_matching.rs`'s `resolve_role_key` gate for
            // type-object `~~` (e.g. `X::Comp::FailGoal ~~ X::Comp`).
            // `X::Nominalizable` / `X::Role::Attribute` (Slice 3) were
            // not in the ADR's original 14 -- its corpus was a 303-name
            // roast/t sample; Slice 3's broader capture (roast/t plus
            // every `X::...` string literal in mutsu's own source)
            // surfaced these two additional roles the same way.
            // `X::Wrapper` used to be listed here too, but it is NOT
            // actually empty in rakudo (unlike the rest of this list) --
            // it is a real, stateful role (`has $!exception`,
            // `!wrappee-message`, ...) that wraps a lower-level exception,
            // and a composing class calling its private methods died with
            // `X::Method::NotFound` against the empty stand-in (#8573).
            // It now gets the full-behaviour prelude treatment instead,
            // the same way `Enumeration`/`Rational`/`IO::Socket` do -- see
            // `X_WRAPPER_ROLE_PRELUDE` in `run.rs`.
            for role_name in [
                "X::Comp",
                "X::Syntax",
                "X::IO",
                "X::OS",
                "X::Trait",
                "X::Proc::Async",
                "X::BadType",
                "X::Temporal",
                "X::MOP",
                "X::Encoding",
                "X::Pod",
                "X::RoleApplier",
                "X::RoleApplier::Method",
                "X::Nominalizable",
                "X::Role::Attribute",
                // Not a marker role like the rest: rakudo mixes
                // `X::Promise::Broken` into the *cause* exception that
                // `Promise.result` rethrows, producing the anonymous
                // mixin type `X::AdHoc+{X::Promise::Broken}`. It is
                // registered here so `eval_does_values` can compose it
                // and `~~ X::Promise::Broken` answers correctly.
                "X::Promise::Broken",
                // The same shape for a `react` block that dies: rakudo
                // rethrows the exception `but X::React::Died` (its `gist`
                // lives in `wrapper_role_gist`).
                "X::React::Died",
                // Also not a marker role: rakudo's
                // `X::AdHoc.from-slurpy(...)` mixes this into the
                // `Capture` it stores as `.payload`, so the payload's
                // `.^name` is `Capture+{X::AdHoc::SlurpySentry}` and
                // `.Str` knows to concatenate rather than render the
                // capture. Registered here so the composition can
                // happen at all.
                "X::AdHoc::SlurpySentry",
            ] {
                roles.insert(
                    role_name.to_string(),
                    RoleDef {
                        attributes: Vec::new(),
                        attribute_built: HashMap::new(),
                        methods: HashMap::new(),
                        is_stub_role: false,
                        is_hidden: false,
                        captured_env: None,
                        wildcard_handles: Vec::new(),
                        role_id: 0,
                        attribute_conflicts: Vec::new(),
                        own_attribute_names: std::collections::HashSet::new(),
                        deferred_body: Vec::new(),
                        body_bind_cells: Vec::new(),
                        decl_file: None,
                        deferred_custom_traits: Vec::new(),
                        pending_param_type_checks: Vec::new(),
                    },
                );
            }
            // CompUnit::Repository role with required stub methods
            {
                let stub_body = vec![Stmt::Expr(Expr::Call {
                    name: Symbol::intern("__mutsu_stub_die"),
                    args: vec![],
                })];
                let stub_method = |body: Vec<Stmt>| MethodDef {
                    syms: Default::default(),
                    lexical_package: crate::symbol::wk::global_package(),
                    params: Vec::new(),
                    param_defs: Vec::new(),
                    body: std::sync::Arc::new(body),
                    is_rw: false,
                    is_raw: false,
                    is_private: false,
                    is_multi: false,
                    is_my: false,
                    role_origin: None,
                    original_role: None,
                    return_type: None,
                    compiled_code: None,
                    compiled_fns: None,
                    delegation: None,
                    is_default: false,
                    deprecated_message: None,
                    is_submethod: false,
                    is_hidden_from_backtrace: false,
                    captured_env: None,
                    source_file: None,
                    role_param_bindings: None,
                    nested_capture_index: None,
                    captured_readonly: None,
                };
                let mut methods = HashMap::new();
                // Rakudo's CompUnit::Repository role requires exactly
                // `id`, `need`, and `loaded` (a class doing the role must
                // implement those three). `load` is NOT a required method.
                for name in ["id", "need", "loaded"] {
                    methods.insert(name.to_string(), vec![stub_method(stub_body.clone())]);
                }
                roles.insert(
                    "CompUnit::Repository".to_string(),
                    RoleDef {
                        attributes: Vec::new(),
                        attribute_built: HashMap::new(),
                        methods,
                        is_stub_role: false,
                        is_hidden: false,
                        captured_env: None,
                        wildcard_handles: Vec::new(),
                        role_id: 0,
                        attribute_conflicts: Vec::new(),
                        own_attribute_names: std::collections::HashSet::new(),
                        deferred_body: Vec::new(),
                        body_bind_cells: Vec::new(),
                        decl_file: None,
                        deferred_custom_traits: Vec::new(),
                        pending_param_type_checks: Vec::new(),
                    },
                );
            }
            // `Distribution` built-in interface role. Real Rakudo defines
            // it with required stub methods `meta` and `content`; user
            // distribution classes (e.g. `Zef::Distribution does
            // Distribution`) supply the implementations. Registering the
            // role lets such classes compose and lets `~~ Distribution`
            // recognize them.
            {
                let stub_body = vec![Stmt::Expr(Expr::Call {
                    name: Symbol::intern("__mutsu_stub_die"),
                    args: vec![],
                })];
                let stub_method = |body: Vec<Stmt>| MethodDef {
                    syms: Default::default(),
                    lexical_package: crate::symbol::wk::global_package(),
                    params: Vec::new(),
                    param_defs: Vec::new(),
                    body: std::sync::Arc::new(body),
                    is_rw: false,
                    is_raw: false,
                    is_private: false,
                    is_multi: false,
                    is_my: false,
                    role_origin: None,
                    original_role: None,
                    return_type: None,
                    compiled_code: None,
                    compiled_fns: None,
                    delegation: None,
                    is_default: false,
                    deprecated_message: None,
                    is_submethod: false,
                    is_hidden_from_backtrace: false,
                    captured_env: None,
                    source_file: None,
                    role_param_bindings: None,
                    nested_capture_index: None,
                    captured_readonly: None,
                };
                let mut methods = HashMap::new();
                for name in ["meta", "content"] {
                    methods.insert(name.to_string(), vec![stub_method(stub_body.clone())]);
                }
                roles.insert(
                    "Distribution".to_string(),
                    RoleDef {
                        attributes: Vec::new(),
                        attribute_built: HashMap::new(),
                        methods,
                        is_stub_role: false,
                        is_hidden: false,
                        captured_env: None,
                        wildcard_handles: Vec::new(),
                        role_id: 0,
                        attribute_conflicts: Vec::new(),
                        own_attribute_names: std::collections::HashSet::new(),
                        deferred_body: Vec::new(),
                        body_bind_cells: Vec::new(),
                        decl_file: None,
                        deferred_custom_traits: Vec::new(),
                        pending_param_type_checks: Vec::new(),
                    },
                );
            }
            roles
        };
        // ADR-0029: role-to-role composition among the 16 `X::` marker
        // roles above, re-verified against real rakudo (2026-08-19,
        // see exception-class-hierarchy-is-mostly-unregistered (#7545)
        // R1) -- exactly three edges exist; the other thirteen compose
        // nothing. (Slice 3 grew the marker-role list from the ADR's
        // original 14 to 16 without re-running this measurement, which
        // is how `X::Role::Attribute does X::RoleApplier` was missed.)
        registry
            .role_parents
            .insert("X::Syntax".to_string(), vec!["X::Comp".to_string()]);
        registry
            .role_parents
            .insert("X::IO".to_string(), vec!["X::OS".to_string()]);
        registry.role_parents.insert(
            "X::Role::Attribute".to_string(),
            vec!["X::RoleApplier".to_string()],
        );
        // ADR-0029: write `register_x`'s collected `does` lists into the
        // composed-role registries that `.^roles`, `~~`, qualified
        // `self.Role::meth` dispatch, and method-candidate collection
        // already read (`class_composed_roles` /
        // `class_direct_composed_roles` / `class_does_only_roles`).
        // `class_composed_roles` is documented as the FLATTENED set, so
        // walk `role_parents` here to pull in roles reached
        // transitively through a composed role's own `does` (a class
        // doing `X::Syntax` also does `X::Comp`).
        for (class_name, does) in register_x_does {
            let mut flattened: Vec<String> = does.clone();
            let mut i = 0;
            while i < flattened.len() {
                if let Some(parents) = registry.role_parents.get(flattened[i].as_str()) {
                    for p in parents {
                        if !flattened.contains(p) {
                            flattened.push(p.clone());
                        }
                    }
                }
                i += 1;
            }
            // `does`-ONLY: a role that is ALSO this class's declared PARENT is
            // a role-as-superclass pun, not a pure composition, and must stay
            // in the class's MRO. rakudo has exactly one in this vocabulary --
            // `X::TooLateForREPR`, whose parent AND composed role are both
            // `X::Comp`, and whose `.^mro` really is
            // `(X::TooLateForREPR X::Comp Exception Any Mu)` there. Recording
            // it here dropped `X::Comp` from `.^mro_unhidden` (and, once
            // `.^mro` started consulting this set, from `.^mro` too).
            let parents: &[String] = registry
                .classes
                .get(&class_name)
                .map_or(&[], |c| c.parents.as_slice());
            let does_only: Vec<String> = does
                .iter()
                .filter(|r| !parents.contains(r))
                .cloned()
                .collect();
            registry
                .class_composed_roles
                .insert(class_name.clone(), flattened);
            registry
                .class_direct_composed_roles
                .insert(class_name.clone(), does);
            registry.class_does_only_roles.insert(class_name, does_only);
        }
        // Only a class with attributes has accessor rows to add, and a fresh
        // registry has none to clear, so attribute-less classes (most of the
        // built-in `X::` tree) are skipped rather than interned for a no-op
        // sync (#10961). The rest are interned straight from the borrowed
        // keys, with no `String` copy of each name.
        debug_assert!(registry.owner_accessor_names.is_empty());
        let class_syms: Vec<Symbol> = registry
            .classes
            .iter()
            .filter(|(_, class_def)| !class_def.attributes.is_empty())
            .map(|(name, _)| Symbol::intern(name))
            .collect();
        for class_sym in class_syms {
            registry.sync_accessor_entries(class_sym);
        }
        Self::seed_builtin_enum_types(&mut registry);
        registry
    }

    pub fn new() -> Self {
        // Every thread that runs an interpreter is a GC mutator: register it
        // before this constructor's first `Gc` write and before the safepoint
        // below, which may itself collect (#11714).
        crate::gc::register_interpreter_thread();
        // Constructing a top-level interpreter is a GC re-entry boundary (no
        // borrow, no lock, no `gc_contents_mut` is held here), and for an
        // embedder that drives mutsu by building one interpreter per request it
        // may be the ONLY one it ever reaches: the candidate buffer is drained
        // by `gc_safepoint`, which the VM emits from its dispatch loops, so a
        // construct-and-drop loop that never runs bytecode buffered the dead
        // nodes of every interpreter it dropped and never collected them (that
        // was the whole of the ~7 KiB/construction retention in #7572 — with
        // `MUTSU_GC=off`, which buffers nothing, it measured 0.08 KiB).
        crate::vm::vm_poll::poll(crate::gc::SafepointKind::Construct, 0);
        // Seed the process-wide magicals.
        let env = {
            let mut env = ValueMap::default();
            env.insert("*PID".to_string(), Value::int(current_process_id()));
            env.insert("*TZ".to_string(), Value::int(local_timezone_offset_secs()));
            env.insert("@*ARGS".to_string(), Value::real_array(Vec::new()));
            env.insert("*INIT-INSTANT".to_string(), Value::make_instant_now());
            crate::vm::process_start_epoch_nanos();
            // Populate %*ENV with all OS environment variables so that
            // %*ENV.keys, %*ENV.elems, and copying %*ENV work correctly.
            let env_hash = os_env_hash();
            env.insert(
                "%*ENV".to_string(),
                Value::hash_with_data(Value::hash_arc(env_hash)),
            );
            env.insert(
                "*SCHEDULER".to_string(),
                Value::make_instance(Symbol::intern("ThreadPoolScheduler"), HashMap::new()),
            );
            env
        };

        let mut interpreter = Self {
            types: type_state::TypeState::new(),
            literal_native_args: 0,
            static_call_args: false,
            env: Env::from(env),
            io: io_state::IoState::new(),
            control: control_state::ControlState::new(),
            main_hidden_from_usage: Default::default(),
            explicit_run_main: false,
            nested_mode: false,
            module: module_state::ModuleState::new(),
            dispatch: Default::default(),
            current_unit: crate::runtime::main_unit(),
            closures_created: 0,
            current_package_sym: Arc::new(std::sync::atomic::AtomicU32::new(
                crate::symbol::Symbol::intern("GLOBAL").id(),
            )),
            routine_stack: crate::runtime::routine_stack::RoutineStack::default(),
            callframe_stack: Vec::new(),
            pending_call_arg_sources: None,
            pending_where_exception: None,
            pending_skip_constraint_recheck: false,
            pending_raw_invocant: None,
            pending_call_arg_source_slots: std::collections::HashMap::new(),
            pending_rw_writeback_slots: std::collections::HashMap::new(),
            test_pending_callsite_line: None,
            nqp_arg_scratch: Vec::new(),
            cur_source_line: 1,
            thread_spawn_origin: None,
            args_scratch_pool: Vec::new(),
            regex_quant_scratch: Vec::new(),
            block_stack: Vec::new(),
            declarator_docs: declarator_docs::DeclaratorDocs::default(),
            topic_state: Default::default(),
            async_state: Default::default(),
            block_scope_depth: 0,
            pending_sigilless_store: None,
            regex_state: Default::default(),
            closure_env_overrides: HashMap::new(),
            caches: Default::default(),
            pending_eval_sigilless: Vec::new(),
            pending_eval_placeholder_params: Vec::new(),
            pending_eval_rw_tail: false,
            pending_eval_context_routine: None,
            repl_compiler: Default::default(),
            pending_supply_block_body: false,
            pending_supply_emitter_sym: None,
            pending_supply_authoritative_free_vars: Vec::new(),
            pending_whenever_inherited_owned: Vec::new(),
            last_block_my_declared: Vec::new(),
            pending_runtime_name_writes: Vec::new(),
            threads: thread_sharing::ThreadSharing::root(),
            lexicals: lexical_state::LexicalState::new(),
            in_lvalue_assignment: false,
            rw_return_context: false,
            trait_mod_writeback_key: None,
            trait_mod_writeback_value: None,
            trait_mod_attr_writeback_value: None,
            trait_mod_default_writeback: None,
            hash_autovivify: false,
            caller_env_stack: Vec::new(),
            raku_cycle_guards: Default::default(),
            last_value: None,
            pending_local_updates: Vec::new(),

            // Merged VM execution registers (CP-3 collapse) — same defaults the
            // former `VM::new` installed.
            stack: Vec::new(),
            locals: crate::runtime::locals::Locals::new(),
            trir: crate::trir::frame::TrStacks::default(),
            trir_outer_cache: rustc_hash::FxHashMap::default(),
            upvalues: Vec::new(),
            frame_authoritative: Vec::new(),
            frame_owned: Vec::new(),
            call_frames: Vec::new(),
            stack_check_countdown: 0,
            handler_fns_snapshot: None,
            current_code: 0,
            numeric_op_site: (0, 0),
            carrier_writes: None,
            resume_ip: None,
            #[cfg(feature = "jit")]
            jit_error: None,
            accessor_ref_pending: false,
            sigilless_bind_source: None,
            mark_ctx: Box::default(),
            array_share_active: false,
            element_share_pending: false,
            vardecl_init_raw: None,
            pending_rw_writeback_sources: Vec::new(),
            pending_caller_var_writeback: rustc_hash::FxHashSet::default(),
            inline_control_env_writes: Vec::new(),
            local_bind_pairs: Vec::new(),
            rw_param_rebinds: Vec::new(),
            call_ic: [crate::opcode::CallIcSlot::EMPTY; crate::opcode::CALL_IC_WAYS],
            pos_light_ic_epoch: 1,
            outer_scope_locals: Vec::new(),
            enter_result_stack: Vec::new(),
            pending_alias_bind_names: Vec::new(),
            nested_run_depth: 0,
        };
        interpreter.init_io_environment();
        interpreter.env.insert("Any".to_string(), Value::NIL);
        {
            // Built-in enum constants (Order/Endian/ProtocolFamily/Signal) are
            // process-wide immutables: collect them into the shared base tier
            // instead of every per-frame env overlay (docs/vm-dual-store.md 4b).
            let mut enum_base: HashMap<Symbol, Value> = HashMap::new();
            // Only the base-tier `Value`s are built here; the enum TYPES
            // themselves are already in the shared built-in registry template
            // (`seed_builtin_enum_types`), so this no longer takes a registry
            // write guard — which used to deep-clone the whole registry.
            Self::init_order_enum(&mut enum_base);
            Self::init_endian_enum(&mut enum_base);
            Self::init_protocol_family_enum(&mut enum_base);
            Self::init_signal_enum(&mut enum_base);
            Self::init_seek_type_enum(&mut enum_base);
            Self::init_file_change_event_enum(&mut enum_base);
            Self::init_promise_status_enum(&mut enum_base);
            // Hoist the immutable process-constant magic/dynamic vars out of every
            // per-frame env overlay into the shared base tier (docs/vm-dual-store.md
            // 4c "natural extension"). These are set once at interpreter start and
            // never reassigned/removed by normal programs; reads fall back to the
            // base tier, and a rare write is promoted into the overlay by
            // `Env::get_mut`, so semantics are preserved while the per-call deep
            // copy forks a smaller overlay. Mutable dynamics ($*OUT, $*CWD, %*ENV,
            // @*ARGS, $*SCHEDULER, $*REPO, handles, ...) intentionally stay in the
            // overlay.
            for key in IMMUTABLE_BASE_DYNAMICS {
                if let Some(v) = interpreter.env.remove(key) {
                    enum_base.insert(Symbol::intern(key), v);
                }
            }
            crate::env::set_global_base(enum_base);
            // Set up $*REPO as a default CompUnit::Repository::FileSystem instance
            // (attrs mirror explicit `.new()` construction, see
            // methods_object_dispatch_new.rs's "CompUnit::Repository::FileSystem" arm,
            // so `.short-id`/`.prefix` behave the same on the default instance).
            let mut attrs = HashMap::new();
            attrs.insert("prefix".to_string(), interpreter.make_io_path_instance("."));
            attrs.insert("short-id".to_string(), Value::str_from("file"));
            attrs.insert("__mutsu_precomp_enabled".to_string(), Value::TRUE);
            attrs.insert(
                super::run_modules_bound_repo::DEFAULT_REPO_HEAD_ATTR.to_string(),
                Value::TRUE,
            );
            let repo =
                Value::make_instance(Symbol::intern("CompUnit::Repository::FileSystem"), attrs);
            interpreter.env.insert("*REPO".to_string(), repo);
            // Every interpreter instance (top-level CLI, REPL, EVAL, and the
            // nested in-process Interpreter that Test::Util's `is_run` spawns
            // for its fast path) needs the default "site" repository wired into
            // module resolution -- not just the top-level CLI -- so a plain
            // `use ModuleName` finds anything installed via
            // `CompUnit::RepositoryRegistry.repository-for-name("site").install(...)`
            // regardless of how the interpreter was embedded.
            interpreter.add_default_site_repo();
            // Rakudo's own core setting bootstraps `nqp::gethllsym("default",
            // "SysConfig")` at BEGIN time (see `bootstrap_hll_syms`).
            interpreter.bootstrap_hll_syms();
        }
        interpreter
    }
}
