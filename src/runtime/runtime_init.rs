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

/// `%*ENV`'s initial value: the OS environment as a `Hash`, each value
/// `val()`-ed as rakudo does (`IntStr` for `42`). The builder of the base
/// tier's lazy `%*ENV` entry (#19224).
// Cost: O(v), v = the environment's variables.
fn env_hash_value() -> Value {
    Value::hash_with_data(Value::hash_arc(os_env_hash()))
}

impl Interpreter {
    /// Take any pending regex security error from the thread-local store.
    pub(crate) fn take_pending_regex_error() -> Option<RuntimeError> {
        // Delegate to the regex_parse module's thread-local error store
        regex_parse::PENDING_REGEX_ERROR.with(|e| e.borrow_mut().take())
    }

    /// [`Self::take_pending_regex_error`], or else the error a grammar action
    /// raised at its subrule's reduction. Only the grammar-parse entry points
    /// read the latter: a match that went on afterwards runs more code (later
    /// actions, `~~`) whose own cleanup must not discard it.
    pub(crate) fn take_pending_regex_or_action_error() -> Option<RuntimeError> {
        Self::take_pending_regex_error()
            .or_else(|| regex_parse::PENDING_ACTION_ERROR.with(|e| e.borrow_mut().take()))
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
        // Every built-in class is a row of the static class catalogue
        // (`builtin_class_catalogue`, ADR-19206), materialized on its first
        // lookup rather than built here; the registry's own class table starts
        // empty over it. A built-in exception class's rakudo attributes are
        // marked for declaration on first access when it is materialized
        // (#18654, #18956), and a catalogue class answers its accessors from
        // its attribute list (`Registry::accessor_is_public_sym`).
        let mut registry = Registry::default();
        registry.classes = crate::runtime::registry::ClassTable::with_builtin_catalogue(
            Default::default(),
        );
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
        ccr.insert(
            "Encoding::Encoder::Builtin".to_string(),
            vec!["Encoding::Encoder".to_string()],
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
                // And for `await` of a broken promise: rakudo rethrows the
                // cause `but X::Await::Died` (gist in `wrapper_role_gist`).
                "X::Await::Died",
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
                    args: vec![], listop: false,
                })];
                let stub_method = |body: Vec<Stmt>| MethodDef {
                    value_prototype: None,
                    syms: Default::default(),
                    lexical_package: crate::symbol::wk::global_package(),
                    params: (Vec::new()).into(),
                    param_defs: (Vec::new()).into(),
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
                    routine_cell: Default::default(),
                    augment_epoch: 0,
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
                    args: vec![], listop: false,
                })];
                let stub_method = |body: Vec<Stmt>| MethodDef {
                    value_prototype: None,
                    syms: Default::default(),
                    lexical_package: crate::symbol::wk::global_package(),
                    params: (Vec::new()).into(),
                    param_defs: (Vec::new()).into(),
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
                    routine_cell: Default::default(),
                    augment_epoch: 0,
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
                .into_iter()
                .map(|(name, def): (String, RoleDef)| (name, def.into()))
                .collect::<crate::runtime::registry_sharded_map::ShardedMap<_, _>>()
                .into()
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
        // ADR-0029: the catalogue rows' `does` lists answer the composed-role
        // registries that `.^roles`, `~~`, qualified `self.Role::meth`
        // dispatch, and method-candidate collection read
        // (`class_composed_roles` / `class_direct_composed_roles` /
        // `class_does_only_roles`). Each row's entries are computed on first
        // lookup (ADR-19206 slice 3, `registry_composed_role_table`), the
        // flattened set walking `role_parents` as it stands here, frozen now.
        // `X::TooLateForREPR` is the one row whose composed role is also its
        // parent (a role-as-superclass pun that stays in its MRO), so its
        // `does`-only set leaves `X::Comp` out.
        super::registry_composed_role_table::freeze_builtin_role_parents(&registry.role_parents);
        {
            use super::registry_composed_role_table::ComposedRoleKind;
            registry
                .class_composed_roles
                .add_builtin_catalogue(ComposedRoleKind::Flattened);
            registry
                .class_direct_composed_roles
                .add_builtin_catalogue(ComposedRoleKind::Direct);
            registry
                .class_does_only_roles
                .add_builtin_catalogue(ComposedRoleKind::DoesOnly);
        }
        // Only a class with attributes has accessor rows to add, and a fresh
        // registry has none to clear, so attribute-less classes (most of the
        // built-in `X::` tree) are skipped rather than interned for a no-op
        // sync (#10961). So are the built-in exception classes, whose
        // attributes are declared on first access (#18956):
        // `Registry::accessor_is_public_sym` answers their accessors from the
        // class. The rest are interned straight from the borrowed keys, with
        // no `String` copy of each name.
        debug_assert!(registry.owner_accessor_names.is_empty());
        // The overlay only: walking the catalogue would materialize every
        // built-in exception class, and theirs are deferred anyway.
        let class_syms: Vec<Symbol> = registry
            .classes
            .iter_overlay()
            .filter(|(_, class_def)| {
                !class_def.attributes.is_deferred() && !class_def.attributes.is_empty()
            })
            .map(|(name, _)| Symbol::intern(name))
            .collect();
        for class_sym in class_syms {
            registry.sync_accessor_entries(class_sym);
        }
        Self::seed_builtin_enum_types(&mut registry);
        registry
    }

    pub fn new() -> Self {
        crate::runtime::install_value_layer_compiler();
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
            // `%*ENV` is not built here: it is a lazy entry of the base tier,
            // installed below and built on its first lookup (#19224).
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
            pending_skip_constraint_recheck: false,
            pending_raw_invocant: None,
            pending_call_arg_source_slots: rustc_hash::FxHashMap::default(),
            pending_rw_writeback_slots: super::rw_writeback_slots::RwWritebackSlots::new(),
            test_pending_callsite_line: None,
            nqp_arg_scratch: Vec::new(),
            cur_source_line: 1,
            thread_spawn_origin: None,
            args_scratch_pool: Vec::new(),
            block_stack: Vec::new(),
            declarator_docs: declarator_docs::DeclaratorDocs::default(),
            topic_state: Default::default(),
            async_state: Default::default(),
            block_scope_depth: 0,
            regex_state: Default::default(),
            caches: Default::default(),
            pending_eval_sigilless: Vec::new(),
            pending_eval_placeholder_params: Vec::new(),
            pending_eval_context_routine: None,
            repl_compiler: Default::default(),
            pending_runtime_name_writes: Vec::new(),
            threads: thread_sharing::ThreadSharing::root(),
            lexicals: lexical_state::LexicalState::new(),
            in_lvalue_assignment: false,
            rw_return_context: false,
            trait_mod_writeback_key: None,
            trait_mod_writeback_value: None,
            trait_mod_attr_writeback_value: None,
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
            mark_ctx: Box::default(),
            array_share_active: false,
            pending_rw_writeback_sources: Vec::new(),
            pending_caller_var_writeback: rustc_hash::FxHashSet::default(),
            local_bind_pairs: Vec::new(),
            rw_param_rebinds: Vec::new(),
            call_ic: [crate::opcode::CallIcSlot::EMPTY; crate::opcode::CALL_IC_WAYS],
            pos_light_ic_epoch: 1,
            outer_scope_locals: Vec::new(),
            enter_result_stack: Vec::new(),
            pending_alias_bind_names: Vec::new(),
            nested_run_depth: 0,
        };
        // The resolution memos start empty: current for the `.wrap` tables
        // as they stand (ADR-17216 §2.3, `sync_world_wraps`).
        interpreter.caches.wraps_gen_seen = interpreter.dispatch.wraps.generation();
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
            // The base tier, carrying `%*ENV` as a lazy entry: reading every
            // environment variable and `val()`-ing each value was ~15% of a
            // `say "hello"`'s instructions, for a hash most programs never
            // name (#19224). Installed after the process-wide tier, which
            // `set_dyn_base` absorbs; `hoist_builtin_dynamics` keeps it.
            let mut base = crate::env::DynBase::default();
            base.insert_lazy(Symbol::intern("%*ENV"), env_hash_value);
            interpreter.env.set_dyn_base(base);
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

