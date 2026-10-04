//! The `types` subsystem of ADR-10779: the type registry and its write
//! generation, type metadata, and the declaration state of classes, roles,
//! enums and subsets (the class being defined or constructed, attribute
//! defaults, lexical and package-scoped type names, reblessing).

use super::*;

pub(crate) struct TypeState {
    pub(crate) method_class_stack: Vec<MethodClassFrame>,
    /// The class whose instance is currently being constructed, set only while
    /// evaluating typed-attribute default type objects so a suppressed nested
    /// class name resolves within its owning class (see `resolve_suppressed_type`).
    pub(crate) constructing_class: Option<String>,
    /// The registry storage key (the fully-qualified/mangled name actually
    /// used as the registry key, NOT the source-level bare name) of the
    /// class most recently registered by `exec_register_class_op`. Set right
    /// before that function returns `Ok`, and consumed immediately by the
    /// very next opcode when it is `PushLastRegisteredClass` — the compiler
    /// only ever emits that opcode directly after `RegisterClass` for a
    /// NAMED `class` declaration used in expression position (`(class A
    /// { ... })`), so nothing else can run between the write and the read.
    /// Exists so that expression evaluates to the type object the
    /// declaration just created, rather than a bareword lookup of `A` that
    /// can resolve to an unrelated, same-named class from a different scope
    /// (e.g. one declared inside `EVAL`'d code running in a different
    /// package than the caller). See
    /// `news/2026-08/class-decl-expr-is-not-a-name-lookup.md`.
    pub(crate) last_registered_class_key: Option<String>,
    /// Registry state from just before the class `register_class_decl` last
    /// registered WITH a parent deferred to `trait_mod:<is>` dispatch, keyed
    /// by that class's storage name. Set only on that path, and consumed by
    /// `exec_register_class_op`: if the dispatch then reports that no
    /// candidate claims the trait, the name really was an unknown parent, the
    /// declaration must fail, and — because the class shell was already
    /// published so the trait handler could see the type object — it has to be
    /// rolled back here rather than by `register_class_decl`'s own snapshot,
    /// which has already gone out of scope. Without it a failed `class B is
    /// NoSuchParent { }` left `B` registered and the next real `class B`
    /// declaration died as a redeclaration.
    pub(crate) deferred_trait_class_rollback: Option<(
        String,
        crate::runtime::registration_class_validate::ClassRegSnapshot,
    )>,
    /// The qualified registry key most recently installed by
    /// `exec_register_role_op`. Consumed immediately by
    /// `PushLastRegisteredRole` for a named role declaration expression.
    pub(crate) last_registered_role_key: Option<String>,
    /// Attribute writes observed through an instance's shared cell while its
    /// BUILD phase runs, one frame per instance under construction (BUILD may
    /// itself construct objects, so the frames nest). A frame is keyed by the
    /// cell's address; `write_attr_cell_by_key` records into the matching frame.
    /// Raku applies a `has $.x = <default>` initializer *after* BUILD and only
    /// for attributes BUILD did not set, so this is what "BUILD set it" means
    /// (an explicit `$!x = Any` counts, exactly like rakudo's null check).
    /// Interior mutability: the write path takes `&self`.
    pub(crate) build_attr_writes: std::cell::RefCell<Vec<BuildWriteFrame>>,
    /// The class whose body is currently being registered, set only while
    /// executing `BEGIN`/`EVAL` code inside a class body (see
    /// `register_class_decl`). Lets a `has`-attribute declaration that reaches
    /// the VM at runtime (`class Foo { BEGIN EVAL q[has $.x] }`) attach the
    /// attribute to the class still under construction rather than throwing
    /// `X::Attribute::NoPackage`.
    pub(crate) defining_class: Option<String>,
    /// An open role-APPLICATION group id, set while the ops the compiler split
    /// out of one `but (R1, R2)` run (see
    /// `Interpreter::open_role_application_group`). `None` outside one, so an
    /// ordinary single-role `but`/`does` mints its own group.
    pub(crate) open_role_group: Option<i64>,
    pub(crate) type_metadata: std::sync::Arc<HashMap<String, ValueMap>>,
    /// Declaration registry (enums/subsets/... — migrated group-by-group, PLAN.md ②),
    /// shared with the VM behind `Arc<RwLock>`. See [`Registry`] and `src/runtime/registry.rs`.
    /// Lock discipline: never hold a guard across user-code re-entry (deadlock).
    ///
    /// The inner `Arc<Registry>` makes a per-thread spawn an O(1) share instead
    /// of a deep clone of ~40 maps: `clone_for_thread` clones the `Arc`, and the
    /// first *write* on either side after the share pays the one deep clone via
    /// `Arc::make_mut` (see `RegistryWriteGuard::deref_mut`). Each thread still
    /// gets its own outer `Arc<RwLock<...>>`, so declarations never leak between
    /// threads — only the initial snapshot is lazily shared.
    pub(crate) registry: Arc<RwLock<Arc<Registry>>>,
    /// Monotonic counter bumped on every `registry_mut()` acquisition, i.e. every
    /// time the declaration registry may have been mutated. Several resolution
    /// caches consult it to detect "did anything write the registry since I last
    /// checked". `AtomicU64` (not `Cell`) so `Interpreter` stays `Send`/`Sync` —
    /// `registry_mut()` takes `&self`.
    pub(crate) registry_write_gen: std::sync::atomic::AtomicU64,
    /// Per-class memo of the native-dispatch numeric-bridge probe, keyed by the
    /// `registry_write_gen` above. See [`numeric_bridge_probe`] for why that
    /// generation is a sound invalidation key (#7712).
    pub(crate) numeric_bridge_probe: numeric_bridge_probe::NumericBridgeProbeCache,
    /// Per-`(class, attribute)` memo of `self_attr_type_constraint`, keyed by
    /// the same `registry_write_gen` (ADR-0121). See `vm_attr_type_constraint`.
    pub(crate) attr_type_constraint_cache: std::cell::RefCell<crate::vm::AttrTypeConstraintCache>,
    /// Class registry keys that are not lexical imports, so an import scope's
    /// class rollback (`pop_import_scope`) must keep them: a loaded module's
    /// own un-namespaced class (`unit class RS3Base;` -- the rollback already
    /// keeps every `A::B`-qualified one) and every type a
    /// `Metamodel::*HOW.new_type` minted at run time. Like `loaded_modules`,
    /// never rolled back.
    pub(crate) persistent_classes: std::sync::Arc<HashSet<String>>,
    /// Classes/roles hidden from package stash lookups (e.g. `Example2::.keys`).
    /// Populated when a `use X::Y` loads modules whose dependency chain neither
    /// declares a class matching the module name nor includes a `package X {}`
    /// declaration, hiding transitive dependencies from the namespace stash.
    pub(crate) package_stash_hidden: std::sync::Arc<HashSet<String>>,
    /// PredictiveIterator backing a `Seq.new(iterator)`, keyed by the
    /// sequence's identity (`SeqBody::identity`, the shared reification
    /// core's address — NOT one handle's `Arc`, so a retagged handle such as
    /// `.cache`'s List view or a `$`-store's `ItemSeq` still finds it).
    /// Kept off the scoped `env` so the association
    /// survives sub/block returns between Seq creation and `.tail`/`.Numeric`
    /// (an env-keyed side table was lost on scope exit).
    /// TODO: entries are never reclaimed; acceptable as predictive Seqs are rare.
    pub(crate) predictive_seq_iters: HashMap<usize, Value>,
    /// Compiled bytecode for subset `where` predicates, keyed by subset name.
    /// A subset's predicate is a fixed `Expr`, so it is compiled once and reused
    /// across all type checks instead of recompiling + cloning the entire
    /// function/proto registry on every check (the old `eval_block_value` path).
    /// Cleared per-name on subset redeclaration; starts empty per thread (the
    /// cache is a pure recomputable optimization). See `type_matches_value`.
    pub(crate) subset_predicate_cache: HashMap<String, SubsetPredicateCompiled>,
    /// Runtime-generated names for inline object-hash key subsets such as
    /// `subset :: of Str where ...`. The declaration parser keeps those key
    /// constraints as source text, so materialize each one lazily on its first
    /// type check and reuse the registered predicate thereafter.
    pub(crate) inline_subset_constraints: HashMap<String, String>,
    /// Short type names a module imported for its OWN lexical scope, keyed by the
    /// module name and by every class/role that module declares:
    /// `{"Drv2" | "Drv2::Native" => {"THING2" => "Drv2::Native::THING2"}}`.
    ///
    /// A module body runs in the *caller's* env (`load_module` → `run_block`), so
    /// the `Package` aliases its own `use` statements install land in whatever
    /// frame triggered the load and die with it. That is invisible for a
    /// compile-time `use` at file scope (the alias outlives every later call),
    /// but a `require` executed inside a method frame loses them the moment the
    /// method returns — and the module's own methods then cannot resolve their
    /// own imported type names. Recording the aliases against the module makes
    /// the resolution lexical to the module instead of dynamic to the frame.
    /// Consulted by `package_type_alias` from `has_type` / `GetBareWord`.
    pub(crate) package_type_aliases: std::sync::Arc<PackageKeyed<String>>,
    /// Attribute `is default(...)` values, keyed by the twigil names a method
    /// body reads them by (`!x`, `.x`, `@!x`, ...). A lexical's default is NOT
    /// here: it lives in the env under `MetaNs::VarDefault` (#10796).
    pub(crate) attr_var_defaults: ValueMap,
    /// Bumped on every change to `attr_var_defaults`; see
    /// `Interpreter::attr_var_defaults_are_current`.
    pub(crate) attr_var_defaults_epoch: u64,
    /// `(owner class, receiver class)` -> the `(attr_var_defaults_epoch, method
    /// generation)` at which method dispatch last registered that pair's
    /// attribute defaults. See `Interpreter::attr_var_defaults_are_current`.
    pub(crate) attr_var_defaults_current:
        rustc_hash::FxHashMap<(crate::symbol::Symbol, crate::symbol::Symbol), (u64, u64)>,
    // Array/Hash element defaults are embedded in `ArrayData.default` /
    // `HashData.default`.
    // An object hash's key type (`%h{Str}`) is carried by `HashData::key_type`
    // on the value and, for the name-keyed lane, by the env-scoped
    // `__mutsu_hash_key_type::<name>` entry — the process-global side table
    // that used to mirror it was retired with ADR-0042 slice 3.
    // Array/Hash/Set/Bag/Mix type metadata and object-hash original keys are
    // embedded in their backing data structs (ArrayData/HashData/SetData/
    // BagData/MixData) — no side tables.
    /// Type metadata for instance values keyed by stable instance id. Lifted
    /// behind `Arc<RwLock>` (the same shared-handle playbook used for
    /// `current_package` / `io_handles`) so the VM and Interpreter can reach it
    /// as peers and CP-3 can fold it by handle transfer rather than ownership
    /// reasoning. Like those handles it is a *per-thread snapshot*, not
    /// live-shared: `clone_for_thread` shares the inner `Arc` (O(1); see the
    /// `registry` field's copy-on-write doc, docs/per-task-clone-slimming.md
    /// slice 4) into a fresh outer `Arc<RwLock<...>>`, so the lock never
    /// contends across threads and the first write on either side after a
    /// share pays the one deep clone via `Arc::make_mut`. Collapses to a plain
    /// VM field once the Interpreter execution path is removed (PLAN.md ④/⑤).
    pub(crate) instance_type_metadata: Arc<RwLock<Arc<HashMap<u64, ContainerTypeInfo>>>>,
    /// Roles whose `.new` is currently constructing through their pun. `.new` on
    /// a role composes it into a class of the same name and re-enters
    /// `dispatch_new` to run *that class's* constructor; the role name is pushed
    /// here for the duration so the re-entry takes the class path instead of
    /// recognising the name as a role again and looping.
    pub(crate) role_pun_construction: Vec<String>,
    /// Pending Proxy subclass attribute reference for writeback on mutating methods.
    /// Set when reading a Proxy subclass attribute; consumed by subsequent .push/.pop etc.
    pub(crate) pending_proxy_subclass_attr: Option<(crate::value::ProxySubclassAttrs, String)>,
    /// The type object of the DECLARE'd class whose registration is currently
    /// driving the user HOW protocol (`new_type` → `add_method`* → `compose`).
    /// A `callsame` from a user `new_type` override returns it as the base
    /// candidate — the native part of `new_type` (creating and registering the
    /// type) has already run by the time the user hook is called.
    pub(crate) pending_declare_new_type: Option<Value>,
    /// Classes whose custom-HOW `compose` hook is currently running, before
    /// the native accessor-installation step it reaches via `callsame`
    /// (`methods_classhow_dispatch.rs`'s `"compose"` arm) has executed. Raku
    /// installs a public attribute's auto-generated reader into
    /// `.^method_table` as part of that native step, not at attribute
    /// declaration time — a custom `compose` override that inspects
    /// `type.^method_table` before calling `callsame` (AttrX::Lazy's
    /// `LazyAttributeContainerHOW.compose`) must see it still absent (#8836).
    /// `class_method_table`/`collect_class_methods` consult this set to hide
    /// a class's own auto-accessors while it is composing; the native
    /// `compose` arm removes the entry (the accessors are installed from
    /// then on, so a hook reading `.^method_table` after its `callsame`
    /// sees them, as in Rakudo), and the caller removes it once the hook
    /// call returns, whether it succeeded or not.
    pub(crate) classes_composing_accessors: HashSet<String>,
    /// Metamodel method fallbacks registered via `.^add_fallback(cond, calc)`:
    /// class_name -> list of (condition, calculator) code pairs. When a method
    /// is not found on a value of that class, each condition is called with
    /// `(invocant, method_name)`; the first that returns True has its calculator
    /// called with `(invocant, method_name)` to produce the method body, which is
    /// then invoked with the invocant.
    pub(crate) method_fallbacks: std::sync::Arc<HashMap<String, Vec<(Value, Value)>>>,
    /// Names suppressed by `anon class`. These bare words should error as undeclared.
    pub(crate) suppressed_names: std::sync::Arc<HashSet<String>>,
    /// Short names of types declared *inside a class body* (`class Outer { grammar
    /// Inner {...} }` records `Inner`). Unlike `suppressed_names` this set is never
    /// cleared: it records the fact that the short name belongs to some owner
    /// package, which stays true for the rest of the program even after another
    /// module registers an unrelated type of the same short name. It gates the
    /// owner-package-chain probe in `resolve_suppressed_type`, so a method body
    /// keeps seeing its own class's nested type (see `resolve_suppressed_type`).
    pub(crate) class_scoped_short_names: std::sync::Arc<HashSet<String>>,
    /// Bare enum variant names poisoned by redeclaration from different enums.
    /// Maps bare name -> latest enum package name.
    pub(crate) poisoned_enum_aliases: std::sync::Arc<HashMap<String, String>>,
    /// Per-scope stack of bare enum names introduced, for cleanup on scope exit.
    pub(crate) enum_scope_names: Vec<Vec<(String, u64)>>,
    /// Fully-qualified names of `my`-scoped classes/subs inside packages.
    /// These should NOT appear in the parent package's stash.
    pub(crate) my_scoped_package_items: std::sync::Arc<HashSet<String>>,
    /// Names published by an explicit `our` declaration; wins over
    /// `my_scoped_package_items` (see `mark_our_scoped_package_item`).
    pub(crate) our_scoped_package_items: std::sync::Arc<HashSet<String>>,
    /// Stack of lexically-scoped class names per block scope depth.
    /// When a block scope exits, classes registered in that scope get suppressed.
    pub(crate) lexical_class_scopes: Vec<Vec<String>>,
    /// Maps a lexical class's qualified name to the storage name a
    /// currently-open scope most recently registered it under, for stub ->
    /// full-definition continuation across two separate `decl_id`s (ADR-0047
    /// P1; see `lexical_class_pending_stub`'s doc comment).
    pub(crate) lexical_class_pending: std::collections::HashMap<String, String>,
    /// Per block-scope stack of `(qualified_name, storage_name)` records added
    /// to `lexical_class_pending` while that scope was open. Released (not
    /// just popped) at `pop_lexical_class_scope` so the map can never answer a
    /// query with an entry from an already-exited scope.
    pub(crate) lexical_class_pending_scopes: Vec<Vec<(String, String)>>,
    /// Metadata for Seq values produced by `squish` with callbacks, used to
    /// provide callback-aware iterator behavior.
    pub(crate) squish_iterator_meta: HashMap<usize, SquishIteratorMeta>,
    /// Metadata for custom types created by Metamodel::Primitives.create_type.
    pub(crate) custom_type_data: HashMap<u64, CustomTypeData>,
    /// Rebless mapping: instance_id -> new HOW value.
    /// Used by Metamodel::Primitives.rebless to track reblessed objects.
    pub(crate) rebless_map: HashMap<u64, Value>,
    /// Names of classes the user declared with a `class`/`role`/`grammar`/`enum`
    /// statement (`register_class_decl`). For such a class the collected public-
    /// attribute list is authoritative: a `.name` accessor resolves ONLY for a
    /// declared public `has $.name`; an undeclared name (e.g. an unknown named arg
    /// `.new` accepted and stored) falls through to X::Method::NotFound (Rakudo:
    /// `class C {}; C.new(x=>3).x` dies). Native/built-in objects (Parameter,
    /// Signature, exception types, ...) are NOT here — their attributes live only
    /// in the stored map and are not collected — so the accessor fallback still
    /// reads them.
    pub(crate) user_declared_classes: std::sync::Arc<std::collections::HashSet<String>>,
}

impl TypeState {
    pub(crate) fn new() -> Self {
        Self {
            open_role_group: None,
            user_declared_classes: Default::default(),
            method_class_stack: Vec::new(),
            constructing_class: None,
            last_registered_class_key: None,
            deferred_trait_class_rollback: None,
            last_registered_role_key: None,
            build_attr_writes: std::cell::RefCell::new(Vec::new()),
            defining_class: None,
            type_metadata: Default::default(),
            registry: Arc::new(RwLock::new(Interpreter::shared_builtin_registry())),
            registry_write_gen: Interpreter::fresh_registry_write_gen(),
            numeric_bridge_probe: Default::default(),
            attr_type_constraint_cache: Default::default(),
            persistent_classes: Default::default(),
            package_stash_hidden: Default::default(),
            predictive_seq_iters: HashMap::new(),
            subset_predicate_cache: HashMap::new(),
            inline_subset_constraints: HashMap::new(),
            package_type_aliases: std::sync::Arc::new(PackageKeyed::default()),
            attr_var_defaults: ValueMap::default(),
            attr_var_defaults_epoch: 0,
            attr_var_defaults_current: Default::default(),
            instance_type_metadata: Arc::new(RwLock::new(Arc::new(HashMap::new()))),
            role_pun_construction: Vec::new(),
            pending_proxy_subclass_attr: None,
            pending_declare_new_type: None,
            classes_composing_accessors: std::collections::HashSet::new(),
            method_fallbacks: Default::default(),
            suppressed_names: Default::default(),
            class_scoped_short_names: Default::default(),
            poisoned_enum_aliases: Default::default(),
            enum_scope_names: vec![Vec::new()],
            my_scoped_package_items: Default::default(),
            our_scoped_package_items: Default::default(),
            lexical_class_scopes: Vec::new(),
            lexical_class_pending: HashMap::new(),
            lexical_class_pending_scopes: Vec::new(),
            squish_iterator_meta: HashMap::new(),
            custom_type_data: HashMap::new(),
            rebless_map: HashMap::new(),
        }
    }

    /// The spawned thread's copy: it sees the parent's declarations through
    /// copy-on-write snapshots of the registry and the type metadata, keeps
    /// the declared-name tables, and starts the in-flight declaration and
    /// construction state fresh.
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self {
            open_role_group: None,
            method_class_stack: Vec::new(),
            constructing_class: None,
            last_registered_class_key: None,
            deferred_trait_class_rollback: None,
            last_registered_role_key: None,
            build_attr_writes: std::cell::RefCell::new(Vec::new()),
            defining_class: None,
            type_metadata: self.type_metadata.clone(),
            // O(1) share of the inner `Arc<Registry>` — a fresh outer lock so the
            // child thread gets an independent snapshot (matches prior per-field
            // clone semantics: the child sees parent declarations but its own new
            // ones don't leak back), while the deep clone itself is deferred until
            // either side's first registry write (`Arc::make_mut` in
            // `RegistryWriteGuard::deref_mut`). In spawn-heavy loops where neither
            // side writes the registry, the deep clone (and its drop) never happens.
            registry: Arc::new(RwLock::new(Arc::clone(&self.registry.read().unwrap()))),
            registry_write_gen: Interpreter::fresh_registry_write_gen(),
            numeric_bridge_probe: Default::default(),
            attr_type_constraint_cache: Default::default(),
            persistent_classes: self.persistent_classes.clone(),
            package_stash_hidden: self.package_stash_hidden.clone(),
            predictive_seq_iters: self.predictive_seq_iters.clone(),
            subset_predicate_cache: HashMap::new(),
            inline_subset_constraints: HashMap::new(),
            package_type_aliases: self.package_type_aliases.clone(),
            attr_var_defaults: self.attr_var_defaults.clone(),
            attr_var_defaults_epoch: self.attr_var_defaults_epoch,
            attr_var_defaults_current: Default::default(),
            // Per-thread snapshot (not a shared-handle clone), but an O(1) share
            // of the inner `Arc` (docs/per-task-clone-slimming.md slice 4): a
            // fresh outer `Arc<RwLock<...>>` keeps the child thread's instance
            // type metadata independent of the parent's (mirroring
            // `io_handles`/`current_package`), while the deep clone itself is
            // deferred until either side's first write (`Arc::make_mut` in
            // `register_container_type_metadata`).
            instance_type_metadata: Arc::new(RwLock::new(Arc::clone(
                &self.instance_type_metadata.read().unwrap(),
            ))),
            role_pun_construction: Vec::new(),
            pending_proxy_subclass_attr: None,
            pending_declare_new_type: None,
            classes_composing_accessors: std::collections::HashSet::new(),
            method_fallbacks: self.method_fallbacks.clone(),
            suppressed_names: self.suppressed_names.clone(),
            class_scoped_short_names: self.class_scoped_short_names.clone(),
            poisoned_enum_aliases: self.poisoned_enum_aliases.clone(),
            enum_scope_names: self.enum_scope_names.clone(),
            my_scoped_package_items: self.my_scoped_package_items.clone(),
            our_scoped_package_items: self.our_scoped_package_items.clone(),
            lexical_class_scopes: self.lexical_class_scopes.clone(),
            lexical_class_pending: self.lexical_class_pending.clone(),
            lexical_class_pending_scopes: self.lexical_class_pending_scopes.clone(),
            squish_iterator_meta: HashMap::new(),
            custom_type_data: self.custom_type_data.clone(),
            rebless_map: self.rebless_map.clone(),
            user_declared_classes: self.user_declared_classes.clone(),
        }
    }
}
