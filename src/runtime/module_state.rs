//! The `module` subsystem of ADR-10779: module search paths and the loaded
//! module table, the compilation-unit and package bookkeeping of a load,
//! export and import tables, operator imports, distributions and the
//! lexical pragmas (`use strict`, `use fatal`, `use MONKEY-TYPING`,
//! `use attributes`, precompilation).

use super::*;

/// Export tags per routine per package (or module): `table[package][name]` is
/// the set of tags `name` is exported under. Fx-hashed: every `is export`
/// probes these by short keys several times (#11761).
pub(crate) type ExportTagTable =
    rustc_hash::FxHashMap<String, rustc_hash::FxHashMap<String, rustc_hash::FxHashSet<String>>>;

pub(crate) struct ModuleState {
    /// NativeCall (`is native`) sub descriptors, keyed by sub name. Populated at
    /// declaration; a call to a name present here is routed through C FFI
    /// instead of running the (`{ * }`) Raku body.
    pub(crate) native_call_specs: std::sync::Arc<HashMap<String, nativecall::NativeCallSpec>>,
    /// Operator sub names (infix:<..>, prefix:<..>, etc.) that have been
    /// imported into the current lexical scope via `use Module`. Used to
    /// preseed the parser when EVAL is called so that imported operators
    /// remain visible, but non-exported operators from loaded modules do not.
    pub(crate) imported_operator_names: std::sync::Arc<HashSet<String>>,
    /// The compilation units that imported an operator candidate family,
    /// keyed by operator name and then by the candidates' declaring unit. An
    /// operator candidate that a `use` imported is visible only to its
    /// declaring unit and to these importers (#9944); see
    /// `runtime/operator_scope.rs`.
    pub(crate) operator_import_units:
        std::sync::Arc<HashMap<Symbol, HashMap<Symbol, HashSet<Symbol>>>>,
    /// Bumped whenever `operator_import_units` gains an entry, so a memo of an
    /// operator-visibility answer (`bare_multi_plan_cache`) can key on it.
    pub(crate) operator_import_gen: u64,
    /// Package-less top-level routines a loaded compunit declared but did NOT
    /// export, keyed by that compunit's unit symbol and then by routine name.
    ///
    /// Raku scopes `sub name {...}` at a compunit's top level lexically to that
    /// compunit; mutsu registers it as a shared `GLOBAL::name` stash entry, so
    /// it used to stay callable, bare, from whatever scope `use`d/`require`d
    /// the module. `seclude_private_toplevel_routines` moves such a routine out
    /// of the shared registry and in here once the load finishes, and
    /// `Interpreter::unit_private_routine` hands it back only to code compiled
    /// in the same unit (or in an `EVAL` nested inside it). See
    /// `runtime/unit_private_routines.rs`.
    pub(crate) unit_private_routines:
        std::sync::Arc<HashMap<Symbol, HashMap<Symbol, Arc<FunctionDef>>>>,
    /// Every name that appears in any `unit_private_routines` table. A cheap
    /// negative test on the resolution hot path, and the signal that a name's
    /// resolution is unit-dependent and therefore must bypass the name-keyed
    /// resolution caches (which are not keyed by unit).
    pub(crate) unit_private_names: std::sync::Arc<HashSet<Symbol>>,
    /// The compilation unit that declared each user class. Attribute defaults
    /// run later, while constructing an instance from an arbitrary caller, so
    /// they need the same unit anchor as the class body to resolve compunit-
    /// private routines. Method parameter defaults use this metadata before the
    /// method's routine frame exists as well.
    pub(crate) class_declaring_units: std::sync::Arc<HashMap<String, Symbol>>,
    /// #7797: the compunit that declared each `use`/`need`/`require`d
    /// top-level package, keyed by the first `::`-segment of the `use`
    /// argument (or the module's own `unit module` name — the two normally
    /// agree). A package NOT in this map has no known foreign declaring
    /// compunit, so `Interpreter::qualified_name_visible_here` treats a
    /// reference to it as permissive (same-compunit `package Foo { }`
    /// blocks, and a script's own top-level `unit module`, never populate
    /// this table, so they are never mistakenly gated).
    pub(crate) package_declaring_units: std::sync::Arc<HashMap<String, Symbol>>,
    /// Which module declarations resolve where: the #7797 package grants and
    /// ADR-11136's GLOBAL-merge provenance and merges
    /// (`runtime::module_merge::ModuleVisibility`).
    pub(crate) module_visibility: module_merge::ModuleVisibility,
    /// Routines installed by a prelude spliced into a host compunit
    /// (`PRELUDE_SUB_TRAIT`, e.g. NativeCall's `nativecast`/`nativesizeof`).
    /// They deliberately live under `GLOBAL` for every compunit that uses them
    /// and belong to no module's export map, so they are ambient rather than
    /// compunit-private and are never secluded.
    pub(crate) prelude_sub_names: std::sync::Arc<HashSet<Symbol>>,
    pub(crate) lib_paths: std::sync::Arc<Vec<String>>,
    /// Bundled-battery module search paths (`modules/<Dist>/lib` shipped
    /// alongside the binary). Searched *after* every `lib_paths` entry so the
    /// bundle is the lowest-priority source — an explicit `-I`/`MUTSULIB` path,
    /// a project-local module, or an `mzef`-installed (site-repo) version all
    /// shadow it. This is the batteries "floor + independent-update" mechanism
    /// (BATTERIES.md §3/§6). Resolved once at startup (exe-relative, or via
    /// `MUTSU_BUNDLE_DIR`).
    pub(crate) bundled_lib_paths: std::sync::Arc<Vec<String>>,
    /// Set while `require` resolves a module: a missing `Test::`-namespace
    /// module must surface as a catchable X::CompUnit::UnsatisfiedDependency
    /// instead of the silent no-op `use Test::Util` relies on.
    pub(crate) require_propagates_missing_module: bool,
    /// Distribution selectors (`:ver`/`:auth`/`:api`) of the `use` currently
    /// being resolved, split off the module name by `use_module_with_tags` and
    /// consulted by `resolve_module_path` to pick among installed dists that
    /// provide the same short name. Saved/restored around each load so a
    /// transitive `use` resolves with its own (usually absent) selectors.
    /// The `-M` modules of the command line, re-announced to the parser
    /// before the mainline parse so their exports are lexically in scope
    /// (`set_compiling_options`, `run`).
    pub(crate) preload_modules: Vec<String>,
    pub(crate) pending_dist_selectors: Vec<(String, String)>,
    /// Arguments passed to the `use` currently being loaded (`use Foo "a", "b"`
    /// / `use Foo <a b c>`), evaluated by the caller and pushed for the
    /// `UseModule` op. Consumed once by `load_module`, which snapshots them into
    /// a local before running the module body (so a transitive `use` inside the
    /// body cannot see them) and hands them to the module's `sub EXPORT`.
    pub(crate) pending_use_export_args: Option<Vec<Value>>,
    /// An `&EXPORT` sub a module *imported* from another module's EXPORT map
    /// (the Slangify pattern: `sub EXPORT($grammar, ...) { ...; Map.new:
    /// '&EXPORT' => &inner-EXPORT }`), keyed by the name of the module that was
    /// loading when the import happened. `apply_module_export` consumes the
    /// entry: the imported sub becomes that module's own EXPORT for *its*
    /// importers, called with their `use` arguments.
    pub(crate) pending_inner_export_subs: ValueMap,
    /// Each loaded module's EXPORT (own `sub EXPORT` or a Slangify-style
    /// imported one), remembered so a re-`use` of the already-loaded module
    /// can run it again with the new import's arguments.
    pub(crate) module_export_defs:
        HashMap<String, crate::runtime::runtime_module_export_sub::ModuleExportDef>,
    pub(crate) loaded_modules: std::sync::Arc<HashSet<String>>,
    /// Package-qualified routine keys a module load introduced (`M::helper`,
    /// `M::EXPORT::ALL::foo`) — never the bare `GLOBAL::` import aliases, which
    /// stay lexical to the importing scope.
    ///
    /// `loaded_modules` is never rolled back, so these must not be either: a
    /// scope that restores the routine registry wholesale (a bare block, an
    /// `EVAL`) would otherwise leave the module marked as loaded while its own
    /// routines are gone, and a re-`use` — being a no-op — could not bring them
    /// back. See `reinstate_module_functions`.
    pub(crate) module_registered_functions: std::sync::Arc<HashSet<Symbol>>,
    /// Packages that received a routine import from a role's own deferred
    /// `use`/`need` body statement (`run_role_deferred_use_stmt`). Consulted
    /// by method dispatch (`vm_method_dispatch.rs`) as an extra reason to set
    /// `current_package` to the receiver's class even when none of the
    /// existing fast-path conditions (class-scoped subs, package lexicals, a
    /// `::`-qualified owner name) apply — a FLAT (non-namespaced) role/class
    /// name can still own package-qualified imported routines that
    /// `bare_name_packages()` can only find by walking outward from itself
    /// (#8646 shape 1).
    pub(crate) packages_with_deferred_use_imports: std::sync::Arc<HashSet<Symbol>>,
    /// The `GLOBAL::`-qualified keys of routines spliced in as a PRELUDE
    /// (`PRELUDE_SUB_TRAIT` — mutsu's NativeCall helpers).
    ///
    /// `module_registered_functions` deliberately does not let its `GLOBAL::`
    /// members be reinstated after an ordinary scope rollback, because a file
    /// with no `unit module` runs its body at `current_package() == GLOBAL` and
    /// its `sub foo is export` is then indistinguishable from an alias
    /// installed *for the importing scope* (`{ require NoModule <&bar>; }` must
    /// not leak `&bar`). A prelude routine carries no such ambiguity: it is
    /// ambient compunit machinery, never an import alias, and every compunit
    /// that needs one carries an identical copy of which only the first
    /// registration wins. Dropping it on a rollback therefore left a module
    /// loaded inside a routine call — `lives-ok { EVAL 'use M' }` — permanently
    /// unable to resolve the helper its own body calls, since `loaded_modules`
    /// still claimed it was loaded and the later real `use` short-circuited.
    pub(crate) prelude_registered_functions: std::sync::Arc<HashSet<Symbol>>,
    /// For each prelude key in `prelude_registered_functions`, the compilation
    /// units the declaration was actually spliced into (`?FILE` at registration
    /// time; `main_unit()` for the main script).
    ///
    /// The registration is process-global by design — a prelude routine has to
    /// be callable by bare name from a method body running under any package
    /// (see `NATIVECALL_SUB_PRELUDES`) — but its *visibility* is not: rakudo
    /// exports these from `NativeCall.rakumod`, so a compunit that never
    /// mentioned NativeCall must not see them. Without this, one module's
    /// `use NativeCall` made `&nativecast` resolvable from every scope in the
    /// process, including the script that merely `use`d that module two levels
    /// up (GH #7612).
    ///
    /// A splice happens per compunit and is idempotent (the first registration
    /// wins, later ones return `Unchanged`), so this set is what records the
    /// later ones. `prelude_visible_here` reads it.
    pub(crate) prelude_declaring_units: std::sync::Arc<HashMap<Symbol, HashSet<Symbol>>>,
    /// The package-qualified globals (`Base::flag`, `$NativeLibs::config`) each
    /// loaded module declared with `our`, keyed by module name.
    ///
    /// The routine-registry counterpart above cannot cover these: `our`
    /// variables live in `env`, and every scope that restores `env` wholesale —
    /// a sub call, a block, an `EVAL` — drops the ones a module load nested
    /// inside it created. `loaded_modules` is never rolled back, so a later
    /// `use` of that module is a no-op and could not bring them back. The
    /// already-loaded path of `use_module_with_tags_inner` reinstates whatever
    /// is missing from here instead.
    pub(crate) module_package_globals: std::sync::Arc<HashMap<String, Vec<(Symbol, Value)>>>,
    pub(crate) need_hidden_classes: std::sync::Arc<HashSet<String>>,
    /// CompUnit::Repository::Installation state (`.loaded` units and the symbols
    /// pulled in by `.need` but not yet merged into GLOBAL).
    ///
    /// Boxed: the whole `Interpreter` is moved by value into a `VM` that lives on
    /// the stack (see `run_block_raw`), and nested module loads stack full copies,
    /// so keeping rarely-used state off the inline struct preserves stack budget.
    pub(crate) cur_repo: Box<CurRepoState>,
    /// Package names declared via `package X {}` during the current module
    /// loading chain. Saved/restored around each top-level `use_module_with_tags`
    /// call so it only contains packages from the current loading chain.
    pub(crate) chain_declared_packages: std::sync::Arc<HashSet<String>>,
    /// What a loaded module's mainline declares directly at its top level,
    /// kept off the env the module body ran in (ADR-0084 §2 groups 1 and 2),
    /// and the depths the executing mainline started at. See
    /// `runtime::toplevel_callable_ids` and `runtime::toplevel_package_symbols`.
    pub(crate) module_toplevel: toplevel_callable_ids::ModuleToplevel,
    /// Maps module names to the set of packages declared during their loading.
    /// Used to propagate package declarations when a module is re-used.
    pub(crate) module_packages: std::sync::Arc<HashMap<String, HashSet<String>>>,
    pub(crate) module_load_stack: Vec<String>,
    /// The current distribution context ($?DISTRIBUTION).
    pub(crate) current_distribution: Option<Value>,
    /// `routine_stack` height when the in-progress module load established
    /// `current_distribution`. Frames at or above it were pushed by code the
    /// loading module called, so they are the ones whose own distribution owns a
    /// `%?RESOURCES` they read; frames below belong to whoever triggered the
    /// load and must not shadow the module being loaded. See
    /// `build_resources_for_package`.
    pub(crate) current_distribution_frame_floor: usize,
    /// Maps package names to their distribution context.
    /// Populated during module loading so OTF compilation can resolve $?DISTRIBUTION.
    pub(crate) package_distributions: std::sync::Arc<ValueMap>,
    /// The module's other file-scope bare names — `constant`s and sigilless
    /// declarations its own routines close over — keyed the same way as
    /// `package_type_aliases`, and lost for the same reason. Consulted by
    /// `module_scope_lexical` as the LAST resort in bareword resolution, just
    /// before the undeclared-bareword-as-`Str` fallback, so a live `env` binding
    /// always wins. Distinct from `package_lexicals`, which is the *mutable*
    /// package-block `my` store with its own writeback path; these are a module's
    /// immutable file-scope terms. `NativeHelpers::Blob`'s `MoarVM::Guts::REPRs`
    /// is the motivating case: `constant Offset` is read by the exported
    /// `OBJECT_BODY` sub of the same module, and resolved to the string
    /// `"Offset"` once the frame that loaded the module was gone.
    ///
    /// For a `unit` compunit's own `constant`s and enum values this is no longer
    /// a fallback but the ONLY store: `load_module_inner` drops their `env`
    /// binding at the end of the load, because the module body ran in the
    /// caller's env and rakudo makes those names package symbols of the
    /// compunit rather than names the importer sees bare (#7787). The value
    /// stays here so the declaring module's own routines and methods still read
    /// it; see `collect_unit_package_scope_names`.
    pub(crate) module_scope_lexicals: std::sync::Arc<PackageLexicals>,
    /// Names the module currently being loaded imported from another module,
    /// accumulated by `import_module` and folded into `module_scope_lexicals`
    /// when the load finishes. The env diff `load_module` takes cannot see these:
    /// re-importing a name a *previously* loaded module already installed adds
    /// nothing to `env`, so `DBDish::mysql::StatementHandle`'s `use
    /// DBDish::mysql::Native` looked like a no-op even though `intptr` is part of
    /// its lexical scope. Saved/restored around each nested load.
    pub(crate) module_imported_names: Vec<(String, Value, Option<Value>)>,
    /// Sigilless terms a `sub EXPORT` hook installed while the module being
    /// loaded imported it (see `install_export_symbol`). Folded into that
    /// module's `module_scope_lexicals` when its load finishes, but never into
    /// `module_imported_lexical_names`, whose entries beat a same-keyed
    /// `$scalar`. Saved/restored around each nested load (#9389).
    pub(crate) module_export_terms: Vec<(String, Value)>,
    /// Imported bare names recorded for each module owner. This is narrower
    /// than `module_scope_lexicals`: the latter also contains a module's own
    /// `our`/class-body names, while the VM's env fallback must only redirect
    /// aliases imported from a nested module.
    pub(crate) module_imported_lexical_names: std::sync::Arc<PackageKeyed<bool>>,
    /// The module name owning each loaded source file. Ordinary (non-`unit`)
    /// module files register top-level routines under `GLOBAL`, but their
    /// private file-scope names remain lexical to the module.
    pub(crate) module_source_packages: std::sync::Arc<rustc_hash::FxHashMap<Symbol, Symbol>>,
    /// Compilation units declared by `unit module`/`unit class` files, keyed by
    /// their compilation-unit symbol. A unit module body runs under GLOBAL, so
    /// its routines need this metadata after the load has finished in order to
    /// resolve the module's own imported aliases lexically.
    pub(crate) unit_module_packages: std::sync::Arc<rustc_hash::FxHashMap<Symbol, Symbol>>,
    /// Declared unit package by requested module path. A module file may be
    /// loaded as `A::B` while declaring `unit module A::C`.
    pub(crate) module_declared_unit_packages: std::sync::Arc<rustc_hash::FxHashMap<Symbol, Symbol>>,
    /// Exported subroutine symbols by package and export tag.
    pub(crate) exported_subs: std::sync::Arc<ExportTagTable>,
    /// Exported variable/constant symbols by package and export tag.
    pub(crate) exported_vars: std::sync::Arc<HashMap<String, HashMap<String, HashSet<String>>>>,
    /// Trait-modified routine values (e.g. a sub with a custom `is` trait that
    /// mixed a role into it) keyed by package and routine name. Captured at
    /// `is export` registration time so `import` can restore the `&name` env
    /// binding with the role mixed in, rather than just the plain FunctionDef.
    pub(crate) exported_sub_values: std::sync::Arc<HashMap<String, ValueMap>>,
    /// Regex bodies of `token`/`rule`/`regex` declarations marked `is export`,
    /// keyed by the module being loaded and the declarator's name.
    ///
    /// A regex declarator does not live in `Registry::functions` like a sub —
    /// it lives in `Registry::token_defs` (ADR-0009: no compiled body), and a
    /// *lexical* one (`my token foo`) is dropped by the block-scope restore
    /// when the module's own scope exits. So the defs are captured here at
    /// registration time and re-installed under the importing package by
    /// `import_module`, which is what makes both `&foo` and `<foo>` resolve in
    /// the importer.
    pub(crate) exported_token_defs: std::sync::Arc<ExportedTokenDefs>,
    /// Mirrored export tables for modules declared with `unit module X`
    /// when the actual runtime package registration used "GLOBAL".
    /// Populated during `load_module` so that `import_module` can perform
    /// tag validation and raise `X::Import::NoSuchTag` for bad tags.
    pub(crate) unit_module_exported_subs: std::sync::Arc<ExportTagTable>,
    /// Stack of unit-module names currently being loaded; used by
    /// `register_exported_sub` to mirror GLOBAL registrations into
    /// `unit_module_exported_subs`.
    pub(crate) unit_module_loading_stack: Vec<String>,
    /// The package a `use`/`need` must import INTO, when the statement is run
    /// by a body that is not a compunit mainline: a role's deferred body, or an
    /// `augment` body. Both re-point `current_package` at the declaring package
    /// around such a statement, but module loading derives the importer package
    /// from `unit_module_loading_stack`, which still names the compunit being
    /// loaded -- so the import landed under the COMPOSING class instead of the
    /// role, and nothing the role imported was reachable from the role's own
    /// package afterwards. That is the half of #8842 that survives anchoring an
    /// attribute default on its declaring package: the anchor was right and the
    /// package's alias table was empty.
    ///
    /// `None` outside such a body, which is every ordinary `use`.
    pub(crate) import_target_package: Option<String>,
    /// #7797: stack of compunits whose OWN mainline is currently executing
    /// via `load_module_inner`'s `run_block`, pushed/popped around exactly
    /// the same window as `unit_module_loading_stack` (but keyed by every
    /// load, not only ones with a `unit module`/`unit class` name). Each
    /// entry also carries `routine_stack.len()` at the moment it was pushed,
    /// so `Interpreter::executing_unit_sym_for_module_load` can tell "still
    /// directly in this module's mainline" (the length hasn't grown, so
    /// nothing has been CALLED since) from "a routine call happened since"
    /// (the length grew, so a fresh frame -- possibly from yet another
    /// compunit -- is what's actually running now).
    ///
    /// `Interpreter::executing_unit_sym` cannot serve this purpose on its
    /// own: it prioritizes `routine_stack`'s topmost frame, which is correct
    /// for an ordinary call but wrong here — a module body runs via
    /// `run_block`, which pushes no routine frame, so a `use` (or any other
    /// qualified-name resolution) reached from a module's mainline while
    /// some UNRELATED routine call is still on the stack (`DBIish
    /// .install-driver` doing `require ::($module)`, whose loaded module in
    /// turn does its own top-level `use NativeLibs;`, or even just reads
    /// `NativeLibs::is-win` in a top-level `constant` initializer) would
    /// otherwise misattribute to that routine's own compunit instead of to
    /// the module whose mainline is actually running. `?FILE` alone has the
    /// opposite problem: an ordinary (non-loading) routine call never
    /// updates it, so consulting it OUTSIDE a load would misattribute to
    /// whichever compunit happened to load last. This stack is unambiguous
    /// exactly because `load_module_inner` is the only writer.
    pub(crate) module_loading_unit_stack: Vec<(Symbol, usize)>,
    /// Exports each module registered while it was the module currently being
    /// loaded (attributed via `module_load_stack`), mapping module -> name ->
    /// tags. Unlike `exported_subs["GLOBAL"]`, which pools every unit-module
    /// export ever hoisted, this correctly attributes an export to the module
    /// that declared it. The `use MOD` tag-filter consults this so it only
    /// hides MOD's *own* exports and never a symbol MOD imported from a
    /// transitively-`use`d module (which MOD's methods must still resolve).
    pub(crate) module_owned_exports: std::sync::Arc<ExportTagTable>,
    /// Qualified classes and roles declared by each module's own body. A
    /// module without a `unit` declarator may still declare a type in an
    /// unrelated package (for example `class Test::Handle`); that package is
    /// visible to a direct importer, but declarations from the module's
    /// dependencies are not. The load stack lets registration attribute the
    /// type to the correct compunit while nested modules are loading.
    pub(crate) module_owned_types:
        std::sync::Arc<HashMap<String, runtime_module::ModuleOwnedTypes>>,
    /// When true, `is export` trait is ignored (used by `CompUnit::Repository.need`
    /// to load without importing; the `need` statement itself registers exports).
    pub(crate) suppress_exports: bool,
    /// True while a `need` (or an empty-import `use Mod ()`) loads a module:
    /// its exports are registered but imported nowhere, so an exported
    /// `MAIN` must not become the program's MAIN.
    pub(crate) loading_without_import: bool,
    /// Stack of snapshots for lexical import scoping.
    /// Each entry saves (function_keys, class_names, newline_mode, strict_mode, fatal_mode)
    /// before a block with `use`.
    pub(crate) import_scope_stack: Vec<ImportScopeSnapshot>,
    /// `import_scope_stack.len()` at the in-position `use` whose module load is
    /// running, or `None` outside one (the BEGIN-time preload, a `require`).
    /// It is what `$*R.find-attach-target` resolves a module's EXPORT-time
    /// request against (`runtime::attach_target`).
    pub(crate) use_attach_depth: Option<usize>,
    /// Routine aliases installed by an import, keyed by their target package
    /// and name. A local `sub` may shadow such an alias, but two declarations
    /// in the same scope must still be rejected. The set is restored together
    /// with routine-registry snapshots so a nested lexical declaration cannot
    /// consume an import belonging to its caller.
    pub(crate) imported_routine_aliases: std::sync::Arc<HashSet<Symbol>>,
    /// Export tags inherited by a local multi that extends an imported
    /// exported proto. Rakudo exports the whole family, including the local
    /// candidate, under those tags.
    pub(crate) imported_exported_proto_tags: std::sync::Arc<HashMap<Symbol, HashSet<String>>>,
    /// Environment keys installed by imports, paired with the spelling that
    /// should appear in a lexical pseudo-stash. Scalar exports are stored in
    /// `env` without their `$` sigil, so the display spelling cannot be
    /// reconstructed from the environment key alone.
    pub(crate) imported_env_aliases: HashMap<Symbol, Symbol>,
    pub(crate) strict_mode: bool,
    pub(crate) fatal_mode: bool,
    /// Whether `use fatal` — explicit, or implied by an enclosing `try` body —
    /// is lexically active for the code currently executing (#9521, #11391).
    /// Every Failure explosion check reads this channel: the store-time
    /// checks (`SetLocal`/`SetGlobal`/`AssignExpr`/`SinkPopAssign`), the
    /// sunk-list check, and the `explode_if_fatal_failure_in_*` composite/
    /// call-argument checks. It is separate from `fatal_mode`, which also
    /// carries `try`'s marking but with genuinely DYNAMIC scope: a deferred
    /// `.map`/`.grep` `Seq`'s `SeqSource::MapGrep::fatal` capture
    /// (`resolution_map_grep.rs`) reads `fatal_mode`, because `try`'s marking
    /// legitimately reaches into a called routine's own deferred-Seq
    /// construction (`t/collections/transform/map-callback-runs-at-consumption.t`,
    /// verified against `raku`). An explosion, by contrast, is lexical: a
    /// routine declared outside a `use fatal` block and called from inside
    /// one — or from inside a `try` — keeps a Failure it stores or sinks soft
    /// (`t/exceptions/try-fatal-is-lexical.t`). Driven by the same statements
    /// that set `fatal_mode` for `use fatal`/`no fatal`, by import-scope
    /// save/restore (`save_pragma_state`/`restore_pragma_state`,
    /// `push_import_scope`/`pop_import_scope`) and by a genuine `try`
    /// (`vm_try_catch_ops.rs`), and ALSO reset at every routine-call entry to
    /// the callee's own `CompiledFunction::captured_fatal_mode` (baked at
    /// compile time from `Compiler::fatal_pragma_active`) or, for a closure,
    /// to the value captured from this field when the closure was built
    /// (`vm_closure_build.rs`). The deferred-`Seq`-consumption pull never
    /// touches it.
    pub(crate) lexical_fatal_mode: bool,
    /// True only on the throwaway nested `Interpreter` `eval-lives-ok`/
    /// `eval-dies-ok` construct to run their code string.
    /// Real raku's own `Test.rakumod` implements both via a helper (`sub
    /// eval_exception($code) { try { EVAL($code) }; $! }`) that calls `EVAL`
    /// with NO explicit `context =>` argument -- unlike `throws-like`, which
    /// explicitly passes `context => $caller-context` (the actual calling
    /// program's lexical scope). An `EVAL` with no context defaults to the
    /// lexical scope where the `EVAL` keyword is textually written, which for
    /// `eval_exception` is Test.rakumod's own module scope, not the calling
    /// program's -- so a `class Foo {}` inside `eval-lives-ok`'s string
    /// installs under a package distinct from the caller's, and does NOT
    /// conflict with a same-named class the calling program already declared
    /// (verified against real raku: `class A {}; eval-lives-ok 'class A {}'`
    /// lives). `throws-like`'s explicit caller context is why its EVAL'd
    /// string DOES conflict with an outer same-named class (also verified).
    /// mutsu provides `Test` natively (no real Test.rakumod compunit to
    /// inherit a distinct package from), so this flag stands in for that:
    /// it gates OFF `check_eval_class_redeclarations`'s cross-boundary
    /// `has_class` check (the same-EVAL-string duplicate-declaration check
    /// is unaffected) for the nested interpreter these two functions create.
    pub(crate) suppress_cross_eval_class_redeclaration_check: bool,
    /// `use attributes :D/:U/:_` pragma — applies default smiley to unsmiley'd attribute type constraints.
    /// Empty string means no pragma active.
    pub(crate) attributes_pragma: String,
    /// Names of classes/roles/enums registered while loading a foreign
    /// compunit via runtime `require` (`require_load_from_file`). Unlike a
    /// plain `class`/`role`/`enum` declaration -- which always installs into
    /// the enclosing PACKAGE no matter how deeply it is nested inside call
    /// frames (#8683) -- `require` installs its symbols into the CURRENT
    /// LEXICAL SCOPE of the `require` statement itself, so a type it loads
    /// while a call frame is live must keep the ordinary frame-scoped `env`
    /// entry as its ONLY route to indirect (`::()`) lookup, and correctly
    /// stop resolving once that frame returns
    /// (`roast/S11-modules/require.t`'s `GlobalOuter.load` requiring
    /// `GlobalInner`: `::('GlobalInner')` succeeds while `.load` is still
    /// running, and fails again once it returns). This set marks such a name
    /// so `resolve_indirect_type_name`'s registry-backed fallback (which
    /// exists precisely to survive a returned frame for the #8683 case) never
    /// trusts it -- the frame-scoped `env` check earlier in that function is
    /// untouched by this set and keeps giving a require-loaded type its
    /// correct, frame-lifetime-bound visibility.
    pub(crate) require_loaded_type_names: std::sync::Arc<HashSet<String>>,
    /// When true, module precompilation cache is enabled.
    pub(crate) precomp_enabled: bool,
    /// When true, `augment class` is allowed (set by `use MONKEY-TYPING` or `use MONKEY`).
    pub(crate) monkey_typing: bool,
    /// Compiled bodies of subs defined in `use`d modules, captured at module-load
    /// time and keyed by the sub's body/signature fingerprint. Unlike the per-call
    /// `otf_compile_cache` (which a worker thread starts *empty* — every thread
    /// re-OTF-compiles a module sub into a *distinct* body, giving distinct `state`
    /// cells), this map is a snapshot shared by value into every spawned thread's
    /// clone. A module sub routed through the same captured body across threads
    /// reaches its `state` variable under a stable cross-thread key, so
    /// `await (^N).map: { start f() }` accumulates into one shared cell — the piece
    /// the per-thread OTF path could not provide. Populated by `load_module`; read
    /// via `imported_state_body_for_def`. Empty for programs that `use` nothing.
    pub(crate) imported_compiled_fns: HashMap<u64, std::sync::Arc<CompiledFunction>>,
    /// Bare names for which a `sub EXPORT`'s returned map installed an
    /// `&name` value into `env` (`install_export_symbol`). A custom EXPORT
    /// hook can re-export a routine wrapped in a closure under the same bare
    /// name it wraps (`'&greet' => -> |c { greet |c, :extra } }`), so a
    /// bareword call of that name must keep re-checking `env` for the
    /// installed override instead of caching straight through to the
    /// registered package sub of the same name (#8746). Unlike
    /// `amp_param_shadowed_names`, the env value here is not gated by
    /// `free_var_syms`: an `EXPORT`-installed override is a lexical import of
    /// the whole importing compunit, not a value inherited from an unrelated
    /// caller frame, so every bareword call to the name within that unit's
    /// reach must see it. Populated at export-symbol installation; checked
    /// cheaply (guarded by `is_empty()`) on each call. Never removed, mirroring
    /// `amp_param_shadowed_names`.
    pub(crate) export_amp_override_names: std::collections::HashSet<Symbol>,
    /// The `&name` callables a `sub EXPORT` map handed to each compunit, keyed
    /// by the (importing file, bare name). `env` holds one `&name` slot for
    /// the whole program, so a second import of the same name (the script
    /// importing a module that re-exports `&to-json` over the `&to-json` it
    /// itself imported) overwrites the first; a bareword call from the first
    /// importer's own unit must still reach what THAT unit imported. Consulted
    /// only when an installed override is rejected as declared in the calling
    /// unit (`callable_declared_in_unit_of`), so it costs nothing on any other
    /// call. Populated at export-symbol installation; never removed.
    pub(crate) unit_imported_callables: std::collections::HashMap<(Symbol, Symbol), Value>,
    /// Sigilless bare names a `sub EXPORT`'s returned map installed into `env`
    /// (`install_export_symbol`). The CORE term keywords `True`/`False`/`Nil`/
    /// `Empty`/`Any` are ordinary lexical bindings in Raku, so such an import
    /// legitimately shadows them for the importing compunit — which mutsu's
    /// parse-time folding of those keywords to literals otherwise made
    /// impossible (#9047, `Logic::Ternary`). `OpCode::GetShadowableTerm`
    /// consults this set before reading `env`, so a same-named key that got
    /// there some OTHER way (an `our $True`, whose scalar read is env-keyed
    /// without its sigil) can never be mistaken for a shadowing import.
    /// Populated at export-symbol installation and never removed, mirroring
    /// `export_amp_override_names`.
    pub(crate) export_term_override_names: std::collections::HashSet<Symbol>,
}

impl ModuleState {
    pub(crate) fn new() -> Self {
        Self {
            native_call_specs: Default::default(),
            imported_operator_names: Default::default(),
            operator_import_units: Default::default(),
            operator_import_gen: 0,
            unit_private_routines: Default::default(),
            unit_private_names: Default::default(),
            class_declaring_units: Default::default(),
            package_declaring_units: Default::default(),
            module_visibility: Default::default(),
            prelude_sub_names: Default::default(),
            lib_paths: Default::default(),
            bundled_lib_paths: Interpreter::bundled_lib_paths_shared(),
            require_propagates_missing_module: false,
            preload_modules: Vec::new(),
            pending_dist_selectors: Vec::new(),
            pending_use_export_args: None,
            pending_inner_export_subs: ValueMap::default(),
            module_export_defs: HashMap::new(),
            loaded_modules: Default::default(),
            module_registered_functions: Default::default(),
            packages_with_deferred_use_imports: Default::default(),
            prelude_registered_functions: Default::default(),
            prelude_declaring_units: Default::default(),
            module_package_globals: Default::default(),
            need_hidden_classes: Default::default(),
            cur_repo: Box::new(CurRepoState::default()),
            chain_declared_packages: Default::default(),
            module_toplevel: Default::default(),
            module_packages: Default::default(),
            module_load_stack: Vec::new(),
            current_distribution: None,
            current_distribution_frame_floor: 0,
            package_distributions: Default::default(),
            module_scope_lexicals: std::sync::Arc::new(PackageLexicals::default()),
            module_imported_names: Vec::new(),
            module_export_terms: Vec::new(),
            module_imported_lexical_names: std::sync::Arc::new(PackageKeyed::default()),
            module_source_packages: Default::default(),
            unit_module_packages: Default::default(),
            module_declared_unit_packages: Default::default(),
            exported_subs: Default::default(),
            exported_sub_values: Default::default(),
            exported_token_defs: Default::default(),
            exported_vars: Default::default(),
            unit_module_exported_subs: Default::default(),
            unit_module_loading_stack: Vec::new(),
            import_target_package: None,
            module_loading_unit_stack: Vec::new(),
            module_owned_exports: Default::default(),
            module_owned_types: Default::default(),
            suppress_exports: false,
            loading_without_import: false,
            import_scope_stack: Vec::new(),
            use_attach_depth: None,
            imported_routine_aliases: Default::default(),
            imported_exported_proto_tags: Default::default(),
            imported_env_aliases: HashMap::new(),
            strict_mode: false,
            fatal_mode: false,
            lexical_fatal_mode: false,
            suppress_cross_eval_class_redeclaration_check: false,
            attributes_pragma: String::new(),
            require_loaded_type_names: Default::default(),
            precomp_enabled: crate::precomp::enabled_by_default(),
            monkey_typing: false,
            imported_compiled_fns: HashMap::new(),
            export_amp_override_names: std::collections::HashSet::new(),
            unit_imported_callables: std::collections::HashMap::new(),
            export_term_override_names: std::collections::HashSet::new(),
        }
    }

    /// The spawned thread's copy: load-time knowledge (loaded modules,
    /// export/import tables, pragmas) is carried over; the per-load stacks
    /// and handoff state of an in-flight `use` start empty.
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self {
            native_call_specs: self.native_call_specs.clone(),
            imported_operator_names: self.imported_operator_names.clone(),
            operator_import_units: self.operator_import_units.clone(),
            operator_import_gen: self.operator_import_gen,
            unit_private_routines: self.unit_private_routines.clone(),
            unit_private_names: self.unit_private_names.clone(),
            class_declaring_units: self.class_declaring_units.clone(),
            package_declaring_units: self.package_declaring_units.clone(),
            module_visibility: self.module_visibility.clone(),
            prelude_sub_names: self.prelude_sub_names.clone(),
            lib_paths: self.lib_paths.clone(),
            bundled_lib_paths: self.bundled_lib_paths.clone(),
            require_propagates_missing_module: false,
            preload_modules: Vec::new(),
            pending_dist_selectors: Vec::new(),
            pending_use_export_args: None,
            pending_inner_export_subs: ValueMap::default(),
            module_export_defs: HashMap::new(),
            loaded_modules: self.loaded_modules.clone(),
            module_registered_functions: self.module_registered_functions.clone(),
            packages_with_deferred_use_imports: self.packages_with_deferred_use_imports.clone(),
            prelude_registered_functions: self.prelude_registered_functions.clone(),
            prelude_declaring_units: self.prelude_declaring_units.clone(),
            module_package_globals: self.module_package_globals.clone(),
            need_hidden_classes: self.need_hidden_classes.clone(),
            cur_repo: self.cur_repo.clone(),
            chain_declared_packages: self.chain_declared_packages.clone(),
            module_toplevel: self.module_toplevel.for_thread(),
            module_packages: self.module_packages.clone(),
            module_load_stack: Vec::new(),
            current_distribution: self.current_distribution.clone(),
            current_distribution_frame_floor: 0,
            package_distributions: self.package_distributions.clone(),
            module_scope_lexicals: self.module_scope_lexicals.clone(),
            module_imported_names: Vec::new(),
            module_export_terms: Vec::new(),
            module_imported_lexical_names: self.module_imported_lexical_names.clone(),
            module_source_packages: self.module_source_packages.clone(),
            unit_module_packages: self.unit_module_packages.clone(),
            module_declared_unit_packages: self.module_declared_unit_packages.clone(),
            exported_subs: self.exported_subs.clone(),
            exported_vars: self.exported_vars.clone(),
            exported_sub_values: self.exported_sub_values.clone(),
            exported_token_defs: self.exported_token_defs.clone(),
            unit_module_exported_subs: self.unit_module_exported_subs.clone(),
            unit_module_loading_stack: Vec::new(),
            import_target_package: None,
            module_loading_unit_stack: Vec::new(),
            module_owned_exports: self.module_owned_exports.clone(),
            module_owned_types: self.module_owned_types.clone(),
            suppress_exports: false,
            loading_without_import: false,
            import_scope_stack: Vec::new(),
            use_attach_depth: None,
            imported_routine_aliases: self.imported_routine_aliases.clone(),
            imported_exported_proto_tags: self.imported_exported_proto_tags.clone(),
            imported_env_aliases: self.imported_env_aliases.clone(),
            strict_mode: self.strict_mode,
            fatal_mode: self.fatal_mode,
            lexical_fatal_mode: self.lexical_fatal_mode,
            suppress_cross_eval_class_redeclaration_check: false,
            attributes_pragma: self.attributes_pragma.clone(),
            require_loaded_type_names: self.require_loaded_type_names.clone(),
            precomp_enabled: self.precomp_enabled,
            monkey_typing: self.monkey_typing,
            // Share the parent's captured module-sub bodies by value so a `start`
            // block that calls a module sub with `state` reaches the same compiled
            // body (and thus the same cross-thread `state` cell) the parent used.
            imported_compiled_fns: self.imported_compiled_fns.clone(),
            // Which env keys an EXPORT hook installed is load-time knowledge,
            // not per-thread run state: a routine the parent loaded may run on
            // the thread and must still see its module's hook-installed names
            // (`start { ... }` around Terminal::MultiProgress's `t.hide-cursor`,
            // #9339).
            export_amp_override_names: self.export_amp_override_names.clone(),
            unit_imported_callables: self.unit_imported_callables.clone(),
            export_term_override_names: self.export_term_override_names.clone(),
        }
    }
}
