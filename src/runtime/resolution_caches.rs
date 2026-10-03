//! The `caches` subsystem of ADR-10779: resolution memos, method/multi
//! caches, call-lane tables and compile caches. Everything here is derived
//! state -- it can be dropped and rebuilt from the registry and the compiled
//! code at any time -- so a spawned thread starts with all of it empty.

use super::*;

/// `private_resolve_cache`'s key: (receiver class, `!name`, argument type keys).
pub(crate) type PrivateResolveKey = (Symbol, Symbol, Vec<Symbol>);
/// `private_resolve_cache`'s entry: the winning (owner, candidate), if any.
pub(crate) type PrivateResolved = Option<(Symbol, Arc<MethodDef>)>;

#[derive(Default)]
pub(crate) struct ResolutionCaches {
    /// One-entry memo of the last closure-capture env, so a closure literal
    /// created over and over from an unchanged scope (`.map({...})` in a loop)
    /// stops rebuilding the same map every time. See
    /// [`crate::vm::vm_capture_cache`]. Boxed like `cur_repo`: it is touched
    /// only by closure creation, and inlining ~180 bytes of it would push the
    /// per-opcode hot fields apart for every program.
    pub(crate) capture_cache: Box<crate::vm::vm_capture_cache::CaptureCache>,
    pub(super) protect_block_cache: ProtectBlockCache,
    /// See `CarrierCompileCache`: reuses `eval_block_value_inner`'s carrier
    /// compile across repeated calls to the same `SubData` id instead of
    /// recompiling its AST every time. Opt-in per call site via
    /// `eval_block_value_cached`/`eval_test_block_value`'s `cache_id`
    /// parameter — starts empty per thread (pure recomputable optimization).
    pub(super) carrier_compile_cache: CarrierCompileCache,
    /// See [`WheneverBodySplit`]: the phaser split of a `whenever` body, reused
    /// across every registration from one parse site. Pure recomputable
    /// optimization; starts empty per thread.
    pub(super) whenever_body_splits: HashMap<WheneverBodyKey, WheneverBodySplit>,
    /// Parsed `s///` / `S///` replacement plans, keyed by the replacement's
    /// source text (see `vm::vm_subst_repl`). The replacement is a `qq` quote,
    /// so it is parsed with the real interpolation grammar; caching keeps a
    /// `:g` substitution from re-parsing it per match and gives the dynamic
    /// plan a stable carrier-compile-cache id. (An assignment-form RHS is a
    /// compiled thunk closure and never reaches this cache.)
    pub(crate) subst_repl_plans: HashMap<String, crate::vm::vm_subst_repl::SubstReplPlan>,
    /// The map/grep/`.first` inline-loop fast paths (`resolution_map_grep.rs`)
    /// compile the callback block once per `.map()`/`.grep()`/`.first()` CALL
    /// and then run every item through the same compiled bytecode via
    /// `run_reuse` -- cheap when one call processes many items, but a block
    /// literal declared *inside* a loop (`for @blocks { @xs.map({ ... }) }`,
    /// the shape `Digest::RIPEMD` hits once per compression round) is a fresh
    /// `SubData` on every outer iteration, so the naive path recompiles its
    /// AST from scratch every time even though the block's own source never
    /// changes. `data.compiled_code` is already an `Arc<CompiledCode>` shared
    /// across every instantiation of the same source closure literal (see
    /// `vm_register_ops::resolve_closure_code` -- it is pulled from the
    /// enclosing scope's `closure_compiled_codes`, baked once at that
    /// enclosing scope's own compile time), so its pointer identity is a
    /// free cache key for this fast path's own (differently-shaped,
    /// tail-normalized) compile. `MapGrepCacheKey` HOLDS a clone of that
    /// `Arc` (not just its address) so the key stays alive for as long as the
    /// cache entry does — a bare `usize` pointer would go unsound the moment
    /// the *original* Arc (e.g. one built fresh per call by a dynamic
    /// `EVAL`/RakuAST closure, never otherwise retained) is dropped and its
    /// address reused by an unrelated later `CompiledCode` allocation, which
    /// would then collide with a stale cache entry
    /// (`t/rakuast-eval-block-arg.t`'s chained `.map().grep()` on one line
    /// caught this during development). Keyed additionally on
    /// `lexically_in_routine` since that is the only other compiler input
    /// drawn from ambient state here. Starts empty per thread (pure
    /// recomputable optimization).
    pub(super) map_grep_compile_cache:
        HashMap<MapGrepCacheKey, (std::sync::Arc<CompiledCode>, std::sync::Arc<CompiledFns>)>,
    /// Compiled bytecode for `gather` block bodies, keyed the same way (pointer
    /// identity of the body's analysis `CompiledCode`). `exec_make_gather_op`
    /// used to run the whole compiler on the body every time the `gather`
    /// EXPRESSION was evaluated, so a `gather` inside a loop re-compiled per
    /// iteration: 3 constant-pool additions per creation, and ~1.75us per body
    /// statement per creation. Kept separate from `map_grep_compile_cache`
    /// because the compile target differs (a body that declares routines is
    /// wrapped in a `Stmt::Block` first), so the two must never share a slot for
    /// the same origin chunk. Starts empty per thread (a pure recomputable
    /// optimization).
    pub(super) gather_compile_cache:
        HashMap<MapGrepCacheKey, (std::sync::Arc<CompiledCode>, std::sync::Arc<CompiledFns>)>,
    pub(super) private_zeroarg_method_cache: HashMap<(String, String), Option<(String, MethodDef)>>,
    /// Per-package memo of the table `establish_grammar_dynamic_vars` computes,
    /// keyed by the `TOKEN_DEFS_GEN` generation it was computed under. A grammar's
    /// `.parse`/subparse is re-entered many times against a stable token registry
    /// (e.g. once per backtrack attempt of an enclosing `<?{ }>` code assertion —
    /// see #8510), so recomputing the full MRO+registry scan on every call is pure
    /// waste once no new token has been registered since. Invalidated wholesale
    /// (by generation mismatch) rather than per-package, since a new token
    /// registration is rare and global.
    pub(super) grammar_dynvar_decls_cache: HashMap<String, (u64, HashMap<String, Vec<String>>)>,
    pub(crate) otf_compile_cache: HashMap<u64, Arc<CompiledFunction>>,
    #[allow(clippy::type_complexity)]
    /// Keyed by `(callee name, callsite package, arity, argument type names)`.
    /// The package is part of the key because `resolve_function_with_types` is
    /// package-sensitive: `PkgA::which` and `PkgB::which` are different
    /// routines reached by the same bare name, and a package-blind key let
    /// whichever package called first answer for both.
    /// The type names are `&'static str` (that is what `value_type_name`
    /// returns), so building a probe key costs one `Vec` and no `String` — this
    /// key is rebuilt on every call that reaches `find_compiled_function`.
    pub(crate) fn_resolve_cache:
        GenCache<(Symbol, Symbol, usize, Vec<&'static str>), (Symbol, u64, String)>,
    /// Memo for the argument-independent plain-routine tail of
    /// `resolve_function_with_types` (#9081); see `plain_fn_resolve_memo.rs`.
    pub(crate) plain_fn_resolve_memo:
        GenCache<plain_fn_resolve_memo::PlainFnResolveKey, Arc<FunctionDef>>,
    /// The version stamp of the registry functions map every generation-tagged
    /// memo above is read and written under: a mirror of
    /// `Registry::functions_version()`, refreshed by
    /// [`Interpreter::invalidate_fn_resolution_for_keys`] and its wholesale
    /// sibling. It is a *name for the map's content*, not a step counter, so it
    /// goes back to its previous value when a scope restore puts a previous map
    /// back — which is what lets a memo outlive the excursion that a
    /// routine-local `my sub` makes of every call (#8314).
    pub(crate) fn_resolve_gen: u64,
    /// Memo for the compiled-key probe chain of a `multi` name
    /// (`find_compiled_function_inner`). `fn_resolve_cache` above deliberately
    /// withholds itself from a multi, because its key cannot tell two calls
    /// apart that share a type signature but pick different candidates — so
    /// every multi call re-ran ~15 `format!`ed key probes against
    /// `compiled_fns`, which for the common shape (the winning candidate is not
    /// in the caller's table) all fail. Keying on the *resolved winner's*
    /// fingerprint sidesteps that: the resolution itself is the sound part, and
    /// the probe outcome is a pure function of this key. Unlike
    /// `fn_resolve_cache` this memo also stores the NEGATIVE answer, which is
    /// the whole point (#7573). Retired with `fn_resolve_cache` by
    /// `fn_resolve_gen`, per entry.
    pub(crate) multi_compiled_key_cache: GenCache<MultiCompiledKey, Option<Symbol>>,
    /// Memo for [`Interpreter::has_multi_candidates`], same key shape as
    /// `has_proto_cache` — `(current_package, innermost lexical_package,
    /// name)` — guarded by `fn_resolve_gen`. Keying on the bare name alone
    /// let a query from one package's context answer for every other
    /// package's: `Config::TOML::Dumper`'s own `multi sub to-toml(Str:D $s)`
    /// registers under `Foo::Dumper::to-toml/…`, invisible from `GLOBAL`, so
    /// the first (correctly negative) `GLOBAL`-context probe cached "false"
    /// for the bare name `to-toml` — then every later probe from *inside*
    /// `Foo::Dumper` reused that stale "false" and let the VM's type-blind
    /// positional light-call cache treat a genuine multi as a monomorphic
    /// function, caching whichever candidate resolved first and reusing it
    /// for every argument type thereafter (#7539).
    pub(crate) multi_candidates_cache: GenCache<(Symbol, Option<Symbol>, Symbol), bool>,
    /// Memo for [`Interpreter::has_proto`], keyed by the full bare-name lookup
    /// context `(current_package, innermost lexical_package, name)` — the
    /// exact inputs `bare_name_packages()` derives the search list from — so
    /// a hit can never answer for the wrong package scope. Invalidated by
    /// `Registry::proto_generation()` (bumped on every `proto_subs`
    /// mutation; the field is private so a bump can't be missed). This probe
    /// used to run 3+ times per `CallFunc` dispatch, each walk paying a
    /// `Vec<String>` + two `format!`s per candidate package.
    pub(crate) has_proto_cache: rustc_hash::FxHashMap<(Symbol, Option<Symbol>, Symbol), bool>,
    pub(crate) has_proto_cache_gen: u64,
    /// Memo for [`Interpreter::has_declared_function`], same key shape as
    /// `has_proto_cache`; guarded by `fn_resolve_gen` like
    /// `multi_candidates_cache` (the `functions` map is what it reads).
    pub(crate) declared_fn_cache: GenCache<(Symbol, Option<Symbol>, Symbol), bool>,
    /// Memo for [`Interpreter::has_multi_function`], same key shape as
    /// `has_proto_cache`; guarded by `fn_resolve_gen`. The uncached probe
    /// scans EVERY registry function key with a `String` resolve per key,
    /// per call.
    pub(crate) multi_fn_cache: GenCache<(Symbol, Option<Symbol>, Symbol), bool>,
    /// Memo for [`Interpreter::bare_name_packages_syms`], keyed by the only two
    /// inputs that list is derived from: the current package and the innermost
    /// routine frame's lexical package.
    ///
    /// The derivation is a pure string walk (split the mangled sub/closure
    /// scope at `::&`, then peel `::` segments outwards), so the memo needs no
    /// invalidation at all — nothing but that pair can change the answer. It is
    /// worth memoizing because the walk allocates a `Vec<String>` plus a
    /// `String` per enclosing package and runs several times per dispatch:
    /// [`Interpreter::has_multi_candidates`], [`Interpreter::has_multi_function`],
    /// [`Interpreter::has_proto`], the candidate gather and
    /// `find_compiled_function_inner`'s probe chain each ask for it
    /// ([#8300](https://github.com/tokuhirom/mutsu/issues/8300)).
    ///
    /// Interior-mutable (and in its own `Box`ed allocation, for the same
    /// aliasing reason as [`Interpreter::readonly_vars`]) because the callers
    /// hold `&self`: the package itself lives behind an `RwLock` precisely so
    /// a temporary switch does not need `&mut self`.
    ///
    /// Having no invalidation, it is never pruned — which is fine because both
    /// key components are drawn from the program's *static* structure, not from
    /// its execution. A package name is a declared package, a `Pkg::&name/2`
    /// routine scope is a declared routine, and a `__state_<pkg>::<name>@<ip>`
    /// scope is a compiled instruction address. So the table is bounded by
    /// program size, like the registry itself, and cannot grow with iteration
    /// count.
    pub(crate) bare_name_packages_memo: BareNamePackagesMemo,
    /// Memo of `resolve_all_multi_candidates_indexed` -- the FULL candidate
    /// list a multi dispatch frame carries for `callsame`/`nextsame` -- keyed by
    /// `(name, current package, frame lexical package)`, the three inputs the
    /// gather reads besides the registry (`bare_name_packages`, the proto
    /// owner). Dropped wholesale when either registry generation it depends on
    /// moves: `fn_resolve_gen` (every function registration/removal) or the
    /// registry's `proto_gen`. Without it every call of a `multi` re-ran the
    /// gather -- package list, prefix strings, key filter, specificity sort --
    /// to rebuild a list that had not changed since the previous call.
    pub(crate) multi_dispatch_candidates_memo: MultiDispatchCandidatesMemo,
    /// `(fn_resolve_gen, proto_gen)` the memo above was filled under.
    pub(crate) multi_dispatch_candidates_memo_gen: (u64, u64),
    /// Keyed by `(callee name, callsite package)` for the same reason as
    /// [`Self::pos_light_call_cache`] below.
    ///
    /// All three name-keyed call caches (this one, `pos_light_call_cache` and
    /// `otf_call_cache`) tag each entry with the `fn_resolve_gen` it was
    /// resolved under instead of being emptied whenever that generation
    /// moves. A routine that declares an inner `my sub` moves it on entry and
    /// moves it back on exit, so the table-wide clear discarded every cached
    /// call target in the program twice per call of that routine, and the next
    /// call of every routine paid the full resolve (#9073).
    pub(crate) light_call_cache: GenCache<(Symbol, Symbol), (Symbol, u64)>,
    /// Keyed by `(callee name, callsite package)`: the same bare name means
    /// different routines in two packages (`PkgA::which` vs `PkgB::which`), and
    /// a name-only key made whichever package called first answer for both.
    pub(crate) pos_light_call_cache: GenCache<(Symbol, Symbol), PosLightTarget>,
    /// The `fn_resolve_gen` the current [`Interpreter::pos_light_ic_epoch`] was
    /// issued under. The direct-mapped `call_ic` slots carry no generation of
    /// their own, so a generation change retires them all by bumping the epoch
    /// — cheap, and the name-keyed entries above survive it and refill them.
    pub(crate) pos_light_call_cache_gen: u64,
    pub(crate) method_resolve_cache:
        rustc_hash::FxHashMap<(Symbol, Symbol), crate::vm::MethodResolveEntry>,
    /// ADR-0019 E3: the generation-keyed resolved-sequence cache (design
    /// decision 5, adr0019-e2-e4-resolver-core (#7540)). Caches the
    /// ordered candidate universe for `(receiver TypeId, method, call shape)`
    /// — not a resolved winner, so unlike `multi_resolve_cache` an ambiguous
    /// per-call ranking never disqualifies an entry from being cached; ranking
    /// against fresh call args happens every time from the cached candidates.
    /// Cleared with the other method caches on any registry generation change
    /// ([`refresh_method_caches_for_generation`](crate::runtime::Interpreter::refresh_method_caches_for_generation)).
    pub(crate) resolved_seq_cache: rustc_hash::FxHashMap<
        (
            crate::type_id::TypeId,
            Symbol,
            resolution_sequence::CallShape,
        ),
        Arc<resolution_sequence::ResolvedSequence>,
    >,
    /// Registry method generation observed when the method caches were last valid.
    pub(crate) method_cache_generation: u64,
    #[allow(clippy::type_complexity)]
    pub(crate) last_method_resolve: Option<(Symbol, Symbol, Symbol, Arc<MethodDef>)>,
    pub(crate) fast_method_cache:
        rustc_hash::FxHashMap<(Symbol, Symbol), crate::vm::FastMethodCacheEntry>,
    /// #8880: `(receiver class, method name, argument type keys)` triples
    /// whose `CallMethodMut` dispatch has been observed to walk the entire
    /// pre-dispatch probe chain without a single probe claiming the call, so
    /// the chain can be skipped. Written only from the dispatch tail that
    /// proves it, and cleared with the other method caches on a registry
    /// generation change. See `vm_call_method_plain_lane`.
    pub(crate) plain_method_lane: rustc_hash::FxHashSet<PlainMethodLaneKey>,
    /// The key the *current* `CallMethodMut` dispatch may install into
    /// [`Self::plain_method_lane`]. Set (or cleared) by that opcode's
    /// gate on every dispatch, so it always describes the innermost one.
    pub(crate) plain_method_lane_candidate: Option<PlainMethodLaneKey>,
    /// One-shot flag handing a proven-inert dispatch straight to the
    /// user-method tail; consumed by
    /// `try_compiled_method_mut_or_interpret_sym`.
    pub(crate) plain_method_lane_active: bool,
    /// ADR-0121 D3: classes whose `.new(named...)` `CallMethodMut` dispatch was
    /// observed to walk the whole probe chain into the native default
    /// constructor, with the `NativeCtorPlan` that construction used. Written
    /// only from that outcome, and cleared with the other method caches on a
    /// registry generation change. See `vm_ctor_lane`.
    pub(crate) ctor_lane: rustc_hash::FxHashMap<Symbol, Arc<NativeCtorPlan>>,
    /// The class the *current* `CallMethodMut` dispatch may install into
    /// [`Self::ctor_lane`]; set (or cleared) by that opcode's gate.
    pub(crate) ctor_lane_candidate: Option<Symbol>,
    /// ADR-0121 D3: `(layout id, method name) -> slot` for a `CallMethodMut`
    /// whose whole dispatch was the generated accessor's plain slot read.
    /// Written only from that outcome, and cleared with the other method caches
    /// on a registry generation change. See `vm_accessor_lane`.
    pub(crate) accessor_lane: rustc_hash::FxHashMap<(u32, Symbol), u32>,
    /// Memoized `class -> NativeCtorPlan` for the native default constructor.
    /// Cleared wherever `fast_method_cache` is cleared, plus the MOP class-shape
    /// mutators (`Attribute.set_build`, `^add_attribute`, `^add_method`,
    /// `^compose`). A class not yet registered is never cached (a role punned
    /// to a class on first use must not freeze a negative plan).
    pub(crate) native_ctor_plan_cache: rustc_hash::FxHashMap<Symbol, Arc<NativeCtorPlan>>,
    /// `grammar_has_user_method_sym` answers per `(class, method)`, valid for
    /// one registry write generation (see `user_method_probe_memo.rs`).
    pub(crate) user_method_probe_memo: user_method_probe_memo::UserMethodProbeMemo,
    /// `nqp::create` / `CREATE` answers per type, valid for one registry
    /// write generation (see `nqp_create.rs`).
    pub(crate) create_memo: nqp_create::CreateMemo,
    /// Sound multi-method resolution cache (§B): for a multi whose dispatch is
    /// purely type+arity based (no `where` / literal / subset / `:D`/`:U` smiley /
    /// coercion candidate), the resolved candidate is a function of the receiver
    /// class + method + the runtime types of the positional args, so it is cached
    /// here keyed on `(class, method, arg-type-keys)`. Cleared with the other
    /// method caches when the registry changes.
    #[allow(clippy::type_complexity)]
    pub(crate) multi_resolve_cache:
        rustc_hash::FxHashMap<(Symbol, Symbol, Vec<Symbol>), Option<(Symbol, Arc<MethodDef>)>>,
    /// Memoized `(class, method) -> is this multi's dispatch type+arity deterministic`
    /// (i.e. cacheable in `multi_resolve_cache`). Computed once by scanning the MRO
    /// candidates for value-dependent constraints.
    pub(crate) multi_type_cacheable: rustc_hash::FxHashMap<(Symbol, Symbol), bool>,
    /// The private-method twin of `multi_resolve_cache`: `$obj!name(args)`
    /// resolved against `(receiver class, "!name", arg-type-keys)`. Filled only
    /// when no candidate of that name is value-dependent
    /// (`private_type_cacheable`), so the winner is a function of the key.
    /// Cleared with the other method caches (generation bump) and by
    /// `clear_private_zeroarg_method_cache`.
    pub(crate) private_resolve_cache: rustc_hash::FxHashMap<PrivateResolveKey, PrivateResolved>,
    /// Memoized `(class, "!name") -> may private_resolve_cache serve it`.
    pub(crate) private_type_cacheable: rustc_hash::FxHashMap<(Symbol, Symbol), bool>,
    /// Memoized `(native type name, method) -> does a user `augment` declare this
    /// method on that type or an MRO ancestor` — the `native_lever_a_user_override`
    /// gate every native method call passes through. The answer is a pure function
    /// of the registry shape, so it is sound to key on the pair and clear it with
    /// the other method caches on a registry-generation bump. Without the memo the
    /// gate re-walked the receiver's whole MRO (`Int` -> `Cool` -> `Any` -> `Mu`),
    /// asking `user_method_overloads` at each level, on EVERY `$x.foo` — which
    /// showed up as ~7% of a native-method-dispatch loop's profile
    /// (`has_user_method` + `class_mro` + `user_method_overloads`) purely to
    /// re-derive "no, nobody augmented Int".
    /// Memo for [`Interpreter::native_lever_a_user_override_sym`], keyed by
    /// `(address of the receiver's `&'static str` type name, method symbol)`.
    /// See that function for why the type half is an address and not a
    /// `Symbol`.
    pub(crate) native_lever_a_override_cache: rustc_hash::FxHashMap<(usize, Symbol), bool>,
    /// Memoized `(class, method) -> does this name have >= 2 structural dispatch
    /// candidates across the MRO` (counting overloads BEFORE arg-matching).
    /// `false` means the name resolves to at most one candidate, so
    /// `push_method_dispatch_frame` can skip the per-call `resolve_all_methods_with_owner`
    /// MRO walk + MethodDef clones entirely (a single/zero candidate never produces
    /// a deferral frame regardless of args — arg-matching only reduces the count).
    /// Structural (registry-shape) only, so it is sound to key on `(class, method)`
    /// and is cleared with the other method caches on any registry change.
    pub(crate) dispatch_multi_candidate: rustc_hash::FxHashMap<(Symbol, Symbol), bool>,
    /// Memoized `(class, method) -> can this name's deferral frame be built
    /// after the call has started` (see `method_dispatch_lazy`). Structural,
    /// cleared with `dispatch_multi_candidate`.
    pub(crate) deferral_build_context_free: rustc_hash::FxHashMap<(Symbol, Symbol), bool>,
    /// Memoized structural fingerprint of a method body, keyed by the *pointer*
    /// of its `Arc<Vec<Stmt>>` body. `function_body_fingerprint` traverses
    /// the whole body AST, which dominated the method-redispatch hot path
    /// (`build_remaining` / `prepare_method_dispatch_frame`, reached by every
    /// `nextsame`/`samewith` and multi-method call) — perf showed ~8% of a
    /// samewith-tight-loop in SipHash-over-Debug back when the traversal went
    /// through `core::fmt`. A `MethodDef` clone shares its
    /// body `Arc`, and two *distinct* methods always have distinct body `Arc`s
    /// (clones are the only way to share one, and clones carry identical
    /// params/param_defs), so the body-`Arc` pointer uniquely identifies the
    /// `(params, param_defs, body)` tuple the fingerprint covers. The cache holds
    /// a strong `Arc` clone of each body so the pointer can never be freed and
    /// reused under a stale entry — it needs no invalidation and is bounded by
    /// the number of distinct method bodies in the program.
    pub(crate) method_body_fp_cache: rustc_hash::FxHashMap<usize, (Arc<Vec<Stmt>>, u64)>,
    /// Sound multi-*function* resolution cache — the function-dispatch analogue of
    /// `multi_resolve_cache`. For a multi sub whose dispatch is purely type+arity
    /// based (no `where` / literal / subset / `:D`/`:U` smiley / coercion
    /// candidate), the winning candidate is a function of `(package, name,
    /// positional arg types)`, so it is cached here. Keyed on
    /// `(package_sym, name_sym, arg-type-keys)`. Each entry is tagged with the
    /// `fn_resolve_gen` it was computed under (like `fn_resolve_cache`), so it
    /// is retired by any registry change and live again when a scope restore
    /// puts that registry back — a routine declaring an inner `my sub` no
    /// longer empties it twice per call (#9073). The two verdict memos below
    /// are tagged the same way.
    #[allow(clippy::type_complexity)]
    pub(crate) func_multi_resolve_cache:
        GenCache<(Symbol, Symbol, Vec<Symbol>), Option<Arc<FunctionDef>>>,
    /// Memoized `(package, name) -> is this multi sub's dispatch type+arity
    /// deterministic` (i.e. cacheable in `func_multi_resolve_cache`). The
    /// function analogue of `multi_type_cacheable`.
    pub(crate) func_multi_type_cacheable: GenCache<(Symbol, Symbol), bool>,
    /// Memoized `(package, name, argument type keys) -> may this ONE argument
    /// type key use `func_multi_resolve_cache` even though `func_multi_type_cacheable`
    /// said the family as a whole is value-dependent`.
    ///
    /// The refinement [#8696](https://github.com/tokuhirom/mutsu/issues/8696)
    /// step 2 adds: a `subset S of Int` candidate cannot match a `Str`
    /// argument, which is decidable from the declared base type alone, so a
    /// family whose value-dependent candidates are ALL excluded that way is
    /// type-deterministic for those argument types after all. See
    /// `dispatch_narrow.rs` for the soundness rules.
    pub(crate) func_multi_argkey_cacheable: GenCache<FuncMultiResolveKey, bool>,
    /// Type-keyed dispatch plans for bare-name multi calls: the gathered,
    /// ranked candidate passes, so a value-dependent family (`where`,
    /// `subset`) re-runs only its bind checks per call (#9967,
    /// `multi_dispatch_plan.rs`).
    pub(crate) bare_multi_plan_cache: GenCache<
        crate::runtime::multi_dispatch_plan::BareMultiPlanKey,
        Arc<crate::runtime::multi_dispatch_plan::BareMultiPlan>,
    >,
    /// `(operator, candidate, argument type keys) -> does the core candidate
    /// set out-rank this user infix candidate` (#10111,
    /// `native_infix_dispatch.rs`). The candidate's `Arc` is held so a hit can
    /// be confirmed by identity.
    pub(crate) core_infix_wins_cache:
        GenCache<crate::runtime::native_infix_dispatch::CoreInfixWinsKey, (Arc<FunctionDef>, bool)>,
    /// Name-keyed cache of OTF-compiled routine bodies. The cached entry records
    /// the package the resolution was made *under* (the callsite's
    /// `current_package`), because the same bare name resolves to different
    /// routines in different packages: a `unit module Foo`'s non-exported sub is
    /// visible as `foo` only while `current_package == Foo`, and reusing that
    /// entry at a GLOBAL callsite would leak it into the consumer's scope
    /// (PLAN 8.22). A package mismatch falls through to a fresh resolve.
    /// Entries are `(callsite package, defining package, body)`. The defining
    /// package is what the body must run under (it scopes `$?PACKAGE`, qualified
    /// name resolution and the `__mutsu_callable_id::PKG::NAME` lookup that keys
    /// `once`); reading `current_package()` at the callsite instead would give
    /// the caller's package, which only happened to agree while every module sub
    /// registered into GLOBAL.
    /// The body is `Arc`-shared, not owned by value: the hot dispatch path in
    /// `exec_call_func_op` used to `remove()` the entry, run the call, and
    /// `insert()` it back purely to avoid holding a borrow on `self`. A
    /// `CompiledFunction` embeds a whole `CompiledCode` (~1 kB of `Vec`/`HashMap`
    /// headers), so that round trip memcpy'd the struct out of and back into the
    /// table on EVERY call — it profiled as the single largest cost of calling a
    /// block-local sub (`memmove` alone was 15% of the run). Cloning the `Arc`
    /// is one refcount bump and leaves the table untouched.
    /// Tagged per entry with `fn_resolve_gen`, like `light_call_cache`.
    pub(crate) otf_call_cache: GenCache<Symbol, (Symbol, Symbol, Arc<CompiledFunction>)>,
}

impl ResolutionCaches {
    /// A spawned thread rebuilds its caches on demand. Starting empty is also
    /// required, not only cheap, for `capture_cache`: its entry holds `Arc`s on
    /// the parent thread's env tiers, which the child neither shares nor
    /// should pin.
    // Cost: O(1).
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self::default()
    }
}
