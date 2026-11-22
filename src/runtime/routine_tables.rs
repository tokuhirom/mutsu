//! The routine tables of one execution context (ADR-17216 §3.2).
//!
//! `functions`, `proto_functions`, `proto_subs`, `our_scoped_functions`,
//! `token_defs`, `proto_tokens` and their indexes and memos. They used to be
//! fields of [`Registry`](super::Registry), but unlike the type tables they are
//! not world state: mutsu uses them as a lexical-scope emulator (a routine scope
//! or `EVAL` snapshots them and puts the snapshot back when it ends, an import
//! scope undoes its installs), and a routine is lexical in Raku. So they stay
//! with the execution context: every thread holds its own, forked
//! copy-on-write at spawn, while the type tables are shared by every thread.
//!
//! Reached through `Interpreter::routines` / `Interpreter::routines_mut`, whose
//! guards follow the same discipline as the registry's: never hold one across
//! a call that re-enters user code.

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::sync::Arc;

use crate::ast::FunctionDef;
use crate::symbol::Symbol;
use crate::value::Value;

use super::dispatch_key;
use super::function_table::{FunctionTable, FunctionTableTransitions};
use super::registry::TokenDefsMap;
use super::SubsetDef;

/// The routine tables of one execution context. See the module docs.
#[derive(Clone, Default)]
pub(crate) struct RoutineTables {
    /// Registry keys of `my sub`s declared inside a routine body that stayed
    /// registry-resolved (not frame lexicals), each with the definition's
    /// `decl_order` and the routine-stack depth it was registered at. An
    /// `EVAL` run by a unit-scoped plain sub hides the ones registered by its
    /// dynamic callers (#16099). An entry is stale once `decl_order` differs
    /// from the registered definition's.
    pub(crate) routine_local_subs:
        std::collections::HashMap<crate::symbol::Symbol, (u64, usize)>,
    /// The same for a `my subset` declared inside a routine body: its bare
    /// name sits in the global [`Registry::subsets`](super::registry::Registry::subsets) with the routine-stack
    /// depth it was declared at. Stale once the registered definition is no
    /// longer the recorded `Arc`.
    pub(crate) routine_local_subsets:
        std::collections::HashMap<String, (std::sync::Arc<SubsetDef>, usize)>,

    // ----- functions / subs / tokens (PR-A slice 5, final PR-A slice) -----
    /// User-defined subs: fully-qualified name -> [`FunctionDef`]. Read on the
    /// sub/multi-dispatch hot path; callers clone the matched `Arc<FunctionDef>`
    /// (a cheap refcount bump) under a short-lived guard. Each def is held
    /// behind `Arc` so the whole-map clones share the def rather than deep-copying
    /// every routine body in the program.
    ///
    /// The *map itself* is behind an `Arc` as well, making it one of the
    /// copy-on-write program tables described on [`Interpreter`](super::Interpreter):
    /// reads go through `Deref` unchanged, while every write goes through
    /// [`RoutineTables::functions_mut`], which copies only while a snapshot still
    /// shares the map. That is what makes the scope snapshots taken *around*
    /// routine and block bodies — `snapshot_routine_registry` (every routine
    /// declaring inner `my sub`s) and `eval_block_value_inner` (every carrier
    /// block, #7887) — a refcount bump instead of an O(registered routines)
    /// copy that the overwhelmingly common declaration-free body then throws
    /// away unused.
    ///
    /// The `Arc` points at a [`FunctionTable`] rather than at the bare map: the
    /// table carries the version stamp that every name-keyed dispatch memo is
    /// tagged with, so restoring a snapshot restores the version along with the
    /// content it names (#8314 — see `runtime::function_table`).
    pub(crate) functions: std::sync::Arc<FunctionTable>,
    /// What each `install_function` write did to [`RoutineTables::functions`]'s
    /// version stamp, so that repeating a write the program has made before
    /// names the state it produced before. See `runtime::function_table`.
    pub(crate) fn_transitions: FunctionTableTransitions,
    /// The last answer to "which `our` subs does a restore put back" for an
    /// untouched our-scoped table (`runtime::our_scoped_restore`, #9170).
    pub(crate) our_scoped_restore_memo: crate::runtime::our_scoped_restore::OurScopedRestoreMemo,
    /// The module-registered routine set and the functions-map version every
    /// key of it was last found in (`reinstate_module_functions`, #9170).
    pub(crate) module_functions_present:
        Option<(std::sync::Arc<std::collections::HashSet<Symbol>>, u64)>,
    /// `our`-scoped subs that persist across block boundaries. Held behind `Arc`
    /// (like `functions`) so block-scope restore and whole-registry clones
    /// (`clone_for_thread`, EVAL copy) share the def rather than deep-cloning it;
    /// the same `Arc` is also what gets re-inserted into `functions`.
    pub(crate) our_scoped_functions: std::sync::Arc<HashMap<Symbol, std::sync::Arc<FunctionDef>>>,
    /// `proto sub` markers (multi proto stubs): name -> proto `FunctionDef`.
    /// Copy-on-write behind an `Arc` for the same reason as
    /// [`RoutineTables::functions`]; write through [`RoutineTables::proto_functions_mut`].
    pub(crate) proto_functions: std::sync::Arc<HashMap<Symbol, std::sync::Arc<FunctionDef>>>,
    /// Grammar token/rule definitions: name -> `[overloads]`. Each overload is
    /// held behind `Arc` so the whole-map snapshot/restore clones (and the
    /// per-resolution candidate merges) are O(n) refcount bumps rather than
    /// deep clones of the token bodies.
    pub(crate) token_defs: std::sync::Arc<TokenDefsMap>,
    /// Each rule's captured declaring scope (`regex_token_closure`), keyed by
    /// the rule's qualified name: the shared cells of the declaring frame's
    /// variables its body names, re-captured on every entry of that frame.
    pub(crate) token_captures: HashMap<Symbol, std::sync::Arc<Vec<(String, Value)>>>,
    /// `proto sub` declaration markers (existence set). Private: every
    /// mutation must go through the `proto_subs_*` accessors below so the
    /// `proto_gen` invalidation counter for `Interpreter::has_proto_cached`
    /// can never miss a write (compiler-enforced completeness). Copy-on-write
    /// behind an `Arc` for the same reason as [`RoutineTables::functions`], which is
    /// what makes `proto_subs_snapshot` O(1).
    proto_subs: std::sync::Arc<HashSet<String>>,
    /// Monotonic invalidation generation for `proto_subs`. Bumped by every
    /// accessor that can change the set's contents; compared by
    /// `Interpreter::has_proto_cached`.
    proto_gen: u64,
    /// `proto token`/`proto rule` declaration markers (existence set).
    pub(crate) proto_tokens: std::sync::Arc<HashSet<String>>,
    /// The signature of a `proto token`/`proto rule` that declares positional
    /// parameters, keyed like `proto_tokens`.
    pub(crate) proto_token_params: HashMap<String, Arc<Vec<crate::ast::ParamDef>>>,
    /// Names this content: shared by every copy-on-write fork, and replaced
    /// by every write through [`RoutinesWriteGuard`] except a version-only
    /// renewal ([`RoutinesWriteGuard::renew_functions_version`]). Two tables
    /// with the same id resolve every name alike, so a closure created on
    /// another context runs on this one's tables without swapping them in
    /// (`Interpreter::enter_closure_routine_context`).
    content_id: ContentId,
}

/// See [`RoutineTables::content_id`]: a fresh value for every newly built
/// table, so two tables built apart never share one.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct ContentId(u64);

impl Default for ContentId {
    // Cost: O(1), one atomic increment.
    fn default() -> Self {
        Self(crate::runtime::registry_gen::next_generation())
    }
}

impl RoutineTables {
    /// Whether `other` holds the same content as these tables (see
    /// [`RoutineTables::content_id`]); the function map's version stamp may
    /// differ.
    // Cost: O(1).
    #[inline]
    pub(crate) fn same_content_as(&self, other: &RoutineTables) -> bool {
        self.content_id == other.content_id
    }

    /// The `proto_subs` invalidation generation (see the field doc).
    pub(crate) fn proto_generation(&self) -> u64 {
        self.proto_gen
    }

    fn bump_proto_gen(&mut self) {
        self.proto_gen = self.proto_gen.wrapping_add(1);
    }

    /// Membership test on the raw `proto sub` marker set (fully-qualified key).
    pub(crate) fn proto_subs_contains(&self, key: &str) -> bool {
        self.proto_subs.contains(key)
    }

    /// Insert a `proto sub` marker. Bumps `proto_gen`.
    pub(crate) fn proto_subs_insert(&mut self, key: String) {
        crate::runtime::cow_table_mut(&mut self.proto_subs).insert(key);
        self.bump_proto_gen();
    }

    /// Retain-filter the `proto sub` markers (module unregistration). Bumps
    /// `proto_gen`.
    pub(crate) fn proto_subs_retain<F: FnMut(&String) -> bool>(&mut self, f: F) {
        crate::runtime::cow_table_mut(&mut self.proto_subs).retain(f);
        self.bump_proto_gen();
    }

    /// Snapshot of the marker set, for save/restore callers (EVAL sandboxing,
    /// nested test interpreters, module snapshots, carrier blocks). An `Arc`
    /// bump, not a copy — the copy happens on the next write, and only if this
    /// snapshot is still alive then.
    pub(crate) fn proto_subs_snapshot(&self) -> std::sync::Arc<HashSet<String>> {
        std::sync::Arc::clone(&self.proto_subs)
    }

    /// Wholesale-replace the marker set from a snapshot. Bumps `proto_gen`.
    ///
    /// Restoring the very set that is installed (the snapshot `Arc` itself,
    /// untouched since) changes nothing, so it does not bump: a routine scope
    /// restores this on every return, and the bump retired every
    /// proto-generation memo in the program once per call (#9073).
    pub(crate) fn proto_subs_restore(&mut self, set: std::sync::Arc<HashSet<String>>) {
        if std::sync::Arc::ptr_eq(&self.proto_subs, &set) {
            return;
        }
        self.proto_subs = set;
        self.bump_proto_gen();
    }

    /// Copy-on-write mutable access to [`RoutineTables::functions`]. The only way to
    /// write the map: it is the `Arc::make_mut` that the copy-on-write share
    /// described on that field needs.
    ///
    /// Going through `FunctionTable::map_mut` renews the table's version stamp,
    /// which is what makes "is this still the map that memo was computed under"
    /// answerable without every mutating site having to say so (#8314).
    #[inline]
    pub(crate) fn functions_mut(&mut self) -> &mut HashMap<Symbol, std::sync::Arc<FunctionDef>> {
        crate::runtime::cow_table_mut(&mut self.functions).map_mut()
    }

    /// Install `def` under `key`, recognising the map state this produces when
    /// it is one this program has already been in.
    ///
    /// Use it in place of `functions_mut().insert(..)` wherever the same key is
    /// re-installed with the same `Arc<FunctionDef>` over and over — the
    /// per-call re-install of a routine-local `my sub` above all. The version
    /// then alternates between the two states the routine's scope alternates
    /// between, instead of naming a new one on every call, so the memos taken on
    /// either side of the boundary both survive it (#8314; see
    /// `runtime::function_table`).
    ///
    /// For a one-off registration it behaves exactly like the plain insert, at
    /// the cost of one hash lookup.
    pub(crate) fn install_function(&mut self, key: Symbol, def: std::sync::Arc<FunctionDef>) {
        // `functions` and `fn_transitions` are disjoint fields; the split
        // borrow is what lets the memo drive the table (and its stamp).
        let transitions = &mut self.fn_transitions;
        transitions.install(&mut self.functions, key, def);
    }

    /// Return the exact function declaration installed under `key`.
    #[inline]
    pub(crate) fn function_def(&self, key: Symbol) -> Option<std::sync::Arc<FunctionDef>> {
        self.functions.get(&key).cloned()
    }

    /// Give [`RoutineTables::functions`] a version it has never had before, without
    /// changing its content.
    ///
    /// The way to retire every memo tagged with the current version when what
    /// changed is not the map itself. See
    /// `Interpreter::invalidate_fn_resolution` for what that covers and why the
    /// content-naming invariant (`fn_resolve_gen == functions_version()`) is
    /// worth keeping even for those callers.
    pub(crate) fn renew_functions_version(&mut self) {
        crate::runtime::cow_table_mut(&mut self.functions).renew_version();
    }

    /// The version stamp of the currently installed functions map.
    ///
    /// This is the value [`Interpreter::invalidate_fn_resolution`] mirrors into
    /// `fn_resolve_gen`; a memo tagged with it stays valid for exactly as long
    /// as this map is installed, across any number of intervening scope
    /// excursions that put it back.
    ///
    /// [`Interpreter::invalidate_fn_resolution`]: super::Interpreter::invalidate_fn_resolution
    #[inline]
    pub(crate) fn functions_version(&self) -> u64 {
        self.functions.version()
    }

    /// Copy-on-write mutable access to [`RoutineTables::proto_functions`].
    #[inline]
    pub(crate) fn proto_functions_mut(
        &mut self,
    ) -> &mut HashMap<Symbol, std::sync::Arc<FunctionDef>> {
        crate::runtime::cow_table_mut(&mut self.proto_functions)
    }

    /// Whether a `proto sub`/`proto` named `name` is declared, visible from the
    /// `current_package` scope. Pure registry+scope read (no env, no re-entry):
    /// the single implementation shared by `Interpreter::has_proto` and the VM's
    /// native dispatch path.
    pub(crate) fn has_proto(&self, current_package: &str, name: &str) -> bool {
        if crate::qualified::is_qualified_str(name) {
            return self.proto_subs.contains(name);
        }
        if dispatch_key::with_qualified(current_package, name, |k| self.proto_subs.contains(k)) {
            return true;
        }
        dispatch_key::with_qualified("GLOBAL", name, |k| self.proto_subs.contains(k))
    }

    /// Whether any `multi` candidate (any arity) exists for `name`, visible from
    /// any of the bare-name search `packages` (see
    /// `Interpreter::bare_name_packages`). Pure registry+scope read, shared by
    /// `Interpreter::has_multi_candidates` and the VM's native dispatch path.
    /// Takes the whole search list so the candidate keys are examined once, not
    /// once per enclosing package.
    pub(crate) fn has_multi_candidates(
        &self,
        base_keys: Option<&[Symbol]>,
        packages: &[Symbol],
        name: &str,
    ) -> bool {
        self.any_candidate_key_of(base_keys, packages, name)
    }

    /// Whether any registered key spells an arity-keyed candidate of `name` in
    /// one of `packages` — the shared body of [`RoutineTables::has_multi_candidates`]
    /// and [`RoutineTables::has_multi_function`], which ask the identical question
    /// for two different callers.
    ///
    /// `base_keys` is [`super::Interpreter::fn_keys_for_base`]'s answer for
    /// `name`, and narrowing to it (rather than walking every registered key,
    /// resolving each through the interner and prefix-comparing it against a
    /// `format!`ed `String` per package) is the whole point: both probes ran
    /// several times per dispatch and each was O(registered routines)
    /// ([#8300](https://github.com/tokuhirom/mutsu/issues/8300)). `None` means
    /// the caller could not reach the index — it fills lazily and so needs
    /// `&mut self`, which the cold `&self` EVAL-time probes do not have — and
    /// asks for the full scan.
    ///
    /// The narrowing is sound because every key this predicate can match —
    /// `{pkg}::{name}/…` — reduces under `function_key_base_name` to the same
    /// base name the index filed it under: the arity suffix that function
    /// strips is the `/<digit>` immediately after `{name}`, which registration
    /// always writes, and the package prefix it strips is `{pkg}::`. The query
    /// name goes through the same extraction, so a qualified `Foo::bar` and a
    /// bare `bar` land in the same bucket as the keys that answer them. In
    /// debug builds that argument is checked rather than trusted: the full scan
    /// still runs and must agree.
    fn any_candidate_key_of(
        &self,
        base_keys: Option<&[Symbol]>,
        packages: &[Symbol],
        name: &str,
    ) -> bool {
        // An exported method is represented in the function registry by a
        // synthetic arity-qualified candidate so it can be imported as a
        // callable.  If the same package also has a plain sub of that name,
        // the plain sub is the ordinary bare-name declaration and the
        // synthetic bridge must not turn the lookup into multi dispatch.  In
        // an importing package there is no exact key, so the bridge remains a
        // real candidate and is still visible through `has_multi_candidates`.
        let has_plain_declaration = packages.iter().any(|package| {
            dispatch_key::qualified_lookup(package.as_str(), name)
                .is_some_and(|key| self.functions.contains_key(&key))
        });
        let matches = |ks: &str| {
            packages.iter().any(|p| {
                if !dispatch_key::key_is_candidate_of(ks, p.as_str(), name) {
                    return false;
                }
                if !has_plain_declaration {
                    return true;
                }
                Symbol::lookup(ks)
                    .and_then(|key| self.functions.get(&key))
                    .is_none_or(|def| def.declarator != crate::ast::RoutineDeclarator::Method)
            })
        };
        let full_scan = || self.functions.keys().any(|k| matches(k.as_str()));
        let Some(base_keys) = base_keys else {
            return full_scan();
        };
        let narrowed = base_keys.iter().any(|k| matches(k.as_str()));
        #[cfg(debug_assertions)]
        assert_eq!(
            narrowed,
            full_scan(),
            "fn_keys_for_base index missed a candidate key for {name:?} in \
             {packages:?}: the base-name narrowing in \
             Registry::any_candidate_key_of is not equivalent to the full \
             functions-map scan"
        );
        narrowed
    }

    /// Whether a (non-multi) function `name` is declared, visible from the
    /// `current_package` scope: either fully qualified under the current package
    /// or as a bare global name. Pure registry+scope read, shared by
    /// `Interpreter::has_declared_function` and the VM's native dispatch path.
    pub(crate) fn has_declared_function(&self, current_package: &str, name: &str) -> bool {
        dispatch_key::qualified_lookup(current_package, name)
            .is_some_and(|fq| self.functions.contains_key(&fq))
            || Symbol::lookup(name).is_some_and(|n| self.functions.contains_key(&n))
    }

    /// Whether a `multi`-dispatched function `name` exists at any arity, visible
    /// from any of the bare-name search `packages`. Pure registry+scope read,
    /// shared by `Interpreter::has_multi_function` and the VM's native dispatch
    /// path. Takes the whole search list so the candidate keys are examined
    /// once, not once per enclosing package.
    pub(crate) fn has_multi_function(
        &self,
        base_keys: Option<&[Symbol]>,
        packages: &[Symbol],
        name: &str,
    ) -> bool {
        self.any_candidate_key_of(base_keys, packages, name)
    }
}

/// Read guard over this execution context's [`RoutineTables`]: derefs straight
/// through the copy-on-write `Arc`.
pub(crate) struct RoutinesReadGuard<'a> {
    inner: crate::runtime::lock_reentry::ReentrantReadGuard<'a, Arc<RoutineTables>>,
}

impl<'a> RoutinesReadGuard<'a> {
    pub(crate) fn new(lock: &'a std::sync::RwLock<Arc<RoutineTables>>) -> Self {
        Self {
            inner: crate::runtime::lock_reentry::ReentrantReadGuard::new(lock, "routines"),
        }
    }
}

impl RoutinesReadGuard<'_> {
    /// The guarded tables' `Arc`, for a copy-on-write fork.
    // Cost: O(1).
    pub(crate) fn clone_arc(&self) -> Arc<RoutineTables> {
        Arc::clone(&self.inner)
    }
}

impl std::ops::Deref for RoutinesReadGuard<'_> {
    type Target = RoutineTables;
    #[inline]
    fn deref(&self) -> &RoutineTables {
        &self.inner
    }
}

/// Write guard over this execution context's [`RoutineTables`]; the first
/// mutable deref after a share (a spawn, a snapshot) pays the one copy.
pub(crate) struct RoutinesWriteGuard<'a> {
    inner: crate::runtime::lock_reentry::ReentrantWriteGuard<'a, Arc<RoutineTables>>,
}

impl<'a> RoutinesWriteGuard<'a> {
    pub(crate) fn new(lock: &'a std::sync::RwLock<Arc<RoutineTables>>) -> Self {
        Self {
            inner: crate::runtime::lock_reentry::ReentrantWriteGuard::new(lock, "routines"),
        }
    }
}

impl std::ops::Deref for RoutinesWriteGuard<'_> {
    type Target = RoutineTables;
    #[inline]
    fn deref(&self) -> &RoutineTables {
        &self.inner
    }
}

impl std::ops::DerefMut for RoutinesWriteGuard<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut RoutineTables {
        let tables = self.make_mut();
        // Any write may change what the tables resolve.
        tables.content_id = ContentId::default();
        tables
    }
}

impl RoutinesWriteGuard<'_> {
    // Cost: O(1), or one copy of the tables while a fork still shares them.
    #[inline]
    fn make_mut(&mut self) -> &mut RoutineTables {
        let arc: &mut Arc<RoutineTables> = &mut self.inner;
        if Arc::strong_count(arc) > 1 {
            crate::vm::vm_stats::record_registry_cow_clone();
        }
        Arc::make_mut(arc)
    }

    /// [`RoutineTables::renew_functions_version`]: the function map gets a
    /// version it never had, and its content -- what every name resolves to
    /// -- keeps its id.
    // Cost: O(1), or one copy of the tables while a fork still shares them.
    pub(crate) fn renew_functions_version(&mut self) {
        self.make_mut().renew_functions_version();
    }

    /// `&mut RoutineTables` by name, for a call chain that reads better than
    /// `&mut *guard`.
    // Cost: O(1) (copy-on-write share aside).
    #[inline]
    pub(crate) fn deref_mut_tables(&mut self) -> &mut RoutineTables {
        self
    }
}
