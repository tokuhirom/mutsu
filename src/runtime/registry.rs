//! Declaration registry shared between the VM and the Interpreter.
//!
//! Holds the program's *declarations* — classes, roles, enums, subsets, subs,
//! tokens and their associated metadata. Historically these lived as ~30
//! separate fields directly on [`Interpreter`](super::Interpreter), which trapped
//! them inside the tree-walking interpreter: VM-native code could only reach them
//! through `self.interpreter.<field>`. This is the "bidirectional ownership knot"
//! that phase ② of the VM/Interpreter decoupling resolves (see PLAN.md ②).
//!
//! The registry is held behind `Arc<RwLock<Registry>>` so the VM and the
//! Interpreter can reach it as *peers* rather than one owning the other. This is
//! transitional scaffolding: the `Arc`/lock exists only because two executors
//! share the data today. Once the Interpreter execution path is removed (PLAN.md
//! ④/⑤), the registry collapses to a plain VM-owned field.
//!
//! Threading: registries are snapshot-cloned per thread (deep clone into a fresh
//! `Arc`), matching the pre-existing `clone_for_thread` semantics — a `start {}`
//! block sees the parent's declarations but its own new declarations do not leak
//! back. `Value` is `Send + Sync` (all-`Arc` internals), so `Registry` is too.
//!
//! Lock discipline (CRITICAL): never hold a read/write guard across a call that
//! re-enters user-code execution (`eval_block_value` / `run_block_raw` /
//! `call_function`). `RwLock` is not reentrant, so a held guard would deadlock.
//! Always use a *temporary* guard (`self.registry().subsets.get(..)`), never a
//! `let`-bound guard that lives across such a call.

use std::collections::{HashMap, HashSet};

use crate::ast::FunctionDef;
use crate::symbol::Symbol;
use crate::value::{EnumValue, RuntimeError, Value};

use super::{ClassDef, MethodDef, RoleCandidateDef, RoleDef, SubsetDef};

/// Program declaration registry. See module docs.
///
/// Fields are migrated here group-by-group (PLAN.md PR-A). Fields are
/// `pub(crate)` so registry-internal runtime code can access them directly;
/// PR-B adds typed lookup methods for the VM to call.
///
/// Note: no `Debug` derive — `ClassDef` (and its `MethodDef`/AST graph) is not
/// `Debug`, and nothing needs to format the registry.
#[derive(Clone, Default)]
pub(crate) struct Registry {
    /// `enum Name (...)` declarations: enum name -> [(variant name, value)].
    pub(crate) enum_types: HashMap<String, Vec<(String, EnumValue)>>,
    /// `subset Name of Base where { ... }` declarations.
    pub(crate) subsets: HashMap<String, SubsetDef>,

    /// User/builtin class definitions: class name -> [`ClassDef`] (parents, MRO,
    /// methods, attributes, ...). Read on hot method-dispatch paths; callers take
    /// short-lived `registry()` guards and clone the minimal projection they need
    /// (e.g. `mro.clone()`, `methods.get(name).cloned()`) rather than the whole
    /// `ClassDef`.
    pub(crate) classes: HashMap<String, ClassDef>,

    // ----- class metadata (PR-A slice 2) -----
    /// Classes declared as a C `union` (native interop helper set).
    pub(crate) cunion_classes: HashSet<String>,
    /// Classes marked `is hidden` (excluded from `.^mro` etc.).
    pub(crate) hidden_classes: HashSet<String>,
    /// Forward-declared class stubs (`class Foo { ... }` declared later).
    pub(crate) class_stubs: HashSet<String>,
    /// Forward-declared package stubs.
    pub(crate) package_stubs: HashSet<String>,
    /// `is hidden` parents deferred until the parent is fully declared.
    pub(crate) hidden_defer_parents: HashMap<String, HashSet<String>>,
    /// `trusts` relationships: class -> set of trusted classes.
    pub(crate) class_trusts: HashMap<String, HashSet<String>>,
    /// Per-class metaclass (`HOW`) value override.
    pub(crate) class_how_values: HashMap<String, Value>,
    /// Roles composed into each class: class -> [role names].
    pub(crate) class_composed_roles: HashMap<String, Vec<String>>,
    /// Roles implicitly composed by enums: enum -> [role names].
    pub(crate) class_enum_roles: HashMap<String, Vec<String>>,
    /// Subs declared inside a class body: class -> (sub name -> value).
    pub(crate) class_subs: HashMap<String, HashMap<String, Value>>,
    /// Per-attribute `BUILD` override: (class, attr) -> builder value.
    pub(crate) attribute_build_overrides: HashMap<(String, String), Value>,
    /// Per-attribute default value: (class, attr) -> default value.
    pub(crate) class_attribute_defaults: HashMap<(String, String), Value>,
    /// Per-attribute declared type: (class, attr) -> type name.
    pub(crate) class_attribute_is_types: HashMap<(String, String), String>,
    /// Per-attribute `is DEPRECATED` message: (class, attr) -> message.
    pub(crate) class_attribute_deprecated: HashMap<(String, String), String>,

    // ----- roles (PR-A slice 4) -----
    /// User/builtin role definitions: role name -> [`RoleDef`] (methods,
    /// attributes, deferred body, ...). Like `classes`, callers clone the
    /// minimal projection under a short-lived guard rather than the whole def.
    pub(crate) roles: HashMap<String, RoleDef>,
    /// Roles explicitly declared via user code (not pre-registered builtins);
    /// used to detect `X::Redeclaration` for role->class name conflicts.
    pub(crate) user_declared_roles: HashSet<String>,
    /// Parameterized role candidates: role name -> [candidate by arity/types].
    pub(crate) role_candidates: HashMap<String, Vec<RoleCandidateDef>>,
    /// Role inheritance: role -> [parent role specs].
    pub(crate) role_parents: HashMap<String, Vec<String>>,
    /// `also hides` relationships on roles: role -> [hidden names].
    pub(crate) role_hides: HashMap<String, Vec<String>>,
    /// Declared type parameters per parameterized role: role -> [param names].
    pub(crate) role_type_params: HashMap<String, Vec<String>>,
    /// Bound role type parameters per class: class -> (param name -> value).
    pub(crate) class_role_param_bindings: HashMap<String, HashMap<String, Value>>,

    // ----- functions / subs / tokens (PR-A slice 5, final PR-A slice) -----
    /// User-defined subs: fully-qualified name -> [`FunctionDef`]. Read on the
    /// sub/multi-dispatch hot path; callers clone the matched `FunctionDef`
    /// (already the prior behavior) under a short-lived guard.
    pub(crate) functions: HashMap<Symbol, FunctionDef>,
    /// `our`-scoped subs that persist across block boundaries.
    pub(crate) our_scoped_functions: HashMap<Symbol, FunctionDef>,
    /// `proto sub` markers (multi proto stubs): name -> proto `FunctionDef`.
    pub(crate) proto_functions: HashMap<Symbol, FunctionDef>,
    /// Grammar token/rule definitions: name -> [overloads].
    pub(crate) token_defs: HashMap<Symbol, Vec<FunctionDef>>,
    /// `proto sub` declaration markers (existence set).
    pub(crate) proto_subs: HashSet<String>,
    /// `proto token`/`proto rule` declaration markers (existence set).
    pub(crate) proto_tokens: HashSet<String>,
}

/// Structural lookups over the declaration registry (PR-B: read-side migration).
///
/// These methods are the single source of truth for the *registry-read* portions
/// of class lookup / MRO computation. They consult only registry fields (no
/// re-entry into user-code execution and no Interpreter state), so they take a
/// plain `&self` / `&mut self` on `Registry` and the caller holds exactly one
/// guard for the whole operation — replacing the previous chains of separate
/// `registry()` / `registry_mut()` acquisitions on the Interpreter side.
impl Registry {
    /// Compute the C3 linearization (method resolution order) for `class_name`
    /// from the registered class hierarchy. Pure read over `self.classes` —
    /// recursion stays within the registry. Does not consult or fill the cached
    /// `ClassDef::mro` write side; that is done by [`Registry::class_mro`].
    pub(crate) fn compute_class_mro(
        &self,
        class_name: &str,
        stack: &mut Vec<String>,
    ) -> Result<Vec<String>, RuntimeError> {
        if stack.iter().any(|name| name == class_name) {
            return Err(RuntimeError::new(format!(
                "C3 MRO cycle detected at {}",
                class_name
            )));
        }
        if let Some(class_def) = self.classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return Ok(class_def.mro.clone());
        }
        stack.push(class_name.to_string());
        let explicit_parents = self
            .classes
            .get(class_name)
            .map(|c| c.parents.clone())
            .unwrap_or_default();
        // If a user-defined class has no explicit parents, it implicitly
        // inherits from Any (which in turn inherits from Mu).  This matches
        // Raku's default class hierarchy.
        let parents = if explicit_parents.is_empty() && self.classes.contains_key(class_name) {
            vec!["Any".to_string()]
        } else {
            explicit_parents
        };
        let mut seqs: Vec<Vec<String>> = Vec::new();
        for parent in &parents {
            if self.classes.contains_key(parent) {
                let mro = self.compute_class_mro(parent, stack)?;
                seqs.push(mro);
            } else if parent == "Any" {
                // Any implicitly inherits from Mu
                seqs.push(vec!["Any".to_string(), "Mu".to_string()]);
            } else if parent == "Cool" {
                seqs.push(vec![
                    "Cool".to_string(),
                    "Any".to_string(),
                    "Mu".to_string(),
                ]);
            } else {
                seqs.push(vec![parent.clone()]);
            }
        }
        seqs.push(parents.clone());
        let mut result = vec![class_name.to_string()];
        while seqs.iter().any(|s| !s.is_empty()) {
            let mut candidate = None;
            for seq in &seqs {
                if seq.is_empty() {
                    continue;
                }
                let head = &seq[0];
                let mut in_tail = false;
                for other in &seqs {
                    if other.len() > 1 && other[1..].contains(head) {
                        in_tail = true;
                        break;
                    }
                }
                if !in_tail {
                    candidate = Some(head.clone());
                    break;
                }
            }
            if let Some(head) = candidate {
                result.push(head.clone());
                for seq in seqs.iter_mut() {
                    if !seq.is_empty() && seq[0] == head {
                        seq.remove(0);
                    }
                }
            } else {
                stack.pop();
                return Err(RuntimeError::new(format!(
                    "Inconsistent class hierarchy for {}",
                    class_name
                )));
            }
        }
        stack.pop();
        Ok(result)
    }

    /// Resolve the MRO for `class_name`, returning the cached `ClassDef::mro`
    /// when present, the hardcoded hierarchy for built-in types that are not
    /// user-defined classes, and otherwise computing + caching via
    /// [`Registry::compute_class_mro`]. Single write guard for the whole op.
    pub(crate) fn class_mro(&mut self, class_name: &str) -> Vec<String> {
        // Built-in type hierarchies for types that are not user-defined classes
        if !self.classes.contains_key(class_name) {
            let builtin_mro: Option<Vec<&str>> = match class_name {
                "Match" => Some(vec!["Match", "Capture", "Cool", "Any", "Mu"]),
                "Capture" => Some(vec!["Capture", "Any", "Mu"]),
                "Distribution::Path" => {
                    Some(vec!["Distribution::Path", "Distribution", "Any", "Mu"])
                }
                "Distribution::Hash" => {
                    Some(vec!["Distribution::Hash", "Distribution", "Any", "Mu"])
                }
                "Distribution::Installation" => Some(vec![
                    "Distribution::Installation",
                    "Distribution",
                    "Any",
                    "Mu",
                ]),
                "CompUnit::DependencySpecification" => {
                    Some(vec!["CompUnit::DependencySpecification", "Any", "Mu"])
                }
                "CompUnit::Repository::FileSystem" => Some(vec![
                    "CompUnit::Repository::FileSystem",
                    "CompUnit::Repository",
                    "Any",
                    "Mu",
                ]),
                "CompUnit::Repository::Installation" => Some(vec![
                    "CompUnit::Repository::Installation",
                    "CompUnit::Repository::Installable",
                    "CompUnit::Repository::Locally",
                    "CompUnit::Repository",
                    "Any",
                    "Mu",
                ]),
                _ => None,
            };
            if let Some(mro) = builtin_mro {
                return mro.into_iter().map(String::from).collect();
            }
        }
        if !self.classes.contains_key(class_name)
            && let Some((base, _)) = class_name.split_once('[')
            && class_name.ends_with(']')
            && self.classes.contains_key(base)
        {
            let mut mro = vec![class_name.to_string()];
            mro.extend(self.class_mro(base));
            return mro;
        }
        if let Some(class_def) = self.classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return class_def.mro.clone();
        }
        let mut stack = Vec::new();
        match self.compute_class_mro(class_name, &mut stack) {
            Ok(mro) => {
                if let Some(class_def) = self.classes.get_mut(class_name) {
                    class_def.mro = mro.clone();
                }
                mro
            }
            Err(_) => vec![class_name.to_string()],
        }
    }

    /// Read-only MRO lookup: the cached `ClassDef::mro` when present, otherwise a
    /// single-element MRO `[class_name]`. Returns `None` when the class is not
    /// registered. Used by non-`&mut` helpers that must not trigger computation.
    pub(crate) fn class_mro_cached(&self, class_name: &str) -> Option<Vec<String>> {
        let class_def = self.classes.get(class_name)?;
        if !class_def.mro.is_empty() {
            return Some(class_def.mro.clone());
        }
        Some(vec![class_name.to_string()])
    }

    /// Whether `class_name` (or any class in its MRO) defines `method_name`
    /// either as a user method or a native method. Pure registry MRO walk.
    pub(crate) fn class_has_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && (class_def.methods.contains_key(method_name)
                    || class_def.native_methods.contains(method_name))
            {
                return true;
            }
        }
        false
    }

    /// The method overloads named `method_name` defined directly on `class_name`
    /// (not inherited). Owned clone — `MethodDef` is `Arc`-backed so the clone is
    /// O(overload count) refcount bumps, matching the prior `.cloned()` call sites.
    pub(crate) fn get_method_overloads(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Option<Vec<MethodDef>> {
        self.classes
            .get(class_name)
            .and_then(|c| c.methods.get(method_name))
            .cloned()
    }

    /// Bound role type parameters for `class_name` (e.g. the `::T` -> value map
    /// of a `class C does R[Int]`). Owned clone.
    pub(crate) fn get_role_param_bindings(
        &self,
        class_name: &str,
    ) -> Option<HashMap<String, Value>> {
        self.class_role_param_bindings.get(class_name).cloned()
    }

    /// Whether `name` is marked `is hidden` (excluded from `.^mro` etc.).
    pub(crate) fn is_hidden_class(&self, name: &str) -> bool {
        self.hidden_classes.contains(name)
    }

    /// Whether `owner` is a deferred `is hidden` parent of `class`. Predicate form
    /// (not an owned-set getter) so the `&self`-only caller keeps the guard local
    /// and clones nothing.
    pub(crate) fn is_hidden_defer_parent(&self, class: &str, owner: &str) -> bool {
        self.hidden_defer_parents
            .get(class)
            .is_some_and(|h| h.contains(owner))
    }

    /// Seed for the composed-role transitive walk: the base names of every role
    /// composed into any class in `mro`, in MRO-then-declaration order. The
    /// parametric suffix is stripped (`R[Int]` -> `R`). Push order is load-bearing
    /// — the caller consumes this LIFO via `.pop()` and relies on first-match-wins,
    /// so this method MUST NOT dedup or sort (dedup happens during the walk).
    pub(crate) fn composed_roles_seed(&self, mro: &[String]) -> Vec<String> {
        let mut seed = Vec::new();
        for cn in mro {
            if let Some(composed) = self.class_composed_roles.get(cn.as_str()) {
                for cr in composed {
                    let base = cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                    seed.push(base.to_string());
                }
            }
        }
        seed
    }
}

// ---------------------------------------------------------------------------
// Reentrancy-detecting guards (debug-only)
// ---------------------------------------------------------------------------
//
// `Arc<RwLock<Registry>>` is a non-reentrant lock. The lock discipline (see the
// module docs) is: never hold a read/write guard across a call that re-enters
// user-code execution, because the re-entry will try to re-acquire the lock and
// deadlock. Registration paths (`register_*_decl`) are the prime offenders —
// they interleave registry writes with `eval_block_value` / `run_block_raw` /
// `call_function` (class body statements, trait handlers, attribute defaults,
// enum variant values, parametric role bodies), so a stray held guard silently
// deadlocks.
//
// A silent deadlock is only caught by `make roast`'s ~13-min timeout, and a
// static text scanner misses code shapes the borrow checker also misses (PR-A
// slice 5: a `write -> write` inside a `match self.registry_mut()...{}` arm hung
// at runtime despite passing both checks). So instead we detect re-entry at
// runtime, in debug builds: each acquisition checks a thread-local record of the
// guards this thread currently holds *on that same lock* and panics with a
// located message before the would-be-deadlocking `.read()`/`.write()` call.
//
// The record is keyed by the lock's address, NOT just by thread: a single thread
// legitimately holds guards on *different* registries at once — e.g.
// `self.registry_mut().classes = nested.registry().classes.clone();` holds a
// write guard on `self`'s registry and a read guard on a sub-interpreter's
// (`nested`) registry simultaneously. Those are independent locks, so there is no
// deadlock; a thread-global flag would false-positive. Only re-acquiring the
// *same* lock deadlocks.
//
// The allowed/forbidden matrix (per lock) matches `std::sync::RwLock`'s actual
// deadlock conditions:
//   - acquiring a WRITE while ANY guard (read or write) on the same lock is held -> deadlock
//   - acquiring a READ while a WRITE on the same lock is held                    -> deadlock
//   - acquiring a READ while only READ guards on the same lock are held          -> tolerated
//     (nested reads are relied upon, e.g. `a().x && b().y` keeps both temporary
//     read guards alive to end-of-statement).
//
// This is `#[cfg(debug_assertions)]` only: the per-access bookkeeping would
// otherwise tax the hot `registry()` read path (method dispatch reads MRO through
// it). CI's release `make roast` still backstops via timeout.

#[cfg(debug_assertions)]
mod reentry_check {
    use std::cell::RefCell;
    use std::collections::HashMap;

    thread_local! {
        // lock address -> (outstanding read guards, write guard held?) on this thread.
        static HELD: RefCell<HashMap<usize, (u32, bool)>> = RefCell::new(HashMap::new());
    }

    /// Called before `RwLock::read()`. Panics if this thread already holds a write
    /// guard on the same lock (read-while-write deadlocks).
    pub(super) fn enter_read(lock: usize) {
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            let entry = h.entry(lock).or_insert((0, false));
            assert!(
                !entry.1,
                "registry(): a read guard was acquired while this thread already \
                 holds a write guard on the same registry. RwLock<Registry> is not \
                 reentrant; drop the write guard before re-entering. See the lock \
                 discipline in src/runtime/registry.rs / docs/vm-registry-ownership.md.",
            );
            entry.0 += 1;
        });
    }

    pub(super) fn exit_read(lock: usize) {
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            if let Some(entry) = h.get_mut(&lock) {
                entry.0 = entry.0.saturating_sub(1);
                if entry.0 == 0 && !entry.1 {
                    h.remove(&lock);
                }
            }
        });
    }

    /// Called before `RwLock::write()`. Panics if this thread already holds any
    /// guard on the same lock (write-while-anything deadlocks).
    pub(super) fn enter_write(lock: usize) {
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            let entry = h.entry(lock).or_insert((0, false));
            assert!(
                !entry.1,
                "registry_mut(): a write guard was acquired while this thread \
                 already holds a write guard on the same registry (write -> write). \
                 RwLock<Registry> is not reentrant; consolidate into a single write \
                 guard. See src/runtime/registry.rs / docs/vm-registry-ownership.md.",
            );
            assert!(
                entry.0 == 0,
                "registry_mut(): a write guard was acquired while this thread holds \
                 a read guard on the same registry (read -> write upgrade). \
                 RwLock<Registry> is not reentrant; drop the read guard first. See \
                 src/runtime/registry.rs / docs/vm-registry-ownership.md.",
            );
            entry.1 = true;
        });
    }

    pub(super) fn exit_write(lock: usize) {
        HELD.with(|h| {
            let mut h = h.borrow_mut();
            if let Some(entry) = h.get_mut(&lock) {
                entry.1 = false;
                if entry.0 == 0 {
                    h.remove(&lock);
                }
            }
        });
    }
}

/// Read guard for the shared [`Registry`]. Derefs to `Registry`; in debug builds
/// it records the acquisition in a thread-local so a re-entrant lock attempt
/// panics with a located message instead of silently deadlocking (see the
/// `reentry_check` module above).
pub(crate) struct RegistryReadGuard<'a> {
    inner: std::sync::RwLockReadGuard<'a, Registry>,
    #[cfg(debug_assertions)]
    lock_id: usize,
}

impl<'a> RegistryReadGuard<'a> {
    pub(crate) fn new(lock: &'a std::sync::RwLock<Registry>) -> Self {
        #[cfg(debug_assertions)]
        let lock_id = lock as *const _ as usize;
        #[cfg(debug_assertions)]
        reentry_check::enter_read(lock_id);
        // Acquire only after the reentry check, so a would-be deadlock panics
        // instead of hanging on the blocking `.read()`.
        let inner = lock.read().unwrap();
        Self {
            inner,
            #[cfg(debug_assertions)]
            lock_id,
        }
    }
}

impl std::ops::Deref for RegistryReadGuard<'_> {
    type Target = Registry;
    #[inline]
    fn deref(&self) -> &Registry {
        &self.inner
    }
}

#[cfg(debug_assertions)]
impl Drop for RegistryReadGuard<'_> {
    fn drop(&mut self) {
        reentry_check::exit_read(self.lock_id);
    }
}

/// Write guard for the shared [`Registry`]. Same reentry instrumentation as
/// [`RegistryReadGuard`].
pub(crate) struct RegistryWriteGuard<'a> {
    inner: std::sync::RwLockWriteGuard<'a, Registry>,
    #[cfg(debug_assertions)]
    lock_id: usize,
}

impl<'a> RegistryWriteGuard<'a> {
    pub(crate) fn new(lock: &'a std::sync::RwLock<Registry>) -> Self {
        #[cfg(debug_assertions)]
        let lock_id = lock as *const _ as usize;
        #[cfg(debug_assertions)]
        reentry_check::enter_write(lock_id);
        let inner = lock.write().unwrap();
        Self {
            inner,
            #[cfg(debug_assertions)]
            lock_id,
        }
    }
}

impl std::ops::Deref for RegistryWriteGuard<'_> {
    type Target = Registry;
    #[inline]
    fn deref(&self) -> &Registry {
        &self.inner
    }
}

impl std::ops::DerefMut for RegistryWriteGuard<'_> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Registry {
        &mut self.inner
    }
}

#[cfg(debug_assertions)]
impl Drop for RegistryWriteGuard<'_> {
    fn drop(&mut self) {
        reentry_check::exit_write(self.lock_id);
    }
}
