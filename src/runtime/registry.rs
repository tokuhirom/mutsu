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

// Registry maps are String-keyed and hit on dispatch cache-miss paths; use
// FxHash instead of SipHash (registry keys are program identifiers, not
// attacker-controlled data, so HashDoS hardening buys nothing here). The
// `HashMap`/`HashSet` names are aliased so the ~40 field declarations below
// stay textually unchanged.
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::sync::Arc;

use crate::ast::FunctionDef;
use crate::symbol::Symbol;
use crate::value::{EnumValue, RuntimeError, Value};

use super::{ClassDef, MethodDef, RoleCandidateDef, RoleDef, SubsetDef};

/// Canonical method-table key. Both built-in handlers and user candidates will
/// occupy this namespace; the latter still live in `ClassDef::methods` during
/// the next migration step.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct MethodEntryKey {
    pub(crate) owner: Symbol,
    pub(crate) name: Symbol,
}

#[derive(Clone, Default)]
pub(crate) struct MethodEntry {
    pub(crate) builtin: Option<crate::builtins::builtin_type_methods::BuiltinMethodEntry>,
    pub(crate) user_candidates: Vec<MethodDef>,
    /// Visibility of the auto-generated accessor for a `has $.x`/`has $!x`
    /// attribute this class declares directly, keyed by attribute name under
    /// the same `(owner, name)` row as a same-named method would occupy
    /// (ADR-0019 D2d). `Some(is_public)`, not a nested `Option` around a
    /// descriptor: callers only ever need the visibility bit, not the full
    /// `ClassAttributeDef` (that stays in `ClassDef::attributes`, the source
    /// this is synced from).
    pub(crate) accessor: Option<bool>,
    /// `proto method`/`proto submethod` body declared directly on this
    /// `(owner, name)` (ADR-0019 E8, folded from the formerly-standalone
    /// `Registry::proto_methods` table in E8b/E8c). Written by
    /// [`Registry::set_proto_method`]; read by `Interpreter::
    /// lookup_proto_method`'s MRO walk via [`Registry::method_entry_proto`].
    pub(crate) proto: Option<FunctionDef>,
}

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
    /// Canonical type x method table. It initially owns the built-in entries;
    /// declaration registration will add user candidates to the same table.
    pub(crate) method_entries: HashMap<MethodEntryKey, MethodEntry>,
    /// Monotonic invalidation generation for the canonical method table.
    pub(crate) method_generation: u64,
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
    /// Classes declared `is repr('CStruct')` (native interop). Used by
    /// NativeCall to recognize a signature type as an opaque native handle
    /// passed by pointer, even when the class name is lowercase (e.g.
    /// `evp_cipher_st`) and so would not match the name-shape heuristic.
    pub(crate) cstruct_classes: HashSet<String>,
    /// Classes declared `is repr('CPointer')` — an opaque native handle with no
    /// declared field layout of its own (OpenSSL's `BIO`). Tracked separately
    /// from [`cstruct_classes`] because such a class has no layout to compute,
    /// but a *field* of that type is still one pointer wide inside an enclosing
    /// CStruct.
    pub(crate) cpointer_classes: HashSet<String>,
    /// Classes marked `is hidden` (excluded from `.^mro` etc.).
    pub(crate) hidden_classes: HashSet<String>,
    /// Forward-declared class stubs (`class Foo { ... }` declared later).
    pub(crate) class_stubs: HashSet<String>,
    /// Forward-declared package stubs.
    pub(crate) package_stubs: HashSet<String>,
    /// Declarator keyword used for a bare `package`/`module`/`grammar` (a
    /// `Stmt::Package`), so `.HOW` reports the matching metaclass
    /// (`PackageHOW`/`ModuleHOW`/`GrammarHOW`) instead of the default
    /// `ClassHOW`. Classes and roles are absent (they default to `ClassHOW` /
    /// the role HOW).
    pub(crate) package_kinds: HashMap<String, crate::ast::PackageKind>,
    /// `is hidden` parents deferred until the parent is fully declared.
    pub(crate) hidden_defer_parents: HashMap<String, HashSet<String>>,
    /// `trusts` relationships: class -> set of trusted classes.
    pub(crate) class_trusts: HashMap<String, HashSet<String>>,
    /// Per-class metaclass (`HOW`) value override.
    pub(crate) class_how_values: HashMap<String, Value>,
    /// Grammars declared under a custom EXPORTHOW `grammar` metaclass whose HOW
    /// class defines a user `find_method`: grammar name -> HOW instance. The
    /// regex engine consults this to route subrule dispatch through the custom
    /// `find_method` (Metamodel::GrammarHOW protocol).
    pub(crate) grammar_custom_how: HashMap<String, Value>,
    /// Classes declared under a custom EXPORTHOW `class` metaclass whose HOW
    /// class defines a user `compose` method: names queued for a post-`is`-trait
    /// `compose` call. Drained by the class-decl VM op after custom `is` traits
    /// run (so `@!aspects` populated by the trait is visible to `compose`), which
    /// wraps the class's methods (`advent2011-day14` AOP).
    pub(crate) pending_class_compose: Vec<String>,
    /// True once any user class inheriting a builtin metamodel class
    /// (Metamodel::ClassHOW / Metamodel::GrammarHOW) has been declared. Cheap
    /// gate for the per-dispatch `is_metamodel_how_class` check.
    pub(crate) has_metamodel_how_classes: bool,
    /// Roles composed into each class: class -> [role names]. This is the
    /// FLATTENED set (includes roles reached transitively through a composed
    /// role's own `does`), used for `~~`/role-membership checks.
    pub(crate) class_composed_roles: HashMap<String, Vec<String>>,
    /// Roles DIRECTLY declared on each class's `does` list (NOT the transitive
    /// closure): class -> [role names]. Qualified `self.Role::method` resolution
    /// of a parametric role uses this so a concretization reached only
    /// transitively (e.g. `R1[Num]` pulled in via `does R2[Num]` where
    /// `R2[::T] does R1[::T]`) does not make a directly-declared `R1[Int]`
    /// ambiguous (Raku resolves a qualified role call against the immediate
    /// roles of the consumer).
    pub(crate) class_direct_composed_roles: HashMap<String, Vec<String>>,
    /// Roles composed PURELY via `does` (not `is Role` puns): class -> [role names].
    /// A `does`-composed role provides methods but is NOT an MRO entry in Rakudo's
    /// `.^mro_unhidden`, so this set is filtered out of that introspection.
    pub(crate) class_does_only_roles: HashMap<String, Vec<String>>,
    /// Roles implicitly composed by enums: enum -> [role names].
    pub(crate) class_enum_roles: HashMap<String, Vec<String>>,
    /// Subs declared inside a class body: class -> (sub name -> value).
    pub(crate) class_subs: HashMap<String, HashMap<String, Value>>,
    /// Per-attribute `BUILD` override: (class, attr) -> builder value.
    pub(crate) attribute_build_overrides: HashMap<(String, String), Value>,
    /// Per-attribute default value: (class, attr) -> default value.
    pub(crate) class_attribute_defaults: HashMap<(String, String), Value>,
    /// Per-attribute `is default(...)` expression for a parametric role, where the
    /// value cannot be evaluated until the role is composed and its type params
    /// are bound: (role-base, attr) -> expr. Evaluated at instance construction
    /// (with role param bindings in scope) to tag `@`/`%` containers.
    pub(crate) role_attribute_default_exprs: HashMap<(String, String), crate::opcode::DeclTraitArg>,
    /// Class-level (`my $.x` / `our $.x`) role attributes: (role-base, attr) ->
    /// optional default expr. These generate an accessor on the *type object*
    /// (not per-instance), so at composition they are copied into the consuming
    /// class's `class_level_attrs` rather than its per-instance attributes.
    pub(crate) role_class_level_attrs:
        HashMap<(String, String), Option<crate::opcode::DeclTraitArg>>,
    /// Per-attribute deferred `is default(...)` expression carried from a composed
    /// parametric role onto a consuming class: (class, attr) -> expr. Evaluated at
    /// construction with the class's role type-param bindings in scope.
    pub(crate) class_attribute_default_exprs:
        HashMap<(String, String), crate::opcode::DeclTraitArg>,
    /// Per-attribute declared type: (class, attr) -> type name.
    pub(crate) class_attribute_is_types: HashMap<(String, String), String>,
    /// Per-attribute `is Type` container trait declared on a *role* attribute:
    /// (role, attr) -> type name. Carried onto a consuming class's
    /// `class_attribute_is_types` at composition (`has @.a is G::A` in a
    /// parametric role) so the attribute's element type is enforced.
    pub(crate) role_attribute_is_types: HashMap<(String, String), String>,
    /// Per-attribute declared type constraint on a *role* attribute
    /// (`role R { has Int $.x }`, `has Callable %!c{Mu:U}`): (role, attr) ->
    /// type constraint, in the same `ValueType{KeyType}` encoding a class
    /// attribute uses. Roles keep their own table because a role is registered
    /// before it is known which class will consume it; at composition (and when
    /// a role is punned to a class) the entries are copied — with role type
    /// parameters substituted — into the class's `attribute_types`.
    pub(crate) role_attribute_types: HashMap<(String, String), String>,
    /// Per-attribute definiteness smiley on a *role* attribute
    /// (`role R { has Int:D $.x }`): (role, attr) -> "D"/"U"/"_". Copied
    /// alongside `role_attribute_types`.
    pub(crate) role_attribute_smileys: HashMap<(String, String), String>,
    /// Per-attribute `does Role` traits: (class, attr) -> role names mixed into
    /// the attribute's container (`has $.x does Foo`). Applied to the attribute's
    /// value at construction so `$o.x` does the role.
    pub(crate) class_attribute_does_roles: HashMap<(String, String), Vec<String>>,
    /// Per-attribute container role mixins recorded by a custom `trait_mod:<is>`
    /// that mixes a role into `$attr.container.VAR` (e.g.
    /// `$a.container.VAR does doc($arg)`): (class, attr) -> list of mixin-override
    /// maps (each holding `__mutsu_role__X` / `__mutsu_attr__X` keys). Applied to
    /// the attribute's value at construction so `$o.attr.VAR` does the role.
    pub(crate) class_attribute_container_mixins:
        HashMap<(String, String), Vec<HashMap<String, Value>>>,
    /// Per-attribute `is DEPRECATED` message: (class, attr) -> message.
    pub(crate) class_attribute_deprecated: HashMap<(String, String), String>,
    /// The Attribute meta-object a custom `trait_mod:<is>` was applied to,
    /// per (class, attr). Instance attrs are a shared cell, so mutations the
    /// trait made (`$a does JSON::Name::NamedAttribute; $a.json-name = ...`)
    /// live in this stored object; `^attributes` returns it (topped up with
    /// the standard meta keys) so the mixin state survives introspection.
    pub(crate) class_attribute_trait_objects: HashMap<(String, String), crate::value::Value>,

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
    /// Compositions whose role body has already been run, keyed
    /// `"{kind}:{role}"` (`pun:R`, `mixin:R`). Rakudo runs a role body once per
    /// composition and memoises the composed type, so punning a role twice — or
    /// mixing it into a second value — must not run its body again. A `does`
    /// composition is not memoised here: each consuming class is its own
    /// composition and runs the body again, as Rakudo does.
    pub(crate) composed_role_bodies: HashSet<String>,
    /// Bound role type parameters per class: class -> (param name -> value).
    pub(crate) class_role_param_bindings: HashMap<String, HashMap<String, Value>>,

    // ----- functions / subs / tokens (PR-A slice 5, final PR-A slice) -----
    /// User-defined subs: fully-qualified name -> [`FunctionDef`]. Read on the
    /// sub/multi-dispatch hot path; callers clone the matched `Arc<FunctionDef>`
    /// (a cheap refcount bump) under a short-lived guard. Held behind `Arc` so
    /// the per-call `snapshot_routine_registry` clone (taken whenever a routine
    /// declaring inner `my sub`s is entered, to scope them) is an O(n) Arc-bump
    /// rather than a deep copy of every routine body in the program.
    pub(crate) functions: HashMap<Symbol, std::sync::Arc<FunctionDef>>,
    /// `our`-scoped subs that persist across block boundaries. Held behind `Arc`
    /// (like `functions`) so block-scope restore and whole-registry clones
    /// (`clone_for_thread`, EVAL copy) share the def rather than deep-cloning it;
    /// the same `Arc` is also what gets re-inserted into `functions`.
    pub(crate) our_scoped_functions: HashMap<Symbol, std::sync::Arc<FunctionDef>>,
    /// `proto sub` markers (multi proto stubs): name -> proto `FunctionDef`.
    pub(crate) proto_functions: HashMap<Symbol, std::sync::Arc<FunctionDef>>,
    /// Grammar token/rule definitions: name -> [overloads]. Each overload is
    /// held behind `Arc` so the whole-map snapshot/restore clones (and the
    /// per-resolution candidate merges) are O(n) refcount bumps rather than
    /// deep clones of the token bodies.
    pub(crate) token_defs: HashMap<Symbol, Vec<std::sync::Arc<FunctionDef>>>,
    /// `proto sub` declaration markers (existence set).
    pub(crate) proto_subs: HashSet<String>,
    /// `proto token`/`proto rule` declaration markers (existence set).
    pub(crate) proto_tokens: HashSet<String>,
    /// Whether ANY `proto method`/`proto submethod` has been declared
    /// anywhere in the program (ADR-0019 E8c). A monotonic flag — proto
    /// bodies are never unregistered — set by [`Registry::set_proto_method`]
    /// and consulted by `Interpreter::lookup_proto_method` as a cheap
    /// whole-program fast path (skip the MRO walk entirely when no class has
    /// ever declared a proto method), the same role the now-retired
    /// `proto_methods.is_empty()` check on the standalone table used to
    /// play. The actual proto bodies live in `MethodEntry::proto`
    /// (`method_entries`), read per-owner via [`Registry::method_entry_proto`].
    pub(crate) has_proto_methods: bool,
}

impl Registry {
    /// Install the static built-in catalog when a registry is constructed.
    /// This is data-only initialization: it must not invoke native handlers.
    pub(crate) fn seed_builtin_method_entries(&mut self) {
        use crate::builtins::builtin_type_methods::{
            BUILTIN_METHOD_OWNERS, builtin_method_entries,
        };

        for owner in BUILTIN_METHOD_OWNERS {
            for entry in builtin_method_entries(owner) {
                let key = MethodEntryKey {
                    owner: Symbol::intern(entry.owner),
                    name: Symbol::intern(entry.name),
                };
                let slot = self.method_entries.entry(key).or_default();
                debug_assert!(slot.builtin.is_none(), "duplicate built-in method entry");
                slot.builtin = Some(entry);
            }
        }
        self.bump_method_generation();
    }

    pub(crate) fn builtin_method_names(&self, type_name: &str) -> Vec<&'static str> {
        let owner = crate::builtins::builtin_type_methods::canonical_builtin_owner(type_name);
        if owner.is_empty() {
            return Vec::new();
        }
        let owner = Symbol::intern(owner);
        let mut entries: Vec<_> = self
            .method_entries
            .iter()
            .filter_map(|(key, entry)| (key.owner == owner).then_some(entry.builtin).flatten())
            .collect();
        entries.sort_unstable_by_key(|entry| entry.order);
        entries.into_iter().map(|entry| entry.name).collect()
    }

    pub(crate) fn sync_user_method_entries(&mut self, class_name: &str) {
        let owner = Symbol::intern(class_name);
        self.method_entries.retain(|key, entry| {
            if key.owner == owner {
                entry.user_candidates.clear();
                entry.accessor = None;
            }
            // `entry.proto` (ADR-0019 E8b) is NOT reset here even for a
            // `key.owner == owner` row: unlike `user_candidates`/`accessor`,
            // it has no `ClassDef`-backed source this function re-derives
            // from below (it is written once, directly, by
            // `Registry::set_proto_method` at proto-method declaration
            // time) — clearing it here would just delete it with nothing to
            // repopulate it. It must still count toward keeping the row
            // alive, or a proto-only entry (no builtin/user_candidates/
            // accessor) is silently dropped the next time ANY sync call
            // touches this owner (composition, augmentation, re-declaration
            // — all of `registration_class_body.rs`'s own call sites run
            // this after the proto decl already landed), which is exactly
            // what the E8b shadow probe caught during its first sweep.
            entry.builtin.is_some()
                || !entry.user_candidates.is_empty()
                || entry.accessor.is_some()
                || entry.proto.is_some()
        });
        let Some(class_def) = self.classes.get(class_name) else {
            self.bump_method_generation();
            return;
        };
        let methods = class_def.methods.clone();
        let attributes = class_def.attributes.clone();
        for (name, candidates) in methods {
            self.method_entries
                .entry(MethodEntryKey {
                    owner,
                    name: Symbol::intern(&name),
                })
                .or_default()
                .user_candidates = candidates;
        }
        // A later same-name declaration within the class overrides an earlier
        // one (mirrors `collect_class_attributes`'/`has_public_accessor`'s
        // former remove-then-push / rev().find() semantics): iterating in
        // declaration order and letting each write clobber the last gives the
        // same "most recent wins" result.
        for attr in &attributes {
            self.method_entries
                .entry(MethodEntryKey {
                    owner,
                    name: Symbol::intern(&attr.name),
                })
                .or_default()
                .accessor = Some(attr.is_public);
        }
        self.bump_method_generation();
    }

    pub(crate) fn user_method_overloads(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Option<Vec<MethodDef>> {
        self.method_entries
            .get(&MethodEntryKey {
                owner: Symbol::intern(class_name),
                name: Symbol::intern(method_name),
            })
            .filter(|entry| !entry.user_candidates.is_empty())
            .map(|entry| entry.user_candidates.clone())
    }

    /// Visibility of the auto-generated accessor `method_name` declares
    /// directly on `class_name`, if any (ADR-0019 D2d). `None` means this
    /// class does not declare an attribute of that name at all — distinct
    /// from `Some(false)` (a private attribute, which still occupies the
    /// name and must not fall through to an ancestor's same-named accessor).
    pub(crate) fn accessor_is_public(&self, class_name: &str, method_name: &str) -> Option<bool> {
        self.method_entries
            .get(&MethodEntryKey {
                owner: Symbol::intern(class_name),
                name: Symbol::intern(method_name),
            })
            .and_then(|entry| entry.accessor)
    }

    /// Per-level (`has_local_method`, `has_role_method`) presence used by
    /// `resolve_user_method_or_accessor` (ADR-0019 D2d): whether `class_name`
    /// directly declares a visible (non-private, not `is my`-shadowed-from-an-
    /// ancestor) candidate for `method_name`, split by whether it originated
    /// in a composed role. A table probe against `method_entries` instead of
    /// `class_def.methods.get(...)` plus a `Vec<MethodDef>` clone — same data,
    /// no allocation, since only the two booleans are needed here.
    pub(crate) fn user_method_local_role_presence(
        &self,
        class_name: &str,
        method_name: &str,
        is_ancestor: bool,
    ) -> (bool, bool) {
        let Some(entry) = self.method_entries.get(&MethodEntryKey {
            owner: Symbol::intern(class_name),
            name: Symbol::intern(method_name),
        }) else {
            return (false, false);
        };
        let (mut local, mut role) = (false, false);
        for d in &entry.user_candidates {
            if d.is_private || (d.is_my && is_ancestor) {
                continue;
            }
            if d.role_origin.is_none() {
                local = true;
            } else {
                role = true;
            }
        }
        (local, role)
    }

    pub(crate) fn replace_method_entries_from(&mut self, source: &Self) {
        self.method_entries = source.method_entries.clone();
        self.bump_method_generation();
    }

    /// Register a `proto method`/`proto submethod` body for `(class_name,
    /// method_name)` (ADR-0019 E8, authoritative since E8c). Single call site
    /// (`registration_class_body.rs`'s `class_body_proto_method_decl`).
    pub(crate) fn set_proto_method(
        &mut self,
        class_name: &str,
        method_name: &str,
        def: FunctionDef,
    ) {
        self.method_entries
            .entry(MethodEntryKey {
                owner: Symbol::intern(class_name),
                name: Symbol::intern(method_name),
            })
            .or_default()
            .proto = Some(def);
        self.has_proto_methods = true;
        self.bump_method_generation();
    }

    /// The `MethodEntry.proto` column at exactly `(class_name, method_name)`
    /// — no MRO walk (the caller supplies the chain, mirroring how
    /// `user_method_overloads` is a per-level, not per-chain, probe). The
    /// single read site for `Interpreter::lookup_proto_method`'s MRO walk
    /// (ADR-0019 E8c).
    pub(crate) fn method_entry_proto(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Option<FunctionDef> {
        self.method_entries
            .get(&MethodEntryKey {
                owner: Symbol::intern(class_name),
                name: Symbol::intern(method_name),
            })
            .and_then(|entry| entry.proto.clone())
    }

    fn bump_method_generation(&mut self) {
        self.method_generation = self.method_generation.wrapping_add(1);
        if self.method_generation == 0 {
            self.method_generation = 1;
        }
    }
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
            return Ok(class_def.mro.iter().map(|s| s.resolve()).collect());
        }
        stack.push(class_name.to_string());
        let explicit_parents = self
            .classes
            .get(class_name)
            .map(|c| c.parents.clone())
            .unwrap_or_default();
        // If a user-defined class has no explicit parents, it implicitly
        // inherits from Any (which in turn inherits from Mu).  This matches
        // Raku's default class hierarchy. A directly-*augmented* builtin
        // collection/Cool type (`augment class Array {...}`, which registers a
        // `ClassDef` for "Array" with no `is` clause of its own) instead keeps
        // its real builtin parent (`List`, not `Any`) so the rest of its
        // catalog ancestor chain (`Cool`/`Any`/`Mu`) is still reachable below.
        let parents = if explicit_parents.is_empty() && self.classes.contains_key(class_name) {
            match crate::builtins::builtin_type_catalog::builtin_type_info(class_name) {
                Some(info) if info.mro.len() > 1 => vec![info.mro[1].to_string()],
                _ => vec!["Any".to_string()],
            }
        } else {
            explicit_parents
        };
        // Reorder so a `is Role` parent's class-ancestor sits immediately after the
        // role (`class C3 is R3a is R3b` where `role R3a is C2` -> C3, R3a, C2, C1,
        // R3b, ...  matching Rakudo's C3 linearization). Registration appends the
        // role's class-ancestor at the END of `parents`, which would otherwise let a
        // sibling parent (`is R3b`) sort ahead of it.
        let parents: Vec<String> = {
            let mut ordered: Vec<String> = Vec::new();
            for p in &parents {
                if !ordered.contains(p) {
                    ordered.push(p.clone());
                }
                if let Some(rps) = self.role_parents.get(p) {
                    for rp in rps {
                        let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp).to_string();
                        if self.classes.contains_key(&rp_base) && !ordered.contains(&rp_base) {
                            ordered.push(rp_base);
                        }
                    }
                }
            }
            for p in &parents {
                if !ordered.contains(p) {
                    ordered.push(p.clone());
                }
            }
            ordered
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
            } else if let Some((base, _)) = parent.split_once('[')
                && parent.ends_with(']')
                && !self.roles.contains_key(base)
            {
                // A parametric CLASS/native parent (e.g. `Array[Int]`) contributes
                // both the parameterized name and its base type's MRO, so that a
                // subclass of `Array[Int]` is recognized as array-backed (`Array`
                // in the MRO). A parametric ROLE parent (`does R[Int]`) is left as
                // just the parameterized name: its base must NOT enter the MRO, or
                // a qualified `self.R::meth` call would resolve against the bare
                // base and skip the two-concretization ambiguity check.
                let mut seq = vec![parent.clone()];
                if self.classes.contains_key(base) {
                    seq.extend(self.compute_class_mro(base, stack)?);
                } else {
                    seq.push(base.to_string());
                }
                seqs.push(seq);
            } else if let Some(info) =
                crate::builtins::builtin_type_catalog::builtin_type_info(parent)
            {
                // An unregistered bare (non-parametrized; the bracketed case is
                // handled above) builtin collection/Cool parent (`List`,
                // `Hash`, `Range`, ...): use the catalog's own full ancestor
                // chain instead of stopping at the bare name (which would
                // silently drop `Cool`/`Any`/`Mu`, the same gap `Any`/`Cool`
                // are hardcoded above to avoid).
                seqs.push(info.mro.iter().map(|s| s.to_string()).collect());
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

    /// Hardcoded MRO for built-in types that are not user-defined classes.
    fn builtin_mro_table(class_name: &str) -> Option<&'static [&'static str]> {
        match class_name {
            "Match" => Some(&["Match", "Capture", "Cool", "Any", "Mu"]),
            "Capture" => Some(&["Capture", "Any", "Mu"]),
            "IO::Spec" => Some(&["IO::Spec", "Any", "Mu"]),
            "IO::Spec::Unix" => Some(&["IO::Spec::Unix", "IO::Spec", "Any", "Mu"]),
            // Win32/Cygwin/QNX specialize the Unix spec (Raku MRO).
            "IO::Spec::Win32" => {
                Some(&["IO::Spec::Win32", "IO::Spec::Unix", "IO::Spec", "Any", "Mu"])
            }
            "IO::Spec::Cygwin" => Some(&[
                "IO::Spec::Cygwin",
                "IO::Spec::Unix",
                "IO::Spec",
                "Any",
                "Mu",
            ]),
            "IO::Spec::QNX" => Some(&["IO::Spec::QNX", "IO::Spec::Unix", "IO::Spec", "Any", "Mu"]),
            "Distribution::Path" => Some(&["Distribution::Path", "Distribution", "Any", "Mu"]),
            "Distribution::Hash" => Some(&["Distribution::Hash", "Distribution", "Any", "Mu"]),
            "Distribution::Installation" => {
                Some(&["Distribution::Installation", "Distribution", "Any", "Mu"])
            }
            "CompUnit::DependencySpecification" => {
                Some(&["CompUnit::DependencySpecification", "Any", "Mu"])
            }
            "CompUnit::Repository::FileSystem" => Some(&[
                "CompUnit::Repository::FileSystem",
                "CompUnit::Repository",
                "Any",
                "Mu",
            ]),
            "CompUnit::Repository::Installation" => Some(&[
                "CompUnit::Repository::Installation",
                "CompUnit::Repository::Installable",
                "CompUnit::Repository::Locally",
                "CompUnit::Repository",
                "Any",
                "Mu",
            ]),
            _ => None,
        }
    }

    /// Resolve the MRO for `class_name`, returning the cached `ClassDef::mro`
    /// when present, the hardcoded hierarchy for built-in types that are not
    /// user-defined classes, and otherwise computing + caching via
    /// [`Registry::compute_class_mro`]. Single write guard for the whole op.
    pub(crate) fn class_mro(&mut self, class_name: &str) -> std::sync::Arc<[Symbol]> {
        if let Some(mro) = self.class_mro_readonly(class_name) {
            return mro;
        }
        // A parametrized name (`Blob[uint32]`) whose BASE class MRO is not yet
        // cached: the readonly twin declined (its recursion only reads), so
        // compute-and-cache the base through this write side, then prepend.
        // Falling through to `compute_class_mro(class_name)` instead would treat
        // the unregistered parametrized name as parentless and yield a wrong
        // single-element MRO.
        if !self.classes.contains_key(class_name)
            && let Some((base, _)) = class_name.split_once('[')
            && class_name.ends_with(']')
        {
            if self.classes.contains_key(base) {
                let mut mro = vec![Symbol::intern(class_name)];
                mro.extend(self.class_mro(base).iter().copied());
                return mro.into();
            }
            // `base` is not a user-registered class but IS a catalog builtin
            // (`Array[Int]`, `array[int32]`, `CArray[uint8]`, ...): splice the
            // catalog's own chain directly rather than recursing through
            // `class_mro`, whose `compute_class_mro` fallback would otherwise
            // treat an un-registered `base` like "Array" as parentless.
            if let Some(info) = crate::builtins::builtin_type_catalog::builtin_type_info(base) {
                let mut mro = vec![Symbol::intern(class_name)];
                mro.extend(info.mro.iter().map(|s| Symbol::intern(s)));
                return mro.into();
            }
        }
        let mut stack = Vec::new();
        match self.compute_class_mro(class_name, &mut stack) {
            Ok(mro) => {
                let mro: std::sync::Arc<[Symbol]> = mro.iter().map(|s| Symbol::intern(s)).collect();
                if let Some(class_def) = self.classes.get_mut(class_name) {
                    class_def.mro = mro.clone();
                }
                mro
            }
            Err(_) => [Symbol::intern(class_name)].into(),
        }
    }

    /// Read-only twin of [`Registry::class_mro`]: resolves every MRO shape that
    /// needs no cache write — the builtin table, parametrized names
    /// (`Blob[uint32]`), an already-cached `ClassDef::mro` — and returns `None`
    /// exactly when the write side would compute AND cache (a registered class
    /// whose `mro` is still empty). Callers holding only a read guard use this
    /// first so the hot dispatch path does not take `registry_mut()` (whose
    /// first mutable deref pays a full-registry COW clone after a spawn share).
    pub(crate) fn class_mro_readonly(&self, class_name: &str) -> Option<std::sync::Arc<[Symbol]>> {
        if !self.classes.contains_key(class_name) {
            if let Some(mro) = Self::builtin_mro_table(class_name) {
                return Some(mro.iter().map(|s| Symbol::intern(s)).collect());
            }
            if let Some((base, _)) = class_name.split_once('[')
                && class_name.ends_with(']')
            {
                if self.classes.contains_key(base) {
                    let mut mro = vec![Symbol::intern(class_name)];
                    mro.extend(self.class_mro_readonly(base)?.iter().copied());
                    return Some(mro.into());
                }
                if let Some(info) = crate::builtins::builtin_type_catalog::builtin_type_info(base) {
                    let mut mro = vec![Symbol::intern(class_name)];
                    mro.extend(info.mro.iter().map(|s| Symbol::intern(s)));
                    return Some(mro.into());
                }
            }
            // A bare (non-parametrized) builtin collection/Cool type name
            // (`Array`, `List`, `Hash`, `Range`, ...) that has never itself
            // been augmented: the catalog already carries its full ancestor
            // chain -- without this, an unregistered "Array" fell through to
            // `compute_class_mro`, which has no `ClassDef` to read parents
            // from and answers the singleton `[Array]`, silently losing
            // `List`/`Cool`/`Any`/`Mu`. That made `has_user_method("Array",
            // "first")` blind to a method the user augmented onto `List`
            // instead of `Array` directly (still legal raku -- `@a.first`
            // dispatches through `List` either way). Checked AFTER the
            // bracketed-parametrized branch above: a catalog row for a
            // parametrized name like `Blob[uint8]` deliberately omits its own
            // base (`Blob`) from `mro` (tracked instead via `roles`), so
            // matching it here first would drop `Blob` from the chain that
            // branch already builds correctly.
            if let Some(info) = crate::builtins::builtin_type_catalog::builtin_type_info(class_name)
            {
                return Some(info.mro.iter().map(|s| Symbol::intern(s)).collect());
            }
            // Not a registered class at all: the write side computes but has no
            // `ClassDef` to cache into, so the result is identical read-only.
            let mut stack = Vec::new();
            return Some(match self.compute_class_mro(class_name, &mut stack) {
                Ok(mro) => mro.iter().map(|s| Symbol::intern(s)).collect(),
                Err(_) => [Symbol::intern(class_name)].into(),
            });
        }
        let class_def = self.classes.get(class_name)?;
        if !class_def.mro.is_empty() {
            return Some(class_def.mro.clone());
        }
        None
    }

    /// Read-only MRO lookup: the cached `ClassDef::mro` when present, otherwise a
    /// single-element MRO `[class_name]`. Returns `None` when the class is not
    /// registered. Used by non-`&mut` helpers that must not trigger computation.
    pub(crate) fn class_mro_cached(&self, class_name: &str) -> Option<std::sync::Arc<[Symbol]>> {
        let class_def = self.classes.get(class_name)?;
        if !class_def.mro.is_empty() {
            return Some(class_def.mro.clone());
        }
        Some([Symbol::intern(class_name)].into())
    }

    /// Whether `class_name` (or any class in its MRO) defines `method_name`
    /// either as a user method or a native method. Pure registry MRO walk.
    pub(crate) fn class_has_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro.iter() {
            let has_user = self
                .user_method_overloads(cn.as_str(), method_name)
                .is_some();
            let has_native = self
                .classes
                .get(cn.as_str())
                .is_some_and(|class| class.native_methods.contains(method_name));
            if has_user || has_native {
                return true;
            }
        }
        false
    }

    /// Whether `class_name` (or any class in its MRO) defines `method_name` as a
    /// *user-declared* method (i.e. present in `.methods`), ignoring native
    /// builtin methods. Used by the coercion path to decide whether to run a
    /// user coercion method (e.g. `method Str {...}`) via `run_instance_method`
    /// versus routing a native builtin method (e.g. `IO::Path.Str`) through the
    /// native dispatcher. Pure registry MRO walk.
    pub(crate) fn class_has_user_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro.iter() {
            if self
                .user_method_overloads(cn.as_str(), method_name)
                .is_some()
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
        self.user_method_overloads(class_name, method_name)
    }

    /// Bound role type parameters for `class_name` (e.g. the `::T` -> value map
    /// of a `class C does R[Int]`). Owned clone.
    pub(crate) fn get_role_param_bindings(
        &self,
        class_name: &str,
    ) -> Option<HashMap<String, Value>> {
        self.class_role_param_bindings.get(class_name).cloned()
    }

    /// Whether a `proto sub`/`proto` named `name` is declared, visible from the
    /// `current_package` scope. Pure registry+scope read (no env, no re-entry):
    /// the single implementation shared by `Interpreter::has_proto` and the VM's
    /// native dispatch path.
    pub(crate) fn has_proto(&self, current_package: &str, name: &str) -> bool {
        if name.contains("::") {
            return self.proto_subs.contains(name);
        }
        let local = format!("{}::{}", current_package, name);
        if self.proto_subs.contains(&local) {
            return true;
        }
        self.proto_subs.contains(&format!("GLOBAL::{}", name))
    }

    /// Whether any `multi` candidate (any arity) exists for `name`, visible from
    /// any of the bare-name search `packages` (see
    /// `Interpreter::bare_name_packages`). Pure registry+scope read, shared by
    /// `Interpreter::has_multi_candidates` and the VM's native dispatch path.
    /// Takes the whole search list so the registry is scanned once, not once per
    /// enclosing package.
    pub(crate) fn has_multi_candidates(&self, packages: &[String], name: &str) -> bool {
        let prefixes: Vec<String> = packages
            .iter()
            .map(|pkg| format!("{}::{}/", pkg, name))
            .collect();
        self.functions.keys().any(|k| {
            let ks = k.resolve();
            prefixes.iter().any(|p| ks.starts_with(p))
        })
    }

    /// Whether a (non-multi) function `name` is declared, visible from the
    /// `current_package` scope: either fully qualified under the current package
    /// or as a bare global name. Pure registry+scope read, shared by
    /// `Interpreter::has_declared_function` and the VM's native dispatch path.
    pub(crate) fn has_declared_function(&self, current_package: &str, name: &str) -> bool {
        let fq = format!("{}::{}", current_package, name);
        self.functions.contains_key(&Symbol::intern(&fq))
            || self.functions.contains_key(&Symbol::intern(name))
    }

    /// Whether a `multi`-dispatched function `name` exists at any arity, visible
    /// from any of the bare-name search `packages`. Pure registry+scope read,
    /// shared by `Interpreter::has_multi_function` and the VM's native dispatch
    /// path. Takes the whole search list so the registry is scanned once.
    pub(crate) fn has_multi_function(&self, packages: &[String], name: &str) -> bool {
        let prefixes: Vec<String> = packages
            .iter()
            .map(|pkg| format!("{}::{}/", pkg, name))
            .collect();
        self.functions.keys().any(|k| {
            let ks = k.resolve();
            prefixes.iter().any(|p| ks.starts_with(p))
        })
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
    /// The roles a *built-in* role itself composes. `role_parents` only records
    /// what user code declared, so without this a `class F does Real` knew
    /// nothing of `Real does Numeric` and `F ~~ Numeric` answered False —
    /// which sent every `is-approx(Numeric, Numeric, ...)` in `Test.rakumod`
    /// past its own candidates and into the native provider's separate counter
    /// (`roast/S32-num/real-bridge.t`).
    pub(crate) fn builtin_role_parents(role_name: &str) -> &'static [&'static str] {
        match role_name {
            "Real" => &["Numeric"],
            "Setty" | "Baggy" => &["QuantHash", "Associative"],
            "Mixy" => &["Baggy"],
            _ => &[],
        }
    }

    /// Every role the named role composes, declared or built-in.
    pub(crate) fn role_parents_of(&self, role_name: &str) -> Vec<String> {
        let mut parents: Vec<String> = self
            .role_parents
            .get(role_name)
            .cloned()
            .unwrap_or_default();
        for builtin in Self::builtin_role_parents(role_name) {
            if !parents.iter().any(|p| p == builtin) {
                parents.push((*builtin).to_string());
            }
        }
        parents
    }

    pub(crate) fn composed_roles_seed(&self, mro: &[Symbol]) -> Vec<String> {
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
// The reentrancy-detecting guard machinery is shared with the IO handle table
// (PLAN.md ③) in [`crate::runtime::lock_reentry`]; see that module for the full
// rationale (lock-address keying, the allowed/forbidden matrix, debug-only
// instrumentation). The registry's guards are concrete type aliases over the
// generic guards, identified by the `"registry"` lock name in panic messages.

/// Read guard for the shared [`Registry`]. Wraps a [`ReentrantReadGuard`] over
/// the copy-on-write `Arc<Registry>` (see the `registry` field doc on
/// `Interpreter`) and derefs straight through to `Registry`, so the ~819
/// existing call sites (`self.registry().foo`) are unaffected by the added
/// `Arc` layer. See [`crate::runtime::lock_reentry`].
///
/// [`ReentrantReadGuard`]: crate::runtime::lock_reentry::ReentrantReadGuard
pub(crate) struct RegistryReadGuard<'a> {
    inner: crate::runtime::lock_reentry::ReentrantReadGuard<'a, Arc<Registry>>,
}

impl<'a> RegistryReadGuard<'a> {
    pub(crate) fn new(lock: &'a std::sync::RwLock<Arc<Registry>>, name: &'static str) -> Self {
        Self {
            inner: crate::runtime::lock_reentry::ReentrantReadGuard::new(lock, name),
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

/// Write guard for the shared [`Registry`]. Wraps a [`ReentrantWriteGuard`] over
/// the copy-on-write `Arc<Registry>`; the first mutable deref after a share
/// pays the one deep clone via `Arc::make_mut` (recorded as
/// `registry_cow_clones`), then behaves exactly like a plain `&mut Registry`.
/// See [`crate::runtime::lock_reentry`].
///
/// [`ReentrantWriteGuard`]: crate::runtime::lock_reentry::ReentrantWriteGuard
pub(crate) struct RegistryWriteGuard<'a> {
    inner: crate::runtime::lock_reentry::ReentrantWriteGuard<'a, Arc<Registry>>,
}

impl<'a> RegistryWriteGuard<'a> {
    pub(crate) fn new(lock: &'a std::sync::RwLock<Arc<Registry>>, name: &'static str) -> Self {
        Self {
            inner: crate::runtime::lock_reentry::ReentrantWriteGuard::new(lock, name),
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
        let arc: &mut Arc<Registry> = &mut self.inner;
        if Arc::strong_count(arc) > 1 {
            crate::vm::vm_stats::record_registry_cow_clone();
        }
        Arc::make_mut(arc)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builtin_method_catalog_is_registry_owned_and_ordered() {
        let mut registry = Registry::default();
        registry.seed_builtin_method_entries();

        assert_eq!(
            registry.builtin_method_names("Str"),
            crate::builtins::builtin_type_methods::introspected_type_method_names("Str")
        );
        assert!(matches!(
            registry.method_entries.get(&MethodEntryKey {
                owner: Symbol::intern("Str"),
                name: Symbol::intern("chars"),
            }),
            Some(MethodEntry {
                builtin: Some(_),
                ..
            })
        ));
    }

    #[test]
    fn builtin_method_catalog_resolves_type_aliases() {
        let mut registry = Registry::default();
        registry.seed_builtin_method_entries();

        assert_eq!(
            registry.builtin_method_names("FatRat"),
            registry.builtin_method_names("Rat")
        );
        assert_eq!(
            registry.builtin_method_names("Method"),
            registry.builtin_method_names("Sub")
        );
    }

    #[test]
    fn user_override_shares_the_builtin_method_entry() {
        let mut registry = Registry::default();
        registry.seed_builtin_method_entries();
        let seeded_generation = registry.method_generation;
        let method = MethodDef {
            lexical_package: "GLOBAL".to_string(),
            params: Vec::new(),
            param_defs: Vec::new(),
            body: std::sync::Arc::new(Vec::new()),
            is_rw: false,
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
            captured_env: None,
        };
        let mut class = ClassDef::default();
        class.methods.insert("chars".to_string(), vec![method]);
        registry.classes.insert("Str".to_string(), class);
        registry.sync_user_method_entries("Str");
        assert!(registry.method_generation > seeded_generation);

        let entry = registry
            .method_entries
            .get(&MethodEntryKey {
                owner: Symbol::intern("Str"),
                name: Symbol::intern("chars"),
            })
            .expect("Str.chars entry");
        assert!(entry.builtin.is_some());
        assert_eq!(entry.user_candidates.len(), 1);

        let override_generation = registry.method_generation;
        registry.classes.remove("Str");
        registry.sync_user_method_entries("Str");
        assert!(registry.method_generation > override_generation);
        let entry = registry
            .method_entries
            .get(&MethodEntryKey {
                owner: Symbol::intern("Str"),
                name: Symbol::intern("chars"),
            })
            .expect("built-in entry survives user removal");
        assert!(entry.builtin.is_some());
        assert!(entry.user_candidates.is_empty());
    }

    fn dummy_proto_def(owner: &str, name: &str) -> FunctionDef {
        FunctionDef {
            package: Symbol::intern(owner),
            name: Symbol::intern(name),
            params: Vec::new(),
            param_defs: Vec::new(),
            body: Vec::new(),
            is_test_assertion: false,
            is_rw: false,
            is_raw: false,
            is_method: true,
            empty_sig: false,
            is_stub: false,
            return_type: None,
            is_default: false,
            deprecated_message: None,
            source_file: None,
            decl_order: 0,
            compiled: None,
            body_fp_cache: std::sync::OnceLock::new(),
            body_facts_cache: std::sync::OnceLock::new(),
            rw_tail_expr: None,
        }
    }

    /// ADR-0019 E8c: `set_proto_method` populates the `MethodEntry.proto`
    /// column (the sole store since the E8c cutover) and flips the
    /// whole-program `has_proto_methods` fast-path flag.
    #[test]
    fn set_proto_method_populates_method_entries_and_the_fast_path_flag() {
        let mut registry = Registry::default();
        let seeded_generation = registry.method_generation;
        assert!(!registry.has_proto_methods);
        let def = dummy_proto_def("Foo", "bar");
        registry.set_proto_method("Foo", "bar", def.clone());
        assert!(registry.method_generation > seeded_generation);
        assert!(registry.has_proto_methods);

        assert_eq!(
            registry
                .method_entry_proto("Foo", "bar")
                .map(|f| f.body_fingerprint()),
            Some(def.body_fingerprint())
        );
    }

    /// `method_entry_proto` is a per-name probe (no MRO walk): a class that
    /// never declared its own proto method reports `None` even if an
    /// ancestor did — the MRO walk is the caller's job
    /// (`Interpreter::lookup_proto_method`).
    #[test]
    fn method_entry_proto_is_scoped_to_the_exact_owner() {
        let mut registry = Registry::default();
        registry.set_proto_method("Base", "greet", dummy_proto_def("Base", "greet"));
        assert!(registry.method_entry_proto("Base", "greet").is_some());
        assert!(registry.method_entry_proto("Child", "greet").is_none());
        assert!(registry.method_entry_proto("Base", "other").is_none());
    }
}
