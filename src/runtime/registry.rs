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
    pub(crate) role_attribute_default_exprs: HashMap<(String, String), crate::ast::Expr>,
    /// Class-level (`my $.x` / `our $.x`) role attributes: (role-base, attr) ->
    /// optional default expr. These generate an accessor on the *type object*
    /// (not per-instance), so at composition they are copied into the consuming
    /// class's `class_level_attrs` rather than its per-instance attributes.
    pub(crate) role_class_level_attrs: HashMap<(String, String), Option<crate::ast::Expr>>,
    /// Per-attribute deferred `is default(...)` expression carried from a composed
    /// parametric role onto a consuming class: (class, attr) -> expr. Evaluated at
    /// construction with the class's role type-param bindings in scope.
    pub(crate) class_attribute_default_exprs: HashMap<(String, String), crate::ast::Expr>,
    /// Per-attribute declared type: (class, attr) -> type name.
    pub(crate) class_attribute_is_types: HashMap<(String, String), String>,
    /// Per-attribute `is Type` container trait declared on a *role* attribute:
    /// (role, attr) -> type name. Carried onto a consuming class's
    /// `class_attribute_is_types` at composition (`has @.a is G::A` in a
    /// parametric role) so the attribute's element type is enforced.
    pub(crate) role_attribute_is_types: HashMap<(String, String), String>,
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
    /// `proto method`/`proto submethod` bodies: (class_name, method_name) ->
    /// proto `FunctionDef`. When a multi method is dispatched and its class (or
    /// an ancestor in the MRO) has a proto body here, the body runs first and
    /// its `{*}` dispatches to the matching multi candidate.
    pub(crate) proto_methods: HashMap<(String, String), FunctionDef>,
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
        // Raku's default class hierarchy.
        let parents = if explicit_parents.is_empty() && self.classes.contains_key(class_name) {
            vec!["Any".to_string()]
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
    pub(crate) fn class_mro(&mut self, class_name: &str) -> std::sync::Arc<[Symbol]> {
        // Built-in type hierarchies for types that are not user-defined classes
        if !self.classes.contains_key(class_name) {
            let builtin_mro: Option<&[&str]> = match class_name {
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
                "IO::Spec::QNX" => {
                    Some(&["IO::Spec::QNX", "IO::Spec::Unix", "IO::Spec", "Any", "Mu"])
                }
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
            };
            if let Some(mro) = builtin_mro {
                return mro.iter().map(|s| Symbol::intern(s)).collect();
            }
        }
        if !self.classes.contains_key(class_name)
            && let Some((base, _)) = class_name.split_once('[')
            && class_name.ends_with(']')
            && self.classes.contains_key(base)
        {
            let mut mro = vec![Symbol::intern(class_name)];
            mro.extend(self.class_mro(base).iter().copied());
            return mro.into();
        }
        if let Some(class_def) = self.classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return class_def.mro.clone();
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
            if let Some(class_def) = self.classes.get(cn.as_str())
                && (class_def.methods.contains_key(method_name)
                    || class_def.native_methods.contains(method_name))
            {
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
            if let Some(class_def) = self.classes.get(cn.as_str())
                && class_def.methods.contains_key(method_name)
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
    /// the `current_package` scope. Pure registry+scope read, shared by
    /// `Interpreter::has_multi_candidates` and the VM's native dispatch path.
    pub(crate) fn has_multi_candidates(&self, current_package: &str, name: &str) -> bool {
        let prefixes = [
            format!("{}::{}/", current_package, name),
            format!("GLOBAL::{}/", name),
        ];
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
    /// from the `current_package` scope. Pure registry+scope read, shared by
    /// `Interpreter::has_multi_function` and the VM's native dispatch path.
    pub(crate) fn has_multi_function(&self, current_package: &str, name: &str) -> bool {
        let fq_slash = format!("{}::{}/", current_package, name);
        self.functions
            .keys()
            .any(|k| k.resolve().starts_with(&fq_slash))
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

/// Read guard for the shared [`Registry`]; a [`ReentrantReadGuard`] keyed by the
/// `"registry"` lock name. See [`crate::runtime::lock_reentry`].
///
/// [`ReentrantReadGuard`]: crate::runtime::lock_reentry::ReentrantReadGuard
pub(crate) type RegistryReadGuard<'a> =
    crate::runtime::lock_reentry::ReentrantReadGuard<'a, Registry>;

/// Write guard for the shared [`Registry`]; a [`ReentrantWriteGuard`] keyed by
/// the `"registry"` lock name. See [`crate::runtime::lock_reentry`].
///
/// [`ReentrantWriteGuard`]: crate::runtime::lock_reentry::ReentrantWriteGuard
pub(crate) type RegistryWriteGuard<'a> =
    crate::runtime::lock_reentry::ReentrantWriteGuard<'a, Registry>;
