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

use crate::value::{EnumValue, Value};

use super::{ClassDef, RoleCandidateDef, RoleDef, SubsetDef};

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
}
