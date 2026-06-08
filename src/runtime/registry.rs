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

use std::collections::HashMap;

use crate::value::EnumValue;

use super::SubsetDef;

/// Program declaration registry. See module docs.
///
/// Fields are migrated here group-by-group (PLAN.md PR-A). Fields are
/// `pub(crate)` so registry-internal runtime code can access them directly;
/// PR-B adds typed lookup methods for the VM to call.
#[derive(Debug, Clone, Default)]
pub(crate) struct Registry {
    /// `enum Name (...)` declarations: enum name -> [(variant name, value)].
    pub(crate) enum_types: HashMap<String, Vec<(String, EnumValue)>>,
    /// `subset Name of Base where { ... }` declarations.
    pub(crate) subsets: HashMap<String, SubsetDef>,
}
