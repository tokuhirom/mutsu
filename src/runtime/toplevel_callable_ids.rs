//! Registration clone ids of a module's top-level routines, kept off the
//! per-frame `Env` (ADR-0084 §2 group 1, #7817).
//!
//! Every `RegisterSub` execution mints a fresh *registration clone id* for the
//! routine `Pkg::name` and records it under the marker key
//! `__mutsu_callable_id::Pkg::name`. The id is what a `state` variable's scope,
//! a non-local `return`'s target and a `wrap` chain's redefinition check are
//! keyed on, and it is deliberately *lexical*: a sub declared inside another
//! routine is re-registered on every call of its enclosing routine, gets a new
//! id each time (so its `state` re-initializes per clone), and a block scope
//! restores the enclosing binding's marker on exit.
//!
//! None of that applies to a routine declared at the top level of a loaded
//! module. A module's mainline runs exactly once per process (the module cache
//! makes every later `use` a no-op), so the id such a registration mints is
//! fixed for the life of the program — and because the module body runs in the
//! IMPORTER's env, storing it there left one marker per module routine in
//! every frame env of the program that loaded it: 197 of the 824 entries of a
//! frame env after `use Cro::HTTP2::RequestParser`, each one copied by every
//! copy-on-write deep copy of that env.
//!
//! So such a registration goes to [`Interpreter::toplevel_callable_ids`]
//! instead, a per-interpreter table that frames neither clone nor capture, and
//! every reader asks [`Interpreter::registration_callable_id`], which consults
//! the env first (a lexical registration shadows) and this table second.
//!
//! What counts as "the top level of a module's mainline" is decided by depth,
//! not by text: [`Interpreter::run_module_mainline`] records the routine stack
//! depth and the block-scope depth the mainline starts at, and a registration
//! made at exactly those depths is one the mainline makes directly. Anything
//! nested — a sub declared in a routine the mainline calls, or in a block or a
//! loop body that opens a scope — is deeper and keeps its env marker.
//!
//! A package-less (`GLOBAL::`) routine's key can be shared with one the
//! importing program declares itself. Reading the env first and writing the
//! env whenever it already holds the key keeps the outcome of such a clash
//! what it was when both lived in the env — the later registration wins —
//! while the common case (no clash) still leaves the env alone.

use super::*;
use crate::meta_ns::MetaNs;

/// The depths a module's mainline starts at (see the module comment).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct ModuleToplevelDepth {
    routines: usize,
    block_scopes: usize,
}

impl Interpreter {
    /// The depths the executing code sits at, comparable with a recorded
    /// [`ModuleToplevelDepth`].
    // Cost: O(1).
    fn current_toplevel_depth(&self) -> ModuleToplevelDepth {
        ModuleToplevelDepth {
            routines: self.routine_stack().len(),
            block_scopes: self.block_declared_vars.all_frames().len(),
        }
    }

    /// Run a loaded module's mainline, marking its top level so the routines it
    /// registers there record their clone ids in
    /// [`Self::toplevel_callable_ids`]. Saved and restored, so a nested module
    /// load marks its own mainline and gives the outer one back.
    // Cost: O(1) beyond `body`.
    pub(crate) fn run_module_mainline<T>(
        &mut self,
        body: impl FnOnce(&mut Self) -> Result<T, RuntimeError>,
    ) -> Result<T, RuntimeError> {
        let depth = self.current_toplevel_depth();
        let saved = self.module_toplevel_depth.replace(depth);
        let result = body(self);
        self.module_toplevel_depth = saved;
        result
    }

    /// Record a fresh registration clone id for the routine `package::name`.
    ///
    /// The one writer of the `__mutsu_callable_id::` marker: a registration
    /// made directly by a module's mainline goes to
    /// [`Self::toplevel_callable_ids`]; every other one stays a lexical env
    /// entry.
    // Cost: O(|package| + |name|) for the memoized key lookup, plus one
    // amortized O(1) insert (a copy-on-write table clone, O(t), only while a
    // spawned thread still shares the table, t = recorded routines).
    pub(crate) fn note_registration_callable_id(&mut self, package: &str, name: &str) {
        let key = MetaNs::CallableId.key_pair_for_strs(package, name);
        let id = crate::value::next_instance_id() as i64;
        if self
            .module_toplevel_depth
                .is_some_and(|depth| depth == self.current_toplevel_depth())
            // An env marker for the same routine (an earlier lexical
            // registration still in scope) would shadow the table; overwrite
            // it in place rather than leave a stale id in front.
            && !self.env.contains_key_sym(key)
        {
            crate::runtime::cow_table_mut(&mut self.toplevel_callable_ids).insert(key, id);
            return;
        }
        self.env.insert_sym_noting(key, Value::int(id));
    }

    /// The registration clone id recorded under the marker `key` (built with
    /// [`MetaNs::CallableId`]): the visible env marker, else the module
    /// top-level table. `None` when neither holds a non-zero id.
    // Cost: O(1), one env probe and at most one table probe.
    pub(crate) fn registration_callable_id(&self, key: Symbol) -> Option<i64> {
        match self.env().get_sym(key) {
            Some(v) => v.as_int(),
            None => self.toplevel_callable_ids.get(&key).copied(),
        }
        .filter(|id| *id != 0)
    }
}
