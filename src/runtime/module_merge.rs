//! A module's GLOBAL merge is lexical to the scope that loaded it (ADR-11136).
//!
//! mutsu's type, package and `our` stores are process-global: a module load
//! publishes into them and keeps the entries for the life of the process,
//! because escaped instances, the module's own code and a no-op re-`use` all
//! need them. Rakudo instead merges a module's `GLOBALish` into the lexical
//! scope holding the `need`/`use`/`require`, so its declarations are visible
//! there and nowhere else -- not after the block, and not to whoever imports
//! the module that did the `use` (no transitivity).
//!
//! This module reconstructs that visibility on top of the global stores:
//!
//! - **provenance** -- [`Interpreter::record_module_provenance`] attributes
//!   each bare name a module's own body published to that module;
//! - **merges** -- [`Interpreter::merge_module_into_importer`] records a
//!   `need`/`use` either in the innermost import scope's env tier (a block of
//!   the importing compunit, so it ends with the block and a closure keeps it)
//!   or, at a compunit's top level, in `unit_merged_modules`;
//! - **the gate** -- [`Interpreter::bare_name_visible_here`] and
//!   [`Interpreter::module_merged_here`], consulted by bareword resolution, the
//!   indirect `::('...')` lookup, the `EVAL` undeclared-name check and #7797's
//!   qualified gate.

use super::*;
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;

impl Interpreter {
    /// Attribute the bare names a module's own body just published to it, and
    /// remember the module's compunit. A name already attributed keeps its
    /// owner: a nested load finishes first and claims its own declarations.
    // Cost: O(n), n = names published by the load.
    pub(crate) fn record_module_provenance(
        &mut self,
        module: &str,
        unit: Symbol,
        names: impl IntoIterator<Item = Symbol>,
    ) {
        let module_sym = Symbol::intern(module);
        crate::runtime::cow_table_mut(&mut self.module_units).insert(module_sym, unit);
        let mut names = names.into_iter().peekable();
        if names.peek().is_none() {
            return;
        }
        let table = crate::runtime::cow_table_mut(&mut self.module_name_providers);
        for name in names {
            table.entry(name).or_insert(module_sym);
        }
    }

    /// Attribute the package-less `our sub`s a module's own body published
    /// (their `GLOBAL::name` keys, by bare name) to it. Such a name resolves
    /// differently depending on where it is asked, so the name-keyed routine
    /// caches leave it alone (`is_unit_scoped_routine_name`).
    // Cost: O(n), n = routines published by the load.
    pub(crate) fn record_module_routine_provenance(
        &mut self,
        module: &str,
        names: impl IntoIterator<Item = Symbol>,
    ) {
        let mut names = names.into_iter().peekable();
        if names.peek().is_none() {
            return;
        }
        let module_sym = Symbol::intern(module);
        let table = crate::runtime::cow_table_mut(&mut self.module_routine_providers);
        for name in names {
            table.entry(name).or_insert(module_sym);
        }
        self.invalidate_fn_resolution();
    }

    /// Whether the registry routine under `key` resolves from the code running
    /// right now: always, unless it is a module's own package-less `our sub`
    /// (`GLOBAL::name`) and that module is not merged here (ADR-11136).
    // Cost: O(1) when no module published an `our sub`; otherwise one
    // provenance probe plus `module_merged_here` for a published one.
    pub(crate) fn module_routine_visible_here(&self, key: Symbol) -> bool {
        if self.module_routine_providers.is_empty() {
            return true;
        }
        let Some(name) =
            key.with_str(|k| Self::toplevel_global_routine_name(k).and_then(Symbol::lookup))
        else {
            return true;
        };
        match self.module_routine_providers.get(&name) {
            None => true,
            Some(&module) => self.module_merged_here(module),
        }
    }

    /// The package-scope names a package-less module declares at its own top
    /// level that are not types -- `constant`s, `package`/`module` blocks,
    /// enums and subsets -- and that nothing in scope already answers to, so
    /// attributing them to the module never hides a name the loading program
    /// declared itself. Read before the module body runs.
    // Cost: O(n) over the unit's top-level statements, plus one `env` probe
    // and one type lookup per declared name.
    pub(crate) fn module_scope_declared_names(&self, stmts: &[crate::ast::Stmt]) -> Vec<Symbol> {
        use crate::ast::Stmt;
        let mut names: Vec<Symbol> = Vec::new();
        for stmt in crate::ast::scope_members(stmts) {
            let name = match stmt {
                Stmt::VarDecl {
                    name,
                    custom_traits,
                    ..
                } if custom_traits.iter().any(|(t, _)| t == "__constant")
                    && name.starts_with(|c: char| c.is_alphabetic() || c == '_') =>
                {
                    Symbol::intern(name)
                }
                Stmt::Package {
                    name,
                    is_unit: false,
                    ..
                }
                | Stmt::EnumDecl { name, .. }
                | Stmt::SubsetDecl { name, .. } => *name,
                _ => continue,
            };
            if crate::qualified::is_qualified(name) {
                continue;
            }
            let known = name.with_str(|n| {
                self.env.contains_key(n) || self.has_type(n) || Self::is_builtin_type(n)
            });
            if !known {
                names.push(name);
            }
        }
        names
    }

    /// Merge `module`'s GLOBAL into the scope running its `need`/`use`, and
    /// grant `importer` the packages the module's load granted (#7797).
    ///
    /// When the innermost import scope belongs to `importer` -- a block or
    /// routine body of the importing compunit -- the merge is block-level: a
    /// `MetaNs::ModuleMerge` env key that the scope's pop removes again. The
    /// package grant then stays off `compunit_visible_packages`, which is
    /// unit-wide, and the qualified gate finds the merge through
    /// `package_granting_modules` instead. Otherwise the `need`/`use` is at
    /// the compunit's top level and both are recorded for the whole unit.
    // Cost: O(g), g = packages the module granted.
    pub(crate) fn merge_module_into_importer(
        &mut self,
        importer: Symbol,
        module: &str,
        granted: &HashSet<String>,
    ) {
        let module_sym = Symbol::intern(module);
        {
            let inverse = crate::runtime::cow_table_mut(&mut self.package_granting_modules);
            for package in granted {
                inverse
                    .entry(package.clone())
                    .or_default()
                    .insert(module_sym);
            }
        }
        // The BEGIN-time preload loads a block's module at the head of the
        // unit, inside a preload scope that keeps what it installs; the
        // in-position `need`/`use` replays the merge where it belongs.
        if self
            .import_scope_stack
            .last()
            .is_some_and(|scope| !scope.scope_classes)
        {
            return;
        }
        let block_level = self
            .import_scope_stack
            .last()
            .is_some_and(|scope| scope.unit == importer);
        if block_level {
            let key = MetaNs::ModuleMerge.key(module_sym);
            let previous = self.env.get_sym(key).cloned();
            if let Some(scope) = self.import_scope_stack.last_mut()
                && scope.imported_env_keys.insert(key)
                && let Some(previous) = previous
            {
                scope.shadowed_env_values.insert(key, previous);
            }
            self.env.insert_sym(key, Value::TRUE);
            return;
        }
        crate::runtime::cow_table_mut(&mut self.unit_merged_modules)
            .entry(importer)
            .or_default()
            .insert(module_sym);
        crate::runtime::cow_table_mut(&mut self.compunit_visible_packages)
            .entry(importer)
            .or_default()
            .extend(granted.iter().cloned());
    }

    /// Whether `module`'s GLOBAL is merged into the code running right now:
    /// a block-level merge is live in the env, the running compunit (or an
    /// `EVAL` parent of it) merged it at its top level, or the running
    /// compunit is the module itself.
    // Cost: O(d), d = depth of the EVAL parent chain (bounded at 64), twice.
    pub(crate) fn module_merged_here(&self, module: Symbol) -> bool {
        if self.env.contains_key_sym(MetaNs::ModuleMerge.key(module)) {
            return true;
        }
        let own_unit = self.module_units.get(&module).copied();
        // Two anchors, as `prelude_visible_here` explains: `?FILE` is right
        // while a module's mainline runs, `current_unit` names an EVAL unit
        // whose `?FILE` a nested frame has moved on from.
        for anchor in [self.executing_unit_sym_for_module_load(), self.current_unit] {
            let mut unit = Some(anchor);
            for _ in 0..64 {
                let Some(sym) = unit else { break };
                if Some(sym) == own_unit
                    || self
                        .unit_merged_modules
                        .get(&sym)
                        .is_some_and(|merged| merged.contains(&module))
                {
                    return true;
                }
                unit = crate::runtime::eval_unit_parent(sym);
            }
        }
        false
    }

    /// Whether the bare name `name` resolves from the code running right
    /// now: always, unless a module's own load published it and that module
    /// is not merged here (ADR-11136).
    // Cost: O(1) when no module published `name`; otherwise
    // `module_merged_here`.
    #[inline]
    pub(crate) fn bare_name_visible_here(&self, name: Symbol) -> bool {
        if self.module_name_providers.is_empty() {
            return true;
        }
        match self.module_name_providers.get(&name) {
            None => true,
            Some(&module) => self.module_merged_here(module),
        }
    }

    /// Whether a block-level merge live here granted the package `top`
    /// (`package_granting_modules`) -- the block-scoped half of #7797's
    /// qualified gate, whose unit-wide half is `compunit_visible_packages`.
    // Cost: O(m), m = modules that granted `top`.
    pub(crate) fn package_merged_here(&self, top: &str) -> bool {
        self.package_granting_modules
            .get(top)
            .is_some_and(|modules| {
                modules
                    .iter()
                    .any(|&module| self.env.contains_key_sym(MetaNs::ModuleMerge.key(module)))
            })
    }
}
