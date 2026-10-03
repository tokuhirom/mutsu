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

/// The state that decides which module declarations resolve where (#7797,
/// ADR-11136): the package grants a `need`/`use` earns its importer, and which
/// module published each bare name, which modules each compunit merged at its
/// top level, and the reverse index from a granted package to its modules.
/// Every table is copy-on-write (`cow_table_mut`), so a thread clone shares
/// them until one side writes.
#[derive(Default, Clone)]
pub(crate) struct ModuleVisibility {
    /// #7797: for a compunit that successfully `use`d/`need`d/`require`d a
    /// module, the top-level package names (same first-segment granularity
    /// as `package_declaring_units`) it is therefore entitled to reference
    /// package-qualified — e.g. `use OuterConst;` grants `"OuterConst"`, but
    /// NOT `"InnerConst"` even though `OuterConst.rakumod` itself `use`d
    /// `InnerConst`: rakudo installs a `use`d package into the *importing*
    /// compunit's `MY::` only, so visibility does not transit through a
    /// second `use`. `Interpreter::qualified_name_visible_here` walks the
    /// `EVAL` parent chain (`eval_unit_parent`) from the executing unit
    /// consulting this table, exactly as `prelude_visible_here` does for
    /// prelude splices.
    pub(crate) compunit_visible_packages: std::sync::Arc<HashMap<Symbol, HashSet<String>>>,
    /// The package names one module's FIRST load granted to its importer
    /// (`compunit_visible_packages`), keyed by the module name — its own
    /// name, the `unit module`/`unit class` package it declares, every type
    /// it registered under that prefix, and each of their top-level
    /// `::`-segments.
    ///
    /// A re-`use` of an already-loaded module never re-runs that load, so it
    /// cannot recompute the set; without replaying it, the second importer
    /// only ever learns the module's own name. That is invisible while the
    /// declared package matches the file name, and fatal when it does not:
    /// `Acme/Cow.rakumod` says `unit module Cow;`, so a script whose first
    /// load came from an `EVAL` (`Test`'s `use-ok`) reached `Cow::cow` only
    /// through a grant its own `use Acme::Cow;` never made.
    pub(crate) module_granted_packages: std::sync::Arc<HashMap<String, HashSet<String>>>,
    /// ADR-11136: the module whose load published each bare package-scope
    /// name (a class, role, enum, subset, package, `our` sub or term) its own
    /// body declared. A name the program or a module had published before
    /// is never attributed, so only a module's own GLOBAL merge is gated.
    pub(crate) module_name_providers: std::sync::Arc<HashMap<Symbol, Symbol>>,
    /// ADR-11136: the module whose load published each package-less
    /// `our sub` (`GLOBAL::name`), by bare name.
    pub(crate) module_routine_providers: std::sync::Arc<HashMap<Symbol, Symbol>>,
    /// ADR-11136: the compunit each loaded module's source is.
    pub(crate) module_units: std::sync::Arc<HashMap<Symbol, Symbol>>,
    /// ADR-11136: the modules a compunit merged at its top level. A
    /// block-level merge lives in the block's env tier instead
    /// (`MetaNs::ModuleMerge`).
    pub(crate) unit_merged_modules: std::sync::Arc<HashMap<Symbol, HashSet<Symbol>>>,
    /// ADR-11136: the modules whose load granted each package
    /// (`module_granted_packages` inverted), so the #7797 qualified gate
    /// can honour a block-level merge.
    pub(crate) package_granting_modules: std::sync::Arc<HashMap<String, HashSet<Symbol>>>,
}

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
        crate::runtime::cow_table_mut(&mut self.module_visibility.module_units)
            .insert(module_sym, unit);
        let mut names = names.into_iter().peekable();
        if names.peek().is_none() {
            return;
        }
        let table =
            crate::runtime::cow_table_mut(&mut self.module_visibility.module_name_providers);
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
        let table =
            crate::runtime::cow_table_mut(&mut self.module_visibility.module_routine_providers);
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
        if self.module_visibility.module_routine_providers.is_empty() {
            return true;
        }
        let Some(name) =
            key.with_str(|k| Self::toplevel_global_routine_name(k).and_then(Symbol::lookup))
        else {
            return true;
        };
        match self.module_visibility.module_routine_providers.get(&name) {
            None => true,
            // An imported alias of the routine (`our sub f is export` is
            // imported under the very `GLOBAL::f` key) resolves wherever the
            // import is in scope.
            Some(&module) => {
                self.module_merged_here(module)
                    || name.with_str(|n| self.imported_routine_alias_in_scope(n))
            }
        }
    }

    /// A declaration of `name` by some other code than the module it is
    /// attributed to -- the program's own `package Foo { }` after a preloaded
    /// module published a `class Foo` -- makes the name that code's too, so it
    /// is no longer one module's private merge.
    // Cost: O(1) when `name` is unattributed or declared by its own module;
    // otherwise a copy-on-write removal from the provenance table.
    pub(crate) fn release_foreign_provenance(&mut self, name: &str) {
        if self.module_visibility.module_name_providers.is_empty() {
            return;
        }
        let Some(sym) = Symbol::lookup(name) else {
            return;
        };
        let Some(&provider) = self.module_visibility.module_name_providers.get(&sym) else {
            return;
        };
        let declaring_module = self.module_load_stack.last().map(|m| Symbol::intern(m));
        if declaring_module != Some(provider) {
            crate::runtime::cow_table_mut(&mut self.module_visibility.module_name_providers)
                .remove(&sym);
        }
    }

    /// The package-scope names a package-less module declares at its own top
    /// level -- classes and grammars (not `my class`), roles, `package`/`module`
    /// blocks, enums, subsets and `our` constants -- that nothing in scope
    /// already answers to, so
    /// attributing them to the module never hides a name the loading program
    /// declared itself. Read before the module body runs.
    // Cost: O(n) over the unit's top-level statements, plus one `env` probe
    // and one type lookup per declared name.
    pub(crate) fn module_scope_declared_names(&self, stmts: &[crate::ast::Stmt]) -> Vec<Symbol> {
        use crate::ast::Stmt;
        let mut names: Vec<Symbol> = Vec::new();
        for stmt in crate::ast::scope_members(stmts) {
            let name = match stmt {
                // Only an `our` constant (the `constant` default) is part of the
                // merge; a `my constant` is lexical to the module.
                Stmt::VarDecl {
                    name,
                    custom_traits,
                    is_our: true,
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
                | Stmt::ClassDecl {
                    name,
                    is_lexical: false,
                    is_unit: false,
                    ..
                }
                | Stmt::RoleDecl { name, .. }
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

    /// Record the modules `unit` merges at its top level before its body runs.
    ///
    /// A top-level `need`/`use` is a compile-time merge in Rakudo: the whole
    /// unit sees the module, including the CHECK-time checks mutsu runs ahead
    /// of the in-position statement, and including code that runs before the
    /// statement because the module was already loaded elsewhere. A
    /// conditional `use Foo:if(...)` is left to its in-position merge.
    // Cost: O(n), n = the unit's top-level statements.
    pub(crate) fn premerge_top_level_uses(&mut self, unit: Symbol, stmts: &[crate::ast::Stmt]) {
        use crate::ast::Stmt;
        let modules: Vec<Symbol> = crate::ast::scope_members(stmts)
            .through_unit_package()
            .filter_map(|stmt| match stmt {
                Stmt::Use {
                    module,
                    condition: None,
                    ..
                }
                | Stmt::Need { module } => Some(Symbol::intern(module)),
                _ => None,
            })
            .collect();
        if modules.is_empty() {
            return;
        }
        crate::runtime::cow_table_mut(&mut self.module_visibility.unit_merged_modules)
            .entry(unit)
            .or_default()
            .extend(modules);
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
            let inverse =
                crate::runtime::cow_table_mut(&mut self.module_visibility.package_granting_modules);
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
        crate::runtime::cow_table_mut(&mut self.module_visibility.unit_merged_modules)
            .entry(importer)
            .or_default()
            .insert(module_sym);
        crate::runtime::cow_table_mut(&mut self.module_visibility.compunit_visible_packages)
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
        let own_unit = self.module_visibility.module_units.get(&module).copied();
        // Two anchors, as `prelude_visible_here` explains: `?FILE` is right
        // while a module's mainline runs, `current_unit` names an EVAL unit
        // whose `?FILE` a nested frame has moved on from.
        for anchor in [self.executing_unit_sym_for_module_load(), self.current_unit] {
            let mut unit = Some(anchor);
            for _ in 0..64 {
                let Some(sym) = unit else { break };
                if Some(sym) == own_unit
                    || self
                        .module_visibility
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
        if self.module_visibility.module_name_providers.is_empty() {
            return true;
        }
        match self.module_visibility.module_name_providers.get(&name) {
            None => true,
            // A name imported explicitly into a live scope (an `is export`ed
            // type, `require M <Name>`) is visible however the module was
            // reached.
            Some(&module) => {
                self.module_merged_here(module) || self.imported_env_aliases.contains_key(&name)
            }
        }
    }

    /// Whether a block-level merge live here granted the package `top`
    /// (`package_granting_modules`) -- the block-scoped half of #7797's
    /// qualified gate, whose unit-wide half is `compunit_visible_packages`.
    // Cost: O(m), m = modules that granted `top`.
    pub(crate) fn package_merged_here(&self, top: &str) -> bool {
        self.module_visibility
            .package_granting_modules
            .get(top)
            .is_some_and(|modules| {
                modules
                    .iter()
                    .any(|&module| self.env.contains_key_sym(MetaNs::ModuleMerge.key(module)))
            })
    }
}
