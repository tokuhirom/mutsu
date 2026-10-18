//! What a module merge contributes to the stash views (#13802, ADR-8751).
//!
//! Rakudo installs a `use`d module's packages lexically, in the importing
//! scope's pad (`MY::`), never in `GLOBAL`. mutsu keeps a loaded module's
//! packages in the process-wide type and package tables, so the stash views
//! reconstruct that rule here: `GLOBAL::` (and `OUR::` at the root) leaves out
//! what only a module merge brought, and `MY::` of the scope whose `use` merged
//! a module lists the module's top-level packages.
use super::*;
use crate::meta_ns::MetaNs;
use crate::value::ValueMap;
use std::collections::HashSet;

impl Interpreter {
    /// Whether the type or package `name` is a module's own declaration, as
    /// far as the module-load provenance tables know (ADR-11136, #7797).
    // Cost: O(1), three set probes.
    fn is_module_declared_package(&self, name: &str) -> bool {
        self.module
            .module_visibility
            .module_declared_types
            .contains(name)
            || self.module.package_declaring_units.contains_key(name)
            || Symbol::lookup(name).is_some_and(|sym| {
                self.module
                    .module_visibility
                    .module_name_providers
                    .contains_key(&sym)
            })
    }

    /// The top-level names (the first `::` segment) of every package or type
    /// some loaded module declared.
    // Cost: O(t), t = module-declared types + attributed names + declaring-unit
    // entries.
    fn module_merge_tops(&self) -> HashSet<Symbol> {
        let visibility = &self.module.module_visibility;
        visibility
            .module_declared_types
            .iter()
            .map(|name| Symbol::intern(name))
            .chain(visibility.module_name_providers.keys().copied())
            .chain(
                self.module
                    .package_declaring_units
                    .keys()
                    .map(|name| Symbol::intern(name)),
            )
            .filter(|sym| !sym.with_str(|s| s.starts_with(['$', '@', '%', '&'])))
            .map(|sym| crate::qualified::segments(sym)[0])
            .collect()
    }

    /// The top-level names the program (not a module load) declared a type or
    /// package under. Such a name is the program's own `GLOBAL` member even
    /// when a module also declared something under it.
    // Cost: O(c), c = user-declared classes, roles and package kinds.
    fn program_declared_tops(&self) -> HashSet<Symbol> {
        let registry = self.registry();
        self.types
            .user_declared_classes
            .iter()
            .chain(registry.user_declared_roles.iter())
            .chain(registry.package_kinds.keys())
            .filter(|name| !self.is_module_declared_package(name))
            .map(|name| crate::qualified::segments(Symbol::intern(name))[0])
            .collect()
    }

    /// Whether `GLOBAL`'s routine entry `&name` is only there because of an
    /// import or a module's package-less `our sub`, both of which rakudo keeps
    /// out of `GLOBAL`.
    // Cost: O(1), two set probes.
    pub(super) fn global_routine_from_module_merge(&self, name: &str) -> bool {
        let qualified =
            crate::qualified::qualified(crate::symbol::wk::global_package(), Symbol::intern(name));
        self.module.imported_routine_aliases.contains(&qualified)
            || Symbol::lookup(name).is_some_and(|sym| {
                self.module
                    .module_visibility
                    .module_routine_providers
                    .contains_key(&sym)
            })
    }

    /// The compunits of the loaded modules.
    // Cost: O(m), m = loaded modules.
    pub(super) fn loaded_module_units(&self) -> HashSet<Symbol> {
        self.module
            .module_visibility
            .module_units
            .values()
            .copied()
            .collect()
    }

    /// Whether the routine `def` was declared in one of the compunits `units`.
    // Cost: O(f), f = bytes of its source file name.
    pub(super) fn routine_declared_in_units(
        &self,
        def: &crate::ast::FunctionDef,
        units: &HashSet<Symbol>,
    ) -> bool {
        def.source_file
            .as_deref()
            .is_some_and(|file| units.contains(&self.unit_of_source(Some(file))))
    }

    /// Drop from a `GLOBAL` stash build the entries a module merge put there:
    /// a module's top-level packages the program did not declare under the
    /// same name, imported routines, and an `EXPORT` package only modules
    /// exported into.
    // Cost: O(s + t + c), s = stash entries, t and c as `module_merge_tops`
    // and `program_declared_tops`.
    pub(super) fn drop_module_merges_from_global(&self, symbols: &mut ValueMap) {
        let tops = self.module_merge_tops();
        let program = if tops.is_empty() {
            HashSet::new()
        } else {
            self.program_declared_tops()
        };
        let keep_export = symbols.contains_key("EXPORT") && self.program_exports_from_global();
        symbols.retain(|key, _| {
            if key == "EXPORT" {
                return keep_export;
            }
            if let Some(routine) = key.strip_prefix('&') {
                return !self.global_routine_from_module_merge(routine);
            }
            if key.starts_with(['$', '@', '%']) {
                return true;
            }
            let Some(sym) = Symbol::lookup(key) else {
                return true;
            };
            !tops.contains(&sym) || program.contains(&sym)
        });
    }

    /// Whether a root-package export (`GLOBAL::EXPORT`) is the program's own
    /// rather than only a package-less module's (`sub f is export` in a file
    /// with no `unit` declarator).
    // Cost: O(e + r), e = root-package exports, r = registered routines.
    fn program_exports_from_global(&self) -> bool {
        let global = crate::symbol::wk::global_package();
        if self
            .module
            .exported_vars
            .get("GLOBAL")
            .is_some_and(|vars| !vars.is_empty())
        {
            return true;
        }
        let Some(subs) = self.module.exported_subs.get("GLOBAL") else {
            return false;
        };
        let units = self.loaded_module_units();
        let registry = self.registry();
        subs.keys().any(|name| {
            let name = Symbol::intern(name);
            !registry.functions.values().any(|def| {
                def.package == global
                    && def.name == name
                    && self.routine_declared_in_units(def, &units)
            })
        })
    }

    /// The top-level packages merging `module` installs in the importing
    /// scope: the first `::` segment of each type the module declared and of
    /// each bare name its load published.
    // Cost: O(d + n), d = types the module declared, n = attributed names.
    fn module_merge_packages(&self, module: Symbol) -> Vec<Symbol> {
        let mut tops: Vec<Symbol> = Vec::new();
        let mut push = |sym: Symbol| {
            let top = crate::qualified::segments(sym)[0];
            if !tops.contains(&top) {
                tops.push(top);
            }
        };
        if let Some(owned) = module.with_str(|m| self.module.module_owned_types.get(m)) {
            for name in &owned.declared {
                push(Symbol::intern(name));
            }
        }
        for (name, provider) in self.module.module_visibility.module_name_providers.iter() {
            if *provider == module && !name.with_str(|s| s.starts_with(['$', '@', '%', '&'])) {
                push(*name);
            }
        }
        tops
    }

    /// Add to a `MY::` view the top-level packages of each module in
    /// `modules`, as a bareword read of the name sees them.
    // Cost: O(m * (d + n)), m = modules, d and n as `module_merge_packages`.
    pub(crate) fn add_merged_module_packages(
        &self,
        modules: impl IntoIterator<Item = Symbol>,
        entries: &mut ValueMap,
    ) {
        for module in modules {
            for top in self.module_merge_packages(module) {
                let name = top.resolve();
                if crate::runtime::term_names::term_spelling(&name).is_some() {
                    continue;
                }
                entries.entry(name).or_insert_with(|| Value::package(top));
            }
        }
    }

    /// The module a `MetaNs::ModuleMerge` env key names, if `key` is one.
    // Cost: O(m), m = bytes of the key.
    pub(crate) fn module_merge_key_module(key: Symbol) -> Option<Symbol> {
        key.with_str(|s| {
            s.strip_prefix(MetaNs::ModuleMerge.prefix())
                .map(Symbol::intern)
        })
    }

    /// The modules merged at the running compunit's top level, plus those a
    /// block-level merge live in the env merged: what the unit's root pad (or
    /// a `LEXICAL::` view) sees.
    // Cost: O(v + m), v = env entries, m = modules the unit merged.
    pub(crate) fn modules_merged_in_view(&self) -> Vec<Symbol> {
        let unit = self.executing_unit_sym_for_module_load();
        let mut modules: Vec<Symbol> = self
            .module
            .module_visibility
            .unit_merged_modules
            .get(&unit)
            .map(|set| set.iter().copied().collect())
            .unwrap_or_default();
        for (key, _) in self.env().iter() {
            if let Some(module) = Self::module_merge_key_module(*key)
                && !modules.contains(&module)
            {
                modules.push(module);
            }
        }
        modules
    }
}
