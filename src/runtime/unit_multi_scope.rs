//! Compunit scoping for a loaded module's package-less `multi`/`proto`
//! families (#11004).
//!
//! A `multi sub` or `proto sub` declared at the top level of a file with no
//! package of its own is, in Raku, a lexical of that compilation unit, exactly
//! like a plain `sub`. Its `is export` trait only puts it in the module's
//! `EXPORT` stash; it reaches another compunit only when that compunit
//! *imports* it (`use`). mutsu registers such candidates under the shared
//! `GLOBAL::name/<sig>` keys, and multi candidates are additive across
//! compunits by design (several modules contribute candidates to one
//! `multi trait_mod:<is>`), so neither the private-routine seclusion
//! (`runtime/unit_private_routines.rs`, one routine per name) nor the
//! top-level hide/restore can take them. Left alone, a
//! `CompUnit::Repository.need`, a `need` statement, or an unexported
//! `multi helper` made the whole family callable from every later scope.
//!
//! The candidates therefore stay where they are, and their *visibility* is
//! scoped instead, with the per-candidate gate imported operators already use
//! (`runtime/operator_scope.rs`, #9944): after the load, each package-less
//! family the module body declared gets a record keyed by its declaring unit
//! with no importers yet, so only the declaring unit sees it. An import
//! (`import_module`) then adds the importing unit, which is what `use` means.
//! Every candidate walk that filters with
//! [`Interpreter::retain_visible_operator_candidates`], and the name probes
//! (`has_proto`, `has_multi_candidates`, ...), then answer per executing unit.

use super::dispatch_resolve::function_key_base_name;
use super::*;

impl Interpreter {
    /// Scope the package-less `multi`/`proto` families the compunit at
    /// `source_path` declared to that compunit: from now on only its own
    /// code, and units that import the family, see them.
    ///
    /// Only families the unit itself declared are recorded (each candidate's
    /// `source_file` names it); an alias the body imported from another module
    /// belongs to that module's record. `MAIN` keeps its own handling
    /// (`remove_leaked_main_routines` / `promote_exported_main_to_global`), a
    /// prelude splice its own gate (`prelude_visible_here`), and an
    /// `our multi`/`our proto` is a real `GLOBAL` stash entry.
    // Cost: O(r + p), r = registered functions, p = registered protos; once per
    // module load.
    pub(crate) fn scope_unit_multi_families(&mut self, source_path: &str) {
        let decl_unit = self.unit_of_source(Some(source_path));
        self.scope_multi_families_of_unit(decl_unit, |_| true);
    }

    /// Scope the families the importing compunit `importer` has declared so
    /// far, right before it loads a module (#11310).
    ///
    /// A `multi` is hoisted to the start of its block, so a compunit's
    /// candidates are registered before the `use` statements in its body run.
    /// [`Self::scope_unit_multi_families`] only runs once a module's load
    /// finishes, so without this the loading module's candidates were visible
    /// to every module it loads in turn: a `multi trait_mod:<is>(Routine $r,
    /// :$symbol!)` in one module captured the `is array_type(...)` and
    /// `is export` traits of a module it merely `use`d (upstream
    /// `NativeCall.rakumod` over `NativeCall::Types`). In Raku the candidate is
    /// lexical to its compunit, and the loaded module never sees it.
    ///
    /// A module importer gets every family scoped, which is what its own load
    /// would do at its end anyway. The main script's families are left
    /// unscoped by default, since a scoped name bypasses the unit-blind
    /// dispatch caches (see [`Self::scope_main_family_if_contested`]); a
    /// valid module cannot call a script routine it never declared, so the
    /// leak only matters for a name the module uses *without* declaring it.
    /// Its traits are that name: a module applies `trait_mod:<is>` and kin to
    /// its own declarations while it loads, and the core candidates are what
    /// it means. So the script's `trait_mod:<...>` families are scoped here.
    // Cost: O(r + p), r = registered functions, p = registered protos; once per
    // module load.
    pub(crate) fn scope_importer_families_for_nested_load(&mut self, importer: Symbol) {
        if importer == crate::runtime::main_unit() {
            self.scope_multi_families_of_unit(importer, |name| name.starts_with("trait_mod:<"));
        } else {
            self.scope_multi_families_of_unit(importer, |_| true);
        }
    }

    /// The shared body of [`Self::scope_unit_multi_families`] and
    /// [`Self::scope_importer_families_for_nested_load`]: scope the
    /// package-less families `decl_unit` declared whose name passes `wanted`.
    // Cost: O(r + p), r = registered functions, p = registered protos.
    fn scope_multi_families_of_unit(&mut self, decl_unit: Symbol, wanted: impl Fn(&str) -> bool) {
        let mut names: HashSet<Symbol> = HashSet::new();
        // Names this unit declared inside a package (`module M { ... }`, a
        // namespaced `unit module`). Their `GLOBAL::` entries are export
        // aliases of a package routine (`register_proto_decl_as_global`), not
        // package-less declarations, and keep their existing import handling.
        let mut packaged: HashSet<Symbol> = HashSet::new();
        {
            let registry = self.registry();
            let routines = registry
                .functions
                .iter()
                .map(|(key, def)| (key, def, true))
                .chain(
                    registry
                        .proto_functions
                        .iter()
                        .map(|(key, def)| (key, def, false)),
                );
            for (key, def, is_candidate_map) in routines {
                if self.unit_of_source(def.source_file.as_deref()) != decl_unit {
                    continue;
                }
                let ks = key.as_str();
                let base = function_key_base_name(ks);
                let Some(tail) = ks.strip_prefix("GLOBAL::") else {
                    // The `EXPORT::<tag>::` stash aliases of a package-less
                    // export are not a package declaration.
                    if !ks.starts_with("EXPORT::") && !ks.contains("::EXPORT::") {
                        packaged.insert(Symbol::intern(base));
                    }
                    continue;
                };
                let package_less = if is_candidate_map {
                    // A package-less candidate key is `GLOBAL::<base>/<sig>`.
                    tail.strip_prefix(base)
                        .is_some_and(|rest| rest.starts_with('/'))
                        && crate::qualified::is_global_package(def.package)
                } else {
                    Self::toplevel_global_routine_name(ks).is_some()
                };
                if package_less {
                    names.insert(Symbol::intern(base));
                }
            }
        }
        names.retain(|name| !packaged.contains(name));
        // The main script's own package-less candidates of a name this module
        // now scopes, declared before the load. See
        // [`Self::scope_main_family_if_contested`].
        let main = crate::runtime::main_unit();
        let mut contested_main: HashSet<Symbol> = HashSet::new();
        if decl_unit != main {
            let registry = self.registry();
            for (key, def) in registry.functions.iter() {
                let ks = key.as_str();
                let base = function_key_base_name(ks);
                if ks
                    .strip_prefix("GLOBAL::")
                    .and_then(|tail| tail.strip_prefix(base))
                    .is_some_and(|rest| rest.starts_with('/'))
                    && crate::qualified::is_global_package(def.package)
                    && self.unit_of_source(def.source_file.as_deref()) == main
                {
                    contested_main.insert(Symbol::intern(base));
                }
            }
        }
        names.retain(|name| {
            let name_str = name.as_str();
            wanted(name_str)
                && name_str != "MAIN"
                && name_str != "EXPORT"
                && !self.module.prelude_sub_names.contains(name)
                && !self
                    .our_scoped_package_items
                    .contains(crate::qualified::qualified(Symbol::intern("GLOBAL"), *name).as_str())
        });
        if names.is_empty() {
            return;
        }
        let table = crate::runtime::cow_table_mut(&mut self.module.operator_import_units);
        for name in names {
            let families = table.entry(name).or_default();
            families.entry(decl_unit).or_default();
            if contested_main.contains(&name) {
                families.entry(main).or_default();
            }
        }
        self.module.operator_import_gen += 1;
        self.invalidate_fn_resolution();
    }

    /// Narrow a positive multi-candidate probe for a scoped `name` (see
    /// [`Self::operator_has_import_scope`]) to the candidates the running code
    /// can see: a family only another compunit declared or imported leaves
    /// the name undeclared here, not declared-but-uncallable. `base_keys` is
    /// the base-name index answer when the caller has one, `None` for a full
    /// scan.
    // Cost: O(k * p * e), k = keys scanned, p = search packages, e = EVAL
    // nesting depth; only for a scoped name.
    pub(crate) fn any_visible_candidate_of(
        &self,
        base_keys: Option<&[Symbol]>,
        packages: &[Symbol],
        name: &str,
    ) -> bool {
        let name_sym = Symbol::intern(name);
        let registry = self.registry();
        let visible = |key: &Symbol, def: &Arc<FunctionDef>| {
            packages
                .iter()
                .any(|pkg| dispatch_key::key_is_candidate_of(key.as_str(), pkg.as_str(), name))
                && self.operator_candidate_visible(name_sym, def)
        };
        match base_keys {
            Some(keys) => keys.iter().any(|key| {
                registry
                    .functions
                    .get(key)
                    .is_some_and(|def| visible(key, def))
            }),
            None => registry
                .functions
                .iter()
                .any(|(key, def)| visible(key, def)),
        }
    }

    /// Whether the `proto` of a scoped `name` that the bare-name search finds
    /// in `packages` is visible to the running code. The proto carries its
    /// declaring compunit like any candidate.
    // Cost: O(p * e), p = search packages, e = EVAL nesting depth.
    pub(crate) fn scoped_proto_visible(&self, packages: &[Symbol], name: &str) -> bool {
        let name_sym = Symbol::intern(name);
        let registry = self.registry();
        packages.iter().any(|pkg| {
            dispatch_key::qualified_lookup(pkg.as_str(), name)
                .and_then(|key| registry.proto_functions.get(&key))
                .is_some_and(|def| self.operator_candidate_visible(name_sym, def))
        })
    }

    /// Make the scoped families of `name` that `value` (a routine a custom
    /// `sub EXPORT` hands the importer) belongs to visible to the importing
    /// unit. A dispatcher names its captured candidates' units and a plain
    /// code object its own; a by-name routine reference names none, and then
    /// every family of `name` is granted.
    // Cost: O(c + f), c = the value's captured candidates, f = families of
    // `name`; O(1) when `name` is not scoped.
    pub(crate) fn grant_scoped_family_import(&mut self, name: &str, value: &Value) {
        let Some(name_sym) = Symbol::lookup(name) else {
            return;
        };
        if !self.operator_has_import_scope_sym(name_sym) {
            return;
        }
        let mut units: HashSet<Symbol> = HashSet::new();
        if let ValueView::Sub(data) = value.view() {
            match data
                .env
                .get("__mutsu_multi_dispatch_candidates")
                .map(Value::view)
            {
                Some(ValueView::Array(cands, _)) => {
                    for cand in cands.iter() {
                        if let ValueView::Sub(cd) = cand.view() {
                            units.insert(self.unit_of_source(cd.source_file.as_deref()));
                        }
                    }
                }
                _ => {
                    units.insert(self.unit_of_source(data.source_file.as_deref()));
                }
            }
        }
        let importer = self.current_unit;
        let Some(families) = self.module.operator_import_units.get(&name_sym) else {
            return;
        };
        let grant: Vec<Symbol> = families
            .iter()
            .filter(|(unit, importers)| {
                (units.is_empty() || units.contains(*unit)) && !importers.contains(&importer)
            })
            .map(|(unit, _)| *unit)
            .collect();
        if grant.is_empty() {
            return;
        }
        let table = crate::runtime::cow_table_mut(&mut self.module.operator_import_units);
        if let Some(families) = table.get_mut(&name_sym) {
            for unit in grant {
                families.entry(unit).or_default().insert(importer);
            }
        }
        self.module.operator_import_gen += 1;
    }

    /// Scope the main script's own package-less family of `name` to the main
    /// script, once a loaded module's family of the same name is scoped
    /// (#11081).
    ///
    /// Both families register under the same `GLOBAL::name/<sig>` keys. A
    /// candidate with no record is visible everywhere, so without this the
    /// script's `multi sub name(Str)` joined the dispatch inside the module,
    /// which in Raku only sees its own lexical family. Only a contested name
    /// is recorded: an uncontested script multi keeps the unit-blind dispatch
    /// caches, which a scoped name bypasses.
    // Cost: O(1) unless `name` is scoped; then O(1) hash probes.
    pub(crate) fn scope_main_family_if_contested(&mut self, name: &str, source_file: Option<&str>) {
        let Some(name_sym) = Symbol::lookup(name) else {
            return;
        };
        if !self.operator_has_import_scope_sym(name_sym) {
            return;
        }
        let main = crate::runtime::main_unit();
        if self.unit_of_source(source_file) != main
            || self
                .module
                .operator_import_units
                .get(&name_sym)
                .is_some_and(|families| families.contains_key(&main))
        {
            return;
        }
        crate::runtime::cow_table_mut(&mut self.module.operator_import_units)
            .entry(name_sym)
            .or_default()
            .entry(main)
            .or_default();
        self.module.operator_import_gen += 1;
        self.invalidate_fn_resolution();
    }
}
