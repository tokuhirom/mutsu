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
        names.retain(|name| {
            let name_str = name.as_str();
            name_str != "MAIN"
                && name_str != "EXPORT"
                && !self.prelude_sub_names.contains(name)
                && !self
                    .our_scoped_package_items
                    .contains(crate::qualified::qualified(Symbol::intern("GLOBAL"), *name).as_str())
        });
        if names.is_empty() {
            return;
        }
        let table = crate::runtime::cow_table_mut(&mut self.operator_import_units);
        for name in names {
            table.entry(name).or_default().entry(decl_unit).or_default();
        }
        self.operator_import_gen += 1;
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
        let Some(families) = self.operator_import_units.get(&name_sym) else {
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
        let table = crate::runtime::cow_table_mut(&mut self.operator_import_units);
        if let Some(families) = table.get_mut(&name_sym) {
            for unit in grant {
                families.entry(unit).or_default().insert(importer);
            }
        }
        self.operator_import_gen += 1;
    }
}
