//! Compilation-unit scoping for imported operator candidates (#9944).
//!
//! Raku resolves an operator lexically: `$a * $b` inside module `B` sees the
//! core `infix:<*>` plus whatever `B` itself declared or imported, never a
//! candidate some *other* compunit imported. mutsu's routine registry is flat,
//! and a script's (or a `unit module` body's) imports land as `GLOBAL::`
//! aliases, so a bare-name operator walk from any module used to reach every
//! candidate that anyone had imported. With a `where` clause on the foreign
//! candidate, every `*` of an unrelated module paid for evaluating it (the
//! Bitcoin distribution's `FiniteField` spent 0.8 s per point operation).
//!
//! Two layers make the visibility follow the importing compunit:
//!
//! - the name-level gate `user_declared_infix_ops` records the *importing*
//!   unit rather than "visible everywhere", so a unit that neither declared
//!   nor imported an `infix:<op>` keeps the native fast paths;
//! - [`Interpreter::operator_candidate_visible`] filters the candidate walk
//!   itself, so a unit that has an `infix:<op>` of its own in scope still does
//!   not see a candidate family only some other unit imported.
//!
//! A candidate is identified by its declaring unit (its `source_file`) and its
//! operator name. Only candidates that were imported somewhere carry a record;
//! a candidate with none keeps the older name-level scoping alone.

use super::*;

impl Interpreter {
    /// Record that the unit executing right now imported the candidates
    /// `defs` of `name`.
    ///
    /// With `create`, a family with no record yet gets one (an operator is
    /// scoped by being imported); without it only families that already carry
    /// a record -- the package-less ones `scope_unit_multi_families` scoped to
    /// their declaring unit (#11004) -- gain the importer.
    // Cost: O(d), d = the imported candidates.
    pub(crate) fn record_operator_import<'a>(
        &mut self,
        name: &str,
        defs: impl IntoIterator<Item = &'a Arc<FunctionDef>>,
        create: bool,
    ) {
        let importer = self.current_unit;
        let name_sym = Symbol::intern(name);
        let mut decl_units: HashSet<Symbol> = defs
            .into_iter()
            .map(|def| self.unit_of_source(def.source_file.as_deref()))
            .collect();
        if !create {
            let Some(families) = self.operator_import_units.get(&name_sym) else {
                return;
            };
            decl_units.retain(|unit| {
                families
                    .get(unit)
                    .is_some_and(|importers| !importers.contains(&importer))
            });
        }
        if decl_units.is_empty() {
            return;
        }
        self.operator_import_gen += 1;
        let table = crate::runtime::cow_table_mut(&mut self.operator_import_units);
        let families = table.entry(name_sym).or_default();
        for unit in decl_units {
            families.entry(unit).or_default().insert(importer);
        }
    }

    /// Record the importing unit in the name-level operator gate
    /// (`user_declared_infix_ops`) for an imported `infix:<op>`: the operator
    /// is lexically visible in the unit that imported it, not everywhere.
    pub(crate) fn record_infix_import_gate(&mut self, name: &str) {
        if !name.starts_with("infix:<") {
            return;
        }
        let importer = self.current_unit;
        crate::runtime::cow_table_mut(&mut self.user_declared_infix_ops)
            .entry(name.to_string())
            .or_default()
            .insert(importer);
        crate::vm::vm_jit::note_user_infix_decl();
    }

    /// Whether operator candidate `def` of `name_sym` is visible to the code
    /// running right now: always, unless a `use` imported it somewhere, in
    /// which case only its declaring unit and the importing units see it.
    ///
    /// Two anchors, as in `prelude_visible_here`: `current_unit` names the
    /// unit of the routine being executed (including an `EVAL` unit), and the
    /// frame's `def_file` covers a body entered without switching it.
    // Cost: O(e), e = EVAL nesting depth of the running code; O(1) when no
    // operator was ever imported.
    pub(crate) fn operator_candidate_visible(&self, name_sym: Symbol, def: &FunctionDef) -> bool {
        if self.operator_import_units.is_empty() {
            return true;
        }
        let Some(families) = self.operator_import_units.get(&name_sym) else {
            return true;
        };
        let decl_unit = self.unit_of_source(def.source_file.as_deref());
        let Some(importers) = families.get(&decl_unit) else {
            return true;
        };
        let executing = self.executing_unit_sym();
        [self.current_unit, executing].into_iter().any(|anchor| {
            self.unit_chain_contains_unit(anchor, decl_unit)
                || self.unit_chain_contains(anchor, importers)
        })
    }

    /// Whether any operator candidate of `name` carries an import record, so
    /// its resolution depends on the unit asking.
    // Cost: O(1).
    #[inline]
    pub(crate) fn operator_has_import_scope(&self, name: &str) -> bool {
        !self.operator_import_units.is_empty()
            && Symbol::lookup(name).is_some_and(|sym| self.operator_import_units.contains_key(&sym))
    }

    /// [`Self::operator_has_import_scope`] for an already-interned name.
    // Cost: O(1).
    #[inline]
    pub(crate) fn operator_has_import_scope_sym(&self, name: Symbol) -> bool {
        !self.operator_import_units.is_empty() && self.operator_import_units.contains_key(&name)
    }

    /// Drop the operator candidates of `name` that the running code cannot
    /// see (see [`Self::operator_candidate_visible`]).
    // Cost: O(c * e), c = candidates, e = EVAL nesting depth.
    pub(crate) fn retain_visible_operator_candidates(
        &self,
        name: &str,
        candidates: &mut Vec<(String, Arc<FunctionDef>)>,
    ) {
        if !self.operator_has_import_scope(name) {
            return;
        }
        let name_sym = Symbol::intern(name);
        candidates.retain(|(_, def)| self.operator_candidate_visible(name_sym, def));
    }

    /// `def` itself when the running code can see it as a candidate of
    /// operator `name`, `None` otherwise.
    pub(crate) fn visible_operator_def(
        &self,
        name: &str,
        def: Arc<FunctionDef>,
    ) -> Option<Arc<FunctionDef>> {
        if !self.operator_has_import_scope(name)
            || self.operator_candidate_visible(Symbol::intern(name), &def)
        {
            Some(def)
        } else {
            None
        }
    }
}
