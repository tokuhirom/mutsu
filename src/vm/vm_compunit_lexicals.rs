//! The file-scope lexicals of a module file with no `unit` declarator
//! (ADR-12529 §3, slice 24).
//!
//! A module body runs in its loader's env, so its top-level `my` declarations
//! land under the loader's own keys. A `unit module X` file has them moved
//! into `unit_lexicals` under `X` once the body has run, and its routines,
//! which run in package `X`, find them there. A file without `unit` (the
//! shape of Rakudo's `Telemetry.rakumod`) declares its routines in `GLOBAL`,
//! so no package names its compunit; its lexicals used to stay in the
//! loader's env. A routine of the module then read them through its caller's
//! env chain: a caller's own same-named lexical answered instead, a rebind
//! reached the module's other routines only through the return merge, and
//! the importer's `EVAL` saw the module's variables.
//!
//! Such a file's lexicals now move into `unit_lexicals` under the compunit's
//! own key, its source path, and a routine finds them by the file it was
//! declared in.

use super::*;

impl Interpreter {
    /// The compunit of the package-less module file the running routine was
    /// declared in, when its file-scope lexicals have a bucket. An inlined
    /// bare block belongs to the routine that encloses it, so the walk passes
    /// over those frames.
    // Cost: O(1) when no package-less module file was loaded; otherwise O(b),
    // b = inlined block frames on top of the running routine's.
    pub(crate) fn running_compunit_lexical_unit(&self) -> Option<Symbol> {
        if self.lexicals.compunit_lexical_units.is_empty() {
            return None;
        }
        // A multi candidate being matched has no frame yet: its declaring
        // unit answers while nothing has been pushed over the caller's frame.
        if let Some((unit, depth)) = self.lexicals.candidate_scope_unit
            && depth == self.routine_stack().len()
        {
            return self
                .lexicals
                .compunit_lexical_units
                .contains(&unit)
                .then_some(unit);
        }
        let def_file = self
            .routine_stack()
            .iter()
            .rev()
            .find(|f| !f.is_inlined_block)?
            .def_file?;
        let unit = self.unit_of_source_sym(Some(def_file));
        self.lexicals
            .compunit_lexical_units
            .contains(&unit)
            .then_some(unit)
    }

    /// [`Self::running_compunit_lexical_unit`], when that file declares
    /// `name` at its top level: the `unit_lexicals` bucket holding it.
    // Cost: as `running_compunit_lexical_unit`, plus one bucket probe.
    pub(crate) fn compunit_lexical_bucket(&self, name: &str) -> Option<Symbol> {
        let unit = self.running_compunit_lexical_unit()?;
        self.lexicals
            .unit_lexicals
            .get(unit.as_str())
            .is_some_and(|m| m.contains_key(name))
            .then_some(unit)
    }

    /// The file-scope lexical `name` of the package-less module file
    /// `def_file`, for a caller that knows the routine's declaring file
    /// without a frame for it (TRIR pushes none).
    // Cost: O(1).
    pub(crate) fn compunit_lexical_of_file(&self, def_file: Symbol, name: &str) -> Option<&Value> {
        if self.lexicals.compunit_lexical_units.is_empty() {
            return None;
        }
        let unit = self.unit_of_source_sym(Some(def_file));
        if !self.lexicals.compunit_lexical_units.contains(&unit) {
            return None;
        }
        self.lexicals
            .unit_lexicals
            .get(unit.as_str())
            .and_then(|m| m.get(name))
    }
}
