//! Declarator docs of the running compilation unit (ADR-0136) and the
//! `.WHY` caches built over them. ADR-10779 first listed these under `io`;
//! they are per-compilation-unit declaration metadata, saved and restored
//! around a module load, so they form their own holder in the `module`
//! subsystem. A spawned thread starts with none.

use super::*;

#[derive(Default, Clone)]
pub(crate) struct DeclaratorDocs {
    /// Declarator docs keyed the way `.WHY` looks them up (see
    /// `install_doc_comments`).
    pub(crate) doc_comments: HashMap<String, DocComment>,
    /// Ordered list of doc comments for $=pod
    pub(crate) doc_comment_list: Vec<DocComment>,
    /// Cache for .WHY results so identity checks (=:=) work
    pub(crate) why_cache: ValueMap,
    /// Pod declarators keyed by the concrete WHEREFORE object's stable id.
    /// DOC INIT uses AST-built declarants before runtime registration, so a
    /// name key would collide for multis and same-named parameters.
    pub(crate) why_object_cache: HashMap<u64, Value>,
    /// The named-declaration docs of every module loaded so far, by the
    /// module's compilation unit, in load order. A module's own
    /// [`Self::doc_comments`] are replaced by the importer's when the load
    /// ends, but a declaration of the module is documented for as long as it
    /// lives: `.WHY` on `&inc` finds `inc`'s doc after `use M` returned, from
    /// the importer and from the module's own routines alike (#12037). Only
    /// modules with at least one documented named declaration have an entry.
    pub(crate) loaded_units: Vec<(Symbol, std::sync::Arc<HashMap<String, DocComment>>)>,
}

impl DeclaratorDocs {
    /// Keep what a module's load recorded once the load ends: its own named
    /// docs under `unit`, and the docs of the modules it loaded in turn (a
    /// nested load returned them to the table that was current while it ran,
    /// which is this one).
    // Cost: O(u + d), u = loaded units already recorded, d = the module's named docs.
    pub(crate) fn keep_loaded_unit(&mut self, unit: Symbol, module: DeclaratorDocs) {
        for (nested_unit, docs) in module.loaded_units {
            self.set_loaded_unit(nested_unit, docs);
        }
        if !module.doc_comments.is_empty() {
            self.set_loaded_unit(unit, std::sync::Arc::new(module.doc_comments));
        }
    }

    fn set_loaded_unit(
        &mut self,
        unit: Symbol,
        docs: std::sync::Arc<HashMap<String, DocComment>>,
    ) {
        match self.loaded_units.iter_mut().find(|(u, _)| *u == unit) {
            Some(slot) => slot.1 = docs,
            None => self.loaded_units.push((unit, docs)),
        }
    }

    /// The doc a loaded module recorded under the first of `keys` it has an
    /// entry for. `unit` is the compilation unit the documented declaration
    /// was declared in, when the code object says (a routine does); with it
    /// only that unit's docs are consulted, so two modules documenting `&inc`
    /// stay apart. Without it (a type object names no file) every loaded unit
    /// is consulted in load order. Answers the unit too, to key the `.WHY`
    /// cache by.
    // Cost: O(u * k), u = loaded units consulted, k = keys.
    pub(crate) fn loaded_doc<'a>(
        &'a self,
        unit: Option<Symbol>,
        keys: &'a [String],
    ) -> Option<(Symbol, &'a String, &'a DocComment)> {
        self.loaded_units
            .iter()
            .filter(|(u, _)| unit.is_none_or(|wanted| wanted == *u))
            .find_map(|(u, docs)| {
                keys.iter()
                    .find_map(|key| docs.get(key).map(|doc| (*u, key, doc)))
            })
    }
}
