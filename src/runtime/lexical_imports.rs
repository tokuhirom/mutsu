//! The routines a block-scoped `use` binds lexically (ADR-8751 stage 1).
//!
//! Rakudo installs a `use`'s symbols in the lexical pad of the block that
//! contains it. mutsu's `use` still installs them into the routine registry
//! for the block's dynamic extent (the `ImportScope` snapshot); on top of
//! that, a `use` below the unit's top level records the routines it imports
//! in a *lexical import set*, keyed by an id the compiler gives the `use`
//! (its module, tags and arguments, so every `use` with the same import list
//! shares one set). A bareword call lexically inside the block carries the
//! ids of its enclosing sets (`OpCode::LexImportCall`) and calls the bound
//! routine ahead of everything else, so a closure created in the block keeps
//! the import after the block exited, and the import wins over the importing
//! unit's own routine of the same name.

use super::*;

/// What a lexical import set binds a routine name to.
#[derive(Clone)]
pub(crate) enum LexicalImport {
    /// A code object a `sub EXPORT` hook bound (`'&name' => -> |c { ... }`).
    Code(Value),
    /// A routine the import aliased from its module: its definition, which
    /// outlives the block's registry alias. It is called as the alias was,
    /// by definition (`compile_and_call_qualified_def`), so it runs in the
    /// module's own scope.
    Routine(std::sync::Arc<crate::runtime::FunctionDef>),
}

/// What a `use` that fills a lexical import set imported, collected while
/// it runs.
pub(crate) struct LexicalImportCapture {
    /// The compunit doing the importing: a module loaded by this `use`
    /// records its own `use`s through the same hooks, and those are not
    /// this block's imports.
    unit: Symbol,
    /// `(package, name)` of each routine alias the import installed.
    routines: Vec<(Symbol, Symbol)>,
    /// The `&name` env keys the import wrote (a `sub EXPORT` hook binds its
    /// routines there, not in the registry).
    amp_env_keys: HashSet<Symbol>,
}

impl Interpreter {
    /// Start collecting the imports of the `use` filling the lexical import
    /// set `id`; `None` when the set is already filled (every `use` with the
    /// same id imports the same routines).
    // Cost: O(1).
    pub(crate) fn begin_lexical_import_capture(
        &mut self,
        id: Symbol,
    ) -> Option<Option<LexicalImportCapture>> {
        if self.module.lexical_import_sets.contains_key(&id) {
            return None;
        }
        let capture = LexicalImportCapture {
            unit: self.executing_unit_sym_for_module_load(),
            routines: Vec::new(),
            amp_env_keys: HashSet::new(),
        };
        Some(self.module.lexical_import_capture.replace(capture))
    }

    /// Finish the capture [`Self::begin_lexical_import_capture`] started and
    /// bind the imported routines into the set `id`. `outer` is the capture
    /// that was running when this one began (a nested load's own `use`).
    // Cost: O(r), r = routines the `use` imported.
    pub(crate) fn finish_lexical_import_capture(
        &mut self,
        id: Symbol,
        outer: Option<LexicalImportCapture>,
    ) {
        let Some(capture) = std::mem::replace(&mut self.module.lexical_import_capture, outer)
        else {
            return;
        };
        let mut set = HashMap::new();
        for (package, name) in capture.routines {
            let amp = Symbol::intern(&format!("&{}", name.resolve()));
            let binding = if capture.amp_env_keys.contains(&amp) {
                self.env
                    .get_sym(amp)
                    .filter(|value| matches!(value.view(), ValueView::Sub(_)))
                    .cloned()
                    .map(LexicalImport::Code)
            } else {
                // A multi family has no single definition under its plain
                // key; it keeps the registry path until imports bind
                // dispatcher values (ADR-8751 stage 2).
                self.registry()
                    .function_def(crate::qualified::qualified(package, name))
                    .map(LexicalImport::Routine)
            };
            if let Some(binding) = binding {
                set.insert(name, binding);
            }
        }
        self.module
            .lexical_import_sets
            .insert(id, std::sync::Arc::new(set));
    }

    /// Note a routine alias the running import installed (see
    /// [`Self::record_imported_routine_alias`]).
    // Cost: O(1).
    pub(crate) fn note_lexical_import_routine(&mut self, package: &str, name: &str) {
        let importer = self.executing_unit_sym_for_module_load();
        if let Some(capture) = self.module.lexical_import_capture.as_mut()
            && capture.unit == importer
        {
            capture
                .routines
                .push((Symbol::intern(package), Symbol::intern(name)));
        }
    }

    /// Note an env key the running import wrote (see
    /// [`Self::record_import_env_key`]).
    // Cost: O(1).
    pub(crate) fn note_lexical_import_env_key(&mut self, key: Symbol) {
        if !key.resolve().starts_with('&') {
            return;
        }
        let importer = self.executing_unit_sym_for_module_load();
        if let Some(capture) = self.module.lexical_import_capture.as_mut()
            && capture.unit == importer
        {
            capture.amp_env_keys.insert(key);
        }
    }

    /// The routine `name` is bound to in the first of the import sets
    /// `chain` (innermost first) that binds it.
    // Cost: O(c), c = enclosing `use`-blocks (usually 1).
    pub(crate) fn lexical_import_binding(
        &self,
        chain: &[Symbol],
        name: Symbol,
    ) -> Option<LexicalImport> {
        chain
            .iter()
            .find_map(|id| self.module.lexical_import_sets.get(id)?.get(&name).cloned())
    }
}
