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
    Routine(std::sync::Arc<crate::ast::FunctionDef>),
}

/// Separates a lexical import set id's import list from the multi families
/// the importing block declares itself (`Compiler::with_block_multis`).
pub(crate) const BLOCK_MULTIS_MARK: &str = "\u{1}multis:";

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
    /// The plain routines the import bound lexically instead of aliasing
    /// them into the registry ([`Interpreter::bind_routine_lexically`]).
    lexical_defs: HashMap<Symbol, std::sync::Arc<crate::ast::FunctionDef>>,
    /// The multi candidates the import installed, per family name: what the
    /// family's dispatcher value binds. Rakudo's import binds the module's
    /// dispatcher in the block, hiding an outer family of the same name.
    multi_defs: HashMap<Symbol, Vec<std::sync::Arc<crate::ast::FunctionDef>>>,
    /// The multi families the importing block declares itself: the import
    /// merges with them, so they keep the registry path.
    block_multis: HashSet<Symbol>,
}

impl Interpreter {
    /// Start collecting the imports of a `use` that fills a lexical import
    /// set. Every run of the `use` refills it: the import binds its plain
    /// routines only there, never in the registry. Returns the capture that
    /// was running (a module loading inside an outer block's `use`).
    // Cost: O(n), n = length of the set id `id`.
    pub(crate) fn begin_lexical_import_capture(
        &mut self,
        id: Symbol,
    ) -> Option<LexicalImportCapture> {
        let block_multis = id
            .resolve()
            .split_once(BLOCK_MULTIS_MARK)
            .map(|(_, names)| names.split(',').map(Symbol::intern).collect())
            .unwrap_or_default();
        let capture = LexicalImportCapture {
            block_multis,
            unit: self.executing_unit_sym_for_module_load(),
            routines: Vec::new(),
            amp_env_keys: HashSet::new(),
            lexical_defs: HashMap::new(),
            multi_defs: HashMap::new(),
        };
        self.module.lexical_import_capture.replace(capture)
    }

    /// Whether the running import binds `name` lexically rather than
    /// aliasing it into the registry, and if so record `def` as its binding.
    /// Only a plain routine (no multi candidates, no proto, not an operator)
    /// imported by the compunit that wrote the block-scoped `use` qualifies;
    /// everything else keeps the registry alias (ADR-8751 stage 2).
    // Cost: O(1).
    pub(crate) fn bind_routine_lexically(
        &mut self,
        name: &str,
        def: &std::sync::Arc<crate::ast::FunctionDef>,
    ) -> bool {
        let importer = self.executing_unit_sym_for_module_load();
        let Some(capture) = self.module.lexical_import_capture.as_mut() else {
            return false;
        };
        if capture.unit != importer {
            return false;
        }
        capture
            .lexical_defs
            .insert(Symbol::intern(name), std::sync::Arc::clone(def));
        true
    }

    /// Whether the running import binds the multi family `name` (its
    /// candidates `entries`, `proto` its proto if it exported one) lexically
    /// rather than aliasing it into the registry, and if so record the
    /// candidates as the family's binding (ADR-8751 stage 2). The family's
    /// dispatcher value is built from the candidates alone, so a proto that
    /// does more than dispatch (a body beyond `{*}`, a signature beyond a
    /// capture) keeps the registry alias, as an operator does.
    // Cost: O(c), c = candidates.
    pub(crate) fn bind_family_lexically(
        &mut self,
        name: &str,
        entries: &[(Symbol, std::sync::Arc<crate::ast::FunctionDef>)],
        proto: Option<&crate::ast::FunctionDef>,
    ) -> bool {
        if entries.is_empty()
            || !entries.iter().all(|(key, _)| key.as_str().contains('/'))
            || self.module.lexical_import_capture.as_ref().is_some_and(|capture| {
                capture.block_multis.contains(&Symbol::intern(name))
            })
            || proto.is_some_and(|proto| !proto_only_dispatches(proto))
        {
            return false;
        }
        let importer = self.executing_unit_sym_for_module_load();
        let Some(capture) = self.module.lexical_import_capture.as_mut() else {
            return false;
        };
        if capture.unit != importer {
            return false;
        }
        capture.multi_defs.insert(
            Symbol::intern(name),
            entries.iter().map(|(_, def)| std::sync::Arc::clone(def)).collect(),
        );
        true
    }

    /// Finish the capture [`Self::begin_lexical_import_capture`] started and
    /// bind the imported routines into the set `id`. `outer` is the capture
    /// that was running when this one began (a nested load's own `use`).
    // Cost: O(r) plus O(e + c) per imported multi family, r = routines the
    // `use` imported, e = the frame env (the dispatcher captures it), c = the
    // family's candidates.
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
            if set.contains_key(&name) {
                // Each candidate of a multi family notes the family's name.
                continue;
            }
            let amp = Symbol::intern(&format!("&{}", name.resolve()));
            let binding = if let Some(def) = capture.lexical_defs.get(&name) {
                Some(LexicalImport::Routine(std::sync::Arc::clone(def)))
            } else if let Some(candidates) = capture.multi_defs.get(&name) {
                // A multi family binds as its dispatcher value over the
                // candidates the import installed (ADR-8751 stage 2), which a
                // closure keeps after the block's registry aliases are gone.
                // Neither the `&name` env key nor the proto's plain key may
                // answer instead: a call through either re-dispatches by name.
                Some(LexicalImport::Code(
                    self.sub_value_from_multi_candidates(&name.resolve(), candidates.clone()),
                ))
            } else if capture.amp_env_keys.contains(&amp) {
                self.env
                    .get_sym(amp)
                    .filter(|value| matches!(value.view(), ValueView::Sub(_)))
                    .cloned()
                    .map(LexicalImport::Code)
            } else {
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

    /// Note a multi candidate of family `name` the running import installed.
    // Cost: O(1) amortized.
    pub(crate) fn note_lexical_import_candidate(
        &mut self,
        name: &str,
        def: &std::sync::Arc<crate::ast::FunctionDef>,
    ) {
        let importer = self.executing_unit_sym_for_module_load();
        if let Some(capture) = self.module.lexical_import_capture.as_mut()
            && capture.unit == importer
            && !capture.block_multis.contains(&Symbol::intern(name))
        {
            capture
                .multi_defs
                .entry(Symbol::intern(name))
                .or_default()
                .push(std::sync::Arc::clone(def));
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

    /// Record that the innermost import scope's block filled set `id`.
    // Cost: O(s), s = sets the scope already holds (usually 0).
    pub(crate) fn note_active_lexical_import_set(&mut self, id: Symbol) {
        if let Some(top) = self.module.import_scope_stack.last_mut()
            && !top.lexical_sets.contains(&id)
        {
            top.lexical_sets.push(id);
        }
    }

    /// The lexical import sets of the blocks whose import scope is open,
    /// innermost first: what a lookup by a run-time name sees.
    // Cost: O(s), s = open import scopes' sets.
    pub(crate) fn active_lexical_import_chain(&self) -> Vec<Symbol> {
        self.module
            .import_scope_stack
            .iter()
            .rev()
            .flat_map(|scope| scope.lexical_sets.iter().rev())
            .copied()
            .collect()
    }

    /// The code object `&name` is lexically imported as in an open block, for
    /// a by-name lookup (`::('&name')`, `&::("name")`).
    // Cost: O(s) plus building the code object.
    pub(crate) fn active_lexical_import_value(&self, name: &str) -> Option<Value> {
        if self.module.import_scope_stack.is_empty() {
            return None;
        }
        let chain = self.active_lexical_import_chain();
        if chain.is_empty() {
            return None;
        }
        Some(match self.lexical_import_binding(&chain, Symbol::intern(name))? {
            LexicalImport::Code(value) => value,
            LexicalImport::Routine(def) => self.sub_value_from_function_def((*def).clone()),
        })
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

/// Whether `proto` only dispatches: its body is `{*}` (or empty) and its
/// signature is a bare capture (`proto f(|) {*}`), so its candidates alone
/// answer every call exactly as it does.
// Cost: O(p + s), p = parameters, s = body statements.
fn proto_only_dispatches(proto: &crate::ast::FunctionDef) -> bool {
    use crate::ast::{Expr, Stmt};
    let significant: Vec<&Stmt> = proto.body.iter().filter(|s| !s.is_marker()).collect();
    let trivial_body = match significant.as_slice() {
        [] => true,
        [Stmt::Expr(expr)] => matches!(expr, Expr::Whatever) || expr.is_onlystar_dispatch(),
        _ => false,
    };
    trivial_body
        && proto
            .param_defs
            .iter()
            .all(|param| param.slurpy && param.sigilless && !param.named)
}
