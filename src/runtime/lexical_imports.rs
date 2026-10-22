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

/// Separates a lexical import set id's import list from the variables the
/// compiler declares in the importing block for it
/// (`Compiler::bind_block_imported_vars`): the import writes no env alias
/// for those.
pub(crate) const BLOCK_VARS_MARK: &str = "\u{1}vars:";

/// Marks the id of a lexical import set an `EVAL`'d unit's top-level `use`
/// fills (`Compiler::enter_eval_unit_lexical_uses`). Each `EVAL` is a
/// compilation of its own, so such a `use` is never replayed
/// (`lexical_import_replay`): its module's `sub EXPORT` runs every time.
pub(crate) const EVAL_UNIT_MARK: &str = "\u{1}eval";

/// Marks the id of a lexical import set a module unit's top-level `use`
/// fills (`Compiler::defer_unit_lexical_use_sets`). The variables, constants
/// and types it lists are pad bindings of the module's mainline: the load
/// moves them into the module's compunit lexicals
/// (`ModuleState::module_unit_bound_imports`). A module's mainline runs
/// once, so such a set is never replayed either.
pub(crate) const MODULE_UNIT_MARK: &str = "\u{1}module";

/// The names listed after `mark` in the set id `id`, up to the next mark.
// Cost: O(n), n = length of `id`.
pub(super) fn id_section(id: &str, mark: &str) -> HashSet<Symbol> {
    id.split_once(mark)
        .map(|(_, rest)| {
            rest.split('\u{1}')
                .next()
                .unwrap_or_default()
                .split(',')
                .filter(|name| !name.is_empty())
                .map(Symbol::intern)
                .collect()
        })
        .unwrap_or_default()
}

/// What a `use` that fills a lexical import set imported, collected while
/// it runs.
pub(crate) struct LexicalImportCapture {
    /// The compunit doing the importing: a module loaded by this `use`
    /// records its own `use`s through the same hooks, and those are not
    /// this block's imports.
    pub(super) unit: Symbol,
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
    /// The core listop families whose candidates extend CORE's dispatch set
    /// (ADR-0044 D1): they keep the registry path.
    core_extending: HashSet<Symbol>,
    /// The proto of each family in `multi_defs` whose body does more than
    /// dispatch: the family's dispatcher value runs it.
    protos: HashMap<Symbol, std::sync::Arc<crate::ast::FunctionDef>>,
    /// The variables (in their declaration spelling, `x` / `@a`) the importing
    /// block declares itself, bound to the module's package variables: the
    /// import writes no env alias for them.
    block_vars: HashSet<Symbol>,
    /// The values a `sub EXPORT` hook returned for names in `block_vars`
    /// (their declaration keys), which the block's bindings read
    /// (`ModuleState::lexical_import_hook_values`).
    pub(super) hook_values: HashMap<Symbol, Value>,
    /// The plain routines a `sub EXPORT` hook returned, by name: bound in
    /// the set as they are, with no env alias.
    pub(super) hook_routines: HashMap<Symbol, Value>,
    /// Whether the set is a module unit's own top-level one
    /// (`MODULE_UNIT_MARK`): its pad bindings belong to the module's
    /// mainline, not to an open block of the loading scope.
    module_unit: bool,
}

/// Whether `name` is one of the seven core array listops (ADR-0044 D1).
// Cost: O(1).
fn is_core_listop_name(name: &str) -> bool {
    matches!(
        name,
        "push" | "pop" | "shift" | "unshift" | "append" | "prepend" | "splice"
    )
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
        let text = id.resolve();
        let block_multis = id_section(&text, BLOCK_MULTIS_MARK);
        let block_vars = id_section(&text, BLOCK_VARS_MARK);
        let importer = self.executing_unit_sym_for_module_load();
        // A module unit's own top-level `use`: its mainline declares these.
        let module_unit = text.contains(MODULE_UNIT_MARK);
        if module_unit && !block_vars.is_empty() {
            self.module
                .module_unit_bound_imports
                .extend(block_vars.iter().copied());
        }
        let capture = LexicalImportCapture {
            block_multis,
            core_extending: HashSet::new(),
            block_vars,
            unit: importer,
            routines: Vec::new(),
            amp_env_keys: HashSet::new(),
            lexical_defs: HashMap::new(),
            multi_defs: HashMap::new(),
            protos: HashMap::new(),
            hook_values: HashMap::new(),
            hook_routines: HashMap::new(),
            module_unit,
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

    /// Whether the importing block declares the variable `name` (its
    /// declaration spelling) itself, bound to the module's package variable,
    /// so the running import writes no env alias for it (ADR-8751).
    // Cost: O(1).
    pub(crate) fn block_binds_imported_var(&self, name: &str) -> bool {
        let Some(capture) = self.module.lexical_import_capture.as_ref() else {
            return false;
        };
        !capture.block_vars.is_empty()
            && capture.unit == self.executing_unit_sym_for_module_load()
            && (capture.block_vars.contains(&Symbol::intern(name))
                // A type is bound as a term (`my \T := ...`).
                || capture
                    .block_vars
                    .contains(&Symbol::intern(&crate::runtime::term_names::term_key(name))))
    }

    /// Whether the running import binds the multi family `name` (its
    /// candidates `entries`, `proto` its proto if it exported one) lexically
    /// rather than aliasing it into the registry, and if so record the
    /// candidates as the family's binding (ADR-8751 stage 2). A proto whose
    /// body does more than `{*}` is recorded too: the family's dispatcher
    /// value runs it, and its `{*}` comes back to the candidates.
    // Cost: O(c), c = candidates.
    pub(crate) fn bind_family_lexically(
        &mut self,
        name: &str,
        entries: &[(Symbol, std::sync::Arc<crate::ast::FunctionDef>)],
        proto: Option<&crate::ast::FunctionDef>,
    ) -> bool {
        // An only-star proto with no candidates (a bodied one imports as a
        // plain routine) has nothing to dispatch to, but binding it keeps the
        // import out of the registry, where it would hide the importer's
        // routine of the name from callees outside the block (#13994).
        if entries.is_empty()
            && let Some(proto) = proto
        {
            return self.bind_routine_lexically(name, &std::sync::Arc::new(proto.clone()));
        }
        if proto.is_none() && is_core_listop_name(name) {
            if let Some(capture) = self.module.lexical_import_capture.as_mut() {
                capture.core_extending.insert(Symbol::intern(name));
            }
            return false;
        }
        if entries.is_empty()
            || !entries.iter().all(|(key, _)| key.as_str().contains('/'))
            || self.module.lexical_import_capture.as_ref().is_some_and(|capture| {
                capture.block_multis.contains(&Symbol::intern(name))
            })
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
        // `entries` come from a registry key walk (hash order), but the family
        // later ranks equally narrow candidates by the order they are listed
        // here (`pick_captured_multi_candidate` stamps them in turn): keep the
        // declaration order Rakudo breaks such ties by.
        let mut defs: Vec<std::sync::Arc<crate::ast::FunctionDef>> =
            entries.iter().map(|(_, def)| std::sync::Arc::clone(def)).collect();
        defs.sort_by_key(|def| def.decl_order);
        capture.multi_defs.insert(Symbol::intern(name), defs);
        if let Some(proto) = proto.filter(|proto| !proto_body_only_dispatches(&proto.body)) {
            capture
                .protos
                .insert(Symbol::intern(name), std::sync::Arc::new(proto.clone()));
        }
        true
    }

    /// Whether the operator routine `name` binds in a lexical import set
    /// instead of the registry: an `infix:<op>` that is not a built-in
    /// operator, whose every use site reads the set (`LexImportInfix`,
    /// `LexImportMeta`, `LexImportCall` for `&infix:<op>`), or a `prefix`,
    /// `postfix` or `circumfix` operator that is not a built-in one: the
    /// parser turns each use of one into a bareword call of its full name,
    /// which reads the set through `LexImportCall`. A built-in operator's
    /// opcode finds user candidates in the registry, so it keeps the alias,
    /// as does `postcircumfix` (its uses compile to subscript ops).
    // Cost: O(m), m = bytes of `name`.
    pub(crate) fn lexically_bindable_operator(name: &str) -> bool {
        use crate::compiled_operator::{
            is_builtin_circumfix, is_builtin_infix, is_builtin_postfix, is_builtin_prefix,
        };
        let Some((category, rest)) = name.split_once(":<") else {
            return false;
        };
        let Some(op) = rest.strip_suffix('>').filter(|op| !op.is_empty()) else {
            return false;
        };
        match category {
            // `(elem)` / `(cont)` are built-ins the reduction tables do not
            // list; `crate::lexical_builtin_op` names them.
            "infix" => {
                (!is_builtin_infix(op) && !crate::lexical_builtin_op::binds_lexically(name))
                    || crate::lexical_builtin_op::is_call_spelled_infix(op)
                    || crate::lexical_builtin_op::is_replaced_infix(op)
            }
            "prefix" => !is_builtin_prefix(op),
            "postfix" => {
                !is_builtin_postfix(op) || crate::lexical_builtin_op::is_lexical_postfix(op)
            }
            "circumfix" => !is_builtin_circumfix(op),
            _ => false,
        }
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
        if capture.hook_values.is_empty() {
            self.module.lexical_import_hook_values.remove(&id);
        } else {
            self.module
                .lexical_import_hook_values
                .insert(id, std::sync::Arc::new(capture.hook_values));
        }
        // A family an earlier `use` of this scope bound too (the provider and
        // a module extending its exported proto) is one family: this set's
        // dispatcher spans the earlier candidates as well.
        let mut multi_defs = capture.multi_defs;
        // Candidates for a core listop name extend CORE's dispatch set
        // (ADR-0044 D1): a dispatcher over them alone would hide the core
        // array forms, so a family without its own proto stays a registry alias.
        multi_defs.retain(|name, _| !capture.core_extending.contains(name));
        let importer = self.executing_unit_sym_for_module_load();
        let earlier_sets: Vec<Symbol> = match self.module.import_scope_stack.last() {
            Some(scope) if scope.unit == importer => scope.lexical_sets.clone(),
            _ => self
                .module
                .unit_lexical_sets
                .get(&importer)
                .cloned()
                .unwrap_or_default(),
        }
        .into_iter()
        .filter(|s| *s != id)
        .collect();
        for (name, defs) in multi_defs.iter_mut() {
            for earlier in &earlier_sets {
                let Some(prior) = self
                    .module
                    .lexical_set_families
                    .get(earlier)
                    .and_then(|families| families.get(name))
                else {
                    continue;
                };
                for def in prior.iter().rev() {
                    if !defs.iter().any(|d| std::sync::Arc::ptr_eq(d, def)) {
                        defs.insert(0, std::sync::Arc::clone(def));
                    }
                }
            }
        }
        self.module
            .lexical_set_families
            .insert(id, multi_defs.clone());
        let mut set = HashMap::new();
        for (package, name) in capture.routines {
            if set.contains_key(&name) {
                // Each candidate of a multi family notes the family's name.
                continue;
            }
            let amp = Symbol::intern(&format!("&{}", name.resolve()));
            // A value the `sub EXPORT` hook handed out under the name is the
            // import, ahead of a same-named routine a module the hook `use`d
            // for itself left in the capture.
            let binding = if let Some(value) = capture.hook_routines.get(&name) {
                Some(LexicalImport::Code(value.clone()))
            } else if let Some(def) = capture.lexical_defs.get(&name) {
                Some(LexicalImport::Routine(std::sync::Arc::clone(def)))
            } else if let Some(candidates) = multi_defs
                .get(&name)
                .filter(|_| !capture.amp_env_keys.contains(&amp))
            {
                // (A family a `sub EXPORT` hook also handed candidates of
                // keeps the env alias that carries both.)
                // A multi family binds as its dispatcher value over the
                // candidates the import installed (ADR-8751 stage 2), which a
                // closure keeps after the block's registry aliases are gone.
                // Neither the `&name` env key nor the proto's plain key may
                // answer instead: a call through either re-dispatches by name.
                let dispatcher =
                    self.sub_value_from_multi_candidates(&name.resolve(), candidates.clone());
                // A built-in operator's user candidates add to the built-in:
                // a call none of them accepts is the operator's own.
                let dispatcher = if extends_builtin_operator(&name.resolve()) {
                    self.note_lexical_operator_candidate_defs(&dispatcher, candidates);
                    Self::with_builtin_operator_fallback(dispatcher)
                } else {
                    dispatcher
                };
                Some(LexicalImport::Code(match capture.protos.get(&name) {
                    Some(proto) => self.with_dispatch_proto(dispatcher, proto),
                    None => dispatcher,
                }))
            } else if capture.amp_env_keys.contains(&amp) {
                // A hook handed out candidates of a built-in operator: they
                // join the operator through its env alias (the operator's own
                // dispatch finds them there, with the built-in as fallback), a
                // set binding would shadow the built-in for a call none of
                // them accepts.
                if extends_builtin_operator(&name.resolve()) {
                    None
                } else {
                    self.env
                        .get_sym(amp)
                        .filter(|value| matches!(value.view(), ValueView::Sub(_)))
                        .cloned()
                        .map(LexicalImport::Code)
                }
            } else if capture.core_extending.contains(&name) {
                None
            } else {
                self.registry()
                    .function_def(crate::qualified::qualified(package, name))
                    .map(LexicalImport::Routine)
            };
            if let Some(binding) = binding {
                set.insert(name, binding);
            }
        }
        if set
            .values()
            .any(|b| matches!(b, LexicalImport::Code(v) if Self::is_builtin_operator_dispatcher(v)))
        {
            self.module.lexical_operator_import_sets.insert(id);
        } else {
            self.module.lexical_operator_import_sets.remove(&id);
        }
        self.module
            .lexical_import_sets
            .insert(id, std::sync::Arc::new(set));
    }

    /// Record the definition of each captured candidate of `dispatcher`,
    /// built from `defs` in order (`lexical_operator_candidate_defs`).
    // Cost: O(c), c = candidates.
    fn note_lexical_operator_candidate_defs(
        &mut self,
        dispatcher: &Value,
        defs: &[std::sync::Arc<crate::ast::FunctionDef>],
    ) {
        let ValueView::Sub(data) = dispatcher.view() else {
            return;
        };
        let Some(candidates) = data
            .env
            .get_sym(crate::symbol::wk::multi_dispatch_candidates())
            .and_then(|list| list.as_list_items().map(ToOwned::to_owned))
        else {
            return;
        };
        let table = crate::runtime::cow_table_mut(&mut self.module.lexical_operator_candidate_defs);
        for (candidate, def) in candidates.iter().zip(defs) {
            if let ValueView::Sub(cand) = candidate.view() {
                table.insert(cand.id, std::sync::Arc::clone(def));
            }
        }
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

    /// Record that the innermost import scope's block declares the import
    /// `name` itself (a variable, constant or type it binds in its pad), so
    /// a routine written outside the block hides it like an imported env key.
    // Cost: O(|name|).
    pub(crate) fn note_block_bound_import(&mut self, name: &str) {
        let name = crate::runtime::term_names::term_spelling(name).unwrap_or(name);
        let term = crate::runtime::term_names::term_key(name);
        // A unit's own top-level `use` binds in the unit's mainline (a
        // module unit's, or the program's, which opens no import scope): the
        // import is the unit's, recorded as its provenance, which the unit's
        // own routines consult (`bare_name_visible_here`).
        if self.module.import_scope_stack.is_empty()
            || self
                .module
                .lexical_import_capture
                .as_ref()
                .is_some_and(|capture| capture.module_unit)
        {
            self.note_unit_env_import(Symbol::intern(name));
            self.note_unit_env_import(Symbol::intern(&term));
            return;
        }
        if let Some(top) = self.module.import_scope_stack.last_mut() {
            top.block_bound_keys.insert(Symbol::intern(name));
            top.block_bound_keys.insert(Symbol::intern(&term));
        }
    }

    /// Record that the innermost import scope's block filled set `id`.
    // Cost: O(s), s = sets the scope already holds (usually 0).
    pub(crate) fn note_active_lexical_import_set(&mut self, id: Symbol) {
        let importer = self.executing_unit_sym_for_module_load();
        // A `use` at the unit's own top level, or in a module's body loaded
        // while an importer's block is open, belongs to the unit (the scope on
        // top is the importer's).
        match self.module.import_scope_stack.last_mut() {
            Some(top) if top.unit == importer => {
                if !top.lexical_sets.contains(&id) {
                    top.lexical_sets.push(id);
                }
            }
            _ => {
                let sets = self.module.unit_lexical_sets.entry(importer).or_default();
                if !sets.contains(&id) {
                    sets.push(id);
                }
            }
        }
    }

    /// The routines the running unit's top-level `use`s bound in their import
    /// sets, by name.
    // Cost: O(r), r = routines those sets bind.
    pub(crate) fn unit_lexical_import_routines(&self) -> Vec<(Symbol, Value)> {
        let mut routines: Vec<(Symbol, Value)> = Vec::new();
        for id in self.running_unit_lexical_sets() {
            let Some(set) = self.module.lexical_import_sets.get(&id) else {
                continue;
            };
            for (name, binding) in set.iter() {
                if routines.iter().any(|(seen, _)| seen == name) {
                    continue;
                }
                let value = match binding {
                    LexicalImport::Code(value) => value.clone(),
                    LexicalImport::Routine(def) => self.sub_value_from_function_def((**def).clone()),
                };
                routines.push((*name, value));
            }
        }
        routines
    }

    /// Whether the code running now is a module's own top level (or a role
    /// body composed from it) while a routine of its importer is still on the
    /// stack: the module sees its own `use`s, not the importer's scopes (#14481).
    // Cost: O(1).
    fn running_module_mainline(&self) -> bool {
        !self.module.module_loading_unit_stack.is_empty()
            && self.executing_unit_sym_for_module_load() != self.executing_unit_sym()
    }

    /// The import sets around code running in a module's own top level: the
    /// open blocks of that module, then its top-level `use`s.
    // Cost: O(s), s = open import scopes' sets.
    fn module_mainline_import_chain(&self) -> Vec<Symbol> {
        let unit = self.executing_unit_sym_for_module_load();
        self.module
            .import_scope_stack
            .iter()
            .rev()
            .filter(|scope| scope.unit == unit)
            .flat_map(|scope| scope.lexical_sets.iter().rev())
            .copied()
            .chain(self.running_unit_lexical_sets())
            .collect()
    }

    /// The import sets the running unit's top-level `use`s filled, innermost
    /// first.
    // Cost: O(s), s = sets the unit filled.
    fn running_unit_lexical_sets(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.module
            .unit_lexical_sets
            .get(&self.executing_unit_sym_for_module_load())
            .filter(|_| !self.module.unit_sets_hidden)
            .into_iter()
            .flat_map(|sets| sets.iter().rev().copied())
    }

    /// The lexical import sets of the blocks whose import scope is open,
    /// innermost first: what a lookup by a run-time name sees.
    // Cost: O(s), s = open import scopes' sets.
    pub(crate) fn active_lexical_import_chain(&self) -> Vec<Symbol> {
        if self.running_module_mainline() {
            return self.module_mainline_import_chain();
        }
        if let Some(chain) = &self.module.eval_import_chain {
            let mut chain = chain.clone();
            // A routine of another compunit the EVAL'd code called (a role
            // body composed in the EVAL, #14481) sees its own unit's
            // top-level `use`s, which the EVAL's chain cannot name.
            if self.module.eval_import_unit != Some(self.executing_unit_sym()) {
                for set in self.running_unit_lexical_sets() {
                    if !chain.contains(&set) {
                        chain.push(set);
                    }
                }
            }
            return chain;
        }
        self.module
            .import_scope_stack
            .iter()
            .rev()
            .flat_map(|scope| scope.lexical_sets.iter().rev())
            .copied()
            .chain(self.running_unit_lexical_sets())
            .collect()
    }

    /// The import sets an `EVAL` called from the running frame starts inside,
    /// innermost first: the sets of the `use`-blocks open in this frame (a
    /// scope a routine this frame was called from opened is outside it), then
    /// the chain the running code's `EVAL` sites were compiled under. Code
    /// that records no `EVAL` site keeps every open set.
    // Cost: O(s + e), s = open import scopes' sets, e = the code's EVAL site chains.
    pub(crate) fn eval_lexical_import_chain(&self) -> Vec<Symbol> {
        if self.running_module_mainline() {
            return self.module_mainline_import_chain();
        }
        let Some(code) = self.running_code() else {
            return self.active_lexical_import_chain();
        };
        let mut sites = code.eval_site_chains.iter();
        let Some(first) = sites.next() else {
            return self.active_lexical_import_chain();
        };
        // The frame this EVAL runs in: the innermost routine or closure,
        // not an inlined bare block.
        let frame_depth = self
            .routine_stack()
            .iter()
            .rposition(|f| !f.is_inlined_block)
            .map_or(0, |i| i + 1);
        let mut chain: Vec<Symbol> = self
            .module
            .import_scope_stack
            .iter()
            .rev()
            .filter(|scope| scope.routine_depth >= frame_depth)
            .flat_map(|scope| scope.lexical_sets.iter().rev())
            .copied()
            .collect();
        for set in self.running_unit_lexical_sets() {
            if !chain.contains(&set) {
                chain.push(set);
            }
        }
        // Innermost first, so the sets every EVAL site shares are a suffix.
        let mut common: &[Symbol] = first;
        for site in sites {
            let shared = common
                .iter()
                .rev()
                .zip(site.iter().rev())
                .take_while(|(a, b)| a == b)
                .count();
            common = &common[common.len() - shared..];
        }
        for set in common {
            if !chain.contains(set) {
                chain.push(*set);
            }
        }
        chain
    }

    /// Whether an open block's `use` bound the routine `name` lexically, for
    /// the sets an `EVAL` running now starts inside.
    // Cost: O(s), s = open import scopes' sets.
    pub(crate) fn lexically_imported_routine(&self, name: &str) -> bool {
        if self.module.import_scope_stack.is_empty()
            && self.module.eval_import_chain.is_none()
            && self.module.unit_lexical_sets.is_empty()
        {
            return false;
        }
        let chain = self.active_lexical_import_chain();
        !chain.is_empty()
            && self
                .lexical_import_binding(&chain, Symbol::intern(name))
                .is_some()
    }

    /// Whether the running unit declares a routine `name` itself (in its own
    /// file), as opposed to importing one.
    // Cost: O(1) hashed probes plus one unit lookup.
    pub(crate) fn running_unit_declares_routine(&self, name: &str) -> bool {
        let anchor = self.current_unit;
        self.resolve_function(name).is_some_and(|def| {
            def.source_file.is_some()
                && self.unit_of_declaring_file(def.source_file.as_deref()) == anchor
        })
    }

    /// The code object `&name` is lexically imported as in an open block, for
    /// a by-name lookup (`::('&name')`, `&::("name")`).
    // Cost: O(s) plus building the code object.
    pub(crate) fn active_lexical_import_value(&self, name: &str) -> Option<Value> {
        if self.module.import_scope_stack.is_empty() && self.module.unit_lexical_sets.is_empty() {
            return None;
        }
        // The sets lexically around the lookup site, as for an `EVAL`: a
        // routine called from the `use`-block is outside them.
        let mut chain = self.eval_lexical_import_chain();
        // A routine the running unit declares itself wins over what its
        // top-level `use`s import (Rakudo reports a redeclaration there);
        // a block's own `use` still wins over the unit's routine.
        if self.running_unit_declares_routine(name) {
            let unit_sets: Vec<Symbol> = self.running_unit_lexical_sets().collect();
            chain.retain(|id| !unit_sets.contains(id));
        }
        if chain.is_empty() {
            return None;
        }
        Some(match self.lexical_import_binding(&chain, Symbol::intern(name))? {
            LexicalImport::Code(value) => value,
            LexicalImport::Routine(def) => self.sub_value_from_function_def((*def).clone()),
        })
    }

    /// [`Self::lexical_import_binding`] for the user candidates of a built-in
    /// operator: every import set of `chain` that binds `name` adds its
    /// candidates, as two `use`s of modules that both extend `+` add both
    /// modules' candidates to the block's `+` (roast `S11-modules/export.t`).
    // Cost: O(c) probe, c = enclosing `use`-blocks; O(k) more to merge, k =
    // candidates, when more than one set binds the operator.
    pub(crate) fn lexical_import_operator_binding(
        &self,
        chain: &[Symbol],
        name: Symbol,
    ) -> Option<LexicalImport> {
        // No set binds a built-in operator's dispatcher: nothing to merge.
        if self.module.lexical_operator_import_sets.is_empty() {
            return self.lexical_import_binding(chain, name);
        }
        let mut found = chain.iter().filter_map(|id| {
            match self.module.lexical_import_sets.get(id)?.get(&name)? {
                LexicalImport::Code(value) if Self::is_builtin_operator_dispatcher(value) => {
                    Some(value.clone())
                }
                _ => None,
            }
        });
        let Some(first) = found.next() else {
            return self.lexical_import_binding(chain, name);
        };
        let rest: Vec<Value> = found.collect();
        if rest.is_empty() {
            return Some(LexicalImport::Code(first));
        }
        let candidates_of = |value: &Value| -> Vec<Value> {
            match value.view() {
                ValueView::Sub(data) => data
                    .env
                    .get_sym(crate::symbol::wk::multi_dispatch_candidates())
                    .and_then(|list| list.as_list_items().map(ToOwned::to_owned))
                    .unwrap_or_default(),
                _ => Vec::new(),
            }
        };
        let mut candidates = candidates_of(&first);
        for value in &rest {
            candidates.extend(candidates_of(value));
        }
        let ValueView::Sub(data) = first.view() else {
            return Some(LexicalImport::Code(first));
        };
        let mut merged: crate::value::SubData = (**data).clone();
        merged.env.insert_sym(
            crate::symbol::wk::multi_dispatch_candidates(),
            Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(candidates)),
                crate::value::ArrayKind::List,
            ),
        );
        Some(LexicalImport::Code(Value::sub_value(crate::gc::Gc::new(merged))))
    }

    /// Whether `value` is a dispatcher over user candidates of a built-in
    /// operator (`Interpreter::with_builtin_operator_fallback`).
    // Cost: O(1).
    pub(crate) fn is_builtin_operator_dispatcher(value: &Value) -> bool {
        matches!(value.view(), ValueView::Sub(data)
            if data.env.get_sym(crate::symbol::wk::multi_dispatch_builtin_fallback()).is_some())
    }

    /// The routine `name` is bound to in the first of the import sets
    /// `chain` (innermost first) that binds it.
    // Cost: O(c), c = enclosing `use`-blocks (usually 1).
    pub(crate) fn lexical_import_binding(
        &self,
        chain: &[Symbol],
        name: Symbol,
    ) -> Option<LexicalImport> {
        if self.module.builtin_op_fallback_depth > 0 && name.with_str(|n| n.contains(":<")) {
            return None;
        }
        chain
            .iter()
            .find_map(|id| self.module.lexical_import_sets.get(id)?.get(&name).cloned())
    }
}

/// Whether `name` is a built-in operator (`prefix:<->`, `infix:<+>`) that a
/// user `multi` adds candidates to. Its dispatcher value falls back to the
/// built-in (`with_builtin_operator_fallback`): without that, a candidate's
/// own `-$a.v` on an Int found no match once the family was bound in the
/// block's set (ADR-8751 stage 2).
// Cost: O(m), m = bytes of `name`.
fn extends_builtin_operator(name: &str) -> bool {
    name.contains(":<")
        && (crate::lexical_builtin_op::binds_lexically(name)
            || !Interpreter::lexically_bindable_operator(name))
}

/// Whether a proto body only dispatches: it is `{*}` or empty. Such a proto
/// adds nothing to a call its candidates answer: mutsu checks an only-star
/// proto's signature only for literal arguments (`call_proto_function`), so
/// a captured-candidate dispatcher does not run it (ADR-8751 stage 2).
// Cost: O(s), s = body statements.
pub(crate) fn proto_body_only_dispatches(body: &[crate::ast::Stmt]) -> bool {
    use crate::ast::{Expr, Stmt};
    let significant: Vec<&Stmt> = body.iter().filter(|s| !s.is_marker()).collect();
    match significant.as_slice() {
        [] => true,
        [Stmt::Expr(expr)] => matches!(expr, Expr::Whatever) || expr.is_onlystar_dispatch(),
        _ => false,
    }
}
