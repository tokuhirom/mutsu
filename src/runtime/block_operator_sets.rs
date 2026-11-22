//! A block's own operator declarations bind in a lexical set (#17771,
//! ADR-8751 stage 2 applied to declarations).
//!
//! `{ sub infix:<**>(\a, \b) { 42 }; 3 ** -1 }` declares the operator in the
//! block's pad: rakudo resolves each use at its call site's lexical scope, so
//! a routine written outside the block keeps the built-in `**` even when the
//! block calls it. mutsu used to register the declaration in the flat routine
//! registry and add the whole unit to the `user_declared_infix_ops` gate, so
//! every routine the block called saw it for the block's dynamic extent.
//!
//! The compiler now gives such a block a synthetic lexical import set
//! (`Compiler::enter_block_operator_set`, its id led by [`BLOCK_OPS_MARK`]),
//! open for the whole block: each use of the operator inside it gets the
//! `LexImportBuiltinOp` / `LexImportInfix` / `LexImportMeta` /
//! `LexImportCall` prefix an imported operator gets. When the declaration
//! registers, [`Interpreter::bind_block_operator`] binds it in that set as a
//! one-candidate family that shadows the built-in, exactly as a block's
//! `use` binds an imported plain `sub infix:<+>` (#17767), and the
//! registration leaves the unit gate as it was. The block also opens an
//! import region (`OpCode::ImportScope`), which holds the set while the block
//! runs: a `where` predicate or an `EVAL` compiled inside the block reads it
//! through `eval_lexical_import_chain`, as it reads the block's imports.

use super::*;

/// Leads the id of the synthetic lexical import set a block's own plain
/// operator declarations bind in (`Compiler::enter_block_operator_set`). The
/// rest of the id names the block (source file, line, statement index) and
/// lists the operators, comma-separated, after the last `\u{1}`.
pub(crate) const BLOCK_OPS_MARK: &str = "\u{1}block-ops\u{1}";

/// The operator names the block operator set `id` binds, or `None` when `id`
/// is not a block operator set.
// Cost: O(n), n = length of `id`.
pub(crate) fn block_operator_set_names(id: &str) -> Option<impl Iterator<Item = &str>> {
    let rest = id.strip_prefix(BLOCK_OPS_MARK)?;
    let (_, names) = rest.rsplit_once('\u{1}')?;
    Some(names.split(',').filter(|name| !name.is_empty()))
}

/// The import region key of the block whose operator set is `id`.
// Cost: O(n), n = length of `id`.
fn block_operator_set_region(id: &str) -> Option<&str> {
    let rest = id.strip_prefix(BLOCK_OPS_MARK)?;
    Some(rest.rsplit_once('\u{1}')?.0)
}

impl Interpreter {
    /// Bind the plain operator routine `def` the running block declares in
    /// the block's operator set `id`: a one-candidate family that shadows the
    /// built-in operator, as an imported plain operator binds
    /// (`bind_plain_builtin_operator_lexically`). The family's dispatcher
    /// captures the running frame, so the routine keeps the block's lexicals.
    // Cost: O(e), e = the running frame's env (the dispatcher captures it).
    pub(crate) fn bind_block_operator(&mut self, id: Symbol, def: std::sync::Arc<FunctionDef>) {
        let name = def.name;
        let dispatcher = self.sub_value_from_multi_candidates(&name.resolve(), vec![def]);
        let dispatcher = Self::with_proto_shadowing_core(Self::with_builtin_operator_fallback(
            dispatcher,
        ));
        self.install_block_operator(id, name, dispatcher);
    }

    /// Bind the family of the operator the running block declares the
    /// `multi` candidate `def` of in the block's operator set `id` (#18617).
    /// A block's `multi` adds to the candidates in scope around the block, so
    /// the family is the block's own candidates so far plus those of the
    /// enclosing scopes: the innermost enclosing block that binds the family,
    /// else the registry's (the unit's own and its top-level imports). A call
    /// none of them accepts falls back to the built-in.
    // Cost: O(f log f) for the registry scan, f = registered functions, plus
    // O(e + c), e = the running frame's env (the dispatcher captures it), c =
    // the family's candidates.
    pub(crate) fn bind_block_operator_candidate(
        &mut self,
        id: Symbol,
        def: std::sync::Arc<FunctionDef>,
    ) {
        let name = def.name;
        // Each run of the block declares its candidates again (and the hoist
        // pass ahead of the in-sequence one): the latest derivation of a
        // candidate replaces the earlier one.
        let fingerprint = def.body_fingerprint();
        let mut own: Vec<std::sync::Arc<FunctionDef>> = self
            .module
            .lexical_imports
            .read()
            .families
            .get(&id)
            .and_then(|families| families.get(&name).cloned())
            .unwrap_or_default();
        own.retain(|d| d.body_fingerprint() != fingerprint);
        own.push(def);
        let enclosing = self
            .active_lexical_import_chain()
            .into_iter()
            .filter(|set| *set != id)
            .find_map(|set| {
                self.module
                    .lexical_imports
                    .read()
                    .families
                    .get(&set)
                    .and_then(|families| families.get(&name).cloned())
            });
        let mut defs = enclosing.unwrap_or_else(|| {
            self.operator_family_defs(&self.current_package(), &name.resolve())
        });
        for d in &own {
            if !defs.iter().any(|e| std::sync::Arc::ptr_eq(e, d)) {
                defs.push(std::sync::Arc::clone(d));
            }
        }
        defs.sort_by_key(|d| d.decl_order);
        let dispatcher = self.sub_value_from_multi_candidates(&name.resolve(), defs.clone());
        self.note_lexical_operator_candidate_defs(&dispatcher, &defs);
        let dispatcher = Self::with_builtin_operator_fallback(dispatcher);
        self.module.lexical_imports.write(|tables| {
            tables.families.entry(id).or_default().insert(name, own);
        });
        self.install_block_operator(id, name, dispatcher);
    }

    /// The registered candidates of the operator family `name` visible from
    /// `package`, in declaration order.
    // Cost: O(f log f), f = registered functions.
    pub(crate) fn operator_family_defs(
        &self,
        package: &str,
        name: &str,
    ) -> Vec<std::sync::Arc<FunctionDef>> {
        self.routine_candidate_defs(package, name)
            .into_iter()
            .filter(|(_, is_multi)| *is_multi)
            .map(|(def, _)| def)
            .collect()
    }

    /// Bind `dispatcher` as the operator `name` in the block operator set
    /// `id`, and record the set in the block's open import region.
    // Cost: O(b), b = bindings of the set (it is copied on write).
    fn install_block_operator(&mut self, id: Symbol, name: Symbol, dispatcher: Value) {
        super::lexical_operator_import_flag::note_operator_import();
        self.module.lexical_imports.write(|tables| {
            let mut set = tables
                .sets
                .get(&id)
                .map(|set| (**set).clone())
                .unwrap_or_default();
            set.insert(name, super::lexical_imports::LexicalImport::Code(dispatcher));
            tables.sets.insert(id, std::sync::Arc::new(set));
            tables.operator_sets.insert(id);
        });
        // While the block runs, its import region holds the set, so what is
        // compiled or resolved by a run-time name inside it (a `where`
        // predicate, an `EVAL`) sees the operator, as it sees a block's
        // imports (`eval_lexical_import_chain`).
        let importer = self.executing_unit_sym_for_module_load();
        let text = id.resolve();
        let region = block_operator_set_region(&text);
        if let Some(top) = self.module.import_scope_stack.last_mut()
            && top.unit == importer
            && region.is_some_and(|region| top.region.is_some_and(|r| r.resolve() == region))
            && !top.lexical_sets.contains(&id)
        {
            top.lexical_sets.push(id);
        }
    }
}
