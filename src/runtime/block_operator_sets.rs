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
