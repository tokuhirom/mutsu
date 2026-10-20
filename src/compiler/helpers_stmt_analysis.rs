use super::*;

use crate::runtime::lexical_imports::EVAL_UNIT_MARK;

impl Compiler {
    /// Whether `stmts` declares a `state` variable at its OWN statement level.
    ///
    /// Deliberately shallow: a `state` inside a nested loop, `if` branch or bare
    /// block belongs to that construct's clone and is reset at ITS entry (see
    /// `OpCode::ResetStateLocals` / `reset_state_locals_in_range`), so descending
    /// would only make this block emit a redundant reset. Drives whether an
    /// inline nested block needs a `ResetStateLocals` at all, so the common
    /// state-free `if` keeps its current bytecode. A statement modifier opens no
    /// block, so `state $n = 0 if 1` declares at this level.
    // Cost: O(n), n = size of the part of `stmts` in the block's own scope.
    pub(super) fn stmts_declare_state(stmts: &[Stmt]) -> bool {
        super::body_scans::stmts_declare_state(stmts)
    }

    /// Emit a [`OpCode::ResetStateLocals`] for an inline nested block body that
    /// declares `state` at its own level, returning the index to patch once the
    /// body is compiled. `None` (nothing emitted) otherwise.
    pub(super) fn emit_nested_block_state_reset(&mut self, stmts: &[Stmt]) -> Option<usize> {
        Self::stmts_declare_state(stmts)
            .then(|| self.code.emit(OpCode::ResetStateLocals { body_end: 0 }))
    }

    /// Whether a loop body consists of exactly one source `{ ... }` block
    /// (the statement-modifier form `{ ... } for @xs` parses that way, with
    /// only `SetLine` markers beside it). That block IS the loop's body — the
    /// loop statement clones it once and its iterations share the clone, so
    /// its `state` must persist across iterations (raku: `{ state $n = 0;
    /// $n = $n + 1; say $n } for 1..3` prints 1 2 3). The compile sites set
    /// [`Compiler::suppress_loop_block_state_reset`] from this so the block's
    /// per-execution `ResetStateLocals` is skipped; the loop-entry reset
    /// already restarts the state when the loop STATEMENT re-executes.
    pub(super) fn loop_body_is_sole_block(body: &[Stmt]) -> bool {
        let mut semantic = body.iter().filter(|s| !s.is_marker());
        matches!(
            (semantic.next(), semantic.next()),
            (Some(Stmt::Block(_)), None)
        )
    }

    /// [`Self::emit_nested_block_state_reset`] for an `if`/`unless` branch: a
    /// postfix statement MODIFIER introduces no block, so the statement it gates
    /// belongs to the enclosing block and its `state` must not restart
    /// (`sub f { state $n = 0 if 1; ++$n }` counts across calls).
    pub(super) fn emit_branch_state_reset(
        &mut self,
        stmts: &[Stmt],
        is_statement_modifier: bool,
    ) -> Option<usize> {
        (!is_statement_modifier)
            .then(|| self.emit_nested_block_state_reset(stmts))
            .flatten()
    }

    /// Patch the [`OpCode::ResetStateLocals`] emitted by
    /// [`Self::emit_nested_block_state_reset`] to end at the current position.
    pub(super) fn patch_nested_block_state_reset(&mut self, idx: Option<usize>) {
        if let Some(idx) = idx {
            self.code.patch_reset_state_locals_end(idx);
        }
    }

    /// Check if a statement list contains `let` or `temp` saves this block's
    /// save frame must resolve (not inside closures, routines or loop bodies,
    /// which own a frame of their own).
    ///
    /// This is what decides whether a block gets an `OpCode::LetBlock` save
    /// frame, so every position a `let` can hide in has to be seen — a
    /// declaration's or an assignment's initializer (`my $seen = $( let $a =
    /// 23; $a )`, GH-7645), a ternary arm, an operand, a `given`/`when` body.
    /// Missing one does not merely defer the resolution to the enclosing
    /// block: nothing resolves the save at all and the speculative value
    /// becomes permanent. The walk is `body_scans::has_let` (ADR-0137).
    // Cost: O(n), n = size of `stmts` outside nested code objects.
    pub(super) fn has_let_deep(stmts: &[Stmt]) -> bool {
        super::body_scans::has_let(stmts, false)
    }

    /// Check if a statement list contains actual `let` (not `temp`) statements.
    /// Used to decide whether the block's return value matters for save/restore.
    // Cost: O(n), n = size of `stmts` outside nested code objects.
    pub(super) fn has_real_let_deep(stmts: &[Stmt]) -> bool {
        super::body_scans::has_let(stmts, true)
    }

    /// Compile `body` inside an `OpCode::ImportScope` region when `stmts` (the
    /// block it compiles) directly contains a `use`/`no`/`import`. An import
    /// is lexical to its block, and a module's EXPORT may attach a LEAVE
    /// phaser to that block (`runtime::attach_target`); the region is what
    /// closes both on every exit. Blocks that the compiler inlines (a tail
    /// block, a loop body) have no other scope opcode to hang this on.
    pub(super) fn with_import_scope_region(
        &mut self,
        stmts: &[Stmt],
        body: impl FnOnce(&mut Self),
    ) {
        let idx =
            Self::has_use_stmt(stmts).then(|| self.code.emit(OpCode::ImportScope { body_end: 0 }));
        let chain_mark = self.enter_lexical_use_block(stmts);
        body(self);
        self.lexical_import_chain.truncate(chain_mark);
        if let Some(idx) = idx {
            self.code.patch_import_scope_end(idx);
        }
    }

    /// Open the lexical import sets of the `use`s `stmts` (a block body)
    /// holds: every call site the block encloses sees them, one before the
    /// `use` included (ADR-8751). Only a block below the unit's top level
    /// comes here -- the unit's own statements are not a block region -- so a
    /// top-level import stays the whole unit's, on the registry. Returns the
    /// mark to truncate `lexical_import_chain` back to on exit.
    // Cost: O(s), s = statements of the block.
    pub(super) fn enter_lexical_use_block(&mut self, stmts: &[Stmt]) -> usize {
        let mark = self.lexical_import_chain.len();
        let mut local_multis: Option<Vec<String>> = None;
        for stmt in stmts {
            if let Some(id) = Self::lexical_use_set_id(stmt) {
                let local = local_multis.get_or_insert_with(|| Self::block_multi_names(stmts));
                let id = Self::with_block_multis(id, local);
                if !self.lexical_import_chain.contains(&id) {
                    self.lexical_import_chain.push(id);
                }
            }
        }
        mark
    }

    /// The `multi`/`proto` routine names `stmts` (a block body) declares
    /// directly, sorted. Operators are left out: an operator family keeps
    /// its registry alias anyway.
    // Cost: O(s log s), s = statements of the block.
    fn block_multi_names(stmts: &[Stmt]) -> Vec<String> {
        let mut names: Vec<String> = stmts
            .iter()
            .filter_map(|stmt| match stmt {
                Stmt::SubDecl {
                    name, multi: true, ..
                }
                | Stmt::ProtoDecl { name, .. } => Some(name.resolve()),
                _ => None,
            })
            .filter(|name| !name.contains(":<"))
            .collect();
        names.sort();
        names.dedup();
        names
    }

    /// The lexical import set id of a `use` in a block that declares the
    /// multi families `local` of its own: the base id plus those names, which
    /// the import must merge with rather than hide (`{ use M; multi f($x)
    /// {...} }` dispatches over M's and the block's candidates of `f`), so
    /// such a family keeps the registry path (ADR-8751 stage 2).
    // Cost: O(n), n = length of the names.
    fn with_block_multis(id: Symbol, local: &[String]) -> Symbol {
        if local.is_empty() {
            return id;
        }
        Symbol::intern(&format!(
            "{}{}{}",
            id.resolve(),
            crate::runtime::lexical_imports::BLOCK_MULTIS_MARK,
            local.join(",")
        ))
    }

    /// The lexical import set the `use` statement `stmt` fills: its id as
    /// [`Self::enter_lexical_use_block`] opened it for the innermost block.
    // Cost: O(c + n), c = open import sets, n = length of an id.
    pub(super) fn open_lexical_use_set(&self, stmt: &Stmt) -> Option<Symbol> {
        let full = Self::lexical_use_set_id(stmt)?;
        // An `EVAL` unit's sets leave the variables out
        // (`enter_eval_unit_lexical_uses`).
        [full, Self::eval_unit_set_id(full)]
            .into_iter()
            .find_map(|base| {
                let base_text = base.resolve();
                self.lexical_import_chain.iter().rev().copied().find(|id| {
                    *id == base
                        || id
                            .resolve()
                            .strip_prefix(base_text.as_str())
                            .is_some_and(|rest| {
                                rest.starts_with(crate::runtime::lexical_imports::BLOCK_MULTIS_MARK)
                            })
                })
            })
    }

    /// The lexical import set id of `stmt` when it is a `use` that binds
    /// routines lexically in its block (ADR-8751 stage 1): a module `use`
    /// with no `:if` condition. The id names the import list, not the
    /// statement, so a recompile of the block finds the same set and every
    /// `use` importing the same list shares it.
    // Cost: O(n), n = size of the `use`'s argument expression.
    pub(super) fn lexical_use_set_id(stmt: &Stmt) -> Option<Symbol> {
        let Stmt::Use {
            module,
            tags,
            arg,
            condition: None,
            ..
        } = stmt
        else {
            return None;
        };
        super::begin_use::preloadable_module_name(module, tags, None, None)?;
        if tags.iter().any(|t| t == "from") {
            return None;
        }
        let vars = if arg.is_none() {
            Self::block_imported_vars_id_suffix(module, tags)
        } else {
            String::new()
        };
        let arg = arg.as_ref().map(|a| format!("{a:?}")).unwrap_or_default();
        Some(Symbol::intern(&format!(
            "{module}\u{1}{}\u{1}{arg}{vars}",
            tags.join(",")
        )))
    }

    /// Start this recompile of a block's body (a `.map`/`gather` callback
    /// compiled again from its AST) inside the import sets its original
    /// compile `origin` was under (ADR-8751): the chain every call site of
    /// `origin` shares, its outermost sets. A `use` inside the body opens
    /// its own again as the body compiles.
    // Cost: O(s * c), s = call-site chains of `origin`, c = sets per chain.
    pub(crate) fn seed_lexical_import_chain_from(&mut self, origin: &crate::opcode::CompiledCode) {
        let mut chains = origin.lexical_import_chains.iter();
        let Some(first) = chains.next() else {
            return;
        };
        // Innermost first, so the shared outer sets are a common suffix.
        let mut common: &[Symbol] = first;
        for chain in chains {
            let shared = common
                .iter()
                .rev()
                .zip(chain.iter().rev())
                .take_while(|(a, b)| a == b)
                .count();
            common = &common[common.len() - shared..];
        }
        self.seed_lexical_import_chain(common.to_vec());
    }

    /// Open the import sets of the top-level `use`s of an `EVAL`'d unit
    /// `stmts` for the whole unit (ADR-8751): what such a `use` imports is
    /// lexical to the EVAL, so it binds as a block's `use` does.
    // Cost: O(s), s = top-level statements.
    ///
    /// Its sets bind routines, multis and operators only: an `EVAL`'s
    /// top-level declarations are not a pad of their own in mutsu (a `my`
    /// there reaches the caller's later `EVAL`s), so the variables, constants
    /// and types it imports keep the env alias that `with_eval_import_scope`
    /// rolls back. The set ids leave out their `BLOCK_VARS_MARK` section,
    /// which tells `Stmt::Use`'s compile to declare none, and carry
    /// `EVAL_UNIT_MARK`: a block's `use` of the same import list fills a set
    /// of its own, which an `EVAL`'s rolled-back import must not stand in for
    /// (`lexical_import_replay`).
    pub(crate) fn enter_eval_unit_lexical_uses(&mut self, stmts: &[Stmt]) {
        let mut local_multis: Option<Vec<String>> = None;
        for stmt in stmts {
            if let Some(id) = Self::lexical_use_set_id(stmt) {
                let id = Self::eval_unit_set_id(id);
                let local = local_multis.get_or_insert_with(|| Self::block_multi_names(stmts));
                let id = Self::with_block_multis(id, local);
                if !self.lexical_import_chain.contains(&id) {
                    self.lexical_import_chain.push(id);
                }
            }
        }
    }

    /// The id of the `EVAL`-unit set for the block set id `id`: without the
    /// variables section it names (`BLOCK_VARS_MARK`), marked
    /// `EVAL_UNIT_MARK`.
    // Cost: O(n), n = length of `id`.
    fn eval_unit_set_id(id: Symbol) -> Symbol {
        let text = id.resolve();
        let base = text
            .split_once(crate::runtime::lexical_imports::BLOCK_VARS_MARK)
            .map_or(text.as_str(), |(base, _)| base);
        Symbol::intern(&format!("{base}{EVAL_UNIT_MARK}"))
    }

    /// Start this (EVAL) unit inside the import sets `innermost_first` of the
    /// blocks running around its `EVAL` call (ADR-8751).
    pub(crate) fn seed_lexical_import_chain(&mut self, innermost_first: Vec<Symbol>) {
        self.lexical_import_chain = innermost_first.into_iter().rev().collect();
    }

    /// The `LexImportCall` chain index for a bareword call of `name` compiled
    /// here, or `None` when no enclosing block `use`s a module or the name
    /// cannot be an imported routine's.
    // Cost: O(c), c = enclosing `use`-blocks.
    /// Mark this code as naming lexicals by string when a routine it declares
    /// does (`sub inner { ::('$x') }` in a closure body): the routine is not
    /// linked to the code it is declared in, so it resolves such a name
    /// through this code's frame, and that frame's capture must hold every
    /// lexical its creation site sees, as cells (#13595). Run before
    /// `compute_needs_env_sync`, which reads the flag.
    // Cost: O(r), r = routines compiled in this body (its own compiler's
    // `compiled_functions` or its code's `compiled_fns`, which hold nothing
    // from outside it).
    pub(super) fn note_routines_naming_lexicals(&mut self) {
        // A closure body hands its routines to its code (`compiled_fns`)
        // before this runs; a sub body still holds them.
        let names_lexicals = self
            .compiled_functions
            .values()
            .chain(self.code.compiled_fns.iter().flat_map(|fns| fns.values()))
            .any(|cf| cf.code.static_name_lookup);
        if names_lexicals {
            self.code.static_name_lookup = true;
        }
    }

    /// Emit an `OpCode::LexImportInfix` prefix for the binary `InfixFunc` over
    /// operator `op` the caller emits next, when an enclosing block's `use`
    /// may have imported `infix:<op>` (ADR-8751 stage 2). Every path that
    /// emits a binary `InfixFunc` calls this -- the RakuAST frontend lowers a
    /// user operator to an `Expr::Binary`, not an `Expr::InfixFunc`.
    // Cost: O(c + m), c = enclosing `use`-blocks, m = bytes of `op`.
    pub(super) fn emit_lex_import_infix(&mut self, op: &str) {
        let name = Symbol::intern(&format!("infix:<{op}>"));
        if let Some(chain_idx) = self.lexical_import_chain_idx(name) {
            let name_idx = self.code.add_constant(Value::str(name.resolve()));
            self.code.emit(OpCode::LexImportInfix {
                chain_idx,
                name_idx,
            });
        }
    }

    /// Emit an `OpCode::LexImportBuiltinOp` prefix for the binary `opcode`
    /// the caller emits next, when an enclosing block's `use` may have
    /// imported user candidates of the built-in operator it applies
    /// (ADR-8751 stage 2, `crate::lexical_builtin_op`).
    // Cost: O(c), c = enclosing `use`-blocks; O(1) outside every `use`-block.
    pub(super) fn emit_lex_import_builtin_op(&mut self, opcode: &OpCode) {
        if self.lexical_import_chain.is_empty() {
            return;
        }
        if let Some(name) = crate::lexical_builtin_op::infix_of_opcode(opcode) {
            self.emit_lex_import_builtin_named(name);
        } else if let Some(name) = crate::lexical_builtin_op::relation_of_opcode(opcode)
            .or_else(|| crate::lexical_builtin_op::replaced_infix_of_opcode(opcode))
        {
            // A set relation's opcode, and every other op of
            // `crate::lexical_builtin_op::is_replaced_infix`, asks no user
            // candidate: an imported one replaces the op through the call
            // prefix, which skips it on a hit.
            let sym = Symbol::intern(name);
            if let Some(chain_idx) = self.lexical_import_chain_idx(sym) {
                let name_idx = self.code.add_constant(Value::str(name.to_string()));
                self.code.emit(OpCode::LexImportInfix {
                    chain_idx,
                    name_idx,
                });
            }
        }
    }

    /// [`Self::emit_lex_import_builtin_op`] for the compound assignment over
    /// `op` the caller emits next.
    // Cost: as `emit_lex_import_builtin_op`.
    pub(super) fn emit_lex_import_builtin_compound(&mut self, op: crate::opcode::CompoundBaseOp) {
        if self.lexical_import_chain.is_empty() {
            return;
        }
        if let Some(name) = crate::lexical_builtin_op::infix_of_compound(op) {
            self.emit_lex_import_builtin_named(name);
        }
    }

    /// [`Self::emit_lex_import_builtin_op`] for the subscript op
    /// (`OpCode::Index`) the caller emits next: its user candidates are
    /// `postcircumfix:<[ ]>`'s or `postcircumfix:<{ }>`'s.
    // Cost: as `emit_lex_import_builtin_op`.
    pub(super) fn emit_lex_import_subscript(&mut self, is_positional: bool) {
        if self.lexical_import_chain.is_empty() {
            return;
        }
        self.emit_lex_import_builtin_named(crate::lexical_builtin_op::postcircumfix_of_subscript(
            is_positional,
        ));
    }

    // Cost: O(c + m), c = enclosing `use`-blocks, m = bytes of `name`.
    fn emit_lex_import_builtin_named(&mut self, name: &str) {
        let sym = Symbol::intern(name);
        if let Some(chain_idx) = self.lexical_import_chain_idx(sym) {
            let name_idx = self.code.add_constant(Value::str(name.to_string()));
            self.code.emit(OpCode::LexImportBuiltinOp {
                chain_idx,
                name_idx,
            });
        }
    }

    /// Emit an `OpCode::LexImportMeta` prefix for the reduction or hyper over
    /// user operator `op` the caller emits next, when an enclosing block's
    /// `use` may have imported it (ADR-8751 stage 2). A built-in operator
    /// whose user candidates bind lexically gets `LexImportBuiltinOp`
    /// instead; any other built-in operator gets none: its opcode finds user
    /// candidates in the registry.
    // Cost: O(c + m), c = enclosing `use`-blocks, m = bytes of `op`.
    pub(super) fn emit_lex_import_meta(&mut self, op: Symbol) {
        let op = op.as_str();
        if op.is_empty() || self.lexical_import_chain.is_empty() {
            return;
        }
        // A set operator runs the bound dispatcher, which falls back to the
        // built-in (`crate::lexical_builtin_op::meta_runs_bound_dispatcher`).
        if crate::compiled_operator::is_builtin_infix(op)
            && !crate::lexical_builtin_op::meta_runs_bound_dispatcher(op)
        {
            if let Some(name) = crate::lexical_builtin_op::infix_of_meta(op) {
                self.emit_lex_import_builtin_named(&name);
            }
            return;
        }
        let name = Symbol::intern(&format!("infix:<{op}>"));
        if let Some(chain_idx) = self.lexical_import_chain_idx(name) {
            let name_idx = self.code.add_constant(Value::str(name.resolve()));
            self.code.emit(OpCode::LexImportMeta {
                chain_idx,
                name_idx,
            });
        }
    }

    /// Emit an `OpCode::LexImportBuiltinOp` prefix for the `X`/`Z`/`R`
    /// meta-op over the built-in `op` the caller emits next, when its user
    /// candidates bind lexically (ADR-8751 stage 2). A meta-op over a user
    /// operator does not parse yet (#13591).
    // Cost: O(c + m), c = enclosing `use`-blocks, m = bytes of `op`; O(1)
    // outside every `use`-block.
    pub(super) fn emit_lex_import_builtin_meta(&mut self, op: &str) {
        if self.lexical_import_chain.is_empty() {
            return;
        }
        if let Some(name) = crate::lexical_builtin_op::infix_of_meta(op) {
            self.emit_lex_import_builtin_named(&name);
        }
    }

    pub(super) fn lexical_import_chain_idx(&mut self, name: Symbol) -> Option<u32> {
        if self.lexical_import_chain.is_empty()
            || crate::qualified::is_qualified(name)
            || name.resolve().starts_with("__mutsu")
        {
            return None;
        }
        let chain: Vec<Symbol> = self.lexical_import_chain.iter().rev().copied().collect();
        let chains = &mut self.code.lexical_import_chains;
        if let Some(i) = chains.iter().position(|c| *c == chain) {
            return Some(i as u32);
        }
        chains.push(chain);
        Some((chains.len() - 1) as u32)
    }

    /// Check if a block directly contains a `use`/`need`/`require`/`import`/`no`
    /// statement (non-recursive). A `need` or `require` merges a module into the
    /// block too (ADR-11136).
    pub(super) fn has_use_stmt(stmts: &[Stmt]) -> bool {
        stmts.iter().any(Self::opens_import_scope)
    }

    /// Whether `stmt` makes the block holding it open a run-time import scope
    /// (`OpCode::ImportScope`): a `use`, `need`, `import`, `no` or `require`.
    pub(super) fn opens_import_scope(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Use { .. } | Stmt::Need { .. } | Stmt::Import { .. } | Stmt::No { .. } => true,
            Stmt::Expr(crate::ast::Expr::Call { name, .. }) => *name == "require",
            _ => false,
        }
    }

    /// The declaration inside the bind source of `$target := my $z = ...`,
    /// with the variable it declares, when both names are plain scalar
    /// lexicals. Such a bind aliases `$target` to `$z`'s container, so the
    /// caller compiles the declaration and then binds `$z` as a variable
    /// (#9308).
    pub(super) fn scalar_decl_bind_source<'a>(
        target: &str,
        expr: &'a Expr,
    ) -> Option<(&'a Stmt, &'a str)> {
        let Expr::DoStmt(stmt) = expr else {
            return None;
        };
        let Stmt::VarDecl { name, .. } = stmt.as_ref() else {
            return None;
        };
        (Self::is_plain_lexical_name(target) && Self::is_plain_lexical_name(name))
            .then_some((stmt.as_ref(), name.as_str()))
    }

    pub(super) fn next_tmp_name(&mut self, prefix: &str) -> String {
        let name = format!("${}{}", prefix, self.tmp_counter);
        self.tmp_counter += 1;
        name
    }
}
