use super::*;

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
        for stmt in stmts {
            if let Some(id) = Self::lexical_use_set_id(stmt)
                && !self.lexical_import_chain.contains(&id)
            {
                self.lexical_import_chain.push(id);
            }
        }
        mark
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
        let arg = arg.as_ref().map(|a| format!("{a:?}")).unwrap_or_default();
        Some(Symbol::intern(&format!(
            "{module}\u{1}{}\u{1}{arg}",
            tags.join(",")
        )))
    }

    /// The `LexImportCall` chain index for a bareword call of `name` compiled
    /// here, or `None` when no enclosing block `use`s a module or the name
    /// cannot be an imported routine's.
    // Cost: O(c), c = enclosing `use`-blocks.
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
        stmts.iter().any(|s| match s {
            Stmt::Use { .. } | Stmt::Need { .. } | Stmt::Import { .. } | Stmt::No { .. } => true,
            Stmt::Expr(crate::ast::Expr::Call { name, .. }) => *name == "require",
            _ => false,
        })
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
