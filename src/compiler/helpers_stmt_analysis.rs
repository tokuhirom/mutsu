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
        body(self);
        if let Some(idx) = idx {
            self.code.patch_import_scope_end(idx);
        }
    }

    /// Check if a block directly contains a `use`/`no` statement (non-recursive).
    pub(super) fn has_use_stmt(stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|s| matches!(s, Stmt::Use { .. } | Stmt::Import { .. } | Stmt::No { .. }))
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
