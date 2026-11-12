//! A block with both a `CATCH`/`CONTROL` handler and `ENTER`/`LEAVE`-family
//! phasers: the handler region is compiled *inside* the phaser scope.
//!
//! Rakudo runs a block's `CATCH` while the block is still live -- its
//! lexicals bound -- and its `LEAVE` phasers afterwards, on the way out. mutsu
//! used to wrap the phaser scope in the handler region instead, so an
//! exception unwinding out of the body first left the scope (running `LEAVE`
//! and restoring its locals) and only then reached the handler, which read the
//! block's `my` variables as gone: EEL's CLI returned `Nil` instead of its exit
//! code from `CATCH { default { return $phase } }` in a routine with a
//! `LEAVE`, so every configuration error exited 0.

use super::*;

/// What [`Compiler::compile_try_region`] was asked to build, carried into the
/// phaser scope it now nests inside.
pub(crate) struct PhaserTryRegion {
    pub(super) catch: Option<Vec<Stmt>>,
    pub(super) traps: bool,
    pub(super) catch_value: bool,
    pub(super) decay_failure: bool,
    pub(super) is_bare_block: bool,
}

impl Compiler {
    /// Whether `body` (a handler region's statements) also carries phasers
    /// that need a phaser block scope, with a value-producing tail rather than
    /// a trailing `ENTER` (whose entry-time value the scope itself supplies).
    // Cost: O(s), s = statements.
    pub(super) fn try_region_needs_inner_phaser_scope(body: &[Stmt]) -> bool {
        if !Self::has_block_enter_leave_phasers(body) {
            return false;
        }
        let last = crate::ast::last_value_stmt_index(body, crate::ast::TailSkip::Markers);
        !matches!(
            last.and_then(|i| body.get(i)),
            Some(Stmt::Phaser {
                kind: PhaserKind::Enter,
                ..
            })
        )
    }

    /// Compile the body section of a phaser scope as the handler region: the
    /// non-phaser statements (the `CATCH`/`CONTROL` among them) go through
    /// `compile_try_region`, which leaves exactly one value on the stack. A
    /// trailing `LEAVE`/`KEEP`/... makes the block's value `Nil`.
    // Cost: O(s) compile time, s = body statements.
    pub(super) fn compile_phaser_scope_try_body(
        &mut self,
        body_stmts: &[&Stmt],
        region: PhaserTryRegion,
        nil_tail: bool,
    ) {
        let owned: Vec<Stmt> = body_stmts.iter().map(|s| (*s).clone()).collect();
        self.next_try_decays_failure = region.decay_failure;
        self.next_try_is_bare_block = region.is_bare_block;
        self.compile_try_region(&owned, &region.catch, region.traps, region.catch_value);
        if nil_tail {
            self.code.emit(OpCode::Pop);
            self.emit_nil_value();
        }
    }
}
