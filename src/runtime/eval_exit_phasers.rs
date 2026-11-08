//! The unit-level exit phasers of an `EVAL`'d unit (`LEAVE`, `KEEP`, `UNDO`).
//!
//! The main program splits them off its body (`split_block_phasers`) and runs
//! the queue its outcome selects once the mainline is done. An `EVAL`'d unit
//! is a compilation unit of its own, so its exit phasers run when the unit
//! ends, not when the process does.

use super::*;

impl Interpreter {
    /// Take the unit-level `LEAVE`/`KEEP`/`UNDO` phasers out of `stmts` and
    /// return the `(success, failure)` queues, last-written first.
    // Cost: O(n), n = statements of the unit.
    pub(super) fn take_unit_exit_phasers(stmts: &mut Vec<Stmt>) -> (Vec<Stmt>, Vec<Stmt>) {
        let is_exit = |s: &Stmt| {
            matches!(
                s,
                Stmt::Phaser {
                    kind: PhaserKind::Leave | PhaserKind::Keep | PhaserKind::Undo,
                    ..
                }
            )
        };
        if !stmts.iter().any(is_exit) {
            return (Vec::new(), Vec::new());
        }
        let (exits, rest): (Vec<Stmt>, Vec<Stmt>) = std::mem::take(stmts).into_iter().partition(is_exit);
        *stmts = rest;
        let mut success = Vec::new();
        let mut failure = Vec::new();
        for stmt in exits.into_iter().rev() {
            let Stmt::Phaser { kind, body, .. } = stmt else {
                continue;
            };
            match kind {
                PhaserKind::Leave => {
                    success.push(Stmt::Block(body.clone()));
                    failure.push(Stmt::Block(body));
                }
                PhaserKind::Keep => success.push(Stmt::Block(body)),
                _ => failure.push(Stmt::Block(body)),
            }
        }
        (success, failure)
    }

    /// Run the queue `outcome` selects, after the unit ran. A phaser's own
    /// error replaces a successful outcome.
    // Cost: O(p) calls, p = phasers of the queue.
    pub(super) fn run_unit_exit_phasers(
        &mut self,
        outcome: &mut Result<Value, RuntimeError>,
        success: &[Stmt],
        failure: &[Stmt],
    ) {
        if success.is_empty() && failure.is_empty() {
            return;
        }
        let ran_ok = match outcome {
            Ok(value) => crate::runtime::types::value_is_defined(value),
            Err(_) => false,
        };
        let queue = if ran_ok { success } else { failure };
        if let Err(e) = self.run_block_raw(queue)
            && outcome.is_ok()
        {
            *outcome = Err(e);
        }
    }
}
