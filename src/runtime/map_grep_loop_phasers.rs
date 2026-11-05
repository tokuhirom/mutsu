//! The loop phasers of a `.map` / `.grep` block (#14299).
//!
//! A `.map` or `.grep` iterates its block the way a `for` loop iterates its
//! body, so the block's `FIRST` phaser runs before the first call and a map
//! block's `LAST` runs after the last one. Rakudo runs no `LAST` for a
//! `.grep` block. The inline loops compile the block's body on its own, where
//! a loop phaser statement compiles to nothing, so each loop runs the phaser
//! bodies itself through [`Interpreter::run_block_loop_phasers`].

use super::*;
use crate::ast::{PhaserKind, Stmt};

impl Interpreter {
    /// Run each top-level `kind` phaser of the block `data`, compiled on its
    /// own, against the environment the calling loop has entered: the block's
    /// captured lexicals are in scope and a phaser's writes to them persist.
    /// The phaser's captured-outer writes are recorded so the call site drains
    /// them into the caller's local slots, as the loop body's are.
    // Cost: O(s) to scan the block's s top-level statements, plus compiling
    // and running each matching phaser body.
    pub(crate) fn run_block_loop_phasers(
        &mut self,
        data: &crate::value::SubData,
        kind: PhaserKind,
    ) -> Result<(), RuntimeError> {
        for stmt in data.body.iter() {
            let Stmt::Phaser {
                kind: phaser_kind,
                body,
                ..
            } = stmt
            else {
                continue;
            };
            if *phaser_kind != kind {
                continue;
            }
            let (code, fns) = crate::compiler::Compiler::new().compile(body);
            self.run_reuse(&code, &fns)?;
            self.record_eager_block_free_var_writeback(&code, &[]);
        }
        Ok(())
    }

    /// Run each top-level `kind` phaser of the signature-bearing block `data`
    /// as a one-shot block called with `args`. A loop that binds through the
    /// full binder restores the call-local environment after every call, so
    /// the phaser binds the block's signature itself (a destructuring
    /// sub-signature included) to the chunk it belongs to: the first for
    /// `FIRST`, the final one for `LAST`. Captured lexicals keep their closure
    /// semantics.
    // Cost: O(s) to scan the block's s top-level statements, plus one call
    // per matching phaser.
    pub(crate) fn call_block_loop_phasers(
        &mut self,
        data: &crate::value::SubData,
        kind: PhaserKind,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        for stmt in data.body.iter() {
            let Stmt::Phaser {
                kind: phaser_kind,
                body,
                ..
            } = stmt
            else {
                continue;
            };
            if *phaser_kind != kind {
                continue;
            }
            let mut phaser_data = data.clone();
            Self::compile_phaser_body(&mut phaser_data, body.clone());
            let phaser = Value::sub_value(crate::gc::Gc::new(phaser_data));
            self.call_sub_value(phaser, args.clone())?;
        }
        Ok(())
    }

    /// The `FIRST` phasers of a `.map` / `.grep` callback `func`, run before
    /// its first call on `first`, the first source element, by a loop that
    /// calls the callback one element at a time (a lazy pipe). A callback that
    /// is not a block has none.
    // Cost: as [`Self::call_block_loop_phasers`]; O(1) for a non-block.
    pub(crate) fn run_callback_first_phasers(
        &mut self,
        func: &Value,
        first: &Value,
    ) -> Result<(), RuntimeError> {
        match func.view() {
            ValueView::Sub(data) => {
                self.call_block_loop_phasers(&data, PhaserKind::First, vec![first.clone()])
            }
            _ => Ok(()),
        }
    }
}
