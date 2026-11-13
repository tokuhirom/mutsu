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

/// A loop phaser's body compiled on its own, with the block body it belongs
/// to: the `Weak` both proves the entry still names that body (a live body
/// upgrades to the same allocation) and keeps the allocation's address from
/// being reused by another body while the entry exists.
type CompiledPhaser = std::rc::Rc<(CompiledCode, CompiledFns)>;

/// [`LOOP_PHASER_CACHE`]'s map: (block body address, statement index) to the
/// body's `Weak` and its compiled phaser.
type LoopPhaserMap =
    rustc_hash::FxHashMap<(usize, usize), (std::sync::Weak<Vec<Stmt>>, CompiledPhaser)>;

thread_local! {
    /// [`compiled_loop_phaser`]'s cache, keyed by the block body's address
    /// and the phaser statement's index in it. Per thread, so a cached entry
    /// is never shared with a run on another thread.
    static LOOP_PHASER_CACHE: std::cell::RefCell<LoopPhaserMap> =
        std::cell::RefCell::new(LoopPhaserMap::default());
}

/// The compiled form of the phaser statement `index` of `block_body` (whose
/// body is `phaser_body`). A `NEXT` phaser runs once per element of the
/// `.map`/`.grep` it belongs to, and compiling it from the AST each time cost
/// ~40k instructions per element (#17337). The compilation reads nothing
/// but the phaser's body, so one per body is every run's.
// Cost: O(1) expected after the first call per phaser; the first compiles it.
fn compiled_loop_phaser(
    block_body: &std::sync::Arc<Vec<Stmt>>,
    index: usize,
    phaser_body: &[Stmt],
) -> CompiledPhaser {
    let key = (std::sync::Arc::as_ptr(block_body) as usize, index);
    let cached = LOOP_PHASER_CACHE.with(|cache| {
        cache.borrow().get(&key).and_then(|(owner, compiled)| {
            owner
                .upgrade()
                .is_some_and(|live| std::sync::Arc::ptr_eq(&live, block_body))
                .then(|| compiled.clone())
        })
    });
    if let Some(compiled) = cached {
        return compiled;
    }
    let compiled = std::rc::Rc::new(crate::compiler::Compiler::new().compile(phaser_body));
    LOOP_PHASER_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        // Entries of bodies that are gone are dropped as they are found.
        cache.retain(|_, (owner, _)| owner.strong_count() > 0);
        cache.insert(
            key,
            (std::sync::Arc::downgrade(block_body), compiled.clone()),
        );
    });
    compiled
}

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
        for (index, stmt) in data.body.iter().enumerate() {
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
            let compiled = compiled_loop_phaser(&data.body, index, body);
            let (code, fns) = &*compiled;
            self.run_reuse(code, fns)?;
            self.record_eager_block_free_var_writeback(code, &[]);
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
