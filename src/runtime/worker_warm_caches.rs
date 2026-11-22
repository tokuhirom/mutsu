//! A pool worker hands its last task's warm caches to its next task
//! (ADR-17337).
//!
//! Every task a worker runs gets an interpreter forked from its spawner
//! (`clone_for_thread`, ADR-0020's per-task contract), and that fork starts
//! with empty resolution caches. A task that runs the same code as the task
//! before it then re-derives every answer the previous one already had --
//! method resolution, constructor plans, MROs, multi candidates, compiled
//! call targets. ParaSeq's `BlockMapper.run` paid that once per batch, which
//! kept every batch near a millisecond and its adaptive batch size at the
//! floor (#17337).
//!
//! Those caches are memos over the declaration [`Registry`]: within one
//! interpreter they are already required to stay correct across any amount
//! of execution, because the program state they do not key on (env, frames,
//! the current package) changes all the time. What they cannot survive is a
//! different registry. So the handoff is gated on registry (and routine-table) identity:
//!
//! - after a task, the worker keeps its caches only if the task left the
//!   registry it was forked with untouched -- the very `Arc` the fork
//!   shared with its spawner (any write copies it: the snapshot kept here
//!   holds a second reference, so `Arc::make_mut` can never write in place);
//! - before a task, the fresh fork adopts them only if it was forked with
//!   that same `Arc`, i.e. the spawner has not written its registry since.
//!
//! Under that gate the two interpreters have one registry, so every answer
//! the caches hold is an answer the new task would compute. A write
//! generation travels with the caches: the previous task's generation names
//! that registry, and the previous task's interpreter is gone, so no other
//! interpreter will ever count on from it.

use std::cell::RefCell;
use std::sync::Arc;
use std::sync::atomic::Ordering;

use crate::runtime::Interpreter;
use crate::runtime::registry::Registry;
use crate::runtime::resolution_caches::ResolutionCaches;

/// What a task leaves for the next task on the same worker thread.
struct WarmCaches {
    /// The registry and routine tables the task was forked with and left
    /// untouched.
    registry: Arc<Registry>,
    routines: Arc<RoutineTables>,
    /// Its registry write generation at the end.
    registry_write_gen: u64,
    caches: ResolutionCaches,
    attr_type_constraint_cache: RefCell<crate::vm::AttrTypeConstraintCache>,
    class_does_role_memo: RefCell<crate::runtime::type_state::ClassDoesRoleMemo>,
    subset_predicate_cache: std::collections::HashMap<String, super::SubsetPredicateCompiled>,
}

thread_local! {
    /// One per worker thread: a task runs on one thread from start to end.
    static WARM: RefCell<Option<WarmCaches>> = const { RefCell::new(None) };
}

/// The registry a task was forked with, from [`Interpreter::adopt_worker_warm_caches`]
/// to [`Interpreter::leave_worker_warm_caches`].
pub(crate) struct TaskRegistry(Arc<Registry>, Arc<RoutineTables>);

use crate::runtime::routine_tables::RoutineTables;

impl Interpreter {
    /// The registry version this interpreter currently reads.
    // Cost: O(1).
    fn registry_arc(&self) -> Arc<Registry> {
        self.types.registry.snapshot()
    }

    /// The routine-tables `Arc` this interpreter currently reads.
    // Cost: O(1), one read-lock acquisition.
    fn routines_arc(&self) -> Arc<RoutineTables> {
        crate::runtime::routine_tables::RoutinesReadGuard::new(&self.types.routines).clone_arc()
    }

    /// Run at the start of a pool task on a freshly forked interpreter: take
    /// over the caches the previous task on this worker left, when both were
    /// forked with the same registry (see the module docs).
    // Cost: O(1); the caches move, nothing is copied.
    pub(crate) fn adopt_worker_warm_caches(&mut self) -> TaskRegistry {
        let registry = self.registry_arc();
        let routines = self.routines_arc();
        if let Some(warm) = WARM.with(|w| w.borrow_mut().take())
            && Arc::ptr_eq(&warm.registry, &registry)
            && Arc::ptr_eq(&warm.routines, &routines)
        {
            self.caches = warm.caches;
            self.types.attr_type_constraint_cache = warm.attr_type_constraint_cache;
            self.types.class_does_role_memo = warm.class_does_role_memo;
            self.types.subset_predicate_cache = warm.subset_predicate_cache;
            self.types
                .registry_write_gen
                .store(warm.registry_write_gen, Ordering::Relaxed);
            // The registry version is the one the warm caches were filled
            // under (the warm handoff holds a reference to it, so a write
            // since would have copied it): reconcile with its generation.
            self.types
                .registry_world_seen
                .store(self.types.registry.generation(), Ordering::Relaxed);
        }
        TaskRegistry(registry, routines)
    }

    /// Run at the end of a pool task: leave this interpreter's caches for the
    /// next task on this worker, unless the task wrote its registry.
    // Cost: O(1); the caches move, nothing is copied.
    pub(crate) fn leave_worker_warm_caches(&mut self, forked_with: TaskRegistry) {
        let TaskRegistry(registry, routines) = forked_with;
        let untouched = Arc::ptr_eq(&self.registry_arc(), &registry)
            && Arc::ptr_eq(&self.routines_arc(), &routines);
        let warm = untouched.then(|| WarmCaches {
            registry,
            routines,
            registry_write_gen: self.registry_write_generation(),
            caches: std::mem::take(&mut self.caches),
            attr_type_constraint_cache: std::mem::take(&mut self.types.attr_type_constraint_cache),
            class_does_role_memo: std::mem::take(&mut self.types.class_does_role_memo),
            subset_predicate_cache: std::mem::take(&mut self.types.subset_predicate_cache),
        });
        // A task that wrote its registry leaves nothing: the previous
        // task's caches were moved into this one at its start.
        WARM.with(|w| *w.borrow_mut() = warm);
    }
}
