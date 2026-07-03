//! GC Level 1a root enumeration (ADR-0001 / ADR-0002,
//! `docs/gc-level1-detailed-design.md` §2.2 / §11 step 1).
//!
//! [`Interpreter::visit_roots`] is the single place that walks every `Value`
//! this interpreter's own execution/dispatch state can directly reach. It
//! exists ahead of any `Gc<T>` / candidate-buffer machinery (those land in
//! later steps of the design doc's implementation order) so root enumeration
//! is never duplicated at a future collector call site.
//!
//! Deliberately out of scope for this pass (see the design doc for why each is
//! sequenced later):
//! - The process-global supply/async registries (`supplier_state_map`,
//!   `promise_combinator_map`, `supply_taps_map`, ...) — §11 step 7, a
//!   separate root-visitor pass mirroring the first/second/third-wave split
//!   for GC-managed node *types* themselves.
//! - `LazyList` internals (`env`/`cache`/coroutine/lazy-pipe state) — §11 step
//!   10 (third wave). A root that merely *points at* a `LazyList` (e.g.
//!   `ForLoopResumeState::LazyGather`) is noted but not traced through yet.
//! - `Value`s embedded in compiled bytecode constant pools (`CompiledCode`) —
//!   these are compile-time literals, not part of the mutable runtime object
//!   graph that can form a reference cycle.

use crate::gc::{RootVisitor, visit_map_values, visit_opt, visit_slice};
use crate::value::ForLoopResumeState;

use super::Interpreter;

impl Interpreter {
    /// Visit every `Value` reachable from this interpreter's own root set.
    ///
    /// Only exercised by this module's tests for now (GC Level 1a step 1);
    /// becomes a live production caller once the collector (step 4) lands.
    #[allow(dead_code)]
    pub(crate) fn visit_roots(&self, visitor: &mut dyn RootVisitor) {
        self.visit_vm_registers(visitor);
        self.visit_lexical_envs(visitor);
        self.visit_dispatch_state(visitor);
        self.visit_persistent_caches(visitor);
    }

    /// Former `VM` struct fields (CP-3 collapse) — the live bytecode
    /// execution registers.
    fn visit_vm_registers(&self, visitor: &mut dyn RootVisitor) {
        visit_slice(visitor, &self.stack);
        visit_slice(visitor, &self.locals);
        for v in &self.upvalues {
            visit_opt(visitor, v);
        }
        visit_opt(visitor, &self.last_topic_value);
        visit_slice(visitor, &self.topic_save_stack);
        if let Some((_, v, _)) = &self.element_source {
            visitor.visit_value(v);
        }
        for (_, v) in &self.for_param_restore_stack {
            visit_opt(visitor, v);
        }
        for frame in &self.call_frames {
            frame.saved_env.visit_values(visitor);
            visit_slice(visitor, &frame.saved_locals);
            for v in &frame.saved_upvalues {
                visit_opt(visitor, v);
            }
        }
        visit_opt(visitor, &self.last_value);
        for (_, v) in &self.pending_local_updates {
            visitor.visit_value(v);
        }
        visit_slice(visitor, &self.enter_result_stack);
        visit_opt(visitor, &self.rw_map_topic_capture);
        visit_opt(visitor, &self.action_made);
        visit_opt(visitor, &self.current_grammar_actions);
        // ForLoopResumeState::LazyGather holds an `Arc<LazyList>`, not a bare
        // `Value` — LazyList's internal Value graph is traced starting in the
        // third wave (design doc §11 step 10); revisit once
        // `LazyList::visit_gc_children` exists.
        if let Some(ForLoopResumeState::List { items, .. }) = &self.gather_for_loop_resume {
            visit_slice(visitor, items);
        }
    }

    /// Lexical environments: the live `env`, saved snapshots on assorted
    /// stacks, and closure-capture overrides.
    fn visit_lexical_envs(&self, visitor: &mut dyn RootVisitor) {
        self.env.visit_values(visitor);
        for (_, env) in &self.end_phasers {
            env.visit_values(visitor);
        }
        for env in self.closure_env_overrides.values() {
            env.visit_values(visitor);
        }
        for env in &self.caller_env_stack {
            env.visit_values(visitor);
        }
        for map in &self.loop_local_saved_env {
            visit_map_values(visitor, map);
        }
        for vec in &self.outer_scope_locals {
            visit_slice(visitor, vec);
        }
    }

    /// Multi/method dispatch stacks and wrap chains — all hold live invocants
    /// and arguments for a dispatch in progress.
    fn visit_dispatch_state(&self, visitor: &mut dyn RootVisitor) {
        for (_, _, args, _) in &self.multi_dispatch_stack {
            visit_slice(visitor, args);
        }
        for frame in &self.method_dispatch_stack {
            visitor.visit_value(&frame.invocant);
            visit_slice(visitor, &frame.args);
        }
        for (_, v) in &self.samewith_context_stack {
            visit_opt(visitor, v);
        }
        for chain in self.wrap_chains.values() {
            for (_, v) in chain {
                visitor.visit_value(v);
            }
        }
        visit_map_values(visitor, &self.wrap_name_to_sub);
        for frame in &self.wrap_dispatch_stack {
            visit_slice(visitor, &frame.remaining);
            visit_slice(visitor, &frame.args);
        }
        for chain in self.method_wrap_chains.values() {
            for (_, v) in chain {
                visitor.visit_value(v);
            }
        }
        for fallbacks in self.method_fallbacks.values() {
            for (cond, calc) in fallbacks {
                visitor.visit_value(cond);
                visitor.visit_value(calc);
            }
        }
        for (_, args, proto_ctx) in &self.proto_dispatch_stack {
            visit_slice(visitor, args);
            if let Some(ctx) = proto_ctx {
                visitor.visit_value(&ctx.invocant);
            }
        }
    }

    /// Name-keyed persistent stores and per-run caches that hold live
    /// `Value`s across statements/calls (as opposed to pure metadata).
    fn visit_persistent_caches(&self, visitor: &mut dyn RootVisitor) {
        visit_map_values(visitor, &self.why_cache);
        for inner in self.type_metadata.values() {
            visit_map_values(visitor, inner);
        }
        for vec in &self.gather_items {
            visit_slice(visitor, vec);
        }
        visit_slice(visitor, &self.block_stack);
        visit_map_values(visitor, &self.predictive_seq_iters);
        visit_opt(visitor, &self.current_distribution);
        visit_map_values(visitor, &self.package_distributions);
        for inner in self.exported_sub_values.values() {
            visit_map_values(visitor, inner);
        }
        visit_opt(visitor, &self.trait_mod_writeback_value);
        visit_map_values(visitor, &self.our_vars);
        for inner in self.package_lexicals.values() {
            visit_map_values(visitor, inner);
        }
        visit_map_values(visitor, &self.escaped_our_lexical_cells);
        visit_map_values(visitor, &self.state_vars);
        visit_map_values(visitor, &self.closure_captured_state);
        visit_map_values(visitor, &self.once_values);
        visit_map_values(visitor, &self.var_defaults);
        for (_, v, _, _) in &self.let_saves {
            visitor.visit_value(v);
        }
        for vec in &self.supply_emit_buffer {
            visit_slice(visitor, vec);
        }
        for vec in &self.supply_emit_timed_buffer {
            for (v, _) in vec {
                visitor.visit_value(v);
            }
        }
        for consumer in &self.supply_stream_consumers {
            visitor.visit_value(&consumer.consumer_cb);
        }
        // `shared_vars` is genuinely live-shared across threads (not a
        // per-thread snapshot — see `clone_for_thread`), so a `Promise`/
        // `Channel`/container stored under a shared key is reachable from
        // every interpreter that shares this `Arc`.
        if let Ok(guard) = self.shared_vars.read() {
            visit_map_values(visitor, &guard);
        }
        visit_map_values(visitor, &self.rebless_map);
        for meta in self.squish_iterator_meta.values() {
            visit_slice(visitor, &meta.source_items);
            visit_opt(visitor, &meta.as_func);
            visit_opt(visitor, &meta.with_func);
            visit_map_values(visitor, &meta.revert_values);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    struct CountingVisitor {
        count: usize,
    }

    impl RootVisitor for CountingVisitor {
        fn visit_value(&mut self, _value: &Value) {
            self.count += 1;
        }
    }

    #[test]
    fn visit_roots_finds_vm_stack_and_locals() {
        let mut interp = Interpreter::new();
        interp.stack.push(Value::Int(1));
        interp.stack.push(Value::Int(2));
        interp.locals.push(Value::Int(3));

        let mut visitor = CountingVisitor { count: 0 };
        interp.visit_roots(&mut visitor);

        assert!(
            visitor.count >= 3,
            "expected at least the 3 pushed values, saw {}",
            visitor.count
        );
    }

    #[test]
    fn visit_roots_finds_env_and_shared_vars() {
        let interp = Interpreter::new();
        interp
            .shared_vars
            .write()
            .unwrap()
            .insert("x".to_string(), Value::Int(42));

        let mut visitor = CountingVisitor { count: 0 };
        interp.visit_roots(&mut visitor);

        assert!(visitor.count >= 1, "expected to see the shared_vars entry");
    }

    #[test]
    fn visit_roots_does_not_panic_on_a_freshly_run_program() {
        let mut interp = Interpreter::new();
        interp
            .run("my @a = 1, 2, 3; my %h = a => 1; say @a.elems;")
            .unwrap();

        let mut visitor = CountingVisitor { count: 0 };
        interp.visit_roots(&mut visitor);
        // No assertion on the exact count — this just proves the traversal
        // is sound (no panics/deadlocks) against real post-execution state.
    }
}
