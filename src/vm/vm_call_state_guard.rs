//! RAII guards that restore interpreter call-dispatch state on drop --
//! including when a Rust panic unwinds through the guarded call, not just on
//! normal return.
//!
//! Several call-dispatch functions (`call_compiled_closure`,
//! `call_compiled_closure_with_topic`, `call_compiled_function_named_inner`)
//! temporarily mutate a piece of `Interpreter` state for the duration of a
//! call and restore it with a plain `self.field = saved;` statement near the
//! function's end. A Rust panic caught at an outer `catch_unwind` boundary
//! (`run_inner_guarded`/`run_range_guarded`) skips straight past such a
//! statement -- only `Drop` runs on unwind -- leaking the callee's value into
//! the code that resumes at the boundary. See
//! `todo/deep/panic-unwind-leaks-side-channel-call-state.md`.
//!
//! # History: two unsound designs tried before landing on this one
//!
//! **v1 (raw pointer to the whole `Interpreter`).** Earlier revisions of
//! `StateScopeGuard`/`WhenMatchedGuard`/`MarkContextGuard` captured `interp
//! as *mut Interpreter` at construction and dereferenced it back to `&mut
//! Interpreter` in `Drop`. That is unsound: the guarding function keeps
//! calling other `&mut self` methods on the SAME `Interpreter` for the rest
//! of its body (e.g. `call_compiled_method_fast` calls
//! `self.push_light_call_frame()` after constructing a `MarkContextGuard`).
//! Under Stacked Borrows, a `&mut self` call performs a "Unique" retag of the
//! callee's argument, which invalidates ("pops") any earlier `SharedReadWrite`
//! tag covering the SAME memory the guard's raw pointer was derived from.
//! `Drop` then reborrowing `&mut *self.interp` retags from a tag no longer on
//! the borrow stack: UB, caught by `cargo miri test --lib gc::soundness_smoke`
//! (root-caused 2026-08-21; introduced across the `MarkContextGuard`-adding
//! PR and the `StateScopeGuard`/`WhenMatchedGuard`-adding PR, not caught at
//! the time because CI's `miri` job only runs when the diff touches
//! `src/gc/**`/`src/value/**`, which neither introducing PR did).
//!
//! The OLD module-level "safety invariant" comment for v1 claimed the risk
//! was re-entering through a DIFFERENT `&mut Interpreter` alias. That is not
//! what Stacked Borrows actually tracks -- it tracks the linear borrow-stack
//! history of a memory location, not whether two borrows are "logically the
//! same object". A second `&mut self` call through the SAME `self` most
//! certainly invalidates an earlier raw pointer's tag too.
//!
//! **v2 (plain `Cell<T>` fields on `Interpreter`, still one raw pointer to
//! the whole struct).** The obvious-looking fix is to make each guarded field
//! a `Cell<T>` and have the guard's raw pointer, in both `new` and `Drop`,
//! reach it by direct place projection (`(*ptr).field.get()/.set()`) without
//! ever binding an intermediate `&Interpreter`/`&mut Interpreter` local. This
//! is STILL UB, verified empirically with the same `cargo miri test`: Miri's
//! retagging for `&Interpreter`/`&mut Interpreter` operates on the WHOLE
//! pointee type's byte range, not per-field -- a struct containing `Cell`
//! fields anywhere is not `Freeze` as a whole, so both (a) the `interp as
//! *const Interpreter` cast at construction and (b) a later ordinary `&mut
//! self` call elsewhere in the guarding function each retag the FULL struct
//! range uniformly (`SharedReadWrite` for (a), `Unique` for (b)) -- there is
//! no per-field carve-out that exempts an embedded `Cell`'s own bytes from a
//! later whole-struct `Unique` retag. So a raw pointer into `Interpreter`
//! itself goes stale exactly as in v1, even when the only field it ever
//! touches is a `Cell`.
//!
//! # v3 (this file): each guarded field is its own separate heap allocation
//!
//! `current_package` already had the right pattern in `runtime::accessors_stack`
//! (`CurrentPackageGuard`): its backing storage (`Arc<RwLock<String>>` /
//! `Arc<AtomicU32>`) lives in a heap allocation SEPARATE from `Interpreter`'s
//! own, reached through one level of pointer indirection. Retagging `&mut
//! Interpreter` only concerns the bytes physically embedded in
//! `Interpreter`'s own allocation (here, just the pointer bytes of the boxed
//! field) -- it is not transitive through indirection, so it never touches
//! whatever a `Box`/`Arc` field points AT. `state_scope_id`, `when_matched`,
//! and the `MarkContextGuard` flag family (`bind_context` et al.) now follow
//! the same shape: each field is `Box<Cell<T>>` on `Interpreter` (see
//! `runtime::mod::Interpreter`'s field docs), and each guard:
//!
//! - is constructed from a plain `&Interpreter` (no `&mut` needed at all --
//!   reading/writing a `Cell` only needs a shared reference), and
//! - stores one `*const Cell<T>` PER FIELD -- taken by dereferencing the
//!   `Box` (`&*interp.field as *const Cell<T>`) -- rather than a single
//!   `*const Interpreter`. Each such pointer targets the field's OWN heap
//!   allocation, entirely disjoint from `Interpreter`'s allocation, so it
//!   stays valid on its own independent borrow stack no matter how many
//!   further `&mut self` calls the guarding function makes to `Interpreter`
//!   in between -- there is nothing left for those retags to invalidate.
//!
//! The `Box` itself (i.e. the field slot on `Interpreter`) must not be
//! replaced/dropped while the guard is alive -- true at every call site: an
//! ordinary `&mut self` dispatch method that owns the `Interpreter` for its
//! entire body and never reassigns these fields. No `unsafe fn` is needed to
//! construct a guard; only the `Drop` pointer dereferences are `unsafe`, and
//! even those never form a reference to the whole `Interpreter` struct --
//! only to the individual boxed `Cell`.
//!
//! `PragmaGuard` needs no raw pointer at all: it is only used by
//! `call_compiled_closure`, a short function where every subsequent `self.*`
//! access can be rewritten to go through the guard's `Deref`/`DerefMut`
//! instead, so a plain safe `&'a mut Interpreter` borrow works.
use super::*;
use std::cell::Cell;

/// RAII guard restoring [`Interpreter::state_scope_id`] on drop. Used by
/// `call_compiled_closure_with_topic` and `call_compiled_function_named_inner`,
/// which switch it to the callee's `state` scope for the call's duration.
pub(crate) struct StateScopeGuard {
    /// Raw pointer into `state_scope_id`'s OWN `Box` allocation (see the
    /// module doc's "v3" section) -- never a pointer to `Interpreter` itself.
    cell: *const Cell<Option<u64>>,
    saved: Option<u64>,
}

impl StateScopeGuard {
    pub(crate) fn new(interp: &Interpreter, new_value: Option<u64>) -> Self {
        let saved = interp.state_scope_id.get();
        interp.state_scope_id.set(new_value);
        StateScopeGuard {
            cell: &*interp.state_scope_id as *const Cell<Option<u64>>,
            saved,
        }
    }
}

impl Drop for StateScopeGuard {
    fn drop(&mut self) {
        // SAFETY: `cell` was taken from `state_scope_id`'s own `Box`
        // allocation at construction (see module doc); that allocation
        // outlives the guard and never moves (the `Box` is not reassigned
        // while the guard is alive). This never forms a reference to
        // `Interpreter` itself, so it is unaffected by any `&mut self` calls
        // made elsewhere while the guard was alive.
        unsafe {
            (*self.cell).set(self.saved);
        }
    }
}

/// RAII guard restoring the `when_matched` flag (via
/// [`Interpreter::when_matched`] / `set_when_matched`) on drop. Used by
/// `call_compiled_function_named_inner`, which resets it to `false` for a
/// routine body so a bare `when` inside it does not leak its match state into
/// an enclosing `given`/`with`.
pub(crate) struct WhenMatchedGuard {
    /// Raw pointer into `when_matched`'s OWN `Box` allocation -- see
    /// `StateScopeGuard::cell`.
    cell: *const Cell<bool>,
    saved: bool,
}

impl WhenMatchedGuard {
    pub(crate) fn new(interp: &Interpreter, new_value: bool) -> Self {
        let saved = interp.when_matched.get();
        interp.when_matched.set(new_value);
        WhenMatchedGuard {
            cell: &*interp.when_matched as *const Cell<bool>,
            saved,
        }
    }
}

impl Drop for WhenMatchedGuard {
    fn drop(&mut self) {
        // SAFETY: see `StateScopeGuard::drop` -- same shape, same invariant.
        unsafe {
            (*self.cell).set(self.saved);
        }
    }
}

/// RAII guard restoring lexical pragma state (`use fatal`, `use strict`,
/// `use newline`, `use MONKEY-TYPING` -- see [`Interpreter::save_pragma_state`])
/// on drop. Unlike [`StateScopeGuard`]/[`WhenMatchedGuard`], this one needs no
/// raw pointer: it is only used by `call_compiled_closure`, a short function
/// where every subsequent `self.*` access can be rewritten to go through the
/// guard's `Deref`/`DerefMut` instead, so a plain safe `&'a mut Interpreter`
/// borrow works.
pub(crate) struct PragmaGuard<'a> {
    interp: &'a mut Interpreter,
    saved: (bool, bool, crate::runtime::NewlineMode, bool),
}

impl<'a> PragmaGuard<'a> {
    pub(crate) fn new(interp: &'a mut Interpreter) -> Self {
        let saved = interp.save_pragma_state();
        PragmaGuard { interp, saved }
    }
}

impl<'a> std::ops::Deref for PragmaGuard<'a> {
    type Target = Interpreter;
    fn deref(&self) -> &Interpreter {
        self.interp
    }
}

impl<'a> std::ops::DerefMut for PragmaGuard<'a> {
    fn deref_mut(&mut self) -> &mut Interpreter {
        self.interp
    }
}

impl<'a> Drop for PragmaGuard<'a> {
    fn drop(&mut self) {
        self.interp.restore_pragma_state(self.saved);
    }
}

/// RAII guard isolating the "mark context" family of one-shot VM flags across
/// a live function/method call boundary. See
/// `todo/deep/mark-context-flags-leak-across-live-call-boundary.md`.
///
/// `MarkBindContext` and its siblings (`MarkScalarBindContext`,
/// `MarkParamRawBindContext`, `MarkRebindContext`, `MarkConstantContext`,
/// `MarkArrayShareSource`, `MarkExplicitInitializerContext`,
/// `MarkVarDeclContext`) are compiler-emitted opcodes that set a single
/// `Interpreter`-wide flag immediately before a `:=`/vardecl target's own
/// store op (`SetLocal`/`SetGlobal`), meant to be consumed by that VERY NEXT
/// store op. When a real function/method CALL sits between the mark and its
/// consumer (`@!other := make();` compiles to `MarkBindContext; ...;
/// CallFuncNamed; ...; SetGlobal`), the callee's own body runs with the
/// flag still set, so any vardecl/store inside the callee is wrongly treated
/// as a bind target too.
///
/// `vm_run_loop.rs`'s nested-run boundary (EVAL, `dies-ok`/`lives-ok` blocks)
/// already isolates this same flag family around `f(self)` — this guard
/// applies the identical save/clear/restore to the ordinary compiled-call
/// dispatch functions (`call_compiled_function_light_spec`,
/// `call_compiled_function_positional_light`, `call_compiled_function_fast`,
/// `call_compiled_function_named_inner`, `call_compiled_closure_with_topic`),
/// which push call frames in-place in a flat bytecode loop rather than
/// through a nested Rust-level `run()` invocation, so `vm_run_loop.rs`'s
/// boundary never fires for them.
///
/// Each field below is a raw pointer into that flag's OWN `Box<Cell<_>>`
/// allocation on `Interpreter` (see the module doc's "v3" section), never a
/// pointer to `Interpreter` itself.
pub(crate) struct MarkContextGuard {
    bind_context: *const Cell<bool>,
    scalar_bind_context: *const Cell<bool>,
    param_raw_bind_context: *const Cell<bool>,
    bound_decont_active: *const Cell<bool>,
    rebind_context: *const Cell<bool>,
    constant_context: *const Cell<bool>,
    array_share_context: *const Cell<bool>,
    array_share_source: *const Cell<Option<String>>,
    explicit_initializer_context: *const Cell<bool>,
    vardecl_context: *const Cell<bool>,
    saved_bind_context: bool,
    saved_scalar_bind_context: bool,
    saved_param_raw_bind_context: bool,
    saved_bound_decont_active: bool,
    saved_rebind_context: bool,
    saved_constant_context: bool,
    saved_array_share_context: bool,
    saved_array_share_source: Option<String>,
    saved_explicit_initializer_context: bool,
    saved_vardecl_context: bool,
}

impl MarkContextGuard {
    pub(crate) fn new(interp: &Interpreter) -> Self {
        let guard = MarkContextGuard {
            bind_context: &*interp.bind_context as *const Cell<bool>,
            scalar_bind_context: &*interp.scalar_bind_context as *const Cell<bool>,
            param_raw_bind_context: &*interp.param_raw_bind_context as *const Cell<bool>,
            bound_decont_active: &*interp.bound_decont_active as *const Cell<bool>,
            rebind_context: &*interp.rebind_context as *const Cell<bool>,
            constant_context: &*interp.constant_context as *const Cell<bool>,
            array_share_context: &*interp.array_share_context as *const Cell<bool>,
            array_share_source: &*interp.array_share_source as *const Cell<Option<String>>,
            explicit_initializer_context: &*interp.explicit_initializer_context
                as *const Cell<bool>,
            vardecl_context: &*interp.vardecl_context as *const Cell<bool>,
            saved_bind_context: interp.bind_context.get(),
            saved_scalar_bind_context: interp.scalar_bind_context.get(),
            saved_param_raw_bind_context: interp.param_raw_bind_context.get(),
            saved_bound_decont_active: interp.bound_decont_active.get(),
            saved_rebind_context: interp.rebind_context.get(),
            saved_constant_context: interp.constant_context.get(),
            saved_array_share_context: interp.array_share_context.get(),
            saved_array_share_source: interp.array_share_source.take(),
            saved_explicit_initializer_context: interp.explicit_initializer_context.get(),
            saved_vardecl_context: interp.vardecl_context.get(),
        };
        interp.bind_context.set(false);
        interp.scalar_bind_context.set(false);
        interp.param_raw_bind_context.set(false);
        interp.bound_decont_active.set(false);
        interp.rebind_context.set(false);
        interp.constant_context.set(false);
        interp.array_share_context.set(false);
        // Already cleared by the `.take()` above.
        interp.explicit_initializer_context.set(false);
        interp.vardecl_context.set(false);
        guard
    }
}

impl Drop for MarkContextGuard {
    fn drop(&mut self) {
        // SAFETY: see the module doc's "v3" section -- each pointer was taken
        // from that field's own `Box` allocation at construction, which
        // outlives the guard and never moves (none of these fields are
        // reassigned while the guard is alive). None of these dereferences
        // ever forms a reference to `Interpreter` itself, so they are
        // unaffected by any `&mut self` calls made elsewhere while the guard
        // was alive.
        unsafe {
            (*self.bind_context).set(self.saved_bind_context);
            (*self.scalar_bind_context).set(self.saved_scalar_bind_context);
            (*self.param_raw_bind_context).set(self.saved_param_raw_bind_context);
            (*self.bound_decont_active).set(self.saved_bound_decont_active);
            (*self.rebind_context).set(self.saved_rebind_context);
            (*self.constant_context).set(self.saved_constant_context);
            (*self.array_share_context).set(self.saved_array_share_context);
            (*self.array_share_source).set(self.saved_array_share_source.take());
            (*self.explicit_initializer_context).set(self.saved_explicit_initializer_context);
            (*self.vardecl_context).set(self.saved_vardecl_context);
        }
    }
}
