#![allow(clippy::result_large_err)]
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::ast::Stmt;
use crate::env::Env;
use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{
    ArrayKind, EnumValue, GatherCoroutineState, JunctionKind, LazyList, RuntimeError, Value,
    make_rat,
};
use num_traits::{Signed, Zero};

pub(crate) type MethodResolveEntry = Option<(String, Arc<crate::runtime::MethodDef>)>;

thread_local! {
    /// Set while execution is inside the `Interpreter::run` catch_unwind boundary, so the
    /// custom panic hook can stay quiet for panics that we are about to convert
    /// into a catchable `X::` error (instead of dumping a Rust backtrace).
    static IN_VM_PANIC_BOUNDARY: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

static VM_PANIC_HOOK_INIT: std::sync::Once = std::sync::Once::new();

/// Install (once) a panic hook that suppresses the default backtrace dump for
/// panics caught at the `Interpreter::run` boundary, unless the user opted into details
/// via `RUST_BACKTRACE`/`MUTSU_TRACE`. Panics outside the boundary (genuine
/// internal bugs) still print normally.
fn install_vm_panic_hook() {
    VM_PANIC_HOOK_INIT.call_once(|| {
        let default_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            let suppress = IN_VM_PANIC_BOUNDARY.with(|f| f.get());
            let want_details = std::env::var("RUST_BACKTRACE")
                .map(|v| !v.is_empty() && v != "0")
                .unwrap_or(false)
                || std::env::var("MUTSU_TRACE")
                    .map(|v| !v.is_empty())
                    .unwrap_or(false);
            if suppress && !want_details {
                return;
            }
            default_hook(info);
        }));
    });
}

/// Extract a human-readable message from a caught panic payload.
fn panic_payload_message(payload: &(dyn std::any::Any + Send)) -> String {
    if let Some(s) = payload.downcast_ref::<&str>() {
        (*s).to_string()
    } else if let Some(s) = payload.downcast_ref::<String>() {
        s.clone()
    } else {
        "unknown panic".to_string()
    }
}

/// Run a worker-thread body under the same panic->`X::AdHoc` boundary that
/// `run_inner_guarded`/`run_range_guarded` install for the main thread.
///
/// `start{}`/`Promise` bodies execute on a freshly spawned thread via
/// `call_value`, which does NOT route through `run_top` — so the main thread's
/// outer boundary does not cover them. Without this, a Rust panic
/// (overflow/index-OOB/capacity-overflow) raised by user code in a worker
/// terminates only that thread, leaving its `Promise` forever unresolved and
/// hanging the awaiter (or, under `panic=abort`, crashing the whole process).
/// Wrapping the body here converts such a panic into a catchable
/// `RuntimeError` (X::AdHoc) the caller can `break_with`, so `await` rethrows
/// it as a normal `X::Await::Died` instead of hanging.
pub(crate) fn guard_worker_panic<T>(
    body: impl FnOnce() -> Result<T, RuntimeError>,
) -> Result<T, RuntimeError> {
    install_vm_panic_hook();
    // The worker is a distinct thread, so its thread-local starts `false`;
    // setting it suppresses the default backtrace dump for the panic we are
    // about to convert (the panic hook reads the panicking thread's local).
    let prev = IN_VM_PANIC_BOUNDARY.with(|f| f.replace(true));
    let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(body));
    IN_VM_PANIC_BOUNDARY.with(|f| f.set(prev));
    match caught {
        Ok(r) => r,
        Err(payload) => Err(Interpreter::vm_panic_error(panic_payload_message(
            payload.as_ref(),
        ))),
    }
}

/// Pre-computed fast dispatch entry for compiled methods.
/// Caches all the information needed to skip intermediate dispatch steps
/// (wrap chain check, compiled_code extraction, fast-path eligibility checks).
pub(crate) struct FastMethodCacheEntry {
    owner_class: Symbol,
    method_def: Arc<crate::runtime::MethodDef>,
    compiled_code: Arc<CompiledCode>,
    can_skip_merge: bool,
    positional_count: usize,
    has_defaults: bool,
}

/// env-loan (CP-1 1e): call an interpreter carrier/helper that reads
/// `self.env`, lending the Interpreter-owned env for the duration.
///
/// Expands to: swap the real env into the interpreter's loan slot, run
/// `$self.<call>`, swap it back. Unlike a closure-based helper, the
/// call is inlined so it keeps the *partial* `self.interpreter` borrow — args may
/// still borrow other `self` fields (e.g. `self.locals[i]`) exactly as the
/// original `self.<call>` did, so no new borrow conflicts arise.
///
/// Only for **value/Result-returning** helpers: the post-call swap re-borrows
/// `self.interpreter`, so a returned reference into the interpreter cannot escape
/// (those few sites are handled individually). See docs/vm-state-ownership.md.
macro_rules! loan_env {
    ($self:ident, $($call:tt)+) => {{
        // CP-3 collapse: the Interpreter dissolved into the Interpreter, so env is no
        // longer loaned across a Interpreter->interpreter boundary — it is just `self.env`.
        // This macro is now a thin self-call kept so the ~350 call sites need no
        // edit; it will be removed in a later cosmetic pass.
        $self.$($call)+
    }};
}

mod vm_arith_int_ops;
mod vm_arith_ops;
mod vm_bitwise_ops;
mod vm_call_autothread;
mod vm_call_dispatch;
mod vm_call_eligibility;
mod vm_call_exec_ops;
mod vm_call_fast;
mod vm_call_func_ops;
mod vm_call_helpers;
mod vm_call_light;
mod vm_call_light_typed;
mod vm_call_method_compiled;
mod vm_call_method_compiled_cache;
mod vm_call_method_compiled_coerce;
mod vm_call_method_compiled_interpret;
mod vm_call_method_compiled_io;
mod vm_call_method_compiled_mut;
mod vm_call_method_mut_ops;
mod vm_call_method_ops;
mod vm_call_named;
mod vm_call_named_inner;
mod vm_call_resolve;
mod vm_closure_dispatch;
mod vm_coerce_concat_ops;
mod vm_comparison_container_ops;
mod vm_comparison_ops;
mod vm_comparison_order_ops;
mod vm_control_ops;
mod vm_core_helpers;
mod vm_data_io_ops;
mod vm_data_ops;
mod vm_data_push_ops;
mod vm_dispatch_helpers;
mod vm_env_helpers;
mod vm_exec_dispatch;
pub(crate) mod vm_flipflop_ops;
mod vm_for_loop_body;
mod vm_for_loop_dispatch;
mod vm_for_loop_intrange;
mod vm_for_loop_lazy;
mod vm_given_when_ops;
mod vm_helpers;
mod vm_helpers_junction;
mod vm_helpers_lazy;
mod vm_helpers_lazy_pull;
mod vm_helpers_lazy_scan;
pub(crate) mod vm_hyper_func;
mod vm_hyper_method_ops;
pub(crate) mod vm_hyper_ops;
mod vm_hyper_race_parallel;
mod vm_loop_cstyle_repeat;
mod vm_loop_writeback;
mod vm_loop_writeback_quant;
pub(crate) mod vm_meta_ops;
pub(crate) mod vm_method_dispatch;
pub(crate) mod vm_misc_assign;
pub(crate) mod vm_misc_block;
pub(crate) mod vm_misc_codevar;
pub(crate) mod vm_misc_coerce;
pub(crate) mod vm_misc_ops;
pub(crate) mod vm_misc_reduction_exec;
pub(crate) mod vm_misc_reduction_scan;
pub(crate) mod vm_misc_scope;
pub(crate) mod vm_misc_typecheck;
pub(crate) mod vm_misc_typed_range;
mod vm_mixin_does_ops;
mod vm_module_ops;
mod vm_native_dispatch;
mod vm_native_extrema;
mod vm_native_first;
mod vm_native_json;
mod vm_native_map;
mod vm_native_sort;
mod vm_native_subst;
mod vm_native_test;
mod vm_react_loop;
mod vm_react_subscriptions;
mod vm_react_supply_helpers;
mod vm_register_ops;
mod vm_register_sub_ops;
mod vm_run_loop;
mod vm_scope_ops;
mod vm_set_arith_ops;
mod vm_set_ops;
pub(crate) mod vm_smart_match;
mod vm_smartmatch_ops;
pub(crate) mod vm_stats;
pub(crate) mod vm_string_regex_ops;
pub(crate) mod vm_subst_apply;
pub(crate) mod vm_subst_exec;
mod vm_try_catch_ops;
mod vm_typedecl_ops;
mod vm_value_helpers;
mod vm_var_assign_coerce;
mod vm_var_assign_computed_attr;
mod vm_var_assign_element;
mod vm_var_assign_index_named;
mod vm_var_assign_local;
mod vm_var_assign_local_get;
mod vm_var_assign_ops;
mod vm_var_assign_post_incdec;
mod vm_var_assign_set_local;
mod vm_var_assign_typed;
mod vm_var_delete_ops;
mod vm_var_exists_ops;
mod vm_var_get_ops;
mod vm_var_index_ops;
mod vm_var_index_tracking;
mod vm_var_multidim_helpers;
mod vm_var_multidim_ops;
mod vm_var_ops;
mod vm_var_trait_ops;

fn cmp_values(left: &Value, right: &Value) -> std::cmp::Ordering {
    crate::runtime::compare_values(left, right).cmp(&0)
}

/// Saved state for a compiled function/closure/method call frame.
/// A CONTROL handler active on the dynamic (Rust) call stack. Pushed when a
/// block with a `CONTROL { }` phaser begins executing its protected body and
/// popped when that body finishes. `control_handlers.len()` is kept equal to
/// `control_handler_depth` so the innermost handler is always `.last()`.
pub(crate) struct ControlHandlerEntry {
    /// Whether this handler unconditionally `.resume`s (see `OpCode::TryCatch`
    /// `resume_safe`). When false, `handler` is `None` and a deep warn falls
    /// back to the unwinding path.
    pub resume_safe: bool,
    /// Present only for `resume_safe` handlers: the bytecode + range + function
    /// table needed to run the handler INLINE at a deep `warn` raise site.
    pub handler: Option<ControlHandlerCode>,
}

/// Self-contained bytecode for running a `resume_safe` CONTROL handler inline.
/// The `code` is an owned `Arc` clone of the installing frame's `CompiledCode`
/// (intra-range jump targets stay valid because indices are preserved); the
/// `compiled_fns` clone lets the handler body call user subs visible at install.
pub(crate) struct ControlHandlerCode {
    pub code: std::sync::Arc<CompiledCode>,
    pub control_begin: usize,
    pub end: usize,
    pub compiled_fns: HashMap<String, CompiledFunction>,
}

pub(crate) struct VmCallFrame {
    pub saved_env: Env,
    pub saved_locals: Vec<Value>,
    pub saved_upvalues: Vec<Option<Value>>,
    pub saved_stack_depth: usize,
    /// None when using light call frame (simple methods that don't use `:=` binding).
    pub saved_readonly: Option<HashSet<String>>,
    /// Read-only var names this frame *newly* added to `readonly_vars` (used by
    /// the light-frame method path, which marks `$` scalar params read-only
    /// without cloning the whole set). Removed on `pop_call_frame`. Empty for the
    /// slow path, which restores the full set via `saved_readonly` instead.
    pub readonly_added: Vec<String>,
    /// Read-only var names this frame *removed* from `readonly_vars` because a
    /// writable (`is copy`/`is rw`/`is raw`) param shadows a caller's same-named
    /// read-only variable. Re-added on `pop_call_frame` to restore the caller's
    /// state. Light-frame method path counterpart to `readonly_added`.
    pub readonly_removed: Vec<String>,
    pub saved_local_bind_pairs: Vec<(usize, usize)>,
}

// CP-3 collapse: the bytecode Interpreter has been fully dissolved into the `Interpreter`
// struct — the `Interpreter` *is* the bytecode Interpreter. The former `Interpreter` type alias is
// gone; the `impl` blocks in `src/vm/` are `impl Interpreter`. (The `vm`/`src/vm`
// module names are retained as the home of the bytecode-execution methods.)
