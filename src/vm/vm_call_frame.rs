//! The call-frame record (`VmCallFrame`): what a call saves of its caller's
//! interpreter registers and restores on return (`push_call_frame` /
//! `pop_call_frame` in `vm_env_helpers`).

use crate::env::Env;
use crate::value::Value;

pub(crate) struct VmCallFrame {
    pub saved_env: Env,
    /// The address of the caller's `CompiledCode` (`Interpreter::current_code`
    /// at the push; 0 when none ran yet): the names of the slots in
    /// `saved_locals_base`'s region, for a reader that walks the dynamic
    /// scope by name (`DYNAMIC::<key>`, `vm_dynamic_stash`). The caller's code
    /// outlives the frame: it is the code waiting for this call to return.
    pub caller_code: usize,
    /// The callee's argument capture, recorded only for code that reads it
    /// (`nqp::usecapture`, see `runtime::call_capture`). Kept on the frame
    /// rather than in the env so a return merge can never copy it into the
    /// caller.
    pub call_capture: Option<crate::runtime::call_capture::CallCapture>,
    /// The caller's `cur_source_line` at frame push, restored on pop (the line
    /// the callee body's ops advanced to must not leak into the caller).
    pub saved_cur_line: i64,
    /// Handle to the caller's slot frame, closed by `pop_call_frame` (ADR-0077:
    /// a frame is a region of the shared slot stack, not its own vector). In an
    /// `Option` only so that pop can move the handle out — closing a frame
    /// consumes it — while still handing the frame back to its caller.
    pub saved_locals_base: Option<crate::runtime::locals::CallerFrame>,
    /// The caller's per-activation registers that are usually empty, moved
    /// out of the interpreter only when one of them is not (see
    /// [`VmCallFrameSpill`]). `None` means the caller had all of them empty,
    /// so `pop_call_frame` only clears what the callee left behind.
    pub saved_spill: Option<Box<VmCallFrameSpill>>,
    pub saved_stack_depth: usize,
    /// Rollback mark of this frame's readonly scope (see
    /// `enter_readonly_frame`): `pop_call_frame` replays the readonly-set
    /// mutations journaled since the frame was pushed. Replaces both the old
    /// whole-set `Arc` snapshot and the light-frame `readonly_added`/
    /// `readonly_removed` delta lists — the journal records exactly the
    /// mutations either mechanism would have to undo.
    pub readonly_mark: usize,
    /// The caller's loop-body-local declaration scopes (ADR-0078: a base into
    /// the shared `ScopeStack`, not a moved-out vector). These VM fields track
    /// which `my` names a `for`/`while` body declared so they can be restored at
    /// loop exit (see `pop_loop_local_scope`). A compiled-function body runs via
    /// its own `exec_one` mini-loop (not `run()`, which saves them), so without
    /// hiding them here a callee invoked from inside a loop body would
    /// register its own `my` declarations in the *caller's* active loop scope and
    /// have them clobbered at the caller's loop exit.
    pub saved_loop_local_vars: Option<crate::runtime::scope_stack::ScopeFrame>,
    pub saved_loop_local_saved_env: Option<crate::runtime::scope_stack::ScopeFrame>,
    /// The caller's block-scope `my`-declaration tracking stack. A `BlockScope`
    /// pushes a frame here and, on exit, reverts every name it records to the
    /// pre-block value (block-local `my` must not leak out). A compiled-function
    /// body runs via its own `exec_one` mini-loop, so without hiding this a
    /// callee's routine-level `my $x` (which runs before the callee enters any of
    /// its own blocks) would register in the *caller's* active `BlockScope` frame
    /// and get reverted at the caller's block exit — clobbering a same-named
    /// caller variable across a recursive call.
    pub saved_block_declared_vars: Option<crate::runtime::scope_stack::ScopeFrame>,
    /// The caller's active-for-loop-parameter-name stack (ADR-0023). A
    /// compiled-function body runs via its own `exec_one` mini-loop, not
    /// `run()`/`with_nested_registers` (which save these), so without
    /// hiding it here a callee's own free variable that merely shares an
    /// OUTER loop's parameter name would be mistaken for that loop's
    /// per-iteration binding by `block_captured_scalars`.
    pub saved_active_loop_param_names: Option<crate::runtime::scope_stack::ScopeFrame>,
    /// The caller frame's `active_loop_rw_param_names` stack — saved and
    /// restored exactly like `saved_active_loop_param_names`.
    pub saved_active_loop_rw_param_names: Option<crate::runtime::scope_stack::ScopeFrame>,
    /// A slot frame's slim record (ADR-18393 D1, `vm_method_slot_frame`): it
    /// saved only the env handle, the caller code, the line, the slot window
    /// and the stack depth. Every other field is empty and was never moved
    /// out of the interpreter, so `pop_call_frame` must not restore it.
    pub slot: bool,
}

/// The part of a [`VmCallFrame`] that saves caller registers which are empty
/// in almost every activation (#18267 stage 3). Holding them in the record
/// itself made every call move six empty containers in and out of the call
/// stack and drop them again; `push_call_frame` now boxes them only when one
/// of them is non-empty.
#[derive(Default)]
pub(crate) struct VmCallFrameSpill {
    pub saved_upvalues: Vec<Option<Value>>,
    pub saved_local_bind_pairs: Vec<(usize, usize)>,
    /// The caller frame's `frame_authoritative` set — the free-var names this
    /// frame vouches for so a closure created inside it inherits authoritative
    /// (overwrite) capture semantics (runtime transitive vouching). Saved here so
    /// a nested call restores it, mirroring `saved_upvalues`.
    pub saved_frame_authoritative: Vec<crate::symbol::Symbol>,
    /// The caller frame's `frame_owned` set (ADR-0027) — the free-var names
    /// this frame vouches for as loop-frozen, so a closure created inside it
    /// inherits owned (force-overwrite) capture semantics for those names
    /// whose currently captured value is plain (not a live shared cell).
    /// Saved here so a nested call restores it, mirroring
    /// `saved_frame_authoritative`.
    pub saved_frame_owned: Vec<crate::symbol::Symbol>,
    /// The caller's pending caller-var writebacks, hidden while this frame
    /// runs. Each names a caller lexical that some write changed; the first
    /// frame whose `code` has a local of that name refreshes its slot from
    /// env. A frame entered after the write cannot own that variable, but it
    /// can have a local of the same name -- Test's `proclaim($cond, $desc is
    /// copy)` took the `$desc` a `lives-ok { $desc = ... }` block had just
    /// written and the caller kept its old value. `pop_call_frame` merges the
    /// frame's own unclaimed writebacks back over these.
    pub saved_pending_caller_var_writeback: rustc_hash::FxHashSet<String>,
    /// The caller frame's `rw_param_rebinds` (see `vm_rw_param_rebind`),
    /// hidden while this frame runs and restored on pop.
    pub saved_rw_param_rebinds: Vec<(u32, Option<Value>)>,
}

impl VmCallFrame {
    /// The caller's upvalue array this frame hid (empty when it had none).
    // Cost: O(1).
    pub(crate) fn saved_upvalues(&self) -> &[Option<Value>] {
        self.saved_spill
            .as_deref()
            .map_or(&[], |spill| spill.saved_upvalues.as_slice())
    }
}
