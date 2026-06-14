#![allow(clippy::result_large_err)]
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

use crate::ast::Stmt;
use crate::env::Env;
use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{
    ArrayKind, EnumValue, ForLoopResumeState, GatherCoroutineState, JunctionKind, LazyList,
    RuntimeError, Value, make_rat,
};
use num_traits::{Signed, Zero};

type MethodResolveEntry = Option<(String, Arc<crate::runtime::MethodDef>)>;

/// Pre-computed fast dispatch entry for compiled methods.
/// Caches all the information needed to skip intermediate dispatch steps
/// (wrap chain check, compiled_code extraction, fast-path eligibility checks).
struct FastMethodCacheEntry {
    owner_class: Symbol,
    method_def: Arc<crate::runtime::MethodDef>,
    compiled_code: Arc<CompiledCode>,
    can_skip_merge: bool,
    positional_count: usize,
    has_defaults: bool,
}

mod vm_arith_ops;
mod vm_call_autothread;
mod vm_call_dispatch;
mod vm_call_exec_ops;
mod vm_call_func_ops;
mod vm_call_helpers;
mod vm_call_method_compiled;
mod vm_call_method_mut_ops;
mod vm_call_method_ops;
mod vm_closure_dispatch;
mod vm_comparison_ops;
mod vm_control_ops;
mod vm_data_ops;
mod vm_dispatch_helpers;
mod vm_env_helpers;
mod vm_helpers;
mod vm_hyper_method_ops;
mod vm_hyper_race_parallel;
mod vm_method_dispatch;
pub(crate) mod vm_misc_ops;
mod vm_native_dispatch;
mod vm_native_extrema;
mod vm_native_first;
mod vm_native_map;
mod vm_native_sort;
mod vm_native_subst;
mod vm_native_test;
mod vm_register_ops;
mod vm_set_ops;
pub(crate) mod vm_smart_match;
pub(crate) mod vm_stats;
pub(crate) mod vm_string_regex_ops;
mod vm_value_helpers;
mod vm_var_assign_ops;
mod vm_var_delete_ops;
mod vm_var_exists_ops;
mod vm_var_get_ops;
mod vm_var_index_ops;
mod vm_var_multidim_ops;
mod vm_var_ops;

fn cmp_values(left: &Value, right: &Value) -> std::cmp::Ordering {
    crate::runtime::compare_values(left, right).cmp(&0)
}

/// Saved state for a compiled function/closure/method call frame.
pub(super) struct VmCallFrame {
    pub saved_env: Env,
    pub saved_locals: Vec<Value>,
    pub saved_stack_depth: usize,
    /// None when using light call frame (simple methods that don't use `:=` binding).
    pub saved_readonly: Option<HashSet<String>>,
    pub saved_env_dirty: bool,
    pub saved_local_bind_pairs: Vec<(usize, usize)>,
}

pub(crate) struct VM {
    interpreter: Interpreter,
    /// Handle to the shared declaration [`Registry`](crate::runtime::Registry),
    /// cloned from the interpreter at construction (PLAN.md ② → A: the VM holds the
    /// registry as a *peer* rather than reaching through `self.interpreter`). Points
    /// at the same `RwLock` as `self.interpreter`'s handle, so mutations through
    /// either are visible. Stable for the VM's lifetime: the interpreter's registry
    /// Arc is only ever replaced by `clone_for_thread` (which builds a fresh
    /// Interpreter for a child thread), never reassigned in place during a run.
    registry: Arc<RwLock<crate::runtime::Registry>>,
    /// Handle to the shared [`IoHandleTable`](crate::runtime::IoHandleTable),
    /// cloned from the interpreter at construction (PLAN.md ③ native IO PR-C: the
    /// VM holds the IO handle table as a *peer*, mirroring `registry`). Points at
    /// the same `RwLock` as `self.interpreter`'s handle, so mutations through
    /// either are visible. Stable for the VM's lifetime (same `clone_for_thread`
    /// reasoning as `registry`). Used by `try_native_io_handle_method` to resolve
    /// pure-handle IO methods natively instead of bouncing through the interpreter.
    io_handles: Arc<RwLock<crate::runtime::IoHandleTable>>,
    /// The VM's clone of the shared [`OutputSink`](crate::runtime::OutputSink)
    /// handle (same `RwLock` as the interpreter's), so Stdout/Stderr output
    /// dispatch resolves natively instead of bouncing through the interpreter
    /// (③後段 PR-C).
    output_sink: Arc<RwLock<crate::runtime::OutputSink>>,
    /// The VM's clone of the shared `current_package` handle (same `RwLock` as
    /// the interpreter's), so the VM reads/writes the in-scope package name
    /// through its own peer handle instead of bouncing through
    /// `self.interpreter` — a step toward removing the interpreter bridge.
    /// Stable for the VM's lifetime (same `clone_for_thread` reasoning as
    /// `registry`).
    current_package: Arc<RwLock<String>>,
    stack: Vec<Value>,
    locals: Vec<Value>,
    in_smartmatch_rhs: bool,
    /// Set by transliterate op when executed inside smartmatch RHS.
    /// The smartmatch handler uses this to return the transliterate result
    /// as a StrDistance-like value instead of performing eq comparison.
    transliterate_in_smartmatch: bool,
    /// Set by substitution op when executed inside smartmatch RHS.
    /// The smartmatch handler returns `$/` (Match) on success or False on failure.
    substitution_in_smartmatch: bool,
    /// Tracks the last value passed to SetTopic, used as the REPL display value.
    last_topic_value: Option<Value>,
    topic_save_stack: Vec<Value>,
    /// Container name from when/default body (for Scalar container binding)
    container_ref_var: Option<String>,
    /// When true, the container ref is reversed (e.g. `for @a.reverse`)
    container_ref_reversed: bool,
    /// Source variable name for topic binding in for loops
    topic_source_var: Option<String>,
    /// rw aggregate view writeback for `for @a.grep(...) { $_++ }`: when the
    /// for-loop iterable is a grep result with a registered grep-view binding,
    /// this holds (source array Arc, per-filtered-index source indices, kind)
    /// so the loop can write modified topics back to the original array slots.
    for_grep_view: Option<(
        Arc<crate::value::ArrayData>,
        Vec<usize>,
        crate::value::ArrayKind,
    )>,
    /// Names of multi-param for-loop bindings (`-> %a, %b`) whose `%`/`@` params
    /// must preserve a QuantHash (Set/Bag/Mix) value rather than coercing it to
    /// a plain Hash, matching Raku's parameter-binding semantics.
    quanthash_bind_params: Vec<String>,
    /// Stack of single named for-loop params whose prior binding must be
    /// restored *after* the loop's LAST/post phasers run. A for-loop pushes its
    /// saved (name, prior-value) on normal completion; the matching
    /// `RestoreForParam` opcode (emitted after the post phasers) pops and
    /// applies it. LIFO so nested loops with the same param name nest correctly.
    for_param_restore_stack: Vec<(String, Option<Value>)>,
    /// Stack of saved call frames for compiled function/closure/method calls.
    call_frames: Vec<VmCallFrame>,
    /// When true, locals may be stale relative to env (interpreter bridge modified env).
    /// Cleared after sync_locals_from_env or pop_call_frame.
    env_dirty: bool,
    /// Set by a compiled method dispatch to report whether it left the caller's
    /// local slots coherent (Slice 6.3). `true` means the call provably wrote
    /// nothing the caller can observe in its slots (a read-only / no-merge method
    /// or a merge that propagated nothing), so the CallMethod/CallMethodMut
    /// opcode skips its env_dirty mark and the caller avoids a per-call env->locals
    /// pull. Defaults to `false` (conservative) for every non-compiled path
    /// (native fallback, interpreter bridge, blocks) so a missed signal can only
    /// cost a redundant pull, never a stale read.
    method_dispatch_pure: bool,
    /// The instruction pointer to resume at after a .resume call in a CATCH block.
    /// Set when Die/Fail creates an exception, used by exec_try_catch_op.
    resume_ip: Option<usize>,
    /// When true, the next SetLocal is a `:=` bind (preserves container type for `@` vars).
    bind_context: bool,
    /// When true, the next SetLocal binds a `$` scalar to a Positional via `:=` and
    /// must record the scalar as decontainerized. Independent of `bind_context` so
    /// scalar binds keep their existing (fast-path) store routing.
    scalar_bind_context: bool,
    /// True once any scalar has been bound (`:=`) to a Positional value, marking
    /// it as decontainerized in the env (`__mutsu_bound_decont::$name`). Guards the
    /// (otherwise hot) marker-clearing in scalar assignment so the common case
    /// (no such binds) stays zero-cost.
    bound_decont_active: bool,
    /// When true, the next SetLocal is a `:=` rebind (not VarDecl).
    /// Used to trigger cleanup of old bind pairs and reverse aliases.
    rebind_context: bool,
    /// When true, the next SetLocal skips @/% container coercion (for `constant @x`).
    constant_context: bool,
    /// When true, the next SetLocal came from an explicit initializer (`= expr`).
    explicit_initializer_context: bool,
    /// When true, the next SetLocal is from a `my` VarDecl (not a plain assignment).
    /// Used to allow overwriting immutable Blob containers in loop redeclarations.
    vardecl_context: bool,
    /// Local-slot binding pairs created by `:=` VarDecl in the current scope.
    /// Each entry is (source_slot, target_slot): writing to source_slot should
    /// propagate the value to target_slot.
    local_bind_pairs: Vec<(usize, usize)>,
    /// Cache for on-the-fly compiled functions, keyed by fingerprint.
    /// Prevents re-compilation which would break state variables.
    otf_compile_cache: HashMap<u64, CompiledFunction>,
    /// Current closure instance ID for state variable scoping.
    /// When Some, state var keys are suffixed with `#c{id}` to give each
    /// closure clone its own state. Pushed/popped around closure calls.
    state_scope_id: Option<u64>,
    /// Cache for compiled function resolution.
    /// Maps (name, arity) → (compiled_fns key, fingerprint).
    #[allow(clippy::type_complexity)]
    fn_resolve_cache: HashMap<(Symbol, usize, Vec<String>), (String, u64, String)>,
    /// Generation counter for fn_resolve_cache invalidation.
    fn_resolve_gen: u64,
    /// The generation at which fn_resolve_cache was last populated.
    fn_resolve_cache_gen: u64,
    /// Cache for has_multi_candidates results, invalidated by fn_resolve_gen.
    multi_candidates_cache: HashMap<Symbol, bool>,
    /// The generation at which multi_candidates_cache was last valid.
    multi_candidates_cache_gen: u64,
    /// Light-call cache: maps function name symbol to (compiled_fns key, fingerprint).
    /// Used by the light call fast path in exec_call_func_op to skip expensive
    /// function resolution on repeated calls to the same function.
    light_call_cache: HashMap<Symbol, (String, u64)>,
    /// The generation at which light_call_cache was last valid.
    light_call_cache_gen: u64,
    /// Positional light-call cache for simple positional-only functions.
    pos_light_call_cache: HashMap<Symbol, (String, u64)>,
    /// The generation at which pos_light_call_cache was last valid.
    pos_light_call_cache_gen: u64,
    /// Method resolution cache: maps (class_name, method_name) to resolved
    /// (owner_class, MethodDef). Avoids repeated MRO walks for the same method.
    method_resolve_cache: HashMap<(Symbol, Symbol), MethodResolveEntry>,
    /// Monomorphic inline cache: stores the last (class, method, owner, def)
    /// resolution result. Skips HashMap lookup for repeated same-class calls.
    last_method_resolve: Option<(Symbol, Symbol, String, Arc<crate::runtime::MethodDef>)>,
    /// Fast method dispatch cache: maps (class, method) to pre-computed dispatch
    /// info including compiled code and fast-path eligibility. Skips wrap chain
    /// checks, compiled_code extraction, and param_def eligibility scans.
    fast_method_cache: HashMap<(Symbol, Symbol), FastMethodCacheEntry>,
    /// Stack of sets tracking variable names declared (via SetVarDynamic) within
    /// each active BlockScope. Used during BlockScope restoration to avoid
    /// propagating block-local variable values to the outer scope.
    block_declared_vars: Vec<std::collections::HashSet<String>>,
    /// Stack of sets tracking variable names declared (via SetVarDynamic) within
    /// each active loop body. A loop body is re-entered per iteration, so a `my`
    /// declared in it is a *fresh binding each iteration* — a closure created in
    /// the body must capture that iteration's value, not the shared lexical name
    /// (Raku per-iteration binding). When a closure is created, free variables
    /// found in this stack are recorded as its `owned_captures`, and at call time
    /// the closure reads them from its own frozen captured env (overwriting the
    /// caller's current value) — immune to the dual-store slot re-injection that
    /// otherwise leaks the loop's final value into every closure. See
    /// docs/vm-dual-store.md / PLAN.md lever C.
    loop_local_vars: Vec<std::collections::HashSet<String>>,
    /// Parallel stack to `loop_local_vars`: for each loop-body-local `my` name
    /// that *shadows an existing outer binding*, the env value it shadowed at the
    /// moment it was first declared in the loop body. On loop exit that outer
    /// value is restored (in both env and the shared local slot), so a block-local
    /// `my $x` inside a loop body does not clobber an enclosing same-named outer
    /// `$x` (Raku lexical block scoping). Names with no prior binding are not
    /// recorded. See pop_loop_local_scope / PLAN.md lever C Slice 3.
    loop_local_saved_env: Vec<std::collections::HashMap<String, Value>>,
    /// True while a loop's *condition* (not its body) is being evaluated. A `my`
    /// declared in a `while`/`until`/`loop` condition — including the statement-
    /// modifier form `... until COND` — is lexically the enclosing scope's, not
    /// the loop body's, and is commonly read after the loop. Suppress recording
    /// such a declaration in `loop_local_saved_env` so loop-exit restoration does
    /// not wipe it. See pop_loop_local_scope.
    loop_cond_active: bool,
    /// Stack of saved locals snapshots for each active BlockScope.
    /// Used by GetOuterVar to access variables from enclosing lexical scopes.
    outer_scope_locals: Vec<Vec<Value>>,
    /// Pending alias-based bind pairs created by `:=` binding inside closures
    /// (e.g. `$a := $arg` where `$arg is rw`).  Each entry is
    /// (target_name, source_name): after the closure returns, the caller
    /// creates local_bind_pairs from these so writes to source propagate to target.
    pending_alias_bind_names: Vec<(String, String)>,
    /// On-the-fly compiled function call cache: maps function name to its
    /// compiled form. Used to avoid going through the interpreter fallback
    /// for user-defined functions that were compiled on-the-fly.
    otf_call_cache: HashMap<Symbol, CompiledFunction>,
    /// The generation at which otf_call_cache was last valid.
    otf_call_cache_gen: u64,
    /// Depth counter for CHECK phaser scope. When > 0, runtime errors should
    /// be wrapped in X::Comp::BeginTime before propagating.
    check_phaser_depth: u32,
    /// Temporary storage for for-loop resume state when a gather take-limit
    /// interrupts a for loop.
    gather_for_loop_resume: Option<ForLoopResumeState>,
    /// Scratch slot for the native rw-`.map` writeback path. When a closure call
    /// is made with rw-topic capture armed, `call_compiled_closure_with_topic`
    /// stashes the block's final `$_` mutation here (read from
    /// `__mutsu_rw_map_topic__` before the call frame is popped) so the native
    /// map loop can write it back to the source array element. `None` when the
    /// block did not mutate `$_`. See `vm_native_map.rs`.
    rw_map_topic_capture: Option<Value>,
}

impl VM {
    /// Wrap a runtime error in X::Comp::BeginTime (used for errors inside CHECK phasers).
    fn wrap_in_begin_time(inner: RuntimeError) -> RuntimeError {
        use std::collections::HashMap;
        let inner_exception = inner
            .exception
            .as_ref()
            .map(|e| e.as_ref().clone())
            .unwrap_or_else(|| Value::str(inner.message.clone()));
        let msg = format!(
            "An exception occurred while evaluating a CHECK\nException details:\n  {}",
            inner.message
        );
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert("exception".to_string(), inner_exception);
        RuntimeError::typed("X::Comp::BeginTime", attrs)
    }

    /// Mark a Failure on top of the stack as handled (used by boolean/defined checks).
    fn mark_failure_handled_on_stack(stack: &mut [Value]) {
        if let Some(Value::Instance {
            class_name,
            id,
            attributes,
            ..
        }) = stack.last_mut()
            && *class_name == "Failure"
        {
            attributes.insert("handled".to_string(), Value::Bool(true));
            crate::value::mark_failure_handled(*id);
        }
    }

    fn validate_labels(code: &CompiledCode) -> Result<(), RuntimeError> {
        let mut seen: HashSet<String> = HashSet::new();
        for op in &code.ops {
            if let OpCode::Label(name_idx) = op {
                let label_name = Self::const_str(code, *name_idx);
                if !seen.insert(label_name.to_string()) {
                    return Err(RuntimeError::new(format!(
                        "X::Redeclaration: Label '{}' already declared",
                        label_name
                    )));
                }
            }
        }
        Ok(())
    }

    fn runtime_error_from_exception_value(
        &mut self,
        value: Value,
        default_message: &str,
        is_fail: bool,
    ) -> RuntimeError {
        if matches!(value, Value::Nil) {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "payload".to_string(),
                Value::str(default_message.to_string()),
            );
            attrs.insert(
                "message".to_string(),
                Value::str(default_message.to_string()),
            );
            let exception = Value::make_instance(Symbol::intern("X::AdHoc"), attrs);
            let mut err = RuntimeError::new(default_message);
            err.is_fail = is_fail;
            err.exception = Some(Box::new(exception));
            return err;
        }

        let message = if let Value::Instance { attributes, .. } = &value {
            attributes
                .as_map()
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    // Try calling the user-defined .Str method
                    self.interpreter
                        .call_method_with_values(value.clone(), "Str", vec![])
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|_| value.to_string_value())
                })
        } else if let Value::Array(items, _) = &value {
            // Multi-arg die: concatenate .Str of each element
            let mut parts = Vec::new();
            for item in items.iter() {
                let s = self
                    .interpreter
                    .call_method_with_values(item.clone(), "Str", vec![])
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|_| item.to_string_value());
                parts.push(s);
            }
            parts.join("")
        } else {
            value.to_string_value()
        };

        let mut err = RuntimeError::new(&message);
        err.is_fail = is_fail;
        if let Value::Instance { class_name, .. } = &value {
            let cn = class_name.resolve();
            let is_exception = cn == "Exception"
                || cn.starts_with("X::")
                || cn.starts_with("CX::")
                || self
                    .interpreter
                    .mro_readonly(&cn)
                    .iter()
                    .any(|p| p == "Exception" || p.starts_with("X::") || p.starts_with("CX::"));
            if is_exception {
                err.exception = Some(Box::new(value));
            } else {
                // Non-exception instance: wrap in X::AdHoc with payload
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("payload".to_string(), value);
                attrs.insert("message".to_string(), Value::str(message));
                err.exception = Some(Box::new(Value::make_instance(
                    Symbol::intern("X::AdHoc"),
                    attrs,
                )));
            }
        } else {
            // Non-instance value (Str, Int, etc.): wrap in X::AdHoc with payload
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("payload".to_string(), value);
            attrs.insert("message".to_string(), Value::str(message));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::AdHoc"),
                attrs,
            )));
        }
        err
    }

    pub(crate) fn new(interpreter: Interpreter) -> Self {
        let registry = interpreter.registry_handle();
        let io_handles = interpreter.io_handles_handle();
        let output_sink = interpreter.output_sink_handle();
        let current_package = interpreter.current_package_handle();
        Self {
            interpreter,
            registry,
            io_handles,
            output_sink,
            current_package,
            stack: Vec::new(),
            locals: Vec::new(),
            in_smartmatch_rhs: false,
            transliterate_in_smartmatch: false,
            substitution_in_smartmatch: false,
            last_topic_value: None,
            topic_save_stack: Vec::new(),
            container_ref_var: None,
            container_ref_reversed: false,
            topic_source_var: None,
            for_grep_view: None,
            quanthash_bind_params: Vec::new(),
            for_param_restore_stack: Vec::new(),
            call_frames: Vec::new(),
            env_dirty: false,
            method_dispatch_pure: false,
            resume_ip: None,
            bind_context: false,
            scalar_bind_context: false,
            bound_decont_active: false,
            rebind_context: false,
            constant_context: false,
            explicit_initializer_context: false,
            vardecl_context: false,
            local_bind_pairs: Vec::new(),
            otf_compile_cache: HashMap::new(),
            state_scope_id: None,
            fn_resolve_cache: HashMap::new(),
            fn_resolve_gen: 0,
            fn_resolve_cache_gen: 0,
            multi_candidates_cache: HashMap::new(),
            multi_candidates_cache_gen: 0,
            light_call_cache: HashMap::new(),
            light_call_cache_gen: 0,
            pos_light_call_cache: HashMap::new(),
            pos_light_call_cache_gen: 0,
            method_resolve_cache: HashMap::new(),
            last_method_resolve: None,
            fast_method_cache: HashMap::new(),
            block_declared_vars: Vec::new(),
            loop_local_vars: Vec::new(),
            loop_local_saved_env: Vec::new(),
            loop_cond_active: false,
            outer_scope_locals: Vec::new(),
            pending_alias_bind_names: Vec::new(),
            otf_call_cache: HashMap::new(),
            otf_call_cache_gen: 0,
            check_phaser_depth: 0,
            gather_for_loop_resume: None,
            rw_map_topic_capture: None,
        }
    }

    /// Write access to the shared declaration [`Registry`](crate::runtime::Registry)
    /// via the VM's own handle (no `self.interpreter` bounce). Same lock and same
    /// guard discipline as the interpreter's `registry_mut`: never hold the guard
    /// across user-code re-entry.
    #[inline]
    pub(crate) fn registry_mut(&self) -> crate::runtime::RegistryWriteGuard<'_> {
        crate::runtime::RegistryWriteGuard::new(&self.registry, "registry")
    }

    /// Read access to the shared declaration [`Registry`] via the VM's own handle.
    /// Used by the VM-native dispatch predicates ([`Self::has_proto`] /
    /// [`Self::has_multi_candidates`]) so they read the registry without bouncing
    /// through `self.interpreter`. The guard must not be held across user-code
    /// re-entry (same discipline as `registry_mut`).
    #[inline]
    pub(crate) fn registry(&self) -> crate::runtime::RegistryReadGuard<'_> {
        crate::runtime::RegistryReadGuard::new(&self.registry, "registry")
    }

    /// VM-native `proto`-declaration check, mirroring
    /// `Interpreter::has_proto`: reads the VM's own registry + `current_package`
    /// handles and delegates to the single [`Registry::has_proto`] implementation.
    #[inline]
    pub(crate) fn has_proto(&self, name: &str) -> bool {
        let pkg = self.current_package();
        self.registry().has_proto(&pkg, name)
    }

    /// VM-native multi-candidate check, mirroring
    /// `Interpreter::has_multi_candidates` via the single
    /// [`Registry::has_multi_candidates`] implementation.
    #[inline]
    pub(crate) fn has_multi_candidates(&self, name: &str) -> bool {
        let pkg = self.current_package();
        self.registry().has_multi_candidates(&pkg, name)
    }

    /// VM-native declared-(non-multi-)function check, mirroring
    /// `Interpreter::has_declared_function` via the single
    /// [`Registry::has_declared_function`] implementation.
    #[inline]
    pub(crate) fn has_declared_function(&self, name: &str) -> bool {
        let pkg = self.current_package();
        self.registry().has_declared_function(&pkg, name)
    }

    /// Alias of [`Self::has_declared_function`], mirroring
    /// `Interpreter::has_function` (which delegates to `has_declared_function`).
    #[inline]
    pub(crate) fn has_function(&self, name: &str) -> bool {
        self.has_declared_function(name)
    }

    /// VM-native multi-function check, mirroring
    /// `Interpreter::has_multi_function` via the single
    /// [`Registry::has_multi_function`] implementation.
    #[inline]
    pub(crate) fn has_multi_function(&self, name: &str) -> bool {
        let pkg = self.current_package();
        self.registry().has_multi_function(&pkg, name)
    }

    /// The in-scope package name, read out of the VM's own `current_package`
    /// handle as an owned `String` (no `self.interpreter` bounce). The guard is
    /// dropped before returning, so no lock is held across the caller's work.
    #[inline]
    pub(crate) fn current_package(&self) -> String {
        self.current_package.read().unwrap().clone()
    }

    /// Set the in-scope package name through the VM's own handle. Visible to the
    /// interpreter too (same `RwLock`), preserving save/restore semantics across
    /// the VM↔interpreter ping-pong.
    #[inline]
    pub(crate) fn set_current_package(&self, pkg: String) {
        *self.current_package.write().unwrap() = pkg;
    }

    /// Write access to the shared [`IoHandleTable`](crate::runtime::IoHandleTable)
    /// via the VM's own handle (no `self.interpreter` bounce). Same lock and guard
    /// discipline as the interpreter's `io_handles_mut`: never hold the guard
    /// across a re-entrant handle op.
    #[inline]
    pub(crate) fn io_handles_mut(&self) -> crate::runtime::IoHandlesWriteGuard<'_> {
        crate::runtime::IoHandlesWriteGuard::new(&self.io_handles, "io_handles")
    }

    /// Write access to the shared [`OutputSink`](crate::runtime::OutputSink) via
    /// the VM's own handle. Same guard discipline as `io_handles_mut`.
    #[inline]
    pub(crate) fn output_sink_mut(&self) -> crate::runtime::OutputSinkWriteGuard<'_> {
        crate::runtime::OutputSinkWriteGuard::new(&self.output_sink, "output_sink")
    }

    /// VM-native Stdout emit (③後段 PR-C), mirroring `Interpreter::emit_output`:
    /// bump the Stdout-target handle's `bytes_written`, then push to the sink
    /// (immediate real-stdout flush / buffer / thread-clone shared buffer per the
    /// sink's decision). `subtest_active` comes from the interpreter (TAP state
    /// stays interpreter-owned). Build the payload before calling — no guard is
    /// held across re-entrant work.
    pub(crate) fn vm_emit_stdout(&mut self, text: &str) {
        let byte_count = text.len() as i64;
        {
            let mut table = self.io_handles_mut();
            if let Some(h) = table.map.values_mut().find(|h| h.is_stdout_target()) {
                h.add_bytes_written(byte_count);
            }
        }
        let subtest_active = self.interpreter.subtest_active();
        self.output_sink_mut().emit(text, subtest_active);
    }

    /// VM-native Stderr emit (③後段 PR-C), mirroring the `Stderr` branch of
    /// `write_to_handle_value_trying` (immediate real-stderr flush or the stderr
    /// buffer; no `bytes_written` scan, no `output_emitted`).
    pub(crate) fn vm_emit_stderr(&mut self, text: &str) {
        let subtest_active = self.interpreter.subtest_active();
        self.output_sink_mut().emit_stderr(text, subtest_active);
    }

    /// Run the compiled bytecode. Always returns the interpreter back
    /// (even on error) so the caller can restore it.
    pub(crate) fn run(
        mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> (Interpreter, Result<Option<Value>, RuntimeError>) {
        if let Err(e) = Self::validate_labels(code) {
            return (self.interpreter, Err(e));
        }
        // Initialize local variable slots
        self.locals = vec![Value::Nil; code.locals.len()];
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
        self.load_state_locals(code);
        let root_once_scope = self.interpreter.next_once_scope_id();
        self.interpreter.push_once_scope(root_once_scope);
        let mut ip = 0;
        while ip < code.ops.len() {
            if let Err(e) = self.exec_one(code, &mut ip, compiled_fns) {
                if e.is_goto
                    && let Some(label) = e.label.as_deref()
                    && let Some(target_ip) = self.find_label_target(code, label)
                {
                    ip = target_ip;
                    continue;
                }
                if e.is_warn && self.interpreter.control_handler_depth == 0 {
                    if !self.interpreter.warning_suppressed() {
                        self.interpreter.write_warn_to_stderr(&e.message);
                    }
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    ip += 1;
                    continue;
                }
                self.sync_state_locals(code);
                self.interpreter.pop_once_scope();
                // An uncaught CX::Return signal that escapes the top-level
                // VM loop means the lexical target routine was not on the
                // dynamic call stack when `return` executed, so it surfaces
                // as `X::ControlFlow::Return` with out-of-dynamic-scope set.
                // Only perform this conversion when the current dynamic call
                // stack contains no routine — otherwise the return is meant
                // for an enclosing routine that will catch it via its own
                // call-frame handling further up the stack.
                if e.is_return && self.interpreter.routine_stack().is_empty() {
                    let inner_err = RuntimeError::controlflow_return(true);
                    if self.check_phaser_depth > 0 {
                        return (self.interpreter, Err(Self::wrap_in_begin_time(inner_err)));
                    }
                    return (self.interpreter, Err(inner_err));
                }
                if self.check_phaser_depth > 0 {
                    return (self.interpreter, Err(Self::wrap_in_begin_time(e)));
                }
                return (self.interpreter, Err(e));
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        self.sync_state_locals(code);
        self.interpreter.pop_once_scope();
        // Sync local variables back to the interpreter's env so that
        // callers (e.g. eval_block_value) can observe side effects.
        self.sync_env_from_locals(code);
        let last_stack_value = self.stack.last().cloned();
        let fallback = self.last_topic_value;
        (self.interpreter, Ok(last_stack_value.or(fallback)))
    }

    /// Invoke a callable value using the VM fast paths when available and
    /// return the interpreter state to the caller.
    pub(crate) fn call_value(
        mut self,
        target: Value,
        args: Vec<Value>,
    ) -> (Interpreter, Result<Value, RuntimeError>) {
        let result = self.vm_call_on_value(target, args, None);
        (self.interpreter, result)
    }

    /// Run compiled bytecode without consuming self.
    /// Used by map/grep to avoid VM creation/destruction per iteration.
    pub(crate) fn run_reuse(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        Self::validate_labels(code)?;
        self.stack.clear();
        // Initialize local variable slots
        self.locals.resize(code.locals.len(), Value::Nil);
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            } else {
                self.locals[i] = Value::Nil;
            }
        }
        self.load_state_locals(code);
        let root_once_scope = self.interpreter.next_once_scope_id();
        self.interpreter.push_once_scope(root_once_scope);
        let mut ip = 0;
        while ip < code.ops.len() {
            if let Err(e) = self.exec_one(code, &mut ip, compiled_fns) {
                if e.is_goto
                    && let Some(label) = e.label.as_deref()
                    && let Some(target_ip) = self.find_label_target(code, label)
                {
                    ip = target_ip;
                    continue;
                }
                if e.is_warn && self.interpreter.control_handler_depth == 0 {
                    if !self.interpreter.warning_suppressed() {
                        self.interpreter.write_warn_to_stderr(&e.message);
                    }
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    ip += 1;
                    continue;
                }
                self.sync_state_locals(code);
                self.interpreter.pop_once_scope();
                return Err(e);
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        self.sync_state_locals(code);
        self.interpreter.pop_once_scope();
        Ok(())
    }

    /// Resolve a state variable key, applying the current closure scope if set.
    fn scoped_state_key(&self, key: &str) -> String {
        if let Some(id) = self.state_scope_id {
            format!("{key}#c{id}")
        } else {
            key.to_string()
        }
    }

    fn load_state_locals(&mut self, code: &CompiledCode) {
        for (slot, key) in &code.state_locals {
            if let Some(val) = self.interpreter.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }
    }

    fn sync_state_locals(&mut self, code: &CompiledCode) {
        for (slot, key) in &code.state_locals {
            let local_name = &code.locals[*slot];
            let val = self
                .interpreter
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.interpreter.set_state_var(key.clone(), val);
        }
    }

    /// Sync only state variables whose `StateVarInit` opcode falls within
    /// the given instruction range [start..end). This avoids prematurely
    /// syncing state variables that haven't been initialized yet.
    fn sync_state_locals_in_range(&mut self, code: &CompiledCode, start: usize, end: usize) {
        for (slot, key) in &code.state_locals {
            // Check if this exact state variable (by key) has its StateVarInit in the range.
            // We match both slot and key_idx to avoid false matches when multiple
            // state variables share the same local slot.
            let has_init_in_range = code.ops[start..end].iter().any(|op| {
                if let OpCode::StateVarInit(s, k) = op {
                    if *s as usize != *slot {
                        return false;
                    }
                    // Verify the key constant matches
                    if let Value::Str(ref stored_key) = code.constants[*k as usize] {
                        stored_key.as_ref() == key.as_str()
                    } else {
                        false
                    }
                } else {
                    false
                }
            });
            if !has_init_in_range {
                continue;
            }
            let local_name = &code.locals[*slot];
            let val = self
                .interpreter
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.interpreter.set_state_var(key.clone(), val);
        }
    }

    /// Get a reference to the interpreter (for reading env values).
    pub(crate) fn interpreter(&self) -> &Interpreter {
        &self.interpreter
    }

    /// Get a mutable reference to the interpreter (for setting env values).
    pub(crate) fn interpreter_mut(&mut self) -> &mut Interpreter {
        &mut self.interpreter
    }

    pub(crate) fn last_stack_value(&self) -> Option<&Value> {
        if self.stack.len() == 1 {
            self.stack.last()
        } else {
            None
        }
    }

    /// Override the source variable used when mutating `$_` in VM execution.
    pub(crate) fn set_topic_source_var(&mut self, name: Option<String>) {
        self.topic_source_var = name;
    }

    /// Consume the VM and return the interpreter.
    pub(crate) fn into_interpreter(self) -> Interpreter {
        self.interpreter
    }

    /// Execute opcodes in [start..end), used by loop compound opcodes.
    fn run_range(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let mut ip = start;
        while ip < end {
            if let Err(e) = self.exec_one(code, &mut ip, compiled_fns) {
                if e.is_goto
                    && let Some(label) = e.label.as_deref()
                    && let Some(target_ip) = self.find_label_target(code, label)
                    && (start..end).contains(&target_ip)
                {
                    ip = target_ip;
                    continue;
                }
                // Handle warn signals inline when no CONTROL handler is active.
                if e.is_warn && self.interpreter.control_handler_depth == 0 {
                    if !self.interpreter.warning_suppressed() {
                        self.interpreter.write_warn_to_stderr(&e.message);
                    }
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    // If a resume point was recorded for the original warn
                    // site (e.g., when a CONTROL block rethrew the CX::Warn),
                    // resume there so execution continues after the warn
                    // rather than past whatever op propagated the signal.
                    if let Some(resume_point) = self.resume_ip.take() {
                        ip = resume_point;
                    } else {
                        ip += 1;
                    }
                    continue;
                }
                return Err(e);
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        Ok(())
    }

    /// Run LEAVE/KEEP/UNDO phaser queue with per-phaser error guarding.
    /// Each individual LEAVE phaser (delimited by `LeaveGuard` opcodes) is
    /// run independently. If one throws, the error is collected and execution
    /// continues with the next phaser. Collected exceptions are returned as
    /// an `X::PhaserExceptions` error at the end.
    fn run_leave_queue_guarded(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        if start >= end {
            return Ok(());
        }
        // Check if there are any LeaveGuard markers in this range
        let has_guards = (start..end).any(|i| matches!(code.ops[i], OpCode::LeaveGuard { .. }));
        if !has_guards {
            return self.run_range(code, start, end, compiled_fns);
        }

        let mut collected_errors: Vec<RuntimeError> = Vec::new();
        let mut ip = start;
        while ip < end {
            match &code.ops[ip] {
                OpCode::LeaveGuard { next } => {
                    let guard_next = *next as usize;
                    // Run this phaser's body (from ip+1 to guard_next)
                    let result = self.run_range(code, ip + 1, guard_next, compiled_fns);
                    if let Err(e) = result {
                        collected_errors.push(e);
                    }
                    ip = guard_next;
                }
                _ => {
                    // Non-guarded code before the first guard; run normally
                    self.exec_one(code, &mut ip, compiled_fns)?;
                }
            }
        }

        if collected_errors.is_empty() {
            Ok(())
        } else if collected_errors.len() == 1 {
            Err(collected_errors.into_iter().next().unwrap())
        } else {
            // Create X::PhaserExceptions with all collected exceptions
            let exceptions: Vec<Value> = collected_errors
                .iter()
                .map(|e| {
                    if let Some(ex) = e.exception.as_ref() {
                        *ex.clone()
                    } else {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(e.message.clone()));
                        Value::make_instance(crate::symbol::Symbol::intern("Exception"), attrs)
                    }
                })
                .collect();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("exceptions".to_string(), Value::array(exceptions));
            let exception =
                Value::make_instance(crate::symbol::Symbol::intern("X::PhaserExceptions"), attrs);
            let mut err = RuntimeError::new("Multiple exceptions in LEAVE phasers".to_string());
            err.exception = Some(Box::new(exception));
            Err(err)
        }
    }

    fn find_label_target(&self, code: &CompiledCode, label: &str) -> Option<usize> {
        code.ops.iter().enumerate().find_map(|(i, op)| match op {
            OpCode::Label(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                if name == label { Some(i + 1) } else { None }
            }
            _ => None,
        })
    }

    /// Execute one opcode at *ip, advancing ip for the next instruction.
    fn exec_one(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        crate::trace::trace_log!(
            "vm",
            "exec_one[{}]: {:?}",
            ip,
            std::mem::discriminant(&code.ops[*ip])
        );
        match &code.ops[*ip] {
            // -- Constants --
            OpCode::LoadConst(idx) => {
                self.stack.push(code.constants[*idx as usize].clone());
                *ip += 1;
            }
            OpCode::LoadNil => {
                self.stack.push(Value::Nil);
                *ip += 1;
            }
            OpCode::LoadTrue => {
                self.stack.push(Value::Bool(true));
                *ip += 1;
            }
            OpCode::LoadFalse => {
                self.stack.push(Value::Bool(false));
                *ip += 1;
            }

            // -- Variables --
            OpCode::GetGlobal(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                if name == "?CALLER::LINE" {
                    let line = self.interpreter.get_caller_line(1).unwrap_or(Value::Nil);
                    self.stack.push(line);
                    *ip += 1;
                    return Ok(());
                }
                // $*THREAD: dynamically create a Thread instance with current thread ID
                if name == "*THREAD" || name == "$*THREAD" {
                    self.stack.push(Self::make_thread_instance());
                    *ip += 1;
                    return Ok(());
                }
                // Atomic-variable read: only possible once some `atomicint`/atomic
                // storage has been registered. Skip the whole check (a `format!`
                // plus two `var_type_constraint` lookups) on the hot read path when
                // no atomics exist, which is the overwhelmingly common case.
                if self.interpreter.atomic_var_seen() {
                    let atomic_name = name.strip_prefix('$').unwrap_or(name);
                    let atomic_name_key = format!("__mutsu_atomic_name::{atomic_name}");
                    let is_atomic_int = self.interpreter.var_type_constraint(name).as_deref()
                        == Some("atomicint")
                        || self.interpreter.var_type_constraint(atomic_name).as_deref()
                            == Some("atomicint")
                        || self.interpreter.get_shared_var(&atomic_name_key).is_some();
                    if is_atomic_int {
                        let fetched = self.interpreter.call_function(
                            "__mutsu_atomic_fetch_var",
                            vec![Value::str(atomic_name.to_string())],
                        )?;
                        self.stack.push(fetched);
                        *ip += 1;
                        return Ok(());
                    }
                }
                // Phase 3 Stage 2c (ii): a sigilless attribute (`has $x`) compiles
                // to a bare `Var("x")` that reads via GetGlobal (it is not a method
                // local), so route it to `self`'s shared cell here too — otherwise
                // a read after a nested-frame mutation sees the stale entry copy.
                // `read_self_attr_cell` is gated on `sigilless_attrs_active`, so
                // non-sigilless programs pay only a string check.
                if let Some(cell_val) = self.read_self_attr_cell(name) {
                    self.stack.push(cell_val);
                    *ip += 1;
                    return Ok(());
                }
                let val = self
                    .get_env_with_main_alias(name)
                    .or_else(|| {
                        // Fall back to the persistent our_vars store for `our`-scoped
                        // variables accessed via package-qualified names (e.g., $Pkg::var).
                        // Bare variable names should NOT fall back to our_vars — the
                        // lexical alias for `our` variables is block-scoped.
                        if name.contains("::") {
                            self.interpreter
                                .get_our_var(name)
                                .cloned()
                                .or_else(|| self.our_var_pseudo_unqualified(name))
                                .or_else(|| {
                                    // Nested package shorthand: when looking up
                                    // `$D2::d3` from inside package `D1::D2` (or any
                                    // ancestor of it), also try the fully-qualified
                                    // forms by prepending each ancestor prefix.
                                    let cur = self.current_package().to_string();
                                    if cur.is_empty() || cur == "GLOBAL" {
                                        return None;
                                    }
                                    let (sigil, bare) = if let Some(rest) = name.strip_prefix('$') {
                                        ("$", rest)
                                    } else if let Some(rest) = name.strip_prefix('@') {
                                        ("@", rest)
                                    } else if let Some(rest) = name.strip_prefix('%') {
                                        ("%", rest)
                                    } else if let Some(rest) = name.strip_prefix('&') {
                                        ("&", rest)
                                    } else {
                                        ("", name)
                                    };
                                    // Walk up the current package, trying each prefix
                                    // joined with the requested name.
                                    let parts: Vec<&str> = cur.split("::").collect();
                                    for i in (0..=parts.len()).rev() {
                                        let prefix = parts[..i].join("::");
                                        let candidate = if prefix.is_empty() {
                                            format!("{sigil}{bare}")
                                        } else {
                                            format!("{sigil}{prefix}::{bare}")
                                        };
                                        if candidate == name {
                                            continue;
                                        }
                                        if let Some(v) =
                                            self.interpreter.get_our_var(&candidate).cloned()
                                        {
                                            return Some(v);
                                        }
                                        if let Some(v) = self.get_env_with_main_alias(&candidate) {
                                            return Some(v);
                                        }
                                    }
                                    None
                                })
                        } else {
                            None
                        }
                    })
                    .or_else(|| {
                        // Outer-lexical fallback: when a package-qualified name
                        // (e.g. `A::x`) is not found in any package store, fall
                        // back to looking up just the bare component (`x`) in env.
                        // This handles class body statements that access outer
                        // lexical variables which are stored in env under their
                        // unqualified names (not as `A::x`).
                        if !name.contains("::") {
                            return None;
                        }
                        // Only apply when the qualifier matches the current package
                        // (i.e. the name was auto-qualified by the compiler, not
                        // explicitly written as a package-qualified access).
                        let cur = self.current_package().to_string();
                        if cur.is_empty() || cur == "GLOBAL" {
                            return None;
                        }
                        // Extract bare component after the last `::`
                        let bare = if let Some(pos) = name.rfind("::") {
                            &name[pos + 2..]
                        } else {
                            return None;
                        };
                        if bare.is_empty() {
                            return None;
                        }
                        self.get_env_with_main_alias(bare)
                    })
                    .or_else(|| {
                        // Bare-name fallback: when looking up an unqualified
                        // name (e.g. `msg` or `$msg`) inside a routine whose
                        // current_package is a real package (e.g. `Gee`), try
                        // resolving via the package's `our` store. This makes
                        // `our $msg` accessible from `our sub talk { $msg }`
                        // when `talk` is invoked from outside the package.
                        if name.contains("::") {
                            return None;
                        }
                        let cur = self.current_package().to_string();
                        if cur.is_empty() || cur == "GLOBAL" || cur.contains("::&") {
                            return None;
                        }
                        // Skip special names that shouldn't be package-qualified.
                        let bare_first = name.trim_start_matches(['$', '@', '%', '&']);
                        if bare_first.is_empty() {
                            return None;
                        }
                        let first_ch = bare_first.chars().next().unwrap();
                        if matches!(first_ch, '_' | '/' | '!' | '?' | '*' | '.' | '=')
                            || first_ch.is_ascii_digit()
                        {
                            return None;
                        }
                        let candidate = if let Some(rest) = name.strip_prefix('$') {
                            format!("${cur}::{rest}")
                        } else if let Some(rest) = name.strip_prefix('@') {
                            format!("@{cur}::{rest}")
                        } else if let Some(rest) = name.strip_prefix('%') {
                            format!("%{cur}::{rest}")
                        } else if let Some(rest) = name.strip_prefix('&') {
                            format!("&{cur}::{rest}")
                        } else {
                            format!("{cur}::{name}")
                        };
                        self.interpreter
                            .get_our_var(&candidate)
                            .cloned()
                            .or_else(|| self.get_env_with_main_alias(&candidate))
                    })
                    // Anonymous state variable (`$`): fall back to persisted
                    // state so the value survives across closure calls.
                    .or_else(|| self.anon_state_value(name))
                    .map(Ok)
                    .unwrap_or_else(|| {
                        if name.starts_with('^') {
                            Ok(Value::Bool(true))
                        } else if name == "self" || name.ends_with("::self") {
                            Err(RuntimeError::new(
                                "'self' used where no object is available".to_string(),
                            ))
                        } else if name.starts_with('!')
                            && name.len() > 1
                            && name[1..]
                                .chars()
                                .next()
                                .is_some_and(|c| c.is_alphanumeric() || c == '_')
                        {
                            if self.get_env_with_main_alias("self").is_some() {
                                Ok(Value::Nil)
                            } else {
                                Err(RuntimeError::new(format!(
                                    "Variable $!{} used where no 'self' is available",
                                    &name[1..]
                                )))
                            }
                        } else {
                            Ok(Value::Nil)
                        }
                    })?;
                // When the value is Nil and the variable has a type constraint,
                // return the type object (consistent with GetLocal behavior).
                let val = if matches!(val, Value::Nil) {
                    if let Some(def) = self.interpreter.var_default(name) {
                        def.clone()
                    } else if let Some(constraint) =
                        self.interpreter.var_type_constraint_fast(name).cloned()
                    {
                        let nominal = self
                            .interpreter
                            .nominal_type_object_name_for_constraint(&constraint);
                        Value::Package(Symbol::intern(&nominal))
                    } else {
                        val
                    }
                } else {
                    val
                };
                // Force lazy thunks transparently on access
                let val = if let Value::LazyThunk(ref thunk_data) = val {
                    self.force_lazy_thunk(thunk_data)?
                } else {
                    val
                };
                // Auto-deref ContainerRef for stack use (ContainerRef axis of the
                // decont family; moves through for the common non-container case).
                let val = val.into_deref();
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetSelfOrNoSelf(name_idx) => {
                // Load `self` for a `$.attr` accessor from the captured env.
                if let Some(self_val) = self.get_env_with_main_alias("self") {
                    self.stack.push(self_val);
                    *ip += 1;
                } else {
                    // No enclosing method/submethod: X::Syntax::NoSelf.
                    let variable = Self::const_str(code, *name_idx).to_string();
                    let message =
                        format!("Variable {} used where no 'self' is available", variable);
                    let mut err = RuntimeError::new(message.clone());
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("variable".to_string(), Value::str(variable));
                    attrs.insert("message".to_string(), Value::str(message));
                    err.exception = Some(Box::new(Value::make_instance(
                        Symbol::intern("X::Syntax::NoSelf"),
                        attrs,
                    )));
                    return Err(err);
                }
            }
            OpCode::GetArrayVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                // Reject @!attr (private attribute twigil) when no self is available
                if let Some(bare) = name.strip_prefix("@!")
                    && !bare.is_empty()
                    && bare.as_bytes()[0].is_ascii_alphabetic()
                    && self.get_env_with_main_alias("self").is_none()
                {
                    return Err(RuntimeError::new(format!(
                        "X::Syntax::NoSelf: Variable {} used where no 'self' is available",
                        name
                    )));
                }
                // Phase 3 Stage 2b: array attributes (`@!a`/`@.a`) read straight
                // from `self`'s shared cell so a mutation in a nested method frame
                // is visible here.
                if let Some(cell_val) = self.read_self_attr_cell(name) {
                    let val = match cell_val {
                        Value::Hash(ref map) => Value::real_array(
                            map.iter()
                                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                                .collect(),
                        ),
                        other => other,
                    };
                    self.stack.push(val);
                    *ip += 1;
                    return Ok(());
                }
                let val = self
                    .get_env_with_main_alias(name)
                    .or_else(|| self.get_local_by_bare_name(code, name))
                    .or_else(|| {
                        // Fallback: check bare name in env (for closures capturing params)
                        name.strip_prefix('@')
                            .and_then(|bare| self.interpreter.env().get(bare).cloned())
                    })
                    .or_else(|| {
                        // Fallback for fast-path method dispatch (skip_env_setup=true):
                        // @.attr and @!attr are not set in env, so read directly from
                        // self's instance attributes when available.
                        let attr_name = name
                            .strip_prefix("@.")
                            .or_else(|| name.strip_prefix("@!"))?;
                        if attr_name.is_empty() {
                            return None;
                        }
                        let self_val = self.get_env_with_main_alias("self")?;
                        if let Value::Instance { attributes, .. } = &self_val {
                            attributes.as_map().get(attr_name).cloned()
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| {
                        // Anonymous @-sigil variables default to empty Array
                        if name.contains("__ANON_ARRAY_") || name == "@__ANON_ARRAY__" {
                            Value::real_array(vec![])
                        } else {
                            Value::Nil
                        }
                    });
                // A whole-container `:=` bind (`my @b := @a`) stores a shared
                // `ContainerRef` cell in the slot so both aliases observe
                // mutations. Decontainerize the top-level cell here so the read
                // yields the inner Array/Hash (the cell is a binding alias, not
                // an array element). Element-level cells are handled at Index.
                let val = val.into_deref();
                // When @-sigil dereferences a Hash, convert to a list of pairs
                let val = match val {
                    Value::Hash(ref map) => {
                        let pairs: Vec<Value> = map
                            .iter()
                            .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                            .collect();
                        Value::real_array(pairs)
                    }
                    other => other,
                };
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetHashVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                // Reject %!attr (private attribute twigil) when no self is available
                if let Some(bare) = name.strip_prefix("%!")
                    && !bare.is_empty()
                    && bare.as_bytes()[0].is_ascii_alphabetic()
                    && self.get_env_with_main_alias("self").is_none()
                {
                    return Err(RuntimeError::new(format!(
                        "X::Syntax::NoSelf: Variable {} used where no 'self' is available",
                        name
                    )));
                }
                // %?RESOURCES — build from the current package's distribution context
                if name == "%?RESOURCES" {
                    let resources = self.interpreter.build_resources_for_package();
                    self.stack.push(resources);
                    *ip += 1;
                    return Ok(());
                }
                // Phase 3 Stage 2b: hash attributes (`%!h`/`%.h`) read straight
                // from `self`'s shared cell (cross-frame visibility).
                if let Some(cell_val) = self.read_self_attr_cell(name) {
                    self.stack.push(cell_val);
                    *ip += 1;
                    return Ok(());
                }
                let val = self
                    .get_env_with_main_alias(name)
                    .or_else(|| self.get_local_by_bare_name(code, name))
                    .or_else(|| {
                        name.strip_prefix('%')
                            .and_then(|bare| self.interpreter.env().get(bare).cloned())
                    })
                    .or_else(|| {
                        // Fallback for fast-path method dispatch (skip_env_setup=true):
                        // %.attr and %!attr are not set in env, so read directly from
                        // self's instance attributes when available.
                        let attr_name = name
                            .strip_prefix("%.")
                            .or_else(|| name.strip_prefix("%!"))?;
                        if attr_name.is_empty() {
                            return None;
                        }
                        let self_val = self.get_env_with_main_alias("self")?;
                        if let Value::Instance { attributes, .. } = &self_val {
                            attributes.as_map().get(attr_name).cloned()
                        } else {
                            None
                        }
                    });
                match val {
                    // Decontainerize a top-level `ContainerRef` cell from a
                    // whole-container `:=` bind (`my %h2 := %h`); the read
                    // yields the inner Hash.
                    Some(v) => self.stack.push(v.into_deref()),
                    None => {
                        // %ENV (without * twigil) is not declared in Raku;
                        // only %*ENV is valid. Throw an undeclared error for %ENV specifically.
                        if name == "%ENV" {
                            return Err(RuntimeError::undeclared("name", "%ENV"));
                        }
                        // Anonymous %-sigil variables default to empty Hash
                        if name == "%__ANON_HASH__" {
                            self.stack
                                .push(Value::hash(std::collections::HashMap::new()));
                        } else {
                            self.stack.push(Value::Nil);
                        }
                    }
                }
                *ip += 1;
            }
            OpCode::GetBareWord(name_idx) => {
                self.exec_get_bare_word_op(code, *name_idx, compiled_fns)?;
                *ip += 1;
            }
            OpCode::GetPseudoStash(name_idx) => {
                self.exec_get_pseudo_stash_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::GetOurVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self
                    .interpreter
                    .get_our_var(name)
                    .cloned()
                    .or_else(|| self.get_env_with_main_alias(name))
                    .unwrap_or(Value::Nil);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::SetGlobalRaw(name_idx) | OpCode::SetGlobal(name_idx) => {
                let raw_mode = matches!(code.ops[*ip], OpCode::SetGlobalRaw(_));
                let is_rebind = self.rebind_context;
                self.bind_context = false;
                // Only clear rebind_context if this is actually a binding operation
                if is_rebind {
                    self.rebind_context = false;
                }
                let name_str = match &code.constants[*name_idx as usize] {
                    Value::Str(s) => s.as_str(),
                    _ => unreachable!("SetGlobal name must be a string constant"),
                };
                // Fast path for the anonymous state scalar (`$` and `$.` desugaring).
                // `__ANON_STATE__` is a synthetic internal name that can never be a
                // private attribute, package/class, sigilless-bound alias, or strict-
                // undeclared symbol, so the heavy general store path below (including
                // the O(env) reverse-alias scan, shared-var sync, and our-var store)
                // is unnecessary. This is extremely hot in tight loops assigning to `$`.
                // The remaining special cases (typed/readonly anon scalar, fatal-mode
                // Failure explosion, `:=` container write-through, capture RHS) are
                // excluded by the guards and fall through to the general path.
                if name_str == "__ANON_STATE__"
                    && !raw_mode
                    && !is_rebind
                    && !self.interpreter.fatal_mode
                    && self
                        .interpreter
                        .var_type_constraint_fast(name_str)
                        .is_none()
                    && !self.interpreter.is_readonly(name_str)
                    && !matches!(self.stack.last(), Some(Value::Capture { .. }))
                    && !matches!(
                        self.interpreter.env().get(name_str),
                        Some(Value::ContainerRef(_))
                    )
                {
                    let val = self.stack.pop().unwrap_or(Value::Nil);
                    // Preserve `$` state persistence across closure calls.
                    self.sync_anon_state_value("__ANON_STATE__", &val);
                    let sym = Symbol::intern("__ANON_STATE__");
                    if let Some(slot) = self.interpreter.env_mut().get_mut_sym(sym) {
                        *slot = val;
                    } else {
                        self.interpreter.env_mut().insert_sym(sym, val);
                    }
                    *ip += 1;
                    return Ok(());
                }
                let name = name_str.to_string();
                // Reject private attribute twigil (!) assignment when no self is available
                {
                    let bare = name.trim_start_matches(['$', '@', '%', '&']);
                    if bare.starts_with('!')
                        && bare.len() > 1
                        && bare.as_bytes()[1].is_ascii_alphabetic()
                        && self.get_env_with_main_alias("self").is_none()
                    {
                        // Reconstruct the display name with sigil
                        let display = if name.starts_with('!') {
                            format!("${}", name)
                        } else {
                            name.clone()
                        };
                        return Err(RuntimeError::new(format!(
                            "X::Syntax::NoSelf: Variable {} used where no 'self' is available",
                            display
                        )));
                    }
                }
                if self.interpreter.strict_mode
                    && !name.contains("::")
                    && !self.interpreter.env().contains_key(&name)
                {
                    return Err(self.strict_undeclared_error(&name));
                }
                // Check readonly variables (e.g., $*USAGE).
                // Skip readonly check for SetGlobalRaw which is used for constant
                // declarations — the constant will be re-marked readonly after this.
                if !raw_mode {
                    self.interpreter.check_readonly_for_modify(&name)?;
                } else {
                    // Clear any previous readonly marking so this constant
                    // redeclaration can proceed (e.g., `constant sym` followed
                    // by `constant $sym` which share the same env name).
                    let bare = name
                        .rsplit("::")
                        .next()
                        .unwrap_or(&name)
                        .trim_start_matches(['$', '@', '%', '&']);
                    self.interpreter.unmark_readonly(bare);
                }
                // Prevent re-assignment of immutable containers (Mix, Set, Bag)
                // Only when the variable has an explicit immutable type constraint
                // (e.g., `my %h is Mix`), not for regular scalar variables holding
                // an immutable value.
                if let Some(constraint) = self.interpreter.var_type_constraint(&name) {
                    let base = constraint.split('[').next().unwrap_or(&constraint);
                    if matches!(base, "Mix" | "Set" | "Bag")
                        && let Some(existing) = self.interpreter.env().get(&name)
                        && matches!(
                            existing,
                            Value::Mix(_, false) | Value::Set(_, false) | Value::Bag(_, false)
                        )
                    {
                        let type_name = match existing {
                            Value::Mix(..) => "Mix",
                            Value::Set(..) => "Set",
                            Value::Bag(..) => "Bag",
                            _ => unreachable!(),
                        };
                        return Err(RuntimeError::new(format!(
                            "Cannot modify an immutable {} ({})",
                            type_name,
                            existing.to_string_value()
                        )));
                    }
                }
                // Reject assignment to immutable type objects (e.g., `Foo .= new`)
                if !name.starts_with('$')
                    && !name.starts_with('@')
                    && !name.starts_with('%')
                    && !name.starts_with('&')
                    && !name.contains("::")
                    && matches!(self.interpreter.env().get(&name), Some(Value::Package(_)))
                    && self.interpreter.has_class(&name)
                {
                    return Err(RuntimeError::new(format!(
                        "Cannot modify an immutable '{}' type object",
                        name
                    )));
                }
                let raw_val = self.stack.pop().unwrap_or(Value::Nil);
                let (raw_val, bind_source) = if let Value::Capture { positional, named } = &raw_val
                {
                    if positional.is_empty() {
                        if let (Some(Value::Str(source_name)), Some(inner)) = (
                            named.get("__mutsu_varref_name"),
                            named.get("__mutsu_varref_value"),
                        ) {
                            (inner.clone(), Some(source_name.to_string()))
                        } else {
                            (raw_val, None)
                        }
                    } else {
                        (raw_val, None)
                    }
                } else {
                    (raw_val, None)
                };
                let mut val = if raw_mode && name.starts_with('@') {
                    // Constants with @ sigil coerce to List (not Array).
                    // `constant @x = 42` gives `(42,)`, not `[42]`.
                    // Explicit Arrays ([1,2,3]) are preserved.
                    // Instance objects that do Positional are kept as-is
                    // (they already went through CoerceToList).
                    match raw_val {
                        Value::Array(items, kind) if kind.is_real_array() => {
                            Value::Array(items, kind)
                        }
                        Value::Array(items, _) => {
                            Value::Array(items, crate::value::ArrayKind::List)
                        }
                        Value::Instance { ref class_name, .. } => {
                            let cn = class_name.resolve();
                            let does_positional = matches!(
                                cn.as_str(),
                                "Array"
                                    | "List"
                                    | "Slip"
                                    | "Seq"
                                    | "Range"
                                    | "Buf"
                                    | "Blob"
                                    | "utf8"
                                    | "buf8"
                                    | "buf16"
                                    | "buf32"
                            ) || self
                                .interpreter
                                .class_composed_roles(&cn)
                                .is_some_and(|roles| roles.iter().any(|r| r == "Positional"));
                            if does_positional {
                                raw_val
                            } else {
                                Value::Array(
                                    std::sync::Arc::new(crate::value::ArrayData::new(vec![
                                        raw_val,
                                    ])),
                                    crate::value::ArrayKind::List,
                                )
                            }
                        }
                        other => Value::Array(
                            std::sync::Arc::new(crate::value::ArrayData::new(vec![other])),
                            crate::value::ArrayKind::List,
                        ),
                    }
                } else if raw_mode && name.starts_with('%') {
                    // `constant %x` coerces non-Associative values to Map.
                    self.coerce_constant_hash_value(&name, raw_val)?
                } else if raw_mode {
                    raw_val
                } else if name.starts_with('%') {
                    // Apply quant-hash (SetHash/BagHash/MixHash) coercion first
                    // so that typed container assignment sees a Set/Bag/Mix,
                    // not a Hash with element-level type errors.
                    self.coerce_hash_var_value(&name, raw_val)?
                } else if name.starts_with('@') {
                    runtime::coerce_to_array(raw_val)
                } else {
                    raw_val
                };
                if (name.starts_with('@') || name.starts_with('%'))
                    && (self.interpreter.var_type_constraint(&name).is_some()
                        || self.interpreter.var_hash_key_constraint(&name).is_some())
                {
                    val = self.coerce_typed_container_assignment(&name, val, false)?;
                }
                if let Some(constraint) = self.interpreter.var_type_constraint(&name)
                    && !name.starts_with('%')
                    && !name.starts_with('@')
                {
                    if !matches!(val, Value::Nil)
                        && !self.interpreter.type_matches_value(&constraint, &val)
                    {
                        // When assigning an unhandled Failure to a typed variable
                        // that can't hold it, explode the Failure first (Raku behavior)
                        if let Value::Instance { class_name, .. } = &val
                            && class_name.resolve() == "Failure"
                            && !val.is_failure_handled()
                            && let Some(err) =
                                self.interpreter.failure_to_runtime_error_if_unhandled(&val)
                        {
                            return Err(err);
                        }
                        return Err(runtime::utils::type_check_assignment_typed_error(
                            &name,
                            &constraint,
                            &val,
                        ));
                    }
                    if !matches!(val, Value::Nil) {
                        val = self
                            .interpreter
                            .try_coerce_value_for_constraint(&constraint, val)?;
                    }
                    // Wrap native integer values on assignment (overflow wrapping)
                    val = Self::wrap_native_int_by_constraint(&constraint, val)?;
                }
                if self.interpreter.fatal_mode
                    && !name.contains("__mutsu_")
                    && let Some(err) = self.interpreter.failure_to_runtime_error_if_unhandled(&val)
                {
                    return Err(err);
                }
                let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
                let alias_key = format!("__mutsu_sigilless_alias::{}", name);
                if matches!(
                    self.interpreter.env().get(&readonly_key),
                    Some(Value::Bool(true))
                ) && !matches!(self.interpreter.env().get(&alias_key), Some(Value::Str(_)))
                {
                    return Err(RuntimeError::assignment_ro(None));
                }
                if let Some(source_name) = bind_source {
                    let mut resolved_source = source_name.clone();
                    let mut seen = std::collections::HashSet::new();
                    while seen.insert(resolved_source.clone()) {
                        let key = format!("__mutsu_sigilless_alias::{}", resolved_source);
                        let Some(Value::Str(next)) = self.interpreter.env().get(&key) else {
                            break;
                        };
                        resolved_source = next.to_string();
                    }
                    self.interpreter
                        .env_mut()
                        .insert(alias_key.clone(), Value::str(resolved_source.clone()));
                    // Propagate readonly status from the source variable.
                    // Binding to a readonly parameter should make the target
                    // readonly as well (persisted in env for cross-scope survival).
                    let source_readonly = self.interpreter.is_readonly(&source_name);
                    self.interpreter
                        .env_mut()
                        .insert(readonly_key.clone(), Value::Bool(source_readonly));
                    if source_readonly {
                        self.interpreter.mark_readonly(&name);
                    }
                    // Create a shared ContainerRef for cross-scope binding persistence.
                    if !name.starts_with('@')
                        && !name.starts_with('%')
                        && !name.starts_with('&')
                        && !source_readonly
                    {
                        let container = if let Value::ContainerRef(ref arc) = val {
                            Value::ContainerRef(arc.clone())
                        } else {
                            val.clone().into_container_ref()
                        };
                        // Store ContainerRef in target and source env
                        self.set_env_with_main_alias(&name, container.clone());
                        self.interpreter
                            .env_mut()
                            .insert(resolved_source.clone(), container.clone());
                        // If the target is an attribute alias (`has $x` makes `x`
                        // an alias for `!x`), also store the ContainerRef under
                        // the private attribute key so writeback picks it up when
                        // the method returns. Check via the reverse alias:
                        // `__mutsu_sigilless_alias::!x` → `"x"`.
                        {
                            let reverse_key = format!("__mutsu_sigilless_alias::!{}", name);
                            if let Some(Value::Str(ref target)) =
                                self.interpreter.env().get(&reverse_key).cloned()
                                && target.as_str() == name
                            {
                                let priv_key = format!("!{}", name);
                                self.interpreter
                                    .env_mut()
                                    .insert(priv_key, container.clone());
                            }
                        }
                        // When rebinding to a new source, the old alias target
                        // keeps its existing value/ContainerRef — we only break
                        // the alias, we do NOT propagate the new ContainerRef to
                        // the old target.
                        // Update source local if present
                        if let Some(source_idx) =
                            code.locals.iter().rposition(|n| n == &resolved_source)
                        {
                            self.locals[source_idx] = container.clone();
                            self.flush_local_to_env(code, source_idx);
                        }
                        // Propagate to saved call frames (env AND locals)
                        for frame in self.call_frames.iter_mut().rev() {
                            if frame.saved_env.contains_key(&resolved_source) {
                                frame
                                    .saved_env
                                    .insert(resolved_source.clone(), container.clone());
                            }
                            // Also update saved locals so a later restore doesn't
                            // overwrite the ContainerRef with a stale plain value.
                            for (i, local_name) in code.locals.iter().enumerate() {
                                if local_name == &resolved_source && i < frame.saved_locals.len() {
                                    frame.saved_locals[i] = container.clone();
                                }
                            }
                        }
                        // Persist ContainerRef in our_vars for `our` variables.
                        // Store under both the bare name and any existing
                        // package-qualified variants (e.g., "K::x" for bare "x")
                        // so GetGlobal fallback (which uses qualified keys) can
                        // find the binding.
                        self.interpreter
                            .set_our_var(name.clone(), container.clone());
                        // Update the package-qualified our_var key (e.g., "K::x"
                        // for bare "x" in class K) so GetGlobal fallback can find
                        // the binding. Only match the exact class from the method
                        // class stack to avoid clobbering unrelated package vars.
                        if let Some(method_class) = self.interpreter.method_class_stack_top() {
                            let qualified = format!("{}::{}", method_class, name);
                            if self.interpreter.get_our_var(&qualified).is_some() {
                                self.interpreter
                                    .set_our_var(qualified.clone(), container.clone());
                                self.interpreter
                                    .env_mut()
                                    .insert(qualified, container.clone());
                            }
                        }
                        *ip += 1;
                        return Ok(());
                    }
                    // Record pending alias bind for the caller to create
                    // local_bind_pairs after the closure returns.
                    if !source_readonly {
                        self.pending_alias_bind_names
                            .push((name.clone(), resolved_source));
                    }
                }
                // Write through ContainerRef: update inner value for env-based variables.
                // Return early to avoid overwriting the ContainerRef in env with a plain value.
                if !is_rebind && !raw_mode {
                    // Check env directly (not through alias resolution to avoid circular lookups)
                    if let Some(Value::ContainerRef(arc)) =
                        self.interpreter.env().get(&name).cloned()
                    {
                        arc.lock().unwrap().clone_from(&val);
                        *ip += 1;
                        return Ok(());
                    }
                    // Also check alias target for sigilless attributes
                    let alias_key_check = format!("__mutsu_sigilless_alias::{}", name);
                    if let Some(Value::Str(alias_target)) =
                        self.interpreter.env().get(&alias_key_check).cloned()
                        && let Some(Value::ContainerRef(arc)) =
                            self.interpreter.env().get(alias_target.as_str()).cloned()
                    {
                        arc.lock().unwrap().clone_from(&val);
                        *ip += 1;
                        return Ok(());
                    }
                }
                if raw_mode && name.starts_with('@') {
                    // For `constant @x`, bypass set_shared_var's List→Array
                    // normalization so the container type (List) is preserved.
                    self.interpreter.env_mut().insert(name.clone(), val.clone());
                } else {
                    self.set_env_with_main_alias(&name, val.clone());
                }
                // Persist anonymous state variable (`$`) so it survives
                // across closure calls (e.g. `$ ~= $_` in classify block).
                self.sync_anon_state_value(&name, &val);
                // Persist `our`-scoped variables so they survive block-scope
                // restoration (which only preserves env keys that existed
                // before the block).  `::('name')` falls back to this store.
                self.interpreter.set_our_var(name.clone(), val.clone());
                // Track topic mutations for map rw writeback
                if name == "_" {
                    self.interpreter
                        .env_mut()
                        .insert("__mutsu_rw_map_topic__".to_string(), val.clone());
                }
                // Sync to shared_vars for cross-thread visibility.
                // Skip for raw_mode @-variables to preserve List kind.
                if !(raw_mode && name.starts_with('@')) {
                    self.interpreter.set_shared_var(&name, val.clone());
                }
                let mut alias_name = self.interpreter.env().get(&alias_key).and_then(|v| {
                    if let Value::Str(name) = v {
                        Some(name.to_string())
                    } else {
                        None
                    }
                });
                let mut seen_aliases = std::collections::HashSet::new();
                while let Some(current_alias) = alias_name {
                    if !seen_aliases.insert(current_alias.clone()) {
                        break;
                    }
                    self.set_env_with_main_alias(&current_alias, val.clone());
                    self.update_local_if_exists(code, &current_alias, &val);
                    // Sigilless attribute write: mirror an attr-twigil alias (`!x`)
                    // into self's shared cell so a same-method cell-direct read of
                    // the sigilless attr sees the new value (Phase 3 Stage 2c (ii)).
                    self.write_self_attr_cell(&current_alias, val.clone());
                    let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
                    alias_name = self.interpreter.env().get(&next_key).and_then(|v| {
                        if let Value::Str(name) = v {
                            Some(name.to_string())
                        } else {
                            None
                        }
                    });
                }
                if name == "_"
                    && let Some(ref source_var) = self.topic_source_var
                    && !source_var.starts_with('@')
                    && !source_var.starts_with('%')
                {
                    let source_name = source_var.clone();
                    self.set_env_with_main_alias(&source_name, val.clone());
                    self.update_local_if_exists(code, &source_name, &val);
                }
                // Reverse alias propagation: find all variables that are
                // bound TO this variable (i.e. `my $x := $name`) and update
                // them so the alias stays in sync.
                {
                    let prefix = "__mutsu_sigilless_alias::";
                    let reverse_targets: Vec<String> = self
                        .interpreter
                        .env()
                        .iter()
                        .filter_map(|(k, v)| {
                            if let Some(var_name) = k.strip_prefix_str(prefix)
                                && let Value::Str(target) = v
                                && target.as_str() == name
                            {
                                Some(var_name)
                            } else {
                                None
                            }
                        })
                        .collect();
                    for target_var in reverse_targets {
                        self.set_env_with_main_alias(&target_var, val.clone());
                        self.update_local_if_exists(code, &target_var, &val);
                    }
                }
                *ip += 1;
            }
            OpCode::SetVarType { name_idx, tc_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                let raw_constraint = Self::const_str(code, *tc_idx).to_string();
                // Resolve type capture variables (e.g., `T` → `Int` when `::T`
                // was captured earlier in the signature).
                let constraint = self.interpreter.resolved_type_capture_name(&raw_constraint);
                // Clear stale atomic CAS state when an @-variable is
                // (re-)declared with a type constraint like atomicint.
                if name.starts_with('@') && constraint == "atomicint" {
                    self.interpreter.clear_atomic_array_state(&name);
                }
                self.interpreter
                    .set_var_type_constraint(&name, Some(constraint.clone()));
                // For scalar variables, if the current value is Nil, set it to the type object.
                // Exception: if the constraint is "Nil", keep the value as Value::Nil
                // (the Nil type object is Value::Nil, not Value::Package("Nil")).
                if !name.starts_with('@') && !name.starts_with('%') && constraint != "Nil" {
                    let is_nil =
                        matches!(self.interpreter.env().get(&name), Some(Value::Nil) | None);
                    if is_nil {
                        // Native types get zero/empty defaults instead of type objects.
                        let init_val =
                            if crate::runtime::native_types::is_native_int_type(&constraint) {
                                Value::Int(0)
                            } else if matches!(constraint.as_str(), "num" | "num32" | "num64") {
                                Value::Num(0.0)
                            } else if constraint == "str" {
                                Value::Str(String::new().into())
                            } else {
                                Value::Package(Symbol::intern(
                                    &self
                                        .interpreter
                                        .var_type_constraint(&name)
                                        .unwrap_or(constraint.clone()),
                                ))
                            };
                        self.set_env_with_main_alias(&name, init_val.clone());
                        self.update_local_if_exists(code, &name, &init_val);
                    }
                } else if let Some(value) = self.get_env_with_main_alias(&name) {
                    let info = crate::runtime::ContainerTypeInfo {
                        value_type: self
                            .interpreter
                            .var_type_constraint(&name)
                            .unwrap_or(constraint),
                        key_type: if name.starts_with('%') {
                            self.interpreter.var_hash_key_constraint(&name)
                        } else {
                            None
                        },
                        declared_type: None,
                    };
                    // Hashes embed metadata in `HashData`; write the tagged value
                    // back (no-op Arc for array/instance side-table containers).
                    let tagged = self.interpreter.tag_container_metadata(value, info);
                    self.set_env_with_main_alias(&name, tagged.clone());
                    self.update_local_if_exists(code, &name, &tagged);
                }
                *ip += 1;
            }
            OpCode::SetTopic => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                self.last_topic_value = Some(val.clone());
                self.interpreter.env_mut().insert("_".to_string(), val);
                *ip += 1;
            }
            OpCode::SaveTopic => {
                let current = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                self.topic_save_stack.push(current);
                *ip += 1;
            }
            OpCode::RestoreTopic => {
                if let Some(saved) = self.topic_save_stack.pop() {
                    self.interpreter.env_mut().insert("_".to_string(), saved);
                }
                *ip += 1;
            }

            // -- Arithmetic --
            OpCode::Add => {
                self.exec_add_op()?;
                *ip += 1;
            }
            OpCode::Sub => {
                self.exec_sub_op()?;
                *ip += 1;
            }
            OpCode::Mul => {
                self.exec_mul_op()?;
                *ip += 1;
            }
            OpCode::Div => {
                self.exec_div_op()?;
                *ip += 1;
            }
            OpCode::Mod => {
                self.exec_mod_op()?;
                *ip += 1;
            }
            OpCode::Pow => {
                self.exec_pow_op()?;
                *ip += 1;
            }
            OpCode::Negate => {
                self.exec_negate_op()?;
                *ip += 1;
            }
            OpCode::IntBitNeg => {
                self.exec_int_bit_neg_op();
                *ip += 1;
            }
            OpCode::BoolBitNeg => {
                self.exec_bool_bit_neg_op();
                *ip += 1;
            }
            OpCode::StrBitNeg => {
                self.exec_str_bit_neg_op();
                *ip += 1;
            }
            OpCode::MakeSlip => {
                self.exec_make_slip_op();
                *ip += 1;
            }
            OpCode::Decont => {
                self.exec_decont_op();
                *ip += 1;
            }
            OpCode::Itemize => {
                // Wrap Array/List values in their itemized (Scalar-container)
                // variant so they are treated as single items in list context.
                // Hash is already an item (not flattened in list context), so
                // it is left unchanged.
                let val = self.stack.pop().unwrap_or(Value::Nil);
                let itemized = match val {
                    Value::Array(items, kind) if !kind.is_itemized() => {
                        Value::Array(items, kind.itemize())
                    }
                    // Itemize a Seq by wrapping it in a scalar container
                    Value::Seq(items) => Value::Scalar(Box::new(Value::Seq(items))),
                    other => other,
                };
                self.stack.push(itemized);
                *ip += 1;
            }
            OpCode::ItemizeVar(name_idx) => {
                // Itemize a scalar variable's value for `@a = $var`, UNLESS the
                // scalar was bound (`:=`) to a Positional. A bound scalar is not
                // a Scalar container, so its value must flatten on `@`-assignment.
                let val = self.stack.pop().unwrap_or(Value::Nil);
                let is_bound_decont = if self.bound_decont_active {
                    let var_name = match &code.constants[*name_idx as usize] {
                        Value::Str(s) => s.as_str(),
                        _ => "",
                    };
                    let key = format!("__mutsu_bound_decont::{}", var_name);
                    matches!(self.interpreter.env().get(&key), Some(Value::Bool(true)))
                } else {
                    false
                };
                let result = if is_bound_decont {
                    val
                } else {
                    match val {
                        Value::Array(items, kind) if !kind.is_itemized() => {
                            Value::Array(items, kind.itemize())
                        }
                        Value::Seq(items) => Value::Scalar(Box::new(Value::Seq(items))),
                        other => other,
                    }
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::WrapScalar => {
                // Wrap the top-of-stack value in a Value::Scalar container.
                // Used for `my $ = expr` (anonymous scalar) in argument position
                // so the container is preserved when stored in an immutable List.
                let val = self.stack.pop().unwrap_or(Value::Nil);
                self.stack.push(Value::Scalar(Box::new(val)));
                *ip += 1;
            }
            OpCode::WrapTypedContainer(type_idx) => {
                // Wrap a typed anonymous scalar (`my T $`) in a ContainerRef cell
                // and record its `of`-type, so the constraint travels with the
                // value (e.g. into a Pair value) and is enforced on assignment.
                let type_name = Self::const_str(code, *type_idx).to_string();
                let val = self.stack.pop().unwrap_or(Value::Nil);
                let cell = std::sync::Arc::new(std::sync::Mutex::new(val));
                crate::value::register_container_constraint(&cell, &type_name);
                self.stack.push(Value::ContainerRef(cell));
                *ip += 1;
            }
            OpCode::FlattenSlurpy => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                let mut items = Vec::new();
                Self::flatten_value_for_slurpy(&val, &mut items);
                self.stack.push(Value::real_array(items));
                *ip += 1;
            }

            // -- Logic / coercion --
            OpCode::Not => {
                self.exec_not_op();
                *ip += 1;
            }
            OpCode::BoolCoerce => {
                self.exec_bool_coerce_op();
                *ip += 1;
            }
            OpCode::WrapVarRef(name_idx) => {
                self.exec_wrap_var_ref_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::MarkBindContext => {
                self.bind_context = true;
                *ip += 1;
            }
            OpCode::MarkScalarBindContext => {
                self.scalar_bind_context = true;
                *ip += 1;
            }
            OpCode::MarkRebindContext => {
                self.rebind_context = true;
                *ip += 1;
            }
            OpCode::MarkConstantContext => {
                self.constant_context = true;
                *ip += 1;
            }
            OpCode::MarkExplicitInitializerContext => {
                self.explicit_initializer_context = true;
                *ip += 1;
            }
            OpCode::MarkVarDeclContext => {
                self.vardecl_context = true;
                *ip += 1;
            }

            // -- String --
            OpCode::Concat => {
                self.exec_concat_op();
                *ip += 1;
            }

            // -- Numeric comparison --
            OpCode::NumEq => {
                self.exec_num_eq_op()?;
                *ip += 1;
            }
            OpCode::NumNe => {
                self.exec_num_ne_op()?;
                *ip += 1;
            }
            OpCode::NumNeNative(flags) => {
                let flags = *flags;
                self.exec_num_ne_native_op(flags)?;
                *ip += 1;
            }
            OpCode::NumLt => {
                self.exec_num_lt_op()?;
                *ip += 1;
            }
            OpCode::NumLe => {
                self.exec_num_le_op()?;
                *ip += 1;
            }
            OpCode::NumGt => {
                self.exec_num_gt_op()?;
                *ip += 1;
            }
            OpCode::NumGe => {
                self.exec_num_ge_op()?;
                *ip += 1;
            }
            OpCode::ApproxEq => {
                self.exec_approx_eq_op()?;
                *ip += 1;
            }
            OpCode::ContainerEq(flags) => {
                let flags = *flags;
                self.exec_container_eq_op(flags);
                *ip += 1;
            }
            OpCode::ContainerEqNamed {
                left_name_idx,
                right_name_idx,
            } => {
                self.exec_container_eq_named_op(code, *left_name_idx, *right_name_idx);
                *ip += 1;
            }
            OpCode::ContainerEqIndexed {
                left_name_idx,
                right_name_idx,
            } => {
                self.exec_container_eq_indexed_op(code, *left_name_idx, *right_name_idx);
                *ip += 1;
            }
            OpCode::ContainerEqRaw => {
                self.exec_container_eq_raw_op();
                *ip += 1;
            }

            // -- String comparison --
            OpCode::StrEq => {
                self.exec_str_eq_op()?;
                *ip += 1;
            }
            OpCode::StrNe => {
                self.exec_str_ne_op()?;
                *ip += 1;
            }
            OpCode::StrLt => {
                self.exec_str_lt_op()?;
                *ip += 1;
            }
            OpCode::StrGt => {
                self.exec_str_gt_op()?;
                *ip += 1;
            }
            OpCode::StrLe => {
                self.exec_str_le_op()?;
                *ip += 1;
            }
            OpCode::StrGe => {
                self.exec_str_ge_op()?;
                *ip += 1;
            }

            // -- Three-way comparison --
            OpCode::Spaceship => {
                self.exec_spaceship_op()?;
                *ip += 1;
            }
            OpCode::Before | OpCode::After => {
                let is_before = matches!(code.ops[*ip], OpCode::Before);
                self.exec_before_after_op(is_before);
                *ip += 1;
            }
            OpCode::Cmp => {
                self.exec_cmp_op();
                *ip += 1;
            }
            OpCode::Coll => {
                self.exec_coll_op();
                *ip += 1;
            }
            OpCode::Unicmp => {
                self.exec_unicmp_op();
                *ip += 1;
            }
            OpCode::Leg => {
                self.exec_leg_op();
                *ip += 1;
            }

            // -- Identity/value equality --
            OpCode::StrictEq => {
                self.exec_strict_eq_op()?;
                *ip += 1;
            }
            OpCode::StrictNe => {
                self.exec_strict_ne_op()?;
                *ip += 1;
            }
            OpCode::Eqv => {
                self.exec_eqv_op()?;
                *ip += 1;
            }
            OpCode::SmartMatchExpr {
                rhs_end,
                negate,
                lhs_var,
                rhs_is_match_regex,
                lhs_is_literal,
                rhs_pure_regex,
            } => {
                self.exec_smart_match_expr_op(
                    code,
                    ip,
                    *rhs_end,
                    *negate,
                    lhs_var,
                    *rhs_is_match_regex,
                    *lhs_is_literal,
                    *rhs_pure_regex,
                    compiled_fns,
                )?;
            }
            OpCode::ScalarizeRegexMatchResult => {
                self.exec_scalarize_regex_match_result_op()?;
                *ip += 1;
            }

            // -- Divisibility --
            OpCode::DivisibleBy => {
                self.exec_divisible_by_op()?;
                *ip += 1;
            }
            OpCode::NotDivisibleBy => {
                self.exec_not_divisible_by_op()?;
                *ip += 1;
            }

            // -- Keyword math --
            OpCode::IntDiv => {
                self.exec_int_div_op()?;
                *ip += 1;
            }
            OpCode::IntMod => {
                self.exec_int_mod_op()?;
                *ip += 1;
            }
            OpCode::Gcd => {
                self.exec_gcd_op();
                *ip += 1;
            }
            OpCode::Lcm => {
                self.exec_lcm_op();
                *ip += 1;
            }
            OpCode::InfixMin => {
                self.exec_infix_min_op();
                *ip += 1;
            }
            OpCode::InfixMax => {
                self.exec_infix_max_op();
                *ip += 1;
            }

            // -- Repetition --
            OpCode::StringRepeat => {
                self.exec_string_repeat_op()?;
                *ip += 1;
            }
            OpCode::ListRepeat => {
                self.exec_list_repeat_op()?;
                *ip += 1;
            }
            OpCode::FunctionCompose => {
                self.exec_function_compose_op();
                *ip += 1;
            }

            // -- Mixin / Type check --
            OpCode::ButMixin => {
                self.exec_but_mixin_op()?;
                *ip += 1;
            }
            OpCode::ButMixinTupleElem => {
                self.exec_but_mixin_tuple_elem_op()?;
                *ip += 1;
            }
            OpCode::Isa => {
                self.exec_isa_op();
                *ip += 1;
            }
            OpCode::Does => {
                self.exec_does_op(code)?;
                *ip += 1;
            }
            OpCode::DoesVar(name_idx) => {
                self.exec_does_var_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::SetDoesContext(flag) => {
                self.interpreter.in_does_rhs = *flag;
                *ip += 1;
            }

            // -- Pair --
            OpCode::MakePair => {
                self.exec_make_pair_op(code);
                *ip += 1;
            }
            OpCode::ContainerizePair => {
                let val = self.stack.pop().unwrap();
                let containerized = match val {
                    Value::Pair(k, v) => Value::ValuePair(Box::new(Value::str(k)), v),
                    other => other,
                };
                self.stack.push(containerized);
                *ip += 1;
            }

            // -- Bitwise --
            OpCode::BitAnd => {
                self.exec_bit_and_op();
                *ip += 1;
            }
            OpCode::BitOr => {
                self.exec_bit_or_op();
                *ip += 1;
            }
            OpCode::BitXor => {
                self.exec_bit_xor_op();
                *ip += 1;
            }
            OpCode::BitShiftLeft => {
                self.exec_bit_shift_left_op();
                *ip += 1;
            }
            OpCode::BitShiftRight => {
                self.exec_bit_shift_right_op();
                *ip += 1;
            }
            OpCode::BoolBitOr => {
                self.exec_bool_bit_or_op();
                *ip += 1;
            }
            OpCode::BoolBitAnd => {
                self.exec_bool_bit_and_op();
                *ip += 1;
            }
            OpCode::BoolBitXor => {
                self.exec_bool_bit_xor_op();
                *ip += 1;
            }
            OpCode::StrBitAnd => {
                self.exec_str_bit_and_op();
                *ip += 1;
            }
            OpCode::StrBitOr => {
                self.exec_str_bit_or_op();
                *ip += 1;
            }
            OpCode::StrBitXor => {
                self.exec_str_bit_xor_op();
                *ip += 1;
            }
            OpCode::StrShiftLeft => {
                self.exec_str_shift_left_op();
                *ip += 1;
            }
            OpCode::StrShiftRight => {
                self.exec_str_shift_right_op();
                *ip += 1;
            }

            // -- Set operations --
            OpCode::SetElem => {
                self.exec_set_elem_op()?;
                *ip += 1;
            }
            OpCode::SetCont => {
                self.exec_set_cont_op()?;
                *ip += 1;
            }
            OpCode::SetUnion => {
                self.exec_set_union_op()?;
                *ip += 1;
            }
            OpCode::SetAddition => {
                self.exec_set_addition_op()?;
                *ip += 1;
            }
            OpCode::SetIntersect => {
                self.exec_set_intersect_op()?;
                *ip += 1;
            }
            OpCode::SetMultiply => {
                self.exec_set_multiply_op()?;
                *ip += 1;
            }
            OpCode::SetDiff => {
                self.exec_set_diff_op();
                *ip += 1;
            }
            OpCode::SetSymDiff => {
                self.exec_set_sym_diff_op();
                *ip += 1;
            }
            OpCode::SetSubset => {
                self.exec_set_subset_op();
                *ip += 1;
            }
            OpCode::SetSuperset => {
                self.exec_set_superset_op();
                *ip += 1;
            }
            OpCode::SetStrictSubset => {
                self.exec_set_strict_subset_op();
                *ip += 1;
            }
            OpCode::SetStrictSuperset => {
                self.exec_set_strict_superset_op();
                *ip += 1;
            }
            OpCode::JunctionAny => {
                self.exec_junction_any_op();
                *ip += 1;
            }
            OpCode::JunctionAll => {
                self.exec_junction_all_op();
                *ip += 1;
            }
            OpCode::JunctionOne => {
                self.exec_junction_one_op();
                *ip += 1;
            }
            OpCode::JunctionAnyN(count) => {
                self.exec_junction_n_op(*count, JunctionKind::Any, "infix:<|>")?;
                *ip += 1;
            }
            OpCode::JunctionAllN(count) => {
                self.exec_junction_n_op(*count, JunctionKind::All, "infix:<&>")?;
                *ip += 1;
            }
            OpCode::JunctionOneN(count) => {
                self.exec_junction_n_op(*count, JunctionKind::One, "infix:<^>")?;
                *ip += 1;
            }

            // -- Sequence --
            OpCode::Sequence { exclude_end } => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let out = self
                    .interpreter
                    .eval_sequence_values(left, right, *exclude_end)?;
                self.stack.push(out);
                self.env_dirty = true;
                *ip += 1;
            }

            // -- Nil check --
            OpCode::IsNil => {
                let val = self.stack.pop().unwrap();
                self.stack.push(Value::Bool(matches!(val, Value::Nil)));
                *ip += 1;
            }

            // -- Control flow --
            OpCode::Label(_) => {
                *ip += 1;
            }
            OpCode::Goto => {
                let target = self.stack.pop().unwrap_or(Value::Nil).to_string_value();
                if let Some(target_ip) = self.find_label_target(code, &target) {
                    *ip = target_ip;
                } else {
                    return Err(RuntimeError::goto_signal(target));
                }
            }
            OpCode::Jump(target) => {
                *ip = *target as usize;
            }
            OpCode::JumpIfFalse(target) => {
                // Mark Failures as handled when tested for truthiness (e.g. && operator)
                Self::mark_failure_handled_on_stack(&mut self.stack);
                let val = self.stack.pop().unwrap();
                if !self.eval_truthy(&val) {
                    // Also mark the original (below dup) as handled
                    Self::mark_failure_handled_on_stack(&mut self.stack);
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }
            OpCode::JumpIfTrue(target) => {
                Self::mark_failure_handled_on_stack(&mut self.stack);
                let val = self.stack.last().unwrap().clone();
                if self.eval_truthy(&val) {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }
            OpCode::JumpIfNil(target) => {
                Self::mark_failure_handled_on_stack(&mut self.stack);
                let val = self.stack.last().unwrap();
                if !runtime::types::value_is_defined(val) {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }
            OpCode::JumpIfNotNil(target) => {
                Self::mark_failure_handled_on_stack(&mut self.stack);
                let val = self.stack.last().unwrap();
                if runtime::types::value_is_defined(val) {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }

            OpCode::CallDefined => {
                let val = self.stack.pop().unwrap();
                // Check if the value has a user-defined .defined method
                let class_name = match &val {
                    Value::Package(name) => Some(*name),
                    Value::Instance { class_name, .. } => Some(*class_name),
                    _ => None,
                };
                let has_user_defined = class_name
                    .as_ref()
                    .is_some_and(|cn| self.interpreter.has_user_method(&cn.resolve(), "defined"));
                let defined = if has_user_defined {
                    // Call user method directly, bypassing native method dispatch
                    let cn = class_name.unwrap();
                    let attrs = match &val {
                        Value::Instance { attributes, .. } => attributes.to_map(),
                        _ => std::collections::HashMap::new(),
                    };
                    match self.interpreter.run_instance_method(
                        &cn.resolve(),
                        attrs,
                        "defined",
                        Vec::new(),
                        Some(val.clone()),
                    ) {
                        Ok((result, _)) => result,
                        Err(_) => Value::Bool(runtime::types::value_is_defined(&val)),
                    }
                } else {
                    Value::Bool(runtime::types::value_is_defined(&val))
                };
                self.stack.push(defined);
                *ip += 1;
            }

            // -- Stack manipulation --
            OpCode::XorXor => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                let a_truthy = a.truthy();
                let b_truthy = b.truthy();
                let result = if a_truthy && !b_truthy {
                    a
                } else if !a_truthy && b_truthy {
                    b
                } else if a_truthy && b_truthy {
                    Value::Nil
                } else {
                    // both falsy: return the last falsy value
                    b
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::Dup => {
                let val = self.stack.last().unwrap().clone();
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::CoerceToList => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                let list_val = match val {
                    // Explicit Arrays ([1,2,3]) are preserved as-is.
                    Value::Array(_, kind) if kind.is_real_array() => val,
                    // Comma lists and other non-real arrays become Lists.
                    Value::Array(items, _) => Value::Array(items, crate::value::ArrayKind::List),
                    Value::Seq(items) => Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(items.to_vec())),
                        crate::value::ArrayKind::List,
                    ),
                    // Hash values are flattened to pairs for constant @.
                    Value::Hash(ref map) => {
                        let pairs: Vec<Value> = map
                            .iter()
                            .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                            .collect();
                        Value::Array(
                            std::sync::Arc::new(crate::value::ArrayData::new(pairs)),
                            crate::value::ArrayKind::List,
                        )
                    }
                    // Instance objects: check if Positional; if so keep as-is,
                    // otherwise call .cache for coercion (constant @ semantics).
                    Value::Instance { ref class_name, .. } => {
                        let cn = class_name.resolve();
                        let does_positional = matches!(
                            cn.as_str(),
                            "Array"
                                | "List"
                                | "Slip"
                                | "Seq"
                                | "Range"
                                | "Buf"
                                | "Blob"
                                | "utf8"
                                | "buf8"
                                | "buf16"
                                | "buf32"
                        ) || self
                            .interpreter
                            .class_composed_roles(&cn)
                            .is_some_and(|roles| roles.iter().any(|r| r == "Positional"));
                        if does_positional {
                            val
                        } else {
                            // Call .cache on non-Positional to coerce.
                            // Skip native methods so user-defined .cache is called.
                            let cached =
                                self.call_method_all_with_fallback(&val, "cache", &[], true)?;
                            let cached_val = cached.into_iter().next().unwrap_or(Value::Nil);
                            // Check that .cache returned a Positional
                            let is_pos = matches!(
                                &cached_val,
                                Value::Array(..)
                                    | Value::Seq(_)
                                    | Value::Slip(_)
                                    | Value::LazyList(_)
                                    | Value::LazyIoLines { .. }
                            );
                            if !is_pos {
                                let got_type = crate::runtime::utils::value_type_name(&cached_val);
                                let mut attrs = std::collections::HashMap::new();
                                attrs.insert("got".to_string(), cached_val);
                                attrs.insert(
                                    "expected".to_string(),
                                    Value::Package(crate::symbol::Symbol::intern("Positional")),
                                );
                                attrs.insert(
                                    "message".to_string(),
                                    Value::str(format!(
                                        "Type check failed in assignment; expected Positional but got {}",
                                        got_type
                                    )),
                                );
                                let ex = Value::make_instance(
                                    crate::symbol::Symbol::intern("X::TypeCheck"),
                                    attrs,
                                );
                                let mut err = RuntimeError::new(format!(
                                    "Type check failed in assignment; expected Positional but got {}",
                                    got_type
                                ));
                                err.exception = Some(Box::new(ex));
                                return Err(err);
                            }
                            // Coerce cached result to List
                            match cached_val {
                                Value::Array(items, _) => {
                                    Value::Array(items, crate::value::ArrayKind::List)
                                }
                                Value::Seq(items) => Value::Array(
                                    std::sync::Arc::new(crate::value::ArrayData::new(
                                        items.to_vec(),
                                    )),
                                    crate::value::ArrayKind::List,
                                ),
                                other => Value::Array(
                                    std::sync::Arc::new(crate::value::ArrayData::new(vec![other])),
                                    crate::value::ArrayKind::List,
                                ),
                            }
                        }
                    }
                    other => Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(vec![other])),
                        crate::value::ArrayKind::List,
                    ),
                };
                self.stack.push(list_val);
                *ip += 1;
            }
            OpCode::Pop => {
                if let Some(Value::LazyList(list)) = self.stack.pop() {
                    // Sink context must realize lazy gathers for side effects.
                    self.force_lazy_list_vm(&list)?;
                    self.env_dirty = true;
                }
                *ip += 1;
            }
            OpCode::SinkPop => {
                if let Some(val) = self.stack.pop() {
                    match &val {
                        Value::LazyList(list) => {
                            self.force_lazy_list_vm(list)?;
                            self.env_dirty = true;
                        }
                        Value::LazyIoLines { handle, words, .. } => {
                            // Sinking a lazy IO lines iterator must drain the
                            // underlying handle so that side effects (read
                            // position, .eof) are observable.
                            self.interpreter.force_lazy_io_lines(handle, *words)?;
                            self.env_dirty = true;
                        }
                        _ => {
                            // Sinking an unhandled Failure always throws (Raku behavior)
                            if let Some(err) =
                                self.interpreter.failure_to_runtime_error_if_unhandled(&val)
                            {
                                return Err(err);
                            }
                            // Sinking a Proc with non-zero exitcode throws X::Proc::Unsuccessful
                            if let Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } = &val
                                && class_name.resolve() == "Proc"
                            {
                                let exitcode = match attributes.as_map().get("exitcode") {
                                    Some(Value::Int(i)) => *i,
                                    _ => 0,
                                };
                                // A still-"live" Proc (from `run(:in, ...)`)
                                // carries a placeholder exitcode of -1 until it
                                // is finalized; sinking it must not throw.
                                let is_live = matches!(
                                    attributes.as_map().get("live"),
                                    Some(Value::Bool(true))
                                );
                                if exitcode != 0 && !is_live {
                                    let signal = match attributes.as_map().get("signal") {
                                        Some(Value::Int(i)) => *i,
                                        _ => 0,
                                    };
                                    let command = attributes
                                        .as_map()
                                        .get("command")
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    // When the command could not be spawned at all
                                    // (exit code -1), rakudo reports the underlying
                                    // OS error in the message.
                                    let os_error = attributes
                                        .as_map()
                                        .get("os-error")
                                        .map(|v| v.to_string_value())
                                        .filter(|s| !s.is_empty());
                                    let msg = match &os_error {
                                        Some(oe) => format!(
                                            "The spawned command '{}' exited unsuccessfully (exit code: {}, signal: {}, OS error = {})",
                                            command, exitcode, signal, oe
                                        ),
                                        None => format!(
                                            "The spawned command '{}' exited unsuccessfully (exit code: {}, signal: {})",
                                            command, exitcode, signal
                                        ),
                                    };
                                    let mut ex_attrs = std::collections::HashMap::new();
                                    ex_attrs.insert("message".to_string(), Value::str(msg.clone()));
                                    ex_attrs.insert("proc".to_string(), val);
                                    let exception = Value::make_instance(
                                        crate::symbol::Symbol::intern("X::Proc::Unsuccessful"),
                                        ex_attrs,
                                    );
                                    let mut err = RuntimeError::new(msg);
                                    err.exception = Some(Box::new(exception));
                                    return Err(err);
                                }
                            }
                        }
                    }
                }
                *ip += 1;
            }

            // -- Range creation --
            OpCode::MakeRange => {
                self.exec_make_range_op()?;
                *ip += 1;
            }
            OpCode::MakeRangeExcl => {
                self.exec_make_range_excl_op()?;
                *ip += 1;
            }
            OpCode::MakeRangeExclStart => {
                self.exec_make_range_excl_start_op()?;
                *ip += 1;
            }
            OpCode::MakeRangeExclBoth => {
                self.exec_make_range_excl_both_op()?;
                *ip += 1;
            }

            // -- Composite --
            OpCode::MakeArray(n) => {
                self.exec_make_array_op(*n, false);
                *ip += 1;
            }
            OpCode::MakeRealArray(n) => {
                self.exec_make_array_op(*n, true);
                *ip += 1;
            }
            OpCode::MakeRealArrayNoFlatten(n) => {
                self.exec_make_array_no_flatten_op(*n);
                *ip += 1;
            }
            OpCode::MakeHash(n) => {
                self.exec_make_hash_op(*n);
                *ip += 1;
            }
            OpCode::MakeHashFromPairs(n) => {
                self.exec_make_hash_from_pairs_op(*n);
                *ip += 1;
            }
            OpCode::MakeCapture(n) => {
                self.exec_make_capture_op(code, *n);
                *ip += 1;
            }

            // -- I/O --
            OpCode::Say(n) => {
                self.ensure_locals_synced(code);
                self.sync_env_from_locals(code);
                self.exec_say_op(*n)?;
                self.env_dirty = true;
                *ip += 1;
            }
            OpCode::Put(n) => {
                self.ensure_locals_synced(code);
                self.sync_env_from_locals(code);
                self.exec_put_op(*n)?;
                self.env_dirty = true;
                *ip += 1;
            }
            OpCode::Print(n) => {
                self.ensure_locals_synced(code);
                self.sync_env_from_locals(code);
                self.exec_print_op(*n)?;
                self.env_dirty = true;
                *ip += 1;
            }
            OpCode::Note(n) => {
                self.ensure_locals_synced(code);
                self.sync_env_from_locals(code);
                self.exec_note_op(*n)?;
                self.env_dirty = true;
                *ip += 1;
            }

            // -- Calls --
            OpCode::CallFunc {
                name_idx,
                arity,
                arg_sources_idx,
            } => {
                match self.exec_call_func_op(
                    code,
                    *name_idx,
                    *arity,
                    *arg_sources_idx,
                    compiled_fns,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        // Record a resume point so a call that raises a
                        // control signal (e.g. `warn`) can be resumed after
                        // the call site by `.resume` in a CONTROL block.
                        if !e.is_resume && self.resume_ip.is_none() {
                            self.resume_ip = Some(*ip + 1);
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::CallFuncSlip {
                name_idx,
                regular_arity,
                arg_sources_idx,
                slip_pos,
            } => {
                self.exec_call_func_slip_op(
                    code,
                    *name_idx,
                    *regular_arity,
                    *arg_sources_idx,
                    *slip_pos,
                    compiled_fns,
                )?;
                *ip += 1;
            }
            OpCode::CallMethod {
                name_idx,
                arity,
                modifier_idx,
                quoted,
                arg_sources_idx,
            } => {
                match self.exec_call_method_op(
                    code,
                    *name_idx,
                    *arity,
                    *modifier_idx,
                    *quoted,
                    *arg_sources_idx,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        // Record a resume point so a method that throws can
                        // be resumed after the call site by .resume in CATCH.
                        // Don't overwrite an existing resume_ip: when the
                        // method call is itself a `.resume`/`.rethrow` that
                        // re-raises a control signal, the original resume
                        // point (e.g. after `warn`) must be preserved.
                        if !e.is_resume && self.resume_ip.is_none() {
                            self.resume_ip = Some(*ip + 1);
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::CallMethodDynamic {
                arity,
                modifier_idx,
            } => {
                match self.exec_call_method_dynamic_op(code, *arity, *modifier_idx) {
                    Ok(()) => {}
                    Err(e) => {
                        // Record a resume point so a method that raises a
                        // control signal (e.g. a resumable `warn`) can be
                        // resumed after the call site by `.resume`.
                        if !e.is_resume && self.resume_ip.is_none() {
                            self.resume_ip = Some(*ip + 1);
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::CallMethodDynamicMut {
                arity,
                target_name_idx,
                modifier_idx,
            } => {
                let pre = self.array_hash_attr_env_snapshot(code, *target_name_idx);
                match self.exec_call_method_dynamic_mut_op(
                    code,
                    *arity,
                    *target_name_idx,
                    *modifier_idx,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        if !e.is_resume && self.resume_ip.is_none() {
                            self.resume_ip = Some(*ip + 1);
                        }
                        return Err(e);
                    }
                }
                self.mirror_array_hash_attr_to_cell(code, *target_name_idx, pre);
                *ip += 1;
            }
            OpCode::ArrayPush { target_name_idx } => {
                let pre = self.array_hash_attr_env_snapshot(code, *target_name_idx);
                self.exec_array_push_op(code, *target_name_idx)?;
                self.mirror_array_hash_attr_to_cell(code, *target_name_idx, pre);
                *ip += 1;
            }
            OpCode::CallMethodMut {
                name_idx,
                arity,
                target_name_idx,
                modifier_idx,
                quoted,
                arg_sources_idx,
            } => {
                let pre = self.array_hash_attr_env_snapshot(code, *target_name_idx);
                match self.exec_call_method_mut_op(
                    code,
                    *name_idx,
                    *arity,
                    *target_name_idx,
                    *modifier_idx,
                    *quoted,
                    *arg_sources_idx,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        if !e.is_resume && self.resume_ip.is_none() {
                            self.resume_ip = Some(*ip + 1);
                        }
                        return Err(e);
                    }
                }
                self.mirror_array_hash_attr_to_cell(code, *target_name_idx, pre);
                *ip += 1;
            }
            OpCode::CallOnValue {
                arity,
                arg_sources_idx,
            } => {
                self.exec_call_on_value_op(code, *arity, *arg_sources_idx, compiled_fns)?;
                *ip += 1;
            }
            OpCode::CallOnCodeVar {
                name_idx,
                arity,
                arg_sources_idx,
            } => {
                self.exec_call_on_code_var_op(
                    code,
                    *name_idx,
                    *arity,
                    *arg_sources_idx,
                    compiled_fns,
                )?;
                *ip += 1;
            }
            OpCode::ExecCall {
                name_idx,
                arity,
                arg_sources_idx,
            } => {
                match self.exec_exec_call_op(
                    code,
                    *name_idx,
                    *arity,
                    *arg_sources_idx,
                    compiled_fns,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        if !e.is_resume && self.resume_ip.is_none() {
                            self.resume_ip = Some(*ip + 1);
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::ExecCallPairs { name_idx, arity } => {
                self.exec_exec_call_pairs_op(code, compiled_fns, *name_idx, *arity)?;
                *ip += 1;
            }
            OpCode::ExecCallSlip {
                name_idx,
                regular_arity,
                arg_sources_idx,
                slip_pos,
            } => {
                self.exec_exec_call_slip_op(
                    code,
                    compiled_fns,
                    *name_idx,
                    *regular_arity,
                    *arg_sources_idx,
                    *slip_pos,
                )?;
                *ip += 1;
            }

            // -- Indexing --
            OpCode::Index { is_positional } => {
                self.exec_index_op_with_positional(*is_positional)?;
                *ip += 1;
            }
            OpCode::IndexAutovivify => {
                self.exec_index_autovivify_op()?;
                *ip += 1;
            }
            OpCode::IndexAutovivifyLazy => {
                self.exec_index_autovivify_lazy_op(false)?;
                *ip += 1;
            }
            OpCode::IndexAutovivifyLazyTerminal => {
                self.exec_index_autovivify_lazy_op(true)?;
                *ip += 1;
            }
            OpCode::DeleteIndexNamed(name_idx) => {
                let pre = self.array_hash_attr_env_snapshot(code, *name_idx);
                self.exec_delete_index_named_op(code, *name_idx)?;
                self.mirror_array_hash_attr_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::DeleteIndexExpr => {
                self.exec_delete_index_expr_op()?;
                *ip += 1;
            }
            OpCode::MultiDimIndex(ndims) => {
                self.exec_multi_dim_index_op(*ndims)?;
                *ip += 1;
            }
            OpCode::MultiDimIndexAssign { name_idx, ndims } => {
                let pre = self.array_hash_attr_env_snapshot(code, *name_idx);
                self.exec_multi_dim_index_assign_op(code, *name_idx, *ndims)?;
                self.mirror_array_hash_attr_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::MultiDimIndexAssignGeneric(ndims) => {
                self.exec_multi_dim_index_assign_generic_op(*ndims)?;
                *ip += 1;
            }
            OpCode::HyperSlice(adverb) => {
                self.exec_hyper_slice_op(*adverb)?;
                *ip += 1;
            }
            OpCode::HyperIndex => {
                self.exec_hyper_index_op()?;
                *ip += 1;
            }

            // -- String interpolation --
            OpCode::StringConcat(n) => {
                self.exec_string_concat_op(*n)?;
                *ip += 1;
            }

            // -- Loop control --
            OpCode::Last(label) => {
                let mut sig = RuntimeError::last_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            OpCode::Next(label) => {
                let mut sig = RuntimeError::next_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            OpCode::Redo(label) => {
                let mut sig = RuntimeError::redo_signal();
                sig.label = label.clone();
                return Err(sig);
            }

            // -- Given/When control --
            OpCode::Proceed => {
                return Err(RuntimeError::proceed_signal());
            }
            OpCode::Succeed => {
                return Err(RuntimeError::succeed_signal());
            }
            OpCode::ReactDone => {
                return Err(RuntimeError::react_done_signal());
            }
            OpCode::TagContainerRef(name_idx) => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.container_ref_var = Some(name);
                self.container_ref_reversed = false;
                *ip += 1;
            }
            OpCode::TagContainerRefReversed(name_idx) => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.container_ref_var = Some(name);
                self.container_ref_reversed = true;
                *ip += 1;
            }

            OpCode::UndefineAggregate(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                if let Some(val) = self.get_env_with_main_alias(name) {
                    match &val {
                        Value::Array(arc, _) => {
                            let ptr = Arc::as_ptr(arc) as *mut crate::value::ArrayData;
                            unsafe { (&mut *ptr).items.clear() };
                        }
                        Value::Hash(arc) => {
                            let ptr = Arc::as_ptr(arc) as *mut crate::value::HashData;
                            unsafe { (*ptr).map.clear() };
                        }
                        _ => {}
                    }
                }
                // Also update locals if present
                if let Some(slot) = self.find_local_slot(code, name) {
                    match &self.locals[slot] {
                        Value::Array(arc, _) => {
                            let ptr = Arc::as_ptr(arc) as *mut crate::value::ArrayData;
                            unsafe { (&mut *ptr).items.clear() };
                        }
                        Value::Hash(arc) => {
                            let ptr = Arc::as_ptr(arc) as *mut crate::value::HashData;
                            unsafe { (*ptr).map.clear() };
                        }
                        _ => {
                            self.locals[slot] = Value::Nil;
                            self.flush_local_to_env(code, slot);
                        }
                    }
                }
                self.stack.push(Value::Nil);
                *ip += 1;
            }

            // -- Postfix operators --
            OpCode::PostIncrement(name_idx) => {
                self.exec_post_increment_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::PostDecrement(name_idx) => {
                self.exec_post_decrement_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::PostIncrementIndex(name_idx) => {
                self.exec_post_increment_index_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::PostDecrementIndex(name_idx) => {
                self.exec_post_decrement_index_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::IndexAssignExprNamed {
                name_idx,
                is_positional,
            } => {
                let pre = self.array_hash_attr_env_snapshot(code, *name_idx);
                self.exec_index_assign_expr_named_op(code, *name_idx, *is_positional)?;
                self.mirror_array_hash_attr_to_cell(code, *name_idx, pre);
                *ip += 1;
            }
            OpCode::IndexAssignPseudoStashNamed {
                stash_name_idx,
                key_name_idx,
            } => {
                self.exec_index_assign_pseudo_stash_named_op(code, *stash_name_idx, *key_name_idx)?;
                *ip += 1;
            }
            OpCode::IndexAssignExprNested {
                name_idx,
                outer_positional,
                inner_positional,
            } => {
                self.exec_index_assign_expr_nested_op(
                    code,
                    *name_idx,
                    *outer_positional,
                    *inner_positional,
                )?;
                *ip += 1;
            }
            OpCode::IndexAssignDeepNested {
                name_idx,
                depth,
                positional_flags_idx,
            } => {
                self.exec_index_assign_deep_nested_op(
                    code,
                    *name_idx,
                    *depth,
                    *positional_flags_idx,
                )?;
                *ip += 1;
            }

            // -- Unary coercion --
            OpCode::NumCoerce => {
                self.exec_num_coerce_op()?;
                *ip += 1;
            }
            OpCode::StrCoerce => {
                self.exec_str_coerce_op()?;
                *ip += 1;
            }
            OpCode::UptoRange => {
                self.exec_upto_range_op();
                *ip += 1;
            }

            // -- Prefix increment/decrement --
            OpCode::PreIncrement(name_idx) => {
                self.exec_pre_increment_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::PreDecrement(name_idx) => {
                self.exec_pre_decrement_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::PreIncrementIndex(name_idx) => {
                self.exec_pre_increment_index_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::PreDecrementIndex(name_idx) => {
                self.exec_pre_decrement_index_op(code, *name_idx)?;
                *ip += 1;
            }

            // -- Variable access --
            OpCode::GetCaptureVar(name_idx) => {
                self.exec_get_capture_var_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::GetCodeVar(name_idx) => {
                self.exec_get_code_var_op(code, *name_idx)?;
                *ip += 1;
            }

            // -- Assignment as expression --
            OpCode::AssignExpr(name_idx) => {
                self.exec_assign_expr_op(code, *name_idx)?;
                *ip += 1;
            }

            // -- Loops --
            OpCode::WhileLoop {
                cond_end,
                body_end,
                label,
                collect,
                isolate_topic,
            } => {
                let spec = vm_control_ops::WhileLoopSpec {
                    cond_end: *cond_end,
                    body_end: *body_end,
                    label: label.clone(),
                    collect: *collect,
                    isolate_topic: *isolate_topic,
                };
                self.exec_while_loop_op(code, &spec, ip, compiled_fns)?;
            }
            OpCode::ForLoop {
                param_idx,
                param_local,
                body_end,
                label,
                arity,
                collect,
                restore_topic,
                threaded,
                is_rw,
                do_writeback,
                rw_param_names,
                kv_mode,
                source_var_names,
                autothread_junctions,
                explicit_zero_params,
                multi_param_names,
                loop_var_wraps_element,
            } => {
                let spec = vm_control_ops::ForLoopSpec {
                    param_idx: *param_idx,
                    param_local: *param_local,
                    body_end: *body_end,
                    label: label.clone(),
                    arity: *arity,
                    collect: *collect,
                    restore_topic: *restore_topic,
                    threaded: *threaded,
                    is_rw: *is_rw,
                    do_writeback: *do_writeback,
                    rw_param_names: rw_param_names.clone(),
                    kv_mode: *kv_mode,
                    source_var_names: source_var_names.clone(),
                    autothread_junctions: *autothread_junctions,
                    explicit_zero_params: *explicit_zero_params,
                    multi_param_names: multi_param_names.clone(),
                    loop_var_wraps_element: *loop_var_wraps_element,
                };
                self.exec_for_loop_op(code, &spec, ip, compiled_fns)?;
            }
            OpCode::RestoreForParam => {
                // Restore the single named for-loop param's prior binding now
                // that the loop's LAST/post phasers (which needed the param at
                // its final value) have run. Paired with the push the ForLoop
                // opcode performs on normal completion.
                if let Some((name, saved_val)) = self.for_param_restore_stack.pop() {
                    match saved_val {
                        Some(v) => {
                            self.interpreter.env_mut().insert(name, v);
                        }
                        None => {
                            self.interpreter.env_mut().remove(&name);
                        }
                    }
                }
                *ip += 1;
            }
            OpCode::CStyleLoop {
                cond_end,
                step_start,
                body_end,
                label,
                collect,
            } => {
                let spec = vm_control_ops::CStyleLoopSpec {
                    cond_end: *cond_end,
                    step_start: *step_start,
                    body_end: *body_end,
                    label: label.clone(),
                    collect: *collect,
                };
                self.exec_cstyle_loop_op(code, &spec, ip, compiled_fns)?;
            }

            // -- Given/When/Default --
            OpCode::Given {
                body_end,
                topic_readonly,
            } => {
                self.exec_given_op(code, *body_end, *topic_readonly, ip, compiled_fns)?;
            }
            OpCode::When { body_end } => {
                self.exec_when_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::Default { body_end } => {
                self.exec_default_op(code, *body_end, ip, compiled_fns)?;
            }

            // -- Repeat loop --
            OpCode::RepeatLoop {
                cond_end,
                body_end,
                label,
            } => {
                self.exec_repeat_loop_op(code, *cond_end, *body_end, label, ip, compiled_fns)?;
            }

            // -- Exception handling --
            OpCode::TryCatch {
                catch_start,
                control_start,
                body_end,
                explicit_catch,
            } => {
                self.exec_try_catch_op(
                    code,
                    *catch_start,
                    *control_start,
                    *body_end,
                    *explicit_catch,
                    ip,
                    compiled_fns,
                )?;
            }

            // -- Error handling --
            OpCode::Die => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                // Store the resume point (instruction after Die) for .resume support
                self.resume_ip = Some(*ip + 1);
                // die() with empty array (from parsing die() with parens) should
                // check $! first, falling back to "Died" default
                let val = if matches!(&val, Value::Array(items, _) if items.is_empty()) {
                    let current = self.interpreter.env().get("!").cloned();
                    if let Some(ref c) = current
                        && !matches!(c, Value::Nil)
                    {
                        current.unwrap()
                    } else {
                        Value::Nil
                    }
                } else {
                    val
                };
                let backtrace_str = self.build_backtrace_string();
                let backtrace_val = self.build_backtrace_value();
                let current_line = self.current_source_line();
                let current_file = self.current_source_file();
                let mut err = self.runtime_error_from_exception_value(val, "Died", false);
                if !backtrace_str.is_empty() {
                    err.backtrace = Some(backtrace_str);
                }
                // Attach backtrace, line, and file to the exception value
                if let Some(ref mut exc_box) = err.exception
                    && let Value::Instance { attributes, .. } = exc_box.as_mut()
                {
                    attributes.insert("backtrace".to_string(), backtrace_val);
                    if let Some(line) = current_line {
                        attributes.insert_if_absent("line".to_string(), Value::Int(line as i64));
                    }
                    if let Some(ref file) = current_file {
                        attributes.insert_if_absent("file".to_string(), Value::str_from(file));
                    }
                }
                return Err(err);
            }
            OpCode::Fail => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                // When fail() receives a Failure:D, extract the inner exception
                // and re-arm it (Raku behavior: fail(Failure:D) re-arms)
                let val = if let Value::Instance {
                    class_name,
                    attributes,
                    ..
                } = &val
                    && class_name.resolve() == "Failure"
                {
                    if let Some(exc) = attributes.as_map().get("exception") {
                        exc.clone()
                    } else {
                        val
                    }
                } else {
                    val
                };
                // Build a backtrace from the routine stack so that
                // Exception.gist can show where the fail originated.
                let backtrace_val = self.build_backtrace_value();
                let current_line = self.current_source_line();
                let current_file = self.current_source_file();
                let mut err = self.runtime_error_from_exception_value(val, "Failed", true);
                // Attach backtrace, line, and file to the exception value
                if let Some(ref mut exc_box) = err.exception
                    && let Value::Instance { attributes, .. } = exc_box.as_mut()
                {
                    attributes.insert("backtrace".to_string(), backtrace_val);
                    if let Some(line) = current_line {
                        attributes.insert_if_absent("line".to_string(), Value::Int(line as i64));
                    }
                    if let Some(ref file) = current_file {
                        attributes.insert_if_absent("file".to_string(), Value::str_from(file));
                    }
                }
                return Err(err);
            }
            OpCode::Return => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                // Check if &return has been lexically rebound; if so, call
                // the rebound function instead of performing a built-in return.
                if let Some(rebound) = self.interpreter.env().get("&return").cloned()
                    && matches!(
                        &rebound,
                        Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                    )
                {
                    let result = self.vm_call_on_value(rebound, vec![val], None)?;
                    self.stack.push(result);
                    *ip += 1;
                    return Ok(());
                }
                return Err(RuntimeError::return_signal(val));
            }
            OpCode::ReturnFromNonRoutine(lexically_in_routine) => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                if *lexically_in_routine {
                    // Closure/block lexically inside a routine: propagate a
                    // CX::Return signal up to the enclosing routine boundary.
                    // If the signal escapes all frames up to the VM top-level,
                    // the lexical target routine is no longer on the dynamic
                    // call stack, so it will surface as
                    // `X::ControlFlow::Return` with `out-of-dynamic-scope`.
                    return Err(RuntimeError::return_signal(val));
                }
                // No lexical routine at all (e.g. top-level `return`): throw
                // X::ControlFlow::Return directly.
                let _ = val;
                return Err(RuntimeError::controlflow_return(false));
            }

            // -- Environment variable access --
            OpCode::GetEnvIndex(key_idx) => {
                self.exec_get_env_index_op(code, *key_idx);
                *ip += 1;
            }
            OpCode::ExistsEnvIndex(key_idx) => {
                self.exec_exists_env_index_op(code, *key_idx);
                *ip += 1;
            }
            OpCode::ExistsExpr => {
                self.exec_exists_expr_op();
                *ip += 1;
            }
            OpCode::ExistsIndexAdv(flags) => {
                self.exec_exists_index_adv_op(*flags, None)?;
                *ip += 1;
            }
            OpCode::ExistsIndexNamedAdv { name_idx, flags } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.exec_exists_index_adv_op(*flags, Some(name))?;
                *ip += 1;
            }

            // -- Reduction --
            OpCode::Reduction(op_idx) => {
                self.exec_reduction_op(code, *op_idx)?;
                *ip += 1;
            }

            // -- Magic variables --
            OpCode::RoutineMagic => {
                self.exec_routine_magic_op()?;
                *ip += 1;
            }
            OpCode::BlockMagic => {
                self.exec_block_magic_op()?;
                *ip += 1;
            }

            // -- Substitution --
            OpCode::Subst {
                pattern_idx,
                replacement_idx,
                samecase,
                sigspace,
                samemark,
                samespace,
                global,
                nth_idx,
                x_count,
                perl5,
            } => {
                self.exec_subst_op(
                    code,
                    *pattern_idx,
                    *replacement_idx,
                    *samecase,
                    *sigspace,
                    *samemark,
                    *samespace,
                    *global,
                    *nth_idx,
                    *x_count,
                    *perl5,
                )?;
                *ip += 1;
            }
            OpCode::NonDestructiveSubst {
                pattern_idx,
                replacement_idx,
                samecase,
                sigspace,
                samemark,
                samespace,
                global,
                nth_idx,
                x_count,
                perl5,
            } => {
                self.exec_non_destructive_subst_op(
                    code,
                    *pattern_idx,
                    *replacement_idx,
                    *samecase,
                    *sigspace,
                    *samemark,
                    *samespace,
                    *global,
                    *nth_idx,
                    *x_count,
                    *perl5,
                )?;
                *ip += 1;
            }
            OpCode::Transliterate {
                from_idx,
                to_idx,
                delete,
                complement,
                squash,
                non_destructive,
            } => {
                self.exec_transliterate_op(
                    code,
                    *from_idx,
                    *to_idx,
                    *delete,
                    *complement,
                    *squash,
                    *non_destructive,
                )?;
                *ip += 1;
            }

            // -- Take --
            OpCode::Take => {
                self.exec_take_op()?;
                *ip += 1;
            }

            // -- Package scope --
            OpCode::PackageScope { name_idx, body_end } => {
                self.exec_package_scope_op(code, *name_idx, *body_end, ip, compiled_fns)?;
            }
            OpCode::RegisterPackage { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                let pkg_val = Value::Package(Symbol::intern(&name));
                self.interpreter
                    .env_mut()
                    .insert(name.clone(), pkg_val.clone());
                self.interpreter
                    .chain_declared_packages
                    .insert(name.clone());
                self.update_local_if_exists(code, &name, &pkg_val);
                self.env_dirty = true;
                *ip += 1;
            }
            OpCode::RegisterPackageMy { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                let pkg_val = Value::Package(Symbol::intern(&name));
                self.interpreter
                    .env_mut()
                    .insert(name.clone(), pkg_val.clone());
                self.interpreter
                    .chain_declared_packages
                    .insert(name.clone());
                self.update_local_if_exists(code, &name, &pkg_val);
                // Mark as my-scoped so the package is hidden from global
                // lookups and package stash resolution outside its scope.
                self.interpreter.mark_my_scoped_package_item(name.clone());
                // Mark as block-declared so the name is cleaned up
                // when the enclosing block scope exits.
                if let Some(set) = self.block_declared_vars.last_mut() {
                    set.insert(name);
                }
                self.env_dirty = true;
                *ip += 1;
            }
            OpCode::RegisterPackageStub { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.registry_mut().package_stubs.insert(name);
                *ip += 1;
            }
            OpCode::ClearPackageStub { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.registry_mut().package_stubs.remove(&name);
                *ip += 1;
            }

            // -- Phaser END --
            OpCode::PhaserEnd { idx, site_id } => {
                self.exec_phaser_end_op(code, *idx, *site_id);
                *ip += 1;
            }

            // -- CHECK Phaser scope --
            OpCode::CheckPhaserStart { .. } => {
                self.check_phaser_depth += 1;
                *ip += 1;
            }
            OpCode::CheckPhaserEnd => {
                self.check_phaser_depth = self.check_phaser_depth.saturating_sub(1);
                *ip += 1;
            }

            // -- HyperMethodCall --
            OpCode::HyperMethodCall {
                name_idx,
                arity,
                modifier_idx,
                quoted,
                target_name_idx,
            } => {
                match self.exec_hyper_method_call_op(
                    code,
                    *name_idx,
                    *arity,
                    *modifier_idx,
                    *quoted,
                    *target_name_idx,
                ) {
                    Ok(()) => {}
                    Err(e) => {
                        // A per-element method may raise a resumable warn (the
                        // hyper op re-raises it carrying the full result); record
                        // the resume point so `.resume` continues after the call.
                        if !e.is_resume && self.resume_ip.is_none() {
                            self.resume_ip = Some(*ip + 1);
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }
            OpCode::HyperMethodCallDynamic {
                arity,
                modifier_idx,
            } => {
                match self.exec_hyper_method_call_dynamic_op(code, *arity, *modifier_idx) {
                    Ok(()) => {}
                    Err(e) => {
                        if !e.is_resume && self.resume_ip.is_none() {
                            self.resume_ip = Some(*ip + 1);
                        }
                        return Err(e);
                    }
                }
                *ip += 1;
            }

            // -- HyperOp --
            OpCode::HyperOp {
                op_idx,
                dwim_left,
                dwim_right,
            } => {
                self.exec_hyper_op(code, *op_idx, *dwim_left, *dwim_right)?;
                *ip += 1;
            }

            // -- HyperFuncOp --
            OpCode::HyperFuncOp {
                name_idx,
                dwim_left,
                dwim_right,
                writeback,
            } => {
                self.exec_hyper_func_op(
                    code,
                    *name_idx,
                    *dwim_left,
                    *dwim_right,
                    *writeback,
                    compiled_fns,
                )?;
                *ip += 1;
            }

            // -- MetaOp --
            OpCode::MetaOp { meta_idx, op_idx } => {
                self.exec_meta_op(code, *meta_idx, *op_idx)?;
                *ip += 1;
            }

            // -- InfixFunc --
            OpCode::InfixFunc {
                name_idx,
                right_arity,
                modifier_idx,
            } => {
                self.exec_infix_func_op(code, *name_idx, *right_arity, modifier_idx, compiled_fns)?;
                *ip += 1;
            }
            OpCode::FlipFlopExpr {
                lhs_end,
                rhs_end,
                site_id,
                exclude_start,
                exclude_end,
                is_fff,
            } => {
                self.exec_flip_flop_expr_op(
                    code,
                    ip,
                    *lhs_end,
                    *rhs_end,
                    *site_id,
                    *exclude_start,
                    *exclude_end,
                    *is_fff,
                    compiled_fns,
                )?;
            }

            // -- Type checking --
            OpCode::TypeCheck(tc_idx, var_name_idx) => {
                self.exec_type_check_op(code, *tc_idx, *var_name_idx)?;
                *ip += 1;
            }
            OpCode::TypeCheckBind(tc_idx, var_name_idx) => {
                self.exec_type_check_bind_op(code, *tc_idx, *var_name_idx)?;
                *ip += 1;
            }
            OpCode::SetPragma(name_idx) => {
                let value = self.stack.pop().unwrap_or(Value::Nil);
                let name = Self::const_str(code, *name_idx);
                if let Value::Str(ref s) = value {
                    if name == "variables" {
                        self.interpreter.set_variables_pragma(s);
                    } else if name == "attributes" {
                        self.interpreter.set_attributes_pragma(s);
                    }
                }
                *ip += 1;
            }
            OpCode::IndirectTypeLookup => {
                self.exec_indirect_type_lookup_op();
                *ip += 1;
            }
            OpCode::IndirectCodeLookup(name_idx) => {
                self.exec_indirect_code_lookup_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::SymbolicDeref(sigil_idx) => {
                self.exec_symbolic_deref_op(code, *sigil_idx);
                *ip += 1;
            }
            OpCode::SymbolicDerefStore(sigil_idx) => {
                self.exec_symbolic_deref_store_op(code, *sigil_idx);
                *ip += 1;
            }
            OpCode::IndirectTypeLookupStore => {
                self.exec_indirect_type_lookup_store_op(code);
                *ip += 1;
            }
            OpCode::StateVarInit(slot, key_idx) => {
                self.exec_state_var_init_op(code, *slot, *key_idx);
                *ip += 1;
            }
            OpCode::StateVarInitGuard(key_idx, jump_to) => {
                let base_key = Self::const_str(code, *key_idx);
                let scoped_key = self.scoped_state_key(base_key);
                if self.interpreter.get_state_var(&scoped_key).is_some() {
                    // State already initialized: push a placeholder value on the
                    // stack (StateVarInit will discard it and use the stored value)
                    // and skip the RHS initializer.
                    self.stack.push(Value::Nil);
                    *ip = *jump_to as usize;
                } else {
                    // State not yet initialized: fall through to compile RHS
                    *ip += 1;
                }
            }

            // -- Block scope --
            OpCode::BlockScope {
                pre_end,
                enter_end,
                body_end,
                keep_start,
                undo_start,
                post_start,
                end,
            } => {
                self.exec_block_scope_op(
                    code,
                    [
                        *pre_end,
                        *enter_end,
                        *body_end,
                        *keep_start,
                        *undo_start,
                        *post_start,
                        *end,
                    ],
                    ip,
                    compiled_fns,
                )?;
            }
            OpCode::BlockLocalScope { body_end } => {
                self.exec_block_local_scope_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::CheckPhaser {
                is_pre,
                condition_idx,
            } => {
                let condition = condition_idx.map(|idx| Self::const_str(code, idx).to_string());
                self.exec_check_phaser_op(*is_pre, condition)?;
                *ip += 1;
            }
            OpCode::LeaveGuard { .. } => {
                // No-op marker; the guarded queue runner uses the `next` field
                // to find the next LEAVE phaser boundary on error.
                *ip += 1;
            }
            OpCode::DoBlockExpr {
                body_end,
                label,
                scope_isolate,
            } => {
                self.exec_do_block_expr_op(
                    code,
                    *body_end,
                    label,
                    *scope_isolate,
                    ip,
                    compiled_fns,
                )?;
            }
            OpCode::OnceExpr { key_idx, body_end } => {
                self.exec_once_expr_op(code, *key_idx, *body_end, ip, compiled_fns)?;
            }
            OpCode::DoGivenExpr { body_end } => {
                self.exec_do_given_expr_op(code, *body_end, ip, compiled_fns)?;
            }

            // -- Closures and registration --
            OpCode::MakeGather(idx) => {
                self.exec_make_gather_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::Eager => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                let result = match val {
                    Value::LazyList(ref ll) => {
                        let items = self.force_lazy_list_vm(ll)?;
                        // Sync interpreter env changes back to VM locals.
                        // This ensures side effects from gather bodies propagate
                        // to outer-scope variables (e.g., `$was-lazy = 0`).
                        for (i, name) in code.locals.iter().enumerate() {
                            if let Some(v) = self.interpreter.env().get(name)
                                && i < self.locals.len()
                            {
                                self.locals[i] = v.clone();
                            }
                        }
                        Value::array(items)
                    }
                    Value::Seq(items) => {
                        // Consuming the Seq via eager marks it as consumed.
                        crate::value::seq_sink(&items);
                        Value::array(items.to_vec())
                    }
                    ref other if other.is_range() => {
                        Value::array(crate::runtime::utils::value_to_list(&val))
                    }
                    other => other,
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::MakeAnonSub(idx, cc_idx, is_block) => {
                self.exec_make_anon_sub_op(code, *idx, *cc_idx, *is_block)?;
                *ip += 1;
            }
            OpCode::MakeAnonSubParams(idx, cc_idx, is_wc) => {
                self.exec_make_anon_sub_params_op(code, *idx, *cc_idx, *is_wc)?;
                *ip += 1;
            }
            OpCode::MakeLambda(idx, cc_idx, is_wc) => {
                self.exec_make_lambda_op(code, *idx, *cc_idx, *is_wc)?;
                *ip += 1;
            }
            OpCode::IndexAssignGeneric => {
                self.exec_index_assign_generic_op(code)?;
                *ip += 1;
            }
            OpCode::MakeBlockClosure(idx, cc_idx) => {
                self.exec_make_block_closure_op(code, *idx, *cc_idx)?;
                *ip += 1;
            }
            OpCode::RegisterSub(idx) => {
                self.exec_register_sub_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterToken(idx) => {
                self.exec_register_token_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterProtoSub(idx) => {
                self.exec_register_proto_sub_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterProtoToken(idx) => {
                self.exec_register_proto_token_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::UseModule { name_idx, tags_idx } => {
                self.exec_use_module_op(code, *name_idx, *tags_idx)?;
                *ip += 1;
            }
            OpCode::ImportModule { name_idx, tags_idx } => {
                self.exec_import_module_op(code, *name_idx, *tags_idx)?;
                *ip += 1;
            }
            OpCode::NoModule(name_idx) => {
                self.exec_no_module_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::NeedModule(name_idx) => {
                self.exec_need_module_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::UseLibPath => {
                self.exec_use_lib_path_op(code)?;
                *ip += 1;
            }
            OpCode::PushImportScope => {
                self.interpreter.push_import_scope();
                *ip += 1;
            }
            OpCode::PopImportScope => {
                self.interpreter.pop_import_scope();
                *ip += 1;
            }
            OpCode::RegisterEnum(idx) => {
                self.exec_register_enum_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterClass(idx) => {
                self.exec_register_class_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::AugmentClass(idx) => {
                self.exec_augment_class_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterRole(idx) => {
                self.exec_register_role_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterSubset(idx) => {
                self.exec_register_subset_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::SubtestScope { body_end } => {
                self.exec_subtest_scope_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::ReactScope { body_end } => {
                self.exec_react_scope_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::WheneverScope {
                body_idx,
                param_idx,
                target_var_idx,
            } => {
                self.exec_whenever_scope_op(code, *body_idx, param_idx, target_var_idx)?;
                *ip += 1;
            }

            // -- Local variables --
            OpCode::GetLocal(idx) => {
                self.exec_get_local_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::GetLocalRaw(idx) => {
                self.exec_get_local_raw_op(code, *idx);
                *ip += 1;
            }
            OpCode::SetLocal(idx) => {
                self.exec_set_local_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::SetVarDynamic { name_idx, dynamic } => {
                self.exec_set_var_dynamic_op(code, *name_idx, *dynamic);
                *ip += 1;
            }
            OpCode::RegisterVarExport { name_idx, tags_idx } => {
                self.exec_register_var_export_op(code, *name_idx, *tags_idx)?;
                *ip += 1;
            }
            OpCode::ApplyVarTrait {
                name_idx,
                trait_name_idx,
                has_arg,
            } => {
                self.exec_apply_var_trait_op(code, *name_idx, *trait_name_idx, *has_arg)?;
                *ip += 1;
            }
            OpCode::GetCallerVar { name_idx, depth } => {
                let name = Self::const_str(code, *name_idx);
                let val = self.interpreter.get_caller_var(name, *depth as usize)?;
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::SetCallerVar { name_idx, depth } => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                let name = Self::const_str(code, *name_idx);
                self.interpreter
                    .set_caller_var(name, *depth as usize, val)?;
                *ip += 1;
            }
            OpCode::BindCallerVar {
                target_idx,
                source_idx,
                depth,
            } => {
                let target = Self::const_str(code, *target_idx);
                let source = Self::const_str(code, *source_idx);
                self.interpreter
                    .bind_caller_var(target, source, *depth as usize)?;
                *ip += 1;
            }
            OpCode::GetOuterVar { name_idx, depth } => {
                let name = Self::const_str(code, *name_idx);
                let val = self.get_outer_var(code, name, *depth as usize);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetDynamicVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self.interpreter.get_dynamic_var(name)?;
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::AssignExprLocal(idx) => {
                self.exec_assign_expr_local_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::AssignReadOnly => {
                return Err(RuntimeError::assignment_ro(None));
            }
            OpCode::CheckReadOnly(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                self.interpreter.check_readonly_for_modify(name)?;
                // Also check env-based readonly status set by cross-scope
                // `:=` binding (e.g. binding to a readonly sub parameter
                // in a closure).  The readonly_vars set is scope-local
                // and gets restored on frame pop, but the env key persists.
                let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
                if matches!(
                    self.interpreter.env().get(&readonly_key),
                    Some(Value::Bool(true))
                ) {
                    return Err(RuntimeError::assignment_ro(Some(name)));
                }
                *ip += 1;
            }
            OpCode::MarkVarReadonly(name_idx) => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.interpreter.mark_readonly(&name);
                *ip += 1;
            }

            // -- Let scope management --
            OpCode::LetSave {
                name_idx,
                index_mode,
                is_temp,
            } => {
                self.exec_let_save_op(code, *name_idx, *index_mode, *is_temp);
                *ip += 1;
            }
            OpCode::LetBlock { body_end } => {
                self.exec_let_block_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::SetSourceLine(line) => {
                self.interpreter
                    .env_insert("?LINE".to_string(), Value::Int(*line));
                *ip += 1;
            }
        }
        Ok(())
    }

    /// Check if a value represents a "successful" block exit for `let` purposes.
    /// A block is considered successful if it returns a defined value.
    /// Type objects (Package) and Nil are undefined and count as failure.
    fn is_let_success(val: &Value) -> bool {
        crate::runtime::types::value_is_defined(val)
    }
}
