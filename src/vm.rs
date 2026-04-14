#![allow(clippy::result_large_err)]
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::ast::Stmt;
use crate::env::Env;
use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{ArrayKind, EnumValue, JunctionKind, LazyList, RuntimeError, Value, make_rat};
use num_traits::{Signed, Zero};

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
mod vm_method_dispatch;
mod vm_misc_ops;
mod vm_native_dispatch;
mod vm_register_ops;
mod vm_set_ops;
pub(crate) mod vm_smart_match;
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
    pub saved_readonly: HashSet<String>,
    pub saved_env_dirty: bool,
    pub saved_locals_dirty: bool,
    pub saved_local_bind_pairs: Vec<(usize, usize)>,
}

pub(crate) struct VM {
    interpreter: Interpreter,
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
    /// Container name from when/default body (for Scalar container binding)
    container_ref_var: Option<String>,
    /// When true, the container ref is reversed (e.g. `for @a.reverse`)
    container_ref_reversed: bool,
    /// Source variable name for topic binding in for loops
    topic_source_var: Option<String>,
    /// Stack of saved call frames for compiled function/closure/method calls.
    call_frames: Vec<VmCallFrame>,
    /// When true, locals may be stale relative to env (interpreter bridge modified env).
    /// Cleared after sync_locals_from_env or pop_call_frame.
    env_dirty: bool,
    /// When true, env may be stale relative to locals (simple local SetLocal skipped env write).
    /// Cleared after ensure_env_synced or sync_env_from_locals.
    locals_dirty: bool,
    /// The instruction pointer to resume at after a .resume call in a CATCH block.
    /// Set when Die/Fail creates an exception, used by exec_try_catch_op.
    resume_ip: Option<usize>,
    /// When true, the next SetLocal is a `:=` bind (preserves container type for `@` vars).
    bind_context: bool,
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
    /// Stack of sets tracking variable names declared (via SetVarDynamic) within
    /// each active BlockScope. Used during BlockScope restoration to avoid
    /// propagating block-local variable values to the outer scope.
    block_declared_vars: Vec<std::collections::HashSet<String>>,
}

impl VM {
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
            Arc::make_mut(attributes).insert("handled".to_string(), Value::Bool(true));
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
        Self {
            interpreter,
            stack: Vec::new(),
            locals: Vec::new(),
            in_smartmatch_rhs: false,
            transliterate_in_smartmatch: false,
            substitution_in_smartmatch: false,
            last_topic_value: None,
            container_ref_var: None,
            container_ref_reversed: false,
            topic_source_var: None,
            call_frames: Vec::new(),
            env_dirty: false,
            locals_dirty: false,
            resume_ip: None,
            bind_context: false,
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
            block_declared_vars: Vec::new(),
        }
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
                    return (
                        self.interpreter,
                        Err(RuntimeError::controlflow_return(true)),
                    );
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
        (
            self.interpreter,
            Ok(last_stack_value.or(self.last_topic_value)),
        )
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
                    ip += 1;
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
                                    let cur = self.interpreter.current_package().to_string();
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
                        // Bare-name fallback: when looking up an unqualified
                        // name (e.g. `msg` or `$msg`) inside a routine whose
                        // current_package is a real package (e.g. `Gee`), try
                        // resolving via the package's `our` store. This makes
                        // `our $msg` accessible from `our sub talk { $msg }`
                        // when `talk` is invoked from outside the package.
                        if name.contains("::") {
                            return None;
                        }
                        let cur = self.interpreter.current_package().to_string();
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
                    .unwrap_or_else(|| {
                        if name.starts_with('^') {
                            Value::Bool(true)
                        } else {
                            Value::Nil
                        }
                    });
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
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetArrayVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self
                    .get_env_with_main_alias(name)
                    .or_else(|| self.get_local_by_bare_name(code, name))
                    .or_else(|| {
                        // Fallback: check bare name in env (for closures capturing params)
                        name.strip_prefix('@')
                            .and_then(|bare| self.interpreter.env().get(bare).cloned())
                    })
                    .unwrap_or(Value::Nil);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetHashVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self
                    .get_env_with_main_alias(name)
                    .or_else(|| self.get_local_by_bare_name(code, name))
                    .or_else(|| {
                        name.strip_prefix('%')
                            .and_then(|bare| self.interpreter.env().get(bare).cloned())
                    });
                match val {
                    Some(v) => self.stack.push(v),
                    None => {
                        // %ENV (without * twigil) is not declared in Raku;
                        // only %*ENV is valid. Throw an undeclared error for %ENV specifically.
                        if name == "%ENV" {
                            return Err(RuntimeError::undeclared("name", "%ENV"));
                        }
                        self.stack.push(Value::Nil);
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
                self.bind_context = false;
                let name = match &code.constants[*name_idx as usize] {
                    Value::Str(s) => s.to_string(),
                    _ => unreachable!("SetGlobal name must be a string constant"),
                };
                if self.interpreter.strict_mode
                    && !name.contains("::")
                    && !self.interpreter.env().contains_key(&name)
                {
                    return Err(self.strict_undeclared_error(&name));
                }
                // Check readonly variables (e.g., $*USAGE)
                self.interpreter.check_readonly_for_modify(&name)?;
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
                    match raw_val {
                        Value::Array(items, kind) if kind.is_real_array() => {
                            Value::Array(items, kind)
                        }
                        Value::Array(items, _) => {
                            Value::Array(items, crate::value::ArrayKind::List)
                        }
                        other => Value::Array(
                            std::sync::Arc::new(vec![other]),
                            crate::value::ArrayKind::List,
                        ),
                    }
                } else if raw_mode {
                    raw_val
                } else if name.starts_with('%') {
                    runtime::coerce_to_hash(raw_val)
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
                        return Err(RuntimeError::new(
                            runtime::utils::type_check_assignment_error(&name, &constraint, &val),
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
                    let mut resolved_source = source_name;
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
                        .insert(alias_key.clone(), Value::str(resolved_source));
                    self.interpreter
                        .env_mut()
                        .insert(readonly_key.clone(), Value::Bool(false));
                }
                if raw_mode && name.starts_with('@') {
                    // For `constant @x`, bypass set_shared_var's List→Array
                    // normalization so the container type (List) is preserved.
                    self.interpreter.env_mut().insert(name.clone(), val.clone());
                } else {
                    self.set_env_with_main_alias(&name, val.clone());
                }
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
                *ip += 1;
            }
            OpCode::SetVarType { name_idx, tc_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                let constraint = Self::const_str(code, *tc_idx).to_string();
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
                        let type_obj = Value::Package(Symbol::intern(
                            &self
                                .interpreter
                                .var_type_constraint(&name)
                                .unwrap_or(constraint.clone()),
                        ));
                        self.set_env_with_main_alias(&name, type_obj.clone());
                        self.update_local_if_exists(code, &name, &type_obj);
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
                    self.interpreter
                        .register_container_type_metadata(&value, info);
                }
                *ip += 1;
            }
            OpCode::SetTopic => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                self.last_topic_value = Some(val.clone());
                self.interpreter.env_mut().insert("_".to_string(), val);
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
                    other => other,
                };
                self.stack.push(itemized);
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
            } => {
                self.exec_smart_match_expr_op(
                    code,
                    ip,
                    *rhs_end,
                    *negate,
                    lhs_var,
                    *rhs_is_match_regex,
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

            // -- Pair --
            OpCode::MakePair => {
                self.exec_make_pair_op();
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
                        Value::Instance { attributes, .. } => (**attributes).clone(),
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
                        std::sync::Arc::new(items.to_vec()),
                        crate::value::ArrayKind::List,
                    ),
                    other => Value::Array(
                        std::sync::Arc::new(vec![other]),
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
                        Value::LazyIoLines { handle, .. } => {
                            // Sinking a lazy IO lines iterator must drain the
                            // underlying handle so that side effects (read
                            // position, .eof) are observable.
                            self.interpreter.force_lazy_io_lines(handle)?;
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
                                let exitcode = match attributes.get("exitcode") {
                                    Some(Value::Int(i)) => *i,
                                    _ => 0,
                                };
                                if exitcode != 0 {
                                    let signal = match attributes.get("signal") {
                                        Some(Value::Int(i)) => *i,
                                        _ => 0,
                                    };
                                    let command = attributes
                                        .get("command")
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let msg = format!(
                                        "The spawned command '{}' exited unsuccessfully (exit code: {}, signal: {})",
                                        command, exitcode, signal
                                    );
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
                self.exec_make_capture_op(*n);
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
                self.exec_call_func_op(code, *name_idx, *arity, *arg_sources_idx, compiled_fns)?;
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
                self.exec_call_method_op(
                    code,
                    *name_idx,
                    *arity,
                    *modifier_idx,
                    *quoted,
                    *arg_sources_idx,
                )?;
                *ip += 1;
            }
            OpCode::CallMethodDynamic { arity } => {
                self.exec_call_method_dynamic_op(code, *arity)?;
                *ip += 1;
            }
            OpCode::CallMethodDynamicMut {
                arity,
                target_name_idx,
            } => {
                self.exec_call_method_dynamic_mut_op(code, *arity, *target_name_idx)?;
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
                self.exec_call_method_mut_op(
                    code,
                    *name_idx,
                    *arity,
                    *target_name_idx,
                    *modifier_idx,
                    *quoted,
                    *arg_sources_idx,
                )?;
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
                self.exec_exec_call_op(code, *name_idx, *arity, *arg_sources_idx, compiled_fns)?;
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
            OpCode::DeleteIndexNamed(name_idx) => {
                self.exec_delete_index_named_op(code, *name_idx)?;
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
                self.exec_multi_dim_index_assign_op(code, *name_idx, *ndims)?;
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
            OpCode::IndexAssignExprNamed(name_idx) => {
                self.exec_index_assign_expr_named_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::IndexAssignPseudoStashNamed {
                stash_name_idx,
                key_name_idx,
            } => {
                self.exec_index_assign_pseudo_stash_named_op(code, *stash_name_idx, *key_name_idx)?;
                *ip += 1;
            }
            OpCode::IndexAssignExprNested(name_idx) => {
                self.exec_index_assign_expr_nested_op(code, *name_idx)?;
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
                };
                self.exec_for_loop_op(code, &spec, ip, compiled_fns)?;
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
            OpCode::Given { body_end } => {
                self.exec_given_op(code, *body_end, ip, compiled_fns)?;
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
                return Err(self.runtime_error_from_exception_value(val, "Died", false));
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
                    if let Some(exc) = attributes.get("exception") {
                        exc.clone()
                    } else {
                        val
                    }
                } else {
                    val
                };
                // Build a backtrace from the routine stack so that
                // Exception.gist can show where the fail originated.
                let backtrace = self.build_backtrace_string();
                let mut err = self.runtime_error_from_exception_value(val, "Failed", true);
                // Attach backtrace to the exception value
                if let Some(ref mut exc_box) = err.exception
                    && let Value::Instance { attributes, .. } = exc_box.as_mut()
                {
                    std::sync::Arc::make_mut(attributes)
                        .insert("backtrace".to_string(), Value::str(backtrace));
                }
                return Err(err);
            }
            OpCode::Return => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                // Check if &return has been lexically rebound; if so, call
                // the rebound function instead of performing a built-in return.
                self.ensure_env_synced(code);
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
                self.exec_exists_index_adv_op(*flags)?;
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
                self.update_local_if_exists(code, &name, &pkg_val);
                self.env_dirty = true;
                *ip += 1;
            }
            OpCode::RegisterPackageStub { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.interpreter.package_stubs.insert(name);
                *ip += 1;
            }
            OpCode::ClearPackageStub { name_idx } => {
                let name = Self::const_str(code, *name_idx).to_string();
                self.interpreter.package_stubs.remove(&name);
                *ip += 1;
            }

            // -- Phaser END --
            OpCode::PhaserEnd(idx) => {
                self.exec_phaser_end_op(code, *idx);
                *ip += 1;
            }

            // -- HyperMethodCall --
            OpCode::HyperMethodCall {
                name_idx,
                arity,
                modifier_idx,
                quoted,
            } => {
                self.exec_hyper_method_call_op(code, *name_idx, *arity, *modifier_idx, *quoted)?;
                *ip += 1;
            }
            OpCode::HyperMethodCallDynamic {
                arity,
                modifier_idx,
            } => {
                self.exec_hyper_method_call_dynamic_op(code, *arity, *modifier_idx)?;
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
            } => {
                self.exec_hyper_func_op(code, *name_idx, *dwim_left, *dwim_right, compiled_fns)?;
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
            OpCode::CheckPhaser { is_pre } => {
                self.exec_check_phaser_op(*is_pre)?;
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
                    Value::Seq(items) => Value::array(items.to_vec()),
                    ref other if other.is_range() => {
                        Value::array(crate::runtime::utils::value_to_list(&val))
                    }
                    other => other,
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::MakeAnonSub(idx, cc_idx) => {
                self.exec_make_anon_sub_op(code, *idx, *cc_idx)?;
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
                self.exec_index_assign_generic_op()?;
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
