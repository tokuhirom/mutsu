//! The one closure-construction pipeline shared by the four closure opcodes
//! (`MakeAnonSub`, `MakeAnonSubParams`, `MakeLambda`, `MakeBlockClosure`).
//!
//! Each opcode used to carry its own copy of the capture pipeline and of the
//! `SubData` construction, and the copies drifted (#9454): only the AnonSub
//! pair froze read-only `:=` loop captures, only `MakeLambda` boxed Supply
//! container captures, and `MakeBlockClosure` never stripped an inherited
//! `__mutsu_return_type`. None of those differences was intentional:
//!
//! - **Read-only capture freeze.** A closure that only *reads* a lexical the
//!   enclosing loop re-binds with `:=` must see its own iteration's binding.
//!   That is a property of closures, not of `sub {}` literals, so every kind
//!   runs `freeze_readonly_owned_captures`.
//! - **Supply container boxing.** Gated on the body's `is_supply_block_body`
//!   flag, so it is a no-op for every body that is not a supply block, whatever
//!   opcode built it.
//! - **Return type.** A return type belongs to the routine that declares it and
//!   is never inherited lexically, so every kind strips the enclosing
//!   routine's `__mutsu_return_type` and only a declared `--> T` sets it again.
//!
//! What genuinely differs per kind is data, carried by [`ClosureSpec`]; the
//! opcode handlers only decode their pool statement into one.

use super::*;
use crate::value::ValueMap;

/// The per-kind inputs of [`Interpreter::build_closure`].
pub(super) struct ClosureSpec<'a> {
    /// `anon` for an anonymous literal, the declared name for `anon sub NAME`.
    pub(super) name: Symbol,
    pub(super) signature: crate::opcode::ClosureSignature,
    /// Whether an empty signature means "takes no arguments" (`-> {}`,
    /// `sub () {}`) rather than "no signature at all" (a bare block).
    pub(super) empty_sig: bool,
    pub(super) is_rw: bool,
    pub(super) is_raw: bool,
    /// `Block` rather than `Sub` (a bare block, a placeholder block or a
    /// pointy block).
    pub(super) is_bare_block: bool,
    /// The closure's own declared `--> T`, if any.
    pub(super) return_type: Option<&'a str>,
    /// Overrides the callable's type (`WhateverCode`, `Method`, `Submethod`).
    pub(super) callable_type: Option<&'static str>,
    /// A bare block is not a routine boundary: when it performs a regex match,
    /// its `$/` is the enclosing scope's, so the block captures that cell.
    pub(super) capture_match_var: bool,
}

impl Interpreter {
    /// Build the closure value for pool slot `idx` of `code`, compiled as
    /// `cc_idx`. Does not push it.
    pub(super) fn build_closure(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
        spec: ClosureSpec<'_>,
    ) -> Value {
        // See `closures_created` doc comment: a routine-registry restore gate
        // consults this to detect a closure literal escaping via a side
        // channel (not just the return value).
        self.closures_created += 1;
        let compiled_code = Self::resolve_closure_code(code, cc_idx);
        self.note_frame_lexical_closure_body(code, idx, &compiled_code);
        if spec.capture_match_var {
            self.capture_block_match_var(code, &compiled_code);
        }
        self.box_captured_lexicals(code, &compiled_code);
        if compiled_code
            .as_ref()
            .is_some_and(|cc| cc.is_supply_block_body)
        {
            self.box_supply_container_captures(code, &compiled_code);
        }
        let owned_captures = self.compute_owned_captures(&compiled_code);
        let authoritative_captures = self.compute_authoritative_captures(&compiled_code);
        let mut upvalues = self.capture_upvalues(code, &compiled_code);
        // Upvalue snapshot (single-store Slice E); see `capture_closure_env`.
        let mut env = self.capture_closure_env(code, &compiled_code);
        self.freeze_readonly_owned_captures(
            code,
            &compiled_code,
            &owned_captures,
            &mut env,
            &mut upvalues,
        );
        // A return type is never inherited lexically: the captured env may
        // carry the *enclosing* routine's `__mutsu_return_type`, which would
        // then be enforced on this closure's own return (`sub f(--> blob32) {
        // ({ $^a + $^b })[0](…) }` reported the inner block's Int as a bad
        // `blob32` return). Symbol-keyed: this runs on EVERY closure creation,
        // and the `String`-keyed forms would allocate and re-hash the literal
        // each time. See `symbol::well_known`.
        env.remove_sym(crate::symbol::well_known::return_type());
        if let Some(rt) = spec.return_type {
            env.insert_sym(
                crate::symbol::well_known::return_type(),
                Value::str_from(rt),
            );
        }
        if let Some(callable_type) = spec.callable_type {
            env.insert_sym(
                crate::symbol::well_known::callable_type(),
                Value::str_from(callable_type),
            );
        }
        let source_line = compiled_code
            .as_ref()
            .and_then(|cc| cc.source_line)
            .map(|l| l as u32)
            .or_else(|| self.current_source_line());
        let compiled_fns = compiled_code
            .as_ref()
            .and_then(|cc| cc.compiled_fns.clone());
        Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
            package: self.lexical_closure_package_sym(),
            name: spec.name,
            empty_sig: spec.empty_sig,
            params: spec.signature.params,
            param_defs: spec.signature.param_defs,
            body: code.closure_body_arc(idx as usize),
            is_rw: spec.is_rw,
            is_raw: spec.is_raw,
            env,
            assumed_positional: Vec::new(),
            assumed_named: ValueMap::default(),
            id: crate::value::next_instance_id(),
            is_bare_block: spec.is_bare_block,
            owned_captures,
            authoritative_captures,
            upvalues,
            compiled_code,
            compiled_fns,
            compiled_routine: None,
            is_decl_expr_thunk: false,
            deprecated_message: None,
            source_line,
            // Not `current_source_file()`: that reads the dynamically-scoped
            // `?FILE` env var, which only tracks the unit currently being
            // *loaded* (see `run_modules.rs`) -- correct for a closure built
            // while its module loads, but wrong for one built later, each time
            // an already-loaded module's routine runs and constructs this
            // literal afresh (`?FILE` has reverted to the caller's own file by
            // then; a `-> $v {...}` handed to `.tap` from inside a module could
            // not reach its own compunit's private routines).
            // `executing_source_file()` reads the file baked onto the innermost
            // enclosing routine frame's own `def_file` instead, which stays
            // correct regardless of who is calling.
            source_file: self.executing_source_file(),
            captured_fatal_mode: self.fatal_mode,
            param_name_syms_cache: std::sync::OnceLock::new(),
            source_file_sym_cache: std::sync::OnceLock::new(),
        }))
    }

    /// A bare block that performs a regex match is not a routine boundary: its
    /// `$/` belongs to the lexical scope where it was written, even when
    /// another routine invokes the block. Only such blocks capture the match
    /// variable: capturing it for every callback lets an unrelated nested
    /// routine's match shadow grammar-action `$/` bindings (YAMLish is a
    /// representative failure).
    fn capture_block_match_var(
        &mut self,
        code: &CompiledCode,
        compiled_code: &Option<std::sync::Arc<CompiledCode>>,
    ) {
        let block_writes_match = compiled_code.as_ref().is_some_and(|cc| {
            cc.ops.iter().any(|op| {
                matches!(
                    op,
                    OpCode::SmartMatchExpr {
                        rhs_pure_regex: true,
                        ..
                    }
                )
            })
        });
        if !block_writes_match || self.env().get("/").is_some_and(Value::is_container_ref) {
            return;
        }
        let slash = self
            .env()
            .get("/")
            .cloned()
            .unwrap_or(Value::NIL)
            .into_container_ref();
        self.env_mut().insert("/".to_string(), slash.clone());
        // `$/` can also occupy a local slot in the defining compiled frame.
        // Keep that slot on the same cell; otherwise a later caller-return
        // reconciliation would restore its stale Match value over the
        // captured binding.
        for (slot, name) in code.locals.iter().enumerate() {
            if name == "/" && slot < self.locals.len() {
                self.locals[slot] = slash.clone();
            }
        }
    }
}
