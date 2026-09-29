//! Evaluating a parameter's signature-time expressions (ADR-0132).
//!
//! The binder, multi-candidate selection and sub-signature matching all ask
//! the same three questions of a `ParamDef` — what does its `where` clause,
//! its default, or a shape dimension evaluate to — and each answers it here.
//! The compiler precompiled the expression into the parameter's `ParamCode`
//! when it compiled the owning routine, so answering runs that chunk. Only a
//! `ParamDef` synthesized at runtime (introspection, `.assuming`) arrives
//! without one and still evaluates its AST.
use super::*;

impl Interpreter {
    /// The value of `pd`'s `where` clause, before the caller's truthiness or
    /// smartmatch test: the result of a `where { … }` block, or the clause's
    /// own value (`where 1..5` yields the Range). A one-argument WhateverCode
    /// (`where * > 0`) is answered inline, as a Bool. The caller has already
    /// bound `$_`, the parameter and any placeholders in the env.
    ///
    /// `record_free_var_writes` is
    /// [`Interpreter::eval_block_value_recording_writes`]'s flag: a clause
    /// that assigns a caller lexical (`where { $t ~= 'a' }`) has the write
    /// carried back to the caller's slot.
    ///
    /// Panics if `pd` has no `where` clause; every caller checks first.
    // Cost: O(1) beyond running the clause itself (a precompiled chunk).
    pub(crate) fn eval_param_where_value(
        &mut self,
        pd: &ParamDef,
        record_free_var_writes: bool,
    ) -> Result<Value, RuntimeError> {
        if let Some(chunks) = pd.code.get()
            && let Some(chunk) = chunks.where_chunk.as_ref()
        {
            let value = self.eval_precompiled_block_value(chunk, record_free_var_writes);
            if !chunks.where_inline_predicate {
                return value;
            }
            // The chunk is the WhateverCode's body, so its value IS the
            // verdict. Answer with a Bool, which every caller's smartmatch
            // passes through unchanged. A throw inside the body rejects the
            // value, as smartmatching against the closure did (rakudo agrees:
            // `sub f($x where * < 100) { }; f("abc")` is a binding failure,
            // not X::Str::Numeric).
            return Ok(if value.is_ok_and(|v| v.truthy()) {
                Value::TRUE
            } else {
                Value::FALSE
            });
        }
        let where_expr = pd
            .where_constraint
            .as_deref()
            .expect("eval_param_where_value on a parameter without a where clause");
        // TODO: compile to bytecode — a runtime-synthesized `ParamDef` has no
        // chunk (ADR-0132 Decision 3), so its clause is compiled per call.
        let body = crate::compiler::Compiler::where_chunk_body(where_expr);
        if record_free_var_writes {
            self.eval_block_value_recording_writes(&body)
        } else {
            self.eval_block_value(&body)
        }
    }

    /// The value of `pd`'s default expression `default_expr` (which is
    /// `pd.default`), in whatever scope the caller has prepared.
    // Cost: O(1) beyond running the default itself (a precompiled chunk).
    pub(crate) fn eval_param_default_expr(
        &mut self,
        pd: &ParamDef,
        default_expr: &Expr,
    ) -> Result<Value, RuntimeError> {
        if let Some(chunk) = pd.code.get().and_then(|c| c.default_chunk.as_ref()) {
            return self.eval_precompiled_block_value(chunk, false);
        }
        // TODO: compile to bytecode — see `eval_param_where_value`.
        self.eval_block_value(&[Stmt::Expr(default_expr.clone())])
    }

    /// The value of `pd`'s shape dimension `index` (`dim_expr` is
    /// `pd.shape_constraints[index]`).
    // Cost: O(1) beyond running the dimension expression (a precompiled chunk).
    pub(crate) fn eval_param_shape_dim(
        &mut self,
        pd: &ParamDef,
        index: usize,
        dim_expr: &Expr,
    ) -> Result<Value, RuntimeError> {
        if let Some(chunk) = pd
            .code
            .get()
            .and_then(|c| c.shape_chunks.get(index))
            .and_then(Option::as_ref)
        {
            return self.eval_precompiled_block_value(chunk, false);
        }
        // TODO: compile to bytecode — see `eval_param_where_value`.
        self.eval_block_value(&[Stmt::Expr(dim_expr.clone())])
    }
}
