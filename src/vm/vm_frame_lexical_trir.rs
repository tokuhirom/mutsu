//! The TRIR door of a frame-lexical call (ADR-0110 §4's generic prologue,
//! reached from `call_frame_lexical_target`).
//!
//! A routine-nested `my sub` called from its enclosing routine or a sibling
//! (JSON::Fast's `to-json` calling `jsonify`, `jsonify` calling
//! `pretty-associative`, which calls `jsonify` back) is called through the
//! frame-lexical call table, not `CallFunc`'s cached call, and that path
//! never tried the routine's TRIR chunk: every such call took the light or
//! general binder even when the body compiled. This tries it first, on the
//! same terms as `exec_call_func_op`'s door, and leaves the stack exactly as
//! it found it on a decline.

use super::*;

impl Interpreter {
    /// Run `cf`'s TRIR chunk for this call, or `None` (nothing consumed) when
    /// the routine has none or the chunk declines these arguments.
    // Cost: O(a) to stage the a arguments, plus the routine body.
    pub(super) fn try_frame_lexical_trir(
        &mut self,
        code: &CompiledCode,
        cf: &CompiledFunction,
        args: &[Value],
        call_has_named: bool,
        compiled_fns: &CompiledFns,
    ) -> Option<Result<Value, RuntimeError>> {
        // The same admissions as the `CallFunc` door: a parameterless
        // routine only where `zero_param_trir_ok` allows it (#18080), no
        // named argument, no wrapper to reach (ADR-0110 §3.3). A junction
        // argument was autothreaded, and a slip spread
        // (`spread_call_args_by_syntax`), before this point.
        if cf.trir.is_none()
            || call_has_named
            || (cf.param_defs.is_empty() && !Self::zero_param_trir_ok(cf))
            || self.any_routine_wrapped()
        {
            return None;
        }
        let compiled_fns = cf.compiled_fns.as_deref().unwrap_or(compiled_fns);
        let base = self.stack.len();
        self.stack.extend(args.iter().cloned());
        let out = self.try_call_trir(cf, base, Some(code), compiled_fns);
        if out.is_none() {
            self.stack.truncate(base);
        }
        out
    }
}

impl Interpreter {
    /// The frame-lexical routine a TRIR generic call site names, if `refs`
    /// (the chunk's `TrChunk::lexical_routines`) lists one by that name and
    /// this interpreter has derived it.
    // Cost: O(r), r = the chunk's frame-lexical callees (a handful), plus
    // one hash probe.
    pub(crate) fn trir_frame_lexical_target(
        &self,
        refs: &[crate::opcode::FrameLexicalRef],
        name: Symbol,
    ) -> Option<FrameLexicalTarget> {
        let r = refs.iter().find(|r| r.name == name)?;
        self.lexicals.frame_lexical_routines.get(r).cloned()
    }

    /// Call a frame-lexical routine from a TRIR body with already-evaluated
    /// arguments, through the same dispatch the bytecode's frame-lexical call
    /// uses. A TRIR frame has no bytecode of its own, so the callee's body
    /// stands in as the caller's chunk: the arguments are values or shared
    /// cells, never the `WrapVarRef`s whose promotion would index a caller's
    /// locals by it.
    // Cost: O(a) for the a arguments, plus the callee.
    pub(crate) fn trir_call_frame_lexical(
        &mut self,
        target: &FrameLexicalTarget,
        args: Vec<Value>,
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        // Any by-name write-back the callee records is left for the frame
        // that owns the variable to drain, exactly as for the generic call
        // below it (`call_function`): a TRIR frame has no slots to receive it.
        let code = std::sync::Arc::clone(&target.cf.code);
        self.call_frame_lexical_target(&code, target, args, None, false, compiled_fns)
    }
}
