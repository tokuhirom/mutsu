//! Carrying frame-lexical call tables (ADR-0113) onto chunks compiled
//! outside the routine body's own compile.
//!
//! A closure's compiled chunk is equipped with the frame-lexical routines it
//! calls by `frame_lexical_routines.rs`. Several runtime paths do not run
//! that chunk but compile the closure's AST again (the inline `map`/`grep`
//! path, a sequence generator, the carrier `eval_block_value` family). The
//! fresh chunk calls the same routines by the same bare names, and none of
//! them is in the registry, so it must inherit the table.

use super::frame_lexical_routines::{scan_chunk, visit_code, visit_code_mut};
use crate::opcode::{CompiledCode, FrameLexicalRef};
use crate::symbol::Symbol;
use std::collections::HashSet;

/// Set [`CompiledCode::lexical_subtree`] on every chunk whose subtree lists
/// a frame-lexical routine, copying a nested chunk out of its `Arc` only when
/// its flag changes.
pub(super) fn mark_lexical_subtree(code: &mut CompiledCode) -> bool {
    fn has(code: &CompiledCode) -> bool {
        !code.lexical_routines.is_empty() || code.closure_compiled_codes.iter().any(|c| has(c))
    }
    let mut any = !code.lexical_routines.is_empty();
    for nested in &mut code.closure_compiled_codes {
        if has(nested) {
            any = true;
            if !nested.lexical_subtree {
                mark_lexical_subtree(std::sync::Arc::make_mut(nested));
            }
        }
    }
    code.lexical_subtree = any;
    any
}

/// A closure body the runtime compiles again from its AST (the `map`/`grep`
/// inline path, a sequence generator) runs that fresh chunk instead of the
/// closure's own compiled chunk. The fresh chunk calls the same routines by
/// the same bare names, so it inherits the frame-lexical call table of the
/// chunk it was compiled from (`origin`, including that chunk's nested
/// closures); otherwise its call to a frame-lexical routine would find no
/// registry entry.
pub(crate) fn inherit_frame_lexical_routines(
    target: &mut CompiledCode,
    fns: &mut crate::opcode::CompiledFns,
    origin: &CompiledCode,
) {
    let mut refs: Vec<FrameLexicalRef> = Vec::new();
    visit_code(origin, &mut |c| {
        for r in &c.lexical_routines {
            if !refs.contains(r) {
                refs.push(*r);
            }
        }
    });
    if refs.is_empty() {
        return;
    }
    let names: HashSet<Symbol> = refs.iter().map(|r| r.name).collect();
    let wants = |c: &CompiledCode| !scan_chunk(c, &names).0.is_empty();
    let mut equip = |c: &mut CompiledCode| {
        for sym in scan_chunk(c, &names).0 {
            if let Some(r) = refs.iter().find(|r| r.name == sym)
                && !c.lexical_routines.contains(r)
            {
                c.lexical_routines.push(*r);
            }
        }
    };
    visit_code_mut(target, &wants, &mut equip);
    mark_lexical_subtree(target);
    let keys: Vec<Symbol> = fns
        .iter()
        .filter(|(_, cf)| {
            let mut any = false;
            visit_code(&cf.code, &mut |c| any |= wants(c));
            any
        })
        .map(|(k, _)| *k)
        .collect();
    for key in keys {
        if let Some(cf) = fns.make_mut(&key) {
            let code = std::sync::Arc::make_mut(&mut cf.code);
            visit_code_mut(code, &wants, &mut equip);
            mark_lexical_subtree(code);
        }
    }
}

impl super::Compiler {
    /// Record that a bare call to `name` was compiled as a call on the `&name`
    /// binding that shadows it (see [`CompiledCode::amp_shadowed_calls`]).
    pub(super) fn note_amp_shadowed_call(&mut self, name: Symbol) {
        if !self.code.amp_shadowed_calls.contains(&name) {
            self.code.amp_shadowed_calls.push(name);
        }
    }

    /// Record that a bare `name(...)` call here reaches the routine a nearer
    /// scope declares, past an outer `&name` binding (see
    /// [`CompiledCode::routine_shadowed_calls`]).
    pub(super) fn note_routine_shadowed_call(&mut self, name: Symbol) {
        if !self.code.routine_shadowed_calls.contains(&name) {
            self.code.routine_shadowed_calls.push(name);
        }
    }

    /// Prepare a fresh compiler that is about to compile `origin`'s AST again
    /// (the inline `map`/`grep` path, a sequence generator): every bare call
    /// `origin` (or a closure nested in it) resolved to a shadowing `&name`
    /// binding must resolve the same way here, so declare those bindings in an
    /// enclosing scope. Without this the recompiled `h()` in
    /// `{ my &h = {10}; (1..2).map({ h() }) }` dispatched to an outer `sub h`.
    pub(crate) fn seed_amp_shadowed_calls_from(&mut self, origin: &CompiledCode) {
        let mut frame = super::lex_scope::ScopeFrame::new();
        visit_code(origin, &mut |c| {
            for name in &c.amp_shadowed_calls {
                frame.insert(format!("&{}", name.resolve()), None);
            }
        });
        if !frame.is_empty() {
            self.enclosing_scopes.push(frame);
        }
        // A block-declared routine the body read through its binding stays
        // one (`block_lexsubs.rs`); the closure carries it in its captured env.
        visit_code(origin, &mut |c| {
            for name in &c.lexsub_refs {
                if !self.enclosing_lexsubs.contains(name) {
                    self.enclosing_lexsubs.push(*name);
                }
            }
        });
    }
}

/// A closure body the runtime compiles again from its AST runs without an
/// upvalue array of its own, so its call of a block-declared routine
/// (`&g(...)`, ADR-12529 phase 2) resolved `&g` by name on every call
/// (#13049). Point each such `CallOnCodeVar` at `origin`'s upvalue for the
/// name instead: the runner installs the closure's upvalue array, which is
/// aligned with `origin.upvalue_syms`, while it runs the fresh chunk (the
/// inline `map` path). A name the fresh chunk binds itself (a `my &g`
/// anywhere in it) keeps the by-name call, as `compute_upvalues` would, and
/// a run without the array installed falls back to it too.
// Cost: O(n + u), n = `target`'s ops, u = `origin`'s upvalues. Once per
// compiled chunk.
pub(crate) fn bind_origin_code_var_upvalues(target: &mut CompiledCode, origin: &CompiledCode) {
    if origin.lexsub_refs.is_empty() {
        return;
    }
    let index_of = |name: &str| -> Option<u32> {
        if target
            .locals
            .iter()
            .any(|l| l.strip_prefix('&') == Some(name))
        {
            return None;
        }
        let i = origin
            .upvalue_syms
            .iter()
            .position(|s| s.with_str(|s| s.strip_prefix('&') == Some(name)))?;
        origin
            .lexsub_refs
            .iter()
            .any(|r| r.with_str(|r| r == name))
            .then_some(i as u32)
    };
    let rewrites: Vec<(usize, u32)> = target
        .ops
        .iter()
        .enumerate()
        .filter_map(|(pos, op)| match op {
            crate::opcode::OpCode::CallOnCodeVar {
                name_idx,
                upvalue: None,
                ..
            } => target
                .constants
                .get(*name_idx as usize)
                .and_then(crate::value::Value::as_str)
                .and_then(index_of)
                .map(|i| (pos, i)),
            _ => None,
        })
        .collect();
    for (pos, i) in rewrites {
        if let crate::opcode::OpCode::CallOnCodeVar { upvalue, .. } = &mut target.ops[pos] {
            *upvalue = Some(i);
        }
    }
}
