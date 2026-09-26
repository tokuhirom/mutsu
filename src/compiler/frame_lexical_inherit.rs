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
            visit_code_mut(&mut cf.code, &wants, &mut equip);
            mark_lexical_subtree(&mut cf.code);
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
    }
}
