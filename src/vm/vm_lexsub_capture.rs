//! A named sub's code object captures the activation that declared it
//! (ADR-12529 phase 3, #12692).
//!
//! Rakudo re-closures a `sub` on every entry of the block that declares it
//! (`capturelex`), so `&g` taken in one iteration of a loop body, or in one
//! call of a `.map` callback, keeps that iteration's lexicals:
//!
//! ```raku
//! my @r = (1, 2).map(-> $v { my $q = $v; sub g { $q }; &g });
//! say @r.map({ $_() });   # (1 2)
//! ```
//!
//! mutsu read such a sub's free variables by name at the call, through
//! stores keyed by the sub's name (ADR-0024's unit-lexical buckets,
//! mutsu#9111's per-activation aliases, the latest-activation cells), and a
//! block activation binds none of them per activation, so every `&g` answered
//! with the latest iteration's `$q`.
//!
//! Here the code object a `&g` read builds in the declaring code holds that
//! activation's cells for the sub's free variables -- boxed in place, as an
//! escaping closure's are (ADR-0018) -- and is marked
//! ([`crate::env::Env::mark_activation_capture`]). Calling it links the sub's
//! frame capture-first through the static link
//! (`call_compiled_closure_with_topic`), so a free variable resolves in the
//! declaring activation, past the callers, and the by-name stores step aside
//! for every name the capture binds (`unit_lexical_slot`). Nothing is looked
//! up through the caller chain to find the activation.

use super::*;

impl Interpreter {
    /// Fill the upvalues of the closure `cc` that `code` is creating for the
    /// block-declared routines it names (`CompiledCode::lexsub_refs`,
    /// ADR-12529 phase 2), and return each `&g` binding the closure ends up
    /// with. A routine `code` itself declares gets the code object of this
    /// activation's routine, which closes over this activation as a `&g` read
    /// here does; one `code` only hands on already holds the binding
    /// `capture_upvalues` took from this frame's own upvalues. A routine this
    /// interpreter cannot resolve leaves its entry empty, and the closure's
    /// call resolves by name.
    ///
    /// The closure also carries the returned bindings in its captured env: a
    /// runtime recompile of its body (the inline `map` path, which can run
    /// after this frame returned) has no upvalue array and reads `&g` by name.
    // Cost: O(r * (p + u) + capture), r = `cc`'s refs, p = `code`'s routine
    // declarations, u = `cc`'s upvalues; only for a closure that names a
    // block-declared routine.
    pub(super) fn bind_lexsub_upvalues(
        &mut self,
        code: &CompiledCode,
        cc: &CompiledCode,
        upvalues: &mut [Option<Value>],
    ) -> Vec<(Symbol, Value)> {
        let mut bound = Vec::new();
        for &name in &cc.lexsub_refs {
            let Some(i) = name.with_str(|n| {
                cc.upvalue_syms
                    .iter()
                    .position(|s| s.with_str(|s| s.strip_prefix('&') == Some(n)))
            }) else {
                continue;
            };
            let Some(entry) = upvalues.get_mut(i) else {
                continue;
            };
            if code.sub_decl_plans.iter().any(|p| p.name == name) {
                let val = self.resolve_code_var_unshadowed(name.as_str());
                if !matches!(val.view(), ValueView::Sub(_)) {
                    continue;
                }
                *entry = Some(self.capture_lexsub_activation(code, val));
            }
            // A body compiled at run time (the inline `map` path) runs without
            // the upvalue array it would hand on from; the binding then lives
            // in the env its closure carries.
            if entry.is_none()
                && let Some(val) = self.env().get_sym(cc.upvalue_syms[i])
                && matches!(val.view(), ValueView::Sub(_))
            {
                *entry = Some(val.clone());
            }
            if let Some(val) = entry {
                bound.push((cc.upvalue_syms[i], val.clone()));
            }
        }
        bound
    }

    /// `val`, the code object the declaring `code` just resolved for its own
    /// nested sub `name`, as a capture of the running activation: each free
    /// variable of the sub that is a local of `code` is boxed into a shared
    /// cell in place and bound in the capture. `val` unchanged when the sub
    /// has none (it reads nothing of this activation).
    // Cost: O(f * l), f = the sub's free variables, l = `code`'s locals; only
    // for a `&name` read of a sub the running code declares.
    pub(super) fn capture_lexsub_activation(&mut self, code: &CompiledCode, val: Value) -> Value {
        let ValueView::Sub(data) = val.view() else {
            return val;
        };
        let Some(cf) = data.compiled_routine.clone() else {
            return val;
        };
        let mut cells: Vec<(Symbol, Value)> = Vec::new();
        for sym in cf
            .code
            .free_var_syms
            .iter()
            .chain(cf.code.free_var_writes.iter())
        {
            if cells.iter().any(|(s, _)| s == sym) {
                continue;
            }
            let name = sym.resolve();
            if !crate::env::is_plain_user_lexical(&name) {
                continue;
            }
            // The innermost declaration of the name in this code: a nested
            // block's `my $q` takes a later slot than an outer one.
            let Some(slot) = code.locals.iter().rposition(|n| *n == name) else {
                continue;
            };
            if let Some(cell) = self.box_local_for_lexsub_capture(code, slot, &name) {
                cells.push((*sym, cell));
            }
        }
        if cells.is_empty() {
            return val;
        }
        let mut new_data = (**data).clone();
        for (sym, cell) in cells {
            new_data.env.insert_sym(sym, cell);
        }
        new_data.env.mark_activation_capture();
        Value::sub_value(crate::gc::Gc::new(new_data))
    }

    /// The shared cell behind local `slot` (`name`) of the running frame,
    /// boxing the local in place when it holds a plain value. `None` for a
    /// local whose storage is elsewhere (`state`, `our`), one not yet
    /// initialized, or one a type constraint keeps unboxed.
    fn box_local_for_lexsub_capture(
        &mut self,
        code: &CompiledCode,
        slot: usize,
        name: &str,
    ) -> Option<Value> {
        if code.state_locals.iter().any(|(s, _)| *s == slot)
            || code.our_locals.iter().any(|(s, _)| *s == slot)
        {
            return None;
        }
        let cur = self.locals.get(slot)?.clone();
        if cur.is_container_ref() {
            return Some(cur);
        }
        if cur.is_nil() || self.type_constrained_unboxable(name) {
            return None;
        }
        let boxed = cur.into_container_ref();
        self.locals[slot] = boxed.clone();
        self.env_mut().insert(name.to_string(), boxed.clone());
        Some(boxed)
    }
}
