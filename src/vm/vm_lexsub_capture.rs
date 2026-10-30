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
            if let Some(plan) = code.sub_decl_plans.iter().find(|p| p.name == name) {
                let val = match self.activation_routine_value(plan) {
                    Some(val) => val,
                    None => self.resolve_code_var_unshadowed(name.as_str()),
                };
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

    /// The code object of the plain `sub` that `plan` registered in the
    /// current package just now, read straight from the registry key the
    /// registration installed, without the generic `&name` resolver's
    /// pseudo-package, alias, multi and proto probes (#13049). `None` when the
    /// routine is not a plain registry entry of this package (a `multi`, a
    /// frame-lexical `my sub`, a qualified or computed name, or one nothing
    /// installed); the caller then resolves by name.
    // Cost: O(d), d = the size of the routine's signature (the code object copies it).
    fn activation_routine_value(&self, plan: &crate::opcode::CompiledSubDeclPlan) -> Option<Value> {
        if plan.multi
            || plan.frame_lexical.is_some()
            || plan.name_chunk.is_some()
            || plan.name.with_str(|n| n.contains(':'))
        {
            return None;
        }
        let key = crate::qualified::qualified(self.current_package_sym(), plan.name);
        let def = self.registry().functions.get(&key)?.clone();
        Some(self.sub_value_from_function_def((*def).clone()))
    }

    /// `val`, the code object the declaring `code` just resolved for its own
    /// nested sub `name`, as a capture of the running activation: each free
    /// variable of the sub that is a local of `code` is boxed into a shared
    /// cell in place and bound in the capture. `val` unchanged when the sub
    /// has none (it reads nothing of this activation).
    // Cost: O(f * l), f = the sub's free variables, l = `code`'s locals; only
    // for a `&name` read of a sub the running code declares.
    pub(super) fn capture_lexsub_activation(&mut self, code: &CompiledCode, val: Value) -> Value {
        let mut building: Vec<(Symbol, Option<Value>)> = Vec::new();
        self.capture_lexsub_activation_in(code, val, &mut building)
    }

    /// [`Self::capture_lexsub_activation`], also binding the sibling routines
    /// of the same declaring code that the sub's body calls by bare name
    /// (#13037): `sub a1 { b1() }; sub b1 { ... }` in a loop body, with `a1`
    /// called from a closure after the iteration ended. `a1`'s body is a named
    /// routine body, so its `b1()` is a by-name call, and the registry holds
    /// only the latest activation's `b1` (none once the block exited). The
    /// capture binds `&b1`, and a by-name call in a frame linked to the
    /// capture calls that binding (`Env::frame_activation_binds_routine`).
    /// Each sibling's binding is its own
    /// capture of this activation. `building` holds the routines whose capture
    /// is under construction: a sibling cycle (`ev` and `od` calling each
    /// other) binds its back edge to a cell that receives the routine's
    /// capture once it is built.
    // Cost: O(s * (k + f * l)) per routine captured, s = `code`'s routine
    // declarations, k = the sub's constants; each sibling is captured once
    // per enclosing capture chain.
    fn capture_lexsub_activation_in(
        &mut self,
        code: &CompiledCode,
        val: Value,
        building: &mut Vec<(Symbol, Option<Value>)>,
    ) -> Value {
        let mut data = match val.view() {
            ValueView::Sub(data) => data.clone(),
            _ => return val,
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
        let mut siblings: Vec<(Symbol, Value)> = Vec::new();
        building.push((data.name, None));
        for plan in &code.sub_decl_plans {
            let name = plan.name;
            if name == data.name
                || siblings.iter().any(|(s, _)| *s == name)
                || !Self::code_names_routine(&cf.code, name)
            {
                continue;
            }
            // A back edge of a sibling cycle: bind the cell the routine's
            // capture is stored into once built.
            if let Some((_, cell)) = building.iter_mut().find(|(s, _)| *s == name) {
                let cell = cell.get_or_insert_with(|| Value::NIL.into_container_ref());
                siblings.push((name, cell.clone()));
                continue;
            }
            let sibling = self.resolve_code_var_unshadowed(name.as_str());
            if !matches!(sibling.view(), ValueView::Sub(_)) {
                continue;
            }
            let sibling = self.capture_lexsub_activation_in(code, sibling, building);
            siblings.push((name, sibling));
        }
        let back_edge = building.pop().and_then(|(_, cell)| cell);
        if cells.is_empty() && siblings.is_empty() {
            if let Some(cell) = back_edge {
                cell.assign_into_container(val.clone());
            }
            return val;
        }
        // The code object is almost always one this activation just built, so
        // its data is rewritten in place; a shared one is copied first
        // (`make_mut`).
        drop(val);
        let new_data = data.make_mut();
        for (sym, cell) in cells {
            new_data.env.insert_sym(sym, cell);
        }
        for (name, sibling) in siblings {
            new_data.env.insert(format!("&{name}"), sibling);
        }
        new_data.env.mark_activation_capture();
        // The routine closes over the declaring method's `self` and `?CLASS`
        // (ADR-14843).
        if new_data.fixtures.is_none() {
            new_data.fixtures = self.capture_fixtures();
        }
        let captured = Value::sub_value(data);
        if let Some(cell) = back_edge {
            cell.assign_into_container(captured.clone());
        }
        captured
    }

    /// Whether `code` (a routine body) names the routine `name` as a call
    /// target: its constant pool, or that of a closure nested in it, holds the
    /// name every by-name call op reads.
    // Cost: O(k), k = the constants of `code` and of its nested closures.
    fn code_names_routine(code: &CompiledCode, name: Symbol) -> bool {
        name.with_str(|n| {
            code.constants
                .iter()
                .any(|c| matches!(c.view(), ValueView::Str(s) if s.as_str() == n))
        }) || code
            .closure_compiled_codes
            .iter()
            .any(|cc| Self::code_names_routine(cc, name))
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

    /// The activation capture and compiled routine of `target` when it is a
    /// block-declared routine's code object (`capture_lexsub_activation`)
    /// that a `CallOnCodeVar` may run on the named-call path
    /// (`call_compiled_function_named_in`) instead of the closure entry: a
    /// plain compiled routine, not wrapped, not a multi dispatcher or an
    /// `.assuming` carrier, and not called for its container (#13049).
    // Cost: O(1), a few field reads and one hashed probe of the wrap table.
    pub(super) fn activation_routine_of(
        &self,
        target: &Value,
    ) -> Option<(
        crate::gc::Gc<crate::value::SubData>,
        Arc<crate::opcode::CompiledFunction>,
    )> {
        let ValueView::Sub(data) = target.view() else {
            return None;
        };
        let cf = data.compiled_routine.as_ref()?;
        if !data.env.is_activation_capture()
            || self.in_lvalue_assignment
            || data.is_decl_expr_thunk
            || !data.assumed_positional.is_empty()
            || !data.assumed_named.is_empty()
            || data
                .compiled_code
                .as_ref()
                .is_some_and(|cc| !Arc::ptr_eq(cc, &cf.code))
            || data
                .env
                .contains_key_sym(crate::symbol::well_known::multi_dispatch_candidates())
            || self.has_wrap_chain(data.id)
            || Self::sub_multi_method_dispatcher_name(&data).is_some()
        {
            return None;
        }
        Some((data.clone(), Arc::clone(cf)))
    }
}
