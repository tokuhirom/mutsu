//! A bare multi call whose winner is a function of its argument types goes
//! straight to the winner's positional light bind (#10109, ADR-12529 §2.4).
//!
//! A `multi` name is kept out of the name-keyed light-call caches, since one
//! entry would serve every argument shape (#10107). Each call therefore took
//! the whole generic dispatch: argument normalization, the resolution and
//! compiled-body probes of `dispatch_func_call_inner`, and a dozen memoized
//! "is this a multi / a proto / declared" predicates, about 1.4x a plain sub
//! call with the same signature.
//!
//! `multi_light_cache` keys the whole dispatch the way
//! `func_multi_resolve_cache` keys the winner: (package, name, argument type
//! keys). The generic path fills it when a call of a type-deterministic family
//! took the positional light bind; a later call with the same argument types
//! re-checks only what can differ per call (the light bind's admissions, a
//! wrapper, the mainline-capture block) and then runs exactly what the
//! generic path's light branch runs: the dispatch frame `nextsame`/`callsame`
//! read, the samewith context, and the light bind.

use super::*;

impl Interpreter {
    /// The cached multi call at a `CallFunc` site, or `false` (stack untouched)
    /// when the call is not one: not a multi name, a named or `|` argument, a
    /// junction or proxy argument, a key the cache does not hold, or a winner
    /// the light bind does not admit for these arguments.
    // Cost: O(a + p), a = arguments (the type keys and one hash probe),
    // p = the winner's parameters (the light bind's admissions), plus the call.
    pub(super) fn try_multi_light_call(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<bool, RuntimeError> {
        let name_sym = code.const_sym(name_idx);
        // The empty test first: a program that never took a multi's light
        // path pays one load per call here, not the multi-name probe.
        if (self.caches.multi_light_cache.is_empty()
            && self.caches.multi_plan_light_cache.is_empty())
            || self.in_lvalue_assignment
            || !self.has_multi_candidates_cached_sym(name_sym)
        {
            return Ok(false);
        }
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Ok(false);
        }
        let start = self.stack.len() - arity;
        // The generic path drops the parser's callsite-line marker (always the
        // last argument) before it resolves, and auto-fetches a proxy; the
        // keys must be those of the arguments it would have resolved with.
        let mut cl: Option<i64> = None;
        for (i, v) in self.stack[start..].iter().enumerate() {
            let view = v.view();
            if matches!(view, ValueView::Junction { .. } | ValueView::Proxy { .. }) {
                return Ok(false);
            }
            if let Some(line) = Self::callsite_line_of_view(&view) {
                if i + 1 != arity {
                    return Ok(false);
                }
                cl = Some(line);
            }
        }
        let end = self.stack.len() - usize::from(cl.is_some());
        let args: Vec<Value> = self.stack[start..end].to_vec();
        let Some(keys) = self.multi_arg_type_keys(&args) else {
            return Ok(false);
        };
        let key = (self.current_package_sym(), name_sym, keys);
        let name = Self::const_str(code, name_idx);
        let typed = self
            .caches
            .multi_light_cache
            .get(self.caches.fn_resolve_gen, &key, || {
                self.dispatch.fn_family_gens.of_name(name_sym)
            });
        // A type-deterministic family's winner comes from the key alone; a
        // value-dependent one's from its stage program, which has already
        // checked the winner's `where` clauses (`vm_multi_plan_light_call`).
        let (hit, where_verified) = match typed {
            Some(Some(hit)) => (hit, false),
            Some(None) => return Ok(false),
            None => {
                match self.multi_plan_light_winner(code, name, name_sym, &args, &key, compiled_fns)
                {
                    Some(hit) => {
                        let verified = hit
                            .def
                            .param_defs
                            .iter()
                            .any(|pd| pd.where_constraint.is_some());
                        (hit, verified)
                    }
                    None => return Ok(false),
                }
            }
        };
        let Some(cf) = compiled_fns
            .get(&hit.key)
            .filter(|cf| cf.fingerprint == hit.fingerprint)
        else {
            return Ok(false);
        };
        if !Self::is_positional_light_call_eligible_where(
            cf,
            name,
            Self::positional_light_argc(&args),
            &args,
            Some(code),
            where_verified,
        ) || Self::call_shares_container_into_scalar_param(cf, &args)
            || loan_env!(self, routine_is_test_assertion_by_name(name, &args))
            || self.wrap_sub_id_for_name(name).is_some()
            || self.light_call_blocked_by_mainline_capture(name)
        {
            return Ok(false);
        }
        self.stack.truncate(start);
        loan_env!(self, set_pending_callsite_line(cl));
        let pushed_dispatch = loan_env!(
            self,
            push_multi_dispatch_frame_with_winner_sym(name, name_sym, &args, Some(&hit.def))
        );
        self.push_samewith_context(name, None, None);
        let result = self.call_compiled_function_positional_light(
            cf,
            &args,
            compiled_fns,
            name,
            name_sym,
            Some(code),
        );
        self.pop_samewith_context();
        if pushed_dispatch {
            self.pop_multi_dispatch();
        }
        let result = match result {
            Ok(v) => loan_env!(self, maybe_fetch_rw_proxy(v, !cf.returns_container()))?,
            Err(e) => {
                // The callee's UNDO/LEAVE writes reach the caller's slots on
                // an exceptional exit too, as on the generic path.
                self.apply_pending_rw_writeback(code);
                return Err(e);
            }
        };
        self.stack.push(result);
        self.drain_and_reconcile_after_cached_call(code);
        Ok(true)
    }

    /// Record a multi call the generic path is about to run through the
    /// positional light bind, for [`Self::try_multi_light_call`]: only for a
    /// family whose winner is a pure function of the argument type keys
    /// (`func_multi_dispatch_type_cacheable`), resolved as such, with a
    /// compiled body in `compiled_fns`.
    // Cost: O(a) for a key already recorded, a = arguments; else O(a + f),
    // f = `compiled_fns` entries (finding the body's key), once per key.
    pub(super) fn note_multi_light_call(
        &mut self,
        name: &str,
        name_sym: Symbol,
        args: &[Value],
        winner: Option<&Arc<crate::ast::FunctionDef>>,
        cf: &CompiledFunction,
        compiled_fns: &CompiledFns,
    ) {
        let Some(def) = winner else {
            return;
        };
        let pkg_sym = self.current_package_sym();
        if self.operator_has_import_scope_sym(name_sym) {
            return;
        }
        if !self.func_multi_dispatch_type_cacheable(pkg_sym, name_sym, name) {
            self.note_multi_plan_light_call(name, name_sym, args, def, cf, compiled_fns);
            return;
        }
        let Some(keys) = self.multi_arg_type_keys(args) else {
            return;
        };
        let generation = self.caches.fn_resolve_gen;
        let key = (pkg_sym, name_sym, keys);
        if self
            .caches
            .multi_light_cache
            .get(generation, &key, || {
                self.dispatch.fn_family_gens.of_name(name_sym)
            })
            .is_some()
        {
            return;
        }
        let entry = compiled_fns
            .iter()
            .find(|(_, f)| std::ptr::eq(Arc::as_ptr(f), cf))
            .map(
                |(k, _)| crate::runtime::resolution_caches::MultiLightWinner {
                    def: Arc::clone(def),
                    key: *k,
                    fingerprint: cf.fingerprint,
                },
            );
        self.caches.multi_light_cache.insert(
            generation,
            self.caches.fn_resolve_gen,
            || self.dispatch.fn_family_gens.of_name(name_sym),
            key,
            entry,
        );
    }

    /// Whether the closure's captured `&name` binding (upvalue `index`) is a
    /// multi dispatcher of the family `name` whose candidate set is the one a
    /// by-name call of `name` resolves right now: built under the current
    /// registry generation (the declaring block is still live, or its exact
    /// map is back) for the running package. Such a `&name(...)` call
    /// (ADR-12529 phase 2, slice 6) then takes the `CallFunc` path, whose
    /// call-site caches the dispatcher value's own call path lacks.
    // Cost: O(1): one upvalue read and two env probes of the dispatcher.
    pub(super) fn family_dispatcher_is_current(
        &self,
        code: &CompiledCode,
        name_idx: u32,
        index: u32,
    ) -> bool {
        let Some(Some(value)) = self.upvalues.get(index as usize) else {
            return false;
        };
        let value = value.clone().into_deref();
        let ValueView::Sub(data) = value.view() else {
            return false;
        };
        data.package == self.current_package_sym()
            && matches!(
                data.env.get("__mutsu_multi_dispatch_gen").map(Value::view),
                Some(ValueView::Int(g)) if g == self.caches.fn_resolve_gen as i64
            )
            && Self::is_family_dispatcher_of(&value, Self::const_str(code, name_idx))
    }
}
