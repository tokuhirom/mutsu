use super::*;

impl Interpreter {
    /// The value of a routine body whose last statement is a `sub`
    /// declaration: Raku makes `sub pw { sub tw($x) { $x } }` return the
    /// `tw` Sub. The body's bytecode ends with that `RegisterDecl(Sub)`, which
    /// pushes nothing, so the frame's natural value is Nil (or `Any` when a
    /// `--> T` return check coerced it); this builds the Sub value instead.
    ///
    /// Shared by the named call path and the closure dispatch path (a named
    /// routine called through its code object, `&pw()`), so both produce the
    /// same value for one routine (#13040). Must run while the callee's frame
    /// (its locals, env and package) is still current.
    ///
    /// Returns `None` when the body did not end with a sub declaration or
    /// already produced a value.
    // Cost: O(f + e), f = free variables of the declared sub, e = entries in the frame's env (flattened into the Sub's capture).
    pub(super) fn trailing_sub_decl_value(
        &mut self,
        code: &CompiledCode,
        ret_val: &Value,
        compiled_fns: &CompiledFns,
    ) -> Option<Value> {
        if !(ret_val.is_nil()
            || matches!(ret_val.view(), ValueView::Package(n) if n.resolve() == "Any"))
        {
            return None;
        }
        let Some(crate::opcode::OpCode::RegisterDecl(idx)) = code.ops.last() else {
            return None;
        };
        let Some(crate::opcode::CompiledDeclPlanRef::Sub(plan_idx)) =
            code.decl_plans.get(*idx as usize)
        else {
            return None;
        };
        let plan = code.sub_decl_plans.get(*plan_idx as usize)?;
        let plan_env = |interp: &Self| {
            let mut env = interp.clone_env();
            Self::own_return_type(&mut env, plan.return_type.as_deref());
            env
        };
        // Build the Sub from the routine the RegisterDecl just installed —
        // it carries the plan's bytecode (`FunctionDef::compiled`), so the
        // returned code object dispatches compiled instead of re-compiling
        // the plan's AST body (ADR-0019 C6e-3, the C6c treatment). The
        // registry key uses the plan's static name; a computed-name
        // declaration (`sub ::($name)`) or a just-out-of-scope def falls
        // back to the plan's own compiled routine (below), and only to
        // its AST body if that too is unresolved.
        let single_key = crate::qualified::qualified(self.current_package_sym(), plan.name);
        let registered = self.registry().functions.get(&single_key).cloned();
        Some(if let Some(def) = registered {
            // Flatten: a Sub returned as a value is dispatched cross-scope.
            let mut captured = self.clone_env();
            Self::own_return_type(&mut captured, def.return_type.as_deref());
            let mut owned: Vec<Symbol> = Vec::new();
            // ...but the flattened env only has what `env` holds. A `my` in
            // THIS routine's body lives in a local slot, so the returned Sub
            // would resolve the name against its caller (or, under
            // shadowing, against an enclosing scope's same-named binding)
            // instead of its declaration scope. Overwrite with the live slot
            // value, exactly as `capture_closure_env` does for an anonymous
            // closure.
            if let Some(callee) = def.compiled_fn() {
                owned = self.inject_frame_locals_for_free_vars(code, &callee.code, &mut captured);
            }
            Value::make_sub_for_routine_owning(
                def.package,
                def.name,
                def.params.clone(),
                def.param_defs.clone(),
                def.body.clone(),
                def.is_rw,
                captured,
                def.compiled_fn().cloned(),
                owned,
            )
        } else {
            // The plan's own compiled routine (same key this call's
            // RegisterSub just tried to install under) still counts as
            // "compiled" even when the registry lookup above missed —
            // build the Sub from it instead of the AST body, mirroring
            // the C6c/C6e-3a treatment applied everywhere else a Sub
            // value is constructed from a plan (ADR-0019 C6e-3c).
            let plan_fully_compiled =
                plan.compiled_routine_keys.len() == 1 + plan.signature_alternates.len();
            let primary_compiled = plan_fully_compiled
                .then(|| compiled_fns.get(&plan.compiled_routine_keys[0]))
                .flatten();
            match primary_compiled {
                Some(compiled) => Value::make_sub_for_routine(
                    Symbol::intern(&self.current_package()),
                    plan.name,
                    plan.params.clone(),
                    plan.param_defs.clone(),
                    Vec::new(),
                    plan.is_rw,
                    plan_env(self),
                    Some(std::sync::Arc::clone(compiled)),
                ),
                // Neither the registry lookup above nor the plan's own
                // compiled routine resolved: build a body-less Sub (the
                // plan no longer carries an AST body, ADR-0019 C6e-3c).
                // Unreachable through ordinary syntax (see the historical
                // zero-hit measurement in
                // `todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`).
                None => Value::make_sub(
                    Symbol::intern(&self.current_package()),
                    plan.name,
                    plan.params.clone(),
                    plan.param_defs.clone(),
                    Vec::new(),
                    plan.is_rw,
                    plan_env(self),
                ),
            }
        })
    }

    /// A routine's return type is its own, never the declaring frame's: the
    /// flattened env carries the *enclosing* routine's `__mutsu_return_type`
    /// (`sub f(--> Code:D) { sub g($x) { $x } }` called through `&f` checked
    /// g's Int result against `Code:D`). Same rule as closure creation
    /// (`vm_closure_build`) and the `EVAL` trailing declaration.
    // Cost: O(1).
    fn own_return_type(env: &mut Env, return_type: Option<&str>) {
        env.remove_sym(crate::symbol::well_known::return_type());
        if let Some(rt) = return_type {
            env.insert_sym(
                crate::symbol::well_known::return_type(),
                Value::str_from(rt),
            );
        }
    }
}
