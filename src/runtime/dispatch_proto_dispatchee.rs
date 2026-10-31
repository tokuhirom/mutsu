//! `Routine::Dispatcher.add_dispatchee`: adding candidates to a `proto` after
//! its declaration (ecosystem `JSON::Fast::Hyper`, `CLI::Version`,
//! `shorten-sub-commands`).
use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// `&proto.add_dispatchee(&code)`: make `other` a candidate of the proto
    /// `package::name`.
    ///
    /// A code VALUE (`ValueView::Sub` — an anonymous `sub (...) { }`, a
    /// closure, or a named routine's `&name`) is held by the proto as that
    /// value (#10929): its registry row carries the value in
    /// [`FunctionDef::dispatchee`], selection ranks the row by its signature
    /// like any declared candidate, and running it calls the value, so the
    /// candidate keeps the lexicals it closed over. A materialized multi
    /// dispatcher contributes each of its captured candidates. A bare
    /// `Routine` handle (no code value behind it) still copies the routine's
    /// registry rows.
    // Cost: O(r + c * a), r = registry size (scan for a `Routine` handle's rows), c = candidates
    // of `other`, a = parameters per candidate.
    pub(super) fn add_routine_dispatchee(
        &mut self,
        package: &str,
        name: &str,
        other: &Value,
    ) -> Result<(), RuntimeError> {
        let proto_key = crate::qualified::qualified(Symbol::intern(package), Symbol::intern(name));
        let proto_key = proto_key.as_str();
        let other = match other.view() {
            ValueView::Mixin(inner, _) if matches!(inner.view(), ValueView::Sub(_)) => {
                inner.as_ref()
            }
            _ => other,
        };
        match other.view() {
            ValueView::Sub(data) => {
                let candidates = data
                    .env
                    .get_sym(crate::symbol::well_known::multi_dispatch_candidates())
                    .cloned()
                    .and_then(Value::into_array);
                match candidates {
                    Some((candidates, _)) => {
                        for candidate in candidates.iter() {
                            if matches!(candidate.view(), ValueView::Sub(_)) {
                                self.add_value_dispatchee(proto_key, name, candidate);
                            }
                        }
                    }
                    None => self.add_value_dispatchee(proto_key, name, other),
                }
                Ok(())
            }
            ValueView::Routine {
                name: oname,
                package: opkg,
                ..
            } => self.add_registered_dispatchee(proto_key, opkg, oname),
            _ => Err(RuntimeError::new(
                "add_dispatchee requires a Routine argument",
            )),
        }
    }

    /// Register the code value `code` as a candidate row of `proto_key`.
    // Cost: O(a + b), a = parameters of `code`, b = its body (one fingerprint hash).
    fn add_value_dispatchee(&mut self, proto_key: &str, proto_name: &str, code: &Value) {
        let Some(mut def) = Self::dispatchee_function_def(code, proto_name) else {
            return;
        };
        // The row extends the family from the unit that adds it. Left unset it
        // would read as the main program's, which a module's private family
        // hides from its own `sub EXPORT` and shows to every importer without
        // the family's other candidates.
        def.source_file = self.executing_source_file_for_module_load();
        let key = Self::dispatchee_row_key(proto_key, &def);
        self.insert_multi_overload(&key, def);
    }

    /// Copy every registry row of the routine `opkg::oname` into `proto_key`.
    // Cost: O(r + c * a), r = registry size, c = rows of the routine, a = parameters per row.
    fn add_registered_dispatchee(
        &mut self,
        proto_key: &str,
        opkg: Symbol,
        oname: Symbol,
    ) -> Result<(), RuntimeError> {
        let single = crate::qualified::qualified(opkg, oname);
        let single = single.as_str();
        let multi_prefix = format!("{single}/");
        let defs: Vec<_> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, _)| {
                let k = k.resolve();
                k == single || k.starts_with(&multi_prefix)
            })
            .map(|(_, d)| d.clone())
            .collect();
        if defs.is_empty() {
            return Err(RuntimeError::new(format!(
                "Cannot add dispatchee: no routine named '{oname}' found"
            )));
        }
        for def in defs {
            let key = Self::dispatchee_row_key(proto_key, &def);
            self.insert_multi_overload(&key, (*def).clone());
        }
        Ok(())
    }

    /// The multi-candidate registry key `def` is filed under in `proto_key`:
    /// `proto/arity` or, with any typed positional, `proto/arity:T1,T2`.
    // Cost: O(a), a = parameters of `def`.
    fn dispatchee_row_key(proto_key: &str, def: &FunctionDef) -> String {
        let positional: Vec<_> = def
            .param_defs
            .iter()
            .filter(|p| {
                !p.named && (!p.slurpy || p.name == "_capture") && !p.is_capture_subsignature()
            })
            .collect();
        let types: Vec<&str> = positional
            .iter()
            .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
            .collect();
        if types.iter().any(|t| *t != "Any") {
            format!("{proto_key}/{}:{}", positional.len(), types.join(","))
        } else {
            format!("{proto_key}/{}", positional.len())
        }
    }

    /// A candidate row for the code value `code` as a member of the family
    /// `name`: its signature, with the value itself as
    /// [`FunctionDef::dispatchee`] so running the row calls the value (and the
    /// lexicals it closed over). `None` for a non-`Sub` value.
    // Cost: O(a + b), a = parameters of `code`, b = its body (one fingerprint hash).
    pub(super) fn dispatchee_function_def(code: &Value, name: &str) -> Option<FunctionDef> {
        let ValueView::Sub(data) = code.view() else {
            return None;
        };
        // Two closures cloned from one literal share params and body; the
        // value's identity keeps them distinct candidates, as in Rakudo, where
        // the multi-registration dedup would otherwise fold them into one.
        let fingerprint = {
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            crate::ast::function_body_fingerprint(&data.params, &data.param_defs, &data.body)
                .hash(&mut hasher);
            data.id.hash(&mut hasher);
            hasher.finish()
        };
        let body_fp_cache = std::sync::OnceLock::new();
        let _ = body_fp_cache.set(fingerprint);
        Some(FunctionDef {
            package: data.package,
            name: Symbol::intern(name),
            params: (*data.params).clone(),
            param_defs: (*data.param_defs).clone(),
            // Never run: the row runs `dispatchee`. Selection reads only the
            // signature, and the fingerprint is seeded above.
            body: Vec::new(),
            is_test_assertion: false,
            is_hidden_from_backtrace: false,
            is_implementation_detail: false,
            is_cached: false,
            is_rw: data.is_rw,
            is_raw: data.is_raw,
            declarator: crate::ast::RoutineDeclarator::Sub,
            empty_sig: false,
            is_stub: false,
            return_type: None,
            is_default: data
                .env
                .get_sym(crate::symbol::wk::multi_is_default())
                .is_some_and(Value::truthy),
            deprecated_message: None,
            op_prec: None,
            source_file: None,
            source_line: None,
            decl_order: crate::runtime::resolution::next_decl_order(),
            compiled: None,
            dispatchee: Some(code.clone()),
            body_fp_cache,
            captured_readonly: None,
            body_facts_cache: std::sync::OnceLock::new(),
            routine_cell: Default::default(),
        })
    }

    /// The narrowest of a multi dispatcher's captured `candidates` for `args`,
    /// for a dispatcher whose family no name reaches from the caller (a
    /// lexical `proto`'s `&name` returned out of its module, #12474).
    ///
    /// The candidates become [`FunctionDef::dispatchee`] rows and go through
    /// the by-name resolver's own ranking, so narrowness, `where` tie-breaks
    /// and `X::Multi::Ambiguous` follow the same rules as a by-name call
    /// rather than declaration order. `Ok(None)` when none applies.
    ///
    /// With the winner comes its deferral chain: the other captured
    /// candidates, narrowest first, which `callsame` / `nextsame` walk,
    /// testing each against the arguments only when they reach it (a block's
    /// `multi` called through its binding after the block exited, ADR-12529
    /// phase 2).
    // Cost: O(c * a + c log c), c = captured candidates, a = parameters per candidate.
    pub(super) fn pick_captured_multi_candidate(
        &mut self,
        family: &str,
        candidates: &[Value],
        args: &[Value],
    ) -> Result<Option<(Value, super::MultiRemaining)>, RuntimeError> {
        // A keyable call is answered from a plan cached per candidate set and
        // argument types (`captured_multi_plan_winner`); the walk below is the
        // miss tail for calls that cannot be keyed.
        if let Some(answer) = self.captured_multi_plan_winner(family, candidates, args) {
            let Some((winner, remaining)) = answer? else {
                return Ok(None);
            };
            let Some(code) = winner.dispatchee.clone() else {
                return Ok(None);
            };
            return Ok(Some((
                code,
                super::MultiRemaining::from_vec(remaining, Some(winner.body_fingerprint())),
            )));
        }
        // One declared `multi` can be captured more than once (it sits under
        // several registry keys), each copy a distinct Sub value: keep the
        // first copy of each declaration, as the by-name gather's dedup does.
        let mut seen = std::collections::HashSet::new();
        let rows: Vec<(String, std::sync::Arc<FunctionDef>)> = candidates
            .iter()
            .filter(|code| match code.view() {
                ValueView::Sub(data) => seen.insert(crate::ast::function_body_fingerprint(
                    &data.params,
                    &data.param_defs,
                    &data.body,
                )),
                _ => false,
            })
            .filter_map(|code| {
                let mut def = Self::dispatchee_function_def(code, family)?;
                // Its `where` clauses and defaults run in its declaring unit.
                if let ValueView::Sub(data) = code.view() {
                    def.source_file = data.source_file.clone();
                }
                Some((family.to_string(), std::sync::Arc::new(def)))
            })
            .collect();
        let mut ordered = rows.clone();
        let Some(winner) = self.choose_best_in_scope_candidate(family, args, rows, None)? else {
            return Ok(None);
        };
        let Some(code) = winner.dispatchee.clone() else {
            return Ok(None);
        };
        self.sort_candidates_by_specificity(&mut ordered);
        // The chain is filtered lazily, as the by-name dispatch's is: the
        // `callsame`/`nextsame` walk tests each candidate against the
        // (possibly new) arguments when it reaches it. Testing them here
        // would run a losing candidate's `where` on every call (#13183).
        let remaining = ordered.into_iter().map(|(_, cand)| cand).collect();
        Ok(Some((
            code,
            super::MultiRemaining::from_vec(remaining, Some(winner.body_fingerprint())),
        )))
    }

    /// Run `candidate`, the winner [`Self::pick_captured_multi_candidate`]
    /// chose from a dispatcher value's captured `family` candidates, as a
    /// dispatch: inside a multi-dispatch frame over `remaining` (so
    /// `callsame`/`nextsame` defer to the next captured candidate) and a
    /// samewith context that re-dispatches through `dispatcher` itself, since
    /// no name reaches the family from here.
    // Cost: O(1) around the call.
    pub(super) fn call_captured_multi_winner(
        &mut self,
        family: &str,
        candidate: Value,
        remaining: super::MultiRemaining,
        args: Vec<Value>,
        dispatcher: Value,
    ) -> Result<Value, RuntimeError> {
        // #13183: the winner's `where` clauses and subset predicates already
        // ran against these exact arguments to make it the winner, so its
        // bind may trust that verdict, as a by-name dispatch does (#8697).
        // Armed only for an unwrapped candidate (a wrapper would bind first
        // and consume the flag for its own signature), and only after the
        // arguments are reified, so no user code runs between arming the
        // flag and the candidate's bind taking it.
        let (rw_params, trust_constraints) = match candidate.view() {
            ValueView::Sub(data) => (
                super::builtins_dispatch_next::rw_scalar_positional_params(&data.param_defs),
                !self.dispatch.wrap_chains.contains_key(&data.id)
                    && data
                        .param_defs
                        .iter()
                        .any(|pd| self.param_runs_constraint_predicate(pd)),
            ),
            _ => (Vec::new(), false),
        };
        if trust_constraints {
            self.reify_map_grep_seq_args(&args)?;
        }
        let dispatch_token = self.next_dispatch_token();
        self.dispatch.multi_dispatch_stack.push((
            family.to_string(),
            remaining,
            args.clone(),
            rw_params,
            dispatch_token,
        ));
        self.push_samewith_callable_context(dispatcher);
        self.pending_skip_constraint_recheck = trust_constraints;
        // A module routine's own `my` statics are its whole scope for those
        // names: a same-named lexical of the calling frame (a class static its
        // method injected) must not answer for them. The named-call entry
        // injects the declaring package's statics the same way.
        let mut shadowed_by_statics: Vec<(String, Option<Value>)> = Vec::new();
        if let ValueView::Sub(data) = candidate.view()
            && !crate::qualified::is_global_package(data.package)
        {
            let package = data.package.resolve();
            if self.class_has_package_lexicals(&package) {
                self.inject_class_body_statics_saving(&package, &mut shadowed_by_statics);
            }
        }
        let result = self.call_sub_value(candidate, args);
        for (name, previous) in shadowed_by_statics.into_iter().rev() {
            match previous {
                Some(v) => self.env_mut().insert(name, v),
                None => self.env_mut().remove(&name),
            };
        }
        // Not consumed (the call took a path with no general bind): never let
        // it reach an unrelated later bind.
        self.pending_skip_constraint_recheck = false;
        self.pop_samewith_context();
        self.dispatch.multi_dispatch_stack.pop();
        result
    }

    /// Whether the innermost samewith context re-dispatches through a multi
    /// dispatcher value of the family `name` (pushed by
    /// `call_captured_multi_winner` around one of its candidates).
    // Cost: O(1), one env probe of the context's dispatcher.
    pub(crate) fn samewith_context_is_family_of(&self, name: Symbol) -> bool {
        self.dispatch
            .samewith_context_stack
            .last()
            .and_then(|ctx| ctx.callable.as_ref())
            .is_some_and(|c| name.with_str(|n| !n.is_empty() && Self::is_family_dispatcher_of(c, n)))
    }
}
