use super::*;

impl Interpreter {
    /// Collect ALL multi dispatch candidates for a function name, regardless of
    /// arity or type matching.  Used to build the full candidate list for
    /// callwith(), which may re-dispatch with different arguments.
    pub(crate) fn resolve_all_multi_candidates(&self, name: &str) -> Vec<Arc<FunctionDef>> {
        let mut all: Vec<(String, Arc<FunctionDef>)> = Vec::new();
        let prefixes: Vec<String> = self
            .bare_name_packages()
            .iter()
            .map(|pkg| format!("{}::{}/", pkg, name))
            .collect();
        let mut seen_fps = Vec::new();
        for prefix in &prefixes {
            let candidates: Vec<(String, Arc<FunctionDef>)> = self
                .registry()
                .functions
                .iter()
                .filter(|(k, _)| k.resolve().starts_with(prefix.as_str()))
                .map(|(k, def)| (k.resolve(), def.clone()))
                .collect();
            for (key, def) in candidates {
                let fp = def.body_fingerprint();
                if !seen_fps.contains(&fp) {
                    seen_fps.push(fp);
                    all.push((key, def));
                }
            }
        }
        // Sort by dispatch specificity (most specific first). The callsame /
        // nextsame consumers (`builtins_dispatch_next.rs`) pick the FIRST
        // arg-matching candidate in this list's order, so it must reflect the
        // real dispatch order rather than arbitrary HashMap iteration order.
        // Without this, when several candidates match the same args — e.g.
        // overlapping `where` constraints plus a generic fallback
        // (`multi foo(Int $ where * > 0)`, `multi foo(Int $ where * < 10)`,
        // `multi foo($)`) — a broader candidate appearing earlier in HashMap
        // order is wrongly chosen before a narrower one, dropping the narrower
        // candidate from the nextsame chain (hash-seed-dependent flake in
        // S12-methods/defer-next.t `nextsame + multi + where`). Mirrors the
        // deterministic winner resolution PR-4 added to `push_multi_dispatch_frame`.
        self.sort_candidates_by_specificity(&mut all);
        all.into_iter().map(|(_, def)| def).collect()
    }

    pub(crate) fn has_proto(&self, name: &str) -> bool {
        self.bare_name_packages()
            .iter()
            .any(|pkg| self.registry().has_proto(pkg, name))
    }

    /// Check if any multi candidates exist for this function name (any arity).
    pub(crate) fn has_multi_candidates(&self, name: &str) -> bool {
        self.registry()
            .has_multi_candidates(&self.bare_name_packages(), name)
    }

    pub(super) fn resolve_proto_function_with_alias(
        &self,
        name: &str,
    ) -> Option<(String, FunctionDef)> {
        if let Some(def) = self.resolve_proto_function(name) {
            return Some((name.to_string(), def));
        }
        if name.contains(':') || name.contains("::") {
            return None;
        }
        for alias in [format!("prefix:<{name}>"), format!("postfix:<{name}>")] {
            if let Some(def) = self.resolve_proto_function(&alias) {
                return Some((alias, def));
            }
        }
        None
    }

    pub(crate) fn resolve_proto_function(&self, name: &str) -> Option<FunctionDef> {
        if name.contains("::") {
            // A `my`-scoped (non-`our`) proto/multi in a module is exported
            // and callable under its short name, but is never in the package
            // stash — a package-qualified call to it must stay unresolved,
            // exactly like the plain-sub gate in `dispatch_resolve.rs`
            // (`qualified_name_hidden_here`). This lookup previously skipped
            // that gate entirely, so `Pkg::protoName(...)` wrongly succeeded
            // from outside the module even without `our`.
            if self.qualified_name_hidden_here(name) {
                return None;
            }
            return self
                .registry()
                .proto_functions
                .get(&Symbol::intern(name))
                .map(|def| (**def).clone());
        }
        for pkg in self.bare_name_packages() {
            if let Some(def) = self
                .registry()
                .proto_functions
                .get(&Symbol::intern(&format!("{}::{}", pkg, name)))
            {
                return Some((**def).clone());
            }
        }
        None
    }

    pub(super) fn call_proto_function(
        &mut self,
        proto_name: &str,
        def: &FunctionDef,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if def.empty_sig && !args.is_empty() {
            return Err(Self::reject_args_for_empty_sig(args));
        }
        let saved_env = self.env.clone();
        let saved_readonly = self.enter_readonly_frame();
        let rw_bindings = match self.bind_function_args_values(&def.param_defs, &def.params, args) {
            Ok(bindings) => bindings,
            Err(e) => {
                self.env = saved_env;
                self.exit_readonly_frame(saved_readonly);
                // Convert binding type-check errors to X::TypeCheck::Argument for proto calls
                let is_binding_param = e.exception.as_ref().is_some_and(|ex| {
                    if let ValueView::Instance { class_name, .. } = ex.as_ref().view() {
                        class_name.resolve() == "X::TypeCheck::Binding::Parameter"
                    } else {
                        false
                    }
                }) || e
                    .message
                    .contains("X::TypeCheck::Binding::Parameter: Type check failed");
                if is_binding_param {
                    let type_names: Vec<String> = args
                        .iter()
                        .filter(|a| !a.is_string_pair_value())
                        .map(|a| super::value_type_name(a).to_string())
                        .collect();
                    let mut err = RuntimeError::new(format!(
                        "X::TypeCheck::Argument: Calling {}({}) will never work with proto signature ({})",
                        proto_name,
                        type_names.join(", "),
                        def.param_defs
                            .iter()
                            .map(|p| {
                                if let Some(tc) = &p.type_constraint {
                                    format!("{} {}", tc, p.name)
                                } else {
                                    p.name.clone()
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                    let signature = format!(
                        "({})",
                        def.param_defs
                            .iter()
                            .map(|p| {
                                // Type-only params (`Str`) carry a synthetic name;
                                // render just the type constraint.
                                if p.name.starts_with("__") {
                                    return p
                                        .type_constraint
                                        .as_deref()
                                        .unwrap_or("Any")
                                        .to_string();
                                }
                                if let Some(tc) = &p.type_constraint {
                                    format!("{} {}", tc, p.name)
                                } else {
                                    p.name.clone()
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(err.message.clone()));
                    attrs.insert("objname".to_string(), Value::str(proto_name.to_string()));
                    attrs.insert("signature".to_string(), Value::str(signature));
                    attrs.insert(
                        "arguments".to_string(),
                        Value::array(type_names.iter().cloned().map(Value::str).collect()),
                    );
                    // This dispatch failure is against a `proto` signature.
                    attrs.insert("protoguilt".to_string(), Value::TRUE);
                    err.exception = Some(Box::new(Value::make_instance(
                        Symbol::intern("X::TypeCheck::Argument"),
                        attrs,
                    )));
                    return Err(err);
                }
                return Err(e);
            }
        };
        self.routine_stack.push(RoutineFrame {
            package: def.package,
            lexical_package: None,
            name: def.name,
            line: None,
            file: None,
            is_method: false,
            is_submethod: false,
            is_block: false,
            def_file: None,
            invocation_id: crate::runtime::next_invocation_id(),
        });
        self.proto_dispatch_stack
            .push((proto_name.to_string(), args.to_vec(), None));
        let result = if def.body.is_empty() {
            // Bodyless proto behaves as implicit {*} dispatch.
            self.call_proto_dispatch()
        } else {
            let rewritten = Self::rewrite_proto_dispatch_stmts(&def.body);
            self.eval_block_value(&rewritten)
        };
        self.proto_dispatch_stack.pop();
        self.routine_stack.pop();
        let mut restored_env = saved_env.clone();
        self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        self.restore_env_preserving_existing(&restored_env, &def.params);
        self.exit_readonly_frame(saved_readonly);
        match result {
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            other => other,
        }
    }

    /// Look up a `proto method` body for `method_name` in `class_name`'s MRO.
    /// Returns the nearest (owner_class, proto `FunctionDef`). Cheap no-op when
    /// no proto methods are declared anywhere (ADR-0019 E8c: reads
    /// `MethodEntry.proto` via `Registry::method_entry_proto`, one probe per
    /// MRO level, guarded by the whole-program `Registry::has_proto_methods`
    /// flag).
    pub(crate) fn lookup_proto_method(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> Option<(String, FunctionDef)> {
        if !self.registry().has_proto_methods {
            return None;
        }
        let mro = self.class_mro(class_name);
        mro.iter().map(|s| s.resolve()).find_map(|cn| {
            self.registry()
                .method_entry_proto(&cn, method_name)
                .map(|f| (cn, f))
        })
    }

    /// Run a `proto method` body, with `{*}` dispatching to the matching multi
    /// candidate on `invocant`. Reuses the normal resolved-method runner so the
    /// proto body gets `self`, attributes, and parameter binding like any method.
    pub(crate) fn run_proto_method(
        &mut self,
        invocant: Value,
        receiver_class: &str,
        owner_class: &str,
        method_name: &str,
        args: Vec<Value>,
        proto: FunctionDef,
    ) -> Result<Value, RuntimeError> {
        let rewritten = Self::rewrite_proto_dispatch_stmts(&proto.body);
        let mut method_def = MethodDef {
            lexical_package: proto.package.resolve(),
            params: proto.params.clone(),
            param_defs: proto.param_defs.clone(),
            body: std::sync::Arc::new(rewritten),
            is_rw: false,
            is_private: false,
            is_multi: false,
            is_my: false,
            role_origin: None,
            original_role: None,
            return_type: None,
            compiled_code: None,
            compiled_fns: None,
            delegation: None,
            is_default: false,
            deprecated_message: None,
            is_submethod: false,
            captured_env: None,
            source_file: proto.source_file.clone(),
            role_param_bindings: None,
        };
        // ADR-0019 D3-8 never compiled proto method bodies at plan-lowering
        // time (`todo/tickets/adr0019-method-body-compile-dedup-remnants.md`
        // item 2): without this cache, `method_def` above always started with
        // `compiled_code: None`, forcing `run_resolved_method_compiled_or_treewalk`'s
        // on-demand-compile path to recompile the SAME proto body from AST on
        // every single call rather than once. Reuse a prior compile of this
        // exact `(owner_class, method_name)` proto if one exists; otherwise
        // compile once here and cache it for every later call.
        if let Some((cc, fns)) = self.registry().get_proto_compiled(owner_class, method_name) {
            method_def.compiled_code = Some(cc);
            method_def.compiled_fns = fns;
        } else {
            let dist = self.resolve_package_distribution(owner_class);
            Self::compile_method_def_in_place_with_dist(&mut method_def, owner_class, dist);
            if let Some(cc) = method_def.compiled_code.clone() {
                self.registry_mut().set_proto_compiled(
                    owner_class,
                    method_name,
                    (cc, method_def.compiled_fns.clone()),
                );
            }
        }
        let attributes = match invocant.view() {
            ValueView::Instance { attributes, .. } => attributes.as_map().clone(),
            _ => crate::value::AttrMap::new(),
        };
        let call_arg_sources = self.pending_call_arg_sources().cloned();
        self.proto_dispatch_stack.push((
            method_name.to_string(),
            args.clone(),
            Some(ProtoMethodCtx {
                invocant: invocant.clone(),
                call_arg_sources,
            }),
        ));
        let result = self.run_resolved_method_compiled_or_treewalk(
            receiver_class,
            owner_class,
            method_name,
            method_def,
            attributes,
            args,
            Some(invocant),
        );
        self.proto_dispatch_stack.pop();
        result.map(|(v, _)| v)
    }

    /// Proto-method-body interception shared by all method-call op handlers and
    /// `call_method_with_values`. If `target`'s class (or an ancestor) declares a
    /// `proto method`/`proto submethod` body for `method`, run that body (its
    /// `{*}` dispatches to the matching multi candidate) and return `Some(result)`.
    /// Returns `None` when the caller should dispatch normally.
    ///
    /// ADR-0019 E9c-2: `{*}`'s redispatch no longer re-enters this interception
    /// by name (`call_proto_dispatch`'s method branch resolves the winning
    /// candidate directly via `resolve_method_within_boundary` and invokes it
    /// through the ordinary resolved-method run leg), so the former one-shot
    /// `proto_method_skip` guard that used to suppress re-interception here has
    /// no re-entry left to guard against and was deleted.
    ///
    /// TODO: methods reached via `handles` delegation forwarders run through
    /// `assign_method_lvalue_with_values` rather than the method-call op handlers
    /// or `call_method_with_values`, so a delegated proto method's body is not yet
    /// intercepted here. Cover that path when delegation dispatch is unified.
    pub(crate) fn try_proto_method_body(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn = match target.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            // A type-object invocant (`P.m(5)`, not `P.new.m(5)`): a
            // non-trivial `proto method` body must still run (raku:
            // `proto(5) / int(5)`), same as for an instance invocant. `new`
            // is excluded — it already has its own proto interception in
            // `dispatch_new`/`methods_object_dispatch_new.rs`, reached
            // before this generic method-call path, so widening the gate to
            // it here would risk double-running that constructor's proto
            // body instead of leaving it to the dedicated leg.
            ValueView::Package(name) if method != "new" => name.resolve(),
            _ => return None,
        };
        let (owner, proto) = self.lookup_proto_method(&cn, method)?;
        // env_dirty substrate (docs/captured-outer-cell-sharing.md §10): a multi
        // candidate dispatched through the proto body's `{*}` runs via the slow
        // `run_resolved_instance_method` path, which merges captured-outer caller
        // scalar writes (e.g. `multi method l(%t,*@l){ $r ~= '%'; ... }` mutating
        // a caller lexical `$r`) back into env but does NOT set `env_dirty` or
        // record a precise writeback. So the owning caller slot was refreshed only
        // by the call site's blanket pull — and the blanket pull does not fire
        // reliably here (the slow path does not flag `env_dirty`), so the caller
        // slot stayed stale in *default* builds too, not just under double-OFF.
        // Snapshot the writeback-safe env scalars before the proto dispatch and
        // record the names it changes into the retain-on-miss caller-var
        // writeback, drained at the proto call site (`apply_pending_rw_writeback`).
        // This is ungated: it is a precise subset of the blanket reconcile (so it
        // never changes a correct result) and it fixes the latent default-build
        // bug rather than merely the double-OFF measurement surface.
        let proto_pre_env: std::collections::HashMap<Symbol, Value> = self
            .env
            .iter()
            .filter(|(_, v)| Self::is_writeback_safe_scalar(v))
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        // The nested proto dispatch rebinds `env["self"]` to its own invocant and
        // does not restore the enclosing method's `self` afterward (unlike the
        // normal method-call path). When a plain method delegates to an attribute
        // whose class declares a cross-module `proto method` (HTTP::Message.field
        // -> HTTP::Header.field), the enclosing method's `self` is left holding the
        // proto's invocant / a stale type object, and the method-return writeback
        // then commits that back to the caller's variable — turning it into `Any`.
        // Save and restore the enclosing invocant across the proto dispatch.
        let saved_self = self.env.get("self").cloned();
        let result =
            self.run_proto_method(target.clone(), &cn, &owner, method, args.to_vec(), proto);
        match saved_self {
            Some(v) => {
                self.env.insert("self".to_string(), v);
            }
            None => {
                self.env.remove("self");
            }
        }
        let changed: Vec<String> = self
            .env
            .iter()
            .filter(|(k, v)| {
                let kn = k.resolve();
                // `self`/`$_` are the callee's invocant and topic, never a caller
                // lexical to write back. The nested proto dispatch can leave the
                // enclosing method's `self` slot holding a type object (a
                // writeback-safe `Package`); recording it here would clobber the
                // enclosing invocant on return — a plain method that merely
                // delegates to an attribute's cross-module `proto method` would
                // turn its own `self` into `Any` (HTTP::Message.field ->
                // HTTP::Header.field). Exclude the invocant explicitly.
                kn != "_"
                    && kn != "$_"
                    && kn != "self"
                    && !kn.starts_with("__mutsu_")
                    && Self::is_writeback_safe_scalar(v)
                    // Only a name that ALREADY existed as a writeback-safe scalar
                    // before the proto dispatch and whose value the dispatch
                    // changed is a genuine caller-lexical mutation to propagate.
                    // A name that is *new* to env (or was a non-scalar before) is
                    // internal leakage from the proto run — bareword type objects
                    // (`Any`/`POuter`), magic vars (`?FILE`), `__mutsu_*` keys, and
                    // even the caller's own instance var reset to a type object.
                    // Recording those clobbers the caller (`unwrap_or(true)` turned
                    // `$o` into `Any`). Require a real pre-existing scalar.
                    && proto_pre_env.get(*k).map(|p| p != *v).unwrap_or(false)
            })
            .map(|(k, _)| k.resolve())
            .collect();
        for n in changed {
            self.record_caller_var_writeback(&n);
        }
        Some(result)
    }
}
