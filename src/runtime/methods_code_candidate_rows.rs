//! The bodies of `Routine`'s rows `add_dispatchee`, `candidates`,
//! `dispatchees` and `cando` (ADR-11276 §9.166): one per code-object kind, a
//! `&name` handle and a `Sub`.

use super::*;

impl Interpreter {
    /// `add_dispatchee`, `candidates` and `dispatchees` of a `&name` handle.
    // Cost: O(c), c = candidates.
    pub(crate) fn routine_handle_dispatchees(
        &mut self,
        target: &Value,
        name: &str,
        package: &str,
        method: &str,
        args: Vec<Value>,
        is_regex: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, name, package, &args, is_regex);
        if method == "add_dispatchee" && args.len() == 1 {
            return Some(
                self.add_routine_dispatchee(package, name, &args[0])
                    .map(|()| target.clone()),
            );
        }
        if matches!(method, "candidates" | "dispatchees") && args.is_empty() {
            let declared = self.routine_candidate_subs(package, name);
            if declared.is_empty() {
                let builtin = self.builtin_routine_candidates(package, name);
                if !builtin.is_empty() {
                    return Some(Ok(Value::array(builtin)));
                }
            }
            return Some(Ok(Value::array(self.with_joined_setting_candidates(
                package, name, declared,
            ))));
        }
        None
    }

    /// `add_dispatchee`, `candidates` and `dispatchees` of a `Sub`.
    // Cost: O(c), c = candidates.
    pub(crate) fn sub_dispatchees(
        &mut self,
        target: &Value,
        data: &crate::value::SubData,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, data, &args);
        if method == "add_dispatchee" && args.len() == 1 {
            let (package, name) = (data.package.resolve(), data.name.resolve());
            Self::grow_captured_dispatcher(data, &args[0]);
            return Some(
                self.add_routine_dispatchee(&package, &name, &args[0])
                    .map(|()| target.clone()),
            );
        }
        if matches!(method, "candidates" | "dispatchees") && args.is_empty() {
            // Multi-dispatch dispatcher: try name-based lookup first (preserves
            // doc comments) -- unless the dispatcher captured candidates the
            // name no longer reaches. A `multi` declared inside `sub EXPORT`
            // (upstream NativeCall's `:$native!` one) is lexical to that call:
            // by the time a later importer asks, the registry has only the
            // family's other candidates, and `&trait_mod:<is>.candidates` would
            // be missing the one the dispatcher was built around.
            if let Some(ValueView::Str(disp_name)) =
                data.env.get("__mutsu_multi_dispatch_name").map(Value::view)
            {
                let name_based =
                    self.routine_candidate_subs(&data.package.resolve(), disp_name.as_str());
                // The family is the union of the name's candidates and the
                // captured ones the name does not reach (a hook's dispatcher
                // imported next to another module's candidates of the same
                // name: `use Test; use Hook` hands the program both).
                if !name_based.is_empty() {
                    let fingerprint = |cand: &Value| match cand.view() {
                        ValueView::Sub(d) => Some(crate::ast::function_body_fingerprint(
                            &d.params,
                            &d.param_defs,
                            &d.body,
                        )),
                        _ => None,
                    };
                    let mut known: std::collections::HashSet<_> =
                        name_based.iter().filter_map(fingerprint).collect();
                    let mut union = name_based;
                    if let Some(cands) = Self::captured_dispatch_candidates(data) {
                        for cand in cands.iter() {
                            if fingerprint(cand).is_some_and(|f| known.insert(f)) {
                                union.push(cand.clone());
                            }
                        }
                    }
                    return Some(Ok(Value::array(self.with_joined_setting_candidates(
                        &data.package.resolve(),
                        disp_name.as_str(),
                        union,
                    ))));
                }
            }
            // Fall back to captured candidates (for out-of-scope multi subs)
            if let Some(cands) = Self::captured_dispatch_candidates(data) {
                if cands.is_empty() {
                    let builtin = self.builtin_routine_candidates(
                        &data.package.resolve(),
                        &data.name.resolve(),
                    );
                    if !builtin.is_empty() {
                        return Some(Ok(Value::array(builtin)));
                    }
                }
                // A module registers each candidate twice (the hoist pass and
                // the in-sequence pass), and an import's family can hold both
                // copies: report one per declaration, as the by-name walk does.
                let mut seen = std::collections::HashSet::new();
                let once = cands
                    .iter()
                    .filter(|cand| match cand.view() {
                        ValueView::Sub(d) => seen.insert(crate::ast::function_body_fingerprint(
                            &d.params,
                            &d.param_defs,
                            &d.body,
                        )),
                        _ => true,
                    })
                    .cloned()
                    .collect::<Vec<_>>();
                // A dispatcher over a scope's own candidates of a built-in
                // operator that falls back to it (a block's `multi
                // infix:<+>`, an import) adds them to the setting's family,
                // which is listed first (#17923, #18617).
                if data
                    .env
                    .contains_key_sym(crate::symbol::wk::multi_dispatch_builtin_fallback())
                    && !data
                        .env
                        .contains_key_sym(crate::symbol::wk::multi_dispatch_proto_shadows_core())
                    && let Some(ValueView::Str(disp_name)) =
                        data.env.get("__mutsu_multi_dispatch_name").map(Value::view)
                {
                    let mut family = self.setting_multi_candidates("GLOBAL", disp_name.as_str());
                    family.extend(once);
                    return Some(Ok(Value::array(family)));
                }
                return Some(Ok(Value::array(once)));
            }
            if let Some(ValueView::Str(cls)) = data.env.get("__mutsu_lookup_class").map(Value::view)
                && let Some(ValueView::Str(meth)) =
                    data.env.get("__mutsu_lookup_method").map(Value::view)
            {
                let candidates = self.classhow_lookup_all_candidates(&cls, &meth, data.package);
                return Some(Ok(Value::array(candidates)));
            }
            // A routine with code of its own is one candidate, even when it is
            // named like a built-in operator (a block-nested `multi infix:<+>`
            // the CHECK phase builds from its declaration): its candidates are
            // itself, as in rakudo.
            let own_code = !data.body.is_empty()
                || data.compiled_routine.is_some()
                || data.compiled_code.is_some();
            let builtin = if own_code {
                Vec::new()
            } else {
                self.builtin_routine_candidates(&data.package.resolve(), &data.name.resolve())
            };
            if !builtin.is_empty() {
                return Some(Ok(Value::array(builtin)));
            }
            return Some(Ok(Value::array(vec![target.clone()])));
        }
        None
    }

    /// `cando` of a `&name` handle.
    // Cost: O(c * p), c = candidates, p = parameters each.
    pub(crate) fn routine_handle_cando(
        &mut self,
        target: &Value,
        name: &str,
        package: &str,
        method: &str,
        args: Vec<Value>,
        is_regex: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, name, package, &args, is_regex);
        if method == "cando" && args.len() == 1 {
            let call_args = Self::capture_to_call_args(&args[0]);
            let builtin = self.builtin_routine_candidates(package, name);
            if !builtin.is_empty() {
                return Some(self.cando_matching(builtin, &call_args));
            }
            let matching = match self.resolve_all_matching_candidates(name, &call_args) {
                Ok(matching) => matching,
                Err(e) => return Some(Err(e)),
            };
            let matching = matching
                .into_iter()
                .map(|def| {
                    if let Some(code) = &def.dispatchee {
                        return code.clone();
                    }
                    let compiled_routine = def.compiled_fn().cloned();
                    Value::make_sub_for_routine(
                        def.package,
                        def.name,
                        def.params,
                        def.param_defs,
                        def.body,
                        def.is_rw,
                        self.env.clone(),
                        compiled_routine,
                    )
                })
                .collect();
            return Some(Ok(Value::array(matching)));
        }
        None
    }

    /// `cando` of a `Sub`.
    // Cost: O(c * p), c = candidates, p = parameters each.
    pub(crate) fn sub_cando(
        &mut self,
        target: &Value,
        data: &crate::value::SubData,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, data, &args);
        if method == "cando" && args.len() == 1 {
            if let (Some(class_name), Some(method_name)) = (
                data.env
                    .get("__mutsu_lookup_class")
                    .map(Value::to_string_value),
                data.env
                    .get("__mutsu_lookup_method")
                    .map(Value::to_string_value),
            ) {
                let call_args = Self::capture_to_call_args(&args[0]);
                let methods = self.collect_can_methods(
                    &Value::package(Symbol::intern(&class_name)),
                    &method_name,
                );
                return Some(self.cando_matching(methods, &call_args));
            }
            // Multi-dispatch dispatcher: try name-based lookup first
            if let Some(ValueView::Str(disp_name)) =
                data.env.get("__mutsu_multi_dispatch_name").map(Value::view)
            {
                let name_based =
                    self.routine_candidate_subs(&data.package.resolve(), disp_name.as_str());
                if !name_based.is_empty() {
                    let call_args = Self::capture_to_call_args(&args[0]);
                    let family = self.with_joined_setting_candidates(
                        &data.package.resolve(),
                        disp_name.as_str(),
                        name_based,
                    );
                    return Some(self.cando_matching(family, &call_args));
                }
            }
            // Fall back to captured candidates
            if let Some(cands) = Self::captured_dispatch_candidates(data) {
                let call_args = Self::capture_to_call_args(&args[0]);
                if cands.is_empty() {
                    let builtin = self.builtin_routine_candidates(
                        &data.package.resolve(),
                        &data.name.resolve(),
                    );
                    if !builtin.is_empty() {
                        return Some(self.cando_matching(builtin, &call_args));
                    }
                }
                return Some(self.cando_matching(cands.iter().cloned(), &call_args));
            }
            let call_args = Self::capture_to_call_args(&args[0]);
            // A built-in candidate (one `.candidates` of a core multi handed
            // out) is a single routine: it answers for itself, not for its
            // whole family.
            let builtin = if data.env.contains_key("__mutsu_is_multi_candidate") {
                Vec::new()
            } else {
                self.builtin_routine_candidates(&data.package.resolve(), &data.name.resolve())
            };
            if !builtin.is_empty() {
                return Some(self.cando_matching(builtin, &call_args));
            }
            return Some(self.cando_matching(std::iter::once(target.clone()), &call_args));
        }
        None
    }

    /// `declared`, the program's own candidates of `package`/`name`, behind
    /// the setting candidates its `multi`s join
    /// ([`Self::joined_setting_candidates`]), as rakudo's derived proto lists
    /// them.
    // Cost: as `joined_setting_candidates`, plus O(d), d = `declared`.
    fn with_joined_setting_candidates(
        &self,
        package: &str,
        name: &str,
        declared: Vec<Value>,
    ) -> Vec<Value> {
        let mut family = self.joined_setting_candidates(package, name);
        if family.is_empty() {
            return declared;
        }
        family.extend(declared);
        family
    }

    /// The `candidates` a `cando` capture binds, as an Array. A `where` that
    /// dies fails the `cando` itself, as in rakudo.
    // Cost: O(c * p), c = candidates, p = parameters each.
    fn cando_matching(
        &mut self,
        candidates: impl IntoIterator<Item = Value>,
        call_args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut matching = Vec::new();
        for candidate in candidates {
            if self.candidate_matches_capture(&candidate, call_args)? {
                matching.push(candidate);
            }
        }
        Ok(Value::array(matching))
    }
}
