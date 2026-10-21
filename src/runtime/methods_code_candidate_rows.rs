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
            return Some(Ok(Value::array(declared)));
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
            // Counted by declaration: a candidate is registered by both the
            // hoist pass and the in-sequence pass, and a dispatcher built from
            // an export table can carry both copies of one declaration.
            let captured_len = match data
                .env
                .get("__mutsu_multi_dispatch_candidates")
                .map(Value::view)
            {
                Some(ValueView::Array(cands, _)) => cands
                    .iter()
                    .filter_map(|cand| match cand.view() {
                        ValueView::Sub(d) => Some(crate::ast::function_body_fingerprint(
                            &d.params,
                            &d.param_defs,
                            &d.body,
                        )),
                        _ => None,
                    })
                    .collect::<std::collections::HashSet<_>>()
                    .len(),
                _ => 0,
            };
            if let Some(ValueView::Str(disp_name)) =
                data.env.get("__mutsu_multi_dispatch_name").map(Value::view)
            {
                let name_based =
                    self.routine_candidate_subs(&data.package.resolve(), disp_name.as_str());
                if !name_based.is_empty() && name_based.len() >= captured_len {
                    return Some(Ok(Value::array(name_based)));
                }
            }
            // Fall back to captured candidates (for out-of-scope multi subs)
            if let Some(ValueView::Array(cands, _)) = data
                .env
                .get("__mutsu_multi_dispatch_candidates")
                .map(Value::view)
            {
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
                return Some(Ok(Value::array(once)));
            }
            if let Some(ValueView::Str(cls)) = data.env.get("__mutsu_lookup_class").map(Value::view)
                && let Some(ValueView::Str(meth)) =
                    data.env.get("__mutsu_lookup_method").map(Value::view)
            {
                let candidates = self.classhow_lookup_all_candidates(&cls, &meth, data.package);
                return Some(Ok(Value::array(candidates)));
            }
            let builtin =
                self.builtin_routine_candidates(&data.package.resolve(), &data.name.resolve());
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
                let matching: Vec<Value> = builtin
                    .into_iter()
                    .filter(|cand| self.candidate_matches_call_args(cand, &call_args))
                    .collect();
                return Some(Ok(Value::array(matching)));
            }
            let matching = self
                .resolve_all_matching_candidates(name, &call_args)
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
                let method_args = call_args.get(1..).unwrap_or(&[]);
                return Some(Ok(Value::array(
                    self.collect_can_methods(
                        &Value::package(Symbol::intern(&class_name)),
                        &method_name,
                    )
                    .into_iter()
                    .filter(|candidate| self.candidate_matches_call_args(candidate, method_args))
                    .collect(),
                )));
            }
            // Multi-dispatch dispatcher: try name-based lookup first
            if let Some(ValueView::Str(disp_name)) =
                data.env.get("__mutsu_multi_dispatch_name").map(Value::view)
            {
                let name_based =
                    self.routine_candidate_subs(&data.package.resolve(), disp_name.as_str());
                if !name_based.is_empty() {
                    let call_args = Self::capture_to_call_args(&args[0]);
                    let matches: Vec<Value> = name_based
                        .into_iter()
                        .filter(|c| self.candidate_matches_call_args(c, &call_args))
                        .collect();
                    return Some(Ok(Value::array(matches)));
                }
            }
            // Fall back to captured candidates
            if let Some(ValueView::Array(cands, _)) = data
                .env
                .get("__mutsu_multi_dispatch_candidates")
                .map(Value::view)
            {
                let call_args = Self::capture_to_call_args(&args[0]);
                if cands.is_empty() {
                    let builtin = self.builtin_routine_candidates(
                        &data.package.resolve(),
                        &data.name.resolve(),
                    );
                    if !builtin.is_empty() {
                        let matches: Vec<Value> = builtin
                            .into_iter()
                            .filter(|c| self.candidate_matches_call_args(c, &call_args))
                            .collect();
                        return Some(Ok(Value::array(matches)));
                    }
                }
                let matches: Vec<Value> = cands
                    .iter()
                    .filter(|c| self.candidate_matches_call_args(c, &call_args))
                    .cloned()
                    .collect();
                return Some(Ok(Value::array(matches)));
            }
            let call_args = Self::capture_to_call_args(&args[0]);
            let builtin =
                self.builtin_routine_candidates(&data.package.resolve(), &data.name.resolve());
            if !builtin.is_empty() {
                let matches: Vec<Value> = builtin
                    .into_iter()
                    .filter(|c| self.candidate_matches_call_args(c, &call_args))
                    .collect();
                return Some(Ok(Value::array(matches)));
            }
            let matches = if self.candidate_matches_call_args(target, &call_args) {
                vec![target.clone()]
            } else {
                Vec::new()
            };
            return Some(Ok(Value::array(matches)));
        }
        None
    }
}
