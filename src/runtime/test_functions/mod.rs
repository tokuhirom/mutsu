mod basic;
mod comparison;
mod eval_exception;
mod fails_like;
mod subprocess;
mod tap_ok;
mod tap_subtest;
mod throws_like;
mod util;

use super::*;

impl Interpreter {
    pub(crate) fn sync_eval_definition_state(&mut self, nested: &Interpreter) {
        self.type_metadata = nested.type_metadata.clone();
        self.classes = nested.classes.clone();
        self.cunion_classes = nested.cunion_classes.clone();
        self.hidden_classes = nested.hidden_classes.clone();
        self.class_stubs = nested.class_stubs.clone();
        self.package_stubs = nested.package_stubs.clone();
        self.hidden_defer_parents = nested.hidden_defer_parents.clone();
        self.class_trusts = nested.class_trusts.clone();
        self.class_composed_roles = nested.class_composed_roles.clone();
        self.roles = nested.roles.clone();
        self.role_candidates = nested.role_candidates.clone();
        self.role_parents = nested.role_parents.clone();
        self.role_hides = nested.role_hides.clone();
        self.role_type_params = nested.role_type_params.clone();
        self.class_role_param_bindings = nested.class_role_param_bindings.clone();
        self.attribute_build_overrides = nested.attribute_build_overrides.clone();
        self.subsets = nested.subsets.clone();
        self.need_hidden_classes = nested.need_hidden_classes.clone();
    }

    pub(crate) fn cmp_eqv_bool(left: &Value, right: &Value) -> bool {
        use crate::value::JunctionKind;
        match (left, right) {
            (
                Value::Junction {
                    kind: lkind,
                    values: lvals,
                },
                _,
            ) => match lkind {
                JunctionKind::Any => lvals.iter().any(|lv| Self::cmp_eqv_bool(lv, right)),
                JunctionKind::All => lvals.iter().all(|lv| Self::cmp_eqv_bool(lv, right)),
                JunctionKind::One => {
                    lvals
                        .iter()
                        .filter(|lv| Self::cmp_eqv_bool(lv, right))
                        .count()
                        == 1
                }
                JunctionKind::None => lvals.iter().all(|lv| !Self::cmp_eqv_bool(lv, right)),
            },
            (
                _,
                Value::Junction {
                    kind: rkind,
                    values: rvals,
                },
            ) => match rkind {
                JunctionKind::Any => rvals.iter().any(|rv| Self::cmp_eqv_bool(left, rv)),
                JunctionKind::All => rvals.iter().all(|rv| Self::cmp_eqv_bool(left, rv)),
                JunctionKind::One => {
                    rvals
                        .iter()
                        .filter(|rv| Self::cmp_eqv_bool(left, rv))
                        .count()
                        == 1
                }
                JunctionKind::None => rvals.iter().all(|rv| !Self::cmp_eqv_bool(left, rv)),
            },
            _ => left.eqv(right),
        }
    }

    pub(crate) fn unwrap_test_arg_value(value: &Value) -> Value {
        match value {
            Value::Capture { positional, named }
                if positional.is_empty()
                    && matches!(named.get("__mutsu_varref_name"), Some(Value::Str(_)))
                    && named.contains_key("__mutsu_varref_value") =>
            {
                named
                    .get("__mutsu_varref_value")
                    .cloned()
                    .unwrap_or(Value::Nil)
            }
            Value::Pair(key, val) => {
                Value::Pair(key.clone(), Box::new(Self::unwrap_test_arg_value(val)))
            }
            _ => value.clone(),
        }
    }

    pub(crate) fn program_mentions_qx(program: &str) -> bool {
        for marker in ["qx", "qqx"] {
            let mut search_from = 0usize;
            while let Some(offset) = program[search_from..].find(marker) {
                let idx = search_from + offset;
                let before_ok = if idx == 0 {
                    true
                } else {
                    !program[..idx]
                        .chars()
                        .next_back()
                        .is_some_and(|c| c.is_alphanumeric() || matches!(c, '_' | '\'' | '-'))
                };
                let after = &program[idx + marker.len()..];
                let after_ok = after
                    .chars()
                    .next()
                    .is_some_and(|c| !c.is_alphanumeric() && !matches!(c, '_' | '\'' | '-'));
                if before_ok && after_ok {
                    return true;
                }
                search_from = idx + marker.len();
            }
        }
        false
    }

    /// Returns true when the Test module has been loaded (plan or test
    /// state exists), indicating that test function names should be resolved
    /// as function calls rather than bare words.
    pub(crate) fn test_mode_active(&self) -> bool {
        self.test_state.is_some()
    }

    /// Check if a name matches a known test function (Test or Test::Util).
    /// Used by the bare word resolver to dispatch zero-arg test function calls.
    pub(crate) fn is_test_function_name(name: &str) -> bool {
        matches!(
            name,
            "ok" | "nok"
                | "diag"
                | "pass"
                | "flunk"
                | "is"
                | "isnt"
                | "plan"
                | "done-testing"
                | "skip"
                | "skip-rest"
                | "bail-out"
                | "cmp-ok"
                | "like"
                | "unlike"
                | "is-deeply"
                | "is-approx"
                | "lives-ok"
                | "dies-ok"
                | "isa-ok"
                | "force_todo"
                | "force-todo"
                | "eval-lives-ok"
                | "eval-dies-ok"
                | "throws-like"
                | "throws-like-any"
                | "fails-like"
                | "is_run"
                | "run"
                | "get_out"
                | "use-ok"
                | "does-ok"
                | "can-ok"
                | "todo"
                | "subtest"
                | "tap-ok"
                | "warns-like"
                | "doesn't-warn"
                | "is-eqv"
                | "group-of"
                | "doesn't-hang"
                | "make-temp-file"
                | "make-temp-path"
                | "make-temp-dir"
                | "is-deeply-junction"
                | "is-path"
        )
    }

    /// Dispatch Test module functions. Returns `Ok(Some(value))` if the name
    /// matched a Test function, `Ok(None)` if it did not.
    pub(crate) fn call_test_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let normalized_args: Vec<Value> = args.iter().map(Self::unwrap_test_arg_value).collect();
        // Auto-FETCH any Proxy containers in test function arguments
        let fetched_args: Vec<Value> = normalized_args
            .into_iter()
            .map(|v| self.auto_fetch_proxy(&v))
            .collect::<Result<Vec<_>, _>>()?;
        let args = fetched_args.as_slice();
        match name {
            "ok" => self.test_fn_ok(args).map(Some),
            "nok" => self.test_fn_nok(args).map(Some),
            "diag" => self.test_fn_diag(args).map(Some),
            "pass" => self.test_fn_pass(args).map(Some),
            "flunk" => self.test_fn_flunk(args).map(Some),
            "is" => self.test_fn_is(args).map(Some),
            "isnt" => self.test_fn_isnt(args).map(Some),
            "plan" => self.test_fn_plan(args).map(Some),
            "done-testing" => self.test_fn_done_testing().map(Some),
            "skip" => self.test_fn_skip(args).map(Some),
            "skip-rest" => self.test_fn_skip_rest(args).map(Some),
            "bail-out" => self.test_fn_bail_out(args).map(Some),
            "cmp-ok" => self.test_fn_cmp_ok(args).map(Some),
            "like" => self.test_fn_like(args).map(Some),
            "unlike" => self.test_fn_unlike(args).map(Some),
            "is-deeply" => self.test_fn_is_deeply(args).map(Some),
            "is-approx" => self.test_fn_is_approx(args).map(Some),
            "lives-ok" => self.test_fn_lives_ok(args).map(Some),
            "dies-ok" => self.test_fn_dies_ok(args).map(Some),
            "isa-ok" => self.test_fn_isa_ok(args).map(Some),
            "force_todo" | "force-todo" => self.test_fn_force_todo(args).map(Some),
            "eval-lives-ok" => self.test_fn_eval_lives_ok(args).map(Some),
            "eval-dies-ok" => self.test_fn_eval_dies_ok(args).map(Some),
            "throws-like" => self.test_fn_throws_like(args).map(Some),
            "throws-like-any" => self.test_fn_throws_like_any(args).map(Some),
            "fails-like" => self.test_fn_fails_like(args).map(Some),
            "is_run" => self.test_fn_is_run(args).map(Some),
            "run" | "Test::Util::run" => self.test_fn_run(args).map(Some),
            "get_out" => self.test_fn_get_out(args).map(Some),
            "use-ok" => self.test_fn_use_ok(args).map(Some),
            "does-ok" => self.test_fn_does_ok(args).map(Some),
            "can-ok" => self.test_fn_can_ok(args).map(Some),
            "todo" => self.test_fn_todo(args).map(Some),
            "subtest" => self.test_fn_subtest(args).map(Some),
            "tap-ok" => self.test_fn_tap_ok(args).map(Some),
            "warns-like" => self.test_fn_warns_like(args).map(Some),
            "doesn't-warn" => self.test_fn_doesnt_warn(args).map(Some),
            "is-eqv" => self.test_fn_is_eqv(args).map(Some),
            "group-of" => self.test_fn_group_of(args).map(Some),
            "doesn't-hang" => self.test_fn_doesnt_hang(args).map(Some),
            "make-temp-file" | "make-temp-path" => self.test_fn_make_temp_file(args).map(Some),
            "make-temp-dir" => self.test_fn_make_temp_dir(args).map(Some),
            "is-deeply-junction" => self.test_fn_is_deeply_junction(args).map(Some),
            "is-path" => self.test_fn_is_path(args).map(Some),
            _ => Ok(None),
        }
    }

    /// Get a diagnostic string for a value, trying .gist then .raku,
    /// falling back to basic string representation.
    pub(crate) fn value_for_diag(&mut self, val: &Value) -> String {
        // For instances, try .gist first, then .raku
        if matches!(val, Value::Instance { .. } | Value::Package(_)) {
            if let Ok(result) = self.call_method_with_values(val.clone(), "gist", vec![]) {
                return result.to_string_value();
            }
            if let Ok(result) = self.call_method_with_values(val.clone(), "raku", vec![]) {
                return result.to_string_value();
            }
        }
        val.to_string_value()
    }

    pub(crate) fn value_raku_repr(val: &Value) -> String {
        if let Some(Ok(Value::Str(s))) =
            crate::builtins::native_method_0arg(val, crate::symbol::Symbol::intern("raku"))
        {
            s.to_string()
        } else {
            val.to_string_value()
        }
    }
}
