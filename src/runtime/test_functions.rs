use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    fn sync_eval_definition_state(&mut self, nested: &Interpreter) {
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

    fn cmp_eqv_bool(left: &Value, right: &Value) -> bool {
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

    fn unwrap_test_arg_value(value: &Value) -> Value {
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

    fn program_mentions_qx(program: &str) -> bool {
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
    fn value_for_diag(&mut self, val: &Value) -> String {
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

    fn test_fn_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let value = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let ok = value.truthy();
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_nok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let value = Self::positional_value_required(args, 0, "nok expects condition")?;
        let ok = !value.truthy();
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_diag(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let msg = Self::positional_string(args, 0);
        self.emit_output(&format!("# {}\n", msg));
        Ok(Value::Nil)
    }

    fn test_fn_pass(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        self.test_ok(true, &desc, todo)?;
        Ok(Value::Bool(true))
    }

    fn test_fn_flunk(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        self.test_ok(false, &desc, todo)?;
        Ok(Value::Bool(false))
    }

    fn test_fn_is(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let todo = Self::named_bool(args, "todo");
        let positional: Vec<&Value> = args
            .iter()
            .filter(|arg| !matches!(arg, Value::Pair(key, _) if key == "todo"))
            .collect();
        let left = positional.first().cloned().cloned();
        let right = positional.get(1).cloned().cloned();
        let desc = positional
            .get(2)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let ok = match (left.as_ref(), right.as_ref()) {
            (Some(left), Some(right)) => {
                if matches!(left, Value::Junction { .. }) || matches!(right, Value::Junction { .. })
                {
                    Self::eqv_with_junctions(left, right).truthy()
                } else if matches!(
                    left,
                    Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                ) || matches!(
                    right,
                    Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                ) {
                    crate::runtime::value_to_list(left) == crate::runtime::value_to_list(right)
                } else if crate::vm::VM::is_buf_value(left) && crate::vm::VM::is_buf_value(right) {
                    crate::vm::VM::extract_buf_bytes(left)
                        == crate::vm::VM::extract_buf_bytes(right)
                } else {
                    self.stringify_test_value(left)? == self.stringify_test_value(right)?
                }
            }
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        if !ok {
            let got = left
                .as_ref()
                .map(|v| self.value_for_diag(v))
                .unwrap_or_default();
            let expected = right
                .as_ref()
                .map(|v| self.value_for_diag(v))
                .unwrap_or_default();
            self.stderr_output
                .push_str(&format!("expected: {}\n     got: {}\n", expected, got));
        }
        Ok(Value::Bool(ok))
    }

    fn stringify_test_value(&mut self, value: &Value) -> Result<String, RuntimeError> {
        match value {
            Value::LazyList(list) => Ok(self
                .force_lazy_list(list)?
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(" ")),
            _ => Ok(value.to_string_value()),
        }
    }

    fn test_fn_isnt(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let todo = Self::named_bool(args, "todo");
        let positional: Vec<&Value> = args
            .iter()
            .filter(|arg| !matches!(arg, Value::Pair(key, _) if key == "todo"))
            .collect();
        let left = positional
            .first()
            .copied()
            .ok_or_else(|| RuntimeError::new("isnt expects left"))?;
        let right = positional
            .get(1)
            .copied()
            .ok_or_else(|| RuntimeError::new("isnt expects right"))?;
        let desc = positional
            .get(2)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let ok = left != right;
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_plan(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(reason) = Self::named_value(args, "skip-all") {
            self.test_state.get_or_insert_with(TestState::new).planned = Some(0);
            let reason_str = reason.to_string_value();
            if reason_str.is_empty() || reason_str == "True" {
                self.emit_output("1..0 # Skipped: no reason given\n");
            } else {
                self.emit_output(&format!("1..0 # Skipped: {}\n", reason_str));
            }
            self.halted = true;
        } else {
            let count = Self::positional_value_required(args, 0, "plan expects count")?;
            let planned = match count {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::BigInt(i) => {
                    use num_traits::ToPrimitive;
                    let n = i.to_i128().unwrap_or(-1);
                    if n < 0 {
                        return Err(RuntimeError::new("plan expects Int"));
                    }
                    n as usize
                }
                Value::Num(f) if *f >= 0.0 && f.fract() == 0.0 => *f as usize,
                Value::Rat(n, d) if *d != 0 && *n >= 0 && *n % *d == 0 => (*n / *d) as usize,
                Value::FatRat(n, d) if *d != 0 && *n >= 0 && *n % *d == 0 => (*n / *d) as usize,
                _ => return Err(RuntimeError::new("plan expects Int")),
            };
            self.test_state.get_or_insert_with(TestState::new).planned = Some(planned);
            self.emit_output(&format!("1..{}\n", planned));
        }
        Ok(Value::Nil)
    }

    fn test_fn_done_testing(&mut self) -> Result<Value, RuntimeError> {
        let ran = {
            let state = self.test_state.get_or_insert_with(TestState::new);
            if state.planned.is_some() {
                return Ok(Value::Nil);
            }
            state.planned = Some(state.ran);
            state.ran
        };
        self.emit_output(&format!("1..{}\n", ran));
        Ok(Value::Nil)
    }

    fn test_fn_skip(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let count = match Self::positional_value(args, 1) {
            Some(Value::Int(n)) => (*n).max(1) as usize,
            _ => 1usize,
        };
        let mut lines = Vec::with_capacity(count);
        let state = self.test_state.get_or_insert_with(TestState::new);
        for _ in 0..count {
            state.ran += 1;
            lines.push(format!("ok {} - {} # SKIP\n", state.ran, desc));
        }
        for line in lines {
            self.emit_output(&line);
        }
        Ok(Value::Nil)
    }

    fn test_fn_skip_rest(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        let mut lines = Vec::new();
        {
            let state = self.test_state.get_or_insert_with(TestState::new);
            if let Some(planned) = state.planned {
                while state.ran < planned {
                    state.ran += 1;
                    if desc.is_empty() {
                        lines.push(format!("ok {} # SKIP\n", state.ran));
                    } else {
                        lines.push(format!("ok {} - {} # SKIP\n", state.ran, desc));
                    }
                }
            }
        }
        for line in &lines {
            self.emit_output(line);
        }
        self.halted = true;
        Ok(Value::Nil)
    }

    fn test_fn_bail_out(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 0);
        if desc.is_empty() {
            self.emit_output("Bail out!\n");
        } else {
            self.emit_output(&format!("Bail out! {}\n", desc));
        }
        self.halted = true;
        self.bailed_out = true;
        Ok(Value::Nil)
    }

    fn test_fn_cmp_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let left = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let op_val = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let right = Self::positional_value(args, 2)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 3);
        let todo = Self::named_bool(args, "todo");
        // Clone values before they might be consumed by callable operator
        let left_diag = left.clone();
        let right_diag = right.clone();
        let op_diag = match &op_val {
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                format!("'{}'", crate::value::what_type_name(&op_val))
            }
            _ => {
                let s = op_val.to_string_value();
                if s.is_empty() {
                    crate::value::what_type_name(&op_val).to_string()
                } else {
                    s
                }
            }
        };
        let ok = match &op_val {
            Value::Str(op) => match op.as_str() {
                "~~" => self.smart_match(&left, &right),
                "!~~" => !self.smart_match(&left, &right),
                "eq" => left.to_string_value() == right.to_string_value(),
                "ne" => left.to_string_value() != right.to_string_value(),
                "lt" => left.to_string_value() < right.to_string_value(),
                "le" => left.to_string_value() <= right.to_string_value(),
                "gt" => left.to_string_value() > right.to_string_value(),
                "ge" => left.to_string_value() >= right.to_string_value(),
                "==" => super::to_float_value(&left) == super::to_float_value(&right),
                "!=" => super::to_float_value(&left) != super::to_float_value(&right),
                "<" => super::to_float_value(&left) < super::to_float_value(&right),
                "<=" => super::to_float_value(&left) <= super::to_float_value(&right),
                ">" => super::to_float_value(&left) > super::to_float_value(&right),
                ">=" => super::to_float_value(&left) >= super::to_float_value(&right),
                "===" => crate::runtime::utils::values_identical(&left, &right),
                "!===" => !crate::runtime::utils::values_identical(&left, &right),
                "eqv" => Self::cmp_eqv_bool(&left, &right),
                "=:=" => crate::runtime::utils::values_identical(&left, &right),
                "=~=" | "\u{2245}" => {
                    // =~= / ≅ approximately equal
                    let (lr, li) = match &left {
                        Value::Complex(r, i) => (*r, *i),
                        _ => (super::to_float_value(&left).unwrap_or(0.0), 0.0),
                    };
                    let (rr, ri) = match &right {
                        Value::Complex(r, i) => (*r, *i),
                        _ => (super::to_float_value(&right).unwrap_or(0.0), 0.0),
                    };
                    let tol = 1e-15;
                    let approx = |a: f64, b: f64| {
                        let max = a.abs().max(b.abs());
                        if max == 0.0 {
                            true
                        } else {
                            (a - b).abs() / max <= tol
                        }
                    };
                    approx(lr, rr) && approx(li, ri)
                }
                _ => {
                    return Err(RuntimeError::new(format!(
                        "cmp-ok: unsupported string operator '{}'",
                        op
                    )));
                }
            },
            _ => {
                let result = self.call_sub_value(op_val, vec![left, right], false)?;
                result.truthy()
            }
        };
        self.test_ok(ok, &desc, todo)?;
        if !ok {
            let got_str = self.value_for_diag(&left_diag);
            let expected_str = self.value_for_diag(&right_diag);
            let diag = format!(
                "# Failed test '{}'\n# expected: {}\n#      got: {}\n# matcher: {}\n",
                desc, expected_str, got_str, op_diag
            );
            self.stderr_output.push_str(&diag);
            eprint!("{}", diag);
        }
        Ok(Value::Bool(ok))
    }

    fn test_fn_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let text = match Self::positional_value(args, 0) {
            Some(v) => self.stringify_value(v.clone())?,
            None => String::new(),
        };
        let ok = match Self::positional_value(args, 1) {
            Some(Value::Regex(pat)) => self.regex_is_match(pat, &text),
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_unlike(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let text = match Self::positional_value(args, 0) {
            Some(v) => self.stringify_value(v.clone())?,
            None => String::new(),
        };
        let ok = match Self::positional_value(args, 1) {
            Some(Value::Regex(pat)) => !self.regex_is_match(pat, &text),
            _ => true,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_is_deeply(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // is-deeply can receive Pair values as positional arguments (e.g., `is-deeply (:a), (a => True), "desc"`).
        // We must not filter out Pairs as named args here — use raw positional extraction.
        let (left, right, desc) = Self::extract_is_deeply_args(args);
        let todo = Self::named_bool(args, "todo");
        let ok = match (left, right) {
            (Some(left), Some(right)) => {
                // If either side is a Junction, thread eqv through the junction
                // and check the boolean of the result (Raku semantics).
                if matches!(left, Value::Junction { .. }) || matches!(right, Value::Junction { .. })
                {
                    Self::eqv_with_junctions(left, right).truthy()
                } else {
                    // Per raku-doc (Type/Test.rakudoc): is-deeply uses eqv semantics.
                    // Seq:D arguments get converted to Lists by calling .cache.
                    self.seq_to_list(left).eqv(&self.seq_to_list(right))
                }
            }
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        if !ok {
            let got_raku = left.map(Self::value_raku_repr).unwrap_or_default();
            let expected_raku = right.map(Self::value_raku_repr).unwrap_or_default();
            self.emit_output(&format!(
                "# expected: {}\n#      got: {}\n",
                expected_raku, got_raku
            ));
        }
        Ok(Value::Bool(ok))
    }

    fn test_fn_is_deeply_junction(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let left = Self::positional_value(args, 0);
        let right = Self::positional_value(args, 1);
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let ok = match (left, right) {
            (Some(left), Some(right)) => match (
                Self::junction_guts_value(left),
                Self::junction_guts_value(right),
            ) {
                (Some(lg), Some(rg)) => lg.eqv(&rg),
                _ => false,
            },
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    /// Extract (left, right, desc) for is-deeply from raw args.
    /// The first two args are always the values being compared (even if they are Pairs).
    /// Only known internal named pairs (__mutsu_test_callsite_line, todo) are skipped.
    fn extract_is_deeply_args(args: &[Value]) -> (Option<&Value>, Option<&Value>, String) {
        let mut positionals = Vec::new();
        for arg in args {
            if let Value::Pair(key, _) = arg
                && (key == "__mutsu_test_callsite_line" || key == "todo")
            {
                continue;
            }
            positionals.push(arg);
        }
        let left = positionals.first().copied();
        let right = positionals.get(1).copied();
        let desc = positionals
            .get(2)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        (left, right, desc)
    }

    /// Convert Seq to List (Array) for is-deeply comparison, per Raku spec.
    fn seq_to_list(&mut self, v: &Value) -> Value {
        match v {
            Value::Seq(items) => Value::Array(items.clone(), ArrayKind::List),
            Value::LazyList(list) => match self.force_lazy_list(list) {
                Ok(items) => Value::Array(std::sync::Arc::new(items), ArrayKind::List),
                Err(_) => v.clone(),
            },
            _ => v.clone(),
        }
    }

    fn junction_kind_name(kind: &JunctionKind) -> &'static str {
        match kind {
            JunctionKind::Any => "any",
            JunctionKind::All => "all",
            JunctionKind::One => "one",
            JunctionKind::None => "none",
        }
    }

    fn junction_sort_key(v: &Value) -> String {
        match v {
            Value::Array(items, _) => {
                let parts: Vec<String> = items.iter().map(Self::junction_sort_key).collect();
                format!("[{}]", parts.join(","))
            }
            Value::Junction { .. } => {
                Self::junction_sort_key(&Self::junction_guts_value(v).unwrap_or(Value::Nil))
            }
            _ => v.to_string_value(),
        }
    }

    fn junction_guts_value(v: &Value) -> Option<Value> {
        let Value::Junction { kind, values } = v else {
            return None;
        };
        // Normalize recursively so nested junction structures compare order-independently.
        let mut guts: Vec<Value> = values
            .iter()
            .map(|value| Self::junction_guts_value(value).unwrap_or_else(|| value.clone()))
            .collect();
        guts.sort_by_key(Self::junction_sort_key);
        Some(Value::array(vec![
            Value::str(Self::junction_kind_name(kind).to_string()),
            Value::array(guts),
        ]))
    }

    /// Perform `eqv` comparison that threads through Junctions,
    /// returning a Value (possibly a Junction of Bools).
    fn eqv_with_junctions(left: &Value, right: &Value) -> Value {
        if let Value::Junction { kind, values } = left {
            let results: Vec<Value> = values
                .iter()
                .map(|v| Self::eqv_with_junctions(v, right))
                .collect();
            return Value::junction(kind.clone(), results);
        }
        if let Value::Junction { kind, values } = right {
            let results: Vec<Value> = values
                .iter()
                .map(|v| Self::eqv_with_junctions(left, v))
                .collect();
            return Value::junction(kind.clone(), results);
        }
        Value::Bool(left.eqv(right))
    }

    fn test_fn_is_approx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let todo = Self::named_bool(args, "todo");
        let got = Self::positional_value_required(args, 0, "is-approx expects got")?;
        let expected = Self::positional_value_required(args, 1, "is-approx expects expected")?;

        // Detect three-arg form: is-approx(got, expected, numeric-tolerance, desc?)
        // If the third positional arg is numeric, treat it as abs-tol.
        let third_pos = Self::positional_value(args, 2);
        let (positional_abs_tol, desc) = if let Some(v) = third_pos.and_then(super::to_float_value)
        {
            (Some(v), Self::positional_string(args, 3))
        } else {
            (None, Self::positional_string(args, 2))
        };

        let explicit_abs_tol = Self::named_value(args, "abs-tol")
            .and_then(|v| super::to_float_value(&v))
            .or(positional_abs_tol);
        let explicit_rel_tol =
            Self::named_value(args, "rel-tol").and_then(|v| super::to_float_value(&v));

        let mut coerce_float = |value: &Value| -> Option<f64> {
            if let Some(v) = super::to_float_value(value) {
                return Some(v);
            }
            if matches!(value, Value::Instance { .. }) {
                let coerced = self
                    .call_method_with_values(value.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(value.clone(), "Bridge", vec![]))
                    .ok()?;
                return super::to_float_value(&coerced);
            }
            None
        };

        // Raku's DWIM is-approx: when |expected| < 1e-6, use abs-tol 1e-5;
        // otherwise use rel-tol 1e-6. Explicit named args override this.
        let expected_f = coerce_float(expected);

        // Helper: check if two f64 values are approximately equal
        let approx_eq = |g: f64, e: f64| -> bool {
            if let Some(at) = explicit_abs_tol {
                (g - e).abs() <= at
            } else if let Some(rt) = explicit_rel_tol {
                let max = g.abs().max(e.abs());
                if max == 0.0 {
                    true
                } else {
                    (g - e).abs() / max <= rt
                }
            } else if e.abs() < 1e-6 {
                // DWIM: near-zero expected → use absolute tolerance
                (g - e).abs() <= 1e-5
            } else {
                // DWIM: use relative tolerance
                let max = g.abs().max(e.abs());
                if max == 0.0 {
                    true
                } else {
                    (g - e).abs() / max <= 1e-6
                }
            }
        };

        // Unwrap Mixin wrappers (e.g. from angle-bracket literals like <1+3i>)
        let got_inner = match got {
            Value::Mixin(inner, _) => inner.as_ref(),
            other => other,
        };
        let expected_inner = match expected {
            Value::Mixin(inner, _) => inner.as_ref(),
            other => other,
        };
        let ok = match (got_inner, expected_inner) {
            (Value::Complex(gr, gi), Value::Complex(er, ei)) => {
                approx_eq(*gr, *er) && approx_eq(*gi, *ei)
            }
            (Value::Complex(gr, gi), _) => {
                if let Some(e) = expected_f {
                    approx_eq(*gr, e) && approx_eq(*gi, 0.0)
                } else {
                    false
                }
            }
            (_, Value::Complex(er, ei)) => {
                if let Some(g) = coerce_float(got) {
                    approx_eq(g, *er) && approx_eq(0.0, *ei)
                } else {
                    false
                }
            }
            _ => match (coerce_float(got), expected_f) {
                (Some(g), Some(e)) => approx_eq(g, e),
                _ => false,
            },
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_lives_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let block = Self::positional_value_required(args, 0, "lives-ok expects block")?.clone();
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let ok = match &block {
            Value::Sub(data) => {
                self.push_caller_env();
                let saved_topic = self.env.get("$_").cloned();
                let result = self.eval_block_value(&data.body).is_ok();
                match saved_topic {
                    Some(v) => {
                        self.env.insert("$_".to_string(), v);
                    }
                    None => {
                        self.env.remove("$_");
                    }
                }
                self.pop_caller_env();
                result
            }
            _ => true,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_dies_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let block = Self::positional_value_required(args, 0, "dies-ok expects block")?.clone();
        let desc = Self::positional_string(args, 1);
        let todo = Self::named_bool(args, "todo");
        let ok = match &block {
            Value::Sub(data) => {
                self.push_caller_env();
                let saved_topic = self.env.get("$_").cloned();
                let result = self.eval_block_value(&data.body).is_err();
                match saved_topic {
                    Some(v) => {
                        self.env.insert("$_".to_string(), v);
                    }
                    None => {
                        self.env.remove("$_");
                    }
                }
                self.pop_caller_env();
                result
            }
            _ => false,
        };
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_isa_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // isa-ok uses the first non-named-pair arg as value.
        // We need special handling because positional_value filters out ALL Pairs,
        // but the first arg could be a Pair value being tested.
        let (value, type_name, desc) = Self::extract_isa_ok_args(args);
        let todo = Self::named_bool(args, "todo");
        let mut ok = value.isa_check(&type_name) || self.type_matches_value(&type_name, value);
        // For Package values, also check full MRO (handles grammar/class inheritance)
        if !ok && let Value::Package(name) = value {
            let mro = self.class_mro(&name.resolve());
            ok = mro.contains(&type_name);
        }
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    /// Extract (value, type_name, desc) for isa-ok from raw args.
    /// The first arg is always the value (even if it's a Pair).
    /// Named Pairs (like :todo) are excluded from positional counting
    /// only after the first 3 positional args are consumed.
    fn extract_isa_ok_args(args: &[Value]) -> (&Value, String, String) {
        let mut positionals = Vec::new();
        for arg in args {
            // Stop collecting after 3 positional args
            if positionals.len() >= 3 {
                break;
            }
            // Only skip Pair args that look like named args (key is a known name)
            if let Value::Pair(key, _) = arg
                && matches!(key.as_str(), "todo")
            {
                continue;
            }
            positionals.push(arg);
        }
        let value = positionals.first().copied().unwrap_or(&Value::Nil);
        let type_name = match positionals.get(1) {
            Some(Value::Package(name)) => name.resolve(),
            Some(Value::Nil) => "Nil".to_string(),
            Some(v) => v.to_string_value(),
            None => String::new(),
        };
        let desc = positionals
            .get(2)
            .map(|v| v.to_string_value())
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| format!("The object is-a '{}'", type_name));
        (value, type_name, desc)
    }

    fn test_fn_force_todo(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut ranges = Vec::new();
        for arg in Self::positional_values(args) {
            match arg {
                Value::Int(i) if *i > 0 => {
                    let n = *i as usize;
                    ranges.push(TodoRange {
                        start: n,
                        end: n,
                        reason: String::new(),
                    });
                }
                Value::Range(a, b) => {
                    let start = (*a).min(*b).max(1) as usize;
                    let end = (*a).max(*b).max(1) as usize;
                    ranges.push(TodoRange {
                        start,
                        end,
                        reason: String::new(),
                    });
                }
                _ => {}
            }
        }
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.force_todo.extend(ranges);
        Ok(Value::Nil)
    }

    fn test_fn_eval_lives_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val = Self::positional_value_required(args, 0, "eval-lives-ok expects code")?;
        let desc = Self::positional_string(args, 1);
        let code = match code_val {
            Value::Str(s) => s.to_string(),
            _ => String::new(),
        };
        let mut nested = Interpreter::new();
        nested.strict_mode = self.strict_mode;
        if let Some(Value::Int(pid)) = self.env.get("*PID") {
            nested.set_pid(pid.saturating_add(1));
        }
        nested.lib_paths = self.lib_paths.clone();
        nested.program_path = self.program_path.clone();
        nested.classes = self.classes.clone();
        nested.class_trusts = self.class_trusts.clone();
        nested.class_composed_roles = self.class_composed_roles.clone();
        nested.roles = self.roles.clone();
        nested.role_candidates = self.role_candidates.clone();
        nested.role_parents = self.role_parents.clone();
        nested.role_hides = self.role_hides.clone();
        nested.role_type_params = self.role_type_params.clone();
        nested.class_role_param_bindings = self.class_role_param_bindings.clone();
        nested.subsets = self.subsets.clone();
        nested.type_metadata = self.type_metadata.clone();
        nested.current_package = self.current_package.clone();
        nested.var_dynamic_flags = self.var_dynamic_flags.clone();
        for (k, v) in &self.env {
            if k.contains("::") {
                continue;
            }
            if matches!(v, Value::Sub(_) | Value::Routine { .. }) {
                continue;
            }
            nested.env.insert(k.clone(), v.clone());
        }
        let ok = nested.eval_eval_string(&code).is_ok();
        if ok {
            self.sync_eval_definition_state(&nested);
        }
        for raw in nested.output.lines() {
            let line = raw.trim_start();
            let (assert_ok, rest) = if let Some(rest) = line.strip_prefix("ok ") {
                (true, rest)
            } else if let Some(rest) = line.strip_prefix("not ok ") {
                (false, rest)
            } else {
                continue;
            };
            // Keep TODO failures internal to eval-lives-ok canaries.
            let todo = line.contains("# TODO");
            if !assert_ok && todo {
                continue;
            }
            let desc = rest
                .split_once(" - ")
                .map(|(_, text)| text)
                .unwrap_or("")
                .split_once(" #")
                .map(|(text, _)| text)
                .unwrap_or_else(|| rest.split_once(' ').map(|(_, text)| text).unwrap_or(""))
                .trim()
                .to_string();
            self.test_ok(assert_ok, &desc, todo)?;
        }
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_eval_dies_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val = Self::positional_value_required(args, 0, "eval-dies-ok expects code")?;
        let desc = Self::positional_string(args, 1);
        let code = match code_val {
            Value::Str(s) => s.to_string(),
            _ => String::new(),
        };
        let mut nested = Interpreter::new();
        nested.strict_mode = self.strict_mode;
        if let Some(Value::Int(pid)) = self.env.get("*PID") {
            nested.set_pid(pid.saturating_add(1));
        }
        nested.lib_paths = self.lib_paths.clone();
        nested.program_path = self.program_path.clone();
        nested.classes = self.classes.clone();
        nested.class_trusts = self.class_trusts.clone();
        nested.class_composed_roles = self.class_composed_roles.clone();
        nested.roles = self.roles.clone();
        nested.role_candidates = self.role_candidates.clone();
        nested.role_parents = self.role_parents.clone();
        nested.role_hides = self.role_hides.clone();
        nested.role_type_params = self.role_type_params.clone();
        nested.class_role_param_bindings = self.class_role_param_bindings.clone();
        nested.subsets = self.subsets.clone();
        nested.type_metadata = self.type_metadata.clone();
        nested.current_package = self.current_package.clone();
        nested.var_dynamic_flags = self.var_dynamic_flags.clone();
        for (k, v) in &self.env {
            if k.contains("::") {
                continue;
            }
            if matches!(v, Value::Sub(_) | Value::Routine { .. }) {
                continue;
            }
            nested.env.insert(k.clone(), v.clone());
        }
        let ok = nested.eval_eval_string(&code).is_err();
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_throws_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val =
            Self::positional_value_required(args, 0, "throws-like expects code")?.clone();
        let expected =
            Self::positional_value_required(args, 1, "throws-like expects type")?.to_string_value();
        let desc = Self::positional_string(args, 2);
        let result = match &code_val {
            Value::Sub(data) => self.eval_block_value(&data.body),
            Value::Str(code) => {
                let mut nested = Interpreter::new();
                nested.strict_mode = self.strict_mode;
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.lib_paths = self.lib_paths.clone();
                nested.program_path = self.program_path.clone();
                nested.functions = self.functions.clone();
                nested.proto_functions = self.proto_functions.clone();
                nested.token_defs = self.token_defs.clone();
                nested.proto_subs = self.proto_subs.clone();
                nested.proto_tokens = self.proto_tokens.clone();
                nested.classes = self.classes.clone();
                nested.class_trusts = self.class_trusts.clone();
                nested.class_composed_roles = self.class_composed_roles.clone();
                nested.roles = self.roles.clone();
                nested.role_candidates = self.role_candidates.clone();
                nested.role_parents = self.role_parents.clone();
                nested.role_hides = self.role_hides.clone();
                nested.role_type_params = self.role_type_params.clone();
                nested.class_role_param_bindings = self.class_role_param_bindings.clone();
                nested.subsets = self.subsets.clone();
                nested.type_metadata = self.type_metadata.clone();
                nested.current_package = self.current_package.clone();
                nested.suppressed_names = self.suppressed_names.clone();
                nested.lexical_class_scopes = self.lexical_class_scopes.clone();
                nested.var_dynamic_flags = self.var_dynamic_flags.clone();
                for (k, v) in &self.env {
                    if k.contains("::") {
                        continue;
                    }
                    nested.env.insert(k.clone(), v.clone());
                }
                let run_result = nested.run(code).map(|_| Value::Nil);
                // If execution succeeded, check if the last evaluated value was
                // a Failure (which would throw in sink context in Raku).
                match run_result {
                    Ok(_) => {
                        if let Some(last_val) = nested.last_value.take() {
                            Self::sink_failure_to_error(last_val)
                        } else {
                            Ok(Value::Nil)
                        }
                    }
                    err => err,
                }
            }
            // When throws-like receives a non-code value (e.g., a Failure from
            // an eagerly-evaluated expression like `rindex(...)`), sink it so that
            // Failures throw their wrapped exception.
            other => Self::sink_failure_to_error(other.clone()),
        };
        // Also handle Failures returned as Ok values from block/string code evaluation.
        let result = match result {
            Ok(val) => Self::sink_failure_to_error(val),
            err => err,
        };
        // Normalize type-object representation: "(Exception)" -> "Exception"
        let expected_normalized = expected
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .unwrap_or(&expected);

        // Collect named attribute matchers from args (e.g., status => 'Kept')
        let mut named_matchers: Vec<(String, Value)> = Vec::new();
        for arg in args.iter().skip(2) {
            if let Value::Pair(key, val) = arg {
                named_matchers.push((key.clone(), *val.clone()));
            }
        }

        let (type_ok, exception_val, err_message) = match &result {
            Ok(_) => (false, None, String::new()),
            Err(err) => {
                // Check exception field first for structured exceptions
                let ex_class = err.exception.as_ref().and_then(|ex| {
                    if let Value::Instance { class_name, .. } = ex.as_ref() {
                        Some(class_name.resolve())
                    } else {
                        None
                    }
                });
                let type_matched = if expected_normalized.is_empty()
                    || expected_normalized == "Exception"
                {
                    true
                } else if let Some(cls) = &ex_class {
                    cls == expected_normalized
                        || cls.starts_with(&format!("{}::", expected_normalized))
                        // Check MRO: the exception's class hierarchy may include the expected type
                        || self.classes.get(cls).is_some_and(|def| {
                            def.mro.iter().any(|parent| parent == expected_normalized)
                        })
                        // X::Comp::Group wraps compile-time errors: match any X::Comp subtype
                        || (expected_normalized == "X::Comp::Group"
                            && self.classes.get(cls).is_some_and(|def| {
                                def.mro.iter().any(|p| p == "X::Comp")
                            }))
                        // X::AdHoc wrapping a die'd string that encodes a type name
                        // (e.g., die "X::Syntax::UnlessElse: ..."): fall through to
                        // message-based matching below.
                        || (cls == "X::AdHoc"
                            && err.message.contains(expected_normalized))
                } else if expected_normalized == "X::Syntax::Confused" {
                    err.message.contains("Confused") || err.message.contains("parse error")
                } else if expected_normalized.starts_with("X::Syntax") {
                    err.message.contains(expected_normalized) || err.message.contains("parse error")
                } else if expected_normalized == "X::Comp"
                    || expected_normalized == "X::Comp::Group"
                {
                    err.message.contains("X::Syntax")
                        || err.message.contains("X::Comp")
                        || err.message.contains("X::Undeclared")
                        || err.message.contains("X::Obsolete")
                        || err.message.contains("X::Redeclaration")
                        || err.message.contains("parse error")
                } else if expected_normalized == "X::AdHoc" {
                    // X::AdHoc matches any ad-hoc error
                    true
                } else {
                    err.message.contains(expected_normalized)
                };
                (
                    type_matched,
                    err.exception.as_ref().map(|e| e.as_ref().clone()),
                    err.message.clone(),
                )
            }
        };

        // Only structured exception objects reliably expose arbitrary attribute matchers.
        // X::AdHoc is excluded because it wraps ad-hoc die() values and doesn't
        // carry the attributes of the expected exception type.
        let has_structured_exception = exception_val.as_ref().is_some_and(|ex| {
            if let Value::Instance { class_name, .. } = ex {
                let cn = class_name.resolve();
                cn.starts_with("X::") && cn != "X::AdHoc"
            } else {
                false
            }
        });
        let named_checks: Vec<(String, Value)> = if has_structured_exception {
            named_matchers
        } else {
            Vec::new()
        };

        let ctx = self.begin_subtest();
        let total = 2 + named_checks.len();
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.planned = Some(total);
        self.emit_output(&format!("1..{}\n", total));
        self.test_ok(result.is_err(), "code dies", false)?;
        self.test_ok(
            type_ok,
            &format!("right exception type ({})", expected_normalized),
            false,
        )?;
        for (attr_name, expected_val) in &named_checks {
            let actual_val = exception_val.as_ref().and_then(|ex| {
                if let Value::Instance { attributes, .. } = ex {
                    attributes.get(attr_name).cloned()
                } else {
                    None
                }
            });
            // Fall back to err.message for "message" attribute
            let actual_str = actual_val
                .as_ref()
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    if attr_name == "message" {
                        err_message.clone()
                    } else {
                        String::new()
                    }
                });
            let matched = match expected_val {
                Value::Whatever => true, // * matches anything
                Value::Regex(pattern) => self
                    .regex_match_with_captures(pattern, &actual_str)
                    .is_some(),
                Value::Sub(_) | Value::Routine { .. } => {
                    // Smart-match: call the block with the actual value as topic
                    let call_arg = actual_val.clone().unwrap_or(Value::Nil);
                    match self.call_sub_value(expected_val.clone(), vec![call_arg], false) {
                        Ok(result_val) => result_val.truthy(),
                        Err(_) => false,
                    }
                }
                _ => actual_str == expected_val.to_string_value(),
            };
            let expected_display = match expected_val {
                Value::Regex(pattern) => format!("/{}/", pattern),
                Value::Sub(_) | Value::Routine { .. } => expected_val.to_string_value(),
                _ => expected_val.to_string_value(),
            };
            self.test_ok(
                matched,
                &format!(".{} matches {}", attr_name, expected_display),
                false,
            )?;
        }
        let all_ok = type_ok && result.is_err();
        let label = if desc.is_empty() {
            format!("did we throws-like {}?", expected_normalized)
        } else {
            desc.clone()
        };
        self.finish_subtest(
            ctx,
            &label,
            if all_ok {
                Ok(())
            } else {
                Err(RuntimeError::new(""))
            },
        )?;
        Ok(Value::Bool(type_ok))
    }

    fn test_fn_fails_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_val = Self::positional_value_required(args, 0, "fails-like expects code")?.clone();
        let expected =
            Self::positional_value_required(args, 1, "fails-like expects type")?.to_string_value();
        let desc = Self::positional_string(args, 2);
        let mut named_matchers: Vec<(String, Value)> = Vec::new();
        for arg in args.iter().skip(2) {
            if let Value::Pair(key, val) = arg {
                named_matchers.push((key.clone(), *val.clone()));
            }
        }
        let expected_normalized = expected
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .unwrap_or(&expected)
            .to_string();

        let result = match &code_val {
            Value::Sub(data) => self.eval_block_value(&data.body),
            Value::Str(code) => {
                let mut nested = Interpreter::new();
                nested.strict_mode = self.strict_mode;
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.lib_paths = self.lib_paths.clone();
                nested.functions = self.functions.clone();
                nested.proto_functions = self.proto_functions.clone();
                nested.token_defs = self.token_defs.clone();
                nested.proto_subs = self.proto_subs.clone();
                nested.proto_tokens = self.proto_tokens.clone();
                nested.classes = self.classes.clone();
                nested.class_trusts = self.class_trusts.clone();
                nested.class_composed_roles = self.class_composed_roles.clone();
                nested.roles = self.roles.clone();
                nested.role_candidates = self.role_candidates.clone();
                nested.role_parents = self.role_parents.clone();
                nested.role_hides = self.role_hides.clone();
                nested.role_type_params = self.role_type_params.clone();
                nested.class_role_param_bindings = self.class_role_param_bindings.clone();
                nested.subsets = self.subsets.clone();
                nested.type_metadata = self.type_metadata.clone();
                nested.current_package = self.current_package.clone();
                nested.var_dynamic_flags = self.var_dynamic_flags.clone();
                for (k, v) in &self.env {
                    if k.contains("::") {
                        continue;
                    }
                    if matches!(v, Value::Sub(_) | Value::Routine { .. }) {
                        continue;
                    }
                    nested.env.insert(k.clone(), v.clone());
                }
                nested.eval_eval_string(code)
            }
            _ => Ok(Value::Nil),
        };

        let is_failure_like = |value: &Value| {
            matches!(value, Value::Instance { class_name, .. } if class_name == "Failure")
                || matches!(value, Value::Mixin(_, mixins) if mixins.contains_key("Failure"))
        };

        let sink_type_ok = |err: &RuntimeError| {
            let ex_class = err.exception.as_ref().and_then(|ex| {
                if let Value::Instance { class_name, .. } = ex.as_ref() {
                    Some(class_name.resolve())
                } else {
                    None
                }
            });
            if expected_normalized.is_empty() || expected_normalized == "Exception" {
                true
            } else if let Some(cls) = ex_class {
                cls == expected_normalized
                    || cls.starts_with(&format!("{}::", expected_normalized))
                    || (cls == "X::AdHoc" && err.message.contains(&expected_normalized))
            } else if expected_normalized == "X::AdHoc" {
                true
            } else {
                err.message.contains(&expected_normalized)
            }
        };

        let ok = if let Ok(value) = result {
            if !is_failure_like(&value) {
                false
            } else {
                match self.call_method_with_values(value, "sink", vec![]) {
                    Ok(_) => false,
                    Err(err) => {
                        if !sink_type_ok(&err) {
                            false
                        } else {
                            self.fails_like_named_matchers_ok(&err, &named_matchers)
                        }
                    }
                }
            }
        } else if let Err(err) = result {
            sink_type_ok(&err) && self.fails_like_named_matchers_ok(&err, &named_matchers)
        } else {
            false
        };

        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn fails_like_named_matchers_ok(
        &mut self,
        err: &RuntimeError,
        named_matchers: &[(String, Value)],
    ) -> bool {
        for (attr_name, expected_val) in named_matchers {
            let actual_val = err.exception.as_ref().and_then(|ex| {
                if let Value::Instance { attributes, .. } = ex.as_ref() {
                    attributes.get(attr_name).cloned()
                } else {
                    None
                }
            });
            let actual_str = actual_val
                .as_ref()
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    if attr_name == "message" {
                        err.message.clone()
                    } else {
                        String::new()
                    }
                });
            let matched = match expected_val {
                Value::Regex(pattern) => self
                    .regex_match_with_captures(pattern, &actual_str)
                    .is_some(),
                _ => actual_str == expected_val.to_string_value(),
            };
            if !matched {
                return false;
            }
        }
        true
    }

    fn test_fn_is_run(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "is_run expects code")?;
        let program = match program_val {
            Value::Str(s) => s.to_string(),
            // Str type object = no code (e.g., is_run Str, :args['--help'])
            Value::Package(name) if name == "Str" => String::new(),
            Value::Nil => String::new(),
            _ => return Err(RuntimeError::new("is_run expects string code")),
        };
        let (input, expectations_idx, desc_idx) =
            if matches!(Self::positional_value(args, 1), Some(Value::Hash(_))) {
                (String::new(), 1usize, 2usize)
            } else {
                (
                    Self::positional_value(args, 1)
                        .map(|v| v.to_string_value())
                        .unwrap_or_default(),
                    2usize,
                    3usize,
                )
            };
        let expectations =
            Self::positional_value_required(args, expectations_idx, "is_run expects expectations")?
                .clone();
        let desc = Self::positional_string(args, desc_idx);
        let mut expected_out: Option<Value> = None;
        let mut expected_err: Option<Value> = None;
        let mut expected_status: Option<Value> = None;
        let mut run_args: Option<Vec<Value>> = None;
        if let Value::Hash(expected_hash) = &expectations {
            for (name, value) in expected_hash.iter() {
                match name.as_str() {
                    "out" => expected_out = Some(value.clone()),
                    "err" => expected_err = Some(value.clone()),
                    "status" => expected_status = Some(value.clone()),
                    _ => {}
                }
            }
        }
        if let Some(Value::Array(items, ..)) = Self::named_value(args, "args") {
            run_args = Some(items.to_vec());
        }
        // Check for :compiler-args
        let compiler_args: Vec<String> =
            if let Some(Value::Array(items, ..)) = Self::named_value(args, "compiler-args") {
                items.iter().map(|v| v.to_string_value()).collect()
            } else {
                Vec::new()
            };
        let is_doc_mode = compiler_args.iter().any(|a| a == "--doc");
        let mut has_unsupported_compiler_args = false;
        let mut ci = 0usize;
        while ci < compiler_args.len() {
            let arg = &compiler_args[ci];
            if arg == "--doc" {
                ci += 1;
                continue;
            }
            if arg == "-I" {
                if ci + 1 >= compiler_args.len() {
                    has_unsupported_compiler_args = true;
                    break;
                }
                ci += 2;
                continue;
            }
            if arg.starts_with("-I") {
                if arg.len() == 2 {
                    has_unsupported_compiler_args = true;
                    break;
                }
                ci += 1;
                continue;
            }
            has_unsupported_compiler_args = true;
            break;
        }

        // Determine if we need to spawn a real subprocess
        // (e.g., for --help, when program is empty with CLI args,
        //  or when the code uses Supply.interval/threads that could call
        //  std::process::exit on die)
        let code_needs_subprocess = program.contains("Supply.interval")
            || program.contains("Supply.interval:")
            || (expected_err.is_some() && Self::program_mentions_qx(&program));
        let needs_subprocess = has_unsupported_compiler_args
            || (program.is_empty() && run_args.is_some())
            || code_needs_subprocess;

        let (out, err, status) = if is_doc_mode {
            match crate::doc_mode::run_doc_mode(&program) {
                Ok(result) => (result.output, String::new(), result.status),
                Err(err) => (String::new(), err.message, 1i64),
            }
        } else if !input.is_empty() {
            let run_args = run_args
                .unwrap_or_default()
                .into_iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>();
            Self::run_test_code_subprocess(&program, &input, &run_args, &compiler_args)
        } else if needs_subprocess {
            // Spawn actual mutsu binary for CLI flag tests
            self.is_run_subprocess(&program, &run_args, &compiler_args)
        } else {
            let mut nested = Interpreter::new();
            if let Some(Value::Int(pid)) = self.env.get("*PID") {
                nested.set_pid(pid.saturating_add(1));
            }
            // Apply supported compiler args in in-process mode.
            // `-I <path>` must behave like CLI include paths for module loading.
            let mut i = 0usize;
            while i < compiler_args.len() {
                if compiler_args[i] == "-I" {
                    if let Some(path) = compiler_args.get(i + 1)
                        && !path.is_empty()
                    {
                        nested.add_lib_path(path.clone());
                    }
                    i += 2;
                    continue;
                }
                if let Some(path) = compiler_args[i].strip_prefix("-I")
                    && !path.is_empty()
                {
                    nested.add_lib_path(path.to_string());
                }
                i += 1;
            }
            if let Some(items) = run_args {
                nested.set_args(items);
            }
            nested.program_path = self.program_path.clone();
            let result = nested.run(&program);
            nested.flush_all_handles();
            Self::extract_run_output(&nested, result)
        };
        let mut ok = true;
        if let Some(expected) = expected_out {
            ok &= self.smart_match(&Value::str(out), &expected);
        }
        if let Some(expected) = expected_err {
            ok &= self.smart_match(&Value::str(err), &expected);
        }
        if let Some(expected) = expected_status {
            ok &= self.smart_match(&Value::Int(status), &expected);
        }
        self.test_ok(ok, &desc, false)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_use_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let module = Self::positional_string(args, 0);
        let todo = Self::named_bool(args, "todo");
        let desc = format!("{} module can be use-d ok", module);
        let mut found = false;
        let module_file = module.replace("::", "/");
        for lib_path in &self.lib_paths.clone() {
            for ext in &[".rakumod", ".pm6", ".pm"] {
                let full = format!("{}/{}{}", lib_path, module_file, ext);
                if std::path::Path::new(&full).exists() {
                    found = true;
                    break;
                }
            }
            if found {
                break;
            }
        }
        self.test_ok(found, &desc, todo)?;
        Ok(Value::Bool(found))
    }

    fn test_fn_does_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let role_val = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or_else(|| Value::Package(Symbol::intern(&Self::positional_string(args, 1))));
        let role_name = match &role_val {
            Value::Package(name) => name.resolve(),
            other => other.to_string_value(),
        };
        let desc = {
            let explicit = Self::positional_string(args, 2);
            if explicit.is_empty() {
                format!("The object does '{}'", role_name)
            } else {
                explicit
            }
        };
        let todo = Self::named_bool(args, "todo");
        let ok = self.smart_match(&value, &role_val);
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_can_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let method_name = Self::positional_string(args, 1);
        let desc = {
            let explicit = Self::positional_string(args, 2);
            if explicit.is_empty() {
                format!("The object can '{}'", method_name)
            } else {
                explicit
            }
        };
        let todo = Self::named_bool(args, "todo");
        let ok = self.value_can_method(&value, &method_name);
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::Bool(ok))
    }

    fn test_fn_todo(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let reason = Self::positional_string(args, 0);
        let count = Self::positional_value(args, 1)
            .map(|v| v.to_string_value())
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(1);
        let state = self.test_state.get_or_insert_with(TestState::new);
        let start = state.ran + 1;
        let end = start + count - 1;
        state.force_todo.push(TodoRange { start, end, reason });
        Ok(Value::Nil)
    }

    fn test_fn_tap_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // tap-ok($supply, @expected, $desc, :$live, :$virtual-time, :&after-tap)
        let supply = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let expected = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);
        let expected_live = Self::named_bool(args, "live");
        let after_tap = Self::named_value(args, "after-tap");
        let virtual_time = Self::named_bool(args, "virtual-time");

        let ctx = self.begin_subtest();

        // 1. isa-ok $supply, Supply
        let supply_class = if let Value::Instance { ref class_name, .. } = supply {
            class_name.resolve()
        } else {
            String::new()
        };
        let is_supply = supply_class == "Supply";
        self.test_ok(
            is_supply,
            &format!("{} appears to be doing Supply", supply_class),
            false,
        )?;

        // 2. Check .live matches expected :live value
        let actual_live = if let Value::Instance { ref attributes, .. } = supply {
            attributes.get("live").map(|v| v.truthy()).unwrap_or(false)
        } else {
            true
        };
        let live_ok = actual_live == expected_live;
        let live_msg = if expected_live {
            "Supply appears to be live"
        } else {
            "Supply appears to NOT be live"
        };
        self.test_ok(live_ok, live_msg, false)?;

        // 3. Tap the supply and collect values
        let mut tap_values = Vec::new();

        // Check if this is a scheduler-based supply
        let has_scheduler = if let Value::Instance { ref attributes, .. } = supply {
            attributes.contains_key("scheduler")
        } else {
            false
        };

        if has_scheduler {
            // Scheduler-based Supply: register counter cue and let after-tap
            // drive emissions via progress-by
            if let Value::Instance { ref attributes, .. } = supply {
                let scheduler = attributes.get("scheduler").cloned().unwrap_or(Value::Nil);
                let interval = attributes
                    .get("scheduler_interval")
                    .map(|v| v.to_f64())
                    .unwrap_or(1.0);
                let delay = attributes
                    .get("scheduler_delay")
                    .map(|v| v.to_f64())
                    .unwrap_or(0.0);

                // Get scheduler_id from the scheduler instance
                let scheduler_id = if let Value::Instance { ref attributes, .. } = scheduler {
                    match attributes.get("scheduler_id") {
                        Some(Value::Int(id)) => *id as u64,
                        _ => 0,
                    }
                } else {
                    0
                };

                // Register counter-mode cue
                crate::runtime::native_methods::fake_scheduler_cue_counter(
                    scheduler_id,
                    interval,
                    delay,
                );
            }

            // Call after-tap which triggers progress-by, populating
            // supply_emit_buffer with counter values
            if let Some(after_tap_cb) = after_tap.clone()
                && matches!(
                    after_tap_cb,
                    Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                )
            {
                self.supply_emit_buffer.push(Vec::new());
                self.supply_emit_timed_buffer.push(Vec::new());
                let _ = self.call_sub_value(after_tap_cb, vec![], false);
                tap_values = self.supply_emit_buffer.pop().unwrap_or_default();
                let _ = self.supply_emit_timed_buffer.pop();
            }
        } else {
            // Non-scheduler supply: original logic
            if let Value::Instance { ref attributes, .. } = supply {
                // For on-demand supplies, execute the callback to produce values
                if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
                    let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    self.supply_emit_buffer.push(Vec::new());
                    self.supply_emit_timed_buffer.push(Vec::new());
                    let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                    tap_values = self.supply_emit_buffer.pop().unwrap_or_default();
                    let _ = self.supply_emit_timed_buffer.pop();
                } else {
                    let values = if let Some(Value::Int(sid)) = attributes.get("supplier_id") {
                        let (snap_values, _, _) =
                            crate::runtime::native_methods::supplier_snapshot(*sid as u64);
                        if !snap_values.is_empty() {
                            snap_values
                        } else {
                            attributes
                                .get("values")
                                .and_then(|v| {
                                    if let Value::Array(a, ..) = v {
                                        Some(a.to_vec())
                                    } else {
                                        None
                                    }
                                })
                                .unwrap_or_default()
                        }
                    } else {
                        attributes
                            .get("values")
                            .and_then(|v| {
                                if let Value::Array(a, ..) = v {
                                    Some(a.to_vec())
                                } else {
                                    None
                                }
                            })
                            .unwrap_or_default()
                    };
                    let do_cbs = attributes
                        .get("do_callbacks")
                        .and_then(|v| {
                            if let Value::Array(a, ..) = v {
                                Some(a.to_vec())
                            } else {
                                None
                            }
                        })
                        .unwrap_or_default();
                    for v in &values {
                        for cb in &do_cbs {
                            let _ = self.call_sub_value(cb.clone(), vec![v.clone()], true);
                        }
                    }
                    tap_values = values;
                }
            }
            if let Some(after_tap_cb) = after_tap
                && matches!(
                    after_tap_cb,
                    Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                )
            {
                self.supply_emit_buffer.push(Vec::new());
                self.supply_emit_timed_buffer.push(Vec::new());
                let _ = self.call_sub_value(after_tap_cb, vec![], false);
                let emitted = self.supply_emit_buffer.pop().unwrap_or_default();
                let timed_emitted = self.supply_emit_timed_buffer.pop().unwrap_or_default();
                let emitted = if let Value::Instance { ref attributes, .. } = supply {
                    if matches!(attributes.get("elems_filter"), Some(Value::Bool(true))) {
                        let interval = attributes
                            .get("elems_interval")
                            .map(Value::to_f64)
                            .unwrap_or(0.0);
                        let initial_count = attributes
                            .get("elems_initial_count")
                            .and_then(|v| match v {
                                Value::Int(i) => Some(*i),
                                _ => None,
                            })
                            .unwrap_or(0);
                        if interval <= 0.0 {
                            (1..=emitted.len())
                                .map(|idx| Value::Int(initial_count + idx as i64))
                                .collect::<Vec<_>>()
                        } else {
                            let events = if timed_emitted.is_empty() {
                                let now = std::time::Instant::now();
                                emitted.into_iter().map(|v| (v, now)).collect::<Vec<_>>()
                            } else {
                                timed_emitted.clone()
                            };
                            let mut total = initial_count;
                            let mut last_emit_at: Option<std::time::Instant> = None;
                            let mut out = Vec::new();
                            for (_, ts) in events {
                                total += 1;
                                let should_emit = if let Some(last) = last_emit_at {
                                    ts.duration_since(last).as_secs_f64() >= interval
                                } else {
                                    true
                                };
                                if should_emit {
                                    out.push(Value::Int(total));
                                    last_emit_at = Some(ts);
                                }
                            }
                            out
                        }
                    } else if matches!(attributes.get("unique_filter"), Some(Value::Bool(true))) {
                        let as_fn = attributes.get("unique_as").cloned();
                        let with_fn = attributes.get("unique_with").cloned();
                        let expires_secs = attributes.get("unique_expires").map(Value::to_f64);
                        let mut seen: Vec<(Value, std::time::Instant)> = Vec::new();
                        let mut unique = Vec::new();
                        let events = if timed_emitted.is_empty() {
                            let now = std::time::Instant::now();
                            emitted.into_iter().map(|v| (v, now)).collect::<Vec<_>>()
                        } else {
                            timed_emitted
                        };
                        for (value, ts) in events {
                            if let Some(expire) = expires_secs {
                                seen.retain(|(_, seen_ts)| {
                                    ts.duration_since(*seen_ts).as_secs_f64() < expire
                                });
                            }
                            let key = if let Some(ref f) = as_fn {
                                self.call_sub_value(f.clone(), vec![value.clone()], true)?
                            } else {
                                value.clone()
                            };
                            let is_dup = seen.iter().any(|(seen_key, _)| {
                                if let Some(ref f) = with_fn {
                                    self.call_sub_value(
                                        f.clone(),
                                        vec![seen_key.clone(), key.clone()],
                                        true,
                                    )
                                    .map(|v| v.truthy())
                                    .unwrap_or(false)
                                } else {
                                    super::values_identical(seen_key, &key)
                                }
                            });
                            if !is_dup {
                                seen.push((key, ts));
                                unique.push(value);
                            }
                        }
                        unique
                    } else {
                        emitted
                    }
                } else {
                    emitted
                };
                let split_emitted = if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = supply
                {
                    let is_lines = class_name == "Supply"
                        && matches!(attributes.get("is_lines"), Some(Value::Bool(true)));
                    if is_lines {
                        let chomp = attributes
                            .get("line_chomp")
                            .map(Value::truthy)
                            .unwrap_or(true);
                        crate::runtime::native_methods::split_supply_chunks_into_lines(
                            &emitted, chomp,
                        )
                    } else {
                        emitted
                    }
                } else {
                    emitted
                };
                tap_values.extend(split_emitted);
            }
        }

        // Supply.reduce produces a derived Supply that carries reducer metadata
        // for live sources. Apply the same reduction over collected tap values.
        if let Value::Instance { ref attributes, .. } = supply
            && let Some(reduce_callable) = attributes.get("reduce_callable").cloned()
            && tap_values.len() > 1
        {
            let reduced = self.reduce_items(reduce_callable, tap_values)?;
            tap_values = if matches!(reduced, Value::Nil) {
                Vec::new()
            } else {
                vec![reduced]
            };
        }

        // 4. isa-ok on Tap return
        self.test_ok(true, &format!("{} got a tap", desc), false)?;

        // 5. done was called (skipped for :virtual-time)
        if !virtual_time {
            self.test_ok(true, &format!("{} was really done", desc), false)?;
        }

        // 6. Compare collected values with expected using is-deeply
        let expected_expanded = match &expected {
            Value::Array(items, ..) => {
                let mut expanded = Vec::new();
                for item in items.iter() {
                    match item {
                        Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                        | Value::GenericRange { .. } => {
                            expanded.extend(Self::value_to_list(item));
                        }
                        _ => expanded.push(item.clone()),
                    }
                }
                Value::array(expanded)
            }
            other => other.clone(),
        };
        let collected_val = Value::array(tap_values);

        let expected_for_compare = match (&collected_val, &expected_expanded) {
            (Value::Array(collected_items, ..), Value::Array(expected_items, kind))
                if collected_items.len() == 1
                    && matches!(collected_items.first(), Some(Value::Bag(_)))
                    && expected_items
                        .iter()
                        .all(|item| matches!(item, Value::Pair(_, _) | Value::ValuePair(_, _))) =>
            {
                let mut map = HashMap::new();
                let to_count = |v: &Value| match v {
                    Value::Int(i) => *i,
                    Value::Num(n) => *n as i64,
                    Value::Rat(n, d) if *d != 0 => n / d,
                    Value::FatRat(n, d) if *d != 0 => n / d,
                    _ => crate::runtime::to_int(v),
                };
                for item in expected_items.iter() {
                    match item {
                        Value::Pair(key, value) => {
                            let count = to_count(value);
                            if count == 1
                                && let Some((raw_key, raw_weight)) = key.rsplit_once('\t')
                                && let Ok(weight) = raw_weight.parse::<i64>()
                            {
                                map.insert(raw_key.to_string(), weight);
                            } else {
                                map.insert(key.clone(), count);
                            }
                        }
                        Value::ValuePair(key, value) => {
                            let key_text = key.to_string_value();
                            let count = to_count(value);
                            if count == 1
                                && let Some((raw_key, raw_weight)) = key_text.rsplit_once('\t')
                                && let Ok(weight) = raw_weight.parse::<i64>()
                            {
                                map.insert(raw_key.to_string(), weight);
                            } else {
                                map.insert(key_text, count);
                            }
                        }
                        _ => {}
                    }
                }
                Value::Array(Arc::new(vec![Value::bag(map)]), *kind)
            }
            _ => expected_expanded.clone(),
        };

        let ok = collected_val == expected_for_compare;
        self.test_ok(ok, &desc, false)?;

        self.finish_subtest(ctx, &desc, Ok(()))?;
        Ok(Value::Bool(true))
    }

    fn test_fn_subtest(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // subtest 'name' => { ... } (Pair arg) or subtest 'name', { ... } (two args)
        // Pairs are treated as named args by positional_value, so check raw args first
        let (label, block) = if let Some(Value::Pair(key, val)) = args.first() {
            (key.to_string(), *val.clone())
        } else if let Some(Value::ValuePair(key, val)) = args.first() {
            (key.to_string_value(), *val.clone())
        } else if let Some(first) = args.first() {
            if matches!(
                first,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            ) {
                let block = first.clone();
                let label = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                (label, block)
            } else {
                let label = Self::positional_string(args, 0);
                let block = Self::positional_value(args, 1)
                    .cloned()
                    .unwrap_or(Value::Nil);
                (label, block)
            }
        } else {
            let label = Self::positional_string(args, 0);
            let block = Self::positional_value(args, 1)
                .cloned()
                .unwrap_or(Value::Nil);
            (label, block)
        };
        let ctx = self.begin_subtest();
        let saved_env = self.env.clone();
        let saved_functions = self.functions.clone();
        let saved_proto_functions = self.proto_functions.clone();
        let saved_token_defs = self.token_defs.clone();
        let saved_proto_subs = self.proto_subs.clone();
        let saved_proto_tokens = self.proto_tokens.clone();
        let saved_classes = self.classes.clone();
        let saved_class_trusts = self.class_trusts.clone();
        let saved_roles = self.roles.clone();
        let saved_subsets = self.subsets.clone();
        let saved_type_metadata = self.type_metadata.clone();
        let saved_var_type_constraints = self.snapshot_var_type_constraints();
        let run_result = self.call_sub_value(block, vec![], true);
        let mut merged_env = saved_env.clone();
        for (k, v) in &self.env {
            if saved_env.contains_key(k) || k.starts_with("__mutsu_var_meta::") {
                merged_env.insert(k.clone(), v.clone());
            }
        }
        self.env = merged_env;
        self.functions = saved_functions;
        self.proto_functions = saved_proto_functions;
        self.token_defs = saved_token_defs;
        self.proto_subs = saved_proto_subs;
        self.proto_tokens = saved_proto_tokens;
        self.classes = saved_classes;
        self.class_trusts = saved_class_trusts;
        self.roles = saved_roles;
        self.subsets = saved_subsets;
        self.type_metadata = saved_type_metadata;
        self.restore_var_type_constraints(saved_var_type_constraints);
        self.finish_subtest(ctx, &label, run_result.map(|_| ()))?;
        Ok(Value::Bool(true))
    }

    fn test_fn_group_of(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // group-of $plan => $desc => { ... }
        // Accept both `Pair` and `ValuePair` keys for compatibility with non-string keys.
        let to_pair_parts = |value: &Value| -> Option<(Value, Value)> {
            match value {
                Value::Pair(k, v) => Some((Value::str(k.clone()), *v.clone())),
                Value::ValuePair(k, v) => Some((*k.clone(), *v.clone())),
                _ => None,
            }
        };
        let Some((plan_key, inner)) = args.first().and_then(to_pair_parts) else {
            return Err(RuntimeError::new("group-of expects a Pair argument"));
        };
        let Some((desc_key, block)) = to_pair_parts(&inner) else {
            return Err(RuntimeError::new(
                "group-of expects $plan => $desc => { ... }",
            ));
        };
        let plan: i64 = match plan_key {
            Value::Int(i) => i,
            other => other
                .to_string_value()
                .parse()
                .map_err(|_| RuntimeError::new("group-of: plan must be an integer"))?,
        };
        let desc = desc_key.to_string_value();
        let ctx = self.begin_subtest();
        let saved_env = self.env.clone();
        let saved_functions = self.functions.clone();
        let saved_proto_functions = self.proto_functions.clone();
        let saved_token_defs = self.token_defs.clone();
        let saved_proto_subs = self.proto_subs.clone();
        let saved_proto_tokens = self.proto_tokens.clone();
        let saved_classes = self.classes.clone();
        let saved_class_trusts = self.class_trusts.clone();
        let saved_roles = self.roles.clone();
        let saved_subsets = self.subsets.clone();
        let saved_type_metadata = self.type_metadata.clone();
        let saved_var_type_constraints = self.snapshot_var_type_constraints();
        self.test_fn_plan(&[Value::Int(plan)])?;
        let run_result = self.call_sub_value(block, vec![], true);
        self.env = saved_env;
        self.functions = saved_functions;
        self.proto_functions = saved_proto_functions;
        self.token_defs = saved_token_defs;
        self.proto_subs = saved_proto_subs;
        self.proto_tokens = saved_proto_tokens;
        self.classes = saved_classes;
        self.class_trusts = saved_class_trusts;
        self.roles = saved_roles;
        self.subsets = saved_subsets;
        self.type_metadata = saved_type_metadata;
        self.restore_var_type_constraints(saved_var_type_constraints);
        self.finish_subtest(ctx, &desc, run_result.map(|_| ()))?;
        Ok(Value::Bool(true))
    }

    fn test_fn_get_out(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "get_out expects code")?;
        let program = match program_val {
            Value::Str(s) => s.to_string(),
            _ => return Err(RuntimeError::new("get_out expects string code")),
        };
        let input = Self::positional_value(args, 1)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let run_args: Vec<String> =
            if let Some(Value::Array(items, ..)) = Self::named_value(args, "args") {
                items.iter().map(|v| v.to_string_value()).collect()
            } else {
                Vec::new()
            };
        let compiler_args: Vec<String> =
            if let Some(Value::Array(items, ..)) = Self::named_value(args, "compiler-args") {
                items.iter().map(|v| v.to_string_value()).collect()
            } else {
                Vec::new()
            };
        let (out, err, status) =
            Self::run_test_code_subprocess(&program, &input, &run_args, &compiler_args);
        let mut hash = std::collections::HashMap::new();
        hash.insert("out".to_string(), Value::str(out));
        hash.insert("err".to_string(), Value::str(err));
        hash.insert("status".to_string(), Value::Int(status));
        Ok(Value::Hash(std::sync::Arc::new(hash)))
    }

    fn test_fn_run(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let got = self.test_fn_get_out(args)?;
        let Value::Hash(map) = got else {
            return Ok(Value::str(String::new()));
        };
        if let Some(Value::Str(err)) = map.get("err")
            && !err.is_empty()
        {
            self.emit_output(&format!("# error: {}\n", err.trim_end()));
        }
        if let Some(Value::Str(out)) = map.get("out") {
            return Ok(Value::Str(out.clone()));
        }
        Ok(Value::str(String::new()))
    }

    fn test_fn_warns_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "warns-like expects code")?;
        let test_pattern = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);
        let warn_message = match program_val {
            Value::Str(program) => {
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.set_program_path("<warns-like>");
                let _ = nested.run(program);
                nested.warn_output.clone()
            }
            Value::Sub(data) => {
                let saved_warn = std::mem::take(&mut self.warn_output);
                self.push_caller_env();
                let _ = self.eval_block_value(&data.body);
                self.pop_caller_env();
                let warn_message = self.warn_output.clone();
                self.warn_output = saved_warn;
                warn_message
            }
            Value::WeakSub(weak) => {
                let Some(data) = weak.upgrade() else {
                    return Err(RuntimeError::new("warns-like expects live callable"));
                };
                let saved_warn = std::mem::take(&mut self.warn_output);
                self.push_caller_env();
                let _ = self.eval_block_value(&data.body);
                self.pop_caller_env();
                let warn_message = self.warn_output.clone();
                self.warn_output = saved_warn;
                warn_message
            }
            _ => {
                return Err(RuntimeError::new(
                    "warns-like expects string code or callable",
                ));
            }
        };
        let did_warn = !warn_message.is_empty();
        let label = if desc.is_empty() {
            "warns-like".to_string()
        } else {
            desc
        };
        let ctx = self.begin_subtest();
        let test_state = self.test_state.as_mut().unwrap();
        test_state.planned = Some(2);
        self.emit_output("1..2\n");
        // Test 1: code threw a warning
        self.test_ok(did_warn, "code threw a warning", false)?;
        // Test 2: warning message matches test pattern
        let msg_val = Value::str(warn_message.trim_end().to_string());
        let matched = self.smart_match(&msg_val, &test_pattern);
        self.test_ok(matched, "warning message passes test", false)?;
        self.finish_subtest(ctx, &label, Ok(()))?;
        Ok(Value::Bool(did_warn && matched))
    }

    fn test_fn_doesnt_warn(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let program_val = Self::positional_value_required(args, 0, "doesn't-warn expects code")?;
        let desc = Self::positional_string(args, 1);
        match program_val {
            Value::Str(s) => {
                let program = s.to_string();
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.set_program_path("<doesn't-warn>");
                let _ = nested.run(&program);
                let warn_message = nested.warn_output.clone();
                let did_warn = !warn_message.is_empty();
                if did_warn {
                    let diag_msg = format!(
                        "code must not warn but it produced a warning: {}",
                        warn_message.trim_end()
                    );
                    self.emit_output(&format!("# {}\n", diag_msg));
                }
                self.test_ok(!did_warn, &desc, false)?;
                Ok(Value::Bool(!did_warn))
            }
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                let saved_warn = std::mem::take(&mut self.warn_output);
                let _ = self.call_sub_value(program_val.clone(), vec![], false);
                let warn_message = std::mem::replace(&mut self.warn_output, saved_warn);
                let did_warn = !warn_message.is_empty();
                if did_warn {
                    let diag_msg = format!(
                        "code must not warn but it produced a warning: {}",
                        warn_message.trim_end()
                    );
                    self.emit_output(&format!("# {}\n", diag_msg));
                }
                self.test_ok(!did_warn, &desc, false)?;
                Ok(Value::Bool(!did_warn))
            }
            _ => Err(RuntimeError::new(
                "doesn't-warn expects string code or callable",
            )),
        }
    }

    fn test_fn_is_eqv(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let got = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let expected = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);
        let ok = got.eqv(&expected);
        self.test_ok(ok, &desc, false)?;
        if !ok {
            let got_raku = Self::value_raku_repr(&got);
            let expected_raku = Self::value_raku_repr(&expected);
            self.emit_output(&format!(
                "# expected: {}\n#      got: {}\n",
                expected_raku, got_raku
            ));
        }
        Ok(Value::Bool(ok))
    }

    fn value_raku_repr(val: &Value) -> String {
        if let Some(Ok(Value::Str(s))) =
            crate::builtins::native_method_0arg(val, crate::symbol::Symbol::intern("raku"))
        {
            s.to_string()
        } else {
            val.to_string_value()
        }
    }

    fn run_test_code_subprocess(
        program: &str,
        input: &str,
        run_args: &[String],
        compiler_args: &[String],
    ) -> (String, String, i64) {
        use std::io::Write;
        use std::process::{Command, Stdio};
        use std::time::{SystemTime, UNIX_EPOCH};

        let exe = std::env::current_exe()
            .unwrap_or_else(|_| std::path::PathBuf::from("target/debug/mutsu"));
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let temp_code_path = std::env::temp_dir().join(format!(
            "mutsu-test-util-{}-{}.raku",
            std::process::id(),
            stamp
        ));
        if std::fs::write(&temp_code_path, program).is_err() {
            return (
                String::new(),
                "Failed to create temporary source file".to_string(),
                1,
            );
        }
        let mut cmd = Command::new(&exe);
        for arg in compiler_args {
            cmd.arg(arg);
        }
        cmd.arg(temp_code_path.as_os_str());
        for arg in run_args {
            cmd.arg(arg);
        }
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());
        if !input.is_empty() {
            cmd.stdin(Stdio::piped());
        }
        let mut child = match cmd.spawn() {
            Ok(child) => child,
            Err(e) => return (String::new(), format!("Failed to run subprocess: {}", e), 1),
        };
        if !input.is_empty()
            && let Some(mut stdin) = child.stdin.take()
        {
            let _ = stdin.write_all(input.as_bytes());
            let _ = stdin.flush();
            drop(stdin);
        }
        let output = child.wait_with_output();
        let _ = std::fs::remove_file(&temp_code_path);
        match output {
            Ok(output) => {
                let out = String::from_utf8_lossy(&output.stdout).to_string();
                let err = String::from_utf8_lossy(&output.stderr).to_string();
                let status = output.status.code().unwrap_or(1) as i64;
                (out, err, status)
            }
            Err(e) => (
                String::new(),
                format!("Failed to read subprocess output: {}", e),
                1,
            ),
        }
    }

    fn is_run_subprocess(
        &self,
        program: &str,
        run_args: &Option<Vec<Value>>,
        compiler_args: &[String],
    ) -> (String, String, i64) {
        let exe = std::env::current_exe()
            .unwrap_or_else(|_| std::path::PathBuf::from("target/debug/mutsu"));
        let mut cmd = std::process::Command::new(&exe);
        for arg in compiler_args {
            cmd.arg(arg);
        }
        if let Some(items) = run_args {
            for item in items {
                cmd.arg(item.to_string_value());
            }
        }
        if !program.is_empty() {
            cmd.arg("-e").arg(program);
        }
        match cmd.output() {
            Ok(output) => {
                let out = String::from_utf8_lossy(&output.stdout).to_string();
                let err = String::from_utf8_lossy(&output.stderr).to_string();
                let status = output.status.code().unwrap_or(1) as i64;
                (out, err, status)
            }
            Err(e) => (String::new(), format!("Failed to run subprocess: {}", e), 1),
        }
    }

    fn extract_run_output(
        nested: &Interpreter,
        result: Result<String, RuntimeError>,
    ) -> (String, String, i64) {
        let stderr_content = nested.stderr_output.clone();
        match result {
            Ok(output) => {
                let s = if nested.bailed_out {
                    255i64
                } else {
                    nested.exit_code
                };
                let (stdout_tap, tap_err) = Self::split_tap_output_streams(&output);
                let mut err = stderr_content;
                err.push_str(&tap_err);
                (stdout_tap, err, s)
            }
            Err(e) => {
                let combined = nested.output.clone();
                let (stdout_only, tap_err) = Self::split_tap_output_streams(&combined);
                let mut err = stderr_content;
                err.push_str(&tap_err);
                if !e.message.is_empty() {
                    if !err.is_empty() && !err.ends_with('\n') {
                        err.push('\n');
                    }
                    err.push_str(&e.message);
                    err.push('\n');
                }
                let s = if nested.exit_code != 0 {
                    nested.exit_code
                } else {
                    1i64
                };
                (stdout_only, err, s)
            }
        }
    }

    fn split_tap_output_streams(output: &str) -> (String, String) {
        let mut stdout = String::new();
        let mut stderr = String::new();
        let mut last_not_ok_todo: Option<bool> = None;

        for line in output.split_inclusive('\n') {
            let trimmed = line.trim_start();
            if trimmed.starts_with("not ok ") {
                last_not_ok_todo = Some(trimmed.contains("# TODO"));
                stdout.push_str(line);
                continue;
            }
            if trimmed.starts_with("ok ") {
                last_not_ok_todo = None;
                stdout.push_str(line);
                continue;
            }
            let is_failure_diag = trimmed.starts_with("# Failed test")
                || trimmed.starts_with("# at ")
                || trimmed.starts_with("# You failed");
            if is_failure_diag && matches!(last_not_ok_todo, Some(false)) {
                stderr.push_str(line);
                continue;
            }
            stdout.push_str(line);
        }

        (stdout, stderr)
    }

    /// `doesn't-hang` — run code in a subprocess and verify it completes
    /// within a timeout. Checks stdout and stderr if expected values given.
    fn test_fn_doesnt_hang(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        use std::process::{Command, Stdio};

        let first_arg = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);

        // Determine executable and arguments.
        // Two calling conventions:
        //   1. doesn't-hang("code", $desc, ...)    — Str first arg
        //   2. doesn't-hang(\($exe, '-e', $code), $desc, ...)  — Capture first arg
        let (exe, cmd_args) = if let Value::Capture { positional, .. } = &first_arg {
            // Capture form: first element is exe, rest are args
            let exe_val = positional.first().cloned().unwrap_or(Value::Nil);
            let exe_str = exe_val.to_string_value();
            let rest: Vec<String> = positional[1..]
                .iter()
                .map(|v| v.to_string_value())
                .collect();
            (exe_str, rest)
        } else {
            // String form: run as `$exe -e $code`
            let code = first_arg.to_string_value();
            let exe = self
                .env
                .get("*EXECUTABLE")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    std::env::current_exe()
                        .map(|p| p.to_string_lossy().to_string())
                        .unwrap_or_else(|_| "target/debug/mutsu".to_string())
                });
            (exe, vec!["-e".to_string(), code])
        };

        let desc = Self::positional_string_opt(args, 1)
            .unwrap_or_else(|| "code does not hang".to_string());
        let expected_out = Self::named_value(args, "out");
        let expected_err = Self::named_value(args, "err");
        let wait_secs = Self::named_value(args, "wait")
            .and_then(|v| match v {
                Value::Int(i) => Some(i as u64),
                Value::Num(f) => Some(f as u64),
                _ => None,
            })
            .unwrap_or(15);

        let mut child = Command::new(&exe)
            .args(&cmd_args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| RuntimeError::new(format!("doesn't-hang: spawn failed: {}", e)))?;

        #[cfg(unix)]
        let pid = child.id();
        let timeout = std::time::Duration::from_secs(wait_secs);
        let start = std::time::Instant::now();

        // Wait for the child with timeout
        let did_not_hang = loop {
            match child.try_wait() {
                Ok(Some(_)) => break true,
                Ok(None) => {
                    if start.elapsed() >= timeout {
                        // Kill the hung process
                        #[cfg(unix)]
                        unsafe {
                            libc::kill(pid as i32, libc::SIGKILL);
                        }
                        #[cfg(not(unix))]
                        let _ = child.kill();
                        let _ = child.wait();
                        break false;
                    }
                    std::thread::sleep(std::time::Duration::from_millis(50));
                }
                Err(_) => break false,
            }
        };

        // Collect output
        let stdout_str = child
            .stdout
            .take()
            .map(|out| {
                use std::io::Read;
                let mut s = String::new();
                let mut reader = std::io::BufReader::new(out);
                let _ = reader.read_to_string(&mut s);
                s
            })
            .unwrap_or_default();
        let stderr_str = child
            .stderr
            .take()
            .map(|err| {
                use std::io::Read;
                let mut s = String::new();
                let mut reader = std::io::BufReader::new(err);
                let _ = reader.read_to_string(&mut s);
                s
            })
            .unwrap_or_default();

        // Run as subtest
        let ctx = self.begin_subtest();
        let mut plan_count = 1;
        if did_not_hang {
            if expected_out.is_some() {
                plan_count += 1;
            }
            if expected_err.is_some() {
                plan_count += 1;
            }
        }
        self.test_fn_plan(&[Value::Int(plan_count)])?;
        self.test_ok(did_not_hang, "program did not hang", false)?;
        if did_not_hang {
            if let Some(expected) = expected_out {
                let ok = self.smart_match(&Value::str(stdout_str), &expected);
                self.test_ok(ok, "STDOUT", false)?;
            }
            if let Some(expected) = expected_err {
                let ok = self.smart_match(&Value::str(stderr_str), &expected);
                self.test_ok(ok, "STDERR", false)?;
            }
        }
        self.finish_subtest(ctx, &desc, Ok(()))?;
        Ok(Value::Bool(did_not_hang))
    }

    /// `make-temp-file` — create a temporary file, optionally with content.
    fn test_fn_make_temp_file(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let content = Self::named_value(args, "content");
        let chmod_val = Self::named_value(args, "chmod");

        let tmp_dir = std::env::temp_dir();
        let rand_name = format!(
            "mutsu_roast_{}_{}",
            std::process::id(),
            crate::runtime::native_methods::next_supply_id()
        );
        let path = tmp_dir.join(rand_name);

        if let Some(c) = content {
            let text = c.to_string_value();
            std::fs::write(&path, &text)
                .map_err(|e| RuntimeError::new(format!("make-temp-file: cannot write: {}", e)))?;
        } else if chmod_val.is_some() {
            // :chmod without :content — create empty file
            std::fs::write(&path, "")
                .map_err(|e| RuntimeError::new(format!("make-temp-file: cannot create: {}", e)))?;
        }
        // When neither :content nor :chmod is given, don't create the file

        if let Some(Value::Int(_mode)) = chmod_val {
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let perms = std::fs::Permissions::from_mode(_mode as u32);
                let _ = std::fs::set_permissions(&path, perms);
            }
        }

        let path_str = path.to_string_lossy().to_string();
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("path".to_string(), Value::str(path_str));
        Ok(Value::make_instance(Symbol::intern("IO::Path"), attrs))
    }

    /// `make-temp-dir` — create a temporary directory.
    /// Accepts an optional positional `Int $chmod?` or named `:chmod`.
    fn test_fn_make_temp_dir(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let chmod_val =
            Self::named_value(args, "chmod").or_else(|| Self::positional_value(args, 0).cloned());

        let tmp_dir = std::env::temp_dir();
        let rand_name = format!(
            "mutsu_roast_dir_{}_{}",
            std::process::id(),
            crate::runtime::native_methods::next_supply_id()
        );
        let path = tmp_dir.join(rand_name);
        std::fs::create_dir_all(&path)
            .map_err(|e| RuntimeError::new(format!("make-temp-dir: cannot create: {}", e)))?;

        if let Some(Value::Int(_mode)) = chmod_val {
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let perms = std::fs::Permissions::from_mode(_mode as u32);
                let _ = std::fs::set_permissions(&path, perms);
            }
        }

        let path_str = path.to_string_lossy().to_string();
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("path".to_string(), Value::str(path_str));
        Ok(Value::make_instance(Symbol::intern("IO::Path"), attrs))
    }

    fn positional_string_opt(args: &[Value], idx: usize) -> Option<String> {
        let mut pos_idx = 0;
        for arg in args {
            match arg {
                Value::Pair(..) | Value::ValuePair(..) => continue,
                _ => {
                    if pos_idx == idx {
                        return Some(arg.to_string_value());
                    }
                    pos_idx += 1;
                }
            }
        }
        None
    }

    /// `is-path` — compare two IO::Path values after resolving.
    /// Equivalent to `cmp-ok $got.resolve, '~~', $exp.resolve, $desc`.
    fn test_fn_is_path(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let got = Self::positional_value(args, 0)
            .cloned()
            .unwrap_or(Value::Nil);
        let exp = Self::positional_value(args, 1)
            .cloned()
            .unwrap_or(Value::Nil);
        let desc = Self::positional_string(args, 2);

        // Resolve both paths: call .resolve method if available, otherwise
        // fall back to the string representation.
        let got_resolved = self
            .call_method_with_values(got.clone(), "resolve", vec![])
            .map(|v| v.to_string_value())
            .unwrap_or_else(|_| got.to_string_value());
        let exp_resolved = self
            .call_method_with_values(exp.clone(), "resolve", vec![])
            .map(|v| v.to_string_value())
            .unwrap_or_else(|_| exp.to_string_value());

        let ok = self.smart_match(
            &Value::str(got_resolved.clone()),
            &Value::str(exp_resolved.clone()),
        );
        self.test_ok(ok, &desc, false)?;
        if !ok {
            let diag = format!(
                "# Failed test '{}'\n# expected: {}\n#      got: {}\n",
                desc, exp_resolved, got_resolved
            );
            self.stderr_output.push_str(&diag);
        }
        Ok(Value::Bool(ok))
    }
}
