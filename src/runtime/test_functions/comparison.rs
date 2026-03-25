use super::super::*;

impl Interpreter {
    pub(crate) fn test_fn_cmp_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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
                "==" => super::super::to_float_value(&left) == super::super::to_float_value(&right),
                "!=" => super::super::to_float_value(&left) != super::super::to_float_value(&right),
                "<" => super::super::to_float_value(&left) < super::super::to_float_value(&right),
                "<=" => super::super::to_float_value(&left) <= super::super::to_float_value(&right),
                ">" => super::super::to_float_value(&left) > super::super::to_float_value(&right),
                ">=" => super::super::to_float_value(&left) >= super::super::to_float_value(&right),
                "===" => crate::runtime::utils::values_identical(&left, &right),
                "!===" => !crate::runtime::utils::values_identical(&left, &right),
                "eqv" => Self::cmp_eqv_bool(&left, &right),
                "=:=" => crate::runtime::utils::values_identical(&left, &right),
                "=~=" | "\u{2245}" => {
                    // =~= / ≅ approximately equal
                    let (lr, li) = match &left {
                        Value::Complex(r, i) => (*r, *i),
                        _ => (super::super::to_float_value(&left).unwrap_or(0.0), 0.0),
                    };
                    let (rr, ri) = match &right {
                        Value::Complex(r, i) => (*r, *i),
                        _ => (super::super::to_float_value(&right).unwrap_or(0.0), 0.0),
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

    pub(crate) fn test_fn_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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

    pub(crate) fn test_fn_unlike(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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

    pub(crate) fn test_fn_is_deeply(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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

    pub(crate) fn test_fn_is_deeply_junction(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
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

    pub(crate) fn junction_kind_name(kind: &JunctionKind) -> &'static str {
        match kind {
            JunctionKind::Any => "any",
            JunctionKind::All => "all",
            JunctionKind::One => "one",
            JunctionKind::None => "none",
        }
    }

    pub(crate) fn junction_sort_key(v: &Value) -> String {
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

    pub(crate) fn junction_guts_value(v: &Value) -> Option<Value> {
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

    /// Perform `eq` (string) comparison that threads through Junctions,
    /// returning a Value (possibly a Junction of Bools).
    /// Used by `is` which compares stringified values.
    pub(crate) fn eq_with_junctions(left: &Value, right: &Value) -> Value {
        if let Value::Junction { kind, values } = left {
            let results: Vec<Value> = values
                .iter()
                .map(|v| Self::eq_with_junctions(v, right))
                .collect();
            return Value::junction(kind.clone(), results);
        }
        if let Value::Junction { kind, values } = right {
            let results: Vec<Value> = values
                .iter()
                .map(|v| Self::eq_with_junctions(left, v))
                .collect();
            return Value::junction(kind.clone(), results);
        }
        Value::Bool(left.to_string_value() == right.to_string_value())
    }

    /// Perform `eqv` comparison that threads through Junctions,
    /// returning a Value (possibly a Junction of Bools).
    pub(crate) fn eqv_with_junctions(left: &Value, right: &Value) -> Value {
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

    pub(crate) fn test_fn_is_approx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let todo = Self::named_bool(args, "todo");
        let got = Self::positional_value_required(args, 0, "is-approx expects got")?;
        let expected = Self::positional_value_required(args, 1, "is-approx expects expected")?;

        // Detect three-arg form: is-approx(got, expected, numeric-tolerance, desc?)
        // If the third positional arg is a native numeric type (not a Str),
        // treat it as abs-tol.
        let third_pos = Self::positional_value(args, 2);
        let third_is_numeric = third_pos.as_ref().is_some_and(|v| {
            matches!(
                v,
                Value::Int(_)
                    | Value::Num(_)
                    | Value::Rat(_, _)
                    | Value::FatRat(_, _)
                    | Value::BigRat(_, _)
                    | Value::BigInt(_)
            )
        });
        let (positional_abs_tol, desc) = if third_is_numeric {
            let v = super::super::to_float_value(third_pos.as_ref().unwrap());
            (v, Self::positional_string(args, 3))
        } else {
            (None, Self::positional_string(args, 2))
        };

        let explicit_abs_tol = Self::named_value(args, "abs-tol")
            .and_then(|v| super::super::to_float_value(&v))
            .or(positional_abs_tol);
        let explicit_rel_tol =
            Self::named_value(args, "rel-tol").and_then(|v| super::super::to_float_value(&v));

        let mut coerce_float = |value: &Value| -> Option<f64> {
            if let Some(v) = super::super::to_float_value(value) {
                return Some(v);
            }
            if matches!(value, Value::Instance { .. }) {
                let coerced = self
                    .call_method_with_values(value.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(value.clone(), "Bridge", vec![]))
                    .ok()?;
                return super::super::to_float_value(&coerced);
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
                // DWIM: near-zero expected -> use absolute tolerance
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

    pub(crate) fn test_fn_is_eqv(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
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
}
