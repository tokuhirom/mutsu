use super::super::*;
use crate::value::ValueView;

impl Interpreter {
    pub(crate) fn test_fn_cmp_ok(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Extract positionals keeping Pair values (only skip internal named pairs).
        // This is needed because user-provided Pair values (e.g. `cmp-ok $pair, '===', $pair`)
        // would otherwise be filtered out by positional_value which treats all Pairs as named args.
        let mut positionals = Vec::new();
        for arg in args {
            if let ValueView::Pair(key, _) = arg.view()
                && (key == "__mutsu_test_callsite_line" || key == "todo")
            {
                continue;
            }
            positionals.push(arg.clone());
        }
        let left = positionals.first().cloned().unwrap_or(Value::NIL);
        let op_val = positionals.get(1).cloned().unwrap_or(Value::NIL);
        let right = positionals.get(2).cloned().unwrap_or(Value::NIL);
        let desc = positionals
            .get(3)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let todo = Self::named_bool(args, "todo");
        // Clone values before they might be consumed by callable operator
        let left_diag = left.clone();
        let right_diag = right.clone();
        let op_diag = match op_val.view() {
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => {
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
        // When either operand is a Junction, thread the comparison through it.
        // cmp-ok checks the boolean truthiness of the resulting junction.
        let has_junction = matches!(left.view(), ValueView::Junction { .. })
            || matches!(right.view(), ValueView::Junction { .. });
        let ok = match op_val.view() {
            ValueView::Str(op)
                if has_junction
                    && matches!(
                        op.as_str(),
                        "==" | "!="
                            | "<"
                            | "<="
                            | ">"
                            | ">="
                            | "eq"
                            | "ne"
                            | "lt"
                            | "le"
                            | "gt"
                            | "ge"
                            | "==="
                            | "!=="
                            | "eqv"
                    ) =>
            {
                let result = Self::cmp_ok_junction_thread(&left, &right, &op);
                result.truthy()
            }
            // A Unicode infix alias names the same operator as its ASCII
            // spelling (`≤` is `<=`), so accept either here — the real
            // `Test.rakumod` resolves the string through the operator's routine
            // and never sees the difference.
            ValueView::Str(op) => match Self::normalize_unicode_infix(op.as_str()) {
                "~~" => self.smart_match(&left, &right),
                "!~~" => !self.smart_match(&left, &right),
                "eq" | "ne" => {
                    let equal = if Self::is_buf_value(&left) && Self::is_buf_value(&right) {
                        Self::extract_buf_bytes(&left) == Self::extract_buf_bytes(&right)
                    } else {
                        Self::stringify_compare_operand(&left)?
                            == Self::stringify_compare_operand(&right)?
                    };
                    if op.as_str() == "eq" { equal } else { !equal }
                }
                "lt" | "le" | "gt" | "ge" => {
                    let ord = match Self::blob_ordering(&left, &right)? {
                        Some(ord) => ord,
                        None => Self::stringify_compare_operand(&left)?
                            .cmp(&Self::stringify_compare_operand(&right)?),
                    };
                    match op.as_str() {
                        "lt" => ord == std::cmp::Ordering::Less,
                        "le" => ord != std::cmp::Ordering::Greater,
                        "gt" => ord == std::cmp::Ordering::Greater,
                        "ge" => ord != std::cmp::Ordering::Less,
                        _ => unreachable!(),
                    }
                }
                "==" => super::super::to_float_value(&left) == super::super::to_float_value(&right),
                "!=" => super::super::to_float_value(&left) != super::super::to_float_value(&right),
                "<" => super::super::to_float_value(&left) < super::super::to_float_value(&right),
                "<=" => super::super::to_float_value(&left) <= super::super::to_float_value(&right),
                ">" => super::super::to_float_value(&left) > super::super::to_float_value(&right),
                ">=" => super::super::to_float_value(&left) >= super::super::to_float_value(&right),
                "===" => crate::runtime::utils::values_identical(&left, &right),
                "!===" => !crate::runtime::utils::values_identical(&left, &right),
                "eqv" => {
                    // Check if either side is a consumed Seq (throws X::Seq::Consumed)
                    if let ValueView::Seq(items) = left.view()
                        && items.is_consumed()
                    {
                        return Err(crate::value::seq_consumed_error());
                    }
                    if let ValueView::Seq(items) = right.view()
                        && items.is_consumed()
                    {
                        return Err(crate::value::seq_consumed_error());
                    }
                    Self::cmp_eqv_bool(&left, &right)
                }
                "=:=" => crate::runtime::utils::values_identical(&left, &right),
                "=~=" | "\u{2245}" => {
                    // =~= / ≅ approximately equal
                    let (lr, li) = match left.view() {
                        ValueView::Complex(r, i) => (r, i),
                        _ => (super::super::to_float_value(&left).unwrap_or(0.0), 0.0),
                    };
                    let (rr, ri) = match right.view() {
                        ValueView::Complex(r, i) => (r, i),
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
                        *op
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
            self.output_sink_mut().stderr_output.push_str(&diag);
            eprint!("{}", diag);
        }
        Ok(Value::truth(ok))
    }

    /// Thread a comparison operator through junctions for cmp-ok.
    fn cmp_ok_junction_thread(left: &Value, right: &Value, op: &str) -> Value {
        if let ValueView::Junction { kind, values } = left.view() {
            let results: Vec<Value> = values
                .iter()
                .map(|v| Self::cmp_ok_junction_thread(v, right, op))
                .collect();
            return Value::junction(kind, results);
        }
        if let ValueView::Junction { kind, values } = right.view() {
            let results: Vec<Value> = values
                .iter()
                .map(|v| Self::cmp_ok_junction_thread(left, v, op))
                .collect();
            return Value::junction(kind, results);
        }
        let ok = match op {
            "==" => super::super::to_float_value(left) == super::super::to_float_value(right),
            "!=" => super::super::to_float_value(left) != super::super::to_float_value(right),
            "<" => super::super::to_float_value(left) < super::super::to_float_value(right),
            "<=" => super::super::to_float_value(left) <= super::super::to_float_value(right),
            ">" => super::super::to_float_value(left) > super::super::to_float_value(right),
            ">=" => super::super::to_float_value(left) >= super::super::to_float_value(right),
            "eq" => left.to_string_value() == right.to_string_value(),
            "ne" => left.to_string_value() != right.to_string_value(),
            "lt" => left.to_string_value() < right.to_string_value(),
            "le" => left.to_string_value() <= right.to_string_value(),
            "gt" => left.to_string_value() > right.to_string_value(),
            "ge" => left.to_string_value() >= right.to_string_value(),
            "===" => crate::runtime::utils::values_identical(left, right),
            "!==" => !crate::runtime::utils::values_identical(left, right),
            "eqv" => Self::cmp_eqv_bool(left, right),
            _ => false,
        };
        Value::truth(ok)
    }

    pub(crate) fn test_fn_like(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let text = match Self::positional_value(args, 0) {
            Some(v) => self.stringify_value(v.clone())?,
            None => String::new(),
        };
        let ok = self.like_matches(args, &text);
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::truth(ok))
    }

    /// Whether `text` matches the pattern argument of like/unlike. A regex
    /// with adverbs (`rx:i/.../`) goes through the general smartmatch so the
    /// adverbs apply (a plain `ValueView::Regex` keeps the direct fast path).
    fn like_matches(&mut self, args: &[Value], text: &str) -> bool {
        match Self::positional_value(args, 1) {
            Some(re) => match re.view() {
                ValueView::Regex(pat) => self.regex_is_match(&pat, text),
                ValueView::RegexWithAdverbs(_) => {
                    let re = re.clone();
                    self.smart_match_values(&Value::str(text.to_string()), &re)
                }
                _ => false,
            },
            None => false,
        }
    }

    pub(crate) fn test_fn_unlike(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let desc = Self::positional_string(args, 2);
        let todo = Self::named_bool(args, "todo");
        let text = match Self::positional_value(args, 0) {
            Some(v) => self.stringify_value(v.clone())?,
            None => String::new(),
        };
        let ok = !self.like_matches(args, &text);
        self.test_ok(ok, &desc, todo)?;
        Ok(Value::truth(ok))
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
                if matches!(left.view(), ValueView::Junction { .. })
                    || matches!(right.view(), ValueView::Junction { .. })
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
            let got_raku = left.map(|v| self.value_raku_repr(v)).unwrap_or_default();
            let expected_raku = right.map(|v| self.value_raku_repr(v)).unwrap_or_default();
            self.emit_output(&format!(
                "# expected: {}\n#      got: {}\n",
                expected_raku, got_raku
            ));
        }
        Ok(Value::truth(ok))
    }

    /// Extract (left, right, desc) for is-deeply from raw args.
    /// The first two args are always the values being compared (even if they are Pairs).
    /// Only known internal named pairs (__mutsu_test_callsite_line, todo) are skipped.
    fn extract_is_deeply_args(args: &[Value]) -> (Option<&Value>, Option<&Value>, String) {
        let mut positionals = Vec::new();
        for arg in args {
            if let ValueView::Pair(key, _) = arg.view()
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

    fn seq_to_list(&mut self, v: &Value) -> Value {
        // ADR-0040 slices 1-2: a `Seq` stored as a real `Array`/`Hash` element
        // is itemized as `Scalar(Seq)`. `is-deeply` compares with `eqv`, which
        // decontainerizes, so this Seq-to-List normalization has to see through
        // the wrapper too — otherwise one side becomes a `List` and the other
        // stays a `Scalar(Seq)` and they compare unequal even though
        // `Seq eqv $(Seq)` is True. (`roast/S02-types/pair.t`'s `Pair.invert`
        // subtest stages its expectations in `my @tests = [ …, ….Seq ], …`.)
        let v = v.descalarize();
        match v.view() {
            // A deferred Seq (`Seq.new($iterator)`, `IO::Handle.lines` —
            // ADR-0034 folds the latter into the same `SeqSource`) must be
            // pulled before comparing, else it compares as the empty list
            // (`is-deeply Seq.new($user-iter), (...)`) or the opaque "(...)".
            ValueView::Seq(body) if body.needs_touch() => {
                let body = std::sync::Arc::clone(&body);
                match self.reify_seq_body(&body) {
                    Ok(pulled) => Value::array_with_kind(
                        crate::value::Value::array_arc(pulled),
                        ArrayKind::List,
                    ),
                    Err(_) => v.clone(),
                }
            }
            ValueView::Seq(items) => Value::array_with_kind(
                crate::value::Value::array_arc(items.clone().to_vec()),
                ArrayKind::List,
            ),
            ValueView::LazyList(list) => match self.force_lazy_list(&list) {
                Ok(items) => Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                    ArrayKind::List,
                ),
                Err(_) => v.clone(),
            },
            _ => v.clone(),
        }
    }

    /// Perform `eq` (string) comparison that threads through Junctions,
    /// returning a Value (possibly a Junction of Bools).
    /// Used by `is` which compares stringified values.
    pub(crate) fn eq_with_junctions(left: &Value, right: &Value) -> Value {
        if let ValueView::Junction { kind, values } = left.view() {
            let results: Vec<Value> = values
                .iter()
                .map(|v| Self::eq_with_junctions(v, right))
                .collect();
            return Value::junction(kind, results);
        }
        if let ValueView::Junction { kind, values } = right.view() {
            let results: Vec<Value> = values
                .iter()
                .map(|v| Self::eq_with_junctions(left, v))
                .collect();
            return Value::junction(kind, results);
        }
        Value::truth(left.to_string_value() == right.to_string_value())
    }

    /// Perform `eqv` comparison that threads through Junctions,
    /// returning a Value (possibly a Junction of Bools).
    pub(crate) fn eqv_with_junctions(left: &Value, right: &Value) -> Value {
        if let ValueView::Junction { kind, values } = left.view() {
            let results: Vec<Value> = values
                .iter()
                .map(|v| Self::eqv_with_junctions(v, right))
                .collect();
            return Value::junction(kind, results);
        }
        if let ValueView::Junction { kind, values } = right.view() {
            let results: Vec<Value> = values
                .iter()
                .map(|v| Self::eqv_with_junctions(left, v))
                .collect();
            return Value::junction(kind, results);
        }
        Value::truth(left.eqv(right))
    }

    pub(crate) fn test_fn_is_approx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let todo = Self::named_bool(args, "todo");
        let got = Self::positional_value_required(args, 0, "is-approx expects got")?;
        let expected = Self::positional_value_required(args, 1, "is-approx expects expected")?;

        // Detect three-arg form: is-approx(got, expected, numeric-tolerance, desc?)
        // If the third positional arg is a native numeric type (not a Str),
        // treat it as abs-tol.
        let third_pos = Self::positional_value(args, 2);
        let third_is_numeric = third_pos.map(Value::view).is_some_and(|v| {
            matches!(
                v,
                ValueView::Int(_)
                    | ValueView::Num(_)
                    | ValueView::Rat(_, _)
                    | ValueView::FatRat(_, _)
                    | ValueView::BigRat(_, _)
                    | ValueView::BigInt(_)
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
            if matches!(value.view(), ValueView::Instance { .. }) {
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
            let rel_ok = |rt: f64| -> bool {
                let max = g.abs().max(e.abs());
                if max == 0.0 {
                    true
                } else {
                    (g - e).abs() / max <= rt
                }
            };
            if let (Some(at), Some(rt)) = (explicit_abs_tol, explicit_rel_tol) {
                // Both tolerances given: BOTH must be satisfied (Raku Test module
                // semantics — `$abs-tol-ok and $rel-tol-ok`).
                (g - e).abs() <= at && rel_ok(rt)
            } else if let Some(at) = explicit_abs_tol {
                (g - e).abs() <= at
            } else if let Some(rt) = explicit_rel_tol {
                rel_ok(rt)
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
        let got_inner = match got.view() {
            ValueView::Mixin(inner, _) => inner.as_ref(),
            _ => got,
        };
        let expected_inner = match expected.view() {
            ValueView::Mixin(inner, _) => inner.as_ref(),
            _ => expected,
        };
        let ok = match (got_inner.view(), expected_inner.view()) {
            (ValueView::Complex(gr, gi), ValueView::Complex(er, ei)) => {
                approx_eq(gr, er) && approx_eq(gi, ei)
            }
            (ValueView::Complex(gr, gi), _) => {
                if let Some(e) = expected_f {
                    approx_eq(gr, e) && approx_eq(gi, 0.0)
                } else {
                    false
                }
            }
            (_, ValueView::Complex(er, ei)) => {
                if let Some(g) = coerce_float(got) {
                    approx_eq(g, er) && approx_eq(0.0, ei)
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
        Ok(Value::truth(ok))
    }
}
