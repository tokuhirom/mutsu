use super::*;

impl Interpreter {
    pub(super) fn eval_binary_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        f: fn(&mut Interpreter, Value, Value) -> Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        // Auto-FETCH Proxy containers in binary operations
        let left = loan_env!(self, auto_fetch_proxy(&left))?;
        let right = loan_env!(self, auto_fetch_proxy(&right))?;
        // Decontainerize Scalar wrappers
        let left = left.descalarize().clone();
        let right = right.descalarize().clone();
        if let (
            Value::Junction {
                kind: left_kind,
                values: _,
            },
            Value::Junction {
                kind: right_kind,
                values: right_values,
            },
        ) = (&left, &right)
            && Self::thread_right_first(left_kind, right_kind)
        {
            let results: Result<Vec<Value>, RuntimeError> = right_values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::junction(right_kind.clone(), results?));
        }
        if let Value::Junction { kind, values } = left {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(v, right.clone(), f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        // Force LazyList values before arithmetic/comparison operations
        let left = self.force_lazy_if_needed(left)?;
        let right = self.force_lazy_if_needed(right)?;
        f(self, left, right)
    }

    /// Smartmatch with junction threading but WITHOUT forcing lazy values.
    /// For `!~~` (negate=true), we compute `~~` first and then negate the
    /// collapsed result.  Raku defines `$x !~~ $y` as `not ($x ~~ $y)`,
    /// where `not` collapses junctions before negating.
    #[allow(dead_code)]
    pub(super) fn eval_smartmatch_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        negate: bool,
    ) -> Result<Value, RuntimeError> {
        self.eval_smartmatch_with_junctions_ex(left, right, negate, false)
    }

    /// Extended smartmatch with junction threading.
    /// `rhs_is_match_regex` indicates the RHS was originally `m//`, which
    /// changes the failure return from Nil to False.
    pub(super) fn eval_smartmatch_with_junctions_ex(
        &mut self,
        left: Value,
        right: Value,
        negate: bool,
        rhs_is_match_regex: bool,
    ) -> Result<Value, RuntimeError> {
        // For !~~, compute ~~ first, then negate the collapsed boolean.
        if negate {
            let match_result =
                self.eval_smartmatch_with_junctions_ex(left, right, false, rhs_is_match_regex)?;
            let bool_val = match_result.truthy();
            return Ok(Value::Bool(!bool_val));
        }
        // When RHS is the Junction type object, don't auto-thread LHS.
        // $junction ~~ Junction should return True (a Junction isa Junction).
        // Also applies to Mu (the supertype of Junction).
        if matches!(&right, Value::Package(name) if matches!(name.resolve().as_str(), "Junction" | "Mu"))
            && matches!(&left, Value::Junction { .. })
        {
            return self.smart_match_op(left, right, rhs_is_match_regex);
        }
        // Helper: check if a value is a regex (for junction collapse decisions)
        let is_regex_value = |v: &Value| {
            matches!(
                v,
                Value::Regex(_)
                    | Value::RegexWithAdverbs { .. }
                    | Value::Routine { is_regex: true, .. }
            )
        };
        if let (
            Value::Junction {
                kind: left_kind,
                values: _,
            },
            Value::Junction {
                kind: right_kind,
                values: right_values,
            },
        ) = (&left, &right)
            && Self::thread_right_first(left_kind, right_kind)
        {
            let results: Result<Vec<Value>, RuntimeError> = right_values
                .iter()
                .cloned()
                .map(|v| {
                    self.eval_smartmatch_with_junctions_ex(
                        left.clone(),
                        v,
                        false,
                        rhs_is_match_regex,
                    )
                })
                .collect();
            // Smartmatch collapses junctions to Bool
            let junction = Value::junction(right_kind.clone(), results?);
            return Ok(Value::Bool(junction.truthy()));
        }
        if let Value::Junction { kind, values } = left {
            // When RHS is a non-junction regex and LHS is a junction,
            // return the Junction of Match/Nil results without collapsing.
            // For all other cases, collapse to Bool.
            let keep_junction = is_regex_value(&right);
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| {
                    self.eval_smartmatch_with_junctions_ex(
                        v,
                        right.clone(),
                        false,
                        rhs_is_match_regex,
                    )
                })
                .collect();
            let junction = Value::junction(kind, results?);
            if keep_junction {
                return Ok(junction);
            }
            return Ok(Value::Bool(junction.truthy()));
        }
        if let Value::Junction { kind, values } = right {
            // Evaluate junction elements with short-circuit semantics:
            // All: if any element is False, stop early (don't evaluate remaining).
            // Any: if any element is True, stop early.
            // One: always evaluate all (no short-circuit possible).
            let mut results = Vec::with_capacity(values.len());
            for v in values.iter().cloned() {
                let r = self.eval_smartmatch_with_junctions_ex(
                    left.clone(),
                    v,
                    false,
                    rhs_is_match_regex,
                )?;
                let is_truthy = r.truthy();
                results.push(r);
                match &kind {
                    crate::value::JunctionKind::All if !is_truthy => break, // short-circuit: All fails fast
                    crate::value::JunctionKind::Any if is_truthy => break, // short-circuit: Any succeeds fast
                    _ => {}
                }
            }
            // Smartmatch collapses junctions to Bool
            let junction = Value::junction(kind, results);
            return Ok(Value::Bool(junction.truthy()));
        }
        self.smart_match_op(left, right, rhs_is_match_regex)
    }

    pub(super) fn smart_match_op(
        &mut self,
        left: Value,
        right: Value,
        rhs_is_match_regex: bool,
    ) -> Result<Value, RuntimeError> {
        // When RHS is Whatever, autoprime: return a WhateverCode that takes
        // one argument and smartmatches LHS against it.
        // In Raku, `$x ~~ *` produces `-> $a { $x ~~ $a }`.
        if matches!(&right, Value::Whatever) {
            use crate::ast::{Expr, Stmt};
            use crate::env::Env;
            let mut env = Env::new();
            env.insert(
                "__mutsu_callable_type".to_string(),
                Value::str_from("WhateverCode"),
            );
            // Capture the LHS value in the closure environment
            env.insert("__wc_sm_lhs".to_string(), left);
            let param = "__wc_0".to_string();
            let body = vec![Stmt::Expr(Expr::Binary {
                left: Box::new(Expr::Var("__wc_sm_lhs".to_string())),
                op: crate::token_kind::TokenKind::SmartMatch,
                right: Box::new(Expr::Var(param.clone())),
            })];
            return Ok(Value::make_sub(
                Symbol::intern("GLOBAL"),
                Symbol::intern("<whatevercode-smartmatch>"),
                vec![param],
                Vec::new(),
                body,
                false,
                env,
            ));
        }
        let is_regex = matches!(
            &right,
            Value::Regex(_)
                | Value::RegexWithAdverbs { .. }
                | Value::Routine { is_regex: true, .. }
        );
        let matched = self.vm_smart_match(&left, &right);
        // Check for pending regex security error (set by regex parse/match)
        if let Some(err) = crate::runtime::Interpreter::take_pending_regex_error() {
            return Err(err);
        }
        // Check for pending dispatch error (e.g., from Any ~~ Pair method call)
        if let Some(err) = self.take_pending_dispatch_error() {
            return Err(err);
        }
        if is_regex {
            // When $/ is a Junction (from :nth with junction argument),
            // the ~~ operator collapses the result to a Bool.
            let slash = self.env().get("/").cloned().unwrap_or(Value::Nil);
            if matches!(&slash, Value::Junction { .. }) {
                Ok(Value::Bool(matched))
            } else if matched {
                // For regex smartmatch, return the Match object (from $/) or Nil
                Ok(slash)
            } else if rhs_is_match_regex && matches!(&slash, Value::Nil) {
                // Failed m// (non-global) returns False, not Nil.
                // But m:g// returns an empty list from $/, so we check that
                // $/ is Nil before returning False.
                Ok(Value::Bool(false))
            } else {
                // Failed bare // returns Nil; m:g// returns $/ (empty list)
                Ok(slash)
            }
        } else {
            Ok(Value::Bool(matched))
        }
    }

    /// Decont `:=`-bound `ContainerRef` cells inside an Array value.
    ///
    /// When an array element has been bound (`@a[1] := $var`), the element
    /// holds a shared `ContainerRef` cell. This method replaces every such
    /// cell with its current value, so that callers that snapshot or iterate
    /// elements (assignment copies, stringification, `say`, `gist`) see the
    /// live value instead of the cell.
    pub(super) fn resolve_bound_array_elements(&self, val: Value) -> Value {
        if let Value::Array(ref items, kind) = val {
            let needs_resolve = items.iter().any(Value::is_container_ref);
            if !needs_resolve {
                return val;
            }
            let resolved: Vec<Value> = items
                .iter()
                .map(|v| match v {
                    Value::ContainerRef(cell) => cell.lock().unwrap().clone(),
                    other => other.clone(),
                })
                .collect();
            Value::Array(
                std::sync::Arc::new(crate::value::ArrayData::new(resolved)),
                kind,
            )
        } else {
            val
        }
    }

    /// Extend an infinite closure-based sequence to at least `needed` elements
    /// by re-invoking its generator closure over the growing element history.
    /// Returns whatever is available (possibly fewer than `needed`) once the
    /// generator signals termination.
    pub(super) fn extend_closure_sequence(
        &mut self,
        list: &LazyList,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        // Fast path: already cached enough.
        {
            let cache = list.cache.lock().unwrap();
            if let Some(cached) = cache.as_ref()
                && cached.len() >= needed
            {
                return Ok(cached[..needed].to_vec());
            }
        }

        // Snapshot the current history without holding the cache lock across
        // user-code execution.
        let mut history = {
            let cache = list.cache.lock().unwrap();
            cache.as_ref().cloned().unwrap_or_default()
        };

        let state_mutex = list.closure_seq.as_ref().unwrap();
        let mut guard = state_mutex.lock().unwrap();
        let state = &mut *guard;
        let generator = state.generator.clone();

        while history.len() < needed && !state.finished {
            let precompiled = state
                .precompiled
                .as_ref()
                .map(|(c, f)| (c.as_ref(), f.as_ref()));
            match self.sequence_closure_step(
                &generator,
                &history,
                precompiled,
                &mut state.closure_env,
                false,
            )? {
                Some(v) => history.push(v),
                None => {
                    state.finished = true;
                    break;
                }
            }
        }

        // Publish the extended history back to the cache.
        *list.cache.lock().unwrap() = Some(history.clone());
        let take = needed.min(history.len());
        Ok(history[..take].to_vec())
    }

    /// Extend a sequence-spec lazy list's cache to at least `needed` elements.
    /// This generates new elements using the sequence spec (arithmetic/geometric)
    /// without needing any Interpreter or interpreter context.
    pub(super) fn extend_sequence_cache(
        list: &LazyList,
        spec: &crate::value::SequenceSpec,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut cache = list.cache.lock().unwrap();
        let items = cache.get_or_insert_with(Vec::new);
        if items.len() >= needed {
            return Ok(items[..needed].to_vec());
        }
        // Generate more elements
        while items.len() < needed {
            let last = items.last().cloned().unwrap_or(Value::Int(0));
            let next = match spec {
                crate::value::SequenceSpec::Arithmetic { step, all_int } => {
                    if *all_int {
                        if let Value::Int(n) = last {
                            Value::Int(n + step)
                        } else {
                            let n = last.to_f64();
                            Value::Num(n + *step as f64)
                        }
                    } else {
                        let n = last.to_f64();
                        Value::Num(n + *step as f64)
                    }
                }
                crate::value::SequenceSpec::GeometricRat { num, den } => {
                    if let Value::Int(n) = last {
                        // `n * num` can overflow i64 for a growing geometric
                        // sequence (`1, 2, 4 ... *`). Raku's Int is arbitrary
                        // precision, so promote to BigInt on overflow instead of
                        // panicking.
                        match n.checked_mul(*num) {
                            Some(result) if result % den == 0 => Value::Int(result / den),
                            Some(result) => match result.checked_mul(*num) {
                                Some(rn) => Value::Rat(rn, *den),
                                None => {
                                    use num_bigint::BigInt;
                                    crate::value::make_big_rat_arith(
                                        BigInt::from(result) * BigInt::from(*num),
                                        BigInt::from(*den),
                                    )
                                }
                            },
                            None => {
                                use num_bigint::BigInt;
                                let prod = BigInt::from(n) * BigInt::from(*num);
                                let bden = BigInt::from(*den);
                                if (&prod % &bden) == BigInt::from(0) {
                                    crate::value::Value::from_bigint(prod / bden)
                                } else {
                                    crate::value::make_big_rat_arith(
                                        prod * BigInt::from(*num),
                                        bden,
                                    )
                                }
                            }
                        }
                    } else if let Value::BigInt(n) = last {
                        // Continue an already-promoted geometric sequence in exact
                        // BigInt arithmetic (raku keeps `1, 2, 4 ... *` exact past
                        // i64), rather than dropping to lossy f64.
                        use num_bigint::BigInt;
                        let prod = n.as_ref().clone() * BigInt::from(*num);
                        let bden = BigInt::from(*den);
                        if (&prod % &bden) == BigInt::from(0) {
                            crate::value::Value::from_bigint(prod / bden)
                        } else {
                            crate::value::make_big_rat_arith(prod * BigInt::from(*num), bden)
                        }
                    } else {
                        let n = last.to_f64();
                        Value::Num(n * (*num as f64) / (*den as f64))
                    }
                }
                crate::value::SequenceSpec::Geometric { ratio } => {
                    let n = last.to_f64();
                    Value::Num(n * ratio)
                }
            };
            items.push(next);
        }
        Ok(items[..needed].to_vec())
    }
}
