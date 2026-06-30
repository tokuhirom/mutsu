use super::*;

impl Interpreter {
    /// Pull the `idx`-th element of a pipeline source, or `None` when the source
    /// has fewer than `idx + 1` elements (finite source exhausted). Infinite
    /// integer ranges always produce. Nested lazy pipelines / gathers are pulled
    /// incrementally via [`Self::force_lazy_list_vm_n`].
    pub(super) fn pull_source_element(
        &mut self,
        source: &Value,
        idx: usize,
    ) -> Result<Option<Value>, RuntimeError> {
        match source {
            Value::Range(a, b)
            | Value::RangeExcl(a, b)
            | Value::RangeExclStart(a, b)
            | Value::RangeExclBoth(a, b) => {
                let start = match source {
                    Value::RangeExclStart(..) | Value::RangeExclBoth(..) => a.saturating_add(1),
                    _ => *a,
                };
                let inclusive = matches!(source, Value::Range(..) | Value::RangeExclStart(..));
                let cur = match start.checked_add(idx as i64) {
                    Some(v) => v,
                    None => return Ok(None),
                };
                let in_bounds = if inclusive { cur <= *b } else { cur < *b };
                if in_bounds {
                    Ok(Some(Value::Int(cur)))
                } else {
                    Ok(None)
                }
            }
            // Integer-start GenericRange (e.g. `^Inf`, `0..^Inf`): pull the
            // idx-th element directly so an infinite range stays lazy instead of
            // being materialized.
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } if matches!(start.as_ref(), Value::Int(_)) => {
                let s = match start.as_ref() {
                    Value::Int(i) => *i,
                    _ => unreachable!(),
                };
                let base = if *excl_start { s.saturating_add(1) } else { s };
                let cur = match base.checked_add(idx as i64) {
                    Some(v) => v,
                    None => return Ok(None),
                };
                let end_f = end.to_f64();
                let in_bounds = if end_f.is_infinite() && end_f.is_sign_positive() {
                    true
                } else {
                    let cur_f = cur as f64;
                    if *excl_end {
                        cur_f < end_f
                    } else {
                        cur_f <= end_f
                    }
                };
                Ok(in_bounds.then_some(Value::Int(cur)))
            }
            // Finite non-integer numeric start with infinite end (`1.5..Inf`):
            // yield `start + idx` (step 1), preserving the Rat/Num type. (Int
            // starts are handled by the case above; finite-end ranges never
            // reach the pull path — they are not lazy-pipe sources — but the
            // bounds check stays correct if one does.)
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } if start.is_numeric() && start.to_f64().is_finite() => {
                let i = idx as i64 + if *excl_start { 1 } else { 0 };
                let cur = match start.as_ref() {
                    Value::Rat(n, d) => crate::value::make_rat(n + i * *d, *d),
                    Value::Num(f) => Value::Num(f + i as f64),
                    other => Value::Num(other.to_f64() + i as f64),
                };
                let end_f = end.to_f64();
                let cur_f = cur.to_f64();
                let in_bounds = if end_f.is_infinite() && end_f.is_sign_positive() {
                    true
                } else if *excl_end {
                    cur_f < end_f
                } else {
                    cur_f <= end_f
                };
                Ok(in_bounds.then_some(cur))
            }
            // Non-finite-start numeric GenericRange (`-Inf..0`, `NaN..NaN`):
            // the `.succ` of `-Inf`/`NaN` is itself (`-Inf+1 == -Inf`,
            // `NaN+1 == NaN`), so the range yields its start ad infinitum. A
            // `+Inf` start yields nothing (Rakudo: `(Inf..Inf)` produces Nils).
            Value::GenericRange { start, end, .. } if matches!(start.as_ref(), Value::Num(f) if !f.is_finite()) =>
            {
                let s = match start.as_ref() {
                    Value::Num(f) => *f,
                    _ => unreachable!(),
                };
                if s == f64::INFINITY {
                    return Ok(None);
                }
                // Empty if the start strictly exceeds the end (NaN: never).
                if matches!(
                    s.partial_cmp(&end.to_f64()),
                    Some(std::cmp::Ordering::Greater)
                ) {
                    return Ok(None);
                }
                Ok(Some(Value::Num(s)))
            }
            Value::Seq(items) | Value::Slip(items) => Ok(items.get(idx).cloned()),
            Value::Array(items, _) => Ok(items.get(idx).cloned()),
            Value::LazyList(ll) => {
                let items = self.force_lazy_list_vm_n(ll, idx + 1)?;
                Ok(items.get(idx).cloned())
            }
            // Other sources (non-integer GenericRange, etc.) are not gated into
            // the lazy pipeline; materialize once and index.
            other => {
                let items = crate::runtime::value_to_list(other);
                Ok(items.get(idx).cloned())
            }
        }
    }

    /// Force a LazyList into a Seq by evaluating the gather body.
    /// Force a scan-based LazyList, computing up to `needed` elements.
    /// Elements are computed incrementally and cached in the LazyList.
    pub(super) fn force_scan_lazy_list(
        &mut self,
        list: &LazyList,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        let scan_mutex = match &list.scan_spec {
            Some(s) => s,
            None => return Ok(Vec::new()),
        };

        // Read current state under lock, then release before calling reduction methods
        let (base_op, negate, source, mut acc, already, cached_len) = {
            let spec = scan_mutex.lock().unwrap();
            let cache_guard = list.cache.lock().unwrap();
            let cached_len = cache_guard.as_ref().map_or(0, |v| v.len());
            if cached_len >= needed {
                return Ok(cache_guard.as_ref().unwrap()[..needed].to_vec());
            }
            (
                spec.op.clone(),
                spec.negate,
                spec.source.clone(),
                spec.accumulator.clone(),
                spec.computed_count,
                cached_len,
            )
        };

        let callable = self.reduction_callable_for_op(&base_op);
        let remaining = needed - cached_len;

        // Collect new source values to iterate over
        let new_values: Vec<Value> = match &source {
            Value::Range(a, b) => {
                let start = *a + already as i64;
                let end = if *b == i64::MAX {
                    *a + needed as i64
                } else {
                    *b
                };
                (start..=end).take(remaining).map(Value::Int).collect()
            }
            Value::RangeExcl(a, b) => {
                let start = *a + already as i64;
                let end = if *b == i64::MAX {
                    *a + needed as i64
                } else {
                    *b
                };
                (start..end).take(remaining).map(Value::Int).collect()
            }
            Value::RangeExclStart(a, b) => {
                let first = *a + 1;
                let start = first + already as i64;
                let end = if *b == i64::MAX {
                    first + needed as i64
                } else {
                    *b
                };
                (start..=end).take(remaining).map(Value::Int).collect()
            }
            Value::RangeExclBoth(a, b) => {
                let first = *a + 1;
                let start = first + already as i64;
                let end = if *b == i64::MAX {
                    first + needed as i64
                } else {
                    *b
                };
                (start..end).take(remaining).map(Value::Int).collect()
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                ..
            } => {
                let end_f = end.to_f64();
                let is_infinite = end_f.is_infinite() && end_f.is_sign_positive();
                let start_i = start.as_ref().to_f64() as i64;
                let first_i = if *excl_start { start_i + 1 } else { start_i };
                let iter_start = first_i + already as i64;
                let iter_end = if is_infinite {
                    iter_start + remaining as i64
                } else {
                    (end_f as i64).min(iter_start + remaining as i64)
                };
                (iter_start..=iter_end)
                    .take(remaining)
                    .map(Value::Int)
                    .collect()
            }
            _ => {
                let items = crate::runtime::utils::value_to_list(&source);
                items.into_iter().skip(already).take(remaining).collect()
            }
        };

        // Compute new scan elements (no locks held)
        let mut new_out: Vec<Value> = Vec::new();
        let mut computed = already;

        for val in new_values {
            acc = Some(match acc.take() {
                None => {
                    new_out.push(val.clone());
                    val
                }
                Some(prev) => {
                    let call_args = vec![prev, val];
                    let v =
                        self.reduction_step_with_args(&base_op, callable.as_ref(), call_args)?;
                    let v = if negate { Value::Bool(!v.truthy()) } else { v };
                    new_out.push(v.clone());
                    v
                }
            });
            computed += 1;
        }

        // Update spec and cache under lock
        {
            let mut spec = scan_mutex.lock().unwrap();
            spec.accumulator = acc;
            spec.computed_count = computed;

            let mut cache_guard = list.cache.lock().unwrap();
            let out = cache_guard.get_or_insert_with(Vec::new);
            out.extend(new_out);

            if out.len() >= needed {
                Ok(out[..needed].to_vec())
            } else {
                Ok(out.clone())
            }
        }
    }

    pub(super) fn force_lazy_if_needed(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if let Value::LazyList(ll) = &val {
            let items = self.force_lazy_list_vm(ll)?;
            Ok(Value::Seq(std::sync::Arc::new(items)))
        } else {
            Ok(val)
        }
    }
}
