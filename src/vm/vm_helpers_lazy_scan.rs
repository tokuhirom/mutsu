use super::*;

impl Interpreter {
    /// Pull the `idx`-th element of a pipeline source, or `None` when the source
    /// has fewer than `idx + 1` elements (finite source exhausted). Unbounded
    /// ranges always produce. Nested lazy pipelines / gathers are pulled
    /// incrementally via [`Self::force_lazy_list_vm_n`].
    pub(crate) fn pull_source_element(
        &mut self,
        source: &Value,
        idx: usize,
    ) -> Result<Option<Value>, RuntimeError> {
        // An unbounded range with a numeric start (`^Inf`, `1.5..*`,
        // `1e0..Inf`): element `idx` is `first + idx` in the start's own type,
        // with nothing cached. (A non-numeric start reaches a pipe as its
        // `unbounded_range::lazy_list` instead, see `pipe_source`.)
        if matches!(source.view(), ValueView::GenericRange { .. })
            && let Some(first) = crate::runtime::unbounded_range::first(source)
            && let Some(v) = crate::runtime::unbounded_range::nth(&first, idx)
        {
            return Ok(Some(v));
        }
        match source.view() {
            ValueView::Range(a, b)
            | ValueView::RangeExcl(a, b)
            | ValueView::RangeExclStart(a, b)
            | ValueView::RangeExclBoth(a, b) => {
                let start = match source.view() {
                    ValueView::RangeExclStart(..) | ValueView::RangeExclBoth(..) => {
                        a.saturating_add(1)
                    }
                    _ => a,
                };
                let inclusive = matches!(
                    source.view(),
                    ValueView::Range(..) | ValueView::RangeExclStart(..)
                );
                let cur = match start.checked_add(idx as i64) {
                    Some(v) => v,
                    None => return Ok(None),
                };
                let in_bounds = if inclusive { cur <= b } else { cur < b };
                if in_bounds {
                    Ok(Some(Value::int(cur)))
                } else {
                    Ok(None)
                }
            }
            // Non-finite-start numeric GenericRange (`-Inf..0`, `NaN..NaN`):
            // the `.succ` of `-Inf`/`NaN` is itself (`-Inf+1 == -Inf`,
            // `NaN+1 == NaN`), so the range yields its start ad infinitum. A
            // `+Inf` start yields nothing (Rakudo: `(Inf..Inf)` produces Nils).
            ValueView::GenericRange { start, end, .. } if matches!(start.as_ref().view(), ValueView::Num(f) if !f.is_finite()) =>
            {
                let s = match start.as_ref().view() {
                    ValueView::Num(f) => f,
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
                Ok(Some(Value::num(s)))
            }
            // A lazy `Seq.new($iterator)` source is pulled only as far as
            // `idx` (#10891); any other Seq is read as it stands.
            ValueView::Seq(body) => {
                if idx >= body.len() && body.unpulled_iterator().is_some() {
                    body.extend_from_iterator(idx + 1, |iterator, count| {
                        self.pull_iterator_prefix_to_vec(iterator, count)
                    })?;
                }
                Ok(body.get(idx).cloned())
            }
            ValueView::Slip(items) => Ok(items.get(idx).cloned()),
            ValueView::Array(items, _) => Ok(items.get(idx).cloned()),
            ValueView::LazyList(ll) => {
                // An element already produced is read straight from the cache
                // (an adaptor re-reads earlier elements: an overlapping
                // `rotor`, a cross product's inner operand).
                if let Some(v) = ll
                    .cache
                    .lock()
                    .unwrap_or_else(|e| e.into_inner())
                    .as_ref()
                    .and_then(|c| c.get(idx))
                {
                    return Ok(Some(v.clone()));
                }
                let items = self.force_lazy_list_vm_n(&ll, idx + 1)?;
                Ok(items.get(idx).cloned())
            }
            // Other sources (non-integer GenericRange, etc.) are not gated into
            // the lazy pipeline; materialize once and index.
            _ => {
                let items = crate::runtime::value_to_list(source);
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

        let callable = self.reduction_callable_for_op(&base_op, None);
        let remaining = needed - cached_len;
        // The source position runs ahead of the cache by however many elements
        // a front mutation of a lazy `@`-array removed (or behind by however
        // many it added): the source is walked from `already`, the scan's own
        // count, never from the cache length (#10861).
        let source_needed = already.saturating_add(remaining);
        let span = |first: i64, offset: usize| first.saturating_add(offset as i64);
        let remaining_i = i64::try_from(remaining).unwrap_or(i64::MAX);

        // Collect new source values to iterate over
        let new_values: Vec<Value> = match source.view() {
            ValueView::Range(a, b) => {
                let start = span(a, already);
                let end = if b == i64::MAX {
                    span(a, source_needed)
                } else {
                    b
                };
                (start..=end).take(remaining).map(Value::int).collect()
            }
            ValueView::RangeExcl(a, b) => {
                let start = span(a, already);
                let end = if b == i64::MAX {
                    span(a, source_needed)
                } else {
                    b
                };
                (start..end).take(remaining).map(Value::int).collect()
            }
            ValueView::RangeExclStart(a, b) => {
                let first = a + 1;
                let start = span(first, already);
                let end = if b == i64::MAX {
                    span(first, source_needed)
                } else {
                    b
                };
                (start..=end).take(remaining).map(Value::int).collect()
            }
            ValueView::RangeExclBoth(a, b) => {
                let first = a + 1;
                let start = span(first, already);
                let end = if b == i64::MAX {
                    span(first, source_needed)
                } else {
                    b
                };
                (start..end).take(remaining).map(Value::int).collect()
            }
            // An unbounded range with a numeric start: elements `already..`
            // are `first + i` in the start's own type (`1.5..*` scans Rats).
            ValueView::GenericRange { .. }
                if let Some(first) = crate::runtime::unbounded_range::first(&source)
                    && first.is_numeric() =>
            {
                (already..source_needed)
                    .filter_map(|i| crate::runtime::unbounded_range::nth(&first, i))
                    .collect()
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                ..
            } => {
                let end_f = end.to_f64();
                let is_infinite = end_f.is_infinite() && end_f.is_sign_positive();
                let start_i = start.as_ref().to_f64() as i64;
                let first_i = if excl_start { start_i + 1 } else { start_i };
                let iter_start = span(first_i, already);
                let iter_end = if is_infinite {
                    iter_start.saturating_add(remaining_i)
                } else {
                    (end_f as i64).min(iter_start.saturating_add(remaining_i))
                };
                (iter_start..=iter_end)
                    .take(remaining)
                    .map(Value::int)
                    .collect()
            }
            // A lazy pipe (`(1..*).map(...)`, `.grep(...)`, a `gather`) is a
            // `LazyList`, and `value_to_list` declines to materialise one that
            // has no bound — it answered an EMPTY list, so the scan stepped
            // `needed` times over nothing and produced that many `Nil`s
            // (`([\~] (1..*).map(* + 1))[^4]` was `(Nil Nil Nil Nil)`). Pull
            // exactly the prefix this batch needs instead, the same bounded
            // pull every other lazy consumer uses; a finite pipe simply runs
            // out and the scan ends with it.
            ValueView::LazyList(inner) => {
                let inner = inner.clone();
                let items = self.force_lazy_list_vm_n(&inner, source_needed)?;
                items.into_iter().skip(already).take(remaining).collect()
            }
            _ => {
                let items = crate::runtime::utils::value_to_list(&source);
                items.into_iter().skip(already).take(remaining).collect()
            }
        };

        // Compute new scan elements (no locks held). The scan's operator is
        // decoded once, not once per element of the batch.
        let op_shape = crate::compiled_operator::InfixShape::lower(&base_op);
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
                    let v = self.reduction_step_with_args(
                        op_shape.as_ref(),
                        callable.as_ref(),
                        call_args,
                    )?;
                    let v = if negate { Value::truth(!v.truthy()) } else { v };
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
        if let ValueView::LazyList(ll) = val.view() {
            let items = self.force_lazy_list_vm(&ll)?;
            Ok(Value::seq(items))
        } else {
            Ok(val)
        }
    }
}
