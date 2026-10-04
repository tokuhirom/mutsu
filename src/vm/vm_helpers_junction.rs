use super::*;

impl Interpreter {
    pub(crate) fn eval_binary_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        f: fn(&mut Interpreter, Value, Value) -> Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        // ADR-0058: every binary operator below reads its operands' elements
        // through pure code (set ops, comparison, `~`, ...), so a
        // still-deferred `.map` operand has to run its callback first.
        self.reify_map_grep_seq(&left)?;
        self.reify_map_grep_seq(&right)?;
        // Auto-FETCH Proxy containers in binary operations
        let left = loan_env!(self, auto_fetch_proxy(&left))?;
        let right = loan_env!(self, auto_fetch_proxy(&right))?;
        // Decontainerize Scalar wrappers
        let left = left.descalarize().clone();
        let right = right.descalarize().clone();
        if let (
            ValueView::Junction {
                kind: left_kind,
                values: _,
            },
            ValueView::Junction {
                kind: right_kind,
                values: right_values,
            },
        ) = (left.view(), right.view())
            && Self::thread_right_first(&left_kind, &right_kind)
        {
            let results: Result<Vec<Value>, RuntimeError> = right_values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::junction(right_kind, results?));
        }
        if let ValueView::Junction { kind, values } = left.view() {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(v, right.clone(), f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        if let ValueView::Junction { kind, values } = right.view() {
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
            return Ok(Value::truth(!bool_val));
        }
        // When RHS is the Junction type object, don't auto-thread LHS.
        // $junction ~~ Junction should return True (a Junction isa Junction).
        // Also applies to Mu (the supertype of Junction).
        if matches!(right.view(), ValueView::Package(name) if matches!(name.resolve().as_str(), "Junction" | "Mu"))
            && matches!(left.view(), ValueView::Junction { .. })
        {
            return self.smart_match_op(left, right, rhs_is_match_regex);
        }
        // Helper: check if a value is a regex (for junction collapse decisions)
        let is_regex_value = |v: &Value| {
            matches!(
                v.view(),
                ValueView::Regex(_)
                    | ValueView::RegexWithAdverbs { .. }
                    | ValueView::Routine { is_regex: true, .. }
            )
        };
        if let (
            ValueView::Junction {
                kind: left_kind,
                values: _,
            },
            ValueView::Junction {
                kind: right_kind,
                values: right_values,
            },
        ) = (left.view(), right.view())
            && Self::thread_right_first(&left_kind, &right_kind)
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
            let junction = Value::junction(right_kind, results?);
            return Ok(Value::truth(junction.truthy()));
        }
        if let ValueView::Junction { kind, values } = left.view() {
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
            return Ok(Value::truth(junction.truthy()));
        }
        if let ValueView::Junction { kind, values } = right.view() {
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
                match kind {
                    crate::value::JunctionKind::All if !is_truthy => break, // short-circuit: All fails fast
                    crate::value::JunctionKind::Any if is_truthy => break, // short-circuit: Any succeeds fast
                    _ => {}
                }
            }
            // Smartmatch collapses junctions to Bool
            let junction = Value::junction(kind, results);
            return Ok(Value::truth(junction.truthy()));
        }
        self.smart_match_op(left, right, rhs_is_match_regex)
    }

    pub(super) fn smart_match_op(
        &mut self,
        left: Value,
        right: Value,
        rhs_is_match_regex: bool,
    ) -> Result<Value, RuntimeError> {
        // A Whatever *value* on the RHS smartmatches to True (`X ~~ *` is always
        // True per Whatever's ACCEPTS). The autoprime of a *syntactic* bare `*`
        // (`$x ~~ *` → `-> $a { $x ~~ $a }`) is handled at parse time in the
        // precedence parser, which can tell a bare `*` from a parenthesized `(*)`
        // / a variable holding a Whatever — those reach here as a Whatever value
        // and must be True, not re-primed. So do NOT autoprime here; fall through
        // to `vm_smart_match`, whose `pure_smart_match((_, Whatever)) => true`
        // arm returns True.
        let is_regex = matches!(
            right.view(),
            ValueView::Regex(_)
                | ValueView::RegexWithAdverbs { .. }
                | ValueView::Routine { is_regex: true, .. }
        );
        let closure_scope = self.install_regex_closure_scope(&right);
        let matched = self.vm_try_smart_match(&left, &right);
        self.uninstall_regex_closure_scope(closure_scope);
        // Check for pending regex security error (set by regex parse/match)
        if let Some(err) = crate::runtime::Interpreter::take_pending_regex_error() {
            return Err(err);
        }
        // An exception raised while matching (`Any ~~ Pair` naming a missing
        // method, a user `ACCEPTS` that dies).
        let matched = matched?;
        if is_regex {
            // When $/ is a Junction (from :nth with junction argument),
            // the ~~ operator collapses the result to a Bool.
            // A bare block may hold its lexically captured `$/` in a shared
            // cell. Smartmatch returns the Match value, never the container
            // implementing that lexical binding.
            let slash = self
                .env()
                .get_sym(crate::symbol::wk::match_var())
                .cloned()
                .unwrap_or(Value::NIL);
            let slash = if slash.is_container_ref() {
                slash.into_deref()
            } else {
                slash
            };
            if slash.is_junction_value() {
                Ok(Value::truth(matched))
            } else if matched {
                // For regex smartmatch, return the Match object (from $/) or Nil
                Ok(slash)
            } else if rhs_is_match_regex && slash.is_nil() {
                // Failed m// (non-global) returns False, not Nil.
                // But m:g// returns an empty list from $/, so we check that
                // $/ is Nil before returning False.
                Ok(Value::FALSE)
            } else {
                // Failed bare // returns Nil; m:g// returns $/ (empty list)
                Ok(slash)
            }
        } else {
            Ok(Value::truth(matched))
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
        if let ValueView::Array(items, kind) = val.view() {
            let needs_resolve = items
                .iter()
                .any(|v| v.is_container_ref() || v.is_hash_entry_ref_value());
            if !needs_resolve {
                return val;
            }
            let resolved: Vec<Value> = items
                .iter()
                .map(|v| match v.view() {
                    ValueView::ContainerRef(cell) => {
                        let inner = cell.lock().unwrap().clone();
                        if matches!(inner.view(), ValueView::HashEntryRef { .. }) {
                            inner.hash_entry_read()
                        } else {
                            inner
                        }
                    }
                    ValueView::HashEntryRef { .. } => v.hash_entry_read(),
                    _ => v.clone(),
                })
                .collect();
            Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(resolved)),
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
    // Cost: O(needed) for the copy, plus one generator call per new element.
    pub(crate) fn extend_closure_sequence(
        &mut self,
        list: &LazyList,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        self.fill_closure_sequence(list, needed)?;
        Ok(list.cache_window(0, needed))
    }

    /// [`Self::extend_closure_sequence`] without the copy: generate until the
    /// cache holds `needed` elements or the sequence ends.
    // Cost: O(1) amortized per new element, plus the generator call.
    pub(super) fn fill_closure_sequence(
        &mut self,
        list: &LazyList,
        needed: usize,
    ) -> Result<(), RuntimeError> {
        // Fast path: already cached enough.
        {
            let cache = list.cache.lock().unwrap();
            if let Some(cached) = cache.as_ref()
                && cached.len() >= needed
            {
                return Ok(());
            }
        }

        // Snapshot the PRISTINE trailing history from `generation_state`, not
        // `cache`, without holding either lock across user-code execution:
        // the generator closure may read several trailing elements (e.g. a
        // Fibonacci-style `1, 1, * + * ... *`), and `cache` may hold an
        // element an `@`-array mutation overwrote in place
        // (`Interpreter::restore_lazy_array_slot`) -- feeding that override
        // back into the generator would corrupt every later term. See the
        // `generation_state` field doc on `LazyList`.
        //
        // The history is MOVED out of `generation_state` for the run and
        // moved back below (on every exit path): cloning it in and back out
        // cost O(history) per pull, which made a one-element-per-iteration
        // consumer quadratic (#10780).
        let mut history = list
            .generation_state
            .lock()
            .unwrap()
            .take()
            .unwrap_or_default();
        // `cache` and the history end at the same generator frontier, but
        // need not have the same length: a front mutation of a lazy
        // `@`-array (`shift`, `unshift`, `splice`) rewrites the cache's
        // prefix and leaves the generator's own history untouched, as
        // Rakudo's sequence iterator never sees the array it feeds (#10861).
        // So `needed` counts cache elements, and the cache receives exactly
        // the elements this call generates.
        let generated_from = history.len();
        let cached_len = list.cache.lock().unwrap().as_ref().map(Vec::len);
        let generated =
            self.generate_closure_sequence(list, &mut history, needed, generated_from, cached_len);
        if generated.is_ok() {
            // Append only the NEWLY generated tail to `cache` -- positions it
            // already had may hold a user override and must not be clobbered
            // (see `fill_sequence_cache`'s matching comment).
            let mut cache = list.cache.lock().unwrap();
            match cache.as_mut() {
                Some(cached) => cached.extend_from_slice(&history[generated_from..]),
                None => *cache = Some(history.clone()),
            }
        } else {
            history.truncate(generated_from);
        }
        // Publish the extended PRISTINE history back to `generation_state`.
        *list.generation_state.lock().unwrap() = Some(history);
        generated
    }

    /// Run a closure sequence's generator until the cache would hold
    /// `needed` elements (its `cached_len` plus what this call appends past
    /// `generated_from`) or the sequence ends.
    // Cost: O(1) amortized per new element, plus the generator call.
    fn generate_closure_sequence(
        &mut self,
        list: &LazyList,
        history: &mut Vec<Value>,
        needed: usize,
        generated_from: usize,
        cached_len: Option<usize>,
    ) -> Result<(), RuntimeError> {
        let state_mutex = list.closure_seq.as_ref().unwrap();
        let mut guard = state_mutex.lock().unwrap();
        let state = &mut *guard;
        let generator = state.generator.clone();

        // Elements the cache will hold once this call's output is appended.
        let have = |history: &Vec<Value>| match cached_len {
            Some(c) => c + (history.len() - generated_from),
            None => history.len(),
        };
        while have(history) < needed && !state.finished {
            match self.sequence_closure_step(&generator, history, state.generator_shape, false)? {
                // A generator that `slip`s multiple values (`{ slip $^a+1, $^b*2 }`)
                // contributes each as its own sequence element — flatten the Slip
                // into the history so the next step's `$^a`/`$^b` see the newest
                // elements (mirrors the eager `result.extend(items_to_add)` path).
                Some(v) => {
                    let items: Vec<Value> = match v.view() {
                        crate::value::ValueView::Slip(items) => items.iter().cloned().collect(),
                        _ => vec![v],
                    };
                    for item in items {
                        let reached_endpoint = state.endpoint.as_ref().is_some_and(|endpoint| {
                            if let crate::value::ValueView::Package(type_name) = endpoint.view() {
                                Self::seq_type_matches(&item, &type_name.resolve())
                            } else {
                                Self::seq_values_equal(&item, endpoint)
                            }
                        });
                        if reached_endpoint {
                            if !state.exclude_endpoint {
                                history.push(item);
                            }
                            history.extend(state.post_endpoint.iter().cloned());
                            state.finished = true;
                            break;
                        }
                        history.push(item);
                    }
                }
                None => {
                    state.finished = true;
                    break;
                }
            }
        }

        Ok(())
    }

    /// Extend a sequence-spec lazy list's cache to at least `needed` elements.
    /// This generates new elements using the sequence spec (arithmetic/geometric)
    /// without needing any Interpreter or interpreter context.
    // Cost: O(needed) for the copy, plus O(1) per new element.
    pub(super) fn extend_sequence_cache(
        list: &LazyList,
        spec: &crate::value::SequenceSpec,
        needed: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        Self::fill_sequence_cache(list, spec, needed);
        Ok(list.cache_window(0, needed))
    }

    /// [`Self::extend_sequence_cache`] without the copy.
    // Cost: O(1) amortized per new element.
    pub(super) fn fill_sequence_cache(
        list: &LazyList,
        spec: &crate::value::SequenceSpec,
        needed: usize,
    ) {
        let cached_len = {
            let cache = list.cache.lock().unwrap();
            let cached_len = cache.as_ref().map_or(0, Vec::len);
            if cached_len >= needed {
                return;
            }
            cached_len
        };
        // Generate new elements from `generation_state` -- the sequence's OWN
        // trailing history -- NOT from `cache`. `cache` is the user-visible
        // prefix and may hold an element an `@`-array mutation overwrote in
        // place (`Interpreter::restore_lazy_array_slot`); extending from it
        // would let that override corrupt later terms (raku: `@a[2]=99` on
        // `1,2,4...Inf` still computes `8` at index 3, not a value derived
        // from `99`). See the `generation_state` field doc on `LazyList`.
        let mut gen_state = list.generation_state.lock().unwrap();
        let items = gen_state.get_or_insert_with(Vec::new);
        while items.len() < needed {
            let last = items.last().cloned().unwrap_or(Value::int(0));
            let next = match spec {
                crate::value::SequenceSpec::Arithmetic { step, all_int } => {
                    if *all_int {
                        if let ValueView::Int(n) = last.view() {
                            Value::int(n + step)
                        } else {
                            let n = last.to_f64();
                            Value::num(n + *step as f64)
                        }
                    } else {
                        let n = last.to_f64();
                        Value::num(n + *step as f64)
                    }
                }
                crate::value::SequenceSpec::GeometricRat { num, den } => {
                    crate::runtime::Interpreter::seq_mul_rat(&last, *num, *den)
                }
                crate::value::SequenceSpec::Geometric { ratio } => {
                    let n = last.to_f64();
                    Value::num(n * ratio)
                }
                crate::value::SequenceSpec::Succ => {
                    // `unbounded_range::first` only seeds a value that has a
                    // successor, and `.succ` of one has one too.
                    crate::builtins::value_succ(&last).unwrap_or(last)
                }
                crate::value::SequenceSpec::RollPool(pool) => {
                    let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize
                        % pool.len();
                    pool[idx].clone()
                }
            };
            items.push(next);
        }
        // Append only the freshly generated tail to `cache` -- positions it
        // already had (0..old cache len) may hold a user override and must
        // not be clobbered; `cache` and `generation_state` otherwise grow in
        // lockstep, so the tail beyond the current cache length is exactly
        // what generation just produced. Only that tail is copied: cloning
        // the whole history here made a one-element-per-pull consumer
        // quadratic (#10780).
        let tail = items
            .get(cached_len..)
            .map(<[Value]>::to_vec)
            .unwrap_or_default();
        drop(gen_state);
        let mut cache = list.cache.lock().unwrap();
        let cached = cache.get_or_insert_with(Vec::new);
        if cached.len() == cached_len {
            cached.extend(tail);
        }
    }
}
