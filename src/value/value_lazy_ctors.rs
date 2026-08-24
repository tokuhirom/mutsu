use super::*;

// `LazyList` constructors and the scan-reduction forcer, split out of
// `value_lazy.rs` (which holds the `Debug`/`Clone` impls and the accessor
// methods) to keep both files under the repo's 500-line-per-file convention.

impl LazyList {
    /// Create a pre-cached lazy list (no body to evaluate).
    pub(crate) fn new_cached(items: Vec<Value>) -> Self {
        Self {
            body: Vec::new(),
            env: crate::env::Env::new(),
            cache: Mutex::new(Some(items)),
            generation_state: Mutex::new(None),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: None,
            closure_seq: None,
            walk_pending: None,
            cat_pull: None,
            array_context: false,
            list_context: false,
            cached_no_sink: false,
        }
    }

    /// Create an infinite sequence lazy list that can generate elements on demand.
    pub(crate) fn new_sequence(seeds: Vec<Value>, spec: SequenceSpec) -> Self {
        Self {
            body: Vec::new(),
            env: crate::env::Env::new(),
            cache: Mutex::new(Some(seeds.clone())),
            generation_state: Mutex::new(Some(seeds)),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: Some(spec),
            coroutine: None,
            lazy_pipe: None,
            closure_seq: None,
            walk_pending: None,
            cat_pull: None,
            array_context: false,
            list_context: false,
            cached_no_sink: false,
        }
    }

    /// Create a lazy scan (triangle reduce) list that computes elements on demand.
    pub(crate) fn new_scan(spec: ScanSpec) -> Self {
        Self {
            body: Vec::new(),
            env: crate::env::Env::new(),
            cache: Mutex::new(Some(Vec::new())),
            generation_state: Mutex::new(None),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: Some(Mutex::new(spec)),
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: None,
            closure_seq: None,
            walk_pending: None,
            cat_pull: None,
            array_context: false,
            list_context: false,
            cached_no_sink: false,
        }
    }

    /// Create a lazy `map`/`grep` pipeline stage over `source`.
    ///
    /// The result stays lazy: its elements are produced on demand by pulling
    /// from `source` and applying `func`. The `__mutsu_lazylist_from_gather`
    /// marker is set so the VM's `.head`/`.first`/index dispatch routes through
    /// the bounded incremental-pull path.
    pub(crate) fn new_pipe(source: Value, func: Value, is_grep: bool) -> Self {
        let mut env = crate::env::Env::new();
        env.insert(
            "__mutsu_lazylist_from_gather".to_string(),
            Value::Bool(true),
        );
        Self {
            body: Vec::new(),
            env,
            cache: Mutex::new(Some(Vec::new())),
            generation_state: Mutex::new(None),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: Some(Mutex::new(MapGrepSpec {
                source,
                func,
                is_grep,
                source_idx: 0,
                done: false,
                index_transform: None,
            })),
            closure_seq: None,
            walk_pending: None,
            cat_pull: None,
            array_context: false,
            list_context: false,
            cached_no_sink: false,
        }
    }

    /// Create a lazy `.pairs`/`.antipairs`/`.kv` stage over `source`.
    ///
    /// Stays lazy (carries the gather + preserve markers so array assignment
    /// keeps it lazy, matching Rakudo where these methods are `.is-lazy` over a
    /// lazy list). Elements are produced on demand by pulling from `source` and
    /// applying the index transform with the source position as the key.
    pub(crate) fn new_index_pipe(source: Value, transform: IndexTransform) -> Self {
        let mut env = crate::env::Env::new();
        env.insert(
            "__mutsu_lazylist_from_gather".to_string(),
            Value::Bool(true),
        );
        env.insert(
            "__mutsu_preserve_lazy_on_array_assign".to_string(),
            Value::Bool(true),
        );
        Self {
            body: Vec::new(),
            env,
            cache: Mutex::new(Some(Vec::new())),
            generation_state: Mutex::new(None),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: Some(Mutex::new(MapGrepSpec {
                source,
                func: Value::Nil,
                is_grep: false,
                source_idx: 0,
                done: false,
                index_transform: Some(transform),
            })),
            closure_seq: None,
            walk_pending: None,
            cat_pull: None,
            array_context: false,
            list_context: false,
            cached_no_sink: false,
        }
    }

    /// Create the lazy view used by the left-exclusive sequence operators.
    /// The view drops one item as it pulls, preserving any generator carried
    /// by the source instead of snapshotting its currently-realized cache.
    pub(crate) fn new_skip_first_pipe(source: Value) -> Self {
        Self::new_index_pipe(source, IndexTransform::SkipFirst)
    }

    /// Create an infinite closure-based sequence (`1, 1, * + * ... *`).
    ///
    /// `seeds` is the initial element history (already includes any eagerly
    /// generated prefix); `state` carries the generator closure so more
    /// elements can be produced on demand via the VM.
    pub(crate) fn new_closure_sequence(seeds: Vec<Value>, state: ClosureSeqState) -> Self {
        Self {
            body: Vec::new(),
            env: crate::env::Env::new(),
            cache: Mutex::new(Some(seeds.clone())),
            generation_state: Mutex::new(Some(seeds)),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: None,
            closure_seq: Some(Mutex::new(state)),
            walk_pending: None,
            cat_pull: None,
            array_context: false,
            list_context: false,
            cached_no_sink: false,
        }
    }

    /// Create a lazy `IO::CatHandle.lines` / `.handles` list backed by a live
    /// cat instance (sharing its attribute cell). Each element is pulled on
    /// demand by reading from / advancing the cat, so mid-iteration changes to
    /// the cat's attributes take effect.
    pub(crate) fn new_cat_pull(cat: Value, mode: crate::value::CatPullMode) -> Self {
        Self {
            body: Vec::new(),
            env: crate::env::Env::new(),
            cache: Mutex::new(Some(Vec::new())),
            generation_state: Mutex::new(None),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: None,
            closure_seq: None,
            walk_pending: None,
            cat_pull: Some(Mutex::new(crate::value::CatPullSpec {
                cat,
                mode,
                started: false,
                done: false,
            })),
            array_context: false,
            list_context: false,
            cached_no_sink: false,
        }
    }

    /// Force a scan-based lazy list to compute up to `needed` elements.
    /// Uses builtin arithmetic for common operators. Returns the cached elements.
    /// This can be called from contexts without VM access (builtins, interpreter).
    pub(crate) fn force_scan_to(&self, needed: usize) -> Vec<Value> {
        let scan_mutex = match &self.scan_spec {
            Some(s) => s,
            None => return self.cache.lock().unwrap().clone().unwrap_or_default(),
        };

        let mut spec = scan_mutex.lock().unwrap();
        let mut cache_guard = self.cache.lock().unwrap();
        let out = cache_guard.get_or_insert_with(Vec::new);

        if out.len() >= needed {
            return out[..needed].to_vec();
        }

        let remaining = needed - out.len();
        let already = spec.computed_count;
        let source = spec.source.clone();
        let base_op = spec.op.clone();
        let negate = spec.negate;

        // Generate source values
        let new_values: Vec<Value> = match source.view() {
            ValueView::Range(a, b) => {
                let start = a + already as i64;
                let end = if b == i64::MAX { a + needed as i64 } else { b };
                (start..=end).take(remaining).map(Value::Int).collect()
            }
            ValueView::RangeExcl(a, b) => {
                let start = a + already as i64;
                let end = if b == i64::MAX { a + needed as i64 } else { b };
                (start..end).take(remaining).map(Value::Int).collect()
            }
            _ => {
                let items = crate::runtime::utils::value_to_list(&source);
                items.into_iter().skip(already).take(remaining).collect()
            }
        };

        let mut acc = spec.accumulator.clone();
        for val in new_values {
            acc = Some(match acc.take() {
                None => {
                    out.push(val.clone());
                    val
                }
                Some(prev) => {
                    let v = Self::scan_binary_op(&base_op, prev, val);
                    let v = if negate { Value::Bool(!v.truthy()) } else { v };
                    out.push(v.clone());
                    v
                }
            });
            spec.computed_count += 1;
        }
        spec.accumulator = acc;
        out.clone()
    }

    /// Apply a binary operator for scan reduction. Supports common builtin ops.
    fn scan_binary_op(op: &str, left: Value, right: Value) -> Value {
        match op {
            "+" => crate::builtins::arith::arith_add(left, right).unwrap_or(Value::Nil),
            "-" => crate::builtins::arith::arith_sub(left, right),
            "*" => crate::builtins::arith::arith_mul(left, right),
            "/" => crate::builtins::arith::arith_div(left, right).unwrap_or(Value::Nil),
            "%" | "mod" => crate::builtins::arith::arith_mod(left, right).unwrap_or(Value::Nil),
            "**" => crate::builtins::arith::arith_pow(left, right),
            "~" => Value::str(format!(
                "{}{}",
                left.to_string_value(),
                right.to_string_value()
            )),
            "max" => {
                if left.to_f64() >= right.to_f64() {
                    left
                } else {
                    right
                }
            }
            "min" => {
                if left.to_f64() <= right.to_f64() {
                    left
                } else {
                    right
                }
            }
            _ => Value::Nil, // Unsupported op — VM path handles these
        }
    }
}
