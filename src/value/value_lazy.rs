use super::*;

impl std::fmt::Debug for LazyList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LazyList")
            .field("body_len", &self.body.len())
            .field("has_compiled_code", &self.compiled_code.is_some())
            .field("has_coroutine", &self.coroutine.is_some())
            .field("has_lazy_pipe", &self.lazy_pipe.is_some())
            .finish()
    }
}

impl Clone for LazyList {
    fn clone(&self) -> Self {
        Self {
            body: self.body.clone(),
            env: self.env.clone(),
            cache: Mutex::new(self.cache.lock().unwrap().clone()),
            compiled_code: self.compiled_code.clone(),
            compiled_fns: self.compiled_fns.clone(),
            elems_count: self.elems_count.clone(),
            scan_spec: self
                .scan_spec
                .as_ref()
                .map(|s| Mutex::new(s.lock().unwrap().clone())),
            sequence_spec: self.sequence_spec.clone(),
            coroutine: self
                .coroutine
                .as_ref()
                .map(|c| Mutex::new(c.lock().unwrap().clone())),
            lazy_pipe: self
                .lazy_pipe
                .as_ref()
                .map(|p| Mutex::new(p.lock().unwrap().clone())),
            closure_seq: self
                .closure_seq
                .as_ref()
                .map(|c| Mutex::new(c.lock().unwrap().clone())),
        }
    }
}

impl LazyList {
    /// Marker key inserted into `env` when a lazy list is bound/assigned into
    /// an `@` array slot. A `Value::LazyList` carries no `[`-vs-`(` context on
    /// its own, so this flag lets gist/`.WHAT` render `[...]`/`Array` (held in
    /// `@a`) instead of `(...)`/`Seq` (a bare `$s` Seq).
    pub(crate) const ARRAY_CONTEXT_MARKER: &'static str = "__mutsu_lazylist_array_context";

    /// Whether this lazy list was assigned into an `@` array (see marker above).
    pub(crate) fn in_array_context(&self) -> bool {
        matches!(
            self.env.get(Self::ARRAY_CONTEXT_MARKER),
            Some(Value::Bool(true))
        )
    }

    /// True when this list is genuinely lazy (`.is-lazy`), so gist/Str/raku
    /// render a placeholder instead of materializing it.
    ///
    /// An infinite sequence/closure/scan/map-grep generator is only ever stored
    /// as a *live* `LazyList` when actually infinite (finite ones materialize to
    /// a `Seq`), so those specs are unconditionally lazy. A gather coroutine (or
    /// unevaluated body), however, is lazy **only** when explicitly marked
    /// `lazy` — a plain `gather` is `.is-lazy` `False` in Rakudo and must
    /// materialize on gist/Str rather than render a placeholder.
    pub(crate) fn is_genuinely_lazy(&self) -> bool {
        if self.sequence_spec.is_some()
            || self.lazy_pipe.is_some()
            || self.closure_seq.is_some()
            || self.scan_spec.is_some()
        {
            return true;
        }
        (self.coroutine.is_some() || !self.body.is_empty() || self.compiled_code.is_some())
            && self.is_lazy_marked()
    }

    /// Whether this list was produced from a `gather` block (carries the
    /// `__mutsu_lazylist_from_gather` env marker).
    pub(crate) fn is_from_gather(&self) -> bool {
        matches!(
            self.env.get("__mutsu_lazylist_from_gather"),
            Some(Value::Bool(true))
        )
    }

    /// Whether this list is an infinite arithmetic/geometric sequence
    /// (`1..*`, `1,2,3...*`) or an infinite closure sequence (`1,1,*+*...*`).
    /// These reify on demand via `force_lazy_list_vm_n` (`extend_sequence_cache`
    /// / `extend_closure_sequence`), so a method that needs the whole list must
    /// raise `X::Cannot::Lazy` rather than read the (tiny) seed cache (L2b).
    pub(crate) fn is_infinite_spec(&self) -> bool {
        self.sequence_spec.is_some() || self.closure_seq.is_some()
    }

    /// Gate for the VM force/incremental-pull dispatch block: a gather-sourced
    /// list (eager or `lazy`) or an infinite sequence/closure spec.
    pub(crate) fn needs_vm_lazy_dispatch(&self) -> bool {
        self.is_from_gather() || self.is_infinite_spec()
    }

    /// Whether this list carries the `lazy` prefix marker (set by the `lazy`
    /// statement prefix / `.lazy` method).
    fn is_lazy_marked(&self) -> bool {
        matches!(
            self.env.get("__mutsu_preserve_lazy_on_array_assign"),
            Some(Value::Bool(true))
        )
    }

    /// Return a clone of this list tagged as living in `@` array context.
    pub(crate) fn with_array_context(&self) -> Self {
        let mut cloned = self.clone();
        cloned
            .env
            .insert(Self::ARRAY_CONTEXT_MARKER.to_string(), Value::Bool(true));
        cloned
    }

    pub(crate) const LIST_CONTEXT_MARKER: &'static str = "__mutsu_lazylist_list_context";

    /// Whether this lazy list was coerced via `.List` (so `.WHAT` is `List`,
    /// not the default `Seq`). Mutually exclusive with array context in practice.
    pub(crate) fn in_list_context(&self) -> bool {
        matches!(
            self.env.get(Self::LIST_CONTEXT_MARKER),
            Some(Value::Bool(true))
        )
    }

    /// Return a clone of this list tagged as a `.List`-coerced list. Preserves
    /// laziness (the generator is untouched) while making `.WHAT` report `List`.
    pub(crate) fn with_list_context(&self) -> Self {
        let mut cloned = self.clone();
        cloned
            .env
            .insert(Self::LIST_CONTEXT_MARKER.to_string(), Value::Bool(true));
        cloned
    }

    /// Create a pre-cached lazy list (no body to evaluate).
    pub(crate) fn new_cached(items: Vec<Value>) -> Self {
        Self {
            body: Vec::new(),
            env: crate::env::Env::new(),
            cache: Mutex::new(Some(items)),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: None,
            closure_seq: None,
        }
    }

    /// Create an infinite sequence lazy list that can generate elements on demand.
    pub(crate) fn new_sequence(seeds: Vec<Value>, spec: SequenceSpec) -> Self {
        Self {
            body: Vec::new(),
            env: crate::env::Env::new(),
            cache: Mutex::new(Some(seeds)),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: Some(spec),
            coroutine: None,
            lazy_pipe: None,
            closure_seq: None,
        }
    }

    /// Create a lazy scan (triangle reduce) list that computes elements on demand.
    pub(crate) fn new_scan(spec: ScanSpec) -> Self {
        Self {
            body: Vec::new(),
            env: crate::env::Env::new(),
            cache: Mutex::new(Some(Vec::new())),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: Some(Mutex::new(spec)),
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: None,
            closure_seq: None,
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
        }
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
            cache: Mutex::new(Some(seeds)),
            compiled_code: None,
            compiled_fns: None,
            elems_count: None,
            scan_spec: None,
            sequence_spec: None,
            coroutine: None,
            lazy_pipe: None,
            closure_seq: Some(Mutex::new(state)),
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

impl Clone for LazyThunkData {
    fn clone(&self) -> Self {
        Self {
            thunk: self.thunk.clone(),
            cache: Mutex::new(self.cache.lock().unwrap().clone()),
        }
    }
}
