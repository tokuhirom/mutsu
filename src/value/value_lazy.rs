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
            walk_pending: self
                .walk_pending
                .as_ref()
                .map(|w| Mutex::new(w.lock().unwrap().clone())),
            cat_pull: self
                .cat_pull
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
            self.env.get(Self::ARRAY_CONTEXT_MARKER).map(Value::view),
            Some(ValueView::Bool(true))
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
        self.sequence_spec.is_some()
            || self.lazy_pipe.is_some()
            || self.closure_seq.is_some()
            || self.scan_spec.is_some()
            || self.cat_pull.is_some()
            // The `__mutsu_preserve_lazy_on_array_assign` marker is set
            // exclusively by an explicit `lazy` prefix / `.lazy` method call
            // (see `dispatch_core_str.rs`), including on an already-finite
            // list (`lazy 3,4,5` caches its 3 items but stays `.is-lazy` True
            // in Rakudo) — so the marker alone is sufficient regardless of
            // whether the list also carries a coroutine/body/compiled_code.
            || self.is_lazy_marked()
    }

    /// Whether gist/Str/raku should render a `...` placeholder rather than
    /// materializing this list. True for genuinely-lazy lists EXCEPT a
    /// `cat_pull` (`IO::CatHandle.lines`/`.handles`), which is finite — it reads
    /// to the end of the cat's handles — so it must materialize and render its
    /// elements (and compare structurally under `is-deeply`).
    pub(crate) fn renders_lazy_placeholder(&self) -> bool {
        self.is_genuinely_lazy() && self.cat_pull.is_none()
    }

    /// Whether iterating this list could hang or be unsafe to consume twice
    /// right now (a live generator with no complete cache yet) — as opposed
    /// to `is_genuinely_lazy()`, which answers `.is-lazy` and is also True for
    /// an explicitly `lazy`-marked but ALREADY fully-cached, finite list
    /// (`lazy 3,4,5`, or `(lazy ^2).cache`). `eqv` on two such same-type lazy
    /// operands must throw ONLY when forcing could actually hang/misbehave —
    /// a cache-only list (no coroutine/sequence_spec/etc, regardless of the
    /// `lazy` marker) is safe to compare (roast S03-operators/eqv.t: "eqv
    /// between identical lazy Seqs does not die" after `.cache`).
    pub(crate) fn eqv_would_hang(&self) -> bool {
        self.sequence_spec.is_some()
            || self.lazy_pipe.is_some()
            || self.closure_seq.is_some()
            || self.scan_spec.is_some()
            || self.cat_pull.is_some()
            || ((self.coroutine.is_some() || !self.body.is_empty() || self.compiled_code.is_some())
                && self.is_lazy_marked())
    }

    /// Whether this list was produced from a `gather` block (carries the
    /// `__mutsu_lazylist_from_gather` env marker).
    pub(crate) fn is_from_gather(&self) -> bool {
        matches!(
            self.env
                .get("__mutsu_lazylist_from_gather")
                .map(Value::view),
            Some(ValueView::Bool(true))
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

    /// Whether this `.map`/`.grep` lazy pipe bottoms out in a *definitively
    /// finite* source, so a strict reification (`.List`/`for`/`.flat`/gist) can
    /// force it to completion instead of keeping it lazy. Conservative: returns
    /// `true` ONLY when every stage of the source chain is provably finite
    /// (a `gather` coroutine — gathers always terminate —, a finite `Array`/
    /// `Seq`/`Slip`, or a finite `Range`); returns `false` for an infinite range,
    /// an infinite sequence/closure spec, a `cat_pull`, or any unrecognized
    /// source. Worst case a genuinely-finite pipe stays lazy (status quo) — it
    /// never turns an infinite pipe into a hang.
    pub(crate) fn pipe_bottoms_out_finite(&self) -> bool {
        let spec = match self.lazy_pipe.as_ref() {
            Some(p) => p,
            None => return false,
        };
        let source = spec.lock().unwrap().source.clone();
        Self::value_source_is_finite(&source)
    }

    fn value_source_is_finite(source: &Value) -> bool {
        match source.view() {
            ValueView::Array(..) | ValueView::Seq(_) | ValueView::Slip(_) => true,
            // A finite integer range has a concrete end (`i64::MAX` is the
            // sentinel for `..*`/`..Inf`, i.e. infinite).
            ValueView::Range(_, b)
            | ValueView::RangeExcl(_, b)
            | ValueView::RangeExclStart(_, b)
            | ValueView::RangeExclBoth(_, b) => b != i64::MAX,
            ValueView::GenericRange { end, .. } => {
                let end_f = end.to_f64();
                !(end_f.is_infinite() && end_f.is_sign_positive())
            }
            ValueView::LazyList(ll) => {
                if ll.lazy_pipe.is_some() {
                    ll.pipe_bottoms_out_finite()
                } else if ll.is_infinite_spec() || ll.cat_pull.is_some() {
                    false
                } else {
                    // A gather coroutine (or an already-materialized gather body)
                    // is finite; sequence/closure/cat specs were ruled out above.
                    ll.coroutine.is_some() || ll.is_from_gather() || !ll.body.is_empty()
                }
            }
            _ => false,
        }
    }

    /// Gate for the VM force/incremental-pull dispatch block: a gather-sourced
    /// list (eager or `lazy`), an infinite sequence/closure spec, or a lazy
    /// `WALK(method)()` candidate-invocation list.
    pub(crate) fn needs_vm_lazy_dispatch(&self) -> bool {
        self.is_from_gather()
            || self.is_infinite_spec()
            || self.walk_pending.is_some()
            || self.cat_pull.is_some()
    }

    /// Whether `my @a = <this list>` keeps the list as a reify-on-demand lazy
    /// array (L2b step 6, docs/lazy-arrays.md) instead of eagerly
    /// materializing. An explicit `lazy` marker always preserves; otherwise
    /// only a *deterministic* unreifiable source does — an infinite
    /// arithmetic/geometric sequence spec, or a map/grep pipe over an
    /// infinite source. A pipe bottoming out in a finite source (a plain
    /// gather) and a finite cat-pull materialize eagerly, matching raku.
    ///
    /// TODO: closure_seq (`1, {rand} ... *`) and scan_spec stay on the old
    /// capped-Array path: S32-array/create.t "partially-reified" requires
    /// `@a.clone` to SHARE the reifier (clone and original see the same
    /// `{rand}` values), which needs a shared element-cell store —
    /// container-repr territory (ADR-0001 layer 3a), not a per-site fix.
    pub(crate) fn preserve_lazy_on_array_assign(&self) -> bool {
        self.is_lazy_marked()
            || self.sequence_spec.is_some()
            || (self.lazy_pipe.is_some() && !self.pipe_bottoms_out_finite())
    }

    /// Whether this list is genuinely *infinite / unreifiable* — an infinite
    /// `...` sequence spec, or a lazy map/grep pipe over an infinite source —
    /// as opposed to a merely `lazy`-marked but *finite* list (`lazy 1, 2`).
    /// This is `preserve_lazy_on_array_assign` MINUS the `is_lazy_marked` case.
    ///
    /// A `[...]` bracket-array keeps only these lazy (`.is-lazy` True, `.elems`
    /// throws `X::Cannot::Lazy`); a finite `[lazy 1, 2]` still materializes, so
    /// whole-array reads (`cmp`, element access) that would otherwise mis-read
    /// the tiny seed cache keep working.
    pub(crate) fn is_lazy_infinite(&self) -> bool {
        self.sequence_spec.is_some()
            || (self.lazy_pipe.is_some() && !self.pipe_bottoms_out_finite())
    }

    /// Whether this list carries the `lazy` prefix marker (set by the `lazy`
    /// statement prefix / `.lazy` method).
    pub(crate) fn is_lazy_marked(&self) -> bool {
        matches!(
            self.env
                .get("__mutsu_preserve_lazy_on_array_assign")
                .map(Value::view),
            Some(ValueView::Bool(true))
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
            self.env.get(Self::LIST_CONTEXT_MARKER).map(Value::view),
            Some(ValueView::Bool(true))
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

    /// Marker set by `.cache` on a genuinely-lazy list: the result is a cached,
    /// re-iterable view, so sinking it is a no-op (it must NOT drain the
    /// underlying source). A bare lazy Seq sunk still drains; only the
    /// `.cache`-returned view carries this marker.
    pub(crate) const CACHED_NO_SINK_MARKER: &'static str = "__mutsu_lazylist_cached_no_sink";

    /// Whether this list is a `.cache`-returned view whose sink is a no-op.
    pub(crate) fn is_cached_no_sink(&self) -> bool {
        matches!(
            self.env.get(Self::CACHED_NO_SINK_MARKER).map(Value::view),
            Some(ValueView::Bool(true))
        )
    }

    /// Return a clone tagged as a `.cache`-returned view (no-op on sink).
    pub(crate) fn with_cached_no_sink(&self) -> Self {
        let mut cloned = self.clone();
        cloned
            .env
            .insert(Self::CACHED_NO_SINK_MARKER.to_string(), Value::Bool(true));
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
            walk_pending: None,
            cat_pull: None,
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
            walk_pending: None,
            cat_pull: None,
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
            walk_pending: None,
            cat_pull: None,
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
            walk_pending: None,
            cat_pull: None,
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
            walk_pending: None,
            cat_pull: None,
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
            walk_pending: None,
            cat_pull: None,
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

impl Clone for LazyThunkData {
    fn clone(&self) -> Self {
        Self {
            thunk: self.thunk.clone(),
            cache: Mutex::new(self.cache.lock().unwrap().clone()),
        }
    }
}
