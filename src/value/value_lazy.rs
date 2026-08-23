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
            generation_state: Mutex::new(self.generation_state.lock().unwrap().clone()),
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
            array_context: self.array_context,
            list_context: self.list_context,
            cached_no_sink: self.cached_no_sink,
        }
    }
}

impl LazyList {
    /// Whether this lazy list was assigned into an `@` array (see the
    /// `array_context` field doc on `LazyList`).
    pub(crate) fn in_array_context(&self) -> bool {
        self.array_context
    }

    /// True when this list is genuinely lazy (`.is-lazy`), so gist/Str/raku
    /// render a placeholder instead of materializing it. CatHandle pullers
    /// are intentionally excluded: their backing iterator is lazy internally,
    /// but Rakudo exposes them as eager `Seq`s.
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

    /// Whether this list is backed by a live `IO::CatHandle` iterator.
    /// CatHandle iterators pull lazily internally, but Rakudo exposes both
    /// `.lines` and `.handles` as eager `Seq` values (`.is-lazy` is `False`).
    pub(crate) fn is_cat_pull(&self) -> bool {
        self.cat_pull.is_some()
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
        self.sequence_spec.is_some()
            || self
                .closure_seq
                .as_ref()
                .is_some_and(|state| state.lock().unwrap().endpoint.is_none())
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
            ValueView::Array(..) | ValueView::Seq(_) => true,
            ValueView::Slip(_) => true,
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
            ValueView::Junction { values, .. } => values.iter().all(Self::value_source_is_finite),
            _ => false,
        }
    }

    /// Gate for the VM force/incremental-pull dispatch block: a gather-sourced
    /// list (eager or `lazy`), an infinite sequence/closure spec, or a lazy
    /// `WALK(method)()` candidate-invocation list.
    pub(crate) fn needs_vm_lazy_dispatch(&self) -> bool {
        self.is_from_gather()
            || self.is_infinite_spec()
            || self.closure_seq.is_some()
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
        cloned.array_context = true;
        cloned
    }

    /// Whether this lazy list was coerced via `.List` (so `.WHAT` is `List`,
    /// not the default `Seq`). Mutually exclusive with array context in practice.
    pub(crate) fn in_list_context(&self) -> bool {
        self.list_context
    }

    /// Return a clone of this list tagged as a `.List`-coerced list. Preserves
    /// laziness (the generator is untouched) while making `.WHAT` report `List`.
    pub(crate) fn with_list_context(&self) -> Self {
        let mut cloned = self.clone();
        cloned.list_context = true;
        cloned
    }

    /// Whether this list is a `.cache`-returned view whose sink is a no-op
    /// (see the `cached_no_sink` field doc on `LazyList`).
    pub(crate) fn is_cached_no_sink(&self) -> bool {
        self.cached_no_sink
    }

    /// Return a clone tagged as a `.cache`-returned view (no-op on sink).
    pub(crate) fn with_cached_no_sink(&self) -> Self {
        let mut cloned = self.clone();
        cloned.cached_no_sink = true;
        cloned
    }

    /// The shared `.cache` `LazyList` arm: a genuinely-lazy list (infinite
    /// sequence, lazy pipe, cat-handle pull, …) must stay lazy under
    /// `.cache` — Rakudo's `.cache` reifies and caches elements on demand,
    /// it does not force the whole list. Returns the cached, list-context
    /// view (tagged both "sink is a no-op" and "`.WHAT` reports `List`")
    /// when this list needs that treatment; `None` when `.cache` should fall
    /// through to the generic Positional/Seq caching path instead.
    ///
    /// This logic was copy-pasted at five call sites (builtins, the runtime
    /// slow-path dispatcher, and the three VM call-method paths) before
    /// being collapsed here — see
    /// `todo/tickets/collapse-lazylist-cache-copies.md`.
    pub(crate) fn cache_lazy_view(&self) -> Option<Value> {
        if self.is_genuinely_lazy() || self.is_cat_pull() {
            Some(Value::lazy_list(crate::gc::Gc::new(
                self.with_cached_no_sink().with_list_context(),
            )))
        } else {
            None
        }
    }
}

// Constructors (`new_cached`, `new_sequence`, ...) and the scan-reduction
// forcer live in `value_lazy_ctors.rs` (same `impl LazyList` split across
// files to keep both under the repo's 500-line-per-file convention).

impl Clone for LazyThunkData {
    fn clone(&self) -> Self {
        Self {
            thunk: self.thunk.clone(),
            cache: Mutex::new(self.cache.lock().unwrap().clone()),
        }
    }
}
