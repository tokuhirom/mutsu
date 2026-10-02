use super::*;

// `LazyList` constructors, split out of `value_lazy.rs` (which holds the
// `Debug`/`Clone` impls and the accessor methods) to keep both files under the
// repo's 500-line-per-file convention. The scan-reduction forcer and the
// lazy index-pipe method stage live in `builtins::lazy_scan` (issue #10779).

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
            itemized: false,
        }
    }

    /// Create a pre-cached lazy list that is *logically infinite*: the cache is
    /// only a bounded prefix, and `.is-lazy` must answer `True`
    /// (`LHS xx *`, `.pick(**)`, an `X`/`Z` with an infinite operand). Without
    /// the recorded `Inf` count nothing in the value distinguishes such a list
    /// from an ordinary finite cache, so `is_genuinely_lazy` could not see it.
    pub(crate) fn new_cached_infinite(items: Vec<Value>) -> Self {
        let mut ll = Self::new_cached(items);
        ll.elems_count = Some(Value::num(f64::INFINITY));
        ll
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
            itemized: false,
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
            itemized: false,
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
        // `.is-lazy` of a map/grep Seq delegates to its source, so a stage
        // over an explicitly `.lazy` list is lazy even when the list is
        // finite: `(1..5).lazy.map(* + 1).raku` is `(2, ...).lazy.Seq` (#10918).
        if let ValueView::LazyList(ll) = source.view()
            && ll.is_lazy_marked()
        {
            env.insert(
                "__mutsu_preserve_lazy_on_array_assign".to_string(),
                Value::Bool(true),
            );
        }
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
                source: crate::runtime::unbounded_range::pipe_source(source),
                func,
                is_grep,
                source_idx: 0,
                done: false,
                index_transform: None,
                adaptor: None,
            })),
            closure_seq: None,
            walk_pending: None,
            cat_pull: None,
            array_context: false,
            list_context: false,
            cached_no_sink: false,
            itemized: false,
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
                source: crate::runtime::unbounded_range::pipe_source(source),
                func: Value::Nil,
                is_grep: false,
                source_idx: 0,
                done: false,
                index_transform: Some(transform),
                adaptor: None,
            })),
            closure_seq: None,
            walk_pending: None,
            cat_pull: None,
            array_context: false,
            list_context: false,
            cached_no_sink: false,
            itemized: false,
        }
    }

    /// Create a lazy stage driven by a stateful [`PipeAdaptor`] over `source`
    /// (`.skip`/`.rotor`/`.unique`/.../`Z`/`X`/`roundrobin`, #9159). `func`
    /// is only read by [`PipeAdaptor::MultiMap`]. For the multi-operand
    /// adaptors `source` is the first operand; the adaptor pulls from its own
    /// operand list.
    pub(crate) fn new_adaptor_pipe(source: Value, func: Value, adaptor: PipeAdaptor) -> Self {
        let mut ll = Self::new_pipe(source, func, false);
        if let Some(spec) = ll.lazy_pipe.as_mut() {
            spec.get_mut().unwrap_or_else(|e| e.into_inner()).adaptor = Some(Box::new(adaptor));
        }
        ll
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
            itemized: false,
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
            itemized: false,
        }
    }
}
