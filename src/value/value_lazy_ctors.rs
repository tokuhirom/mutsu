use super::*;

// `LazyList` constructors, split out of `value_lazy.rs` (which holds the
// `Debug`/`Clone` impls and the accessor methods) to keep both files under the
// repo's 500-line-per-file convention. The scan-reduction forcer and the
// lazy index-pipe method stage live in `builtins::lazy_scan`, and the
// map/grep/index/adaptor pipe constructors (which route an unbounded source
// through `runtime::unbounded_range`) in `runtime::lazy_pipe_ctors` (#10779).

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
