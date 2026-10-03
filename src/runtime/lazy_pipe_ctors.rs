//! `LazyList` pipe-stage constructors (`map`/`grep`, index transforms,
//! stateful adaptors). They live above `value` because every pipe source goes
//! through [`crate::runtime::unbounded_range::pipe_source`], which needs the
//! `.succ`/`+` builtins to step an unbounded range (#10779).

use crate::value::{IndexTransform, LazyList, MapGrepSpec, PipeAdaptor, Value, ValueView};
use std::sync::Mutex;

impl LazyList {
    /// Create a lazy `map`/`grep` pipeline stage over `source`.
    ///
    /// The result stays lazy: its elements are produced on demand by pulling
    /// from `source` and applying `func`. The `__mutsu_lazylist_from_gather`
    /// marker is set so the VM's `.head`/`.first`/index dispatch routes through
    /// the bounded incremental-pull path.
    pub(crate) fn new_pipe(source: Value, func: Value, is_grep: bool) -> Self {
        let mut env = crate::env::Env::new();
        env.insert("__mutsu_lazylist_from_gather".to_string(), Value::TRUE);
        // `.is-lazy` of a map/grep Seq delegates to its source, so a stage
        // over an explicitly `.lazy` list is lazy even when the list is
        // finite: `(1..5).lazy.map(* + 1).raku` is `(2, ...).lazy.Seq` (#10918).
        if let ValueView::LazyList(ll) = source.view()
            && ll.is_lazy_marked()
        {
            env.insert(
                "__mutsu_preserve_lazy_on_array_assign".to_string(),
                Value::TRUE,
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
        env.insert("__mutsu_lazylist_from_gather".to_string(), Value::TRUE);
        env.insert(
            "__mutsu_preserve_lazy_on_array_assign".to_string(),
            Value::TRUE,
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
                func: Value::NIL,
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
}
