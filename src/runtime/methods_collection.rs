use super::*;

impl Interpreter {
    pub(super) fn dispatch_to_set_with_what(
        &self,
        target: Value,
        what: &str,
    ) -> Result<Value, RuntimeError> {
        crate::builtins::quanthash_coerce::to_set(target, what)
    }

    /// Check if a value is lazy/infinite and cannot be coerced to a QuantHash.
    pub(crate) fn is_lazy_for_coerce(value: &Value) -> bool {
        match value {
            Value::LazyList(_) => true,
            Value::Array(_, kind) if kind.is_lazy() => true,
            Value::GenericRange { start, end, .. } => {
                Self::is_infinite_endpoint(start) || Self::is_infinite_endpoint(end)
            }
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            _ => false,
        }
    }

    /// Operations that need the full (finite) list cannot run on a lazy or
    /// infinite source. Returns `Some(X::Cannot::Lazy)` when `method` is such an
    /// operation and `target` is lazy/infinite, else `None`. Matches raku, which
    /// throws rather than hanging while materializing the whole sequence.
    /// `.head`/`.first`/`.map`/`[]` handle laziness and are NOT listed here.
    pub(crate) fn lazy_guard_error(method: &str, target: &Value) -> Option<RuntimeError> {
        if !Self::is_lazy_for_coerce(target) {
            return None;
        }
        match method {
            "classify" | "categorize" => Some(RuntimeError::cannot_lazy_with_action(
                method,
                "Hash[Any,Mu]",
            )),
            "sort" | "combinations" | "permutations" => Some(RuntimeError::cannot_lazy(method)),
            _ => None,
        }
    }

    /// Whether `.map`/`.grep` on `target` should produce a truly lazy pipeline
    /// stage (a `LazyList` carrying a [`crate::value::MapGrepSpec`]) instead of
    /// materializing the source. Intentionally narrow to keep the blast radius
    /// small: only an infinite integer `Range` (the headline crash/slow case)
    /// and continuation of an existing lazy pipeline (chained `.map`/`.grep`).
    /// Gathers / sequence / scan lists keep their current dispatch unchanged.
    pub(crate) fn is_lazy_pipe_source(target: &Value) -> bool {
        match target {
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            // An exclusive/whatever infinite range (`^Inf`, `0..^Inf`) is stored
            // as a GenericRange with an infinite end — also a lazy pipe source.
            Value::GenericRange { end, .. } => {
                let end_f = end.to_f64();
                end_f.is_infinite() && end_f.is_sign_positive()
            }
            // A lazy pipe, or an infinite arithmetic/geometric/closure sequence
            // (`1..*`, `1,2,3...*`, `1,1,*+*...*`): `.map`/`.grep` append another
            // pipe stage that pulls from the sequence on demand (L2b), instead of
            // materializing the (now O(1)-seeded) cache.
            //
            // A gather-sourced list is also a lazy pipe source: its coroutine can
            // be resumed one `take` at a time, so `.map`/`.grep` over it must pull
            // on demand instead of forcing the whole gather body (which would run
            // trailing side effects the consumer never asked for). This holds for a
            // *plain* gather too — in Rakudo `gather { … }.grep(…)[^3]` pulls only
            // the elements the slice needs and never runs the gather's tail.
            Value::LazyList(ll) => ll.lazy_pipe.is_some() || ll.needs_vm_lazy_dispatch(),
            _ => false,
        }
    }

    /// Build a lazy `map`/`grep` pipeline stage over `target` if `func` is
    /// eligible for single-element pull (arity-1, no full-binding signature and
    /// no grep adverbs handled by the caller). Returns `None` to fall back to
    /// the eager path.
    pub(crate) fn make_lazy_pipe(target: Value, func: Value, is_grep: bool) -> Option<Value> {
        // Only callbacks that consume one element per call can be pulled lazily.
        // A multi-arity block (`-> $a, $b { }`) or a slurpy param (`*@a`)
        // consumes a chunk of the source per call, which single-element pull
        // cannot reproduce; defer those to the eager path (unchanged behaviour).
        // Other signature features (types, defaults, where, …) still bind one
        // element per call in `eval_map_over_items`, so they stay eligible.
        if let Value::Sub(data) = &func {
            let arity = data
                .params
                .len()
                .saturating_sub(data.assumed_positional.len());
            let has_slurpy = data.param_defs.iter().any(|pd| pd.slurpy);
            if arity > 1 || has_slurpy {
                return None;
            }
        }
        Some(Value::LazyList(std::sync::Arc::new(
            crate::value::LazyList::new_pipe(target, func, is_grep),
        )))
    }

    fn is_infinite_endpoint(v: &Value) -> bool {
        match v {
            Value::Num(n) => n.is_infinite(),
            Value::Rat(_, d) | Value::FatRat(_, d) => *d == 0,
            Value::Whatever | Value::HyperWhatever => true,
            _ => false,
        }
    }

    pub(super) fn dispatch_to_bag_with_what(
        &self,
        target: Value,
        what: &str,
    ) -> Result<Value, RuntimeError> {
        crate::builtins::quanthash_coerce::to_bag(target, what)
    }

    /// Check if a value is a lazy iterable (infinite range, lazy list, etc.)
    pub(crate) fn is_lazy_for_set_ops(v: &Value) -> bool {
        match v {
            Value::LazyList(_) => true,
            Value::Array(_, kind) if kind.is_lazy() => true,
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            Value::GenericRange { end, .. } => {
                let end_f = end.to_f64();
                end_f.is_infinite() && end_f.is_sign_positive()
            }
            _ => false,
        }
    }

    pub(super) fn dispatch_to_mix(&self, target: Value) -> Result<Value, RuntimeError> {
        crate::builtins::quanthash_coerce::to_mix(target, "Mix")
    }

    pub(super) fn dispatch_to_mix_with_what(
        &self,
        target: Value,
        what: &str,
    ) -> Result<Value, RuntimeError> {
        crate::builtins::quanthash_coerce::to_mix(target, what)
    }

    /// `.Hash` coercion — delegates to the single pure impl shared with the VM
    /// native path (`builtins::map_hash_coerce::to_hash`).
    pub(super) fn dispatch_to_hash(&self, target: Value) -> Result<Value, RuntimeError> {
        crate::builtins::map_hash_coerce::to_hash(target, true)
    }
}
