//! ADR-0044 D1: native function-form implementation of the seven core array
//! listops (`push`, `pop`, `shift`, `unshift`, `append`, `prepend`,
//! `splice`), reachable from `call_function_fallback`.
//!
//! Raku treats these as ordinary core `multi sub`s living in the setting: a
//! user/imported `multi` for one of these names ADDS a candidate alongside
//! the core behavior, and `&push`/`&splice` are genuine callable `Sub`
//! values. Before this, mutsu only had a compile-time syntactic rewrite
//! (`Compiler::compile_expr_call_inner` -> `CallMethodMut`) with no callable
//! existence behind the name at all, so a competing user/imported `multi`
//! made the core array form unreachable (`No matching candidates for proto
//! sub`), and `&push`/`&splice` either errored (`Unknown function`) or
//! silently did nothing. See `docs/adr/0044-listops-are-routines-not-a-syntactic-rewrite.md`.
//!
//! This does NOT touch the compiler's `CallMethodMut` fast path (still
//! emitted whenever no competing candidate is visible at the call site —
//! D2), and does NOT register these as ranked multi-dispatch candidates
//! (alternative B in the ADR) — it is purely the missing native-function leg
//! of the existing `dispatch_func_call_inner` -> `call_function_fallback`
//! chain that a user `multi abs(Str)` already rides alongside core `abs`.

use super::*;
use crate::value::ValueView;
use std::sync::atomic::{AtomicU64, Ordering};

static LISTOP_TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

impl Interpreter {
    /// Try to dispatch `name(target, ...)` as one of the seven core array
    /// listops in *function* (not method) form. Returns `None` when `name`
    /// is not one of these seven, or when there is no first argument to act
    /// as the invocant (the caller's existing "too few positionals" handling
    /// applies in that case).
    ///
    /// Delegates to the already-correct `call_method_mut_with_values`
    /// (mut-path method dispatch: typed-array element checks, shaped-array
    /// rejection, shared/thread-array bookkeeping, container-ref cells, ...)
    /// instead of reimplementing any of that. The invocant is the argument
    /// **value**, not a compile-time variable name (ADR-0044 D1): when the
    /// call site's own source variable is known (`pending_call_arg_sources`)
    /// and still bound, that exact name is used so this matches the compiled
    /// `CallMethodMut` fast path byte-for-byte; otherwise a synthetic temp
    /// binding stands in. Either way, mutating a real `Array` argument is
    /// visible to the caller because its `Gc<ArrayData>` is shared by
    /// identity (`crate::value::aliased_mut::gc_data_mut`), not copied.
    pub(crate) fn try_call_listop_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            name,
            "push" | "pop" | "shift" | "unshift" | "append" | "prepend" | "splice"
        ) {
            return None;
        }
        let (target, method_args) = args.split_first()?;
        let target = target.clone();
        let method_args = method_args.to_vec();

        let source_name = self
            .pending_call_arg_sources
            .as_ref()
            .and_then(|sources| sources.first())
            .and_then(|s| s.as_deref());

        let (target_var, used_temp): (String, bool) = match source_name {
            // The call site named a real variable (`push(@a, 1)`,
            // `&splice(@a, 1, 2)`) and it is still bound: use it verbatim so
            // every name-keyed mechanism in `call_method_mut_with_values`
            // (typed-array constraints, shared/thread-array atomics,
            // container-ref cells, redeclaration shadowing) behaves exactly
            // as the compiled method-call fast path.
            Some(n) if self.env.contains_key(n) => (n.to_string(), false),
            _ => {
                // No traceable source variable (a computed first argument
                // like `push(f(), 1)`), or `arg_sources` named an
                // element/attribute expression that isn't itself bound in
                // `env` (`push(@a[2], ...)`, `push($obj.attr, ...)` — D3
                // territory, not covered by D1). Bind a synthetic temp: for
                // a genuine Array target this still mutates in place and is
                // visible to the caller by `Gc` identity; for anything else
                // it reaches the ordinary method-dispatch error path
                // ("fails loudly", the accepted D3 consequence) instead of
                // silently doing nothing.
                let sigil = if matches!(target.view(), ValueView::Array(..)) {
                    "@"
                } else {
                    ""
                };
                let id = LISTOP_TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
                (format!("{sigil}__mutsu_listop_fn_target_{id}"), true)
            }
        };
        if used_temp {
            self.env.insert(target_var.clone(), target.clone());
        }
        let result = self.call_method_mut_with_values(&target_var, target, name, method_args);
        if used_temp {
            self.env.remove(&target_var);
        }
        Some(result)
    }
}
