//! Native `.sort` in the VM.
//!
//! `@a.sort` and every comparator/mapper form — `@a.sort({ $^a <=> $^b })`,
//! `@a.sort(*.abs)`, `@a.sort({ $^a.foo <=> $^b.foo })`, `%h.sort`, `.sort(:k)`,
//! a `Routine` comparator — now run entirely in the VM. User callables are
//! invoked through [`VmSortCaller`] (a [`SortCaller`] backed by
//! `vm_call_on_value`), and the orchestration itself (arity dispatch, merge
//! sort, Schwartzian transform, simple-comparator inlining, `:k` index mode) is
//! the *single* shared implementation in
//! [`crate::runtime::methods_collection_ops::sort`] — the same one the
//! interpreter uses. This removes the `.sort` interpreter fallback for plain
//! eager collections while keeping one sort implementation (no duplicate
//! orchestration).
//!
//! Non-plain targets (Shaped multi-dim arrays, `Lazy`, item/scalar-wrapped
//! arrays, `Instance`/`Supply`, …) still fall back to the interpreter's
//! `dispatch_sort` unchanged — which now shares the same orchestration.

use super::*;
use crate::runtime::methods_collection_ops::sort::{
    SortCaller, sort_indices_generic, sort_items_generic,
};
use crate::runtime::utils::pair_as_positional;
use crate::value::{ArrayKind, Value};
use std::sync::Arc;

/// [`SortCaller`] backed by the bytecode VM.
struct VmSortCaller<'a>(&'a mut VM);

impl SortCaller for VmSortCaller<'_> {
    fn call_callable(&mut self, callable: &Value, args: Vec<Value>) -> Value {
        // Hash-sourced `Value::Pair` elements must bind positionally to the
        // block (so `$^a`/`$_` is the pair, not a named argument). See
        // `pair_as_positional`.
        let args: Vec<Value> = args.iter().map(pair_as_positional).collect();
        self.0
            .vm_call_on_value(callable.clone(), args, None)
            .unwrap_or(Value::Nil)
    }

    fn call_method(&mut self, recv: Value, name: &str) -> Value {
        self.0
            .try_compiled_method_or_interpret(recv, name, vec![])
            .unwrap_or(Value::Nil)
    }
}

impl VM {
    /// Try to run `target.sort(...)` natively. Returns `Some(result)` when
    /// handled in the VM, `None` to fall back to the interpreter unchanged.
    ///
    /// `.sort` has no rw-view concern (it always yields a fresh `Seq`), so a
    /// freshly-built sorted array is a faithful result.
    pub(super) fn try_native_sort(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "sort" {
            return None;
        }

        // Collect the eager element list for the supported target shapes. Shaped
        // multi-dim arrays (sort over leaves), Lazy, and item/scalar-wrapped
        // arrays keep their interpreter semantics -> fall back.
        let items: Vec<Value> = match target {
            Value::Array(elems, ArrayKind::Array | ArrayKind::List) => elems.as_ref().clone(),
            Value::Seq(elems) | Value::Slip(elems) => elems.as_ref().clone(),
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => crate::runtime::Interpreter::value_to_list(target),
            Value::Hash(map) => map
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .collect(),
            _ => return None,
        };

        // Parse the (optional) comparator and the `:k` adverb. `:by` names the
        // callable; any other adverb (`:kv`/`:p`/`:v`) needs the interpreter.
        let mut callable: Option<Value> = None;
        let mut return_indices = false;
        for arg in args {
            match arg {
                Value::Pair(key, val) if key == "by" => callable = Some(val.as_ref().clone()),
                Value::Pair(key, val) if key == "k" => return_indices = val.truthy(),
                Value::Pair(..) => return None,
                _ => callable = Some(arg.clone()),
            }
        }

        // Resolve arity via the shared helper (rejects explicitly 0-arity subs).
        let arity = match self.interpreter.sort_callable_arity(callable.as_ref()) {
            Ok(a) => a,
            Err(e) => return Some(Err(e)),
        };

        let mut items = items;
        let mut caller = VmSortCaller(self);
        if return_indices {
            let indices = sort_indices_generic(&mut caller, &items, callable.as_ref(), arity);
            Some(Ok(Value::array(indices)))
        } else {
            sort_items_generic(&mut caller, &mut items, callable.as_ref(), arity);
            Some(Ok(Value::Seq(Arc::new(items))))
        }
    }
}
