//! Native `.sort` in the Interpreter.
//!
//! `@a.sort` and every comparator/mapper form — `@a.sort({ $^a <=> $^b })`,
//! `@a.sort(*.abs)`, `@a.sort({ $^a.foo <=> $^b.foo })`, `%h.sort`, `.sort(:k)`,
//! a `Routine` comparator — now run entirely in the Interpreter. User callables are
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
use crate::runtime::methods_collection_ops::sort::{SortCaller, sort_value_generic};
use crate::runtime::utils::pair_as_positional;
use crate::value::{ArrayKind, Value};

/// [`SortCaller`] backed by the bytecode Interpreter.
struct VmSortCaller<'a>(&'a mut Interpreter);

impl SortCaller for VmSortCaller<'_> {
    fn call_callable(&mut self, callable: &Value, args: Vec<Value>) -> Value {
        // Hash-sourced `Pair` elements must bind positionally to the
        // block (so `$^a`/`$_` is the pair, not a named argument). See
        // `pair_as_positional`.
        let args: Vec<Value> = args.iter().map(pair_as_positional).collect();
        self.0
            .vm_call_on_value(callable.clone(), args, None)
            .unwrap_or(Value::NIL)
    }

    fn call_method(&mut self, recv: Value, name: &str) -> Value {
        self.0
            .try_compiled_method_or_interpret(recv, name, vec![])
            .unwrap_or(Value::NIL)
    }

    fn callable_arity(&self, callable: Option<&Value>) -> Result<usize, RuntimeError> {
        self.0.sort_callable_arity(callable)
    }
}

impl Interpreter {
    /// Try to run `target.sort(...)` natively. Returns `Some(result)` when
    /// handled in the Interpreter, `None` to fall back to the interpreter unchanged.
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

        // Gate: only plain eager target shapes run natively. Shaped multi-dim
        // arrays (sort over leaves), Lazy, item/scalar-wrapped arrays, and
        // `Instance`/`Supply` keep their interpreter semantics -> fall back. The
        // arg parsing, shape dispatch, and orchestration are then the single
        // shared `sort_value_generic` (same code the interpreter runs).
        //
        // Set/Bag/Mix belong here too: `sort_value_generic` decomposes them into
        // `key => weight` pairs and re-enters, so they are as "plain eager" as a
        // Hash. Leaving them out sent them to the interpreter's `InterpCaller`,
        // which cannot invoke a compiled `WhateverCode` mapper — every key came
        // back `Nil`, all elements compared equal, and `.sort(-*.value)` returned
        // the container's (unordered) iteration order. Pinned by
        // `t/sort-setty-whatever.t`.
        match target.view() {
            ValueView::Array(_, ArrayKind::Array | ArrayKind::List)
            | ValueView::Seq(_)
            | ValueView::Slip(_)
            | ValueView::Hash(_)
            | ValueView::Set(..)
            | ValueView::Bag(..)
            | ValueView::Mix(..)
            | ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => {}
            _ => return None,
        }

        let mut caller = VmSortCaller(self);
        Some(sort_value_generic(&mut caller, target.clone(), args))
    }
}
