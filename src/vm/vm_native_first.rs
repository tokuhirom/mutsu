//! Native `.first` in the Interpreter (no-adverb forms).
//!
//! `@a.first`, `@a.first({ .foo })`, `@a.first(* > 3)`, `@a.first(/rx/)`,
//! `%h.first({ .value > 1 })` run entirely in the Interpreter. The `Sub` matcher block
//! (the genuine Category-B fork) is invoked through `vm_call_on_value`; non-block
//! patterns use the interpreter's single shared `smart_match`. The scan itself
//! (iteration, `pair_as_positional`) is the shared
//! [`crate::runtime::resolution_map_grep::find_first_match_generic`] — the same one the
//! interpreter's `find_first_match_over_items` uses.
//!
//! Anything with an adverb (`:k`/`:kv`/`:p`/`:v`/`:end`), a `Bool` matcher (which
//! must raise `X::Match::Bool`), or a non-list target (`Supply`/`Instance`/…)
//! falls back to the interpreter's `builtin_first` / method dispatch unchanged.

use super::*;
use crate::runtime::resolution_map_grep::{FirstMatcher, find_first_match_generic};
use crate::runtime::utils::pair_as_positional;
use crate::value::Value;

/// [`FirstMatcher`] backed by the bytecode Interpreter.
struct VmFirstMatcher<'a>(&'a mut Interpreter);

impl FirstMatcher for VmFirstMatcher<'_> {
    fn item_matches(&mut self, pattern: &Value, item: &Value) -> Result<bool, RuntimeError> {
        if matches!(pattern, Value::Sub(_)) {
            let call_item = pair_as_positional(item);
            Ok(self
                .0
                .vm_call_on_value(pattern.clone(), vec![call_item], None)?
                .truthy())
        } else {
            Ok(self.0.smart_match(item, pattern))
        }
    }
}

impl Interpreter {
    /// Test a `grep`/`first`-style matcher against a single `item` using
    /// Interpreter-native dispatch (a `Sub` block via `vm_call_on_value`, any other
    /// pattern via the shared `smart_match`). Used by the lazy map/grep
    /// pipeline (`force_lazy_pipe`) so the matcher runs in this Interpreter and keeps
    /// locals/env coherent (the interpreter's `eval_grep_over_items` spins up a
    /// nested Interpreter via `mem::take`, which would discard the surrounding frame).
    pub(super) fn vm_grep_item_matches(
        &mut self,
        pattern: &Value,
        item: &Value,
    ) -> Result<bool, RuntimeError> {
        VmFirstMatcher(self).item_matches(pattern, item)
    }

    /// Try to run `target.first(...)` natively. Returns `Some(result)` when
    /// handled in the Interpreter, `None` to fall back unchanged.
    pub(super) fn try_native_first(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "first" {
            return None;
        }

        // Plain list-like targets plus `Hash` (`.first` iterates its pairs);
        // `Supply`/`Instance`/… keep their own semantics -> fall back.
        if !Self::is_plain_eager_list(target) && !matches!(target, Value::Hash(_)) {
            return None;
        }

        // At most one positional matcher and no adverbs. Any `Pair` arg is an
        // adverb (`:k`/`:v`/`:end`/…) handled only by the interpreter.
        let mut func: Option<Value> = None;
        for arg in args {
            match arg {
                Value::Pair(..) => return None,
                // `Bool` matcher must raise X::Match::Bool -> let the interpreter
                // produce the typed exception.
                Value::Bool(_) => return None,
                _ if func.is_none() => func = Some(arg.clone()),
                _ => return None,
            }
        }

        let items = crate::runtime::utils::value_to_list(target);
        let mut matcher = VmFirstMatcher(self);
        match find_first_match_generic(&mut matcher, func.as_ref(), &items, false) {
            Ok(Some((_, value))) => Some(Ok(value)),
            Ok(None) => Some(Ok(Value::Nil)),
            Err(e) => Some(Err(e)),
        }
    }

    /// Lazy `.first` over a gather-sourced `LazyList`: pull elements
    /// incrementally via the coroutine and test the matcher, instead of forcing
    /// the whole (possibly infinite) list. Returns `Some` when handled, `None`
    /// to fall back to the eager full-force path (adverbs / `Bool` matcher).
    pub(super) fn try_lazy_gather_first(
        &mut self,
        list: &crate::value::LazyList,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // Mirror `try_native_first` gating: at most one positional matcher and
        // no adverbs / `Bool` matcher (those keep interpreter semantics).
        let mut func: Option<Value> = None;
        for arg in args {
            match arg {
                Value::Pair(..) | Value::Bool(_) => return None,
                _ if func.is_none() => func = Some(arg.clone()),
                _ => return None,
            }
        }

        // No matcher: `.first` is just the first element.
        let Some(func) = func else {
            return match self.force_lazy_list_vm_n(list, 1) {
                Ok(items) => Some(Ok(items.into_iter().next().unwrap_or(Value::Nil))),
                Err(e) => Some(Err(e)),
            };
        };

        // Pull one element at a time and test it. Pulling singly (rather than in
        // chunks) keeps the gather body's side effects in step with the match
        // position, matching Rakudo (`.first(* >= 3)` runs the body exactly as
        // far as the first match, no further).
        //
        // Slice F (gather coroutine coherence): the matcher closure (`* >= 3`)
        // runs via `exec` and leaves `self.current_code` pointing at *its* frame.
        // `force_lazy_list_vm_n` captures `self.current_code` as the frame to
        // reconcile a captured-outer write into (`my $c; gather { loop { $c++ } }`),
        // so without restoring it each iteration the second-and-later forces
        // reconcile the matcher's frame instead of the caller's, leaving the
        // caller's `$c` slot stale under reverse-sync OFF. Pin the caller frame
        // captured at entry and restore it before every force.
        let caller_code = self.current_code;
        let mut idx = 0usize;
        loop {
            self.current_code = caller_code;
            let items = match self.force_lazy_list_vm_n(list, idx + 1) {
                Ok(v) => v,
                Err(e) => return Some(Err(e)),
            };
            // Fewer items than requested => the gather body finished (finite
            // source) without a match.
            if items.len() <= idx {
                return Some(Ok(Value::Nil));
            }
            let item = items[idx].clone();
            let matched = {
                let mut matcher = VmFirstMatcher(self);
                match matcher.item_matches(&func, &item) {
                    Ok(b) => b,
                    Err(e) => return Some(Err(e)),
                }
            };
            if matched {
                return Some(Ok(item));
            }
            idx += 1;
        }
    }
}
