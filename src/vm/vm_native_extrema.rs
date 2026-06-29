//! Native `.min` / `.max` / `.minmax` in the Interpreter.
//!
//! `@a.min`, `@a.max`, `@a.minmax`, their `:by` block forms (`*.abs`,
//! `{ $^a <=> $^b }`), and the `:k`/`:v`/`:kv`/`:p` adverbs (min/max only) over a
//! plain eager list run entirely in the Interpreter. The `:by` block (the genuine
//! Category-B fork — the only part the pure-native layer cannot do) is invoked
//! through `vm_call_on_value`, and the folds themselves are the *single* shared
//! implementations [`crate::runtime::Interpreter::extrema_from_values_generic`]
//! and [`crate::runtime::Interpreter::minmax_from_values_generic`] — the same
//! ones the interpreter's `extrema_from_values_by` / `builtin_minmax` use. This
//! removes the `.min` / `.max` / `.minmax` interpreter fallback for plain
//! collections without adding a second copy of the fold.
//!
//! Hash targets, `Instance`/`Supply`, and any call with extra positional
//! arguments fall back to the interpreter unchanged.

use super::*;
use crate::value::Value;

impl Interpreter {
    /// Try to run `target.min(...)` / `target.max(...)` natively. Returns
    /// `Some(result)` when handled in the Interpreter, `None` to fall back unchanged.
    ///
    /// `.min`/`.max` yield a fresh value (no rw-view concern).
    pub(super) fn try_native_extrema(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let want_max = match method {
            "min" => false,
            "max" => true,
            _ => return None,
        };

        // Only plain eager lists; everything else (Hash/Instance/Supply/Lazy/…)
        // keeps interpreter semantics -> fall back.
        if !Self::is_plain_eager_list(target) {
            return None;
        }
        // A Range's extremum is its first/last element; the index (for `:k`/`:kv`/
        // `:p`) and the unbounded `n..Inf` case need Range-aware handling that the
        // generic list fold can't do, so defer Ranges to the slow path
        // (`dispatch_min_max_method`).
        if Self::value_is_rangey(target) {
            return None;
        }

        // Parse args: an optional `:by` callable (a bare Sub/Routine first arg or
        // a `:by(...)` pair) and an optional `:k`/`:v`/`:kv`/`:p` adverb. Any
        // other positional argument is an unusual form -> fall back.
        let mut by: Option<Value> = None;
        let mut adverb: Option<String> = None;
        for arg in args {
            match arg {
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } if by.is_none() => {
                    by = Some(arg.clone());
                }
                Value::Pair(name, val) if name == "by" => by = Some((**val).clone()),
                Value::ValuePair(key, val) if matches!(key.as_ref(), Value::Str(n) if n.as_str() == "by") =>
                {
                    by = Some((**val).clone());
                }
                Value::Pair(name, val)
                    if matches!(name.as_str(), "k" | "v" | "kv" | "p")
                        && matches!(val.as_ref(), Value::Bool(true)) =>
                {
                    adverb = Some(name.clone());
                }
                _ => return None,
            }
        }

        let by_arity = by.as_ref().map(|b| self.extrema_callable_arity(b));

        // The generic fold flattens a single list argument, so wrap the target.
        let positional = [target.clone()];
        let fold = crate::runtime::Interpreter::extrema_from_values_generic(
            &positional,
            want_max,
            by.as_ref(),
            by_arity,
            |c, a| self.vm_call_on_value(c.clone(), a, None),
        );
        let result = match fold {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };

        match adverb {
            None => Some(Ok(result)),
            Some(adv) => Some(self.apply_extrema_adverb(&positional, result, want_max, Some(&adv))),
        }
    }

    /// Try to run `target.minmax(...)` natively. Returns `Some(result)` when
    /// handled in the Interpreter, `None` to fall back unchanged.
    pub(super) fn try_native_minmax(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "minmax" {
            return None;
        }
        if !Self::is_plain_eager_list(target) {
            return None;
        }

        // `.minmax` takes only an optional `:by` callable (no `:k`/etc. adverbs).
        let mut by: Option<Value> = None;
        for arg in args {
            match arg {
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } if by.is_none() => {
                    by = Some(arg.clone());
                }
                Value::Pair(name, val) if name == "by" => by = Some((**val).clone()),
                Value::ValuePair(key, val) if matches!(key.as_ref(), Value::Str(n) if n.as_str() == "by") =>
                {
                    by = Some((**val).clone());
                }
                _ => return None,
            }
        }

        let by_arity = by.as_ref().map(|b| self.extrema_callable_arity(b));
        let positional = [target.clone()];
        Some(crate::runtime::Interpreter::minmax_from_values_generic(
            &positional,
            by.as_ref(),
            by_arity,
            |c, a| self.vm_call_on_value(c.clone(), a, None),
        ))
    }
}
