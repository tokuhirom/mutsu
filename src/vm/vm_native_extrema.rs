//! Native `.min` / `.max` in the VM.
//!
//! `@a.min`, `@a.max`, `@a.min(*.abs)`, `@a.max({ $^a <=> $^b })`, and the
//! `:k`/`:v`/`:kv`/`:p` adverbs over a plain eager list run entirely in the VM.
//! The `:by` block (the genuine Category-B fork — the only part the pure-native
//! layer cannot do) is invoked through `vm_call_on_value`, and the fold itself
//! (flatten / failure handling / `Any`-filter / extremum selection) is the
//! *single* shared implementation
//! [`crate::runtime::Interpreter::extrema_from_values_generic`] — the same one
//! the interpreter's `extrema_from_values_by` uses. This removes the `.min` /
//! `.max` interpreter fallback for plain collections without adding a second
//! copy of the fold.
//!
//! Hash targets, `Instance`/`Supply`, `.minmax`, and any call with extra
//! positional arguments fall back to the interpreter's `builtin_min`/`builtin_max`
//! unchanged.

use super::*;
use crate::value::{ArrayKind, Value};

impl VM {
    /// Try to run `target.min(...)` / `target.max(...)` natively. Returns
    /// `Some(result)` when handled in the VM, `None` to fall back unchanged.
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

        // Only plain eager lists. Hash sorts by key via a separate path; ranges
        // expand; everything else (Instance/Supply/Lazy/…) keeps interpreter
        // semantics -> fall back.
        match target {
            Value::Array(_, ArrayKind::Array | ArrayKind::List)
            | Value::Seq(_)
            | Value::Slip(_)
            | Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {}
            _ => return None,
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

        let by_arity = by
            .as_ref()
            .map(|b| self.interpreter.extrema_callable_arity(b));

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
            Some(adv) => Some(self.interpreter.apply_extrema_adverb(
                &positional,
                result,
                want_max,
                Some(&adv),
            )),
        }
    }
}
