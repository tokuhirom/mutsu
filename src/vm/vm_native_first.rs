//! Native `.first` in the VM (no-adverb forms).
//!
//! `@a.first`, `@a.first({ .foo })`, `@a.first(* > 3)`, `@a.first(/rx/)`,
//! `%h.first({ .value > 1 })` run entirely in the VM. The `Sub` matcher block
//! (the genuine Category-B fork) is invoked through `vm_call_on_value`; non-block
//! patterns use the interpreter's single shared `smart_match`. The scan itself
//! (iteration, `pair_as_positional`) is the shared
//! [`crate::runtime::resolution::find_first_match_generic`] — the same one the
//! interpreter's `find_first_match_over_items` uses.
//!
//! Anything with an adverb (`:k`/`:kv`/`:p`/`:v`/`:end`), a `Bool` matcher (which
//! must raise `X::Match::Bool`), or a non-list target (`Supply`/`Instance`/…)
//! falls back to the interpreter's `builtin_first` / method dispatch unchanged.

use super::*;
use crate::runtime::resolution::{FirstMatcher, find_first_match_generic};
use crate::runtime::utils::pair_as_positional;
use crate::value::{ArrayKind, Value};

/// [`FirstMatcher`] backed by the bytecode VM.
struct VmFirstMatcher<'a>(&'a mut VM);

impl FirstMatcher for VmFirstMatcher<'_> {
    fn item_matches(&mut self, pattern: &Value, item: &Value) -> Result<bool, RuntimeError> {
        if matches!(pattern, Value::Sub(_)) {
            let call_item = pair_as_positional(item);
            Ok(self
                .0
                .vm_call_on_value(pattern.clone(), vec![call_item], None)?
                .truthy())
        } else {
            Ok(self.0.interpreter.smart_match(item, pattern))
        }
    }
}

impl VM {
    /// Try to run `target.first(...)` natively. Returns `Some(result)` when
    /// handled in the VM, `None` to fall back unchanged.
    pub(super) fn try_native_first(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "first" {
            return None;
        }

        // Only plain list-like targets; `Supply`/`Instance`/… keep their own
        // semantics -> fall back.
        match target {
            Value::Array(_, ArrayKind::Array | ArrayKind::List)
            | Value::Seq(_)
            | Value::Slip(_)
            | Value::Hash(_)
            | Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {}
            _ => return None,
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
}
