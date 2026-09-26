//! `__mutsu_routine_export` — the native primitive behind the
//! `trait_mod:<is>(Routine:D, :$export!)` CORE.setting prelude
//! (`runtime::run::TRAIT_MOD_IS_EXPORT_PRELUDE`, injected via
//! `runtime::run_prelude::inject_trait_mod_is_export_prelude`).
//!
//! mutsu applies an `is export` written on a declaration natively, from the
//! plan's `is_export`/`export_tags` (`vm_register_sub_ops`). A custom trait
//! that re-dispatches to CORE's candidate — the `Exportable` distribution's
//! `is exportable` does `trait_mod:<is>(r, :export($exportable))` — reaches
//! the routine only as a value, after it was registered, so it needs the same
//! registration performed from that value.

use super::*;

impl Interpreter {
    /// `__mutsu_routine_export($routine, $export)`. `None` for any other
    /// function name, matching `try_trait_mod_does_apply`.
    pub(super) fn try_routine_export(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != "__mutsu_routine_export" {
            return None;
        }
        if args.len() != 2 {
            return Some(Err(RuntimeError::new(format!(
                "__mutsu_routine_export expects 2 arguments, got {}",
                args.len()
            ))));
        }
        let (package, routine) = match args[0].view() {
            ValueView::Sub(data) => (data.package.resolve(), data.name.resolve()),
            _ => return Some(Ok(Value::NIL)),
        };
        if routine.is_empty() || self.suppress_exports {
            return Some(Ok(Value::NIL));
        }
        let tags = Self::export_trait_tags(&args[1]);
        self.register_exported_sub(package, routine.clone(), tags);
        self.invalidate_fn_resolution_for_keys([Symbol::intern(&routine)]);
        Some(Ok(Value::NIL))
    }

    /// The tags an `is export(...)` argument names, as Rakudo's
    /// `trait_mod:<is>(Routine:D, :$export!)` reads them: a `Pair` is one tag
    /// (its key), a list is one tag per `Pair` key, and anything else (`True`)
    /// means `DEFAULT` — the empty list [`Interpreter::register_exported_sub`]
    /// fills in.
    // Cost: O(n), n = elements of a list-valued argument.
    fn export_trait_tags(arg: &Value) -> Vec<String> {
        match arg.view() {
            ValueView::Pair(key, _) => vec![key.to_string()],
            ValueView::ValuePair(key, _) => vec![key.to_string_value()],
            ValueView::Array(items, ..) => items.iter().flat_map(Self::export_trait_tags).collect(),
            _ => Vec::new(),
        }
    }
}
