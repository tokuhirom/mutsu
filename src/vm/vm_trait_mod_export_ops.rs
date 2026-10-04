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
    /// `__mutsu_routine_export($routine, $export, $symbol?)`. `None` for any
    /// other function name, matching `try_trait_mod_does_apply`. A defined
    /// `$symbol` (the candidate's `:$SYMBOL`) naming another `&`-key exports
    /// the routine under that name instead.
    pub(super) fn try_routine_export(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != "__mutsu_routine_export" {
            return None;
        }
        if !(2..=3).contains(&args.len()) {
            return Some(Err(RuntimeError::new(format!(
                "__mutsu_routine_export expects 2 or 3 arguments, got {}",
                args.len()
            ))));
        }
        if self.module.suppress_exports {
            return Some(Ok(Value::NIL));
        }
        let tags = Self::export_trait_tags(&args[1]);
        let own = match args[0].view() {
            ValueView::Sub(data) => Some((data.package.resolve(), data.name.resolve())),
            _ => None,
        };
        // An explicit `:SYMBOL` exports the routine *value* under that key
        // from the current package -- unless it just restates what the
        // by-name registration below already does (the routine's own name,
        // declared in this package). A routine re-exported from another
        // package, or one whose value is not a plain `Sub` (a proto read
        // back out of another module's EXPORT stash), has no by-name
        // registration here to reuse.
        if let Some(symbol) = args
            .get(2)
            .filter(|s| matches!(s.view(), ValueView::Str(_)))
            && let Some(bare) = symbol
                .to_string_value()
                .strip_prefix('&')
                .map(str::to_string)
            && !bare.is_empty()
            && own.as_ref().is_none_or(|(package, routine)| {
                *routine != bare || *package != self.current_package()
            })
        {
            self.export_routine_value_as(&args[0], &bare, tags);
            return Some(Ok(Value::NIL));
        }
        let Some((package, routine)) = own else {
            return Some(Ok(Value::NIL));
        };
        if routine.is_empty() {
            return Some(Ok(Value::NIL));
        }
        self.register_exported_sub(package, routine.clone(), tags);
        self.invalidate_fn_resolution_for_keys([Symbol::intern(&routine)]);
        Some(Ok(Value::NIL))
    }

    /// Export `routine` under the stash key `&bare` rather than its own name,
    /// the way Rakudo's `EXPORT_SYMBOL` does: bind the value into the current
    /// package's `EXPORT::ALL` stash and into `EXPORT::<tag>` for every tag
    /// (`DEFAULT` when none is named). The routine is published as a value,
    /// not re-registered by name, because its own name is not the exported
    /// one and it usually belongs to another package altogether.
    // Cost: O(t), t = number of export tags.
    fn export_routine_value_as(&mut self, routine: &Value, bare: &str, mut tags: Vec<String>) {
        if tags.is_empty() {
            tags.push("DEFAULT".to_string());
        }
        if !tags.iter().any(|t| t == "ALL") {
            tags.insert(0, "ALL".to_string());
        }
        let package = self.current_package_sym();
        let global = crate::qualified::is_global_package(package);
        for tag in tags {
            let stash = if global {
                format!("EXPORT::{tag}")
            } else {
                format!("{package}::EXPORT::{tag}")
            };
            self.publish_package_stash_symbol(stash, "&", bare, routine);
        }
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
