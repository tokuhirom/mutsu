use super::*;
use crate::runtime::dispatch_key;
use crate::symbol::Symbol;

impl Interpreter {
    /// The `&name` value a custom `sub EXPORT` installed into `env` for a
    /// bareword `name` that has no package routine of its own, if any.
    ///
    /// Such a hook may hand back a materialized dispatcher (`Map.new(
    /// EXPORT::all::{'&f'}:p)`) whose candidates live only under the
    /// exporting module's package, so name-based resolution cannot find them
    /// even though a `proto` of that bare name is registered. Every call form
    /// must dispatch through the installed value instead; the statement forms
    /// (then their own opcodes) used to fall through to the registry and die with "Cannot resolve
    /// caller" on any call carrying a named argument (#9261). Ordinary exports
    /// (JSON::Tiny's `from-json`) keep the normal dispatch precedence because
    /// they do have a registered package routine.
    // Cost: O(1) hash probes when `name` is not an EXPORT-installed override.
    ///
    /// A caller compiled in the same unit that declared the installed callable
    /// is not an importer of it: `sub wrap` exported as `&inner-fn` must not
    /// capture the `inner-fn` call its own body makes to the routine it
    /// imported from elsewhere (JSON::Pretty over JSON::Fast's `proto
    /// to-json`, which has no plain package routine to trip `has_function`).
    pub(super) fn export_hook_callable(
        &self,
        name: &str,
        name_sym: Symbol,
        code: &CompiledCode,
    ) -> Option<Value> {
        if !self.export_amp_override_names.contains(&name_sym) || self.has_function(name) {
            return None;
        }
        let installed =
            dispatch_key::with_amp_name(name, |ampname| self.env().get(ampname).cloned())?;
        if Self::callable_declared_in_unit_of(&installed, code) {
            return self.unit_imported_callable(code, name_sym);
        }
        Some(installed)
    }

    /// What the unit `code` was compiled in itself imported under `name`
    /// through a `sub EXPORT` map (`unit_imported_callables`), if anything.
    // Cost: O(1) hash probe.
    pub(super) fn unit_imported_callable(
        &self,
        code: &CompiledCode,
        name_sym: Symbol,
    ) -> Option<Value> {
        let file = code.source_file?;
        self.unit_imported_callables.get(&(file, name_sym)).cloned()
    }

    /// A statement-level call discards its value, so that value is *sunk* —
    /// and sinking an unhandled `Failure` throws, exactly as `OpCode::SinkPop`
    /// does for the call shapes that leave their result on the stack. A call
    /// that discards its value without going through `SinkPop` (a supply's tap
    /// callback) must sink it here, or it swallows the Failure: `EVAL
    /// 'use fatal; "foo"[2]';` once ran on to the next statement where raku
    /// throws.
    ///
    /// A deferred `LazyList`/`LazyIoLines` (e.g. a bare `gather { ... }` as an
    /// `EVAL`'d snippet's tail statement) must also be *forced* here, exactly
    /// as `SinkPop` forces one — otherwise `EVAL 'gather { return 1 }';`
    /// never runs the body at all, so `throws-like`'s own `EVAL $code, context
    /// => $ctx;` call never sees the escaping `return`.
    pub(crate) fn sink_discarded_call_value(&mut self, value: &Value) -> Result<(), RuntimeError> {
        match value.view() {
            // A `.cache`-returned view or a `$s = SEQ`-itemized value must not
            // be force-drained — see the matching guard in `SinkPop`
            // (`vm_exec_dispatch.rs`) for the full rationale.
            ValueView::LazyList(list) if list.is_cached_no_sink() || list.is_itemized() => {}
            ValueView::LazyList(list) => {
                self.force_lazy_list_vm(&list)?;
            }
            ValueView::Seq(body) if body.needs_touch() => {
                let body = std::sync::Arc::clone(&body);
                self.sink_seq_body(&body)?;
            }
            _ => {
                if let Some(err) = self.failure_to_runtime_error_if_unhandled(value) {
                    return Err(err);
                }
                // Under `use fatal`, a sunk list/Seq holding an unhandled Failure
                // throws too; without the pragma such a list stays soft. Same
                // rule as SinkPop.
                if self.fatal_mode
                    && let Some(err) = self.unhandled_failure_in_list_for_fatal(value)
                {
                    return Err(err);
                }
            }
        }
        Ok(())
    }
}
