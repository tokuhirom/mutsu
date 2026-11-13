//! `.^can(NAME)` on an ordinary user class (#17552).
//!
//! `$obj.^can("m")` is `$obj.HOW.can($obj, "m")`. For a receiver whose class
//! has the default `ClassHOW` (no EXPORTHOW / `new_type` metaclass in
//! `class_how_values`) and declares no `method ^can`, the general
//! `CallMethod` / `CallMethodMut` path reached `mop_can` and its per-class
//! memo only after the whole probe chain (~20k instructions per call). Red's
//! `Red::Column.gist` asks `$!attr.package.HOW.^can("as")` for every column
//! it renders, and a user metaclass instance is exactly such a receiver: its
//! own class (`MetamodelX::Red::Model`) has the default metaclass.

use super::*;

impl Interpreter {
    /// Answer `.^can(NAME)` (unquoted, one positional argument) on an instance
    /// or type object of an ordinary user class, replacing the receiver and
    /// the argument on the stack with the result. `None` leaves the stack
    /// untouched and the call to the generic path.
    // Cost: O(1) amortized to decide (hash probes and one memoized MRO probe);
    // a hit costs `mop_can`'s memo probe, O(1) amortized.
    pub(super) fn try_caret_can_lane(
        &mut self,
        code: &CompiledCode,
        method: &str,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "can"
            || arity != 1
            || quoted
            || self.accessor_ref_pending
            || modifier_idx.map(|idx| Self::const_str(code, idx)) != Some("^")
            || self.registry().method_name_ever_wrapped("can")
            || crate::runtime::find_method_intercept::any_user_find_method()
        {
            return None;
        }
        let len = self.stack.len();
        let target = self.stack.get(len.checked_sub(2)?)?;
        let name = self.stack.last()?;
        // A plain name; a Pair would be a named argument, anything else is
        // left to the generic path's coercion.
        name.as_str()?;
        let class = match target.view() {
            ValueView::Instance { class_name, .. } => class_name,
            ValueView::Package(name) => name,
            _ => return None,
        };
        {
            let registry = self.registry();
            if !registry.classes.contains_key(class.as_str())
                || registry.class_has_custom_how(class.as_str())
            {
                return None;
            }
        }
        if Self::is_metamodel_how(&class)
            || self.has_user_method_sym(class.as_str(), crate::symbol::Symbol::intern("^can"))
        {
            return None;
        }
        let name = self.stack.pop()?;
        let target = self.stack.pop()?;
        self.pending_call_arg_source_slots.clear();
        self.set_pending_call_arg_sources(None);
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethod", "caret-can");
        Some(self.mop_can(vec![target, name]))
    }
}
