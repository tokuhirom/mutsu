//! The "Use of uninitialized value $x of type Any in numeric context" warning
//! for a type-object operand of a numeric infix op (`$q + 1`, `$q == 0`), #9359.
use super::*;

impl Interpreter {
    /// Note that the numeric infix op at `ip` of `code` is about to run, and
    /// return the site it replaces, which the caller restores afterwards (see
    /// `Interpreter::numeric_op_site`).
    // Cost: O(1).
    #[inline]
    pub(crate) fn enter_numeric_op_site(
        &mut self,
        code: &CompiledCode,
        ip: usize,
    ) -> (usize, usize) {
        std::mem::replace(
            &mut self.numeric_op_site,
            (code as *const CompiledCode as usize, ip),
        )
    }

    /// Warn about an undefined operand of a numeric infix op and resume with
    /// its numeric zero, as rakudo's generic `Any`-candidate coercion does. A
    /// defined operand, `Mu` (no numeric coercion at all), a concrete numeric
    /// type object (`X::Numeric::Uninitialized`, see
    /// `check_type_object_in_numeric_context`) and a class with its own
    /// `Numeric` method are handed back untouched. `side` is 0 for the left
    /// operand and 1 for the right one.
    // Cost: O(1) for a defined operand; the warning itself is O(h), h = active
    // CONTROL handlers.
    pub(crate) fn warn_uninitialized_numeric_operand(
        &mut self,
        value: Value,
        side: usize,
    ) -> Result<Value, RuntimeError> {
        let ValueView::Package(name) = value.view() else {
            return Ok(value);
        };
        let type_name = name.resolve();
        if matches!(
            type_name.as_ref(),
            "Mu" | "Int" | "Num" | "Rat" | "FatRat" | "Real" | "Bool"
        ) || self.has_user_method(&type_name, "Numeric")
        {
            return Ok(value);
        }
        let var = self.numeric_operand_var_name(side);
        let msg = format!(
            "Use of uninitialized value{} of type {} in numeric context",
            var.map(|v| format!(" {v}")).unwrap_or_default(),
            crate::value::user_facing_type_name(&type_name),
        );
        let caller_code = self.current_code;
        let resumed =
            self.raise_resumable_warning(&msg, Self::type_object_numeric_zero(&type_name))?;
        self.reconcile_caller_after_internal_dispatch(caller_code);
        Ok(resumed)
    }

    /// The source name of operand `side` of the numeric op the interpreter
    /// loop is executing (see `CompiledCode::numeric_operand_names`), unless
    /// that name is bound straight to a value with no container behind it (a
    /// readonly parameter), which rakudo does not name either.
    ///
    /// TODO: a JIT-compiled op reaches here with no op index, so its warning
    /// goes unnamed; the shim would have to pass the op index it was compiled
    /// from.
    fn numeric_operand_var_name(&self, side: usize) -> Option<crate::symbol::Symbol> {
        let (site_code, ip) = self.numeric_op_site;
        if site_code == 0 || site_code != self.current_code {
            return None;
        }
        // SAFETY: `current_code` addresses the CompiledCode of the frame whose
        // op is running right now (an ancestor stack frame, see its doc).
        let code = unsafe { &*(site_code as *const CompiledCode) };
        let name = code.numeric_operand_names_at(ip)[side]?;
        let bare = name.as_str();
        let bare = bare.strip_prefix('$').unwrap_or(bare);
        if self.readonly_kind(bare).is_some() {
            return None;
        }
        Some(name)
    }
}
