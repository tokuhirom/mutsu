//! `nqp::getlexouter` with a literal name (#11498).
//!
//! The op looks a lexical up starting at the scope enclosing the running one,
//! cascading outward — exactly what `OUTERS::<$x>` answers — and dies when no
//! scope has it. Which scope declares a name is a compile-time question, so a
//! literal name compiles to that `OUTERS::` read: the same capture of a
//! shadowed outer binding `OUTER::` uses, with no by-name runtime walk. A
//! computed name, and a literal no scope of this compilation declares (a
//! setting name, or one an `EVAL`'s outer context holds), stay a runtime op
//! (`runtime::nqp_ops_lexical`).

use super::*;

impl Compiler {
    /// Compile `nqp::getlexouter($name)`; `false` (nothing emitted) unless the
    /// operand is a single string literal.
    // Cost: O(1) at run time when the name resolves (the `OUTERS::` read it
    // compiles to); otherwise the runtime op's cost.
    pub(super) fn try_compile_nqp_getlexouter(&mut self, args: &[Expr]) -> bool {
        let [Expr::Literal(lit)] = args else {
            return false;
        };
        let crate::value::ValueView::Str(name) = lit.view() else {
            return false;
        };
        // A scalar lives under its bare name; `@a` / `%h` / `&f` keep the sigil.
        let bare = name.strip_prefix('$').unwrap_or(name.as_str()).to_string();
        if self.outers_declaring_depth(&bare).is_some() {
            self.emit_outers_var_access(bare);
            return true;
        }
        // Only the running scope declares it: a by-name lookup at run time
        // would find that binding, which is not an outer one.
        if self
            .local_scopes
            .last()
            .is_some_and(|own| own.contains_key(&bare))
        {
            let msg = format!("No lexical found with name '{}'", name.as_str());
            let idx = self.code.add_constant(Value::str(msg));
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die { user_throw: false });
            return true;
        }
        false
    }
}
