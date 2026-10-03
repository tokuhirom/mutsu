//! `OpCode::CaptureRwArgCell`: the container an lvalue call relays to an
//! `is rw` parameter (#11077).
//!
//! `f($h) = 1` and `++f($h)` are rewritten by the parser into
//! `__mutsu_assign_named_sub_lvalue("f", [$h], 1)` /
//! `__mutsu_incdec_named_sub_lvalue(...)`, so `f`'s arguments travel inside a
//! list literal. `MakeArray` boxes a `$`-scalar element into its shared cell
//! only when the variable holds a plain value; a Hash/Array/object reaches the
//! routine bare, and an `is rw` parameter rejects it as "a value without a
//! container". This op boxes the variable the way the direct call's binder
//! does, so both spellings bind the same Scalar.

use super::*;

impl Interpreter {
    // Cost: O(1) with the compiler's slot hint; O(l) by-name locals fallback, l = frame locals.
    pub(super) fn exec_capture_rw_arg_cell_op(&mut self, code: &CompiledCode) {
        let val = self.stack.pop().unwrap_or(Value::NIL);
        let ValueView::VarRef {
            name, value: inner, ..
        } = val.view()
        else {
            self.stack.push(val);
            return;
        };
        let source_name = name.resolve();
        // A readonly binding (`my $b := 42`, a non-rw parameter) has no
        // container to alias: hand the binder the VarRef unchanged so it
        // reports the binding the way a direct call does.
        if self.name_is_readonly_binding(&source_name) {
            self.stack.push(val);
            return;
        }
        let slot_hint = val.varref_slot();
        let cell = self.capture_rw_arg_cell(code, &source_name, inner.clone(), slot_hint);
        self.register_container_cell_constraint_for_name(&cell, &source_name);
        self.stack.push(cell);
    }
}
